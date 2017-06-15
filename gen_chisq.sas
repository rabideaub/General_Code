%macro chisq (ds,cutoff,byvar,subset,formats);

	/*Randomly select &subset number of observations from the dataset. Useful for large datasets since
      most of the output is size invariant (e.g. percents and means) and the program processes slowly*/
	%if &subset>0 & &subset~=. %then %do;
		PROC SURVEYSELECT DATA=&ds OUT=&ds METHOD=SRS
			SAMPSIZE=&subset SEED=1234567;
		RUN;
	%end;

	 proc contents data=&ds out=freqchk1;
	 run;
	/**********************************************************
	 Get distributions for each categorical variable
	**********************************************************/

	/*Create numbered macrovariables for each variable*/
	 data _null_;
		 set freqchk1 (keep=name type) end=last;
		 name=lowcase(name);
		 call symput('var'||trim(left(_n_)),trim(left(name)));
		 call symput('fmt'||trim(left(_n_)),'$'||trim(left(name))||'z'); /*Add z because format names cannot end in a number. Formats always char*/
		 if last then call symput('count',trim(left(_n_)));
	 run;

	 %do i=1 %to &count;

		/*Read in the format for the appropriate variable if formats are applicable*/
	 	%if "&formats."="Y" %then %do;
			data fmt (keep=Value Description fmtname rename=(Value=START Description=LABEL));
				set fmt.mds_3_value_labels (where=(lowcase(Variable)="&&var&i."));
				fmtname="&&fmt&i.";
			run;

			proc format cntlin=fmt;
			run; 
		%end;

	/*Check to see if the number of levels in each var is within range set by cutoff*/
		 proc sort data=&ds out=freqchk2 (keep=&&var&i) nodupkey; by &&var&i; run;
		 data _null_;
			 set freqchk2 end=last;
			 if last then do;
			 if _n_<=&cutoff then flag=1;
			 else flag=0;
			 call symput("flag&i",trim(left(flag)));
			 end;
		 run;
		 proc datasets library=work;
		 	delete freqchk2;
		 quit;
	 %end;

	/*Make a list of variables that are not featured in these checks*/	
	 data no_checks;
	 	set freqchk1 (keep=name type);
		%do k=1 %to &count;
			if &&flag&k. = 1 | type=1 then do;
				if name="&&var&k." then delete;
			end;
		%end;
	 run;
	
	proc sql noprint;
		select compress(name) into: no_checks
		separated by '0A'x
		from no_checks;
	quit;

	data _NULL_;
		set freqchk1;
		if _n_=1 then do;
			file print;
			put "*********************************************************************";
			put "THE VARIABLES THAT ARE NOT INCLUDED IN THIS PROGRAM'S CHECKS ARE:****";
			put "The following variables have too many levels and are not numeric:****";
			put "                                                                 ****";
			put "&no_checks.";
			put "*********************************************************************";
			file log;
		end;
	run;

	 /**********************************************************
	  Get distributions for each categorical variable by byvar
	 **********************************************************/
	 %if "&byvar"~="" %then %do;
		 /*Identify all levels of the byvar*/
		 proc freq data=&ds;
		 	tables &byvar / out=byvar;
		 run;
		 data _NULL_;
			 set byvar (keep=&byvar) end=last;
			 call symput('val'||trim(left(_n_)),trim(left(&byvar)));
			 if last then do;
				call symput('num',trim(left(_n_)));
				call symput('vartype',vtype(&byvar));
			 end;
		 run;

	 	 /*Run crosstab for each variable within the cutoff range*/
		 %do i=1 %to &count;
		     %if &&flag&i=1 & "&&var&i"~="&byvar" %then %do;
			 	%do j=1 %to &num;
					%if "&&val&j" ~=" " & "&&val&j" ~= "." %then %do; 

						/*Store the chisq stat in a macro - add it on at the end*/
						 ods output ChiSq = chi;
						 proc freq data=&ds order=freq;
						 	table &&var&i*&byvar / chisq missing norow nocum nofreq;
						 run;
						 ods output close;
				
						 data _NULL_;
							set chi;
							if trim(left(statistic))="Chi-Square" then call symput("chisq",prob);
						run;

						/*Generate the distribution*/
						 proc freq data=&ds order=freq noprint;
						 	table &&var&i*&byvar / chisq missing norow nocum nofreq out=val_&j;
							/*Account for numeric or character by-variable*/
							%if "&vartype"="N" %then %do;
								where &byvar. = &&val&j;
							%end;
							%else %do;
								where &byvar. = "&&val&j";
							%end;
						 run;

						 data val_&j;
							set val_&j;
							drop COUNT &byvar;
							PCT_&&val&j. = input(put(PERCENT,5.2),5.2); 
							drop PERCENT;
						 run;
				         proc sort data=val_&j; by &&var&i; run;
					 %end;
				 %end;
				 data byfreqs_&i;
				 	merge val_:;
					by &&var&i.;
				 run;

				 data byfreqs_&i (drop=&&var&i VALUE rename=(VALUE2=VALUE));
				 	length VARIABLE $32 VALUE VALUE2 $200;
				 	set byfreqs_&i;
					VALUE = trim(left(put(&&var&i.,20.)));
					if _n_=1 then do;
						VARIABLE = "&&var&i";
						Significance="&chisq.";
					end;
					%if "&formats."="Y" %then %do;
						format VALUE &&fmt&i...;
					%end;
					VALUE2=VVALUE(VALUE); /*This is to get around the issue of formatting the same variable name with diff formats*/
				 run;
		     %end;
	      %end;

		  /*Output the dataset with the freqs and append them together into a master dataset*/
		  data bycategorical;
		    set byfreqs_:;
		  run;
		
		  title "DISTRIBUTION OF VALUES FOR EACH CATEGORICAL VARIABLE BY &byvar.";
		  proc print data=bycategorical; 
	 	  run;
		  title;
	   %end;
%mend;
*%chisq(ds=&ds.,cutoff=&cutoff.,byvar=&byvar.,subset=&subset.,formats=&formats.);
