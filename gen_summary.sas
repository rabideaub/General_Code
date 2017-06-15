/************************************************************************************************************
************************************************************************************************************
Program: gen_summary
Created By: Brendan Rabideau
Created Date: 8/7/15
Updated Date: 9/17/15
Purpose: This program creates a data-driven summary of the input dataset. Output includes:
		 1) Contents of the dataset
		 2) List of variables ineligible for summarization (ID vars, categorical variables with too many levels, etc)
		 3) Overall categorical distributions for all eligible variables
		 4) N MIN MAX MEDIAN MEAN NMISS for all numeric variables
		 5) Categorical distribution crosstab for all eligible variables by selected &byvar (by year, by treatment, etc)
		 6) Means for all numeric variables by selected &byvar

Notes: -Data driven means the program evaluates which variables are appropriate for each output - no user input required
	   -Customizable parameters (see below)
	   -Option to subset dataset which are very large to speed up processing (see below)
	
	   To Run: 1) Set a libname statement and read a dataset into the SAS 'work' library
			   2) Update the parameters in the macro invokation:
					a)ds     = Required: Name of the dataset in the work library to be summarized (e.g. med2001)
					b)cutoff = Required: Maximum levels of a variable to be considered categorical (e.g. gender=1 or 2 has 2 levels)
					c)byvar  = Optional: By variable used to stratify summarizations (e.g. cost by year, race by year, etc.)
					d)subset = Optional: Number of randomly selected observations to be used in summarization. Useful for
							   large datasets (1,000,000+) to cut down on lengthy processing time. Suggested value = 100,000
			   3) Run

Updates: 8/12/15 - Updated documentation and formatting. BR
		 9/17/15 - Updated to include optional value formats
************************************************************************************************************
************************************************************************************************************/

options compress=yes nofmterr mprint mtrace mlogic minoperator symbolgen formchar="|----|+|---+=|-/\<>*";

/*************************************
Set Macros
*************************************/

libname dat "/schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Data/HRRP/Final";
libname fmt "/disk/agedisk3/medicare.work/goldman-DUA25731/rabideau/Documentation";
%let out = /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Output;
%let ds = freq_array_pn;
%let cutoff = 60;
%let byvar =year;
%let subset = 100000;
%let formats = N;

/*Read desired data into a work dataset*/

 data &ds.;
 	set dat.freq_array_pn;
 run;
 
/********************************************************************************************
No Modification is Needed Beyond This Point
********************************************************************************************/
%macro summary (ds,cutoff,byvar,subset,formats);
	/*Randomly select &subset number of observations from the dataset. Useful for large datasets since
      most of the output is size invariant (e.g. percents and means) and the program processes slowly*/
	%if &subset>0 & &subset~=. %then %do;
		PROC SURVEYSELECT DATA=&ds OUT=&ds METHOD=SRS
			SAMPSIZE=&subset SEED=1234567;
		RUN;
	%end;

	/*Determine number of observations in the dataset*/
	data _null_;
		set &ds. end=last;
		if last then call symput('tot_obs',trim(left(_n_)));
	run;

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
			set fmt.nber_value_labels (where=(lowcase(Variable)="&&var&i."));
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

	 /*Run frequencies for each variable within the cutoff range*/
	 %do i=1 %to &count;
	     %if &&flag&i=1 %then %do;
			 proc freq data=&ds order=freq noprint;
			 	table &&var&i / missing out=freqs_&&var&i;
			 run;
			 
			 data freqs_&&var&i (keep=VARIABLE VALUE2 COUNT2 PERCENT2 rename=(VALUE2=VALUE COUNT2=COUNT PERCENT2=PERCENT));
			 	length VARIABLE $20 VALUE VALUE2 $200 PERCENT2 $8;
			 	set freqs_&&var&i;
				VALUE = trim(left(put(&&var&i.,20.)));
				if _n_=1 then VARIABLE = "&&var&i";
				%if "&formats."="Y" %then %do;
					format VALUE &&fmt&i...;
				%end;
				VALUE2=VVALUE(VALUE); /*This is to get around the issue of formatting the same variable name with diff formats*/
				COUNT2=put(COUNT,5.2);
				PERCENT2=put(PERCENT,5.2);
				if COUNT<11 then do;
					COUNT2="Censored";
					PERCENT2="Censored";
				end;
			 run;
	     %end;
     %end;

	 /*Output the dataset with the freqs and append them together into a master dataset*/
	 data categorical;
	 	set freqs_:;
	 run;
	
	 title "DISTRIBUTION OF VALUES FOR EACH CATEGORICAL VARIABLE";
	 proc print data=categorical; 
	 run;
	 title;

	/**********************************************************
	Get summary stats for each numeric variable
	**********************************************************/
	 title "SUMMAY STATS FOR EACH NUMERIC VARIABLE";
 	 proc means data=&ds N MIN MAX MEDIAN MEAN NMISS MAXDEC=3 nolabels;
 		var _NUMERIC_;
		output out=means;
 	 run;
	 title;

	 proc transpose data=means out=means name=Variable;
	 	id _STAT_;
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
						 proc freq data=&ds order=freq noprint;
						 	table &&var&i*&byvar / missing norow nocum nofreq out=val_&&val&j;
							/*Account for numeric or character by-variable*/
							%if "&vartype"="N" %then %do;
								where &byvar. = &&val&j;
							%end;
							%else %do;
								where &byvar. = "&&val&j";
							%end;
						 run;

						 data val_&&val&j;
						 	length PCT_&&val&j. $8;
							set val_&&val&j;
							drop COUNT &byvar;
							PCT_&&val&j. = put(PERCENT,5.2); 
							if (11/&tot_obs.)*100 >= PERCENT then PCT_&&val&j.="Censored";
							drop PERCENT;
						 run;
				         proc sort data=val_&&val&j; by &&var&i; run;
					 %end;
				 %end;
				 data byfreqs_&&var&i;
				 	merge val_:;
					by &&var&i.;
				 run;

				 data byfreqs_&&var&i (drop=&&var&i VALUE rename=(VALUE2=VALUE));
				 	length VARIABLE $20 VALUE VALUE2 $200;
				 	set byfreqs_&&var&i;
					VALUE = trim(left(put(&&var&i.,20.)));
					if _n_=1 then VARIABLE = "&&var&i";
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


		 /**********************************************************
		 Get summary stats for each numeric variable by byvar
		 **********************************************************/
 	 	 %do k=1 %to &num.;
		 	%if "&&val&k" ~=" " & "&&val&k" ~= "." %then %do;
				proc means data=&ds MEAN MAXDEC=3 nolabels noprint;
					var _NUMERIC_;
					/*Account for numeric or character by-variable*/
					%if "&vartype"="N" %then %do;
						where &byvar. = &&val&k;
					%end;
					%else %do;
						where &byvar. = "&&val&k";
					%end;
					output out=means_&&val&k mean= /noinherit;
				run;
				/*data means_&&val&k;
					set means_&&val&k;
					rename Means=Mean_&&val&k;
				run;*/
				
				proc transpose data=means_&&val&k out=means_&&val&k;
				run; 

				data means_&&val&k;
					set means_&&val&k;
					rename _NAME_=Variable
						   COL1=Mean_&&val&k;
				run;
				
				proc sort data=means_&&val&k; by Variable; run;
			%end;
		 %end;

		 data bymeans;
			merge means_:;
			by Variable;
		 run;
		  
		 title "MEAN VALUE FOR EACH NUMERIC VARIABLE BY &byvar.";
		 proc print data=bymeans;
		 run;
		 title;
     %end;

	/*********************
	PRINT CHECK
	*********************/
	/*Output a sample of each of the datasets to an excel workbook*/
	ods tagsets.excelxp file="&out./summary_&ds..xml" style=sansPrinter;

	ods tagsets.excelxp options(absolute_column_width='20' sheet_name="All Categorical" frozen_headers='yes');
	proc print data=categorical;
	run;

	ods tagsets.excelxp options(absolute_column_width='20' sheet_name="All Numeric" frozen_headers='yes');
	proc print data=means;
	run;

	ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Categorical by &byvar" frozen_headers='yes');
	proc print data=bycategorical;
	run;

	ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Numeric by &byvar" frozen_headers='yes');
	proc print data=bymeans;
	run;

	ods tagsets.excelxp close;
	/*********************
	CHECK END
	*********************/


	proc datasets library=work kill;
	 run;
	quit;
 %mend summary; 
%summary(ds=&ds.,cutoff=&cutoff.,byvar=&byvar.,subset=&subset.,formats=&formats.);


 
