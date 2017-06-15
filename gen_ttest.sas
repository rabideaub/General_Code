/*This macro performs a ttest for each numeric variable in a dataset and aggregates
  the results in a clean, formatted table called summary_ttest*/

options compress=yes mprint;

%macro gen_ttest(ds,byvar);
	/*************************************************************
	 IDENTIFY THE VARIABLES IN THE DATASET AND STORE THE 
	 APPROPRIATE MACROS
	*************************************************************/
	proc contents data=&ds. out=contents; run;
	proc print data=contents; run;

	data _null_;
		set contents end=last;
		 retain count;
		 if trim(left(name))="&byvar." then call symput('bytype',trim(left(type)));
	 	 call symput('var'||trim(left(_n_)),trim(left(name)));
		 call symput('type'||trim(left(_n_)),trim(left(type)));
		 call symput('label'||trim(left(_n_)),trim(left(label)));
		 if _n_=1 then count=0;
		 count+1;
		 if last then call symput('count',count);
	run;

	proc freq data=&ds.;
		tables &byvar. / out=by_values;
	run;

	proc print data=by_values; run;

	data _null_;
		set by_values end=eof;
		if _n_=1 then call symput('first',trim(left(&byvar.)));
		if eof then call symput('last',trim(left(&byvar.)));
		if ((_n_>1 & ~eof) | (_n_=1 & eof)) then putlog "ERROR: &byvar. HAS MORE THAN 2 LEVELS";
	run;

	%put &first. &last. &bytype. "&first." "&last." "&bytype.";

	/*************************************************************
	 RUN THE TTEST AND FORMAT THE OUTPUT
	*************************************************************/
	%macro ttest(var,classvar,n,label);
		ods output  "Statistics" = stats
		 			"T-Tests" = ttests;
		proc ttest data=&ds.;
			class &classvar.;
			var &var.;
		run;
		ods output close;

		proc print data=stats; run;

		data stats (keep = variable class first last);
			set stats(rename=(mean=avg));
			/*For a character byvar*/
			%if "&bytype."="2" %then %do;
				if trim(left(class)) = "&first." then first = avg;
				if trim(left(class)) = "&last." then last = avg;
			%end;

			/*For a numeric byvar*/
			%else %if "&bytype."="1" %then %do;
				if class=&first. then first = avg;
				if class=&last. then last = avg;
			%end;
		run;

		/*Split the 2 levels of &byvar into 2 datasets, dropping the non-matching one for each dataset*/
		 data &classvar._1 (drop =class last)
			  &classvar._2 (drop = class first);
		 	set stats;
			%if "&bytype."="2" %then %do;
			 	if trim(left(class)) = "&first." then output &classvar._1;
			 	if trim(left(class)) = "&last." then output &classvar._2;
			%end;

			%else %if "&bytype."="1" %then %do;
				if class=&first. then output &classvar._1;
				if class=&last. then output &classvar._2;
			%end;
		 run;

		 proc sort data = &classvar._1; by variable; run;
		 proc sort data = &classvar._2; by variable; run;

		 data stats_final;
		 	merge &classvar._1
			   	  &classvar._2;
		 	by variable;
		 run;

		 /*Add on the p-values*/
		 data ttest (keep=variable probt rename=(probt=Significance));
		 	set ttests;
			if trim(left(Method))="Pooled";
		run;

		proc print data=ttest; run;
		proc sort data=ttest; by variable; run;

		/*Finalize the dataset for this particular variable - add label if appropriate*/
		data ttest_&n.;
			length variable $ 50 label $100;
			merge stats_final
				  ttest;
			by variable;
			variable="&var.";
			label="&label.";
			label first = "&classvar. = &first.";
			label last = "&classvar. = &last.";
		run;

		proc print data=ttest_&n. label noobs; run;
	%mend;

	/*************************************************************
	 LOOP THROUGH ALL APPROPRIATE VARIABLES USING MACROS AS 
	 PARAMETERS
	*************************************************************/
	%macro all_vars(by);
		%do i=1 %to &count.;
			%if "&&var&i."~="&by." & "&&type&i."="1" %then %do; /*If the variable is not the class variable and it is numeric, do a ttest*/
				%ttest(var=&&var&i.,classvar=&by.,n=&i.,label=&&label&i.);
			%end;
		%end;
	%mend;
	%all_vars(by=&byvar.);

	/*************************************************************
	 APPEND ALL THE OUTPUT DATASETS AND OUTPUT
	*************************************************************/
	data summary_ttest;
		set ttest_:;
	run;

	proc print data=summary_ttest label noobs; run;
%mend;

libname dat "/schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Data";

data test;
	set dat.lejr_hosp_geo;
run;

*%gen_ttest(ds=test,byvar=in_cjr);
