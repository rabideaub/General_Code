libname in "/schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Data";

data test;
	set in.lejr_hosp;
run;

proc print data=test (obs=20); run;

ods output ParameterEstimates = parm
		   FitStatistics=fit
		   ModelAnova=anova
		   Nobs=n;
proc glm data=test;
	model total_radm30 = for_profit in_cjr for_profit*in_cjr; 
run;
ods output close;

proc print data=parm; run;
proc print data=fit; run;
proc print data=anova; run;
proc print data=n; run;
proc contents data=n; run;
proc contents data=parm; run;

data n;
	set n (where=(label="Number of Observations Used"));
run;

data parm;
	length Outcome $32 N 8;
	merge parm (keep=Dependent parameter estimate stderr probt rename=(Dependent=Outcome parameter=Covariate Estimate=Coefficient Probt=Significance))
		  n (keep=N);
run;

proc print data=parm; run;

proc means data=test;
	class for_profit in_cjr;
	var total_radm30;
	output out=test_means;
run;

data test_means;
	set test_means;
	if _TYPE_=3 & trim(left(_STAT_))="MEAN";
run;

proc print data=test_means; run;

data treat (keep=in_cjr total_radm30 rename=(total_radm30=treat_means))
	 control (keep=in_cjr total_radm30 rename=(total_radm30=control_means));
	set test_means;
	if for_profit=1 then output treat;
	if for_profit=0 then output control;
run;

proc sort data=treat; by in_cjr; run;
proc sort data=control; by in_cjr; run;

data table_means;
	merge treat
		  control;
	by in_cjr;
run;

proc print data=table_means; run;
