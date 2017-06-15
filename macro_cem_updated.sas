 /*************************************************************************************/
 /*   %CEM SAS Macro: A SAS Macro to perform Coarsened Exact Matching 				  */
 /*                                                              				      */
 /*   NAME: CEM	                                             	 				      */
 /*   TITLE: Macro for Coarsened Exact Matching		        		     		      */
 /*   PRODUCT: STAT                                 		    		 		      */
 /*   SYSTEM: ALL                          			                      		      */
 /*   KEYS: Coarsened Exact Matching, Causal inference, SAS, Matching frontier        */
 /*   AUTHOR: Stefano Verzillo - Paolo Berta - Matteo Bossi                           */
 /*************************************************************************************/

/*-------------------------------------------------------------------------------------
 TITLE
 -----
 CEM: a SAS macro to perform Coarsened Exact Matching
 Requires SAS/STAT.
 Citation: Verzillo, S., Berta, P., & Bossi, M. (2013). '% CEM: A SAS Macro to perform Coarsened Exact Matching', 
 BePress-UNIMI - Research Papers in Economics, Business, and Statistics. Statistics and Mathematics. Working Paper 60;

 SUPPORT
 -------
 The authors are available for support %CEM Macro users.
 Please send emails to paolo.berta@unimib.it with any suggestion or correction.

 HISTORY
 -------
 initial coding                                               		01Sept2013 VBeBo
 a completely revised version of L1 nested macro            		15August2014 VBeBo
 various changes and addiction                                 		01May2015 VBeBo
 checking association between treatment and categorical vars        30May2015 VBeBo
 various changes and addiction 				        30Oct2015 VBeBo

 DESCRIPTION
 -----------
 This macro introduce %CEM, a macro package allowing researchers
 to automatically perform Coarsened Exact Matching (CEM) in the SAS environment.
 CEM is a non-parametric matching method mainly used by researchers
 to avoid the confounding influence of pre-treatment control variables
 improving causal inference in quasi experimental studies. %CEM
 introduces a completely automated process which allows SAS users to
 efficiently perform Coarsened Exact Matching in the fields where huge
 dataset are common and where SAS is the most popular analytics software.
 Moreover a macro option could be specified to test several binning
 combinations of continuous variables. This option provides a visual
 representation of the matching frontier to select the preferred coarsening
 according to the L1 balance measure and the percentage of matched
 units. 
 

 The relevant information is printed at the end of the code.

 USAGE NOTES
 -----------
 For ease of use you should change this file's extension from .txt to .sas, copy the obtained file to your preferred folder 
 and then submit the following code before calling the  %CEM macro: 

 options source2;
 %include "c:/yourfolder_path/Macro_CEM.sas"; 

 
 Note that it's possible to receive unanticipated results in case of using the macro multiple times in a single SAS session because the matched dummy and the weights variable 
 created in one macro may be carried over to the subsequent macro used in the same session. Drop these variables in your dataset before running %CEM again.
 Moreover it's preferable to not store your original dataset in the work library but in another one.
 Finally to improve computational efficiency we suggest to include dummy variables as categorical variables.
 Remember that "Percent" option allows you to perform multiple binning combinations of numerical variables, improving the opportunity to find a more balanced solution. 
 However execution time increases exponentially with the number of numerical variables.

 DISCLAIMER
 ----------
 The %CEM SAS macro was written and is maintained by the authors. It contains SAS source code, a brief description of the macro's function and an example of the macro call. 
 Cem is licensed under GPL2. For more information, see: http://gking.harvard.edu/cem/. These macro is distributed with helpful intent but without any warranty; without even the 
 implied warranty of merchantability or fitness for a particular purpose. Use at your own risk. If you use it please acknowledge the original contributors of the material.

 

 LIST OF PARAMETERS
 ------------------
 
 %CEM (
	lib = , 
	data = , 
	id = , 
	treat = , 
	del_miss = ,
 	match_type = , 
	method = , 
	path_graph = ,
	report = 
      );

where:

 * lib: SAS library containing the original dataset;

 * data: name of the dataset to be read. 
	It must be organized with one row for each observation to be matched (individuals or firms), 
	K observed continuous and/or categorical covariates, a treatment indicator variable and the ID primary-key;

 * id: ID primary-key;

 * treat: name of the treatment variable, it has to be numerical with two different values;

 * del_miss: option for missing values: 0 to keep them as additional categories or 1 to delete them before matching (%CEM modifies the original dataset deleting the
			records with missing values and stores a copy of the original dataset in the LIB library;

 * match_type: ONE (k:k matching) or N (1:n matching with associated strata weights);

 * method: default is "Sturges", that perform the Sturges rule as binning algorithm of numeric variables in order to create Cem strata.
   The alternative is "Percent", that perform a set of automatic coarsening cutpoints of numeric variables (quartiles, quintiles, deciles, centiles and   percentiles);

 * path_graph: option to assign the path where %CEM stores the matching frontier;

 * report: select "on" to printout a final report of selected tables (matching statistics and L_1 index pre and post matching); 



 SYNTAX
 ------
 */

%macro CEM (
			lib = work,
			data = input,
			ID = id ,
			treat = treat,
			DEL_miss = 0,
			MATCH_TYPE = N,
			method = Sturges,
			path_graph = path_graph,
			report = on
			);

data L1_TOT;
winchester=1;
run;


proc freq data=&lib..&data.;
table &treat /OUT = COUNT_TREAT_UNT;
run;


%if "&method" ^= "Sturges" %then %do;  /*if the binning option is Percent then skip*/


%CEM (
lib = &lib,
data = &data,
ID = &ID,
treat = &treat,
DEL_miss = &DEL_miss,
MATCH_TYPE = &MATCH_TYPE,
method = Sturges,
path_graph = &path_graph,
report = off
);


data &lib..&data.;
	set &lib..&data.;
	drop Wsc1_stur matched;
run;
%end;


%if "&method" ^= "Sturges" %then %do;  /*if the binning option is Percent then skip*/
%let vvv1 = 4;
%let vvv2 = 5;
%let vvv3 = 10;
%let vvv4 = 100;
data strata;
format strata $56.;
%do HHH = 1 %to 4;
strata = STRIP("&&vvv&HHH.");
output;
%end;
run;
%end;

%if "&method" = "Sturges" %then %do;  /*if the binning option is Percent then do*/
%let vvv1 = Stur;
data strata;
format strata $56.;
%do HHH = 1 %to 1;
strata = STRIP("&&vvv&HHH.");
output;
%end;
run;
%end;


/*****************************************/
/* RECODING OF INPUT VARIABLE            */
/*****************************************/
proc datasets; contents data= &lib..&data. out= Name_D (keep = NAME TYPE);run;
/*****************************************/
/* CHECK ON TREAT VARIABLE               */
/*****************************************/
data _NULL_;
set Name_D;
if upcase(NAME) = upcase("&Treat.") then call symput ("type_check",type);
run;
%if &type_check. = 2 %then %do;
%put ERROR: TREATMENT VARIABLE HAS TO BE NUMERICAL;
%ABORT;
%end;
proc sort data=&&lib..&data. (keep = &Treat.) out=check_value nodupkey; by &Treat.; run;
data check_value;
set check_value;
rename &Treat. = value;
num = _N_;
run;
proc sql;
select max(num), max(value), min(value) into :observation, :Max_1, :Min_1
from check_value;
quit;
%if &observation. ne 2 %then %do;
%put ERROR: TREATMENT VARIABLE REQUIRES ONLY TWO DIFFERENT VALUES;
%ABORT;
%end;
%if &max_1. ne 1 AND &min_1 ne 0 %then %do;
data &&lib..&data.;
set &&lib..&data.;
if &Treat. = &Max_1. then &Treat = 1;
else &Treat = 0;
run;
%put WARNING: TREAT VARIABLE IS RECODED WITH DEFAULT VALUES 0 (MIN VALUE) AND 1 (MAX VALUE);
%end;
proc sort data=&lib..&data. out=control nodupkey; by &id; run;
data _NULL_;
set &lib..&data.;
call symput ('n_origine', _N_);
run;
data _NULL_;
set control;
call symput ('n_nodupkey', _N_);
run;
%IF %eval(&n_origine.) = %eval(&n_nodupkey.) %THEN %DO;
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* RECODING OF INPUT VARIABLES*/
/*&&&&&&&&&&&&&&&&&&&&&&&&&&&&*/
proc datasets ;
contents data= &lib..&data. out=Name_D(keep=NAME TYPE);
run;
data Name_D_cat Name_D_num;
set Name_D;
if upcase(NAME) = upcase("&ID.") then delete;
if upcase(NAME) = upcase("&TREAT.") then delete;
if TYPE=1 then output Name_D_num;
if TYPE=2 then output Name_D_cat;
run;
DATA TRANSCOD_CAT;
SET Name_D_CAT;
FORMAT TRANS_CAT $4.;
TRANS_CAT = COMPRESS("C_"||_N_);
RUN;
DATA TRANSCOD_NUM;
SET Name_D_NUM;
FORMAT TRANS_NUM $4.;
TRANS_NUM = COMPRESS("V_"||_N_);
RUN;

/******************************************************************************************/
/* CHECKING IF CATEGORICAL VARIABLES ARE EXACTLY ASSOCIATED WITH TREATMENT 				  */
/******************************************************************************************/

%let opcat = %sysfunc(open(Name_D_CAT,i));
%let num_cat = %sysfunc(attrn(&opcat.,nobs));
%let clcat = %sysfunc(close(&opcat.));
%if %eval(&num_cat.)>0 %then %do;

data _NULL_;
	set name_d_cat;
	call symput ("check", _N_);
	call symput (compress("namecat"||_N_), trim(left(NAME)));
run;

%DO v = 1 %to &num_cat.;
proc freq data = &&lib..&data.; table &&namecat&v. * &treat. /out=CheckCat; run;
proc transpose data=CheckCat out=CheckCat1 (drop = _name_ _label_) prefix=Tr;
by &&namecat&v.;
id &treat.;
var COUNT;
run;
data CheckCat2;
	set CheckCat1;
	somma = Tr0 + Tr1;
run; 
proc means data=CheckCat2 sum noprint nway;
output out=CheckCat3 (drop = _type_ _freq_) sum(somma)=somma1;
run;
data _NULL_;
	set CheckCat3;
	call symput ('som', somma1);
run;
%if &som. <=0 %then %do;
%put ERROR: YOU HAVE INSERT A CATEGORICAL VARIABLE EXACTLY ASSOCIATED WITH THE TREATMENT VARIABLE!;
%ABORT;
%end;
%end;
%end;


/******************************************************************************************/
/* DELETING MISSING VALUES WHEN THE MACRO PARAMETER DEL_MISS IS ON		 				  */
/******************************************************************************************/

%if &del_miss. = 1 %then %do;
/*CREATE A COPY OF THE ORIGINAL DATASET. 
THE DATASET SUBMITTED TO THE MACRO WILL BE REDUCED DELETING ALL MISSING RECORDS*/
data &&lib..&data._Original;
	set &&lib..&data.;
run;

/*CATEGORICAL MISSING VALUES*/
%let opcat = %sysfunc(open(Name_D_CAT,i));
%let num_cat = %sysfunc(attrn(&opcat.,nobs));
%let clcat = %sysfunc(close(&opcat.));
%if %eval(&num_cat.)>0 %then %do;
data _NULL_;
	set name_d_cat;
	call symput ("check", _N_);
	call symput (compress("namecat"||_N_), trim(left(NAME)));
run;

data &&lib..&data.; 
	set &&lib..&data.; 
	%do v1 = 1 %to &num_cat.;
	if &&namecat&v1. = "" then delete;
	%end;
run;
%end;

/*NUMERICAL MISSING VALUES*/
%let opNUM = %sysfunc(open(Name_D_NUM,i));
%let num_NUM = %sysfunc(attrn(&opNUM.,nobs));
%let clNUM = %sysfunc(close(&opNUM.));
%if %eval(&num_NUM.)>0 %then %do;
data _NULL_;
	set name_d_NUM;
	call symput ("check", _N_);
	call symput (compress("nameNUM"||_N_), trim(left(NAME)));
run;
data &&lib..&data.; 
	set &&lib..&data.; 
	%do w1 = 1 %to &num_NUM.;
	if &&nameNUM&w1. = . then delete;
	%end;
run;
%end;
%end;

/********************************************************************************/
/* L1 - MACRO																	*/
/********************************************************************************/
%L1 (CODICE = 0,
ID1 = &id,
TREAT1 = &Treat,
COD_DATASET=&lib..&data.);
data L1_TOT;
format codice $20.;
set L1_TOT L1_0;
run;
/********************************************************************************/
/********************************************************************************/

%let opnum = %sysfunc(open(Name_D_num,i));
%let numtot = %sysfunc(attrn(&opnum.,nobs));
%let cltranscat = %sysfunc(close(&opnum.));
%if %eval(&numtot.)>0 %then %do;
/*ONLY NUM*/
data _NULL_;
set Name_D_num;
call symput ('numtot', _N_);
call symput (compress('var_'||_N_),trim(left(NAME)));
run;
%end;
%put ************************************************************************************************;
%put &numtot. number of numeric variables considered for stratification and matching 		     	 ;
%put ************************************************************************************************;

%put ************************************************************************************************;
%put method is &method										    									 ;
%put ************************************************************************************************;

%if "&method" ^= "Sturges" %then %do;  /*if the binning option is Percent then do*/

%put ************************************************************************************************;
%put the binning option is Percent instead of Sturges!						   						 ;
%put ************************************************************************************************;

%if &numtot. ne 0 %then %do;
proc rank data=&lib..&data. out=ranked4 (keep = %do i = 1 %to &numtot;
&&var_&i
%end;
%do i = 1 %to &numtot;
v_&i._4
%end;
&Treat
&ID)
groups=4;
var
%do i = 1 %to &numtot;
&&var_&i
%end;
;
ranks
%do i = 1 %to &numtot;
V_&i._4
%end;
;
run;
proc rank data=&lib..&data.
out=ranked5 (keep = %do i = 1 %to &numtot;
&&var_&i
%end;
%do i = 1 %to &numtot;
v_&i._5
%end;
&ID
&Treat)
groups=5;
var
%do i = 1 %to &numtot;
&&var_&i
%end;
;
ranks
%do i = 1 %to &numtot;
V_&i._5
%end;
;
run;
proc rank data=&lib..&data.
out=ranked10 (keep = %do i = 1 %to &numtot;
&&var_&i
%end;
%do i = 1 %to &numtot;
v_&i._10
%end;
&ID
&Treat)
groups=10;
var
%do i = 1 %to &numtot;
&&var_&i
%end;
;
ranks
%do i = 1 %to &numtot;
V_&i._10
%end;
;
run;
proc rank data=&lib..&data.
out=ranked100 (keep = %do i = 1 %to &numtot;
&&var_&i
%end;
%do i = 1 %to &numtot;
v_&i._100
%end;
&ID
&Treat)
groups=100;
var
%do i = 1 %to &numtot;
&&var_&i
%end;
;
ranks
%do i = 1 %to &numtot;
V_&i._100
%end;
;
run;
PROC SORT DATA=RANKED4; BY &ID; RUN;
PROC SORT DATA=RANKED5; BY &ID; RUN;
PROC SORT DATA=RANKED10; BY &ID; RUN;
PROC SORT DATA=RANKED100; BY &ID; RUN;
DATA rec_v_num ;
MERGE RANKED4 RANKED5 RANKED10 RANKED100;
BY &ID;
RUN;
%end;
%end;

%if "&method" = "Sturges" %then %do;

%put ************************************************************************************************;
%put the default binning option is Sturges!							     ;
%put ************************************************************************************************;

data _NULL_;
	set &lib..&data.;
	call symput ('n', _N_);
run;

%let Z=%sysevalf(1+%sysfunc(log2(%eval(&n))), floor); /*Z is the rounding of Sturges*/
%let Z1=%sysevalf(1+%sysfunc(log2(%eval(&n))), floor); /*Z1 is the rounding of Sturges*/

/*Calculate for each Numerical Variable StdDev Min and Max*/
%do b = 1 %to &numtot;
	proc means data=&lib..&data. (keep = &&var_&b) stddev min max noprint nway;
		output out=stock_&&var_&b (drop = _type_ _freq_)  min(&&var_&b)=min_&&var_&b
														  max(&&var_&b)=max_&&var_&b;
	run;
	/*Stock them*/
	data _NULL_;
		set stock_&&var_&b;
		call symput ("min_&&var_&b", trim(left(min_&&var_&b)));
		call symput ("max_&&var_&b", trim(left(max_&&var_&b)));
	run;

%let W_&&var_&b = %sysevalf( ((&&&&max_&&var_&b - &&&&min_&&var_&b)  / &Z. )); /*Width of each bin*/
/*%let W1_&&var_&b = %sysfunc( (round(&&&&W_&&var_&v, .01) ));*/

%end;

/*Create New Variables based on the binning above*/
data rec_v_num (keep = %do i = 1 %to &numtot;
&&var_&i
%end;
%do i = 1 %to &numtot;
v_&i._Stur
%end;
&ID
&Treat);
	set &lib..&data.;
	%do v = 1 %to &numtot;
		if &&var_&v >= %sysevalf(&&&&min_&&var_&v) AND &&var_&v <= %sysevalf((&&&&min_&&var_&v + &&&&W_&&var_&v)) then V_&v._Stur = 1;
		%do s = 2 %to &Z;
		if &&var_&v > %sysevalf( (&&&&min_&&var_&v + %sysevalf((&s-1)* &&&&W_&&var_&v))) AND &&var_&v <= %sysevalf( (&&&&min_&&var_&v + %sysevalf((&s)* &&&&W_&&var_&v))) then V_&v._Stur = &s;
		/*if &&var_&v >= %sysfunc(round( (&&&&min_&&var_&v + %sysevalf((&s-1)* &&&&W_&&var_&v)) ).1 ) then V_&v._Stur = &s;*/
	%end;
		%end;
run;

%end;


/*ONLY CAT*/
%let dsidtot1 = %sysfunc(open(Name_D_cat,i));
%let numtot1 = %sysfunc(attrn(&dsidtot1.,nobs));
%let rktot1 = %sysfunc(close(&dsidtot1.));
%if &numtot1. ne 0 %then %do;
%do r=1 %to &numtot1.;
%let dsid2 = %sysfunc(open(Name_D_cat,i));
%let varnum2 = %sysfunc(varnum(&dsid2.,NAME));
%let rc2 = %sysfunc(fetchobs(&dsid2.,&r.));
%let label2 = %sysfunc(getvarc(&dsid2.,&varnum2.));
%let rk2 = %sysfunc(close(&dsid2.));
proc freq data=&lib..&data. noprint;
tables &label2. /out=V_CAT_&r.(DROP=COUNT PERCENT);
run;
quit;
data V_CAT_&r.;
set V_CAT_&r.;
format c_&r $32.;
if &label2. = "" then delete;
run;
data V_CAT_&r.;
set V_CAT_&r.;
format c_&r $32.;
c_&r = left(_n_);
run;
%if &DEL_miss. =0 %then %do;
data V_CAT_&r._app;
format c_&r $32.;
&label2. ="";
c_&r ="MISS" ;
run;
data V_CAT_&r.;
format c_&r $32. &label2. $32.;
set V_CAT_&r._app V_CAT_&r.;
run;
%end;
proc sort data=V_CAT_&r.;
by &label2.;
run;
%let opnum1 = %sysfunc(open(TRANSCOD_NUM,i));
%let k = %sysfunc(attrn(&opnum1.,nobs));
%let clnum = %sysfunc(close(&opnum1.));
%if &k.>0 %then %do;
data _NULL_;
set TRANSCOD_NUM;
call symput ('k',_N_);
call symput (compress('v'||_N_),trim(left(name)));
run;
%end;
%if &r.=1 %then %do;
proc sort data=&lib..&data;
by &label2.;
run;
%if &k.>0 %then %do;
data REC_V_CAT
(drop = %do k = 1 %to &k;
&&v&k
%end;
)
;
merge V_CAT_&r. (in=a) &lib..&data. (in=b);
by &label2.;
if a and b;
run;
%end;
%else %do;
data REC_V_CAT ;
merge V_CAT_&r. (in=a) &lib..&data. (in=b);
by &label2.;
if a and b;
run;
%end;
%end;
%if &r.>=2 %then %do;
proc sort data=REC_V_CAT;
by &label2.;
run;
data REC_V_CAT;
merge V_CAT_&r.(in=a) REC_V_CAT (in=b);
by &label2.;
if a and b;
run;
%end;
%end;
%end;
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* updating zero cat or num */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
%let dsid_100 = %sysfunc(open(rec_v_cat ,i));
%let rk_100 = %sysfunc(close(&dsid_100.));
%let dsid_101 = %sysfunc(open(rec_v_num ,i));
%let rk_101 = %sysfunc(close(&dsid_101.));
%if &dsid_100. gt 0 and &dsid_101. gt 0 %then %do;
PROC SQL;
CREATE TABLE REC_DATA AS SELECT 
* 
FROM rec_v_cat AS A
inner join rec_v_num AS B
ON A.&id. = B.&id.;
Quit;
%end;
%else %if &dsid_100. = 0 and &dsid_101. gt 0 %then %do;
PROC
SQL;
CREATE TABLE REC_DATA AS SELECT
*
FROM rec_v_num
;
Quit;
%end;
%else %if &dsid_100. gt 0 and &dsid_101. = 0 %then %do;
PROC SQL;
CREATE TABLE REC_DATA AS SELECT
*
FROM rec_v_cat
;
Quit;
%end;
%let dsidtot2 = %sysfunc(open(Name_D_num,i));
%let numtot2 = %sysfunc(attrn(&dsidtot2.,nobs));
%let rktot2 = %sysfunc(close(&dsidtot2.));
%let dsidtot3 = %sysfunc(open(Name_D_cat,i));
%let numtot3 = %sysfunc(attrn(&dsidtot3.,nobs));
%let rktot3 = %sysfunc(close(&dsidtot3.));
%put ***********************************************************************************************;
%put &numtot2. number of numeric variables considered for stratification and matching 		    ;
%put ***********************************************************************************************;
%put ***********************************************************************************************;
%put &numtot3. number of categorical variables considered for stratification and matching           ;
%put ***********************************************************************************************;
%if %eval(&numtot2.)>0 %then %do;
%do i=1 %to &numtot2. ;
data bin_&i;
format strata_&i $350.;
set strata (rename=(strata=strata_&i));
strata_&i=compress("V_&i._"||strata_&i);
run;
%end;
%do i=1 %to %eval(&numtot2.-1) ;
%let j = &numtot2.;
proc sql;
create table strata&i as select 
bin_&i..strata_&i
,bin_&j..strata_&j
from bin_&i
,bin_&j;
quit;
data bin_&j;
set strata&i;
format strata_&j $350.;
strata_&j=compress(strata_&i||"||"||strata_&j);
drop strata_&i;
run;
%end;
%end;
data _NULL_;
set TRANSCOD_CAT;
call symput ('ncat', _N_);
call symput (compress('cat'||_N_), trim(left(trans_cat)));
run;
%if %eval(&numtot3.)>0 AND %eval(&numtot2.)>0 %then %do;
data Trans_cod;
set bin_&numtot2.;
codcat = compress(
%do c = 1 %to &ncat;
"&&cat&c" || "||" ||
%end;
" ");
strata_finale = compress(codcat||strata_&numtot2.);
run;
%end;
%if %eval(&numtot3.)>0 AND %eval(&numtot2.)=0 %then %do;
data Trans_cod;
%if %eval(&numtot3.)=1 %then %do;
strata_finale = compress(
%do c = 1 %to &ncat;
"&&cat&c"
%end;
);
%end;
%if %eval(&numtot3.)>1 %then %do;
strata_finale = compress(
%do c = 1 %to &ncat;
"&&cat&c"||"||"||
%end;
"");
%end;
run;
%end;
%if %eval(&numtot3.)=0 AND %eval(&numtot2.)>0 %then %do;
data Trans_cod;
set bin_&numtot2.;
strata_finale = compress(strata_&numtot2.);
run;
%end;
DATA _null;
SET Trans_cod;
CALL SYMPUT ('NCOD', _N_);
call symput (compress('Cod_'||_N_), trim(left(strata_finale)));
run;
%if %eval(&ncod)>100 %then
%do; %let maxcod = 10; %end; /*!!!RIMETTERE A 99*/
%else
%do; %let maxcod = &ncod; %end;
/**********************************************/
/* MATCHING ONE-TO-ONE 			      */
/**********************************************/
%if &MATCH_TYPE. = ONE %then %do;

%put ***********************************************************************************************;
%put The number of automatic rounds is ---> &maxcod.			         		    ;
%put ***********************************************************************************************;

%do i=1 %to &maxcod.;
%if %eval(&numtot3.)>1 AND %eval(&numtot2.)=0 %then %do;
data cod_strata;
set rec_data;
cod_num_&i. = compress(&&cod_&i "");
run;
%end;
%if %eval(&numtot3.)=1 AND %eval(&numtot2.)=0 %then %do;
data cod_strata;
set rec_data;
cod_num_&i. = compress(&&cod_&i);
run;
%end;
%if %eval(&numtot2.)>0 %then %do;
data cod_strata;
set rec_data;
cod_num_&i. = compress(&&cod_&i||"");
run;
%end;
PROC FREQ DATA=cod_strata NOPRINT;
TABLES COD_NUM_&i.*&treat / OUT=FREQ_&i.(DROP=PERCENT);
run;
proc transpose data=FREQ_&i. out=FREQ_&i.(drop=_NAME_ _LABEL_) prefix=treat;
by COD_NUM_&i.;
id &treat.;
var COUNT;
run;
data FREQ_&i.;
set FREQ_&i.;
if treat0 = . then treat0=0;
if treat1 = . then treat1=0;
_NSIZE_ = min(treat0, treat1);
run;
data untreated(DROP=&treat) treated(DROP=&treat);
set cod_strata (keep=&ID. COD_NUM_&i. &treat);
if &treat = 0 then output untreated;
if &treat = 1 then output treated;
run;
proc sort data=untreated; by COD_NUM_&i.; run;
proc surveyselect data=untreated out=sample_untreated_&i. (drop= SelectionProb SamplingWeight) method=srs sampsize=FREQ_&i.;
strata COD_NUM_&i.;
id &id.;
run;
proc sort data=treated; by COD_NUM_&i.; run;
proc surveyselect data=treated out=sample_treated_&i. (drop= SelectionProb SamplingWeight) method=srs sampsize=FREQ_&i.;
strata COD_NUM_&i.;
id &id.;
run;

data dataset_matched_&i.;
set sample_treated_&i. sample_untreated_&i.;
matched=1;
run;


/*************** CHECK # OF MATCHED UNITS ********************/
%let dtstmatch = %sysfunc(open(dataset_matched_&i.,i));
%let nmatched = %sysfunc(attrn(&dtstmatch.,nobs));
%let dtsm = %sysfunc(close(&dtstmatch.));

%if %EVAL(&nmatched.) = 0 %then %do;

DATA L1_&i;
	SET L1_TOT;
	drop winchester;
	L1 = 1;
	CODICE = "Cod_num_&i."; 
RUN;	
%if "&method" = "Sturges" %then %do;

proc freq data=&lib..&data.;
table &treat. /OUT = COUNT_TREAT_UNT (DROP = PERCENT);
run;
DATA _null_;
	SET COUNT_TREAT_UNT;
	CALL SYMPUT ('p', _n_);
	CALL SYMPUT (compress('tr'||_N_), trim(left(count)));
run;
%put NO MATCHED UNITS: EXECUTION TERMINATED;
%put ORIGINAL POPULATION: TREATED = &TR2. - UNTREATED = &tr1.;
%put MATCHED UNITS: TREATED = 0 - UNTREATED = 0;
%put L1 = 1;
%GOTO EXIT;
%end;

%end;
/************************************************************/

%ELSE %DO;

proc sql;
create table cod_dataset_&i as select
a.*,
b.matched
from cod_strata as a left join dataset_matched_&i. as b
on a.&id = b.&id
where matched = 1;
quit;

%L1_Nest (CODICE = &i,
ID1 = &id ,
TREAT1 = &treat,
COD_DATASET=cod_dataset_&i);
%END;


data L1_TOT;
set L1_TOT L1_&i;
drop winchester;
if codice = "" then delete;
run;

%if "&method" = "Sturges" %then %do;  /*if the binning option is Percent then skip*/
data L1_Stur;
set L1_TOT;
if substr(codice,9,1)^= "0" then codice = "Cod_num_stur";
run;
data cod_dataset_Stur;
	set cod_dataset_&i;
run;
data L1_TOT;
set L1_TOT;
if substr(codice,9,1)^= "0" then delete;
run;

%end;


%end;
%end;
/**********************************************/
/* MATCHING ONE-TO-N 			      */
/**********************************************/
%if &MATCH_TYPE. = N %then %do;
%do i=1 %to &maxcod.;
%if %eval(&numtot3.)>1 AND %eval(&numtot2.)=0 %then %do;
data cod_strata;
set rec_data;
cod_num_&i. = compress(&&cod_&i "");
run;
%end;
%if %eval(&numtot3.)=1 AND %eval(&numtot2.)=0 %then %do;
data cod_strata;
set rec_data;
cod_num_&i. = compress(&&cod_&i);
run;
%end;
%if %eval(&numtot2.)>0 %then %do;
data cod_strata;
set rec_data;
cod_num_&i. = compress(&&cod_&i||"");
run;
%end;
PROC FREQ DATA=cod_strata NOPRINT;
TABLES COD_NUM_&i.*&treat / OUT=FREQ_&i.(DROP=PERCENT);
run;
proc transpose data=FREQ_&i. out=FREQ_&i.(drop=_NAME_ _LABEL_) prefix=treat;
by COD_NUM_&i.;
id &treat.;
var COUNT;
run;
data FREQ_&i.;
set FREQ_&i.;
if treat0 = . then treat0=0;
if treat1 = . then treat1=0;
_NSIZE_ = min(treat0, treat1);
delta_freq = abs((treat0 / (treat0 + treat1)) - (treat1 / (treat0 +treat1)));
IF _NSIZE_=0 THEN DELETE;
run;

/*************** CHECK # OF MATCHED UNITS ********************/
%let dtstmatchN = %sysfunc(open(FREQ_&i.,i));
%let nmatchedN = %sysfunc(attrn(&dtstmatchN.,nobs));
%let dtsmN = %sysfunc(close(&dtstmatchN.));

%if &nmatchedN. = 0 %then %do;
DATA L1_&i;
	SET L1_TOT;
	drop winchester;
	L1 = 1;
	CODICE = "Cod_num_&i."; 
RUN;	
%if "&method" = "Sturges" %then %do;
proc freq data=&lib..&data.;
table &treat. /OUT = COUNT_TREAT_UNT (DROP = PERCENT);
run;
DATA _null_;
	SET COUNT_TREAT_UNT;
	CALL SYMPUT ('p', _n_);
	CALL SYMPUT (compress('tr'||_N_), trim(left(count)));
run;
%put NO MATCHED UNITS: EXECUTION TERMINATED;
%put ORIGINAL POPULATION: TREATED = &TR2. - UNTREATED = &tr1.;
%put MATCHED UNITS: TREATED = 0 - UNTREATED = 0;
%put L1 = 1;
%GOTO EXIT;
%end;

%end;
/************************************************************/

%else %do;
data freq_&i.;
set freq_&i.;
if treat0 = 0 OR treat1 = 0 then delete;
run;

proc means data=freq_&i. noprint;
output out=Mct_&i. (drop = _type_ _freq_) sum(treat1) = Mt sum(treat0) = Mc;
run;
data _NULL_;
set Mct_&i.;
call symput ('Mt', Mt);
call symput ('Mc', Mc);
run;
%let dsid8 = %sysfunc(open(FREQ_&i.,i));
%let numt_&i. = %sysfunc(attrn(&dsid8.,nobs));
%let rkt = %sysfunc(close(&dsid8.));
%if &&numt_&i. ne 0 %then %do;
data weight_&i;
set freq_&i.;
Wsc_&i. = (treat1 / treat0) * (&Mc. / &Mt.);
run;
proc sql;
create table cod_strata as select
a.*,
b.Wsc_&i.
from cod_strata as a left join weight_&i. as b
on a.cod_num_&i. = b.cod_num_&i.;
quit;
data cod_strata(drop=Wsc_&i.);
set cod_strata;
Wsc1_&i. = Wsc_&i.;
if &treat. = 1 then Wsc1_&i. = 1;
if Wsc_&i. = . then Wsc1_&i. = 0;
run;
%end;
data cod_dataset_&i;
set cod_strata;
matched = 1;
if Wsc1_&i. = 0 then delete;
run;


/**********************************************/

%L1_Nest_W (CODICE = &i,
ID1 = &id ,
TREAT1 = &treat,
COD_DATASET=cod_dataset_&i,
weight =  Wsc1_&i.);

/**********************************************/
%end;

data L1_TOT;
set L1_TOT L1_&i;
drop winchester;
if codice = "" then delete;
run;

%if "&method" = "Sturges" %then %do;  /*if the binning option is Percent then skip*/
data L1_Stur;
set L1_TOT;
if substr(codice,9,1)^= "0" then codice = "Cod_num_stur";
run;
data cod_dataset_Stur;
	set cod_dataset_&i;
run;
data L1_TOT;
set L1_TOT;
if substr(codice,9,1)^= "0" then delete;
run;
%end;

%end;
%end;
/******	MATCHING CLOSED*********/


proc sort data=L1_TOT; by L1; run;
data L1_appo;
	set L1_tot L1_Stur;
	if substr(codice,9,1)= "0" then delete;
run;


/*************** CHECK # OF MATCHED UNITS ********************/
proc MEANS data=L1_appo MEAN;
	OUTPUT OUT=CHECKMATCH (DROP = _TYPE_ _FREQ_) MEAN(L1)=M_L1;
RUN;
DATA _null_;
	SET CHECKMATCH;
	CALL SYMPUT ('MEANL1', trim(left(M_L1)));
run;

%IF &MEANL1. = 1 %THEN %DO;


proc freq data=&lib..&data.;
table &treat. /OUT = COUNT_TREAT_UNT (DROP = PERCENT);
run;
DATA _null_;
	SET COUNT_TREAT_UNT;
	CALL SYMPUT ('p', _n_);
	CALL SYMPUT (compress('tr'||_N_), trim(left(count)));
run;
%put NO MATCHED UNITS: EXECUTION TERMINATED;
%put ORIGINAL POPULATION: TREATED = &TR2. - UNTREATED = &TR1.;
%put MATCHED UNITS: TREATED = 0 - UNTREATED = 0;
%put L1 = 1;
%GOTO EXIT;
%END;
/************************************************************/



data L1_appo1 (keep = cod);
	set L1_appo;
	if substr(codice,9,1)= "0" then delete;
	cod=tranwrd(codice, "Cod_num_", "");
	if _N_>1 then stop;/**/
run;
data _NULL_;
set L1_appo1;
call symput ('numero', trim(left(cod)));
run;

%if &MATCH_TYPE. = N %then %do;
%if "&method" = "Sturges" %then %do;  /*if the binning option is Percent then skip*/
data cod_dataset_&numero.;	
	set cod_dataset_&numero.;
	rename Wsc1_1 = Wsc1_&numero.;
run;
%end;
proc sort data=cod_dataset_&numero. out=cod1_dataset_&numero.(keep = &ID matched Wsc1_&numero.); by &id; run;
proc sort data=&lib..&data.; by &ID; run;
data &lib..&data.;
update &lib..&data. cod1_dataset_&numero.;
by &ID;
run;
%end;

%else %do; /*ONLY IF METHOD = ONE*/
proc sort data=cod_dataset_&numero. out=cod1_dataset_&numero.(keep = &ID matched); by &id; run;
proc sort data=&lib..&data.; by &ID; run;
data &lib..&data.;
update &lib..&data. cod1_dataset_&numero.;
by &ID;
run;
%end;

DATA &lib..L1_finale;
format L1_value $20.;
set L1_TOT L1_Stur;
%if "&method" ^= "Sturges" %then %do;  /*if the binning option is Percent then skip*/
if codice = compress("Cod_num_&numero") then do;
L1_value = "L1 Best";
output;
end;
%end;
if codice = "Cod_num_0" then do;
L1_value = "L1 Original";
output;
end;
if codice = "Cod_num_stur" then do;
L1_value = "L1 Sturges";
output;
end;
run;
proc sort data=&lib..L1_finale nodupkey; by codice; run;
data &lib..L1_finale ;
	set &lib..L1_finale ;
	drop codice;
run;
proc sort data=&lib..L1_finale out=&lib..L1_finale; by descending L1; run;


/****************************START L1-PLOT******************************************/
%if "&method" ^= "Sturges" %then %do;  /*if the binning option is Percent then do*/

proc freq data=&lib..&data.;
	table &treat / out=pippo1;
run;
data _null_;
	set pippo1;
	where &treat = 1;
	call symput (compress('num_treat'), trim(left(count)));
run;

%do i = 1 %to &maxcod;
proc freq data=Cod_dataset_&i;
	table &treat / out=pippo_&i;
run;

%let pluto1 = %sysfunc(open(pippo_&i,i));
%let numpluto_&i = %sysfunc(attrn(&pluto1.,nobs));
%let endpluto1 = %sysfunc(close(&pluto1.));

%if &&numpluto_&i. >0 %then %do;
data _null_;
	set pippo_&i;
	where &treat = 1;
	call symput (compress('n_match'||&i), trim(left(count)));
run;
%end;
%else %do;
%let n_match&i = 0;
%end;
%end;

data L1_tot1;
	set L1_tot;
	where codice ^= "Cod_num_stur";
	if substr(codice,9,1)= "0" then delete;
	cod=tranwrd(codice, "Cod_num_", "") / 1;
run;
proc sort data=L1_tot1; by cod; run;
data L1_tot1;
	set L1_tot1;
		Label Matchate = "Percentage of Matched Units";
	%do i = 1 %to &maxcod;
	if &&numpluto_&i.>0 then do;
	if _N_ = %eval(&i) then Matchate = %sysevalf(%eval(&&n_match&i) / %eval(&num_treat));
	end;
	%end;
	if Matchate=. then Matchate=0;
run;

proc freq data=Cod_dataset_stur;
	table &treat / out=pippo_stur;
run;
data _null_;
	set pippo_stur;
	where &treat = 1;
	call symput (compress('n_match_stur'), trim(left(count)));
run;

%let vref = %sysevalf( %eval(&n_match_stur) / %eval(&num_treat) );
data _null_;
	set L1_Stur;
	where codice = "Cod_num_stur";
	call symput (compress('href'), trim(left(L1)));
run;

proc sort data=L1_tot1; by descending L1; run;
ods listing;
goptions reset=all ; 
FILENAME file "&path_graph./L1_&data..png";
goption device=PNG gsfname=file xpixels=1750 ypixels=900;
axis1 order = (0 to 1 by 0.1);
axis2 order = (0 to 1 by 0.1);
proc gplot data=L1_TOT1(where = (matchate^=0));
title "Matching Frontier - L1 vs Percentage of Treated Units Matched";
plot matchate*l1/ caxis=red 
vref = &vref cvref=red lvref=1
href = &href chref=red lhref=1
vaxis = axis1
haxis = axis2;
run;quit;
%end;
/****************************END L1-PLOT********************************/

%if "&report"  = "on" %then %do;
/**********************************************/
/* 		FINAL REPORT 		      */
/**********************************************/
ods _all_ close;
options nodate nonumber nocenter;
title "Macro CEM Matching summary";

ods html PATH="&path_graph"(URL=NONE) body="Report.html" style=default   /**/;
/*ods NOPROCTITLE;*/

proc freq data=&lib..&data.;
title "Original population";
table &treat;
run;

DATA ZP;
set &lib..L1_finale;
where L1_value = "L1 Original";
title "Multivariate L1 distance of the original population";
proc print;
run;

proc freq data=&lib..&data.;
title "Matched population";
where matched=1;
table &treat;
run;

%if "&method" = "Sturges" %then %do;  /*if the binning option is Percent then skip*/
DATA ZP1;
set &lib..L1_finale;
where L1_value = "L1 Sturges"; 
title "Multivariate L1 distance of the matched population by Sturges' Rule";
proc print;
run;
%end;
%else %do;  /*if the binning option is Percent then skip*/
DATA ZP1;
set &lib..L1_finale;
where L1_value = "L1 Best"; 
title "Multivariate L1 distance of the matched population";
proc print;
run;
%end;

%if "&method" ^= "Sturges" %then %do;  /*if the binning option is Percent then skip
title "Choose your preferred solution in terms of L1 and Percentage of Treated Units Matched";*/
data L1_tot2 (rename=(matchate=Perc_Matched));
set L1_tot1 (drop = cod);
proc print;
run;quit;
%end;

ods html close;
%end;
/************************************************************/

%END;
%ELSE %DO;
%PUT Warning: records in the original dataset are not uniquely identified by the &ID variable. Remove duplicates or re-define your &ID variable!;
%END;

%EXIT: %mend;


%macro L1_Nest (CODICE = ,
			ID1 = ,
			TREAT1 = ,
			COD_DATASET=);



data _null_;
set &cod_dataset;
call symput (compress('n'), _N_);
run;

proc sql;
	create table ranked as select
		a.&id.,
		a.&treat1.,
		%do k = 1 %to &numtot2.;
		V_&k ,
		%end;
		b.FI
	from &cod_dataset as a left join strati as b
	on a.&id. = b.&id.;
quit;

/*Multidimensional Histogram distinct for Treated and Untreated*/
data treat untreat;
	set ranked;
	if &treat1. = 1 then output treat;
	if &treat1. = 0 then output untreat;
run;

proc freq data = treat noprint; 
	table FI /out=freq_treat;
run;
data freq_treat;
	set freq_treat;
	drop count;
	rename percent = rel_treat;
run;
proc freq data = untreat noprint; 
	table FI /out=freq_untreat;
run;
data freq_untreat;
	set freq_untreat;
	rename percent = rel_untreat;
	drop count;
run;

proc sort data=freq_treat;
			by FI;
run;
proc sort data=freq_untreat;
			by FI;
run;

data freq;
	merge freq_treat freq_untreat;
			by FI;
	if rel_treat = . then rel_treat = 0;
	if rel_untreat = . then rel_untreat = 0;
	diff_freq = abs(rel_treat - rel_untreat);
run;

proc means data=freq sum noprint nway;
	output out=L1 (drop = _type_ _freq_) sum(diff_freq) = MM;
run;

data L1_&codice;
	set L1;
	codice = "Cod_num_&codice";
	L1 =  1/2*(MM/100);
	keep codice L1;
run;

/*L1 UNIVARIATE*/
/*The &numtot2. parameter is already defined within the macro %CEM*/
%do k = 1 %to &numtot2.;
%put here var V_&k ;
%end;
%do k = 1 %to &numtot2.;
proc freq data = treat noprint; 
	table V_&k /out=freq_treat_V_&k;
run;
data freq_treat_V_&k ;
	set freq_treat_V_&k ;
	drop count;
	rename percent = rel_treat_V_&k;
run;
proc freq data = untreat noprint; 
	table V_&k /out=freq_untreat_V_&k;
run;
data freq_untreat_V_&k ;
	set freq_untreat_V_&k ;
	rename percent = rel_untreat_V_&k ;
	drop count;
run;

proc sort data=freq_treat_V_&k;
			by V_&k ;
run;
proc sort data=freq_untreat_V_&k;
			by V_&k ;
run;

data freq_V_&k ;
	merge freq_treat_V_&k freq_untreat_V_&k;
			by V_&k ;
	if rel_treat_V_&k  = . then rel_treat_V_&k  = 0;
	if rel_untreat_V_&k  = . then rel_untreat_V_&k  = 0;
	diff_freq_V_&k  = abs(rel_treat_V_&k  - rel_untreat_V_&k );
run;

proc means data=freq_V_&k sum noprint nway;
	output out=L1_V_&k  (drop = _type_ _freq_) sum(diff_freq_V_&k) = MM;
run;

data L11_V_&k ;
	set L1_V_&k ;
	Variabile = "V_&k";
	L1 =  1/2*(MM/100);
	keep L1 Variabile;
run;
%end;

data L1_Univariate_&codice;
	set %do k = 1 %to &numtot2.;
			L11_V_&k
		%end;
		;
run;
%mend;


%macro L1_Nest_w (CODICE = ,
			ID1 = ,
			TREAT1 = ,
			COD_DATASET=,
			weight =  Wsc1_&i.);



data _null_;
set &cod_dataset;
call symput (compress('n'), _N_);
run;

proc sql;
	create table ranked as select
		a.&id.,
		a.&treat1.,
		%do k = 1 %to &numtot2.;
		V_&k ,
		%end;
		&weight,
		b.FI
	from &cod_dataset as a left join strati as b
	on a.&id. = b.&id.;
quit;

/*Multidimensional Histogram distinct for Treated and Untreated*/
data treat untreat;
	set ranked;
	if &treat1. = 1 then output treat;
	if &treat1. = 0 then output untreat;
run;

proc freq data = treat noprint; 
	weight &weight;
	table FI /out=freq_treat;
run;
data freq_treat;
	set freq_treat;
	drop count;
	rename percent = rel_treat;
run;
proc freq data = untreat noprint; 
	weight &weight;
	table FI /out=freq_untreat;
run;
data freq_untreat;
	set freq_untreat;
	rename percent = rel_untreat;
	drop count;
run;

proc sort data=freq_treat;
			by FI;
run;
proc sort data=freq_untreat;
			by FI;
run;

data freq;
	merge freq_treat freq_untreat;
			by FI;
	if rel_treat = . then rel_treat = 0;
	if rel_untreat = . then rel_untreat = 0;
	diff_freq = abs(rel_treat - rel_untreat);
run;

proc means data=freq sum noprint nway;
	output out=L1 (drop = _type_ _freq_) sum(diff_freq) = MM;
run;

data L1_&codice;
	set L1;
	codice = "Cod_num_&codice";
	L1 =  1/2*(MM/100);
	keep codice L1;
run;

/*L1 UNIVARIATE*/
/*The &numtot2. parameter is already defined within the macro %CEM*/
%do k = 1 %to &numtot2.;
%put here V_&k ;
%end;
%do k = 1 %to &numtot2.;
proc freq data = treat noprint; 
	table V_&k /out=freq_treat_V_&k;
run;
data freq_treat_V_&k ;
	set freq_treat_V_&k ;
	drop count;
	rename percent = rel_treat_V_&k;
run;
proc freq data = untreat noprint; 
	table V_&k /out=freq_untreat_V_&k;
run;
data freq_untreat_V_&k ;
	set freq_untreat_V_&k ;
	rename percent = rel_untreat_V_&k ;
	drop count;
run;

proc sort data=freq_treat_V_&k;
			by V_&k ;
run;
proc sort data=freq_untreat_V_&k;
			by V_&k ;
run;

data freq_V_&k ;
	merge freq_treat_V_&k freq_untreat_V_&k;
			by V_&k ;
	if rel_treat_V_&k  = . then rel_treat_V_&k  = 0;
	if rel_untreat_V_&k  = . then rel_untreat_V_&k  = 0;
	diff_freq_V_&k  = abs(rel_treat_V_&k  - rel_untreat_V_&k );
run;

proc means data=freq_V_&k sum noprint nway;
	output out=L1_V_&k  (drop = _type_ _freq_) sum(diff_freq_V_&k) = MM;
run;

data L11_V_&k ;
	set L1_V_&k ;
	Variabile = "V_&k";
	L1 =  1/2*(MM/100);
	keep L1 Variabile;
run;
%end;

data L1_Univariate_&codice;
	set %do k = 1 %to &numtot2.;
			L11_V_&k
		%end;
		;
run;
%mend;



%macro L1 (CODICE = ,
			ID1 = ,
			TREAT1 = ,
			COD_DATASET=);


data appo_trans;
set transcod_num (rename = (trans_num=trans))transcod_cat (rename =(trans_cat=trans));
keep trans type;
run;

data _null_;
set appo_trans;
call symput (compress('g'), trim(left(_N_)));
call symput (compress('var_'||_N_), trim(strip(trans)));
call symput (compress('tt_'||_N_),TYPE );
run;

%do j = 1 %to &g;
%put variabile var_&j = &&var_&j;
run;
%END;


/*Calculate the number of Numeric Variable*/
%let optransnum		= %sysfunc(open(Transcod_num,i));
%let v 				= %sysfunc(attrn(&optransnum.,nobs));
%let cltransnum		= %sysfunc(close(&optransnum.));

%let optranscat		= %sysfunc(open(Transcod_cat,i));
%let w 				= %sysfunc(attrn(&optranscat.,nobs));
%let cltranscat		= %sysfunc(close(&optranscat.));

data _null_;
set &cod_dataset;
call symput (compress('n'), _N_);
run;

data ranked;
	set &cod_dataset;
run;

%if %eval(&w)>0 %then %do;
data _null_;
	set Transcod_cat;
	call symput (compress('w'), _N_);
	call symput (compress('varic_'||_N_), trim(strip(name)));
run;
%end;

%if %eval(&v)>0 %then %do;
data _null_;
	set Transcod_num;
	call symput (compress('v'), _N_);
	call symput (compress('vari_'||_N_), trim(strip(name)));
run;

%do b = 1 %to &v;
	/*Calculate for each Numerical Variable StdDev Min and Max*/
	proc means data=&cod_dataset (keep = &&vari_&b) stddev min max noprint nway;
		output out=stock_&&vari_&b (drop = _type_ _freq_) stddev(&&vari_&b)=sd_&&vari_&b
														  min(&&vari_&b)=min_&&vari_&b
														  max(&&vari_&b)=max_&&vari_&b;
	run;
	/*Stock them*/
	data _NULL_;
		set stock_&&vari_&b;
		call symput ("sd_&&vari_&b", trim(left(sd_&&vari_&b)));
		call symput ("min_&&vari_&b", trim(left(min_&&vari_&b)));
		call symput ("max_&&vari_&b", trim(left(max_&&vari_&b)));
	run;

	/*Create Bins for Numerical Variables based on Scott's Rule*/
	%let num = %eval(&n);

	%let Scott = %sysevalf((3.49 * &&&&sd_&&vari_&b * (&num**(-(1/3)))));
	%let Bins_&&vari_&b = %sysevalf( (&&&&max_&&vari_&b - &&&&min_&&vari_&b)  / (3.49 * &&&&sd_&&vari_&b * (&num**(-(1/3)))) , floor);

	%put &&&&Bins_&&vari_&b;

	/*Create New Variables based on binning above*/
	data ranked;
		set ranked;
		if &&vari_&b >=&&&&min_&&vari_&b then V_&b = 1;
		%do s = 2 %to &&&&Bins_&&vari_&b;
		if &&vari_&b > &&&&min_&&vari_&b + %sysevalf(%eval(&s-1)* &Scott)  AND &&vari_&b <= &&&&min_&&vari_&b + %sysevalf(%eval(&s)* &Scott) then V_&b = &s;
		%end;
	run;
run;



%end;
%end;

%if %eval(&w)>0 %then %do;
		data ranked;
			set ranked;
			%do h = 1 %to &w;
			rename &&varic_&h = C_&h;
			%end;
			i = 1 /*Add i = 1 to calculate the absolute frequency in the next proc means*/;
			keep %do h = 1 %to &w;
				&&varic_&h 
				%end;
				&treat1
				&id1
				i
				%if %eval(&v)>0 %then %do;
				%do b = 1 %to &v;
				V_&b
				%end;
			%end;
			;
		run;
%end;

proc contents data=ranked out=name (keep = name) noprint; run;
data name;
	set name;
	where name in (%do k = 1 %to &v;
					"V_&k"
					%end;
					%do l = 1 %to &w;
					"C_&l"
					%end;
					);
run;

data _null_;
	set name;
	call symput (compress('z'), compress(_N_));
	call symput (compress('variz_'||_N_), trim(strip(name)));
run;


/*Multidimensional Histogram distinct for Treated and Untreated*/
data strati (keep = &id. FI %do k = 1 %to &v;
					V_&k 
				%end;
				%do l = 1 %to &w;
					C_&l 
				%end;);
	set ranked ;
	format FI $500.;
	FI = compress(
				%do k = 1 %to &v;
					V_&k ||
				%end;
				%do l = 1 %to &w;
					C_&l ||
				%end;
				"");
run;
/*Multidimensional Histogram distinct for Treated and Untreated*/
data treat ;
	set ranked ;
	format FI $500.;
	FI = compress(
				%do k = 1 %to &v;
					V_&k ||
				%end;
				%do l = 1 %to &w;
					C_&l ||
				%end;
				"");
	where &treat1 = 1 ;
run;
data untreat ;
	set ranked ;
	format FI $500.;
	FI = compress(
				%do k = 1 %to &v;
					V_&k ||
				%end;
				%do l = 1 %to &w;
					C_&l ||
				%end;
				"");
	where &treat1 = 0 ;
run;

proc freq data = treat noprint; 
	table FI /out=freq_treat;
run;
data freq_treat;
	set freq_treat;
	drop count;
	rename percent = rel_treat;
run;
proc freq data = untreat noprint; 
	table FI /out=freq_untreat;
run;
data freq_untreat;
	set freq_untreat;
	rename percent = rel_untreat;
	drop count;
run;

proc sort data=freq_treat;
			by FI;
run;
proc sort data=freq_untreat;
			by FI;
run;

data freq;
	merge freq_treat freq_untreat;
			by FI;
	if rel_treat = . then rel_treat = 0;
	if rel_untreat = . then rel_untreat = 0;
	diff_freq = abs(rel_treat - rel_untreat);
run;

proc means data=freq sum noprint nway;
	output out=L1 (drop = _type_ _freq_) sum(diff_freq) = MM;
run;

data L1_&codice;
	set L1;
	codice = "Cod_num_&codice";
	L1 =  1/2*(MM/100);
	keep codice L1;
run;

/*L1 UNIVARIATE*/
%do k = 1 %to &v;
proc freq data = treat noprint; 
	table V_&k /out=freq_treat_V_&k;
run;
data freq_treat_V_&k ;
	set freq_treat_V_&k ;
	drop count;
	rename percent = rel_treat_V_&k;
run;
proc freq data = untreat noprint; 
	table V_&k /out=freq_untreat_V_&k;
run;
data freq_untreat_V_&k ;
	set freq_untreat_V_&k ;
	rename percent = rel_untreat_V_&k ;
	drop count;
run;

proc sort data=freq_treat_V_&k;
			by V_&k ;
run;
proc sort data=freq_untreat_V_&k;
			by V_&k ;
run;

data freq_V_&k ;
	merge freq_treat_V_&k freq_untreat_V_&k;
			by V_&k ;
	if rel_treat_V_&k  = . then rel_treat_V_&k  = 0;
	if rel_untreat_V_&k  = . then rel_untreat_V_&k  = 0;
	diff_freq_V_&k  = abs(rel_treat_V_&k  - rel_untreat_V_&k );
run;

proc means data=freq_V_&k sum noprint nway;
	output out=L1_V_&k  (drop = _type_ _freq_) sum(diff_freq_V_&k) = MM;
run;

data L11_V_&k ;
	set L1_V_&k ;
	Variabile = "V_&k";
	L1 =  1/2*(MM/100);
	keep L1 Variabile;
run;
%end;

data L1_Univariate;
	set %do k = 1 %to &v;
			L11_V_&k
		%end;
		;
run;


%mend;



%macro L1_w (CODICE = ,
			ID1 = ,
			TREAT1 = ,
			COD_DATASET=);


data appo_trans;
set transcod_num (rename = (trans_num=trans))transcod_cat (rename =(trans_cat=trans));
keep trans type;
run;

data _null_;
set appo_trans;
call symput (compress('g'), trim(left(_N_)));
call symput (compress('var_'||_N_), trim(strip(trans)));
call symput (compress('tt_'||_N_),TYPE );
run;

%do j = 1 %to &g;
%put variabile var_&j = &&var_&j;
run;
%END;


/*Calculate the number of Numeric Variable*/
%let optransnum		= %sysfunc(open(Transcod_num,i));
%let v 				= %sysfunc(attrn(&optransnum.,nobs));
%let cltransnum		= %sysfunc(close(&optransnum.));

%let optranscat		= %sysfunc(open(Transcod_cat,i));
%let w 				= %sysfunc(attrn(&optranscat.,nobs));
%let cltranscat		= %sysfunc(close(&optranscat.));

data _null_;
set &cod_dataset;
call symput (compress('n'), _N_);
run;

data ranked;
	set &cod_dataset;
run;

%if %eval(&w)>0 %then %do;
data _null_;
	set Transcod_cat;
	call symput (compress('w'), _N_);
	call symput (compress('varic_'||_N_), trim(strip(name)));
run;
%end;

%if %eval(&v)>0 %then %do;
data _null_;
	set Transcod_num;
	call symput (compress('v'), _N_);
	call symput (compress('vari_'||_N_), trim(strip(name)));
run;

%do b = 1 %to &v;
	/*Calculate for each Numerical Variable StdDev Min and Max*/
	proc means data=&cod_dataset (keep = &&vari_&b) stddev min max noprint nway;
		output out=stock_&&vari_&b (drop = _type_ _freq_) stddev(&&vari_&b)=sd_&&vari_&b
														  min(&&vari_&b)=min_&&vari_&b
														  max(&&vari_&b)=max_&&vari_&b;
	run;
	/*Stock them*/
	data _NULL_;
		set stock_&&vari_&b;
		call symput ("sd_&&vari_&b", trim(left(sd_&&vari_&b)));
		call symput ("min_&&vari_&b", trim(left(min_&&vari_&b)));
		call symput ("max_&&vari_&b", trim(left(max_&&vari_&b)));
	run;

	/*Create Bins for Numerical Variables based on Scott's Rule*/
	%let num = %eval(&n);

	%let Scott = %sysevalf((3.49 * &&&&sd_&&vari_&b * (&num**(-(1/3)))));
	%let Bins_&&vari_&b = %sysevalf( (&&&&max_&&vari_&b - &&&&min_&&vari_&b)  / (3.49 * &&&&sd_&&vari_&b * (&num**(-(1/3)))) , floor);

	%put &&&&Bins_&&vari_&b;

	/*Create New Variables based on binning above*/
	data ranked;
		set ranked;
		if &&vari_&b >=&&&&min_&&vari_&b then V_&b = 1;
		%do s = 2 %to &&&&Bins_&&vari_&b;
		if &&vari_&b > &&&&min_&&vari_&b + %sysevalf(%eval(&s-1)* &Scott)  AND &&vari_&b <= &&&&min_&&vari_&b + %sysevalf(%eval(&s)* &Scott) then V_&b = &s;
		%end;
	run;
run;



%end;
%end;

%if %eval(&w)>0 %then %do;
		data ranked;
			set ranked;
			%do h = 1 %to &w;
			rename &&varic_&h = C_&h;
			%end;
			i = 1 /*Add i = 1 to calculate the absolute frequency in the next proc means*/;
			keep %do h = 1 %to &w;
				&&varic_&h 
				%end;
				&treat1
				&id1
				i
				%if %eval(&v)>0 %then %do;
				%do b = 1 %to &v;
				V_&b
				%end;
			%end;
			;
		run;
%end;

proc contents data=ranked out=name (keep = name) noprint; run;
data name;
	set name;
	where name in (%do k = 1 %to &v;
					"V_&k"
					%end;
					%do l = 1 %to &w;
					"C_&l"
					%end;
					);
run;

data _null_;
	set name;
	call symput (compress('z'), compress(_N_));
	call symput (compress('variz_'||_N_), trim(strip(name)));
run;


/*Multidimensional Histogram distinct for Treated and Untreated*/
/*Multidimensional Histogram distinct for Treated and Untreated*/
data strati (keep = &id. FI %do k = 1 %to &v;
					V_&k 
				%end;
				%do l = 1 %to &w;
					C_&l 
				%end;);
	set ranked ;
	format FI $500.;
	FI = compress(
				%do k = 1 %to &v;
					V_&k ||
				%end;
				%do l = 1 %to &w;
					C_&l ||
				%end;
				"");
run;
/*Multidimensional Histogram distinct for Treated and Untreated*/
data treat ;
	set ranked ;
	format FI $500.;
	FI = compress(
				%do k = 1 %to &v;
					V_&k ||
				%end;
				%do l = 1 %to &w;
					C_&l ||
				%end;
				"");
	where &treat1 = 1 ;
run;
data untreat ;
	set ranked ;
	format FI $500.;
	FI = compress(
				%do k = 1 %to &v;
					V_&k ||
				%end;
				%do l = 1 %to &w;
					C_&l ||
				%end;
				"");
	where &treat1 = 0 ;
run;

proc freq data = treat noprint; 
	table FI /out=freq_treat;
run;
data freq_treat;
	set freq_treat;
	drop count;
	rename percent = rel_treat;
run;
proc freq data = untreat noprint; 
	table FI /out=freq_untreat;
run;
data freq_untreat;
	set freq_untreat;
	rename percent = rel_untreat;
	drop count;
run;

proc sort data=freq_treat;
			by FI;
run;
proc sort data=freq_untreat;
			by FI;
run;

data freq;
	merge freq_treat freq_untreat;
			by FI;
	if rel_treat = . then rel_treat = 0;
	if rel_untreat = . then rel_untreat = 0;
	diff_freq = abs(rel_treat - rel_untreat);
run;

proc means data=freq sum noprint nway;
	output out=L1 (drop = _type_ _freq_) sum(diff_freq) = MM;
run;

data L1_&codice;
	set L1;
	codice = "Cod_num_&codice";
	L1 =  1/2*(MM/100);
	keep codice L1;
run;

/*L1 UNIVARIATE*/
%do k = 1 %to &v;
proc freq data = treat noprint; 
	table V_&k /out=freq_treat_V_&k;
run;
data freq_treat_V_&k ;
	set freq_treat_V_&k ;
	drop count;
	rename percent = rel_treat_V_&k;
run;
proc freq data = untreat noprint; 
	table V_&k /out=freq_untreat_V_&k;
run;
data freq_untreat_V_&k ;
	set freq_untreat_V_&k ;
	rename percent = rel_untreat_V_&k ;
	drop count;
run;

proc sort data=freq_treat_V_&k;
			by V_&k ;
run;
proc sort data=freq_untreat_V_&k;
			by V_&k ;
run;

data freq_V_&k ;
	merge freq_treat_V_&k freq_untreat_V_&k;
			by V_&k ;
	if rel_treat_V_&k  = . then rel_treat_V_&k  = 0;
	if rel_untreat_V_&k  = . then rel_untreat_V_&k  = 0;
	diff_freq_V_&k  = abs(rel_treat_V_&k  - rel_untreat_V_&k );
run;

proc means data=freq_V_&k sum noprint nway;
	output out=L1_V_&k  (drop = _type_ _freq_) sum(diff_freq_V_&k) = MM;
run;

data L11_V_&k ;
	set L1_V_&k ;
	Variabile = "V_&k";
	L1 =  1/2*(MM/100);
	keep L1 Variabile;
run;
%end;

data L1_Univariate;
	set %do k = 1 %to &v;
			L11_V_&k
		%end;
		;
run;


%mend;

