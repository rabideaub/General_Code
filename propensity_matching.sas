/* Define the library for study data */
*LIBNAME study '/schaeffer-a/sch-projects/dua-data-projects/OPTUM/rabideau/Data';
/* ************************************* */
/* Perform the Logistic Regression */
/* Calculate and save propensity score */
/* Propensity score name = PROB */
/* Output file = STUDY.AllPropen */
/* ************************************* */


/*PROC LOGISTIC DATA = optum_demog descend;
MODEL chdp_ind = gdr_cd age_cat product D_EDUCATION_LEVEL_CODE D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division;
/ SELECTION = STEPWISE RISKLIMITS LACKFIT RSQUARE PARMLABEL;
 OUTPUT OUT=study.lv_beneficiary_propensity prob=prob ;
RUN;
*/




/* ***************************************** */
/* ***************************************** */
/* Matching Macro */
/* ***************************************** */
/* ***************************************** */
%MACRO OneToManyMTCH (
 Lib, /* Library Name */
 Dataset, /* Data set of all patients */
 depend, /* Dependent variable that indicates Case or Control */
 /* Code 1 for Cases, 0 for Controls */
 SiteN, /* Site/Hospital ID */
 PatientN, /* Patient ID */
 matches, /* Output data set of matched pairs */
 NoContrls); /* Number of controls to match to each case */
/* ********************* */
/* Macro to Create the Case and Control Data sets */
/* ********************* */
%MACRO INITCC(CaseAndCtrls,digits);
data tcases (drop=cprob)
tctrl (drop=aprob) ;
set &CaseAndCtrls. ;
/* Create the data set of Controls */
if &depend. = 0 and prob ne . then
do;
cprob = Round(prob,&digits.);
Cmatch = 0;
Length RandNum 8;
RandNum=ranuni(1234567);
Label RandNum='Uniform Randomization Score';
output tctrl;
end;
/* Create the data set of Cases */
else if &depend. = 1 and prob ne . then
do;
Cmatch = 0;
aprob =Round(prob,&digits.);
output tcases;
end;
run;
%SORTCC;
%MEND INITCC;
/* ********************* */
/* Macro to sort the Cases and Controls data set */
/* ********************* */
%MACRO SORTCC;
proc sort data=tcases out=&LIB..Scase;
by prob;
run;
proc sort data=tctrl out=&LIB..Scontrol;
by prob randnum;
run;
%MEND SORTCC;
/* ********************* */
/* Macro to Perform the Match */
/* ********************* */
%MACRO MATCH (MATCHED,DIGITS);
data &lib..&matched. (drop=Cmatch randnum aprob cprob start oldi curctrl
matched);
/* select the cases data set */
set &lib..SCase ;
curob + 1;
matchto = curob;
if curob = 1 then do;
start = 1;
oldi = 1;
end;
/* select the controls data set */
DO i = start to n;
set &lib..Scontrol point = i nobs = n;
if i gt n then goto startovr;
if _Error_ = 1 then abort;
curctrl = i;
/* output control if match found */
if aprob = cprob then
do;
Cmatch = 1;
output &lib..&matched.;
matched = curctrl;
goto found;
end;
/* exit do loop if out of potential matches */
else if cprob gt aprob then
goto nextcase;
startovr: if i gt n then
goto nextcase;
END; /* end of DO LOOP */
/* If no match was found, put pointer back*/
nextcase:
if Cmatch=0 then start = oldi;
/* If a match was found, output case and increment pointer */
found:
if Cmatch = 1 then do;
oldi = matched + 1;
start = matched + 1;
set &lib..SCase point = curob;
output &lib..&matched.;
end;
retain oldi start;
if _Error_=1 then _Error_=0;
run;
/* get files of unmatched cases and controls */
proc sort data=&lib..scase out=sumcase;
by /*SiteN*/ &PatientN.;
run;
proc sort data=&lib..scontrol out=sumcontrol;
by /*SiteN*/ &PatientN.;
run;
proc sort data=&lib..&matched. out=smatched (keep=/*SiteN*/ &PatientN. matchto);
by /*SiteN*/ &PatientN.;
run;
data tcases (drop=matchto);
merge sumcase(in=a) smatched;
by /*SiteN*/ &PatientN.;
if a and matchto = . ;
cmatch = 0;
aprob =Round(prob,&digits.);
run;
data tctrl (drop=matchto);
merge sumcontrol(in=a) smatched;
by /*SiteN*/ &PatientN.;
if a and matchto = . ;
cmatch = 0;
cprob = Round(prob,&digits.);
run;
%SORTCC
%MEND MATCH;
/* ********************* */
/* Macro to call Macro MATCH for each of the 8-digit to 1-digit matchs */
/* ********************* */
%MACRO CallMATCH;
/* Do a 8-digit match */
%MATCH(Match8,.0000001);
/* Do a 7-digit match on remaining unmatched*/
%MATCH(Match7,.000001);
/* Do a 6-digit match on remaining unmatched*/
%MATCH(Match6,.00001);
/* Do a 5-digit match on remaining unmatched*/
%MATCH(Match5,.0001);
/* Do a 4-digit match on remaining unmatched */
%MATCH(Match4,.001);
/* Do a 3-digit match on remaining unmatched */
%MATCH(Match3,.01);
/* Do a 2-digit match on remaining unmatched */
%MATCH(Match2,.1);
/* Do a 1-digit match on remaining unmatched */
%MATCH(Match1,.1);
%MEND CallMATCH;
/* ********************* */
/* Macro to Merge all the matched files into one file */
/* ********************* */
%MACRO MergeFiles(MatchNo);
data &matches.&MatchNo. (drop = matchto);
set &lib..match8(in=a) &lib..match7(in=b) &lib..match6(in=c) &lib..match5(in=d)
&lib..match4(in=e)
&lib..match3(in=f) &lib..match2(in=g) &lib..match1(in=h);
if a then match_&MatchNo. = matchto;
if b then match_&MatchNo. = matchto + 10000;
if c then match_&MatchNo. = matchto + 100000;
if d then match_&MatchNo. = matchto + 1000000;
if e then match_&MatchNo. = matchto + 10000000;
if f then match_&MatchNo. = matchto + 100000000;
if g then match_&MatchNo. = matchto + 1000000000;
if h then match_&MatchNo. = matchto + 10000000000;
run;
%MEND MergeFiles;
/* ******************************* */
/* ******************************* */
/* Perform the initial 1:1 Match */
/* ******************************* */
/* ******************************* */
/* Create file of cases and controls */
%INITCC(&LIB..&dataset.,.00000001);
/* Perform the 8-digit to 1-digit matches */
%CallMATCH;
/* Merge all the matches files into one file */
%MergeFiles(1)
/* ********************************* */
/* ********************************* */
/* Perform the remaining 1:N Matches */
/* ********************************* */
/* ********************************* */
%IF &NoContrls. gt 1 %Then %DO;
%DO i = 2 %TO &NoContrls.;
%let Lasti=%eval(&i. - 1);
/* ********** */
/* Start with Cases from the last Matched Cases file and the remaining UnMatched
*/
/* Controls. NOTE: The Unmatched Controls file (Scontrol) is created at end of
the */
/* previous match */
/* Select the Matched Cases from the last Matched File */
data &LIB..Scase;
 set &matches.&Lasti.;
 where &Depend. = 1;
run;
/* ********** */
/* Perform the 8-1 digit matches between Matched Cases and the Unmatched
Controls */
%CallMATCH;
/* ********** */
/* Merge the 8-digit to 1-digit matches files into one file */
%MergeFiles(&i.)
%DO m = 1 %TO &Lasti.;
data &matches.&i.;
 set &matches.&i.;
 if &Depend.=0 then Match_&m. = .;
run;
%END;
/* ********** */
/* Determine which OLD Controls correspond to the kept Cases */
%DO c = 1 %TO &Lasti.;
 /* Select the KEPT Cases */
 proc sort data=&matches.&i. out=skeepcases (keep = Match_&c.);
by Match_&c.;

where &Depend. = 1;
 run;
 /* Get the OLD Controls */
 proc sort data = &matches.&Lasti. out = soldcontrols&c.;
by Match_&c.;
where &Depend. = 0 and Match_&c. ne . ;
 run;
 /* Get the OLD Controls that correspond to the kept Cases */
 data keepcontrols&c.;
 merge skeepcases (in = a) soldcontrols&c. (in = b);
 by Match_&c.;
 if a;
run;
%END;
/* ********** */
/* Combine all the OLD Controls into one file */
data keepcontrols;
set keepcontrols1 (obs=0);
run;
%DO k = 1 %TO &Lasti.;
 data keepcontrols;
 set keepcontrols keepcontrols&k.;s
run;
%END;
/* ********** */
/* Append the OLD matched Controls to the new file of matched cases and
controls */
data &matches.&i.;
 set &matches.&i. keepcontrols;
run;
/* ********** */
/* If there are more matches to be made, add the previously matched, but not
kept, */
/* controls back into the pool of unmatched controls */
%if &i. lt &NoContrls. %then %do;
 %DO z = 1 %TO &Lasti.;
/* Select all the KEPT Cases */
proc sort data=&matches.&i. out=skeepcases (keep = Match_&z.);
 by Match_&z.;
 where &Depend. = 1;
run;
/* Select all the OLD Controls */
proc sort data = &matches.&Lasti. out = soldcontrols&z.;
 by Match_&z.;
 where &Depend. = 0 and Match_&z. ne .;
run;
/* Keep the OLD Controls that correspond to the NOT KEPT Cases */
/* Drop the previuos Match_X variable */
data AddBackControls&z. (drop = Match_&z.);
 merge skeepcases (in = a) soldcontrols&z. (in = b);
 by Match_&z.;
 if b and not a;
run;
%END; /* End DO */
/* Drop the previuos Match_X variable */
data &LIB..Scontrol (drop = Match_&lasti. );

 set &LIB..Scontrol;
run;
/* Append */
%DO y = 1 %TO &Lasti.;
 data &LIB..Scontrol;
set &LIB..Scontrol AddBackControls&y.;
 run;
 %END; /* End DO */
%end; /* End IF */
 %END; /* End Main DO */
%END; /* End Main IF */
/* ************************************* */
/* ************************************* */
/* Save the final matched pairs data set */
/* ************************************* */
/* ************************************* */
/* Sort file by Treatment Variable */
proc sort data=&matches.&NoContrls. out = &lib..&matches.;
by &depend.;
run;
%MEND OneToManyMTCH;
