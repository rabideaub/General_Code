libname hc "/schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Data/HospitalCompare";
libname dat "/schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Data/HRRP/Final";
libname den "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/PAC/Data/Raw/Denominator";

%let include = /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Programs/HRRP;
%include "&include./00_Assign_Macro_Variables_and_Libraries.sas";

%let model_vars= AMI ALZHDMTA ALZH ATRIALFB CNCRCLRC CNCRPRST CNCRBRST CNCRLUNG COPD ASTHMA DEPRESSN DIABETES CHF HIPFRAC 
				 HYPERT ISCHMCHT OSTEOPRS RA_OA STRKETIA; /*Covariates recommended by Jose Escarce*/

options compress=yes mprint;


/************************************************************
 DATA PREP
************************************************************/
/*Identify hospitals in test city*/
data hc;
	set hc.hosp_compare_general;
	length hospital_name $100 facility $20;
	rename hospital_name=provname;
	facility="Hospital";
	provid=put(input(Provider_ID,?? 8.),z6.);
	fac_zip=put(input(zip_code,?? 8.), z5.);
	if trim(left(upcase(county_name))) in("LOS ANGELES","ORANGE","SAN BERNARDINO","SAN DIEGO") & upcase(state)="CA";
run;

proc sort data=hc nodupkey; by provid; run;

proc contents data=dat.geocoded_hf; run;
proc sort data=dat.geocoded_hf out=geocoded; by provid; run; /*This is the freq array dataset with geocoded X and Y coordinates and relative distance*/

data geocoded (drop=fac_zip rename=(fac_zip_c=fac_zip));
	set geocoded;
	fac_zip_c=put(input(fac_zip,?? 8.), z5.);
run;

proc contents data=geocoded; run;
proc contents data=hc; run;

proc print data=geocoded (obs=10); 
	var provid; 
run;

proc print data=hc; 
	var provid; 
run;

/*Create a dataset with just Los Angeles hospitals and hospital discharges*/
data community;
	merge geocoded (in=a where=(provid~='' & trim(left(provid))~='.'))
		  hc (in=b);
	by provid;
	if a & b;
run;

proc sort data=community; by provid; run;

/*Data Prep*/
data community (drop=CHF)
	 count_disch; /*Elixhauser CHF is the same name as CCW CHF. Drop elix*/
	set community;
	retain tot_hf;
	by provid;
	if first.provid then tot_hf=0;
	tot_hf+1;
	output community;
	if last.provid then output count_disch;
run;

/*Take a look at some key variables*/
proc means data=community;
	class provid;
	var radm30;
run;

proc means data=count_disch;
	class provid;
	var tot_hf;
run;

proc sort data=count_disch; by provid; run;
proc sort data=community; by provid; run;

/*Drop observations that are discharged from hospitals with less than 80 total discharges (not yearly, total)*/
data community;
	merge community (in=a)
		  count_disch (in=b keep=provid tot_hf where=(tot_hf>=80));
	by provid;
	if a & b;
run;

/*CALCULATE CHRONIC CONDITIONS ON THE INDEX HOSPITALIZATION*/
data community;
	set community;

	AMI=0; 
	ALZHDMTA=0; 
	ALZH=0; 
	ATRIALFB=0; 
	CNCRCLRC=0; 
	CNCRPRST=0; 
	CNCRBRST=0; 
	CNCRLUNG=0; 
	COPD=0; 
	ASTHMA=0; 
	DEPRESSN=0; 
	DIABETES=0; 
	CHF=0; 
	HIPFRAC=0; 
	HYPERT=0; 
	ISCHMCHT=0; 
	OSTEOPRS=0; 
	RA_OA=0; 
	STRKETIA=0;

	/*Identify chronic conditions present as part of the index hospitalization only*/
	%macro loop_dx;
		%do i=1 %to 10;
			if diag&i. in('3310', '33111', '33119','3312', '3317', '2900','29010', '29011', '29012','29013', '29020', '29021',
			'2903', '29040', '29041','29042', '29043', '2940','29410', '29411', '29420','29421', '2948', '797') then ALZHDMTA=1;

			if diag&i. in('3310') then ALZH=1;

			if diag&i. in('1530', '1531', '1532','1533', '1534', '1535', '1536','1537', '1538',
			'1539', '1540', '1541', '2303','2304', 'V1005', 'V1006') then CNCRCLRC=1;

			if diag&i. in('185', '2334', 'V1046') then CNCRPRST=1;

			if diag&i. in('1740', '1741', '1742','1743', '1744', '1745', '1746','1748', '1749', '1750', '1759',
			'2330', 'V103') then CNCRBRST=1;

			if diag&i. in('1622', '1623', '1624','1625', '1628', '1629', '2312','V1011') then CNCRLUNG=1;

			if diag&i. in('490', '4910', '4911','4918', '4919', '4920', '4928',
			'49120', '49121', '49122','4940', '4941', '496') then COPD=1;

			if diag&i. in('49300', '49301', '49302','49310', '49311', '49312','49320', '49321', '49322',
			'49381', '49382', '49390','49391', '49392') then ASTHMA=1;

			if diag&i. in('29620', '29621', '29622','29623', '29624', '29625',
			'29626', '29630', '29631','29632', '29633', '29634','29635', '29636', '29650',
			'29651', '29652', '29653','29654', '29655', '29656','29660', '29661', '29662',
			'29663', '29664', '29665','29666', '29689', '2980','3004', '3091', '311') then DEPRESSN=1;

			if diag&i. in('24900', '24901', '24910','24911', '24920', '24921',
			'24930', '24931', '24940','24941', '24950', '24951','24960', '24961', '24970',
			'24971', '24980', '24981','24990', '24991', '25000','25001', '25002', '25003',
			'25010', '25011', '25012','25013', '25020', '25021','25022', '25023', '25030',
			'25031', '25032', '25033','25040', '25041', '25042','25043', '25050', '25051',
			'25052', '25053', '25060','25061', '25062', '25063','25070', '25071', '25072',
			'25073', '25080', '25081','25082', '25083', '25090','25091', '25092', '25093',
			'3572', '36201', '36202','36203', '36204', '36205','36206', '36641') then DIABETES=1;

			if diag&i. in('39891', '40201', '40211','40291', '40401', '40403','40411', '40413', '40491',
			'40493', '4280', '4281','42820', '42821', '42822','42823', '42830', '42831',
			'42832', '42833', '42840','42841', '42842', '42843','4289') then CHF=1;

			if diag&i. in('73314', '73315', '73396','73397', '73398', '8080','8081', '8082', '8083',
			'80841', '80842', '80843','80844', '80849', '80851','80852', '80853', '80854',
			'80859', '8088', '8089','82000', '82001', '82002','82003', '82009', '82010',
			'82011', '82012', '82013','82019', '82020', '82021','82022', '82030', '82031',
			'82032', '8208', '8209') then HIPFRAC=1;

			if diag&i. in('36211', '4010', '4011','4019', '40200', '40201','40210', '40211', '40290',
			'40291', '40300', '40301','40310', '40311', '40390','40391', '40400', '40401',
			'40402', '40403', '40410','40411', '40412', '40413','40490', '40491', '40492',
			'40493', '40501', '40509','40511', '40519', '40591','40599', '4372') then HYPERT=1;

			if diag&i. in('41000', '41001', '41002','41010', '41011', '41012','41020', '41021', '41022',
			'41030', '41031', '41032','41040', '41041', '41042','41050', '41051', '41052',
			'41060', '41061', '41062','41070', '41071', '41072','41080', '41081', '41082',
			'41090', '41091', '41092','4110', '4111', '41181','41189', '412', '4130', '4131',
			'4139', '41400', '41401','41402', '41403', '41404','41405', '41406', '41407',
			'41412', '4142', '4143','4144', '4148', '4149') then ISCHMCHT=1;

			if diag&i. in('73300', '73301', '73302','73303', '73309') then OSTEOPRS=1;

			if diag&i. in('7140', '7141', '7142','71430', '71431', '71432','71433', '71500', '71504',
			'71509', '71510', '71511','71512', '71513', '71514','71515', '71516', '71517',
			'71518', '71520', '71521','71522', '71523', '71524','71525', '71526', '71527',
			'71528', '71530', '71531','71532', '71533', '71534','71535', '71536', '71537',
			'71538', '71580', '71589','71590', '71591', '71592','71593', '71594', '71595',
			'71596', '71597', '71598','7200', '7210', '7211', '7212','7213', '72190', '72191') then RA_OA=1;

			if diag&i. in('430', '431', '43301','43311', '43321', '43331','43381', '43391', '43400',
			'43401', '43410', '43411','43490', '43491', '4350','4351', '4353', '4358', '4359',
			'436', '99702')  then STRKETIA=1;

			if 800<=input(substr(diag&i.,1,3),8.)<805 | substr(diag&i.,1,3)='V57' then exclude_sk=1;
		%end;

		%do i=1 %to 2;
			if diag&i. in('41001', '41011', '41021','41031', '41041', '41051','41061', '41071', '41081','41091') then AMI=1;
			if diag&i. in('42731') then ATRIALFB=1;
		%end;

		if exclude_sk=1 then STRKETIA=0;
	%mend;
	%loop_dx;

	chronic_count=sum(AMI,ALZHDMTA,ALZH,ATRIALFB,CNCRCLRC,CNCRPRST,CNCRBRST,CNCRLUNG,COPD,ASTHMA,DEPRESSN,DIABETES,CHF,HIPFRAC,
					  HYPERT,ISCHMCHT,OSTEOPRS,RA_OA,STRKETIA);
run;

							%macro out2;
								/*Merge on the chronic conditions from the denominator files*/
								data den_cc (rename=(bene_id=hicno));
									set den.mbsf_cc_summary2009 (in=a)
										den.mbsf_cc_summary2010 (in=b)
										den.mbsf_cc_summary2011 (in=c)
										den.mbsf_cc_summary2012 (in=d)
										den.mbsf_cc_summary2013 (in=e);

									if a then year='2009';
									if b then year='2010';
									if c then year='2011';
									if d then year='2012';
									if e then year='2013';
								run;

								proc contents data=den_cc out=cc_contents (keep=NAME LABEL rename=(NAME=VARIABLE));
								run;

								proc sort data=community; by hicno year; run;
								proc sort data=den_cc nodupkey; by hicno year; run;

								data community;
									merge community (in=a)
										  den_cc (in=b);
									by hicno year;
									if a;
									/*Make a 0-1 binary variable for each CC. Value 3 = appropriate claims and coverate, date is the date of first occurence*/
									ALZH=((ALZH=3 | ALZHM=3) & ALZHE<=ADMIT); /*ALZH=end-year Alzheimers flag, ALZHM=mid-year Alzheimers flag, ALZHE=Alzheimers first occurrence date*/
									ALZHDMTA=((ALZHDMTA=3 | ALZHDMTM=3) & ALZHDMTE<=ADMIT);
									AMI=((AMI=3 | AMIM=3) & AMIE<=ADMIT);
									ANEMIA=((ANEMIA=3 | ANEMIA_MID=3) & ANEMIA_EVER<=ADMIT);
									ASTHMA=((ASTHMA=3 | ASTHMA_MID=3) & ASTHMA_EVER<=ADMIT);
									ATRIALFB=((ATRIALFB=3 | ATRIALFM=3) & ATRIALFE<=ADMIT);
									CATARACT=((CATARACT=3 | CATARCTM=3) & CATARCTE<=ADMIT);
									CHF=((CHF=3 | CHFM=3) & CHFE<=ADMIT);
									CHRNKIDN=((CHRNKIDN=3 | CHRNKDNM=3) & CHRNKDNE<=ADMIT);
									CNCRENDM=((CNCRENDM=3 | CNCENDMM=3) & CNCENDME<=ADMIT);
									CNCRBRST=((CNCRBRST=3 | CNCRBRSM=3) & CNCRBRSE<=ADMIT);
									CNCRCLRC=((CNCRCLRC=3 | CNCRCLRM=3) & CNCRCLRE<=ADMIT);
									CNCRLUNG=((CNCRLUNG=3 | CNCRLNGM=3) & CNCRLNGE<=ADMIT);
									CNCRPRST=((CNCRPRST=3 | CNCRPRSM=3) & CNCRPRSE<=ADMIT);
									COPD=((COPD=3 | COPDM=3) & COPDE<=ADMIT);
									DEPRESSN=((DEPRESSN=3 | DEPRSSNM=3) & DEPRSSNE<=ADMIT);
									DIABETES=((DIABETES=3 | DIABTESM=3) & DIABTESE<=ADMIT);
									GLAUCOMA=((GLAUCOMA=3 | GLAUCMAM=3) & GLAUCMAE<=ADMIT);
									HIPFRAC=((HIPFRAC=3 | HIPFRACM=3) & HIPFRACE<=ADMIT);
									HYPERL=((HYPERL=3 | HYPERL_MID=3) & HYPERL_EVER<=ADMIT);
									HYPERP=((HYPERP=3 | HYPERP_MID=3) & HYPERP_EVER<=ADMIT);
									HYPERT=((HYPERT=3 | HYPERT_MID=3) & HYPERT_EVER<=ADMIT);
									HYPOTH=((HYPOTH=3 | HYPOTH_MID=3) & HYPOTH_EVER<=ADMIT);
									ISCHMCHT=((ISCHMCHT=3 | ISCHMCHM=3) & ISCHMCHE<=ADMIT);
									OSTEOPRS=((OSTEOPRS=3 | OSTEOPRM=3) & OSTEOPRE<=ADMIT);
									RA_OA=((RA_OA=3 | RA_OA_M=3) & RA_OA_E<=ADMIT);
									STRKETIA=((STRKETIA=3 | STRKTIAM=3) & STRKTIAE<=ADMIT);
								run;
							%mend;


/*Create a list of all hospitals in the community, with dummy vars for each one*/
proc sort data=community; by provid; run;

data unique_hosp;
	set community (keep=provid hosp_x hosp_y);
	by provid;
	if last.provid then output;
run;

data unique_hosp;
	set unique_hosp end=eof;
	num=put(_N_,8.);
	provid=trim(left(provid));
	if eof then call symput('tot_hosp',trim(left(_N_)));
	hosp_x_c=put(hosp_x, best.);
	hosp_y_c=put(hosp_y, best.);
run;

proc print data=unique_hosp; run;

/*Create macro vars that make data-driven hospital dummies and residuals. 
  Do it together because numbering will be important (e.g. hosp1 corresponds with resid1)*/
proc sql noprint;
	select compress("hosp"||num||"=(provid='"||provid||"');") into: hosp_dummy
	separated by '0A'x
	from unique_hosp;
quit;

proc sql noprint;
	select "if provid='"||provid||"' then hospital="||num||";" into: hosp_num
	separated by '0A'x
	from unique_hosp;
quit;

proc sql noprint;
	select compress("provid"||num||"="||provid||";") into: provid_num
	separated by '0A'x
	from unique_hosp;
quit;

proc sql noprint;
	select compress("resid"||num||"=abs((hosp"||num||"-pred"||num||"));") into: hosp_resid /*Made this the absoulte value to see if it fixes stuff. 6/16/16 BR*/
	separated by '0A'x
	from unique_hosp;
quit;

proc sql noprint;
	select compress("hosp_x"||num||"="||hosp_x_c||";") into: hosp_x
	separated by '0A'x
	from unique_hosp;
quit;

proc sql noprint;
	select compress("hosp_y"||num||"="||hosp_y_c||";") into: hosp_y
	separated by '0A'x
	from unique_hosp;
quit;

proc sql noprint;
	select compress("distance"||num||"=geodist("||"hosp_y"||num||",hosp_x"||num||",bene_y,bene_x);") into: hosp_distance
	separated by '0A'x
	from unique_hosp;
quit;

%put "&hosp_distance.";

data community;
	set community;
	hosp_x_c=put(hosp_x, best.);
	hosp_y_c=put(hosp_y, best.);
run;

proc print data=community (obs=20); 
	var hicno_case provid hosp_x hosp_x_c hosp_y hosp_y_c bene_x bene_y;
run;

data community;
	set community;
	/*Set the dummy, location, and distance variables*/
	array dist {&tot_hosp.} distance1-distance&tot_hosp.;
	&hosp_dummy.;
	&provid_num.;
	&hosp_x.;
	&hosp_y.;
	&hosp_distance.;
	&hosp_num.;

	/*Bin distance relative to the closest hospital. 0 is the closest hospital */
	min_distance=min(of distance1-distance&tot_hosp.);
	do i=1 to &tot_hosp.;
		dist[i]=dist[i]-min_distance;
	end;
run;

proc univariate data=community;
	var distance1;
	where hosp1=1;
run;

/*Bin distance relative to the closest hospital. 0 is the closest hospital */
proc format;
	value distance 0-1='0-1 miles'
				   1-2='1-2 miles'
				   2-3='2-3 miles'
				   3-5='3-5 miles'
				   5-7='5-7 miles'
				   7-10='7-10 miles'
				   10-15='10-15 miles'
				   15-20='15-20 miles'
				   20-high='20+ miles';

	value age 65-70='65-70'
			  70-75='70-75'
			  75-80='75-80'
			  80-85='80-85'
			  85-high='85+';
run;
data community;
	set community;
	if min_distance<=100;
	format distance1-distance&tot_hosp. distance.;
	format age age.;
run;

title "DATA PREP SUMMARY MEANS";
proc means data=community;
	var distance1-distance&tot_hosp. hosp1-hosp&tot_hosp.;
	output mean=;
run;
title;


/************************************************************
CONDTIONAL LOGIT MODEL
************************************************************/
/*RESHAPE THE DATA WIDE TO LONG*/
data community_long (drop=distance1-distance&tot_hosp. hosp1-hosp&tot_hosp.);
	set community;

	array hosp {&tot_hosp.} hosp1-hosp&tot_hosp.;
	array dist {&tot_hosp.} distance1-distance&tot_hosp.;
	array prov_id {&tot_hosp.} provid1-provid&tot_hosp.;

	retain id 0;
	id+1;

	do i=1 to &tot_hosp.;
		choice=hosp[i];
		distance=dist[i];
		choice_id=prov_id[i];
		output;
	end;
run;

title "CHECK WIDE FILE";
proc print data=community (obs=20);
	var hicno_case provid &model_vars. hosp1-hosp&tot_hosp. distance1-distance&tot_hosp.;
run; 

proc freq data=community;
	tables hosp2;
run;

title "CHECK LONG FILE";
proc print data=community_long (obs=200);
	var hicno_case provid &model_vars. choice choice_id distance;
run;
title;

proc freq data=community_long;
	tables provid*choice;
run;

/*Export to stata to test modelling over there*/
proc export data=community_long 
			outfile = "/schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Data/HRRP/Final/community_long_hf.dta" replace;
run;

/*RUN THE CONDITIONAL LOGIT*/
 proc mdc data=community_long;
 	class distance /*age male*/ ;
    model choice = &model_vars. chronic_count distance distance*chronic_count /*age male age*male*/ /
          type=clogit
          covest=hess
          nchoice=&tot_hosp.;
    id /*hicno_case*/id;
    output out=clogit_long p=pred_choice;
 run;

 proc print data=clogit_long (obs=200);
 	var id hicno_case choice pred_choice;
run;

proc means data=clogit_long;
	class choice;
	var pred_choice;
run;

data clogit_long;
	set clogit_long;
	retain num;
	by id;
	if first.id then num=0;
	num+1;
run;

proc freq data=community_long;
	tables provid*choice;
run;

/*RESHAPE THE DATA LONG TO WIDE WITH THE NEW RESIDUAL ADDED IN*/
 data clogit;
 	set clogit_long;
	resid=choice-pred_choice;

	array hosp {&tot_hosp.} hosp1-hosp&tot_hosp.;
	array res {&tot_hosp.} resid1-resid&tot_hosp.;
	retain hosp1-hosp&tot_hosp. resid1-resid&tot_hosp.;

	by id;
	if first.id then i=1;
	else i+1;
	if i<=&tot_hosp. then do;
		hosp{i}=choice;
		res{i}=resid;
	end;

	if last.id;
run;

proc print data=clogit (obs=50);
	var hicno_case radm30 hosp1-hosp&tot_hosp. resid1-resid&tot_hosp.;
run;

proc freq data=clogit;
	tables hosp1 hosp2 hosp3 hosp45 hosp74;
run;

proc sort data=clogit; by descending radm30; run;

/************************************************************
TWO STAGE RESIDUAL INCLUSION MODEL, 2nd PHASE PROBIT
************************************************************/
/*Run the probit model using the residual and hospital dummies*/
proc probit data=clogit order=data;
	model radm30 = &model_vars. hosp1 hosp3-hosp&tot_hosp. resid1 resid3-resid&tot_hosp.; /*hosp2 and resid2 are references*/
	output out=community_mlogit p=pred_radm30;
run;

proc univariate data=community_mlogit;
	var pred_radm30;
run;



/************************************************************
BASELINE HOSPITAL FIXED EFFECTS
************************************************************/
title "Baeline Regression";
proc sort data=community; by descending radm30; run;

proc probit data=community order=data;
	model radm30 = hosp1-hosp&tot_hosp.;
	output out=community_baseline p=pred_radm30;
run;
title;

/*Look at results*/
proc univariate data=community_baseline;
	var pred_radm30;
run;

/************************************************************
NAIVE MODEL (BASELINE + CHRONIC CONDITIONS)
************************************************************/
title "Naiive Probit Regression"; 
proc probit data=community order=data;
	model radm30 = &model_vars. hosp1 hosp3-hosp&tot_hosp.;
	output out=community_naiive p=pred_radm30;
run;
title;

proc univariate data=community_naiive;
	var pred_radm30;
run;

