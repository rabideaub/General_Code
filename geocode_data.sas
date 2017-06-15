libname geo "/schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Data/Geocode";
libname med "/schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Data/HRRP/Final";
libname hc "/schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Data/HospitalCompare";

%let include = /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Programs/HRRP;
%include "&include./00_Assign_Macro_Variables_and_Libraries.sas";

/*Read in the input dataset*/
data input (drop=zip rename=(zip_n=zip));
	set med.freq_array_&DX. /*(obs=10000)*/;
	plus4=input(substr(zip,length(zip)-3,4),8.)*1;
	zip_n=input(substr(zip,1,5),8.)*1;
	*bene_zip=input(zip,8.);
run;

/*Add on the zip and address infor from hospital compare*/
data hc;
	set hc.hosp_compare_general;
	length hospital_name $100 facility $20;
	rename hospital_name=provname;
	facility="Hospital";
	provid=put(input(provider_id,?? 6.),z6.);
	*fac_zip=put(input(zip_code,?? 5.),z5.);
	fac_zip=zip_code*1;
run;

proc sort data=input; by provid; run;
proc sort data=hc; by provid; run;

data input;
	merge input (in=a)
		  hc (in=b);
	by provid;
	if a;
run;

proc contents data=input; run;
proc contents data=geo.zip4; run;

/*Now do proc geocode with the zip+4 to get the bene X and Y coordinates*/
proc geocode
	method = plus4 /* Zip+4 method */
	data = input /* Addresses to geocode */
	/*addressplus4var = bene_zip*/ /*Zipcode variable on the input dataset*/
	out = work.geocoded /* Geocoded data set */
	lookup = geo.zip4 /* Default world lookup data */
	/*lookupplus4var = PLUS4*/; /*Zipcode variable on the lookup dataset*/
run;

proc print data=geocoded (obs=20); run;

data geocoded (rename=(x=bene_x y=bene_y));
	set geocoded;
run;

/*Now do proc geocode with the hospital address to get the hospital X and Y coordinates*/
proc geocode
	method = street /* Zip+4 method */
	data = geocoded /* Addresses to geocode */
	addressvar = address
	addresscityvar = city
	addressstatevar = state
	addresszipvar = fac_zip
	out = work.geocoded2 /* Geocoded data set */
	lookupstreet = geo.usm; /* Default world lookup data */
run;

proc print data=geocoded2 (obs=20); run;

data med.geocoded_&DX.;
	set geocoded2 (rename=(x=hosp_x y=hosp_y));
	distance=geodist(hosp_y,hosp_x,bene_y,bene_x);
run;

proc univariate data=med.geocoded_&DX.;
	var distance;
run;

proc print data=med.geocoded_&DX.(obs=20); run;
