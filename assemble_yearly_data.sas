
/************************************************************************************************************
************************************************************************************************************
Program: assemble_yearly_data
Created By: Brendan Rabideau
Created Date: 8/7/15
Updated Date: 8/12/15
Purpose: This program assembles yearly-level datasets taking into account changing variable names, variable
		 types, and variable lengths through the years. With the exception of the optional renaming, this 
		 process is entirely data-driven and should work on any combination of datasets.

Notes: - Naming and directory structure must be standardized to facilitate automation. 
	   - Renaming variables is done through an external crosswalk to allow flexibility in future modifications, 
		 reduce clutter in the code, promote standardization between users, and provide a strong source of documentation. 
		 A user must update these crosswalks if he wishes to rename variables. The names of the crosswalks should be 
		 uniform and each sheet/file should correspond to a given year of the data, or have a YEAR variable.

	   Current naming structure is: 'root_path/&year/&ds&year.sas7bdat'

	   To run: Update the libnames and macros in the 'Set Macros' section

Updates: 8/12/15 - Updated documentation and formatting. BR
		 8/13/15 - Updated contents matrix to read in numbers with a length greater than 1 digit
************************************************************************************************************
************************************************************************************************************/

options compress=yes nofmterr mprint mtrace mlogic minoperator symbolgen formchar="|----|+|---+=|-/\<>*";

/*************************************
Set Macros
*************************************/
%let ds=mds; /*Root name of dataset (e.g. root for med2001, med2002, med2003 is med)*/
%let startyr=2003;	/*First year of data yyyy*/
%let endyr=2010;	/*Last year of data yyyy*/
%let rename_xwalk=N; /*Y or N, no quotes. Y indicates there is an external renaming crosswalk for each year of the data*/
%let out = /schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Output; /*Destination of Content Matrix*/
libname datout "/schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Data/MDS"; /*Destination of the final, appended dataset*/


%Macro All_Years(ds,startyr,endyr,rename_xwalk);
%do i=&startyr. %to &endyr.;
	%let year = &i.;
	libname dat "/schaeffer-b/sch-protected/VERTICAL-INTEGRATION/Data/2009-13_PAC/Raw/MDS"; /*Location of input datasets*/


/********************************************************************************************
No Modification is Needed Beyond This Point
********************************************************************************************/

/*******************************************************************
 Rename Using XWalks if Applicable
*******************************************************************/
	%if "&rename_xwalk"="Y" %then %do;	
		/*Read in the rename xwalk*/
		proc import out=renames
					datafile="/disk/agedisk3/medicare.work/goldman-DUA25731/rabideau/Documentation/rename_medpar_&year..csv"
					DBMS=/*xlsx*/csv replace;
					*sheet="MedPAR_&year.";
					getnames=yes;
					guessingrows=32767;
					datarow=2;
		run;
		/*Having trouble importing the xwalk onto the linux server. Currently have it as a .csv instead of xlsx, creating varname difficulties. Quick fix.
		  Full solution here: https://communities.sas.com/message/20370*/
		data renames;
			set renames;
			var2=compress(var2);
			rename VAR2=RENAMED_VAR;
		run;

		proc print data=renames;
		run;
		proc contents data=renames;
		run;
		
		/*Make a list of variable renames*/
		proc sql noprint;
			select compress(ORIG_VAR||"="||RENAMED_VAR) into: rename_vars
			separated by '0A'x
			from renames;
			/*where YEAR="&year."*/ /*proc import is struggling to read in my csv correctly*/
		quit;


		/*Rename the variables*/
		data &ds.&year.;
			set dat.&ds.;
			rename &rename_vars.;
		run;
	%end;
	
	/*If we're not renaming any vars using the xwalk just read in the datasets as work datasets*/
	%else %do;
		data &ds.&year;
			set dat.&ds.&year.;
		run;
	%end;

/*******************************************************************
 Generate Contents for Each Year - Create a Content Matrix
*******************************************************************/
	/*Output the contents for each year, creating a variable with type and length info*/
	proc contents data=&ds.&year. out=contents_&year. noprint; 
	run;
	data contents_&year. (keep=Name Y&year.) label_&year.(keep=Name Label_&year.);
		set contents_&year. (rename=(NAME=Name LABEL=Label_&year.));
		if type=1 then Y&year. = cats("N",length);
		else Y&year = cats("C",length);
		Name=lowcase(Name);
		output contents_&year.;
		output label_&year.;
	run;

	proc sort data=contents_&year.; by Name; run;
	proc sort data=label_&year.; by Name; run;
	
%end;

/*Merge the different years together by the variable names*/
data contents;
	retain Name;
	merge contents_:;
	by Name;
run;
/*Label dataset is optional, but could be useful*/
data labels;
	retain Name;
	merge label_:;
	by Name;
run;
/*Stick the most current label onto the spreadsheet*/
data cont_label;
	retain Name Label_&endyr.;
	merge contents
		  labels (keep=Name Label_&endyr);
	by Name;
	/*Create a variable for the maximum length and the type of each variable. If variable
	  is both char and numeric in different years, make it char.*/
	Max_Len=.;
	Type="N";
	%do i=&startyr. %to &endyr.;
		if substr(trim(left(Y&i.)),1,1)='C' then Type='C';
		Max_Len=max(input(substr(trim(left(Y&i.)),2),8.),Max_Len);
	%end;
	Max_Len_c=put(Max_Len,2.);
run;

proc print data=cont_label;
run;

/*****************************************************************************
 Use the Content Matrix to Determine Which Vars to Convert or Increase Length
*****************************************************************************/
%do yr=&startyr. %to &endyr.;
	/*Make a list of all variables to be made character*/
	proc sql noprint;
		select compress(Name) into: char_vars&yr.
		separated by " "
		from cont_label
		where Type='C' & Y&yr.~="";
	quit;
	/*Make a list of all character values to be renamed*/
	proc sql noprint;
		select compress(Name||"=orig__"||Name) into: char_rename&yr.
		separated by '0A'x
		from cont_label
		where Type='C' & Y&yr.~="";
	quit;
	/*Make a list of all character values to be converted to character*/
	proc sql noprint;
		select compress(Name||"=put(orig__"||Name||","||Max_Len_c||".);") into: convert_char&yr.
		separated by '0A'x
		from cont_label
		where Type='C' & Y&yr.~="";
	quit;
	/*Make the first half of a length statement for all character values*/
	proc sql noprint;
		select compress(Name)||" $ "||Max_Len_c into: char_length&yr.
		separated by " "
		from cont_label
		where Type='C' & Y&yr.~="";
	quit;
	/*Make the 2nd half of a length statement for all numeric values*/
	proc sql noprint;
		select compress(Name)||" "||Max_Len_c into: num_length&yr.
		separated by " "
		from cont_label
		where Type='N' & Y&yr.~="";
	quit;

	%if &yr.=&startyr. %then %do;
		%put &&char_vars&yr;
		%put &&char_rename&yr;
		%put &&convert_char&yr;
		%put &&char_length&yr;
		%put &&num_length&yr;
	%end;
%end;

/*******************************************************************
 Update Each Year With New Lengths and VarType Conversions
*******************************************************************/
/*Convert the appropriate vars to character and set the appropriate length for each var*/
%do j=&startyr. %to &endyr.;
%let length_all = &&char_length&j. &&num_length&j.;

	data final_&ds.&j.;
		/*Set the length for all variables in this year's dataset*/
		length &length_all.; 
		set &ds.&j (rename=(&&char_rename&j.));
		&&convert_char&j. /*No semicolon here bc the semicolon is in the macrovar*/
		drop orig__:; /*All of the pre-converted vars had orig__ appended onto them in the proc sql*/
		&ds._year=&j.;
	run;
%end;

/*Append and output the assembled file*/
data datout.&ds.&startyr._&endyr.;
	set final_&ds.:;
run;

proc contents data=datout.&ds.&startyr._&endyr.; 
run;

/*Export content matrix to excel*/
ods tagsets.excelxp file="&out./contents_&ds._&startyr._&endyr..xml" style=sansPrinter;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Contents by Year" frozen_headers='yes');
proc print data=cont_label;
run;
ods tagsets.excelxp close;

/*Clean up work datasets*/
proc datasets library=work kill;
run;
%mend;
%All_Years(ds=&ds.,startyr=&startyr.,endyr=&endyr.,rename_xwalk=&rename_xwalk.);


