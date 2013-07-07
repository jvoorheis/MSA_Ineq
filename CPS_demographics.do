cd "$MSAINEQ/Data"
do "$MSAINEQ/Code/cps_00036.do"
drop if age<18
drop if age>=65
drop if metarea>=9996
recode educ (999=.)
gen college = educ>110 & educ~=.
gen highschool = educ>73 & educ~=.
gen govtwkr = classwkr==27 | classwkr == 28 | classwkr ==25
gen union_mem = union==2
gen union_cov = union ==2 | union ==3

