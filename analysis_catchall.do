set more off
clear
do "$MSAINEQ/Code/CPS_read_in_clean.do"
clear
use "CPS_raw_microdata.dta"
do "$MSAINEQ/Code/CPS Topcoding Adjustments.do"
clear
use "CPS_topcodes.dta"
do "$MSAINEQ/Code/CPS_topcode_cutoffs.do"
*For MSA analysis, final cleaning step
clear
use "CPS_topcodes.dta"
do "$MSAINEQ/Code/CPS_MSA_finalcleaning.do"


*For State-level analysis, final cleaning step
clear
use "$MSAINEQ/Data/CPS_topcodes.dta"
do "$MSAINEQ/Code/CPS_State_finalcleaning.do"

exit, clear