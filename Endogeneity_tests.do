use "/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/MSA_covariates_cellmean.dta", clear
replace Population = Population/1000000
replace Real_PersIncPC = Real_PersIncPC/1000
gen srw = State_rate_wages
gen RPI = Real_PersIncPC
sort MSA
by MSA: center CPS_top1 CPS_theil CPS_9010 CPS_gini union_mem college_prop Population RPI srw UR
gen c_union = c_union_mem
gen c_pop = c_Population
gen c_coll = c_college_prop

local ineq gini top1 theil 9010
foreach i of local ineq{
display "`i'"
qui xi: ivreg2h c_CPS_`i' (c_union c_coll c_RPI c_UR c_pop c_srw c_UR=) i.year
est sto ivname
qui reg c_CPS_`i' c_union c_coll c_pop c_RPI c_srw i.year c_UR
est sto olsname
hausman ivname olsname
}



qui xi: ivreg2h c_CPS_gini (c_union=) c_coll c_pop c_RPI c_srw c_UR i.year
est sto ivname
qui reg c_CPS_gini c_union c_coll c_pop c_RPI c_srw i.year  c_UR
est sto olsname
hausman ivname olsname

qui xi: ivreg2h c_CPS_gini c_union (c_coll=) c_pop c_RPI c_srw  c_UR i.year
est sto ivname
qui reg c_CPS_gini c_union c_coll c_pop c_RPI c_srw i.year c_UR
est sto olsname
hausman ivname olsname

qui xi: ivreg2h c_CPS_gini c_union c_coll (c_pop=) c_RPI c_srw c_UR i.year
est sto ivname
qui reg c_CPS_gini c_union c_coll c_pop c_RPI c_srw i.year c_UR
est sto olsname
hausman ivname olsname

qui xi: ivreg2h c_CPS_gini c_union c_coll c_pop (c_RPI=) c_srw c_UR i.year
est sto ivname
qui reg c_CPS_gini c_union c_coll c_pop c_RPI c_srw i.year c_UR
est sto olsname
hausman ivname olsname

qui xi: ivreg2h c_CPS_gini c_union c_coll c_pop c_RPI (c_srw=) c_UR i.year
est sto ivname
qui reg c_CPS_gini c_union c_coll c_pop c_RPI c_srw i.year c_UR
est sto olsname
hausman ivname olsname

qui xi: ivreg2h c_CPS_gini c_union c_coll c_pop c_RPI c_srw (c_UR=) i.year
est sto ivname
qui reg c_CPS_gini c_union c_coll c_pop c_RPI c_srw i.year c_UR
est sto olsname
hausman ivname olsname

qui xi: ivreg2h c_CPS_gini (c_union c_coll=) c_RPI c_UR c_pop c_srw c_UR i.year
est sto ivname
qui reg c_CPS_gini c_union c_coll c_pop c_RPI c_srw i.year c_UR
est sto olsname
hausman ivname olsname



