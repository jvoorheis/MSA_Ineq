set more off
use "/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/MSA_covariates_cellmean.dta", clear
xtset MSA year
egen mean_pop=mean(Population), by(MSA)
replace Real_PersIncPC=Real_PersIncPC/1000000
replace Population = Population/1000000
replace UR = UR/100
gen log_gini = log(CPS_gini)
gen log_top1 = log(CPS_top1)
gen log_theil = log(CPS_theil)
gen log_9010 = log(CPS_9010)
gen log_UR = log(UR)
gen log_unionmem = log(union_mem+0.01)
gen log_DPI = log(Real_PersIncPC)
gen log_college = log(college_prop)
gen log_netrate = log(100-State_rate_wages)
gen log_capgains_netrate = log(100-State_rate_capgains)
gen log_pop = log(Population)
set matsize 1500

*drop if Census_9010>100
*drop if weighted_9010>100
*drop if weighted_9010<0

cd "/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/Tables"
local ineq gini top1 theil 9010
foreach i of local ineq{
	*unweighted
	qui reg CPS_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population, vce(robust)
	eststo unweighted_`i'_nofe
	qui estadd local fixed "no" , replace
	qui estadd local yearfe "no" , replace
	qui estadd local trend "no" , replace
	qui estadd local regweights "None", replace
	qui xtreg CPS_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population, fe vce(robust)
	eststo unweighted_`i'_fe
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "no" , replace
	qui estadd local trend "no" , replace
	qui estadd local regweights "None", replace
	qui xtreg CPS_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population i.year, fe vce(robust)
	eststo unweighted_`i'_yearfe
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
	qui estadd local trend "no" , replace
	qui estadd local regweights "None", replace
	qui xtreg CPS_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population i.year i.MSA#c.year, fe vce(robust)
	eststo unweighted_`i'_trend
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
	qui estadd local trend "yes" , replace
	qui estadd local regweights "None", replace

	*esttab unweighted_`i'_nofe unweighted_`i'_fe unweighted_`i'_yearfe unweighted_`i'_trend using unweighted_`i'.tex, style(tex) replace b se keep(union_mem UR Real_PersIncPC college_prop State_rate_wages Population) stats(fixed yearfe trend N, label("MSA FE" "MSA+year FE" "trend")) nomtitles note("no regression weights")

	*weighted by 1/mean(pop)
	qui reg CPS_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population [weight=mean_pop], vce(robust)
	eststo weighted_`i'_nofe
	qui estadd local fixed "no" , replace
	qui estadd local yearfe "no" , replace
	qui estadd local trend "no" , replace
	qui estadd local regweights "Population", replace
	qui xtreg CPS_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population [weight=mean_pop], fe vce(robust)
	eststo weighted_`i'_fe
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "no" , replace
	qui estadd local trend "no" , replace
	qui estadd local regweights "Population", replace
	qui xtreg CPS_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population i.year [weight=mean_pop], fe vce(robust)
	eststo weighted_`i'_yearfe
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
	qui estadd local trend "no" , replace
	qui estadd local regweights "Population", replace
	qui xtreg CPS_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population i.year i.MSA#c.year [weight=mean_pop], fe vce(robust)
	eststo weighted_`i'_trend
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
	qui estadd local trend "yes" , replace
	qui estadd local regweights "Population", replace

	esttab unweighted_`i'_fe unweighted_`i'_yearfe unweighted_`i'_trend weighted_`i'_fe weighted_`i'_yearfe weighted_`i'_trend using weighted_`i'.tex, style(tex) replace b se keep(union_mem UR Real_PersIncPC college_prop State_rate_wages Population) stats(fixed yearfe trend regweights N, label("MSA FE" "year FE" "trend" "Reg. Weights")) nomtitles 
	esttab unweighted_`i'_fe unweighted_`i'_yearfe unweighted_`i'_trend weighted_`i'_fe weighted_`i'_yearfe weighted_`i'_trend, b se keep(union_mem UR Real_PersIncPC college_prop State_rate_wages Population) stats(fixed yearfe trend  regweights N, label("MSA FE" "year FE" "trend" "Reg. Weights")) nomtitles 
}

local log_ineq log_gini log_theil log_9010 log_top1
local indepvars log_UR log_college log_DPI log_unionmem log_pop log_netrate

foreach i of local log_ineq{
	display "`i'"
	qui xtreg `i' `indepvars' if mean_pop>1000000, fe vce(robust)
	eststo log_`i'_fe
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "no" , replace
	qui estadd local trend "no" , replace
	qui estadd local regweights "None", replace
	qui xtreg `i' `indepvars' i.Region#i.year if mean_pop>1000000,  fe vce(robust)
	eststo log_`i'_yearfe
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
	qui estadd local trend "no" , replace
	qui estadd local regweights "None", replace
	qui xtreg `i' `indepvars' i.Region#i.year i.MSA#c.year if mean_pop>1000000, fe vce(robust)
	eststo log_`i'_trend
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
	qui estadd local trend "yes" , replace
	qui estadd local regweights "None", replace
	*With pop weights
	qui xtreg `i' `indepvars' [weight=mean_pop] if mean_pop>1000000, fe vce(robust)
	eststo log_w_`i'_fe
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "no" , replace
	qui estadd local trend "no" , replace
	qui estadd local regweights "None", replace
	qui xtreg `i' `indepvars' i.Region#i.year [weight=mean_pop] if mean_pop>1000000,  fe vce(robust)
	eststo log_w_`i'_yearfe
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
	qui estadd local trend "no" , replace
	qui estadd local regweights "None", replace
	qui xtreg `i' `indepvars' i.Region#i.year i.MSA#c.year [weight=mean_pop] if mean_pop>1000000, fe vce(robust)
	eststo log_w_`i'_trend
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
	qui estadd local trend "yes" , replace
	qui estadd local regweights "None", replace
	esttab log_`i'_fe log_`i'_yearfe log_`i'_trend log_w_`i'_fe log_w_`i'_yearfe log_w_`i'_trend, b se keep(log_UR log_college log_DPI log_unionmem log_pop log_netrate) stats(fixed yearfe trend  regweights N, label("MSA FE" "year FE" "trend" "Reg. Weights")) nomtitles 
}


local ineq gini top1 theil 9010
foreach i of local ineq{
	*unweighted
	display "`i'"
	qui xtreg weighted_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population i.year, fe vce(robust)
	qui eststo average_`i'_yearfe
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
	qui estadd local trend "no" , replace

	qui xtreg weighted_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population i.year i.MSA#c.year, fe vce(robust)
	qui eststo average_`i'_trend
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
	qui estadd local trend "yes" , replace
	*weighted by 1/mean(pop)

	qui xtreg weighted_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population i.year [weight=mean_pop], fe vce(robust)
	qui eststo average_w_`i'_yearfe
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
        qui estadd local trend "no" , replace

	qui xtreg weighted_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population i.year i.MSA#c.year [weight=mean_pop], fe vce(robust)
	qui eststo average_w_`i'_trend
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
	qui estadd local trend "yes" , replace
	esttab average_`i'_yearfe average_`i'_trend  average_w_`i'_yearfe  average_w_`i'_trend, b se keep(union_mem UR Real_PersIncPC college_prop State_rate_wages Population) stats(fixed yearfe trend N, label("FE" "year FE" "trend")) nomtitles note("Weighted by the inverse of population (analytic weights)")
}
local ineq gini top1 theil 9010
cd "/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/Tables"
foreach i of local ineq{
forval j=5(5)95{
	xi: qui rifreg CPS_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population i.MSA i.year, quantile(`j')
	qui eststo rifreg_`i'_`j'
}
	display "`i'"
	esttab rifreg_`i'_10 rifreg_`i'_30 rifreg_`i'_50 rifreg_`i'_70 rifreg_`i'_90, b se keep(union_mem UR Real_PersIncPC college_prop State_rate_wages Population)
	qui esttab rifreg_`i'_10 rifreg_`i'_30 rifreg_`i'_50 rifreg_`i'_70 rifreg_`i'_90 using rifreg_`i'.tex , style(tex) b se keep(union_mem UR Real_PersIncPC college_prop State_rate_wages Population) replace
}

local ineq gini top1 theil 9010
cd "/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/Tables"
foreach i of local ineq{
display "`i'"
forval j=5(5)95{
	xi: qui rifreg CPS_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population i.MSA i.year, quantile(`j')
	qui eststo rifreg_`i'_`j'
	display"`j'"
	estout rifreg_`i'_`j', cells("b(star fmt(3)) ci(par() fmt(3)) p(fmt(3))") stats(r2_p chi2 bic N) collabels("B" "CI" "P>z") keep(college_prop)
}

}

local ineq gini top1 theil 9010
cd "/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/Tables"
foreach i of local ineq{
forval j=10(20)90{
	xi: qui qreg2 CPS_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population i.MSA i.year, quantile(`j')
	qui eststo rifreg_`i'_`j'
}
	display "`i'"
	esttab rifreg_`i'_10 rifreg_`i'_30 rifreg_`i'_50 rifreg_`i'_70 rifreg_`i'_90, b se keep(union_mem UR Real_PersIncPC college_prop State_rate_wages Population)
	*qui esttab rifreg_`i'_10 rifreg_`i'_30 rifreg_`i'_50 rifreg_`i'_70 rifreg_`i'_90 using rifreg_`i'.tex , style(tex) b se keep(union_mem UR Real_PersIncPC college_prop State_rate_wages Population) replace
}

local ineq gini top1 theil 9010
foreach i of local ineq{
	*unweighted
	display "`i'"
	qui xtreg cellmean_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population i.year, fe vce(robust)
	qui eststo average_`i'_yearfe
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
	qui estadd local trend "no" , replace

	qui xtreg cellmean_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population i.year i.MSA#c.year, fe vce(robust)
	qui eststo average_`i'_trend
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
	qui estadd local trend "yes" , replace
	*weighted by 1/mean(pop)

	qui xtreg cellmean_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population i.year [weight=mean_pop], fe vce(robust)
	qui eststo average_w_`i'_yearfe
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
        qui estadd local trend "no" , replace

	qui xtreg cellmean_`i' union_mem UR Real_PersIncPC college_prop State_rate_wages Population i.year i.MSA#c.year [weight=mean_pop], fe vce(robust)
	qui eststo average_w_`i'_trend
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
	qui estadd local trend "yes" , replace
	esttab average_`i'_yearfe average_`i'_trend  average_w_`i'_yearfe  average_w_`i'_trend, b se keep(union_mem UR Real_PersIncPC college_prop State_rate_wages Population) stats(fixed yearfe trend N, label("FE" "year FE" "trend")) nomtitles note("Weighted by the inverse of population (analytic weights)")
}


*Checking REgion x YEar FE as an alternative
local ineq gini top1 theil 9010
foreach i of local ineq{
	*unweighted
	
	
	qui xtreg CPS_`i' union_mem UR Real_PersIncPC college_prop State_rate_capgains Population i.year if mean_pop>1000000, fe vce(robust)
	eststo unweighted_`i'_yearfe
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
	qui estadd local trend "no" , replace
	qui estadd local regweights "None", replace
	
	qui xtreg CPS_`i' union_mem UR Real_PersIncPC college_prop State_rate_capgains Population i.year i.MSA#c.year if mean_pop>1000000, fe vce(robust)
	eststo unweighted_`i'_trend
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
	qui estadd local trend "yes" , replace
	qui estadd local regweights "None", replace

	qui xtreg CPS_`i' union_mem UR Real_PersIncPC college_prop State_rate_capgains Population i.Region#i.year if mean_pop>1000000, fe vce(robust)
	eststo unweighted_r_`i'_yearfe
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
	qui estadd local trend "no" , replace
	qui estadd local regweights "None", replace
	
	qui xtreg CPS_`i' union_mem UR Real_PersIncPC college_prop State_rate_capgains Population i.Region#i.year i.MSA#c.year if mean_pop>1000000, fe vce(robust)
	eststo unweighted_r_`i'_trend
	qui estadd local fixed "yes" , replace
	qui estadd local yearfe "yes" , replace
	qui estadd local trend "yes" , replace
	qui estadd local regweights "None", replace
	display""
	display "`i'"
	display""
	esttab  unweighted_`i'_yearfe unweighted_`i'_trend unweighted_r_`i'_yearfe unweighted_r_`i'_trend , b se keep(union_mem UR Real_PersIncPC college_prop State_rate_capgains Population) stats(fixed yearfe trend regweights N, label("MSA FE" "year FE" "trend" "Reg. Weights")) nomtitles 
}




