#Using plug-in estimates of Lorenz ordinates as dependent variable
use "/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Lorenz_state_memariates.dta", clear
replace ord = round(100*ord)

levelsof ord, local(ordinates)

foreach i of local ordinates{
	di "`i'"
	qui reg LC_ord union_mem median_educ college_prop Real_PersIncPC State_rate_wages RD_percap popdens UR  i.State i.year if ord==`i' [weight=Population], cluster(State)
	eststo rob_fe
	qui reg LC_ord union_mem median_educ college_prop Real_PersIncPC State_rate_wages RD_percap popdens UR  i.State i.year i.State#c.year if ord==`i' [weight=Population], cluster(State)
	eststo rob_trend
	qui reg LC_ord union_mem median_educ college_prop Real_PersIncPC State_rate_wages RD_percap popdens UR  i.Region i.year i.Region#i.year if ord==`i' [weight=Population], cluster(State)
	eststo rob_reg
	esttab rob_fe rob_trend rob_reg, b se keep(union_mem median_educ college_prop Real_PersIncPC State_rate_wages RD_percap popdens UR)
}

