sort statefip year
gen topcoded=0
egen max_incwage = max(incwage), by(statefip year)
replace topcoded=1 if incwage==max_incwage & incwage~=.
egen topcode_incwage=max(incwage) if incwage<max_incwage & incwage~=., by(statefip year)
replace topcode_incwage=max_incwage if year<=1980 
egen topcode_incwage1 = max(topcode_incwage), by(statefip year)
replace incwage=topcode_incwage1 if incwage==max_incwage & incwage~=.

egen max_incbus = max(incbus), by(statefip year)
replace topcoded=1 if incbus==max_incbus & incbus~=.
egen topcode_incbus=max(incbus) if incbus<max_incbus & incbus~=., by(statefip year)
replace topcode_incbus=max_incbus if year<=1980
egen topcode_incbus1 = max(topcode_incbus), by(statefip year)
replace incbus=topcode_incbus1 if incbus==max_incbus & incbus~=.

egen max_incbus00 = max(incbus00), by(statefip year)
replace topcoded=1 if incbus00==max_incbus00 & incbus00~=.
egen topcode_incbus00=max(incbus00) if incbus00<max_incbus00 & incbus00~=., by(statefip year)
replace topcode_incbus00=max_incbus00 if year<=1980
egen topcode_incbus001 = max(topcode_incbus00), by(statefip year)
replace incbus00=topcode_incbus001 if incbus00==max_incbus00 & incbus00~=.

egen max_incfarm = max(incfarm), by(statefip year)
replace topcoded=1 if incfarm==max_incfarm & incfarm~=.
egen topcode_incfarm=max(incfarm) if incfarm<max_incfarm & incfarm~=., by(statefip year)
replace topcode_incfarm=max_incfarm if year<=1980
egen topcode_incfarm1 = max(topcode_incfarm), by(statefip year)
replace incfarm=topcode_incfarm1 if incfarm==max_incfarm & incfarm~=.

egen max_incinvst = max(incinvst), by(statefip year)
replace topcoded=1 if incinvst==max_incinvst & incinvst ~=.
egen topcode_incinvst=max(incinvst) if incinvst<max_incinvst & incinvst~=., by(statefip year)
replace topcode_incinvst=max_incinvst if year<=1980
egen topcode_incinvst1 = max(topcode_incinvst), by(statefip year)
replace incinvst=topcode_incinvst1 if incinvst==max_incinvst & incinvst~=.

egen max_incretir = max(incretir), by(statefip year)
replace topcoded=1 if incretir==max_incretir & incretir ~=.
egen topcode_incretir=max(incretir) if incretir<max_incretir & incretir~=., by(statefip year)
replace topcode_incretir=max_incretir if year<=1980
egen topcode_incretir1 = max(topcode_incretir), by(statefip year)
replace incretir=topcode_incretir1 if incretir==max_incretir & incretir~=.

egen max_incother = max(incother), by(statefip year)
replace topcoded=1 if incother==max_incother & incother ~=.
egen topcode_incother=max(incother) if incother<max_incother & incother~=., by(statefip year)
replace topcode_incother=max_incother if year<=1980
egen topcode_incother1 = max(topcode_incother), by(statefip year)
replace incother=topcode_incother1 if incother==max_incother & incother~=.

*SS, Supplemental SS, welfare are not replaced with cell means
egen max_incss = max(incss), by(statefip year)
replace topcoded=1 if incss==max_incss & incss~=.

egen max_incsupp = max(incsupp), by(statefip year)
replace topcoded=1 if incsupp==max_incsupp & incsupp~=.

egen max_incwelfr = max(incwelfr), by(statefip year)
replace topcoded=1 if incwelfr==max_incwelfr & incwelfr~=.

egen inctot_corr_thresh = rowtotal(incwage incbus incbus00  incfarm   incinvst  incretir  incss  incwelfr  incsupp  incother)

sort serial year
egen hhincome_topcode = total(inctot_corr_thresh), by(serial year)
