*initial cleaning for MSA-level analysis, CPS
replace hhincome=hhincome_cutoff1 if hhincome>3000000 & year<1980 
*drop if metarea>9996
sort serial year
egen hhsize = max(pernum), by(serial year)
keep year serial hhsize hwtsupp hhincome hhincome_cutoff1 hhincome_cutoff2 MSA_FIPS topcoded
collapse (mean) hwtsupp=hwtsupp hhsize=hhsize hhincome_cutoff1=hhincome_cutoff1 hhincome_cutoff2=hhincome_cutoff1 hhincome=hhincome (max) MSA_FIPS=MSA_FIPS topcoded=topcoded, by(serial year)
gen sqrt_equivinc = hhincome_cutoff2/sqrt(hhsize)
gen cellmean_equivinc = hhincome/sqrt(hhsize)
gen topcoded_equivinc = topcoded * sqrt_equivinc
gen bottom_equivinc = (1-topcoded) * sqrt_equivinc
save CPS_topcode_MSA.dta, replace
