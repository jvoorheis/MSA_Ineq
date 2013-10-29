*initial cleaning for MSA-level analysis, CPS
replace hhincome=hhincome_cutoff1 if hhincome_cellmean>3000000 & year<1980 
replace hhincome_cellmean=hhincome_cutoff1 if hhincome_cellmean>3000000 & year<1980 
*drop if metarea>9996
sort serial year
egen hhsize = max(pernum), by(serial year)
*keep year serial hhsize hwtsupp hhincome hhincome_cellmean hhincome_cutoff1 MSA_FIPS topcoded
*collapse (mean) hwtsupp=hwtsupp hhsize=hhsize hhincome=hhincome hhincome_cutoff1=hhincome_cutoff1 hhincome_cellmean=hhincome_cellmean (max) MSA_FIPS=MSA_FIPS topcoded=topcoded, by(serial year)


gen sqrt_equivinc = hhincome_cutoff1/sqrt(hhsize)
gen cellmean_equivinc = hhincome_cellmean/sqrt(hhsize)
gen sqrt_CPScellmean = hhincome/sqrt(hhsize)

replace sqrt_CPScellmean = 0.01 if sqrt_CPScellmean<=0
gen bottom_equivinc = (1-topcoded)*cellmean_equivinc
gen topcoded_equivinc = topcoded*cellmean_equivinc
*drop if age<18
keep metarea statefip county hwtsupp wtsupp age female marst race hispan educ schlcoll union sqrt_equivinc cellmean_equivinc sqrt_CPScellmean bottom_equivinc topcoded topcoded_equivinc hhsize serial year ind occ classwkr hrswork hcovany fedtax fedtaxac statetax stataxac margtax hinscaid hinscare hinspur hinsemp hinsmil eitcred mwpval ctccrd actccrd hhincome_cellmean hhincome_cutoff1 MSA_FIPS

save "$MSAINEQ/Data/CPS_topcode_MSA_demo_tax.dta", replace
