*initial cleaning for MSA-level analysis, CPS
replace hhincome=hhincome_cutoff1 if hhincome_cellmean>3000000 & year<1980 
replace hhincome_cellmean=hhincome_cutoff1 if hhincome_cellmean>3000000 & year<1980 
recode county (.=0)
drop if year<1992
*drop if metarea>9996
sort serial year
recode fica (99999=.)
egen hhsize = max(pernum), by(serial year)
*keep year serial hhsize hwtsupp hhincome hhincome_cellmean hhincome_cutoff1 MSA_FIPS topcoded
*collapse (mean) hwtsupp=hwtsupp hhsize=hhsize hhincome=hhincome hhincome_cutoff1=hhincome_cutoff1 hhincome_cellmean=hhincome_cellmean (max) MSA_FIPS=MSA_FIPS topcoded=topcoded, by(serial year)
gen rent_contrib = hhincome_cutoff1*0.3

gen sqrt_equivinc = hhincome_cutoff1/sqrt(hhsize)
gen cellmean_equivinc = hhincome_cellmean_alt/sqrt(hhsize)
gen sqrt_CPScellmean = hhincome/sqrt(hhsize)
gen sqrt_equivinc_pretrans = hhincome_cutoff1_pretrans/sqrt(hhsize)
gen cellmean_equivinc_pretrans = hhincome_cellmean_pretrans/sqrt(hhsize)


replace sqrt_CPScellmean = 0.01 if sqrt_CPScellmean<=0
gen bottom_equivinc = (1-topcoded)*cellmean_equivinc
gen topcoded_equivinc = topcoded*cellmean_equivinc
gen bottom_equivinc_pretrans = (1-topcoded)*cellmean_equivinc_pretrans
gen topcoded_equivinc_pretrans = topcoded*cellmean_equivinc_pretrans

save "$MSAINEQ/Data/CPS_topcode_MSA_demo_tax.dta", replace
