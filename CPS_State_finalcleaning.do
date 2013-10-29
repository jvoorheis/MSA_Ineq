cd "$MSAINEQ/Data/"

*initial cleaning for MSA-level analysis, CPS
replace hhincome=hhincome_cutoff1 if hhincome_cellmean>3000000 & year<1980 
replace hhincome_cellmean=hhincome_cutoff1 if hhincome_cellmean>3000000 & year<1980 
*Individual states are only identified post-1977 in the Public use CPS
drop if year<1977 
*drop if metarea>9996
sort serial year
egen hhsize = max(pernum), by(serial year)
keep year serial hhsize hwtsupp hhincome hhincome_cellmean hhincome_cutoff1 statefip topcoded
collapse (mean) hwtsupp=hwtsupp hhsize=hhsize hhincome_cutoff1=hhincome_cutoff1 hhincome_cellmean=hhincome_cellmean hhincome=hhincome (max) statefip=statefip topcoded=topcoded, by(serial year)
gen sqrt_equivinc = hhincome_cutoff1/sqrt(hhsize)
gen cellmean_equivinc = hhincome_cellmean/sqrt(hhsize)
gen sqrt_CPScellmean = hhincome/sqrt(hhsize)
replace sqrt_CPScellmean = 0.01 if sqrt_CPScellmean<=0
gen bottom_equivinc = (1-topcoded)*sqrt_equivinc
gen topcoded_equivinc = topcoded*sqrt_equivinc
save CPS_topcode_State.dta, replace


