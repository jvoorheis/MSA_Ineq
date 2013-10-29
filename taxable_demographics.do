
gen married = marst==1 | marst==2
gen divorced = marst ==3 | marst==4
gen widowed = marst==5
gen black = race==200
gen asian = race == 320 | race==652
gen otherrace = black == 0 & asian == 0 & race>100
gen lessthanhs = educ<=71
gen highschool = educ==72 | educ ==73
gen somecollege = educ >= 80 & educ <= 110
gen bachelors = educ == 111
gen postgrad = educ > 111
gen latino = (hispan~=. & hispan>0 & hispan<900)

sort year serial
egen total_fedtax = total(fedtax), by(year serial)
egen total_statax = total(statax), by(year serial)
egen total_eitc = total(eitcred),  by(year serial)
egen total_stataxac = total(stataxac), by(year serial)
egen total_fedtaxac = total(fedtaxac), by(year serial)



gen cellmean_eitc = hhincome_cellmean + total_eitc
gen cellmean_aftertax = .
replace cellmean_aftertax = hhincome_cellmean - total_stataxac - total_fedtaxac if year>=2005
replace cellmean_aftertax = hhincome_cellmean - total_statax - total_fedtax + actccrd + ctccrd + eitcred if year <2005
gen threshold_eitc = hhincome_cutoff1 + total_eitc
gen threshold_aftertax = .
replace threshold_aftertax = hhincome_cutoff1 - total_stataxac - total_fedtaxac if year>=2005
replace threshold_aftertax = hhincome_cutoff1 - total_statax - total_fedtax + actccrd + ctccrd + eitcred if year<2005



gen sqrt_equivinc_posttax = threshold_aftertax/sqrt(hhsize)
gen sqrt_equivinc_eitc = threshold_eitc/sqrt(hhsize)
gen cellmean_equivinc_posttax = cellmean_aftertax/sqrt(hhsize)
gen cellmean_equivinc_eitc = cellmean_eitc/sqrt(hhsize)


gen bottom_equivinc_posttax = (1-topcoded)*cellmean_equivinc_posttax
gen topcoded_equivinc_posttax = topcoded*cellmean_equivinc_posttax
gen bottom_equivinc_eitc = (1-topcoded)*cellmean_equivinc_eitc
gen topcoded_equivinc_eitc = topcoded*cellmean_equivinc_eitc
save "$MSAINEQ/Data/CPS_individual_tax.dta", replace 

collapse (mean) sqrt_equivinc=sqrt_equivinc cellmean_equivinc=cellmean_equivinc hwtsupp=hwtsupp hhsize=hhsize sqrt_equivinc_posttax=sqrt_equivinc_posttax sqrt_equivinc_eitc=sqrt_equivinc_eitc cellmean_equivinc_posttax=cellmean_equivinc_posttax cellmean_equivinc_eitc=cellmean_equivinc_eitc bottom_equivinc_posttax=bottom_equivinc_posttax  bottom_equivinc_eitc=bottom_equivinc_eitc topcoded_equivinc_eitc (max) statefip = statefip topcoded=topcoded, by(serial year)

save "$MSAINEQ/Data/CPS_household_tax.dta", replace
