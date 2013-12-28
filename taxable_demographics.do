recode fica (99999=.)
gen married = marst==1 | marst==2
gen divorced = marst ==3 | marst==4
gen widowed = marst==5
replace black = race==200
gen asian = race == 320 | race==652
gen otherrace = black == 0 & asian == 0 & race>100
gen lessthanhs = educ<=71
gen highschool = educ==72 | educ ==73
gen somecollege = educ >= 80 & educ <= 110
gen bachelors = educ == 111
gen postgrad = educ > 111
replace latino = (hispan~=. & hispan>0 & hispan<900)
gen union_dens = union>=2
gen inschool = schlcoll>0 & schlcoll<5
recode ind (0=.) (992=.)
recode occ (0=.) (992=.)
gen ind0 = (ind<1000 & ind>0 & year>=2003) | (ind<100 & ind>0 & year<2003)
gen ind1 = (ind<2000 & ind>1000 & year>=2003) | (ind<200 & ind>100 & year<2003)
gen ind2 = (ind<3000 & ind>2000 & year>=2003) | (ind<300 & ind>200 & year<2003)
gen ind3 = (ind<4000 & ind>3000 & year>=2003) | (ind<400 & ind>300 & year<2003)
gen ind4 = (ind<5000 & ind>4000 & year>=2003) | (ind<500 & ind>400 & year<2003)
gen ind5 = (ind<6000 & ind>5000 & year>=2003) | (ind<600 & ind>500 & year<2003)
gen ind6 = (ind<7000 & ind>6000 & year>=2003) | (ind<700 & ind>600 & year<2003)
gen ind7 = (ind<8000 & ind>7000 & year>=2003) | (ind<800 & ind>700 & year<2003)
gen ind8 = (ind<9000 & ind>8000 & year>=2003) | (ind<900 & ind>800 & year<2003)
gen ind9 = (ind<10000 & ind>9000 & year>=2003) | (ind<1000 & ind>900 & year<2003)

gen occ0 = (occ<1000 & occ>0 & year>=2003) | (occ<100 & occ>0 & year<2003)
gen occ1 = (occ<2000 & occ>1000 & year>=2003) | (occ<200 & occ>100 & year<2003)
gen occ2 = (occ<3000 & occ>2000 & year>=2003) | (occ<300 & occ>200 & year<2003)
gen occ3 = (occ<4000 & occ>3000 & year>=2003) | (occ<400 & occ>300 & year<2003)
gen occ4 = (occ<5000 & occ>4000 & year>=2003) | (occ<500 & occ>400 & year<2003)
gen occ5 = (occ<6000 & occ>5000 & year>=2003) | (occ<600 & occ>500 & year<2003)
gen occ6 = (occ<7000 & occ>6000 & year>=2003) | (occ<700 & occ>600 & year<2003)
gen occ7 = (occ<8000 & occ>7000 & year>=2003) | (occ<800 & occ>700 & year<2003)
gen occ8 = (occ<9000 & occ>8000 & year>=2003) | (occ<900 & occ>800 & year<2003)
gen occ9 = (occ<10000 & occ>9000 & year>=2003) | (occ<1000 & occ>900 & year<2003)
gen self_emp = classwkr<21
gen pub_sector = classwkr>21 & classwkr<29
gen priv_sector = classwkr==21


sort year serial
egen total_fedtax = total(fedtax), by(year serial)
egen total_statax = total(statetax), by(year serial)
egen total_eitc = total(eitcred),  by(year serial)
egen total_stataxac = total(stataxac), by(year serial)
egen total_fedtaxac = total(fedtaxac), by(year serial)
egen total_childcred = total(ctccrd),  by(year serial)
egen total_achildcred = total(actccrd),  by(year serial)
egen total_payroll = total(fica), by(year serial)
egen hh_margrate = max(margtax), by(year serial)

egen in_kind = rowtotal(ffngcare ffngcaid)
egen hh_inkind = total(in_kind), by(year serial)
egen hh_emcontrib = total(emcontrb), by(year serial)
replace hh_inkind = hh_inkind + stampval + heatval
gen hh_rentsub = pubhous+rentsub

egen hh_wic = total(gotwic), by(year serial)




gen cellmean_eitc = hhincome_cellmean_alt+total_eitc
gen cellmean_aftertax = .
replace cellmean_aftertax = hhincome_cellmean_alt - total_stataxac - total_fedtaxac -total_payroll if year>=2005
replace cellmean_aftertax = hhincome_cellmean_alt - total_statax - total_fedtax -total_payroll + total_eitc + total_childcred + total_achildcred if year <2005
gen cellmean_posttax_pretrans = .
replace cellmean_posttax_pretrans = hhincome_cellmean_pretrans- total_stataxac - total_fedtaxac - total_payroll if year>=2005
replace cellmean_posttax_pretrans = hhincome_cellmean_pretrans - total_statax - total_fedtax + total_eitc + total_childcred + total_achildcred - total_payroll if year <2005
gen threshold_eitc = hhincome_cutoff1 + total_eitc
gen threshold_aftertax = .
replace threshold_aftertax = hhincome_cutoff1 - total_stataxac - total_fedtaxac -total_payroll if year>=2005
replace threshold_aftertax = hhincome_cutoff1 - total_statax - total_fedtax + total_eitc + total_childcred + total_achildcred -total_payroll if year<2005
gen threshold_posttax_pretrans = .
replace threshold_posttax_pretrans = hhincome_cutoff1_pretrans - total_stataxac - total_fedtaxac -total_payroll if year>=2005
replace threshold_posttax_pretrans = hhincome_cutoff1_pretrans - total_statax - total_fedtax + total_eitc + total_childcred + total_achildcred -total_payroll if year<2005

gen cellmean_postinkind = cellmean_aftertax + hh_inkind
gen cellmean_postequity = cellmean_postinkind + housret
gen threshold_postinkind = threshold_aftertax + hh_inkind
gen threshold_postequity = threshold_postinkind + housret


gen sqrt_equivinc_posttax = threshold_aftertax/sqrt(hhsize)
gen sqrt_equivinc_eitc = threshold_eitc/sqrt(hhsize)
gen cellmean_equivinc_posttax = cellmean_aftertax/sqrt(hhsize)
gen cellmean_equivinc_eitc = cellmean_eitc/sqrt(hhsize)

gen sqrt_equivinc_postinkind = threshold_postinkind/sqrt(hhsize)
gen sqrt_equivinc_postequity = threshold_postequity/sqrt(hhsize)
gen cellmean_equivinc_postinkind = cellmean_postinkind/sqrt(hhsize)
gen cellmean_equivinc_postequity = cellmean_postequity/sqrt(hhsize)


gen bottom_equivinc_posttax = (1-topcoded)*cellmean_equivinc_posttax
gen topcoded_equivinc_posttax = topcoded*cellmean_equivinc_posttax
gen bottom_equivinc_eitc = (1-topcoded)*cellmean_equivinc_eitc
gen topcoded_equivinc_eitc = topcoded*cellmean_equivinc_eitc

gen sqrt_equivinc_postpre = threshold_posttax_pretrans/sqrt(hhsize)
gen cellmean_equivinc_postpre = cellmean_posttax_pretrans/sqrt(hhsize)
gen bottom_equivinc_postpre = (1-topcoded)*cellmean_equivinc_postpre
gen topcoded_equivinc_postpre = topcoded*cellmean_equivinc_postpre

gen hhincome_pretax_broad = hhincome_cellmean_pretrans + hh_emcontrib
gen hhincome_posttax_broad = cellmean_postinkind + hh_emcontrib
gen threshold_pretax_broad = hhincome_cutoff1_pretrans + hh_emcontrib
gen threshold_posttax_broad = threshold_aftertax + hh_emcontrib

replace cellmean_equivinc_posttax = 0 if cellmean_equivinc_posttax<0
replace topcoded_equivinc_posttax = 0 if topcoded_equivinc_posttax<0
replace bottom_equivinc_posttax = 0 if bottom_equivinc_posttax < 0
replace sqrt_equivinc_posttax = 0 if sqrt_equivinc_posttax < 0

replace cellmean_equivinc_postpre = 0 if cellmean_equivinc_postpre<0
replace topcoded_equivinc_postpre = 0 if topcoded_equivinc_postpre<0
replace bottom_equivinc_postpre = 0 if bottom_equivinc_postpre < 0
replace sqrt_equivinc_postpre = 0 if sqrt_equivinc_postpre < 0

gen State = statefip
replace statefip=state

keep year serial hwtsupp statefip metarea county housret proptax pubhous rentsub lunchsub frelunch pernum wtsupp age schlcoll occ ind gotwic MSA_FIPS latino black white female fulltime eligible_rooms hhincome_cellmean hhincome_cellmean_alt hhincome_cellmean_pretrans topcoded hh_topcoded hhincome_cutoff1 hhincome_cutoff1_pretrans hhsize rent_contrib sqrt_equivinc cellmean_equivinc sqrt_CPScellmean sqrt_equivinc_pretrans cellmean_equivinc_pretrans bottom_equivinc topcoded_equivinc bottom_equivinc_pretrans topcoded_equivinc_pretrans married divorced widowed asian otherrace lessthanhs highschool somecollege bachelors postgrad  hh_rentsub hh_wic cellmean_eitc cellmean_aftertax cellmean_posttax_pretrans threshold_eitc threshold_aftertax threshold_posttax_pretrans cellmean_postinkind cellmean_postequity threshold_postinkind threshold_postequity sqrt_equivinc_posttax sqrt_equivinc_eitc cellmean_equivinc_posttax cellmean_equivinc_eitc sqrt_equivinc_postinkind sqrt_equivinc_postequity cellmean_equivinc_postinkind cellmean_equivinc_postequity bottom_equivinc_posttax topcoded_equivinc_posttax bottom_equivinc_eitc topcoded_equivinc_eitc sqrt_equivinc_postpre cellmean_equivinc_postpre bottom_equivinc_postpre topcoded_equivinc_postpre hhincome_pretax_broad hhincome_posttax_broad threshold_pretax_broad threshold_posttax_broad union_dens inschool State hh_margrate margtax ind0 ind1 ind2 ind3 ind4 ind5 ind6 ind7 ind8 ind9 occ0 occ1 occ2 occ3 occ4 occ5 occ6 occ7 occ8 occ9 self_emp pub_sector priv_sector union

save "$MSAINEQ/Data/CPS_individual_tax.dta", replace 



collapse (mean) sqrt_equivinc=sqrt_equivinc cellmean_equivinc=cellmean_equivinc hwtsupp=hwtsupp hhsize=hhsize sqrt_equivinc_posttax=sqrt_equivinc_posttax sqrt_equivinc_eitc=sqrt_equivinc_eitc cellmean_equivinc_posttax=cellmean_equivinc_posttax cellmean_equivinc_eitc=cellmean_equivinc_eitc bottom_equivinc_posttax=bottom_equivinc_posttax  bottom_equivinc_eitc=bottom_equivinc_eitc topcoded_equivinc_eitc=topcoded_equivinc_eitc topcoded_equivinc_posttax = topcoded_equivinc_posttax topcoded_equivinc=topcoded_equivinc bottom_equivinc = bottom_equivinc cellmean_equivinc_pretrans = cellmean_equivinc_pretrans sqrt_equivinc_pretrans=sqrt_equivinc_pretrans bottom_equivinc_pretrans=bottom_equivinc_pretrans topcoded_equivinc_pretrans=topcoded_equivinc_pretrans cellmean_equivinc_postpre=cellmean_equivinc_postpre bottom_equivinc_postpre=bottom_equivinc_postpre topcoded_equivinc_postpre=topcoded_equivinc_postpre sqrt_equivinc_postpre =sqrt_equivinc_postpre cellmean_postequity=cellmean_postequity threshold_postequity=threshold_postequity frelunch=frelunch hh_wic = hh_wic hhincome_pretax_broad=hhincome_pretax_broad hhincome_posttax_broad=hhincome_posttax_broad hh_rentsub=hh_rentsub rent_contrib=rent_contrib eligible_rooms = eligible_rooms proptax = proptax housret=housret threshold_pretax_broad =threshold_pretax_broad threshold_posttax_broad =threshold_posttax_broad (max) MSA_FIPS=MSA_FIPS statefip = statefip topcoded=topcoded county=county, by(serial year)

sum if cellmean_equivinc_posttax<0
tab year if cellmean_equivinc_posttax<0
replace cellmean_equivinc_posttax = 0 if cellmean_equivinc_posttax<0
replace topcoded_equivinc_posttax = 0 if topcoded_equivinc_posttax<0
replace bottom_equivinc_posttax = 0 if bottom_equivinc_posttax < 0
replace sqrt_equivinc_posttax = 0 if sqrt_equivinc_posttax < 0

replace cellmean_equivinc_postpre = 0 if cellmean_equivinc_postpre<0
replace topcoded_equivinc_postpre = 0 if topcoded_equivinc_postpre<0
replace bottom_equivinc_postpre = 0 if bottom_equivinc_postpre < 0
replace sqrt_equivinc_postpre = 0 if sqrt_equivinc_postpre < 0

save "$MSAINEQ/Data/CPS_household_tax.dta", replace


