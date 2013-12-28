*Replacing Cellmean values with cutoffs & reconstructing "topcoded" total income amounts, step 2: imposing topcode cutoffs, and reconstructing truncating incomes

*Creating Topcoded Flag

*Flagging all Topcoded observations (note: multiple income sources can be topcoded for a single observation)
*Total Income (post 1988)
replace inclongj=99999 if inclongj>=99999 & year<=1995 & inclongj~=.
replace inclongj=150000 if inclongj>=150000 & year>=1996 & year<=2002 & inclongj~=.
replace inclongj=200000 if inclongj>=200000 & year>=2003 & year<=2010 & inclongj~=.
replace inclongj=250000 if inclongj>=250000 & year>=2011 & inclongj~=.

*Wages
replace incwage=50000 if incwage>=50000 & year<=1981 & incwage~=.
replace incwage=75000 if incwage>=75000 & year>=1982 & year<=1984 & incwage~=.
replace incwage=99999 if incwage>=99999 & year>=1985 & year<=1987 & incwage~=.
replace oincwage=99999 if oincwage>=99999 & year>=1988 & year<=1995 & oincwage~=.
replace oincwage=25000 if oincwage>=25000 & year>=1996 & year<=2002 & oincwage~=.
replace oincwage=35000 if oincwage>=35000 & year>=2003 & year<=2010 & oincwage~=.
replace oincwage=47000 if oincwage>=47000 & year==2011 & oincwage~=.
replace oincwage=50000 if oincwage>=50000 & year==2012 & oincwage~=.

*Business/Self Employment Income
replace incbus=50000 if incbus>=50000 & year<=1981 &incbus~=.
replace incbus=75000 if incbus>=75000 & year>=1982 & year<=1984 &incbus~=.
replace incbus=99999 if incbus>=99999 & year>=1985 & year<=1987 &incbus~=.
replace oincbus=99999 if oincbus>=99999 & year>=1988 & year<=1995 &oincbus~=.
replace oincbus=40000 if oincbus>=40000 & year>=1996 & year<=2002 &oincbus~=.
replace oincbus=50000 if oincbus>=50000 & year>=2003 & year<=2010 &oincbus~=.
replace oincbus=60000 if oincbus>=60000 & year==2011 &oincbus~=.
replace oincbus=60000 if oincbus>=60000 & year==2012 &oincbus~=.

*Farm Income
replace incfarm=50000 if incfarm>=50000 & year<=1981 &incfarm~=.
replace incfarm=75000 if incfarm>=75000 & year>=1982 & year<=1984 &incfarm~=.
replace incfarm=99999 if incfarm>=99999 & year>=1985 & year<=1987 &incfarm~=.
replace oincfarm=99999 if oincfarm>=99999 & year>=1988 & year<=1995 &oincfarm~=.
replace oincfarm=25000 if oincfarm>=25000 & year>=1996 & year<=2002 &oincfarm~=.
replace oincfarm=25000 if oincfarm>=25000 & year>=2003 & year<=2010 &oincfarm~=.
replace oincfarm=30000 if oincfarm>=30000 & year==2011 &oincfarm~=.
replace oincfarm=40000 if oincfarm>=40000 & year==2012 &oincfarm~=.

*SS Income
replace incss=9999 if incss>=9999 & year<=1981 &incss~=.
replace incss=19999 if incss>=19999 & year>=1982 & year<=1987 &incss~=.
replace incss=29999 if incss>=29999 & year>=1988 & year<=1993 &incss~=.
replace incss=49999 if incss>=49999 & year>=1994 &incss~=.

*SSI income
replace incssi=5999 if incssi>=5999 & year<=1984 &incssi~=.
replace incssi=9999 if incssi>=9999 & year>=1985 & year<=1995 &incssi~=.
replace incssi=25000 if incssi>=25000 & year>=1996 &incssi~=.

*Interest Income (Interest income had topcodes hardcoded in IPUMS, so we just need to insert a flag)
replace incint=50000 if incint>=50000 & year<=1981 &incint~=.
replace incint=75000 if incint>=75000 & year>=1982 & year<=1984 &incint~=.
replace incint=99999 if incint>=99999 & year>=1985 & year<=1998 &incint~=.
replace incint=35000 if incint>=35000 & year>=1999 & year<=2002 &incint~=.
replace incint=25000 if incint>=25000 & year>=2003 & year<=2010 &incint~=.
replace incint=24000 if incint>=24000 & year==2011 &incint~=.
replace incint=22000 if incint>=22000 & year==2012 &incint~=.

*Public Assistance Income
replace incwelfr=19999 if incwelfr>=19999 & year<=1993 & incwelfr~=.
replace incwelfr=24999 if incwelfr>=24999 & year>=1994 & incwelfr~=.

*Dividend and Rental Income (Combined 1968-1987)
replace incidr=50000 if incidr>=50000 & year<=1981 & incidr~=.
replace incdrt=50000 if incdrt>=50000 & year<=1981 & incdrt~=.
replace incdrt=75000 if incdrt>=75000 & year>=1982 & year<=1984 & incdrt~=.
replace incdrt=99999 if incdrt>=99999 & year>=1985 & incdrt~=.
replace incdivid=99999 if incdivid>=99999 & year<=1998 & incdivid~=.
replace incdivid=15000 if incdivid>=15000 & year>=1999 & year<=2010 & incdivid~=.
replace incdivid=20000 if incdivid>=20000 & year>=2011 & incdivid~=.
replace incrent=99999 if incrent>=99999 & year<=1998 & incrent~=.
replace incrent=25000 if incrent>=25000 & year>=1999 & year<=2002 & incrent~=.
replace incrent=40000 if incrent>=40000 & year>=2003 & year<=2010 & incrent~=.
replace incrent=50000 if incrent>=50000 & year==2011 & incrent~=.
replace incrent=60000 if incrent>=60000 & year==2012 & incrent~=.

*Gov't Assistnance/Vet/Wk comp (combined pre 1988) - Post 1988, already hardcoded & corrected in CPS_read_in_clean.do
replace incgov=29999 if incgov>=29999 &incgov~=.
replace incwkcom=99999 if incwkcom>=99999 & incwkcom~=.
replace incvet=99999 if incvet>=99999 & incvet~=.

*Retirement Income
replace incretir=50000 if incretir>=50000 & year<=1981 &incretir~=.
replace incretir=75000 if incretir>=75000 & year>=1982 & year<=1984 &incretir~=.
replace incretir=99999 if incretir>=99999 & year>=1985 & year<=1987 &incretir~=.
replace increti1=99999 if increti1>=99999 & year>=1988 & year<=1998 &increti1~=.
replace increti1=45000 if increti1>=45000 & year>=1999 & year<=2010 &increti1~=.
replace increti1=64000 if increti1>=64000 & year==2011 &increti1~=.
replace increti1=67000 if increti1>=67000 & year==2012 &increti1~=.
replace increti2=99999 if increti2>=99999 & year>=1988 & year<=1998 &increti2~=.
replace increti2=45000 if increti2>=45000 & year>=1999 & year<=2010 &increti2~=.
replace increti2=64000 if increti2>=64000 & year==2011 &increti2~=.
replace increti2=67000 if increti2>=67000 & year==2012 &increti2~=.

*Other Income
replace incaloth=50000 if incaloth>=50000 & year<=1981 &incaloth~=.
replace incaloth=75000 if incaloth>=75000 & year>=1982 & year<=1984 &incaloth~=.
replace incaloth=99999 if incaloth>=99999 & year>=1985 & year<=1987 &incaloth~=.
replace incother=99999 if incother>=99999 & year>=1985 & year<=1998 &incother~=.
replace incother=25000 if incother>=25000 & year>=1999 & year<=2010 &incother~=.
replace incother=30000 if incother>=30000 & year==2011 &incother~=.
replace incother=31200 if incother>=31200 & year==2012 &incother~=.

*Alimony
replace incalim=99999 if incalim>=99999 & year<=1998 & incalim~=.
replace incalim=50000 if incalim>=50000 & year==1999 & incalim~=.
replace incalim=40000 if incalim>=40000 & year>=2000 & year<=2002 & incalim~=.
replace incalim=45000 if incalim>=45000 & year>=2003 & year<=2010 & incalim~=.
replace incalim=66000 if incalim>=66000 & year==2011 & incalim~=.
replace incalim=96000 if incalim>=96000 & year==2012 & incalim~=.

*Child Support
replace incchild=99999 if incchild>=99999 & year<=1998 & incchild~=.
replace incchild=15000 if incchild>=15000 & year>=1999 & year<=2010 & incchild~=.
replace incchild=21000 if incchild>=21000 & year==2011 & incchild~=.
replace incchild=18300 if incchild>=18300 & year==2012 & incchild~=.

*Unemployment
replace incunemp=99999 if incunemp>=99999 &incunemp~=.

*Survivor Benefits
replace incsurv=99999 if incsurv>=99999 & year<=1987 & incsurv~=.
replace incsurv1=99999 if incsurv1>=99999 & year>=1988 & year<=1998 & incsurv1~=.
replace incsurv1=50000 if incsurv1>=50000 & year>=1999 & year<=2010 & incsurv1~=.
replace incsurv1=57600 if incsurv1>=57600 & year==2011 & incsurv1~=.
replace incsurv1=75000 if incsurv1>=75000 & year==2012 & incsurv1~=.
replace incsurv2=99999 if incsurv2>=99999 & year>=1988 & year<=1998 & incsurv2~=.
replace incsurv2=50000 if incsurv2>=50000 & year>=1999 & year<=2010 & incsurv2~=.
replace incsurv2=57600 if incsurv2>=57600 & year==2011 & incsurv2~=.
replace incsurv2=75000 if incsurv2>=75000 & year==2012 & incsurv2~=.

*Disability Benefits
replace incdisab=99999 if incdisab>=99999 & year<=1987 & incdisab~=.
replace incdisa1=99999 if incdisa1>=99999 & year>=1988 & year<=1998 & incdisa1~=.
replace incdisa1=35000 if incdisa1>=35000 & year>=1999 & year<=2010 & incdisa1~=.
replace incdisa1=48000 if incdisa1>=48000 & year==2011 & incdisa1~=.
replace incdisa1=44000 if incdisa1>=44000 & year==2012 & incdisa1~=.
replace incdisa2=99999 if incdisa2>=99999 & year>=1988 & year<=1998 & incdisa2~=.
replace incdisa2=35000 if incdisa2>=35000 & year>=1999 & year<=2010 & incdisa2~=.
replace incdisa2=48000 if incdisa2>=48000 & year==2011 & incdisa2~=.
replace incdisa2=44000 if incdisa2>=44000 & year==2012 & incdisa2~=.

*Educational Assistance
replace inceduc=99999 if inceduc>=99999 & year<=1998 & inceduc~=.
replace inceduc=20000 if inceduc>=20000 & year>=1999 & year<=2010 & inceduc~=.
replace inceduc=30000 if inceduc>=30000 & year==2011 & inceduc~=.
replace inceduc=25000 if inceduc>=25000 & year==2012 & inceduc~=.

*Other Assistance
replace incasist=99999 if incasist>=99999 & year<=1998 & incasist~=.
replace incasist=30000 if incasist>=30000 & year>=1999 & year<=2010 & incasist~=.
replace incasist=30000 if incasist>=30000 & year==2011 & incasist~=.
replace incasist=36000 if incasist>=36000 & year==2012 & incasist~=.

save "CPS_topcodes.dta", replace


*Assumption 1: Treat business losses as income but truncate negative household incomes to zero
egen pre1987_inc = rowtotal(incwage incbus incfarm incss incssi incwelfr incint incdrt incidr incretir incaloth incgov)
egen post1988_inc = rowtotal(inclongj oincbus oincfarm oincwage incss incssi incwelfr incint incdivid incrent incalim incchild incunemp incwkcom incvet increti1 increti2 incsurv1 incsurv2 incdisa1 incdisa2 inceduc incasist incother)

egen pre1987_inc_pretrans = rowtotal(incwage incbus incfarm  incint incdrt incidr incretir incaloth )
egen post1988_inc_pretrans = rowtotal(inclongj oincbus oincfarm oincwage incint incdivid incrent incalim incchild  incwkcom increti1 increti2 incsurv1 incsurv2 inceduc incother)


gen inc_cutoff1 = pre1987_inc if year<=1987
replace inc_cutoff1 = post1988_inc if year>=1988
sort year serial
egen hhincome_cutoff1 = total(inc_cutoff1), by(year serial)
replace hhincome_cutoff1=0.01 if hhincome_cutoff1<=0

gen inc_cutoff1_pretrans = pre1987_inc_pretrans if year<=1987
replace inc_cutoff1_pretrans = post1988_inc_pretrans if year>=1988
egen hhincome_cutoff1_pretrans = total(inc_cutoff1_pretrans), by(year serial)
replace hhincome_cutoff1_pretrans=0.01 if hhincome_cutoff1_pretrans<=0

*Assumption 2: Treat business losses like consumption (e.g. truncate business income to zero for losses, but still count other income sources towards hhincome)
*replace incbus = 0 if incbus<0 & incbus~=.
*replace incfarm=0 if incfarm<0 & incfarm~=.
*replace incother = 0 if incother<0 & incother~=.
*replace inceduc= 0 if inceduc<0 & inceduc~=.
*replace incrent = 0 if incrent<0 & incrent~=.
*replace incdrt=0 if incdrt<=0 & incdrt~=.
*replace incidr=0 if incidr<=0 & incidr~=.
*egen incearned2 = rowtotal(incwage incbus incfarm incretir incint incother incdivid incalim incchild incidr incdrt incaloth incasist incrent)
*egen inctransfer2 = rowtotal(incss incwelfr incgov incssi incunemp incwkcom incvet incsurv incdisab inceduc)
*egen inc_cutoff2 = rowtotal(incearned2 inctransfer2)
*sort year serial
*egen hhincome_cutoff2 = total(inc_cutoff2), by(year serial)

save "CPS_topcodes.dta", replace
