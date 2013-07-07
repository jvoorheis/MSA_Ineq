*Replacing Cellmean values with cutoffs & reconstructing "topcoded" total income amounts, step 1: flagging topcoded observations

*Creating Topcoded Flag
gen topcoded=0

*Flagging all Topcoded observations (note: multiple income sources can be topcoded for a single observation)
*Total Income (post 1988)
replace topcoded=1 if inctot>=99999 & year<=1995 & inctot~=.
replace topcoded=1 if inctot>=150000 & year>=1996 & year<=2002 & inctot~=.
replace topcoded=1 if inctot>=200000 & year>=2003 & year<=2010 & inctot~=.
replace topcoded=1 if inctot>=250000 & year>=2011 & inctot~=.

*Wages
replace topcoded=1 if incwage>=50000 & year<=1981 & incwage~=.
replace topcoded=1 if incwage>=75000 & year>=1982 & year<=1984 & incwage~=.
replace topcoded=1 if incwage>=99999 & year>=1985 & year<=1995 & incwage~=.
replace topcoded=1 if incwage>=25000 & year>=1996 & year<=2002 & incwage~=.
replace topcoded=1 if incwage>=35000 & year>=2003 & year<=2010 & incwage~=.
replace topcoded=1 if incwage>=47000 & year==2011 & incwage~=.
replace topcoded=1 if incwage>=50000 & year==2012 & incwage~=.

*Business/Self Employment Income
replace topcoded=1 if incbus>=50000 & year<=1981 &incbus~=.
replace topcoded=1 if incbus>=75000 & year>=1982 & year<=1984 &incbus~=.
replace topcoded=1 if incbus>=99999 & year>=1985 & year<=1995 &incbus~=.
replace topcoded=1 if incbus>=40000 & year>=1996 & year<=2002 &incbus~=.
replace topcoded=1 if incbus>=50000 & year>=2003 & year<=2010 &incbus~=.
replace topcoded=1 if incbus>=60000 & year==2011 &incbus~=.
replace topcoded=1 if incbus>=60000 & year==2012 &incbus~=.

*Farm Income
replace topcoded=1 if incfarm>=50000 & year<=1981 &incfarm~=.
replace topcoded=1 if incfarm>=75000 & year>=1982 & year<=1984 &incfarm~=.
replace topcoded=1 if incfarm>=99999 & year>=1985 & year<=1995 &incfarm~=.
replace topcoded=1 if incfarm>=25000 & year>=1996 & year<=2002 &incfarm~=.
replace topcoded=1 if incfarm>=25000 & year>=2003 & year<=2010 &incfarm~=.
replace topcoded=1 if incfarm>=30000 & year==2011 &incfarm~=.
replace topcoded=1 if incfarm>=40000 & year==2012 &incfarm~=.

*SS Income
replace topcoded=1 if incss>=9999 & year<=1981 &incss~=.
replace topcoded=1 if incss>=19999 & year>=1982 & year<=1987 &incss~=.
replace topcoded=1 if incss>=29999 & year>=1988 & year<=1993 &incss~=.
replace topcoded=1 if incss>=49999 & year>=1994 &incss~=.

*SSI income
replace topcoded=1 if incssi>=5999 & year<=1984 &incssi~=.
replace topcoded=1 if incssi>=9999 & year>=1985 & year<=1995 &incssi~=.
replace topcoded=1 if incssi>=25000 & year>=1996 &incssi~=.

*Interest Income (Interest income had topcodes hardcoded in IPUMS, so we just need to insert a flag)
replace topcoded=1 if incint>=50000 & year<=1981 &incint~=.
replace topcoded=1 if incint>=75000 & year>=1982 & year<=1984 &incint~=.
replace topcoded=1 if incint>=99999 & year>=1985 & year<=1998 &incint~=.
replace topcoded=1 if incint>=35000 & year>=1999 & year<=2002 &incint~=.
replace topcoded=1 if incint>=25000 & year>=2003 & year<=2010 &incint~=.
replace topcoded=1 if incint>=24000 & year==2011 &incint~=.
replace topcoded=1 if incint>=22000 & year==2012 &incint~=.

*Public Assistance Income
replace topcoded=1 if incwelfr>=19999 & year<=1993 & incwelfr~=.
replace topcoded=1 if incwelfr>=24999 & year>=1994 & incwelfr~=.

*Dividend and Rental Income (Combined 1968-1987)
replace topcoded=1 if incidr>=50000 & year<=1981 & incidr~=.
replace topcoded=1 if incdrt>=50000 & year<=1981 & incdrt~=.
replace topcoded=1 if incdrt>=75000 & year>=1982 & year<=1984 & incdrt~=.
replace topcoded=1 if incdrt>=99999 & year>=1985 & incdrt~=.
replace topcoded=1 if incdivid>=99999 & year<=1998 & incdivid~=.
replace topcoded=1 if incdivid>=15000 & year>=1999 & year<=2010 & incdivid~=.
replace topcoded=1 if incdivid>=20000 & year>=2011 & incdivid~=.
replace topcoded=1 if incrent>=99999 & year<=1998 & incrent~=.
replace topcoded=1 if incrent>=25000 & year>=1999 & year<=2002 & incrent~=.
replace topcoded=1 if incrent>=40000 & year>=2003 & year<=2010 & incrent~=.
replace topcoded=1 if incrent>=50000 & year==2011 & incrent~=.
replace topcoded=1 if incrent>=60000 & year==2012 & incrent~=.

*Gov't Assistnance/Vet/Wk comp (combined pre 1988) - Post 1988, already hardcoded & corrected in CPS_read_in_clean.do
replace topcoded=1 if incgov>=29999 &incgov~=.
replace topcoded=1 if incwkcom>=99999 & incwkcom~=.
replace topcoded=1 if incvet>=99999 & incvet~=.

*Retirement Income
replace topcoded=1 if incretir>=50000 & year<=1981 &incretir~=.
replace topcoded=1 if incretir>=75000 & year>=1982 & year<=1984 &incretir~=.
replace topcoded=1 if incretir>=99999 & year>=1985 & year<=1998 &incretir~=.
replace topcoded=1 if incretir>=45000 & year>=1999 & year<=2010 &incretir~=.
replace topcoded=1 if incretir>=64000 & year==2011 &incretir~=.
replace topcoded=1 if incretir>=67000 & year==2012 &incretir~=.

*Other Income
replace topcoded=1 if incaloth>=50000 & year<=1981 &incaloth~=.
replace topcoded=1 if incaloth>=75000 & year>=1982 & year<=1984 &incaloth~=.
replace topcoded=1 if incaloth>=99999 & year>=1985 & year<=1987 &incaloth~=.
replace topcoded=1 if incother>=99999 & year>=1985 & year<=1998 &incother~=.
replace topcoded=1 if incother>=25000 & year>=1999 & year<=2010 &incother~=.
replace topcoded=1 if incother>=30000 & year==2011 &incother~=.
replace topcoded=1 if incother>=31200 & year==2012 &incother~=.

*Alimony
replace topcoded=1 if incalim>=99999 & year<=1998 & incalim~=.
replace topcoded=1 if incalim>=50000 & year==1999 & incalim~=.
replace topcoded=1 if incalim>=40000 & year>=2000 & year<=2002 & incalim~=.
replace topcoded=1 if incalim>=45000 & year>=2003 & year<=2010 & incalim~=.
replace topcoded=1 if incalim>=66000 & year==2011 & incalim~=.
replace topcoded=1 if incalim>=96000 & year==2012 & incalim~=.

*Child Support
replace topcoded=1 if incchild>=99999 & year<=1998 & incchild~=.
replace topcoded=1 if incchild>=15000 & year>=1999 & year<=2010 & incchild~=.
replace topcoded=1 if incchild>=21000 & year==2011 & incchild~=.
replace topcoded=1 if incchild>=18300 & year==2012 & incchild~=.

*Unemployment
replace topcoded=1 if incunemp>=99999 &incunemp~=.

*Survivor Benefits
replace topcoded=1 if incsurv>=99999 & year<=1998 & incsurv~=.
replace topcoded=1 if incsurv>=50000 & year>=1999 & year<=2010 & incsurv~=.
replace topcoded=1 if incsurv>=57600 & year==2011 & incsurv~=.
replace topcoded=1 if incsurv>=75000 & year==2012 & incsurv~=.

*Disability Benefits
replace topcoded=1 if incdisab>=99999 & year<=1998 & incdisab~=.
replace topcoded=1 if incdisab>=35000 & year>=1999 & year<=2010 & incdisab~=.
replace topcoded=1 if incdisab>=48000 & year==2011 & incdisab~=.
replace topcoded=1 if incdisab>=44000 & year==2012 & incdisab~=.

*Educational Assistance
replace topcoded=1 if inceduc>=99999 & year<=1998 & inceduc~=.
replace topcoded=1 if inceduc>=20000 & year>=1999 & year<=2010 & inceduc~=.
replace topcoded=1 if inceduc>=30000 & year==2011 & inceduc~=.
replace topcoded=1 if inceduc>=25000 & year==2012 & inceduc~=.

*Other Assistance
replace topcoded=1 if incasist>=99999 & year<=1998 & incasist~=.
replace topcoded=1 if incasist>=30000 & year>=1999 & year<=2010 & incasist~=.
replace topcoded=1 if incasist>=30000 & year==2011 & incasist~=.
replace topcoded=1 if incasist>=36000 & year==2012 & incasist~=.

sort serial year
egen hh_topcoded = max(topcoded), by(serial year)
replace topcoded = hh_topcoded
save "CPS_topcodes.dta", replace