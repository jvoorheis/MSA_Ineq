*Replacing Cellmean values with cutoffs & reconstructing "topcoded" total income amounts

*Creating Topcoded Flag
gen topcoded=0

*Total Income (post 1988)
replace topcoded=1 if inctot>=99999 & year<=1995
replace topcoded=1 if inctot>=150000 & year>=1996 & year<=2002
replace topcoded=1 if inctot>=200000 & year>=2003 & year<=2010
replace topcoded=1 if inctot>=250000 & year>=2011

*Wages
replace topcoded=1 if incwage>=50000 & year<=1981
replace topcoded=1 if incwage>=75000 & year>=1982 & year<=1984
replace topcoded=1 if incwage>=99999 & year>=1985 & year<=1995
replace topcoded=1 if incwage>=25000 & year>=1996 & year<=2002
replace topcoded=1 if incwage>=35000 & year>=2003 & year<=2010
replace topcoded=1 if incwage>=47000 & year==2011
replace topcoded=1 if incwage>=50000 & year==2012


