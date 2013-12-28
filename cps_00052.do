* NOTE: You need to set the Stata working directory to the path
* where the data file is located.

set more off

clear
quietly infix                ///
  int     year      1-4      ///
  long    serial    5-9      ///
  float   hwtsupp   10-19    ///
  byte    gq        20-20    ///
  byte    statefip  21-22    ///
  int     metarea   23-26    ///
  long    county    27-31    ///
  double  hhincome  32-39    ///
  long    housret   40-44    ///
  int     heatval   45-48    ///
  long    stampval  49-53    ///
  byte    lunchsub  54-54    ///
  byte    frelunch  55-56    ///
  byte    month     57-58    ///
  byte    pernum    59-60    ///
  int     mwpval    61-63    ///
  long    ctccrd    64-69    ///
  long    actccrd   70-74    ///
  long    ffngcare  75-79    ///
  long    ffngcaid  80-84    ///
  float   wtsupp    85-94    ///
  byte    age       95-96    ///
  byte    sex       97-97    ///
  int     race      98-100   ///
  byte    marst     101-101  ///
  int     hispan    102-104  ///
  int     educ      105-107  ///
  byte    schlcoll  108-108  ///
  int     occ       109-112  ///
  int     ind       113-116  ///
  byte    classwkr  117-118  ///
  byte    wkswork1  119-120  ///
  byte    hrswork   121-122  ///
  byte    uhrswork  123-124  ///
  byte    union     125-125  ///
  double  ftotval   126-135  ///
  double  inctot    136-143  ///
  long    incwage   144-150  ///
  long    incbus    151-157  ///
  long    incfarm   158-164  ///
  long    incss     165-169  ///
  long    incwelfr  170-174  ///
  long    incretir  175-180  ///
  long    incssi    181-185  ///
  long    incint    186-190  ///
  long    incunemp  191-195  ///
  long    incwkcom  196-200  ///
  long    incvet    201-205  ///
  long    incsurv   206-211  ///
  long    incdisab  212-217  ///
  long    incdivid  218-223  ///
  long    incrent   224-228  ///
  long    inceduc   229-233  ///
  long    incchild  234-238  ///
  long    incalim   239-243  ///
  long    incasist  244-248  ///
  long    incother  249-253  ///
  long    incdisa1  254-258  ///
  long    incdisa2  259-263  ///
  long    inclongj  264-270  ///
  long    increti1  271-275  ///
  long    increti2  276-280  ///
  long    incsurv1  281-285  ///
  long    incsurv2  286-290  ///
  long    oincbus   291-296  ///
  long    oincfarm  297-302  ///
  long    oincwage  303-309  ///
  byte    srcearn   310-310  ///
  long    adjginc   311-317  ///
  int     eitcred   318-321  ///
  long    fedtax    322-327  ///
  long    fedtaxac  328-333  ///
  long    fica      334-338  ///
  byte    filestat  339-339  ///
  byte    margtax   340-341  ///
  long    statetax  342-347  ///
  long    stataxac  348-353  ///
  long    taxinc    354-360  ///
  byte    hcovany   361-361  ///
  byte    hinsemp   362-362  ///
  byte    hinspur   363-363  ///
  byte    hcovpub   364-364  ///
  byte    hinscaid  365-365  ///
  byte    hinscare  366-366  ///
  byte    hinsmil   367-367  ///
  int     emcontrb  368-371  ///
  byte    gotwic    372-372  ///
  using `"cps_00052.dat"'

gen MSA_FIPS = metarea
replace hwtsupp  = hwtsupp  / 10000
replace wtsupp   = wtsupp   / 10000

format hwtsupp  %10.4f
format hhincome %8.0f
format wtsupp   %10.4f
format ftotval  %10.0f
format inctot   %8.0f

label var year     `"Survey year"'
label var serial   `"Household serial number"'
label var hwtsupp  `"Household weight, Supplement"'
label var gq       `"Group Quarters status"'
label var statefip `"State (FIPS code)"'
label var metarea  `"Metropolitan area"'
label var county   `"FIPS county code"'
label var hhincome `"Total household income"'
label var housret  `"Return to home equity"'
label var heatval  `"Value of energy subsidy"'
label var stampval `"Total value of food stamps"'
label var lunchsub `"Government school lunch food subsidy"'
label var frelunch `"Number of children with government school lunch subsidy"'
label var month    `"Month"'
label var pernum   `"Person number in sample unit"'
label var mwpval   `"Credit received from Making Work Pay"'
label var ctccrd   `"Child Tax Credit"'
label var actccrd  `"Additional Child Tax Credit"'
label var ffngcare `"Family fungible value of Medicare"'
label var ffngcaid `"Family fungible value of Medicaid"'
label var wtsupp   `"Supplement Weight"'
label var age      `"Age"'
label var sex      `"Sex"'
label var race     `"Race"'
label var marst    `"Marital status"'
label var hispan   `"Hispanic origin"'
label var educ     `"Educational attainment recode"'
label var schlcoll `"School or college attendance"'
label var occ      `"Occupation"'
label var ind      `"Industry"'
label var classwkr `"Class of worker"'
label var wkswork1 `"Weeks worked last year"'
label var hrswork  `"Hours worked last week"'
label var uhrswork `"Usual hours worked per week (last yr)"'
label var union    `"Union membership"'
label var ftotval  `"Total family income"'
label var inctot   `"Total personal income"'
label var incwage  `"Wage and salary income"'
label var incbus   `"Non-farm business income"'
label var incfarm  `"Farm income"'
label var incss    `"Social Security income"'
label var incwelfr `"Welfare (public assistance) income"'
label var incretir `"Retirement income"'
label var incssi   `"Income from SSI"'
label var incint   `"Income from interest"'
label var incunemp `"Income from unemployment benefits"'
label var incwkcom `"Income from worker's compensation"'
label var incvet   `"Income from veteran's benefits"'
label var incsurv  `"Income from survivor's benefits"'
label var incdisab `"Income from disability benefits"'
label var incdivid `"Income from dividends"'
label var incrent  `"Income from rent"'
label var inceduc  `"Income from educational assistance"'
label var incchild `"Income from child support"'
label var incalim  `"Income from alimony"'
label var incasist `"Income from assistance"'
label var incother `"Income from other Source not specified"'
label var incdisa1 `"Disability income from first source"'
label var incdisa2 `"Disability income from second source"'
label var inclongj `"Earnings from longest job"'
label var increti1 `"Retirement income from first source"'
label var increti2 `"Retirement income from second source"'
label var incsurv1 `"Survivor benefits income from first source"'
label var incsurv2 `"Survivor benefits income from second source"'
label var oincbus  `"Earnings from other work included business self-employment earnings"'
label var oincfarm `"Earnings from other work included farm self-employment earnings"'
label var oincwage `"Earnings from other work included wage and salary earnings"'
label var srcearn  `"Source of earnings from longest job"'
label var adjginc  `"Adjusted gross income"'
label var eitcred  `"Earnedincome tax credit"'
label var fedtax   `"Federal income tax liability, before credits"'
label var fedtaxac `"Federal income tax liability, after all credits"'
label var fica     `"Social security retirement payroll deduction"'
label var filestat `"Tax filer status"'
label var margtax  `"Federal income marginal tax rate"'
label var statetax `"State income tax liability, before credits"'
label var stataxac `"State income tax liability, after all credits"'
label var taxinc   `"Taxable income amount"'
label var hcovany  `"Any insurance, public or private (summary)"'
label var hinsemp  `"Employer-sponsored insurance (summary)"'
label var hinspur  `"Individually purchased insurance (summary)"'
label var hcovpub  `"Any public insurance (summary)"'
label var hinscaid `"Any Medicaid/SCHIP/other public insurance (summary)"'
label var hinscare `"Medicare coverage (summary)"'
label var hinsmil  `"Any military insurance (summary)"'
label var emcontrb `"Employer contribution for health insurance"'
label var gotwic   `"Received WIC"'

label define hwtsupp_lbl 0000000000 `"0000000000"'
label values hwtsupp hwtsupp_lbl

label define gq_lbl 0 `"NIU (Vacant units)"'
label define gq_lbl 1 `"Households"', add
label define gq_lbl 2 `"Group Quarters"', add
label values gq gq_lbl

label define statefip_lbl 01 `"Alabama"'
label define statefip_lbl 02 `"Alaska"', add
label define statefip_lbl 04 `"Arizona"', add
label define statefip_lbl 05 `"Arkansas"', add
label define statefip_lbl 06 `"California"', add
label define statefip_lbl 08 `"Colorado"', add
label define statefip_lbl 09 `"Connecticut"', add
label define statefip_lbl 10 `"Delaware"', add
label define statefip_lbl 11 `"District of Columbia"', add
label define statefip_lbl 12 `"Florida"', add
label define statefip_lbl 13 `"Georgia"', add
label define statefip_lbl 15 `"Hawaii"', add
label define statefip_lbl 16 `"Idaho"', add
label define statefip_lbl 17 `"Illinois"', add
label define statefip_lbl 18 `"Indiana"', add
label define statefip_lbl 19 `"Iowa"', add
label define statefip_lbl 20 `"Kansas"', add
label define statefip_lbl 21 `"Kentucky"', add
label define statefip_lbl 22 `"Louisiana"', add
label define statefip_lbl 23 `"Maine"', add
label define statefip_lbl 24 `"Maryland"', add
label define statefip_lbl 25 `"Massachusetts"', add
label define statefip_lbl 26 `"Michigan"', add
label define statefip_lbl 27 `"Minnesota"', add
label define statefip_lbl 28 `"Mississippi"', add
label define statefip_lbl 29 `"Missouri"', add
label define statefip_lbl 30 `"Montana"', add
label define statefip_lbl 31 `"Nebraska"', add
label define statefip_lbl 32 `"Nevada"', add
label define statefip_lbl 33 `"New Hampshire"', add
label define statefip_lbl 34 `"New Jersey"', add
label define statefip_lbl 35 `"New Mexico"', add
label define statefip_lbl 36 `"New York"', add
label define statefip_lbl 37 `"North Carolina"', add
label define statefip_lbl 38 `"North Dakota"', add
label define statefip_lbl 39 `"Ohio"', add
label define statefip_lbl 40 `"Oklahoma"', add
label define statefip_lbl 41 `"Oregon"', add
label define statefip_lbl 42 `"Pennsylvania"', add
label define statefip_lbl 44 `"Rhode Island"', add
label define statefip_lbl 45 `"South Carolina"', add
label define statefip_lbl 46 `"South Dakota"', add
label define statefip_lbl 47 `"Tennessee"', add
label define statefip_lbl 48 `"Texas"', add
label define statefip_lbl 49 `"Utah"', add
label define statefip_lbl 50 `"Vermont"', add
label define statefip_lbl 51 `"Virginia"', add
label define statefip_lbl 53 `"Washington"', add
label define statefip_lbl 54 `"West Virginia"', add
label define statefip_lbl 55 `"Wisconsin"', add
label define statefip_lbl 56 `"Wyoming"', add
label define statefip_lbl 61 `"Maine-New Hampshire-Vermont"', add
label define statefip_lbl 65 `"Montana-Idaho-Wyoming"', add
label define statefip_lbl 68 `"Alaska-Hawaii"', add
label define statefip_lbl 69 `"Nebraska-North Dakota-South Dakota"', add
label define statefip_lbl 70 `"Maine-Massachusetts-New Hampshire-Rhode Island-Vermont"', add
label define statefip_lbl 71 `"Michigan-Wisconsin"', add
label define statefip_lbl 72 `"Minnesota-Iowa"', add
label define statefip_lbl 73 `"Nebraska-North Dakota-South Dakota-Kansas"', add
label define statefip_lbl 74 `"Delaware-Virginia"', add
label define statefip_lbl 75 `"North Carolina-South Carolina"', add
label define statefip_lbl 76 `"Alabama-Mississippi"', add
label define statefip_lbl 77 `"Arkansas-Oklahoma"', add
label define statefip_lbl 78 `"Arizona-New Mexico-Colorado"', add
label define statefip_lbl 79 `"Idaho-Wyoming-Utah-Montana-Nevada"', add
label define statefip_lbl 80 `"Alaska-Washington-Hawaii"', add
label define statefip_lbl 81 `"New Hampshire-Maine-Vermont-Rhode Island"', add
label define statefip_lbl 83 `"South Carolina-Georgia"', add
label define statefip_lbl 84 `"Kentucky-Tennessee"', add
label define statefip_lbl 85 `"Arkansas-Louisiana-Oklahoma"', add
label define statefip_lbl 87 `"Iowa-N Dakota-S Dakota-Nebraska-Kansas-Minnesota-Missouri"', add
label define statefip_lbl 88 `"Washington-Oregon-Alaska-Hawaii"', add
label define statefip_lbl 89 `"Montana-Wyoming-Colorado-New Mexico-Utah-Nevada-Arizona"', add
label define statefip_lbl 90 `"Delaware-Maryland-Virginia-West Virginia"', add
label define statefip_lbl 99 `"State not identified"', add
label values statefip statefip_lbl

label define metarea_lbl 0080 `"Akron, OH"'
label define metarea_lbl 0120 `"Albany, GA"', add
label define metarea_lbl 0160 `"Albany-Schenectady-Troy, NY"', add
label define metarea_lbl 0200 `"Albuquerque, NM"', add
label define metarea_lbl 0240 `"Allentown-Bethlehem-Easton, PA/NJ"', add
label define metarea_lbl 0280 `"Altoona, PA MSA"', add
label define metarea_lbl 0320 `"Amarillo, TX"', add
label define metarea_lbl 0380 `"Anchorage, AK"', add
label define metarea_lbl 0400 `"Anderson, IN"', add
label define metarea_lbl 0440 `"Ann Arbor, MI"', add
label define metarea_lbl 0450 `"Anniston, AL"', add
label define metarea_lbl 0451 `"Anniston-Oxford, AL"', add
label define metarea_lbl 0460 `"Appleton,Oshkosh-Neenah, WI"', add
label define metarea_lbl 0461 `"Appleton, WI"', add
label define metarea_lbl 0462 `"Oshkosh-Neenah, WI"', add
label define metarea_lbl 0480 `"Asheville, NC"', add
label define metarea_lbl 0500 `"Athens, GA"', add
label define metarea_lbl 0501 `"Athens-Clark County, GA"', add
label define metarea_lbl 0520 `"Atlanta, GA"', add
label define metarea_lbl 0521 `"Atlanta-Sandy Springs-Marietta, GA"', add
label define metarea_lbl 0560 `"Atlantic City, NJ"', add
label define metarea_lbl 0600 `"Augusta-Aiken, GA-SC"', add
label define metarea_lbl 0601 `"Augusta-Richmond County, GA-SC"', add
label define metarea_lbl 0640 `"Austin, TX"', add
label define metarea_lbl 0641 `"Austin-Round Rock, TX"', add
label define metarea_lbl 0680 `"Bakersfield, CA"', add
label define metarea_lbl 0720 `"Baltimore, MD"', add
label define metarea_lbl 0721 `"Baltimore-Towson, MD"', add
label define metarea_lbl 0730 `"Bangor, ME"', add
label define metarea_lbl 0740 `"Barnstable-Yarmouth, MA"', add
label define metarea_lbl 0741 `"Barnstable Town, MA"', add
label define metarea_lbl 0760 `"Baton Rouge, LA"', add
label define metarea_lbl 0780 `"Battle Creek, MI"', add
label define metarea_lbl 0840 `"Beaumont-Port Arthur-Orange, TX"', add
label define metarea_lbl 0841 `"Beaumont-Port Arthur, TX"', add
label define metarea_lbl 0860 `"Bellingham, WA"', add
label define metarea_lbl 0870 `"Benton Harbor, MI"', add
label define metarea_lbl 0871 `"Niles-Benton Harbor, MI"', add
label define metarea_lbl 0880 `"Billings, MT"', add
label define metarea_lbl 0900 `"Bend, OR"', add
label define metarea_lbl 0920 `"Biloxi-Gulfport, MS"', add
label define metarea_lbl 0960 `"Binghamton, NY"', add
label define metarea_lbl 1000 `"Birmingham, AL"', add
label define metarea_lbl 1001 `"Birmingham-Hoover, AL"', add
label define metarea_lbl 1020 `"Bloomington, IN"', add
label define metarea_lbl 1040 `"Bloomington-Normal, IL"', add
label define metarea_lbl 1080 `"Boise City, ID"', add
label define metarea_lbl 1081 `"Boise City-Nampa, ID"', add
label define metarea_lbl 1120 `"Boston, MA"', add
label define metarea_lbl 1121 `"Lawrence-Haverhill. MA/NH"', add
label define metarea_lbl 1122 `"Lowell, MA/NH"', add
label define metarea_lbl 1123 `"Salem-Gloucester, MA"', add
label define metarea_lbl 1124 `"Boston-Cambridge-Quincy, MA-NH"', add
label define metarea_lbl 1130 `"Bowling Green, KY"', add
label define metarea_lbl 1140 `"Bradenton, FL"', add
label define metarea_lbl 1150 `"Bremerton-Silverdale, WA"', add
label define metarea_lbl 1160 `"Bridgeport, CT"', add
label define metarea_lbl 1161 `"Bridgeport-Stamford-Norwalk, CT"', add
label define metarea_lbl 1200 `"Brockton"', add
label define metarea_lbl 1240 `"Brownsville-Harlingen-San Benito,TX"', add
label define metarea_lbl 1241 `"Brownsville-Harlingen, TX"', add
label define metarea_lbl 1280 `"Buffalo-Niagara Falls, NY"', add
label define metarea_lbl 1281 `"Niagara Falls, NY"', add
label define metarea_lbl 1310 `"Burlington, VT"', add
label define metarea_lbl 1311 `"Burlington-South Burlington, VT"', add
label define metarea_lbl 1320 `"Canton, OH"', add
label define metarea_lbl 1321 `"Canton-Massillon, OH"', add
label define metarea_lbl 1360 `"Cedar Rapids, IA"', add
label define metarea_lbl 1400 `"Champaign-Urbana-Rantoul, IL"', add
label define metarea_lbl 1401 `"Champaign-Urbana, IL"', add
label define metarea_lbl 1440 `"Charleston-North Charleston, SC"', add
label define metarea_lbl 1480 `"Charleston, WV"', add
label define metarea_lbl 1520 `"Charlotte-Gastonia-Rock Hill, NC/SC"', add
label define metarea_lbl 1521 `"Charlotte-Gastonia-Concord, NC/SC"', add
label define metarea_lbl 1560 `"Chattanooga, TN/GA"', add
label define metarea_lbl 1600 `"Chicago-Gary-Lake IL"', add
label define metarea_lbl 1601 `"Aurora-Elgin, IL"', add
label define metarea_lbl 1602 `"Gary-Hamond-East Chicago, IN"', add
label define metarea_lbl 1603 `"Joliet, IL"', add
label define metarea_lbl 1604 `"Lake County, IL"', add
label define metarea_lbl 1605 `"Chicago-Naperville-Joliet, IL-IN-WI"', add
label define metarea_lbl 1620 `"Chico,CA"', add
label define metarea_lbl 1640 `"Cincinnati-Hamilton,OH/KY/IN"', add
label define metarea_lbl 1641 `"Cincinnati-Middleton, OH/KY/IN"', add
label define metarea_lbl 1650 `"1650"', add
label define metarea_lbl 1660 `"Clarksville-Hopkinsville,TN/KY"', add
label define metarea_lbl 1661 `"Clarksville, TN/KY"', add
label define metarea_lbl 1680 `"Cleveland, OH"', add
label define metarea_lbl 1681 `"Cleveland-Lorain-Mentor, OH"', add
label define metarea_lbl 1700 `"Coeur d'Alene, ID"', add
label define metarea_lbl 1720 `"Colorado Springs, CO"', add
label define metarea_lbl 1740 `"Columbia, MO"', add
label define metarea_lbl 1760 `"Columbia, SC"', add
label define metarea_lbl 1800 `"Columbus, GA/AL"', add
label define metarea_lbl 1840 `"Columbus, OH"', add
label define metarea_lbl 1880 `"Corpus Christi, TX"', add
label define metarea_lbl 1920 `"Dallas-Fort Worth, TX"', add
label define metarea_lbl 1921 `"Fort Worth-Arlington, TX"', add
label define metarea_lbl 1922 `"Dallas-Fort Worth-Arlington, TX"', add
label define metarea_lbl 1930 `"Danbury, CT"', add
label define metarea_lbl 1960 `"Davenport-Rock Island-Moline, IA/IL"', add
label define metarea_lbl 2000 `"Dayton-Springfield, OH"', add
label define metarea_lbl 2001 `"Springfield, OH"', add
label define metarea_lbl 2002 `"Dayton, OH"', add
label define metarea_lbl 2020 `"Daytona Beach, FL"', add
label define metarea_lbl 2021 `"Deltona-Daytona Beach-Ormond Beach, FL"', add
label define metarea_lbl 2030 `"Decatur, AL"', add
label define metarea_lbl 2040 `"Decatur, IL"', add
label define metarea_lbl 2080 `"Denver-Boulder-Longmont, CO"', add
label define metarea_lbl 2081 `"Boulder-Longmont, CO"', add
label define metarea_lbl 2082 `"Boulder, CO"', add
label define metarea_lbl 2083 `"Denver-Aurora, CO"', add
label define metarea_lbl 2120 `"Des Moines, IA"', add
label define metarea_lbl 2160 `"Detroit, MI"', add
label define metarea_lbl 2161 `"Detroit-Warren-Livonia, MI"', add
label define metarea_lbl 2190 `"Dover, DE"', add
label define metarea_lbl 2240 `"Duluth-Superior, MN/WI"', add
label define metarea_lbl 2241 `"Duluth, MN/WI"', add
label define metarea_lbl 2281 `"Dutchess County, NY"', add
label define metarea_lbl 2290 `"Eau Claire, WI"', add
label define metarea_lbl 2300 `"El Centro, CA"', add
label define metarea_lbl 2310 `"El Paso, TX"', add
label define metarea_lbl 2360 `"Erie, PA"', add
label define metarea_lbl 2400 `"Eugene-Springfield, OR"', add
label define metarea_lbl 2440 `"Evansville, IN/KY"', add
label define metarea_lbl 2520 `"Fargo-Moorhead, ND/MN"', add
label define metarea_lbl 2521 `"Fargo, ND/MN"', add
label define metarea_lbl 2540 `"Farmington, NM"', add
label define metarea_lbl 2560 `"Fayetteville, NC"', add
label define metarea_lbl 2580 `"Fayetteville-Springdale, AR"', add
label define metarea_lbl 2581 `"Fayetteville-Springdale-Rogers, AR-MO"', add
label define metarea_lbl 2600 `"Fitchburg-Leominster, MA"', add
label define metarea_lbl 2601 `"Leominster-Fitchburg-Gardner, MA"', add
label define metarea_lbl 2640 `"Flint, MI"', add
label define metarea_lbl 2650 `"Florence, AL"', add
label define metarea_lbl 2660 `"Florence, SC"', add
label define metarea_lbl 2670 `"Fort Collins-Loveland, CO"', add
label define metarea_lbl 2680 `"Fort Lauderdale-Hollywood-Pompano Beach, FL"', add
label define metarea_lbl 2700 `"Fort Myers-Cape Coral, FL"', add
label define metarea_lbl 2710 `"Fort Pierce, FL"', add
label define metarea_lbl 2711 `"Port St. Lucie-Fort Pierce, FL"', add
label define metarea_lbl 2720 `"Fort Smith, AR/OK"', add
label define metarea_lbl 2750 `"Fort Walton Beach, FL"', add
label define metarea_lbl 2751 `"Fort Walton Beach-Crestview-Destin, FL"', add
label define metarea_lbl 2760 `"Fort Wayne, IN"', add
label define metarea_lbl 2840 `"Fresno, CA"', add
label define metarea_lbl 2880 `"Gadsden, AL"', add
label define metarea_lbl 2900 `"Gainesville, FL"', add
label define metarea_lbl 2920 `"Galveston-Texas City, TX"', add
label define metarea_lbl 3000 `"Grand Rapids, MI"', add
label define metarea_lbl 3001 `"Grand Rapids-Wyoming, MI"', add
label define metarea_lbl 3002 `"Grand Rapids-Muskegon-Holland, MI MSA"', add
label define metarea_lbl 3003 `"Holland-Grand Haven, MI"', add
label define metarea_lbl 3060 `"Greeley, CO"', add
label define metarea_lbl 3080 `"Green Bay, WI"', add
label define metarea_lbl 3120 `"Greensboro-Winston Salem, NC"', add
label define metarea_lbl 3121 `"Winston-Salem, NC"', add
label define metarea_lbl 3122 `"Greensboro-High Point, NC"', add
label define metarea_lbl 3150 `"Greenville, NC"', add
label define metarea_lbl 3160 `"Greenville-Spartanburg-Anderson, SC"', add
label define metarea_lbl 3161 `"Anderson, SC"', add
label define metarea_lbl 3162 `"Greenville, SC"', add
label define metarea_lbl 3163 `"Spartanburg, SC"', add
label define metarea_lbl 3180 `"Hagerstown, MD"', add
label define metarea_lbl 3181 `"Hagerstown-Martinsburg, MD-WV"', add
label define metarea_lbl 3200 `"Hamilton-Middleton, OH"', add
label define metarea_lbl 3240 `"Harrisburg-Lebanon-Carlisle, PA"', add
label define metarea_lbl 3241 `"Harrisburg-Carlisle, PA"', add
label define metarea_lbl 3260 `"Harrisonburg, VA"', add
label define metarea_lbl 3280 `"Hartford-Bristol-Middleton- New Britain, CT"', add
label define metarea_lbl 3283 `"New Britain, CT"', add
label define metarea_lbl 3284 `"Hartford-West Hartford-East Hartford"', add
label define metarea_lbl 3285 `"Hartford, CT"', add
label define metarea_lbl 3290 `"Hickory-Morganton, NC"', add
label define metarea_lbl 3291 `"Hickory-Morganton-Lenoir, NC"', add
label define metarea_lbl 3320 `"Honolulu, HI"', add
label define metarea_lbl 3350 `"Houma-Thibodaux, LA"', add
label define metarea_lbl 3351 `"Houma-Bayou Cane-Thibodaux, LA"', add
label define metarea_lbl 3360 `"Houston-Brazoria,TX"', add
label define metarea_lbl 3361 `"Brazoria, TX"', add
label define metarea_lbl 3362 `"Houston-Baytown-Sugar Land, TX"', add
label define metarea_lbl 3400 `"Huntington-Ashland,WV/KY/OH"', add
label define metarea_lbl 3440 `"Huntsville,AL"', add
label define metarea_lbl 3480 `"Indianapolis, IN"', add
label define metarea_lbl 3500 `"Iowa City, IA"', add
label define metarea_lbl 3520 `"Jackson, MI"', add
label define metarea_lbl 3560 `"Jackson, MS"', add
label define metarea_lbl 3590 `"Jacksonville,FL"', add
label define metarea_lbl 3600 `"Jacksonville, NC"', add
label define metarea_lbl 3610 `"Jamestown-Dunkirk, NY"', add
label define metarea_lbl 3611 `"Jamestown, NY MSA"', add
label define metarea_lbl 3620 `"Janesville-Beloit, WI"', add
label define metarea_lbl 3621 `"Janvesville, WI"', add
label define metarea_lbl 3660 `"Johnson City-Kingsport-Bristol, TN/VA"', add
label define metarea_lbl 3661 `"Johnson City, TN"', add
label define metarea_lbl 3662 `"Kingsport-Bristol, TN-VA"', add
label define metarea_lbl 3680 `"Johnstown, PA"', add
label define metarea_lbl 3710 `"Joplin, MO"', add
label define metarea_lbl 3720 `"Kalamazoo-Portage, MI"', add
label define metarea_lbl 3721 `"Kalamazoo-Battle Creek, MI MSA"', add
label define metarea_lbl 3740 `"Kankakee, IL"', add
label define metarea_lbl 3741 `"Kankakee-Bradley, IL"', add
label define metarea_lbl 3760 `"Kansas City, MO/KS"', add
label define metarea_lbl 3810 `"Killeen-Temple,TX"', add
label define metarea_lbl 3811 `"Killeen-Temple-Fort Hood, TX"', add
label define metarea_lbl 3830 `"Kingston, NY"', add
label define metarea_lbl 3840 `"Knoxville, TN"', add
label define metarea_lbl 3870 `"LaCrosse, WI"', add
label define metarea_lbl 3880 `"Lafayette, LA"', add
label define metarea_lbl 3960 `"Lake Charles, LA"', add
label define metarea_lbl 3980 `"Lakeland-Winterhaven, FL"', add
label define metarea_lbl 4000 `"Lancaster, PA"', add
label define metarea_lbl 4040 `"Lansing-East Lansing, MI"', add
label define metarea_lbl 4080 `"Laredo, TX"', add
label define metarea_lbl 4100 `"Las Cruces, NM"', add
label define metarea_lbl 4120 `"Las Vegas, NV"', add
label define metarea_lbl 4130 `"Las Vegas-Paradise, NM"', add
label define metarea_lbl 4150 `"Lawrence, KS"', add
label define metarea_lbl 4200 `"Lawton, OK"', add
label define metarea_lbl 4280 `"Lexington-Fayette, KY"', add
label define metarea_lbl 4320 `"Lima, OH"', add
label define metarea_lbl 4360 `"Lincoln, NE"', add
label define metarea_lbl 4400 `"Little Rock-North Little Rock, AR"', add
label define metarea_lbl 4420 `"Longview-Marshall, TX"', add
label define metarea_lbl 4421 `"Longview, TX"', add
label define metarea_lbl 4440 `"Lorain-Elyria, OH"', add
label define metarea_lbl 4480 `"Los Angeles-Long Beach, CA"', add
label define metarea_lbl 4481 `"Anaheim-Santa Ana- Garden Grove, CA"', add
label define metarea_lbl 4482 `"Orange County, CA"', add
label define metarea_lbl 4483 `"Los Angeles-Long Beach-Santa Ana, CA"', add
label define metarea_lbl 4520 `"Louisville, KY/IN"', add
label define metarea_lbl 4600 `"Lubbock, TX"', add
label define metarea_lbl 4640 `"Lynchburg, VA"', add
label define metarea_lbl 4680 `"Macon-Warner Robins, GA"', add
label define metarea_lbl 4681 `"Macon, GA"', add
label define metarea_lbl 4682 `"Warner Robins, GA"', add
label define metarea_lbl 4700 `"Madera, CA"', add
label define metarea_lbl 4720 `"Madison, WI"', add
label define metarea_lbl 4760 `"Manchester, NH"', add
label define metarea_lbl 4800 `"Mansfield, OH"', add
label define metarea_lbl 4880 `"McAllen-Edinburg-Pharr-Mission, TX"', add
label define metarea_lbl 4881 `"McAllen-Edinburg-Pharr, TX"', add
label define metarea_lbl 4890 `"Medford, OR"', add
label define metarea_lbl 4900 `"Melbourne-Titusville-Cocoa-Palm Beach, FL"', add
label define metarea_lbl 4901 `"Palm Bay-Melbourne-Titusville, FL"', add
label define metarea_lbl 4920 `"Memphis, TN/AR/MS"', add
label define metarea_lbl 4940 `"Merced, CA"', add
label define metarea_lbl 5000 `"Miami-Hialeah, FL"', add
label define metarea_lbl 5001 `"Miami-Fort Lauderdale-Miami Beach, FL"', add
label define metarea_lbl 5020 `"Michigan City-La Porte, IN"', add
label define metarea_lbl 5080 `"Milwaukee, WI"', add
label define metarea_lbl 5081 `"Milwaukee-Waukesha-West Allis, WI"', add
label define metarea_lbl 5120 `"Minneapolis-St. Paul, MN"', add
label define metarea_lbl 5121 `"Minneapolis-St. Paul-Bloomington, MN/WI"', add
label define metarea_lbl 5160 `"Mobile, AL"', add
label define metarea_lbl 5170 `"Modesto, CA"', add
label define metarea_lbl 5190 `"Monmouth-Ocean, NJ"', add
label define metarea_lbl 5200 `"Monroe, LA"', add
label define metarea_lbl 5220 `"Monroe, MI"', add
label define metarea_lbl 5240 `"Montgomery, Al"', add
label define metarea_lbl 5320 `"Muskegon-Norton Shores-Muskegon Heights, MI"', add
label define metarea_lbl 5321 `"Muskegon-Norton Shores, MI"', add
label define metarea_lbl 5330 `"Myrtle Beach, SC"', add
label define metarea_lbl 5331 `"Myrtle Beach-Conway-North Myrtle Beach, SC"', add
label define metarea_lbl 5340 `"Naples, FL"', add
label define metarea_lbl 5341 `"Naples-Marco Island, FL"', add
label define metarea_lbl 5350 `"Nashua, NH"', add
label define metarea_lbl 5360 `"Nashville, TN"', add
label define metarea_lbl 5361 `"Nashville-Davidson-Murfreesboro, TN"', add
label define metarea_lbl 5400 `"New Bedford, MA"', add
label define metarea_lbl 5480 `"New Haven-Meriden, CT"', add
label define metarea_lbl 5481 `"New Haven, CT"', add
label define metarea_lbl 5520 `"New London-Norwich, CT/RI"', add
label define metarea_lbl 5560 `"New Orleans, LA"', add
label define metarea_lbl 5561 `"New Orleans-Metairie-Kenner, LA"', add
label define metarea_lbl 5600 `"New York-Northeastern NJ"', add
label define metarea_lbl 5601 `"Nassau-Suffolk, NY"', add
label define metarea_lbl 5602 `"Bergen-Passaic, NJ"', add
label define metarea_lbl 5603 `"Jersey City, NJ"', add
label define metarea_lbl 5604 `"Middlesex-Somerset-Hunterdon, NJ"', add
label define metarea_lbl 5605 `"Newark, NJ"', add
label define metarea_lbl 5606 `"New York-Northern New Jersey-Long Island, NY-NJ-PA"', add
label define metarea_lbl 5607 `"New York, NY"', add
label define metarea_lbl 5640 `"Newark, OH"', add
label define metarea_lbl 5660 `"Newburgh-Middletown, NY"', add
label define metarea_lbl 5720 `"Norfolk-Virginia Beach-Newport News, VA"', add
label define metarea_lbl 5721 `"Virginia Beach-Norfolk-Newport News, VA/NC"', add
label define metarea_lbl 5760 `"Norwalk, CT"', add
label define metarea_lbl 5790 `"Ocala, FL"', add
label define metarea_lbl 5800 `"Odessa, TX"', add
label define metarea_lbl 5801 `"Midland, TX"', add
label define metarea_lbl 5840 `"Ocean City, NJ"', add
label define metarea_lbl 5880 `"Oklahoma City, OK"', add
label define metarea_lbl 5910 `"Olympia, WA"', add
label define metarea_lbl 5920 `"Omaha, NE/IA"', add
label define metarea_lbl 5921 `"Omaha-Council Bluffs, NE/IA"', add
label define metarea_lbl 5950 `"Orange, NY"', add
label define metarea_lbl 5960 `"Orlando, FL"', add
label define metarea_lbl 6010 `"Panama City, FL"', add
label define metarea_lbl 6011 `"Panama City-Lynn Haven, FL"', add
label define metarea_lbl 6080 `"Pensacola, FL"', add
label define metarea_lbl 6081 `"Pensacola-Ferry Pass-Brent, FL"', add
label define metarea_lbl 6120 `"Peoria, IL"', add
label define metarea_lbl 6160 `"Philadelphia, PA/NJ"', add
label define metarea_lbl 6161 `"Philadelphia-Camden-Wilmington, PA/NJ/DE"', add
label define metarea_lbl 6200 `"Phoenix, AZ"', add
label define metarea_lbl 6201 `"Phoenix-Mesa-Scottsdale, AZ"', add
label define metarea_lbl 6280 `"Pittsburg, PA"', add
label define metarea_lbl 6281 `"Beaver County"', add
label define metarea_lbl 6400 `"Portland, ME"', add
label define metarea_lbl 6401 `"Portland-South Portland, ME"', add
label define metarea_lbl 6440 `"Portland-Vancouver, OR/WA"', add
label define metarea_lbl 6441 `"Vancouver, WA"', add
label define metarea_lbl 6442 `"Portland-Vancouver-Beaverton, OR/WA"', add
label define metarea_lbl 6450 `"Portsmouth-Dover-Rochester, NH/ME"', add
label define metarea_lbl 6451 `"Portsmouth-Rochester, NH/ME MSA"', add
label define metarea_lbl 6452 `"Rochester-Dover, NH/ME"', add
label define metarea_lbl 6460 `"Poughkeepsie, NY"', add
label define metarea_lbl 6461 `"Poughkeepsie-Newburgh-Middletown, NY"', add
label define metarea_lbl 6470 `"Prescott, AZ"', add
label define metarea_lbl 6480 `"Providence-Fall River-Pawtucket, MA/RI"', add
label define metarea_lbl 6482 `"Pawtuckett-Woonsocket-Attleboro, RI/MA"', add
label define metarea_lbl 6483 `"Providence-Fall River-Warwick, MA-RI"', add
label define metarea_lbl 6520 `"Provo-Orem, UT"', add
label define metarea_lbl 6560 `"Pueblo, CO"', add
label define metarea_lbl 6580 `"Punta Gorda, FL"', add
label define metarea_lbl 6600 `"Racine, WI"', add
label define metarea_lbl 6640 `"Raleigh-Durham, NC"', add
label define metarea_lbl 6641 `"Durham, NC"', add
label define metarea_lbl 6642 `"Raleigh-Carey, NC"', add
label define metarea_lbl 6680 `"Reading, PA"', add
label define metarea_lbl 6720 `"Reno, NV"', add
label define metarea_lbl 6721 `"Reno-Sparks, NV"', add
label define metarea_lbl 6760 `"Richmond-Petersburg, VA"', add
label define metarea_lbl 6761 `"Richmond, VA"', add
label define metarea_lbl 6780 `"Riverside-San Bernadino, CA"', add
label define metarea_lbl 6800 `"Roanoke, VA"', add
label define metarea_lbl 6840 `"Rochester, NY"', add
label define metarea_lbl 6880 `"Rockford, IL"', add
label define metarea_lbl 6920 `"Sacramento, CA"', add
label define metarea_lbl 6921 `"Sacramento-Arden Arcade-Roseville, CA"', add
label define metarea_lbl 6960 `"Saginaw-Bay City-Midland, MI"', add
label define metarea_lbl 6961 `"Saginaw-Saginaw Township North, MI"', add
label define metarea_lbl 6980 `"St. Cloud, MN"', add
label define metarea_lbl 7040 `"St. Louis, MO/IL"', add
label define metarea_lbl 7080 `"Salem, OR"', add
label define metarea_lbl 7120 `"Salinas-Sea Side-Monterey, CA"', add
label define metarea_lbl 7121 `"Salinas, CA"', add
label define metarea_lbl 7130 `"Salisbury, MD"', add
label define metarea_lbl 7160 `"Salt Lake City-Ogden, UT"', add
label define metarea_lbl 7161 `"Salt Lake City, UT"', add
label define metarea_lbl 7162 `"Ogden-Clearfield, UT"', add
label define metarea_lbl 7240 `"San Antonio, TX"', add
label define metarea_lbl 7320 `"San Diego, CA"', add
label define metarea_lbl 7321 `"San Diego-Carlsbad-San Marcos, CA"', add
label define metarea_lbl 7360 `"San Francisco-Oaklan-Vallejo, CA"', add
label define metarea_lbl 7361 `"Oakland, CA"', add
label define metarea_lbl 7362 `"Vallejo-Fairfield-Napa, CA"', add
label define metarea_lbl 7363 `"Vallejo-Fairfield, CA"', add
label define metarea_lbl 7364 `"Napa, CA"', add
label define metarea_lbl 7365 `"San Francisco-Oakland-Fremont, CA"', add
label define metarea_lbl 7400 `"San Jose, CA"', add
label define metarea_lbl 7401 `"San Jose-Sunnyvale-Santa Clara, CA"', add
label define metarea_lbl 7460 `"San Luis Obispo-Atascadero-Paso Robles, CA"', add
label define metarea_lbl 7461 `"San Luis Obispo-Paso Robles, CA"', add
label define metarea_lbl 7470 `"Santa Barbara-Santa Maria-Lompoc, CA"', add
label define metarea_lbl 7471 `"Santa Barbara-Santa Maria-Goleta, CA"', add
label define metarea_lbl 7480 `"Santa Cruz, CA"', add
label define metarea_lbl 7481 `"Santa Cruz-Watsonville, CA"', add
label define metarea_lbl 7490 `"Santa Fe, NM"', add
label define metarea_lbl 7500 `"Santa Rosa-Petaluma, CA"', add
label define metarea_lbl 7510 `"Sarasota, FL"', add
label define metarea_lbl 7511 `"Sarasota-Bradenton-Venice, FL"', add
label define metarea_lbl 7520 `"Savannah, GA"', add
label define metarea_lbl 7560 `"Scranton-Wilkes-Barre, PA"', add
label define metarea_lbl 7600 `"Seattle-Everett, WA"', add
label define metarea_lbl 7601 `"Seattle-Tacoma-Bellevue, WA"', add
label define metarea_lbl 7610 `"Sharon, PA"', add
label define metarea_lbl 7680 `"Shreveport, LA"', add
label define metarea_lbl 7681 `"Shreveport-Bossier City, LA"', add
label define metarea_lbl 7720 `"Sioux City, IA-NE"', add
label define metarea_lbl 7760 `"Sioux Falls, SD"', add
label define metarea_lbl 7800 `"South Bend-Mishawaka, IN"', add
label define metarea_lbl 7840 `"Spokane, WA"', add
label define metarea_lbl 7880 `"Springfield, IL"', add
label define metarea_lbl 7920 `"Springfield, MO"', add
label define metarea_lbl 8000 `"Springfield-Holyoke-Chicopee, MA"', add
label define metarea_lbl 8001 `"Springfield, MA/CT"', add
label define metarea_lbl 8040 `"Stamford, CT"', add
label define metarea_lbl 8120 `"Stockton, CA"', add
label define metarea_lbl 8160 `"Syracuse, NY"', add
label define metarea_lbl 8200 `"Tacoma, WA"', add
label define metarea_lbl 8240 `"Tallahassee, FL"', add
label define metarea_lbl 8280 `"Tampa-St. Petersburg-Clearwater, FL"', add
label define metarea_lbl 8320 `"Terre Haute, IN"', add
label define metarea_lbl 8400 `"Toledo, OH/MI"', add
label define metarea_lbl 8440 `"Topeka, KS"', add
label define metarea_lbl 8480 `"Trenton, NJ"', add
label define metarea_lbl 8481 `"Trenton-Ewing, NJ"', add
label define metarea_lbl 8520 `"Tucson, AZ"', add
label define metarea_lbl 8560 `"Tulsa, OK"', add
label define metarea_lbl 8600 `"Tuscaloosa, AL"', add
label define metarea_lbl 8680 `"Utica-Rome, NY"', add
label define metarea_lbl 8700 `"Valdosta, GA"', add
label define metarea_lbl 8730 `"Ventura-Oxnard-Simi Valley, CA"', add
label define metarea_lbl 8731 `"Oxnard-Thousand Oaks-Ventura, CA"', add
label define metarea_lbl 8740 `"Vero Beach, FL"', add
label define metarea_lbl 8750 `"Victoria, TX"', add
label define metarea_lbl 8760 `"Vineland-Milville-Bridgetown, NJ"', add
label define metarea_lbl 8780 `"Visalia-Tulare-Porterville, CA"', add
label define metarea_lbl 8781 `"Visalia-Porterville, CA"', add
label define metarea_lbl 8800 `"Waco, TX"', add
label define metarea_lbl 8840 `"Washington, DC/MD/VA"', add
label define metarea_lbl 8880 `"Waterbury, CT"', add
label define metarea_lbl 8920 `"Waterloo-Cedar Falls, IA"', add
label define metarea_lbl 8940 `"Wausau, WI"', add
label define metarea_lbl 8960 `"West Palm Beach-Boca Raton-Delray Beach, FL"', add
label define metarea_lbl 9000 `"Wheeling, WV/OH"', add
label define metarea_lbl 9040 `"Wichita, KS"', add
label define metarea_lbl 9140 `"Williamsport, PA"', add
label define metarea_lbl 9160 `"Wilmington, DE/NJ/MD"', add
label define metarea_lbl 9200 `"Wilmington, NC"', add
label define metarea_lbl 9240 `"Worcester, MA"', add
label define metarea_lbl 9260 `"Yakima, WA"', add
label define metarea_lbl 9270 `"Yolo, CA"', add
label define metarea_lbl 9280 `"York, PA"', add
label define metarea_lbl 9281 `"York-Hanover, PA"', add
label define metarea_lbl 9320 `"Youngstown-Warren, OH/PA"', add
label define metarea_lbl 9321 `"Youngstown-Warren-Boardman, OH"', add
label define metarea_lbl 9340 `"Yuba City, CA"', add
label define metarea_lbl 9360 `"Yuma, AZ"', add
label define metarea_lbl 9997 `"Other metropolitan areas, unidentified"', add
label define metarea_lbl 9998 `"NIU, household not in a metropolitan area"', add
label define metarea_lbl 9999 `"Missing data"', add
label values metarea metarea_lbl

label define lunchsub_lbl 0 `"NIU"'
label define lunchsub_lbl 1 `"Yes, children receive free or reduced price lunch"', add
label define lunchsub_lbl 2 `"No, children did not receive free or reduced price lunch"', add
label values lunchsub lunchsub_lbl

label define frelunch_lbl 00 `"Zero"'
label define frelunch_lbl 01 `"One"', add
label define frelunch_lbl 02 `"Two"', add
label define frelunch_lbl 03 `"Three"', add
label define frelunch_lbl 04 `"Four"', add
label define frelunch_lbl 05 `"Five"', add
label define frelunch_lbl 06 `"Six"', add
label define frelunch_lbl 07 `"Seven"', add
label define frelunch_lbl 08 `"Eight"', add
label define frelunch_lbl 09 `"Nine or more"', add
label define frelunch_lbl 98 `"NIU -- Children didn't eat hot lunch"', add
label define frelunch_lbl 99 `"NIU -- No children in hh"', add
label values frelunch frelunch_lbl

label define month_lbl 01 `"January"'
label define month_lbl 02 `"February"', add
label define month_lbl 03 `"March"', add
label define month_lbl 04 `"April"', add
label define month_lbl 05 `"May"', add
label define month_lbl 06 `"June"', add
label define month_lbl 07 `"July"', add
label define month_lbl 08 `"August"', add
label define month_lbl 09 `"September"', add
label define month_lbl 10 `"October"', add
label define month_lbl 11 `"November"', add
label define month_lbl 12 `"December"', add
label values month month_lbl

label define age_lbl 00 `"Under 1 year"'
label define age_lbl 01 `"1"', add
label define age_lbl 02 `"2"', add
label define age_lbl 03 `"3"', add
label define age_lbl 04 `"4"', add
label define age_lbl 05 `"5"', add
label define age_lbl 06 `"6"', add
label define age_lbl 07 `"7"', add
label define age_lbl 08 `"8"', add
label define age_lbl 09 `"9"', add
label define age_lbl 10 `"10"', add
label define age_lbl 11 `"11"', add
label define age_lbl 12 `"12"', add
label define age_lbl 13 `"13"', add
label define age_lbl 14 `"14"', add
label define age_lbl 15 `"15"', add
label define age_lbl 16 `"16"', add
label define age_lbl 17 `"17"', add
label define age_lbl 18 `"18"', add
label define age_lbl 19 `"19"', add
label define age_lbl 20 `"20"', add
label define age_lbl 21 `"21"', add
label define age_lbl 22 `"22"', add
label define age_lbl 23 `"23"', add
label define age_lbl 24 `"24"', add
label define age_lbl 25 `"25"', add
label define age_lbl 26 `"26"', add
label define age_lbl 27 `"27"', add
label define age_lbl 28 `"28"', add
label define age_lbl 29 `"29"', add
label define age_lbl 30 `"30"', add
label define age_lbl 31 `"31"', add
label define age_lbl 32 `"32"', add
label define age_lbl 33 `"33"', add
label define age_lbl 34 `"34"', add
label define age_lbl 35 `"35"', add
label define age_lbl 36 `"36"', add
label define age_lbl 37 `"37"', add
label define age_lbl 38 `"38"', add
label define age_lbl 39 `"39"', add
label define age_lbl 40 `"40"', add
label define age_lbl 41 `"41"', add
label define age_lbl 42 `"42"', add
label define age_lbl 43 `"43"', add
label define age_lbl 44 `"44"', add
label define age_lbl 45 `"45"', add
label define age_lbl 46 `"46"', add
label define age_lbl 47 `"47"', add
label define age_lbl 48 `"48"', add
label define age_lbl 49 `"49"', add
label define age_lbl 50 `"50"', add
label define age_lbl 51 `"51"', add
label define age_lbl 52 `"52"', add
label define age_lbl 53 `"53"', add
label define age_lbl 54 `"54"', add
label define age_lbl 55 `"55"', add
label define age_lbl 56 `"56"', add
label define age_lbl 57 `"57"', add
label define age_lbl 58 `"58"', add
label define age_lbl 59 `"59"', add
label define age_lbl 60 `"60"', add
label define age_lbl 61 `"61"', add
label define age_lbl 62 `"62"', add
label define age_lbl 63 `"63"', add
label define age_lbl 64 `"64"', add
label define age_lbl 65 `"65"', add
label define age_lbl 66 `"66"', add
label define age_lbl 67 `"67"', add
label define age_lbl 68 `"68"', add
label define age_lbl 69 `"69"', add
label define age_lbl 70 `"70"', add
label define age_lbl 71 `"71"', add
label define age_lbl 72 `"72"', add
label define age_lbl 73 `"73"', add
label define age_lbl 74 `"74"', add
label define age_lbl 75 `"75"', add
label define age_lbl 76 `"76"', add
label define age_lbl 77 `"77"', add
label define age_lbl 78 `"78"', add
label define age_lbl 79 `"79"', add
label define age_lbl 80 `"80"', add
label define age_lbl 81 `"81"', add
label define age_lbl 82 `"82"', add
label define age_lbl 83 `"83"', add
label define age_lbl 84 `"84"', add
label define age_lbl 85 `"85"', add
label define age_lbl 86 `"86"', add
label define age_lbl 87 `"87"', add
label define age_lbl 88 `"88"', add
label define age_lbl 89 `"89"', add
label define age_lbl 90 `"90 (90+, 1988-2002)"', add
label define age_lbl 91 `"91"', add
label define age_lbl 92 `"92"', add
label define age_lbl 93 `"93"', add
label define age_lbl 94 `"94"', add
label define age_lbl 95 `"95"', add
label define age_lbl 96 `"96"', add
label define age_lbl 97 `"97"', add
label define age_lbl 98 `"98"', add
label define age_lbl 99 `"99+"', add
label values age age_lbl

label define sex_lbl 1 `"Male"'
label define sex_lbl 2 `"Female"', add
label define sex_lbl 9 `"NIU"', add
label values sex sex_lbl

label define race_lbl 100 `"White"'
label define race_lbl 200 `"Black/Negro"', add
label define race_lbl 300 `"American Indian/Aleut/Eskimo"', add
label define race_lbl 650 `"Asian or Pacific Islander"', add
label define race_lbl 651 `"Asian only"', add
label define race_lbl 652 `"Hawaiian/Pacific Islander only"', add
label define race_lbl 700 `"Other (single) race, n.e.c."', add
label define race_lbl 801 `"White-Black"', add
label define race_lbl 802 `"White-American Indian"', add
label define race_lbl 803 `"White-Asian"', add
label define race_lbl 804 `"White-Hawaiian/Pacific Islander"', add
label define race_lbl 805 `"Black-American Indian"', add
label define race_lbl 806 `"Black-Asian"', add
label define race_lbl 807 `"Black-Hawaiian/Pacific Islander"', add
label define race_lbl 808 `"American Indian-Asian"', add
label define race_lbl 809 `"Asian-Hawaiian/Pacific Islander"', add
label define race_lbl 810 `"White-Black-American Indian"', add
label define race_lbl 811 `"White-Black-Asian"', add
label define race_lbl 812 `"White-American Indian-Asian"', add
label define race_lbl 813 `"White-Asian-Hawaiian/Pacific Islander"', add
label define race_lbl 814 `"White-Black-American Indian-Asian"', add
label define race_lbl 815 `"American Indian-Hawaiian/Pacific Islander"', add
label define race_lbl 816 `"White-Black--Hawaiian/Pacific Islander"', add
label define race_lbl 817 `"White-American Indian-Hawaiian/Pacific Islander"', add
label define race_lbl 818 `"Black-American Indian-Asian"', add
label define race_lbl 819 `"White-American Indian-Asian-Hawaiian/Pacific Islander"', add
label define race_lbl 820 `"Two or three races, unspecified"', add
label define race_lbl 830 `"Four or five races, unspecified"', add
label define race_lbl 999 `"NIU"', add
label values race race_lbl

label define marst_lbl 1 `"Married, spouse present"'
label define marst_lbl 2 `"Married, spouse absent"', add
label define marst_lbl 3 `"Separated"', add
label define marst_lbl 4 `"Divorced"', add
label define marst_lbl 5 `"Widowed"', add
label define marst_lbl 6 `"Never married/single"', add
label define marst_lbl 9 `"Unknown"', add
label values marst marst_lbl

label define hispan_lbl 000 `"Not Hispanic"'
label define hispan_lbl 100 `"Mexican"', add
label define hispan_lbl 102 `"Mexican American"', add
label define hispan_lbl 103 `"Mexicano/Mexicana"', add
label define hispan_lbl 104 `"Chicano/Chicana"', add
label define hispan_lbl 108 `"Mexican (Mexicano)"', add
label define hispan_lbl 109 `"Mexicano/Chicano"', add
label define hispan_lbl 200 `"Puerto Rican"', add
label define hispan_lbl 300 `"Cuban"', add
label define hispan_lbl 400 `"Other Spanish"', add
label define hispan_lbl 410 `"Central/South American"', add
label define hispan_lbl 901 `"Do not know"', add
label define hispan_lbl 902 `"N/A (and no response 1985-87)"', add
label values hispan hispan_lbl

label define educ_lbl 000 `"NIU or no schooling"'
label define educ_lbl 001 `"NIU"', add
label define educ_lbl 002 `"None or preschool"', add
label define educ_lbl 010 `"Grades 1, 2, 3, or 4"', add
label define educ_lbl 011 `"Grade 1"', add
label define educ_lbl 012 `"Grade 2"', add
label define educ_lbl 013 `"Grade 3"', add
label define educ_lbl 014 `"Grade 4"', add
label define educ_lbl 020 `"Grades 5 or 6"', add
label define educ_lbl 021 `"Grade 5"', add
label define educ_lbl 022 `"Grade 6"', add
label define educ_lbl 030 `"Grades 7 or 8"', add
label define educ_lbl 031 `"Grade 7"', add
label define educ_lbl 032 `"Grade 8"', add
label define educ_lbl 040 `"Grade 9"', add
label define educ_lbl 050 `"Grade 10"', add
label define educ_lbl 060 `"Grade 11"', add
label define educ_lbl 070 `"Grade 12"', add
label define educ_lbl 071 `"12th grade, no diploma"', add
label define educ_lbl 072 `"12th grade, diploma unclear"', add
label define educ_lbl 073 `"High school diploma or equivalent"', add
label define educ_lbl 080 `"1 year of college"', add
label define educ_lbl 081 `"Some college but no degree"', add
label define educ_lbl 090 `"2 years of college"', add
label define educ_lbl 091 `"Associate's degree, occupational/vocational program"', add
label define educ_lbl 092 `"Associate's degree, academic program"', add
label define educ_lbl 100 `"3 years of college"', add
label define educ_lbl 110 `"4 years of college"', add
label define educ_lbl 111 `"Bachelor's degree"', add
label define educ_lbl 120 `"5+ years of college"', add
label define educ_lbl 121 `"5 years of college"', add
label define educ_lbl 122 `"6+ years of college"', add
label define educ_lbl 123 `"Master's degree"', add
label define educ_lbl 124 `"Professional school degree"', add
label define educ_lbl 125 `"Doctorate degree"', add
label define educ_lbl 999 `"Missing/Unknown"', add
label values educ educ_lbl

label define schlcoll_lbl 0 `"NIU"'
label define schlcoll_lbl 1 `"High school full time"', add
label define schlcoll_lbl 2 `"High school part time"', add
label define schlcoll_lbl 3 `"College or university full time"', add
label define schlcoll_lbl 4 `"College or university part time"', add
label define schlcoll_lbl 5 `"Does not attend school, college or university"', add
label values schlcoll schlcoll_lbl

label define occ_lbl 0000 `"0"'
label define occ_lbl 0001 `"1"', add
label define occ_lbl 0002 `"2"', add
label define occ_lbl 0003 `"3"', add
label define occ_lbl 0004 `"4"', add
label define occ_lbl 0005 `"5"', add
label define occ_lbl 0006 `"6"', add
label define occ_lbl 0007 `"7"', add
label define occ_lbl 0008 `"8"', add
label define occ_lbl 0009 `"9"', add
label define occ_lbl 0010 `"10"', add
label define occ_lbl 0011 `"11"', add
label define occ_lbl 0012 `"12"', add
label define occ_lbl 0013 `"13"', add
label define occ_lbl 0014 `"14"', add
label define occ_lbl 0015 `"15"', add
label define occ_lbl 0016 `"16"', add
label define occ_lbl 0017 `"17"', add
label define occ_lbl 0018 `"18"', add
label define occ_lbl 0019 `"19"', add
label define occ_lbl 0020 `"20"', add
label define occ_lbl 0021 `"21"', add
label define occ_lbl 0022 `"22"', add
label define occ_lbl 0023 `"23"', add
label define occ_lbl 0024 `"24"', add
label define occ_lbl 0025 `"25"', add
label define occ_lbl 0026 `"26"', add
label define occ_lbl 0027 `"27"', add
label define occ_lbl 0028 `"28"', add
label define occ_lbl 0029 `"29"', add
label define occ_lbl 0030 `"30"', add
label define occ_lbl 0031 `"31"', add
label define occ_lbl 0032 `"32"', add
label define occ_lbl 0033 `"33"', add
label define occ_lbl 0034 `"34"', add
label define occ_lbl 0035 `"35"', add
label define occ_lbl 0036 `"36"', add
label define occ_lbl 0037 `"37"', add
label define occ_lbl 0099 `"99"', add
label values occ occ_lbl

label define ind_lbl 0000 `"0"'
label values ind ind_lbl

label define classwkr_lbl 00 `"NIU"'
label define classwkr_lbl 10 `"Self-employed"', add
label define classwkr_lbl 13 `"Self-employed, not incorporated"', add
label define classwkr_lbl 14 `"Self-employed, incorporated"', add
label define classwkr_lbl 20 `"Works for wages or salary"', add
label define classwkr_lbl 21 `"Wage/salary, private"', add
label define classwkr_lbl 22 `"Private, for profit"', add
label define classwkr_lbl 23 `"Private, nonprofit"', add
label define classwkr_lbl 24 `"Wage/salary, government"', add
label define classwkr_lbl 25 `"Federal government employee"', add
label define classwkr_lbl 26 `"Armed forces"', add
label define classwkr_lbl 27 `"State government employee"', add
label define classwkr_lbl 28 `"Local government employee"', add
label define classwkr_lbl 29 `"Unpaid family worker"', add
label define classwkr_lbl 99 `"Missing/Unknown"', add
label values classwkr classwkr_lbl

label define wkswork1_lbl 00 `"NIU"'
label define wkswork1_lbl 01 `"1 week"', add
label define wkswork1_lbl 02 `"2 weeks"', add
label define wkswork1_lbl 03 `"3 weeks"', add
label define wkswork1_lbl 04 `"4 weeks"', add
label define wkswork1_lbl 05 `"5 weeks"', add
label define wkswork1_lbl 06 `"6 weeks"', add
label define wkswork1_lbl 07 `"7 weeks"', add
label define wkswork1_lbl 08 `"8 weeks"', add
label define wkswork1_lbl 09 `"9 weeks"', add
label define wkswork1_lbl 10 `"10 weeks"', add
label define wkswork1_lbl 11 `"11 weeks"', add
label define wkswork1_lbl 12 `"12 weeks"', add
label define wkswork1_lbl 13 `"13 weeks"', add
label define wkswork1_lbl 14 `"14 weeks"', add
label define wkswork1_lbl 15 `"15 weeks"', add
label define wkswork1_lbl 16 `"16 weeks"', add
label define wkswork1_lbl 17 `"17 weeks"', add
label define wkswork1_lbl 18 `"18 weeks"', add
label define wkswork1_lbl 19 `"19 weeks"', add
label define wkswork1_lbl 20 `"20 weeks"', add
label define wkswork1_lbl 21 `"21 weeks"', add
label define wkswork1_lbl 22 `"22 weeks"', add
label define wkswork1_lbl 23 `"23 weeks"', add
label define wkswork1_lbl 24 `"24 weeks"', add
label define wkswork1_lbl 25 `"25 weeks"', add
label define wkswork1_lbl 26 `"26 weeks"', add
label define wkswork1_lbl 27 `"27 weeks"', add
label define wkswork1_lbl 28 `"28 weeks"', add
label define wkswork1_lbl 29 `"29 weeks"', add
label define wkswork1_lbl 30 `"30 weeks"', add
label define wkswork1_lbl 31 `"31 weeks"', add
label define wkswork1_lbl 32 `"32 weeks"', add
label define wkswork1_lbl 33 `"33 weeks"', add
label define wkswork1_lbl 34 `"34 weeks"', add
label define wkswork1_lbl 35 `"35 weeks"', add
label define wkswork1_lbl 36 `"36 weeks"', add
label define wkswork1_lbl 37 `"37 weeks"', add
label define wkswork1_lbl 38 `"38 weeks"', add
label define wkswork1_lbl 39 `"39 weeks"', add
label define wkswork1_lbl 40 `"40 weeks"', add
label define wkswork1_lbl 41 `"41 weeks"', add
label define wkswork1_lbl 42 `"42 weeks"', add
label define wkswork1_lbl 43 `"43 weeks"', add
label define wkswork1_lbl 44 `"44 weeks"', add
label define wkswork1_lbl 45 `"45 weeks"', add
label define wkswork1_lbl 46 `"46 weeks"', add
label define wkswork1_lbl 47 `"47 weeks"', add
label define wkswork1_lbl 48 `"48 weeks"', add
label define wkswork1_lbl 49 `"49 weeks"', add
label define wkswork1_lbl 50 `"50 weeks"', add
label define wkswork1_lbl 51 `"51 weeks"', add
label define wkswork1_lbl 52 `"52 weeks"', add
label values wkswork1 wkswork1_lbl

label define hrswork_lbl 00 `"NIU"'
label define hrswork_lbl 01 `"1 hour"', add
label define hrswork_lbl 02 `"2 hours"', add
label define hrswork_lbl 03 `"3 hours"', add
label define hrswork_lbl 04 `"4 hours"', add
label define hrswork_lbl 05 `"5 hours"', add
label define hrswork_lbl 06 `"6 hours"', add
label define hrswork_lbl 07 `"7 hours"', add
label define hrswork_lbl 08 `"8 hours"', add
label define hrswork_lbl 09 `"9 hours"', add
label define hrswork_lbl 10 `"10 hours"', add
label define hrswork_lbl 11 `"11 hours"', add
label define hrswork_lbl 12 `"12 hours"', add
label define hrswork_lbl 13 `"13 hours"', add
label define hrswork_lbl 14 `"14 hours"', add
label define hrswork_lbl 15 `"15 hours"', add
label define hrswork_lbl 16 `"16 hours"', add
label define hrswork_lbl 17 `"17 hours"', add
label define hrswork_lbl 18 `"18 hours"', add
label define hrswork_lbl 19 `"19 hours"', add
label define hrswork_lbl 20 `"20 hours"', add
label define hrswork_lbl 21 `"21 hours"', add
label define hrswork_lbl 22 `"22 hours"', add
label define hrswork_lbl 23 `"23 hours"', add
label define hrswork_lbl 24 `"24 hours"', add
label define hrswork_lbl 25 `"25 hours"', add
label define hrswork_lbl 26 `"26 hours"', add
label define hrswork_lbl 27 `"27 hours"', add
label define hrswork_lbl 28 `"28 hours"', add
label define hrswork_lbl 29 `"29 hours"', add
label define hrswork_lbl 30 `"30 hours"', add
label define hrswork_lbl 31 `"31 hours"', add
label define hrswork_lbl 32 `"32 hours"', add
label define hrswork_lbl 33 `"33 hours"', add
label define hrswork_lbl 34 `"34 hours"', add
label define hrswork_lbl 35 `"35 hours"', add
label define hrswork_lbl 36 `"36 hours"', add
label define hrswork_lbl 37 `"37 hours"', add
label define hrswork_lbl 38 `"38 hours"', add
label define hrswork_lbl 39 `"39 hours"', add
label define hrswork_lbl 40 `"40 hours"', add
label define hrswork_lbl 41 `"41 hours"', add
label define hrswork_lbl 42 `"42 hours"', add
label define hrswork_lbl 43 `"43 hours"', add
label define hrswork_lbl 44 `"44 hours"', add
label define hrswork_lbl 45 `"45 hours"', add
label define hrswork_lbl 46 `"46 hours"', add
label define hrswork_lbl 47 `"47 hours"', add
label define hrswork_lbl 48 `"48 hours"', add
label define hrswork_lbl 49 `"49 hours"', add
label define hrswork_lbl 50 `"50 hours"', add
label define hrswork_lbl 51 `"51 hours"', add
label define hrswork_lbl 52 `"52 hours"', add
label define hrswork_lbl 53 `"53 hours"', add
label define hrswork_lbl 54 `"54 hours"', add
label define hrswork_lbl 55 `"55 hours"', add
label define hrswork_lbl 56 `"56 hours"', add
label define hrswork_lbl 57 `"57 hours"', add
label define hrswork_lbl 58 `"58 hours"', add
label define hrswork_lbl 59 `"59 hours"', add
label define hrswork_lbl 60 `"60 hours"', add
label define hrswork_lbl 61 `"61 hours"', add
label define hrswork_lbl 62 `"62 hours"', add
label define hrswork_lbl 63 `"63 hours"', add
label define hrswork_lbl 64 `"64 hours"', add
label define hrswork_lbl 65 `"65 hours"', add
label define hrswork_lbl 66 `"66 hours"', add
label define hrswork_lbl 67 `"67 hours"', add
label define hrswork_lbl 68 `"68 hours"', add
label define hrswork_lbl 69 `"69 hours"', add
label define hrswork_lbl 70 `"70 hours"', add
label define hrswork_lbl 71 `"71 hours"', add
label define hrswork_lbl 72 `"72 hours"', add
label define hrswork_lbl 73 `"73 hours"', add
label define hrswork_lbl 74 `"74 hours"', add
label define hrswork_lbl 75 `"75 hours"', add
label define hrswork_lbl 76 `"76 hours"', add
label define hrswork_lbl 77 `"77 hours"', add
label define hrswork_lbl 78 `"78 hours"', add
label define hrswork_lbl 79 `"79 hours"', add
label define hrswork_lbl 80 `"80 hours"', add
label define hrswork_lbl 81 `"81 hours"', add
label define hrswork_lbl 82 `"82 hours"', add
label define hrswork_lbl 83 `"83 hours"', add
label define hrswork_lbl 84 `"84 hours"', add
label define hrswork_lbl 85 `"85 hours"', add
label define hrswork_lbl 86 `"86 hours"', add
label define hrswork_lbl 87 `"87 hours"', add
label define hrswork_lbl 88 `"88 hours"', add
label define hrswork_lbl 89 `"89 hours"', add
label define hrswork_lbl 90 `"90 hours"', add
label define hrswork_lbl 91 `"91 hours"', add
label define hrswork_lbl 92 `"92 hours"', add
label define hrswork_lbl 93 `"93 hours"', add
label define hrswork_lbl 94 `"94 hours"', add
label define hrswork_lbl 95 `"95 hours"', add
label define hrswork_lbl 96 `"96 hours"', add
label define hrswork_lbl 97 `"97 hours"', add
label define hrswork_lbl 98 `"98 hours"', add
label define hrswork_lbl 99 `"99 hours"', add
label values hrswork hrswork_lbl

label define uhrswork_lbl 00 `"NIU"'
label define uhrswork_lbl 01 `"1 hour"', add
label define uhrswork_lbl 02 `"2 hours"', add
label define uhrswork_lbl 03 `"3 hours"', add
label define uhrswork_lbl 04 `"4 hours"', add
label define uhrswork_lbl 05 `"5 hours"', add
label define uhrswork_lbl 06 `"6 hours"', add
label define uhrswork_lbl 07 `"7 hours"', add
label define uhrswork_lbl 08 `"8 hours"', add
label define uhrswork_lbl 09 `"9 hours"', add
label define uhrswork_lbl 10 `"10 hours"', add
label define uhrswork_lbl 11 `"11 hours"', add
label define uhrswork_lbl 12 `"12 hours"', add
label define uhrswork_lbl 13 `"13 hours"', add
label define uhrswork_lbl 14 `"14 hours"', add
label define uhrswork_lbl 15 `"15 hours"', add
label define uhrswork_lbl 16 `"16 hours"', add
label define uhrswork_lbl 17 `"17 hours"', add
label define uhrswork_lbl 18 `"18 hours"', add
label define uhrswork_lbl 19 `"19 hours"', add
label define uhrswork_lbl 20 `"20 hours"', add
label define uhrswork_lbl 21 `"21 hours"', add
label define uhrswork_lbl 22 `"22 hours"', add
label define uhrswork_lbl 23 `"23 hours"', add
label define uhrswork_lbl 24 `"24 hours"', add
label define uhrswork_lbl 25 `"25 hours"', add
label define uhrswork_lbl 26 `"26 hours"', add
label define uhrswork_lbl 27 `"27 hours"', add
label define uhrswork_lbl 28 `"28 hours"', add
label define uhrswork_lbl 29 `"29 hours"', add
label define uhrswork_lbl 30 `"30 hours"', add
label define uhrswork_lbl 31 `"31 hours"', add
label define uhrswork_lbl 32 `"32 hours"', add
label define uhrswork_lbl 33 `"33 hours"', add
label define uhrswork_lbl 34 `"34 hours"', add
label define uhrswork_lbl 35 `"35 hours"', add
label define uhrswork_lbl 36 `"36 hours"', add
label define uhrswork_lbl 37 `"37 hours"', add
label define uhrswork_lbl 38 `"38 hours"', add
label define uhrswork_lbl 39 `"39 hours"', add
label define uhrswork_lbl 40 `"40 hours"', add
label define uhrswork_lbl 41 `"41 hours"', add
label define uhrswork_lbl 42 `"42 hours"', add
label define uhrswork_lbl 43 `"43 hours"', add
label define uhrswork_lbl 44 `"44 hours"', add
label define uhrswork_lbl 45 `"45 hours"', add
label define uhrswork_lbl 46 `"46 hours"', add
label define uhrswork_lbl 47 `"47 hours"', add
label define uhrswork_lbl 48 `"48 hours"', add
label define uhrswork_lbl 49 `"49 hours"', add
label define uhrswork_lbl 50 `"50 hours"', add
label define uhrswork_lbl 51 `"51 hours"', add
label define uhrswork_lbl 52 `"52 hours"', add
label define uhrswork_lbl 53 `"53 hours"', add
label define uhrswork_lbl 54 `"54 hours"', add
label define uhrswork_lbl 55 `"55 hours"', add
label define uhrswork_lbl 56 `"56 hours"', add
label define uhrswork_lbl 57 `"57 hours"', add
label define uhrswork_lbl 58 `"58 hours"', add
label define uhrswork_lbl 59 `"59 hours"', add
label define uhrswork_lbl 60 `"60 hours"', add
label define uhrswork_lbl 61 `"61 hours"', add
label define uhrswork_lbl 62 `"62 hours"', add
label define uhrswork_lbl 63 `"63 hours"', add
label define uhrswork_lbl 64 `"64 hours"', add
label define uhrswork_lbl 65 `"65 hours"', add
label define uhrswork_lbl 66 `"66 hours"', add
label define uhrswork_lbl 67 `"67 hours"', add
label define uhrswork_lbl 68 `"68 hours"', add
label define uhrswork_lbl 69 `"69 hours"', add
label define uhrswork_lbl 70 `"70 hours"', add
label define uhrswork_lbl 71 `"71 hours"', add
label define uhrswork_lbl 72 `"72 hours"', add
label define uhrswork_lbl 73 `"73 hours"', add
label define uhrswork_lbl 74 `"74 hours"', add
label define uhrswork_lbl 75 `"75 hours"', add
label define uhrswork_lbl 76 `"76 hours"', add
label define uhrswork_lbl 77 `"77 hours"', add
label define uhrswork_lbl 78 `"78 hours"', add
label define uhrswork_lbl 79 `"79 hours"', add
label define uhrswork_lbl 80 `"80 hours"', add
label define uhrswork_lbl 81 `"81 hours"', add
label define uhrswork_lbl 82 `"82 hours"', add
label define uhrswork_lbl 83 `"83 hours"', add
label define uhrswork_lbl 84 `"84 hours"', add
label define uhrswork_lbl 85 `"85 hours"', add
label define uhrswork_lbl 86 `"86 hours"', add
label define uhrswork_lbl 87 `"87 hours"', add
label define uhrswork_lbl 88 `"88 hours"', add
label define uhrswork_lbl 89 `"89 hours"', add
label define uhrswork_lbl 90 `"90 hours"', add
label define uhrswork_lbl 91 `"91 hours"', add
label define uhrswork_lbl 92 `"92 hours"', add
label define uhrswork_lbl 93 `"93 hours"', add
label define uhrswork_lbl 94 `"94 hours"', add
label define uhrswork_lbl 95 `"95 hours"', add
label define uhrswork_lbl 96 `"96 hours"', add
label define uhrswork_lbl 97 `"97 hours"', add
label define uhrswork_lbl 98 `"98 hours"', add
label define uhrswork_lbl 99 `"99 hours plus"', add
label values uhrswork uhrswork_lbl

label define union_lbl 0 `"NIU"'
label define union_lbl 1 `"No union coverage"', add
label define union_lbl 2 `"Member of labor union"', add
label define union_lbl 3 `"Covered by union but not a member"', add
label values union union_lbl

label define ftotval_lbl 0000999999 `"999999"'
label values ftotval ftotval_lbl

label define inctot_lbl 00999997 `"00999997"'
label define inctot_lbl 99999997 `"99999997"', add
label define inctot_lbl 99999999 `"99999999"', add
label values inctot inctot_lbl

label define incwage_lbl 9999997 `"9999997"'
label define incwage_lbl 9999999 `"9999999"', add
label values incwage incwage_lbl

label define incbus_lbl 9999997 `"9999997"'
label define incbus_lbl 9999999 `"9999999"', add
label values incbus incbus_lbl

label define incfarm_lbl 9999997 `"9999997"'
label define incfarm_lbl 9999999 `"9999999"', add
label values incfarm incfarm_lbl

label define incdivid_lbl 099997 `"099997"'
label define incdivid_lbl 999997 `"999997"', add
label values incdivid incdivid_lbl

label define inclongj_lbl 0099997 `"0099997"'
label define inclongj_lbl 9999997 `"9999997"', add
label values inclongj inclongj_lbl

label define oincwage_lbl 0099997 `"0099997"'
label define oincwage_lbl 9999997 `"9999997"', add
label values oincwage oincwage_lbl

label define srcearn_lbl 0 `"NIU"'
label define srcearn_lbl 1 `"Wage and salary"', add
label define srcearn_lbl 2 `"Self employment"', add
label define srcearn_lbl 3 `"Farm self employment"', add
label define srcearn_lbl 4 `"Without pay"', add
label values srcearn srcearn_lbl

label define adjginc_lbl 0099997 `"0099997"'
label define adjginc_lbl 9999997 `"9999997"', add
label values adjginc adjginc_lbl

label define fedtax_lbl 099997 `"099997"'
label define fedtax_lbl 999997 `"999997"', add
label values fedtax fedtax_lbl

label define fedtaxac_lbl 099997 `"099997"'
label define fedtaxac_lbl 999997 `"999997"', add
label values fedtaxac fedtaxac_lbl

label define filestat_lbl 0 `"No data"'
label define filestat_lbl 1 `"Joint, both less than 65"', add
label define filestat_lbl 2 `"Joint, one less than 65, one 65+"', add
label define filestat_lbl 3 `"Joint, both 65+"', add
label define filestat_lbl 4 `"Head of household"', add
label define filestat_lbl 5 `"Single"', add
label define filestat_lbl 6 `"Nonfiler"', add
label values filestat filestat_lbl

label define statetax_lbl 099997 `"099997"'
label define statetax_lbl 999997 `"999997"', add
label values statetax statetax_lbl

label define stataxac_lbl 099997 `"099997"'
label define stataxac_lbl 999997 `"999997"', add
label values stataxac stataxac_lbl

label define taxinc_lbl 0099997 `"0099997"'
label define taxinc_lbl 9999997 `"9999997"', add
label values taxinc taxinc_lbl

label define hcovany_lbl 1 `"Not covered"'
label define hcovany_lbl 2 `"Covered"', add
label values hcovany hcovany_lbl

label define hinsemp_lbl 1 `"Not covered"'
label define hinsemp_lbl 2 `"Covered"', add
label values hinsemp hinsemp_lbl

label define hinspur_lbl 1 `"Not covered"'
label define hinspur_lbl 2 `"Covered"', add
label values hinspur hinspur_lbl

label define hcovpub_lbl 1 `"Not covered"'
label define hcovpub_lbl 2 `"Covered"', add
label values hcovpub hcovpub_lbl

label define hinscaid_lbl 1 `"Not covered"'
label define hinscaid_lbl 2 `"Covered"', add
label values hinscaid hinscaid_lbl

label define hinscare_lbl 1 `"Not covered"'
label define hinscare_lbl 2 `"Covered"', add
label values hinscare hinscare_lbl

label define hinsmil_lbl 1 `"Not covered"'
label define hinsmil_lbl 2 `"Covered"', add
label values hinsmil hinsmil_lbl

label define emcontrb_lbl 9997 `"9997"'
label values emcontrb emcontrb_lbl

label define gotwic_lbl 0 `"NIU"'
label define gotwic_lbl 1 `"No"', add
label define gotwic_lbl 2 `"Yes"', add
label values gotwic gotwic_lbl


