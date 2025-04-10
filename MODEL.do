//weight
//Variable= a b c d e f g h i j k l m n o
//GNI= dd
xtset id year
egen miss=rowmiss(a b c d e f g h i j k l m n o) drop if miss
pwcorr a dd,sig
pwcorr b dd,sig
pwcorr c dd,sig
pwcorr d dd,sig
pwcorr e dd,sig
pwcorr f dd,sig
pwcorr g dd,sig
pwcorr h dd,sig
pwcorr i dd,sig
pwcorr j dd,sig
pwcorr k dd,sig
pwcorr l dd,sig
pwcorr m dd,sig
pwcorr n dd,sig
pwcorr o dd,sig
global positiveVar a g i j k l n o
global negativeVar b c d e f h m
//Demographic urbanization=de
global positiveVar a 
global negativeVar b c d e f 
global allVar $positiveVar $negativeVar
foreach v in $positiveVar {
    qui sum `v'
    gen z_`v' = (`v'-r(min))/(r(max)-r(min))
    replace z_`v' = 0.0001 if z_`v' == 0
}
foreach v in $negativeVar {
    qui sum `v'
    gen z_`v' = (r(max)-`v')/(r(max)-r(min))
    replace z_`v' = 0.0001 if z_`v' == 0
}
foreach v in $allVar {
    egen sum_`v' = sum(z_`v')
    gen p_`v' = z_`v' / sum_`v'
}
foreach v in $allVar {
    egen sump_`v' = sum(p_`v'*ln(p_`v'))
    gen e_`v' = -1 / ln(_N) * sump_`v'
}
foreach v in $allVar {
    gen d_`v' = 1 - e_`v'
}
egen sumd = rowtotal(d_*)
foreach v in $allVar {
    gen w_`v' = d_`v' / sumd
}
foreach v in $allVar {
    gen score_`v' = w_`v' * z_`v'
}
egen score = rowtotal(score*)
drop z_* p_* e_* d_* sum*
//Spatial urbanization=sp
global positiveVar g 
global negativeVar h 
global allVar $positiveVar $negativeVar
foreach v in $positiveVar {
    qui sum `v'
    gen z_`v' = (`v'-r(min))/(r(max)-r(min))
    replace z_`v' = 0.0001 if z_`v' == 0
}
foreach v in $negativeVar {
    qui sum `v'
    gen z_`v' = (r(max)-`v')/(r(max)-r(min))
    replace z_`v' = 0.0001 if z_`v' == 0
}
foreach v in $allVar {
    egen sum_`v' = sum(z_`v')
    gen p_`v' = z_`v' / sum_`v'
}
foreach v in $allVar {
    egen sump_`v' = sum(p_`v'*ln(p_`v'))
    gen e_`v' = -1 / ln(_N) * sump_`v'
}
foreach v in $allVar {
    gen d_`v' = 1 - e_`v'
}
egen sumd = rowtotal(d_*)
foreach v in $allVar {
    gen w_`v' = d_`v' / sumd
}
foreach v in $allVar {
    gen score_`v' = w_`v' * z_`v'
}
egen score = rowtotal(score*)
drop z_* p_* e_* d_* sum*
//Economic urbanization=ec
global positiveVar i j
global negativeVar
global allVar $positiveVar $negativeVar
foreach v in $positiveVar {
    qui sum `v'
    gen z_`v' = (`v'-r(min))/(r(max)-r(min))
    replace z_`v' = 0.0001 if z_`v' == 0
}
foreach v in $negativeVar {
    qui sum `v'
    gen z_`v' = (r(max)-`v')/(r(max)-r(min))
    replace z_`v' = 0.0001 if z_`v' == 0
}
foreach v in $allVar {
    egen sum_`v' = sum(z_`v')
    gen p_`v' = z_`v' / sum_`v'
}
foreach v in $allVar {
    egen sump_`v' = sum(p_`v'*ln(p_`v'))
    gen e_`v' = -1 / ln(_N) * sump_`v'
}
foreach v in $allVar {
    gen d_`v' = 1 - e_`v'
}
egen sumd = rowtotal(d_*)
foreach v in $allVar {
    gen w_`v' = d_`v' / sumd
}
foreach v in $allVar {
    gen score_`v' = w_`v' * z_`v'
}
egen score = rowtotal(score*)
drop z_* p_* e_* d_* sum*
//Eco-environment urbanization=ece
global positiveVar k l 
global negativeVar m
global allVar $positiveVar $negativeVar
foreach v in $positiveVar {
    qui sum `v'
    gen z_`v' = (`v'-r(min))/(r(max)-r(min))
    replace z_`v' = 0.0001 if z_`v' == 0
}
foreach v in $negativeVar {
    qui sum `v'
    gen z_`v' = (r(max)-`v')/(r(max)-r(min))
    replace z_`v' = 0.0001 if z_`v' == 0
}
foreach v in $allVar {
    egen sum_`v' = sum(z_`v')
    gen p_`v' = z_`v' / sum_`v'
}
foreach v in $allVar {
    egen sump_`v' = sum(p_`v'*ln(p_`v'))
    gen e_`v' = -1 / ln(_N) * sump_`v'
}
foreach v in $allVar {
    gen d_`v' = 1 - e_`v'
}
egen sumd = rowtotal(d_*)
foreach v in $allVar {
    gen w_`v' = d_`v' / sumd
}
foreach v in $allVar {
    gen score_`v' = w_`v' * z_`v'
}
egen score = rowtotal(score*)
drop z_* p_* e_* d_* sum*
//Social urbanization=so
global positiveVar n o
global negativeVar
global allVar $positiveVar $negativeVar
foreach v in $positiveVar {
    qui sum `v'
    gen z_`v' = (`v'-r(min))/(r(max)-r(min))
    replace z_`v' = 0.0001 if z_`v' == 0
}
foreach v in $negativeVar {
    qui sum `v'
    gen z_`v' = (r(max)-`v')/(r(max)-r(min))
    replace z_`v' = 0.0001 if z_`v' == 0
}
foreach v in $allVar {
    egen sum_`v' = sum(z_`v')
    gen p_`v' = z_`v' / sum_`v'
}
foreach v in $allVar {
    egen sump_`v' = sum(p_`v'*ln(p_`v'))
    gen e_`v' = -1 / ln(_N) * sump_`v'
}
foreach v in $allVar {
    gen d_`v' = 1 - e_`v'
}
egen sumd = rowtotal(d_*)
foreach v in $allVar {
    gen w_`v' = d_`v' / sumd
}
foreach v in $allVar {
    gen score_`v' = w_`v' * z_`v'
}
egen score = rowtotal(score*)
drop z_* p_* e_* d_* sum*
//model
//ASDR Schizophrenia=ys
xtset id year
xtreg ys de sp ec ece so, fe
xtreg ys de sp ec ece so, fe i(year)
xtset id year
xtreg ys de sp ec ece so i.year, fe
testparm i.year
xtset id year
xtreg ys de sp ec ece so i.year, fe
//ASPR Schizophrenia=ps
xtset id year
xtreg ps de sp ec ece so, fe
xtreg ps de sp ec ece so, fe i(year)
xtset id year
xtreg ps de sp ec ece so i.year, fe
testparm i.year
xtset id year
xtreg ps de sp ec ece so i.year, fe
//ASIR Schizophrenia=is
xtset id year
xtreg is de sp ec ece so, fe
xtreg is de sp ec ece so, fe i(year)
xtset id year
xtreg is de sp ec ece so i.year, fe
testparm i.year
xtset id year
xtreg is de sp ec ece so, fe
estimates store fixed
xtreg is de sp ec ece so, re
estimates store random
hausman fixed random
//ASDR Major depressive disorder=yd
xtset id year
xtreg yd de sp ec ece so, fe
xtreg yd de sp ec ece so, fe i(year)
xtset id year
xtreg yd de sp ec ece so i.year, fe
testparm i.year
xtset id year
xtreg yd de sp ec ece so i.year, fe
//ASPR Major depressive disorderpd
xtset id year
xtreg pd de sp ec ece so, fe
xtreg pd de sp ec ece so, fe i(year)
xtset id year
xtreg pd de sp ec ece so i.year, fe
testparm i.year
xtset id year
xtreg pd de sp ec ece so i.year, fe
//ASIR Major depressive disorder=ind
xtset id year
xtreg ind de sp ec ece so, fe
xtreg ind de sp ec ece so, fe i(year)
xtset id year
xtreg ind de sp ec ece so i.year, fe
testparm i.year
xtset id year
xtreg ind de sp ec ece so i.year, fe
//ASDR Bipolar disorder=yb
xtset id year
xtreg yb de sp ec ece so, fe
xtreg yb de sp ec ece so, fe i(year)
xtset id year
xtreg yb de sp ec ece so i.year, fe
testparm i.year
xtset id year
xtreg yb de sp ec ece so i.year, fe
//ASPR Bipolar disorder=pb
xtset id year
xtreg pb de sp ec ece so, fe
xtreg pb de sp ec ece so, fe i(year)
xtset id year
xtreg pb de sp ec ece so i.year, fe
testparm i.year
xtset id year
xtreg pb de sp ec ece so i.year, fe
//ASIR Bipolar disorder=ib
xtset id year
xtreg ib de sp ec ece so, fe
xtreg ib de sp ec ece so, fe i(year)
xtset id year
xtreg ib de sp ec ece so i.year, fe
testparm i.year
xtset id year
xtreg ib de sp ec ece so i.year, fe
