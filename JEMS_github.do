version 16.1
clear
set more off
set memory 700m

********************************************************************************
***** load data
********************************************************************************
use "[file_location]Inschool_data.dta", clear

********************************************************************************
***** creating variables
********************************************************************************
***** school id/grade
destring sschlcde, gen(schoolidw1)

clonevar gradew1= s3 
recode gradew1 99=. 13=. 6=.
ta gradew1, g(gradew1)

***** sociodemographic
clonevar age=s1 
recode age 99=.

clonevar male= s2 
recode male 2=0 9=.
gen female=1-male

clonevar whiteN= s6a 
clonevar blackN= s6b 
clonevar asianN= s6c 
clonevar americanN= s6d 
clonevar otherN= s6e
clonevar hispanicN= s4 
recode hispanicN (8/9=.) 

label var whiteN "White"
label var blackN "Black"
label var asianN "Asian"
label var americanN "American Indian"
label var otherN "Other race"
label var hispanicN "Hispanic"

***** parental characteristics
g mombornus= s13 
recode mombornus 7=. 8=. 9=.
g dadbornus= s19  
recode dadbornus 7=. 8=. 9=.
gen parentnonnative=1-min(mombornus, dadbornus)

clonevar momedu_bal= s12
clonevar dadedu_bal= s18

recode momedu_bal 99=. 97=. 11=. 10=0 9=10 8=16 7=16 6=14 5=13 4=12 3=12 2=10 1=8
recode dadedu_bal 99=. 97=. 11=. 10=0 9=10 8=16 7=16 6=14 5=13 4=12 3=12 2=10 1=8

clonevar momcollege=s12
recode momcollege (1/6=0) (7/8=1) (9/10=0) (11/max=.)
clonevar dadcollege=s18
recode dadcollege (1/6=0) (7/8=1) (9/10=0) (11/max=.)
egen patedu_bal=rowmax(momedu_bal dadedu_bal)

gen patcol=1 if patedu_bal>=16 & patedu_bal<=20
replace patcol=0 if patedu_bal<16
gen momcol=1 if momedu_bal>=16 & momedu_bal<=20
replace momcol=0 if momedu_bal<16
gen dadcol=1 if dadedu_bal>=16 & dadedu_bal<=20
replace dadcol=0 if dadedu_bal<16

clonevar livewithmom= s11 
recode livewithmom 9=.

clonevar livewithdad= s17 
recode livewithdad 9=.

gen liveboth=(livewithdad==1 & livewithmom==1)
replace liveboth=. if livewithdad==. & livewithmom==.
replace liveboth=0 if livewithdad==. & livewithmom==0
replace liveboth=1 if livewithdad==. & livewithmom==1
replace liveboth=0 if livewithdad==0 & livewithmom==.
replace liveboth=1 if livewithdad==1 & livewithmom==.

clonevar hhsize=s27
recode hhsize (7/max=.)

***** foreignborn and generational status
g nativeborn= s8 
recode nativeborn 9=.
g foreignborn=1-nativeborn

g immig=.
replace immig=0 if nativeborn==1 & parentnonnative==0 /*3rd (non-immigrant)*/
replace immig=0 if nativeborn==0 & parentnonnative==0 /*3rd (non-immigrant)*/
replace immig=1 if nativeborn==1 & parentnonnative==1 /*2nd/2.5*/
replace immig=2 if nativeborn==0 & parentnonnative==1 /*1st/1.5*/

g immigi=immig
replace immigi=3 if immigi==.  & foreignborn!=. 

ta immig, g(immig)
ta immigi, g(immigi)

***** gpa
recode s10a s10b s10c s10d (5/max=.)
g eng=s10a
g math=s10b
g socstu=s10c
g sci=s10d
recode eng math socstu sci (1=4) (2=3) (3=2) (4=0.5)
egen gpa=rowmean(eng math socstu sci)

***** academic effort
recode s48 s47 s59g s46c s46a (9/max=.)
foreach var of varlist s48 s47 s59g s46c s46a {
revrs `var'
}
factor revs48 revs47 revs59g revs46c revs46a, pf
predict academic4

*** grademate-level measure
foreach var of varlist patedu_bal momedu_bal dadedu_bal patcol momcol dadcol {
egen total = total(`var'), by(schoolidw1 gradew1)
egen n = count(`var'), by(schoolidw1 gradew1)
gen totalMINUSi = total - cond(missing(`var'), 0, `var')
gen mx_`var' = totalMINUSi / (n - !missing(`var'))
drop total totalMINUSi total n
}

*** school-level measure
foreach var of varlist patedu_bal momedu_bal dadedu_bal patcol momcol dadcol {
egen total = total(`var'), by(schoolidw1 )
egen n = count(`var'), by(schoolidw1 )
gen totalMINUSi = total - cond(missing(`var'), 0, `var')
gen sx_`var' = totalMINUSi / (n - !missing(`var'))
drop total totalMINUSi total n
}

********************************************************************************
***** imputation
********************************************************************************
clonevar hispanicN2= s4 
recode hispanicN2 (8/9=0) (. =0)
g miss_hispanicN2=0
replace miss_hispanicN2=1 if s4>=8

foreach var of varlist female momcollege dadcollege liveboth age hhsize patedu_bal {
g miss_`var'=1
replace miss_`var'=0 if `var'!=.
g `var'i=`var'
replace `var'i=0 if `var'==. 
}

********************************************************************************
******** sample restriction / sample calculation
********************************************************************************
bysort schoolidw1: egen counts=count(immigi) 
bysort schoolidw1 gradew1: egen countg=count(immigi) 

bysort schoolidw1: egen maxgr=max(gradew1)
bysort schoolidw1: egen mingr=min(gradew1)
g numgr=maxgr-mingr

g avgstudent=counts/numgr

drop if schoolidw1==.
drop if gradew1==. 
drop if !(countg>=5 & avgstudent>=10) 

********************************************************************************
******* Table 1. descriptive stat
********************************************************************************
egen all=rowmax(nativeborn foreignborn)

g comp12=immig1 if immig!=2
g comp13=immig1 if immig!=1
g comp23=immig2 if immig!=0

estimates clear
foreach env in patedu_bal   {
foreach cat in  immigi  {
foreach out in  academic4   {
xtreg `out' /*`env' */ c.mx_`env' i.`cat' gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize ///
sx_`env' ///
, i(schoolidw1) fe robust cluster(schoolidw1)
g descsamp=1 if e(sample)
}
}
}

global descr academic4 gpa  ///
mx_patedu_bal ///
age female whiteN blackN asianN americanN otherN hispanicN2 ///
momcollege dadcollege liveboth hhsize

estimates clear
foreach var of varlist all   {
estpost summarize $descr if `var'==1 & descsamp==1
estimates store `var'
esttab using "$dir\Table1_descr_`var'.rtf", ///
cells("mean(fmt(3) label(Mean)) sd(fmt(3) label(SD)) min(fmt(3) label(Min)) max(fmt(3) label(Max))") ///
label nomtitle nonumber replace compress 
}
foreach var of varlist  nativeborn foreignborn immig1 immig2 immig3  {
estpost summarize $descr if `var'==1 & descsamp==1
estimates store `var'
esttab using "$dir\Table1_descr_`var'.rtf", ///
cells("mean(fmt(3) label(Mean)) sd(fmt(3) label(SD)) min(fmt(3) label(Min)) max(fmt(3) label(Max))") ///
label nomtitle nonumber replace compress 
}

esttab nativeborn foreignborn immig1 immig2 immig3 using "$dir\Table1_descr_full.rtf", ///
cells("mean(fmt(3) label(Mean))") ///
mtitle("US-born students" "Foreign-born students" "3rd+ Gen" "2/2.5 Gen" "1/1.5 Gen") ///
label  nonumber replace compress 

estpost ttest $descr if descsamp==1, by(nativeborn)
esttab `var' using "$dir\Table1_ttest_native.rtf", cells("mu_2(fmt(3)) mu_1(fmt(3)) t(fmt(3)) p(fmt(3))")  nonumber replace

estpost ttest $descr  if descsamp==1 , by(comp12)
esttab `var' using "$dir\Table1_ttest_12.rtf", cells("mu_2(fmt(3)) mu_1(fmt(3)) t(fmt(3)) p(fmt(3))")  nonumber replace

estpost ttest $descr  if descsamp==1, by(comp13)
esttab `var' using "$dir\Table1_ttest_13.rtf", cells("mu_2(fmt(3)) mu_1(fmt(3)) t(fmt(3)) p(fmt(3))")  nonumber replace

estpost ttest $descr  if descsamp==1, by(comp23)
esttab `var' using "$dir\Table1_ttest_23.rtf", cells("mu_2(fmt(3)) mu_1(fmt(3)) t(fmt(3)) p(fmt(3))")  nonumber replace

*** descriptives on the items for academic effort
estimates clear
estpost summarize revs48 revs47 revs59g  revs46c revs46a if descsamp==1
esttab . using "$dir\descr_academic_effort.rtf", ///
cells("mean(fmt(3) label(Mean)) sd(fmt(3) label(SD)) min(fmt(3) label(Min)) max(fmt(3) label(Max))") ///
label nomtitle nonumber replace compress 

*** missing patterns
misschk academic4 gpa immigi age female whiteN blackN asianN americanN otherN hispanicN momcollege dadcollege liveboth hhsize 

********************************************************************************
***** computing within-school variations 
********************************************************************************
reg mx_patedu_bal i.schoolidw1  if all==1 & descsamp==1
predict pred_ab if e(sample), residuals

reg mx_patedu_bal i.schoolidw1  if immigi1==1 & descsamp==1
predict pred_i1 if e(sample), residuals

reg mx_patedu_bal i.schoolidw1  if immigi2==1 & descsamp==1
predict pred_i2 if e(sample), residuals

reg mx_patedu_bal i.schoolidw1  if immigi3==1 & descsamp==1
predict pred_i3 if e(sample), residuals

g mx_patedu_bal_1=mx_patedu_bal if immigi1==1 & descsamp==1
g mx_patedu_bal_2=mx_patedu_bal if immigi2==1 & descsamp==1
g mx_patedu_bal_3=mx_patedu_bal if immigi3==1 & descsamp==1

estpost summarize mx_patedu_bal mx_patedu_bal_* pred_* if all== 1 & descsamp==1
esttab . using "$dir\within_variation_peer_edu.rtf", ///
cells("count(fmt(0) label(Obs)) mean(fmt(3) label(Mean)) sd(fmt(3) label(SD)) min(fmt(3) label(Min)) max(fmt(3) label(Max))") label nomtitle nonumber replace compress ///
refcat(mx_patedu_bal "{Panel A. Total variation}" pred_ab "{Panel B. Within-school variation}" ///
, nolabel) 

********************************************************************************
***** Figure 1. kdensity
********************************************************************************
twoway kdensity mx_patedu_bal if immigi == 0 || kdensity mx_patedu_bal if immigi == 1 || kdensity mx_patedu_bal if immigi == 2, legend(label(1 "3rd+ Gen") label(2 "2/2.5 Gen") label(3 "1/1.5 Gen") rows(1)) scheme(s2mono) ///
ytitle("Density" " ") xtitle(" " "Peer Parental Education")
graph export $dir\Figure1_kdensity_2.eps, as(eps) replace

********************************************************************************
***** Table 2. balancing tests
********************************************************************************
foreach var of varlist patedu_bal momedu_bal dadedu_bal patcol momcol dadcol {
foreach var2 of varlist immig2 immig3 immigi2 immigi3 immigi4 {
g int`var'`var2'=mx_`var'*`var2'
}
}
foreach var of varlist patedu_bal momedu_bal dadedu_bal patcol momcol dadcol {
foreach var2 of varlist immig2 immig3 immigi2 immigi3 immigi4 {
g cot`var'`var2'=sx_`var'*`var2'
}
}
global variables age female whiteN blackN asianN americanN otherN hispanicN ///
momcollege dadcollege liveboth hhsize

estimates clear
foreach var in patedu_bal  {
foreach var2 of varlist immigi2 immigi3  {
foreach out of varlist $variables {
eststo: areg `out' int`var'`var2' mx_`var' cot`var'`var2' `var2' gradew12 gradew13 gradew14 gradew15 gradew16 ///
, robust cluster(schoolidw1) absorb(schoolidw1)
}
esttab, se nostar
matrix C = r(coefs)
eststo clear
local rnames : rownames C
local models : coleq C
local models : list uniq models
local i 0
foreach name of local rnames {
    local ++i
    local j 0
    capture matrix drop b
    capture matrix drop se
    foreach model of local models {
        local ++j
        matrix tmp = C[`i', 2*`j'-1]
        if tmp[1,1]<. {
            matrix colnames tmp = `model'
            matrix b = nullmat(b), tmp
            matrix tmp[1,1] = C[`i', 2*`j']
            matrix se = nullmat(se), tmp
        }
    }
    ereturn post b
    quietly estadd matrix se
    eststo `name'
}
esttab, p mtitle noobs
esttab int* using "$dir\Table2_balance_int_`var'_`var2'.rtf",  ///
star(* 0.05 ** 0.01 *** 0.001)  b(3) pa se(3) ///
label replace nogaps compress dep noobs ///
addnote("{\i Note.} * {\i p} < 0.05, ** {\i p} < 0.01, *** {\i p} < 0.001" ) nonote
estimates clear
}
}
estimates clear
foreach var in patedu_bal  {
foreach out of varlist $variables {
eststo: areg `out' mx_`var' sx_`var' immigi2 immigi3 immigi4 gradew12 gradew13 gradew14 gradew15 gradew16 ///
, robust cluster(schoolidw1) absorb(schoolidw1)
}
esttab, se nostar
matrix C = r(coefs)
eststo clear
local rnames : rownames C
local models : coleq C
local models : list uniq models
local i 0
foreach name of local rnames {
    local ++i
    local j 0
    capture matrix drop b
    capture matrix drop se
    foreach model of local models {
        local ++j
        matrix tmp = C[`i', 2*`j'-1]
        if tmp[1,1]<. {
            matrix colnames tmp = `model'
            matrix b = nullmat(b), tmp
            matrix tmp[1,1] = C[`i', 2*`j']
            matrix se = nullmat(se), tmp
        }
    }
    ereturn post b
    quietly estadd matrix se
    eststo `name'
}
esttab, p mtitle noobs
esttab mx* using "$dir\Table2_balance_main_`var'.rtf",  ///
star(* 0.05 ** 0.01 *** 0.001)  b(3) pa se(3) ///
label replace nogaps compress dep noobs ///
addnote("{\i Note.} * {\i p} < 0.05, ** {\i p} < 0.01, *** {\i p} < 0.001" ) nonote
}

********************************************************************************
******* Table 3 / Table 4. main and interaction models
********************************************************************************
estimates clear
foreach env in patedu_bal   {
foreach cat in   immigi  {
foreach out in  academic4 gpa  {
xtreg `out' c.mx_`env' i.`cat' gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize ///
sx_`env' ///
, i(schoolidw1) fe robust cluster(schoolidw1)

eststo: reg `out' c.mx_`env' gradew12 gradew13 gradew14 gradew15 gradew16  ///
if e(sample), robust cluster(schoolidw1)

eststo: xtreg `out' c.mx_`env' gradew12 gradew13 gradew14 gradew15 gradew16  sx_`env' ///
if e(sample), i(schoolidw1) fe robust cluster(schoolidw1)

eststo: xtreg `out' c.mx_`env' i.`cat' gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize  ///
sx_`env' ///
, i(schoolidw1) fe robust cluster(schoolidw1)

eststo: xtreg `out' /*`env' */ c.mx_`env'##i.`cat' gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei  ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize  ///
sx_`env' ///
, i(schoolidw1) fe robust cluster(schoolidw1)
}
}
}
esttab using "$dir\Table3_Table4_main_interaction.rtf",  ///
star(+ 0.10 * 0.05 ** 0.01 *** 0.001)  b(3) pa se(3) ///
label stats(N r2, fmt(0 3) labels("N" "R-squared")) ///
replace nogaps compress dep nobase ///
o(mx_* *#*) ///
addnote("{\i Note.} + {\i p} < 0.10, * {\i p} < 0.05, ** {\i p} < 0.01, *** {\i p} < 0.001" ) nonote 

********************************************************************************
****************** Figure 2. two-way interaction figures
********************************************************************************
estimates clear
foreach env in patedu_bal  {
sum mx_`env'
global Mmx_`env'=r(mean)
global Hmx_`env'=r(mean)+r(sd)
global Lmx_`env'=r(mean)-r(sd)
global Hmx2_`env'=r(mean)+2*r(sd)
global Lmx2_`env'=r(mean)-2*r(sd)
}

estimates clear
foreach env in patedu_bal  {
foreach out in academic4  {
eststo: xtreg `out' c.mx_`env'##i.immigi gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei   ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize  ///
sx_`env' ///
, i(schoolidw1) fe robust cluster(schoolidw1)
margins , at(immigi=(0 1 2) mx_`env'=(${Lmx2_`env'} ${Lmx_`env'} ${Mmx_`env'} ${Mmx_`env'}  ${Hmx_`env'} ${Hmx2_`env'})) vsquish 
marginsplot, recast(line) xdim(mx_`env') noci  ///
ytitle(" ") xtitle("Peer Parental Education") ///
scheme(s1mono) title("Academic Effort") xlab(${Lmx2_`env'} "-2SD" ${Lmx_`env'} "-1SD" ${Mmx_`env'} "Mean" ${Hmx_`env'} "+1SD"  ${Hmx2_`env'} "+2SD", angle(45))  ///
legend(order(1 "3rd+ Gen" 2 "2/2.5 Gen" 3 "1/1.5 Gen") rows(1) size(small) ) ///
saving(Figure2_`out'_`env'.eps, replace) name(Figure2_`out'_`env', replace) 
graph export $dir\Figure2_`out'.png, as(png) replace
graph export $dir\Figure2_`out'.eps, as(eps) replace
graph export $dir\Figure2_`out'.pdf, as(pdf) replace
}
}

estimates clear
foreach env in patedu_bal  {
foreach out in gpa  {
eststo: xtreg `out' c.mx_`env'##i.immigi gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei  ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize ///
sx_`env' ///
, i(schoolidw1) fe robust cluster(schoolidw1)
margins , at(immigi=(0 1 2) mx_`env'=(${Lmx2_`env'} ${Lmx_`env'} ${Mmx_`env'} ${Mmx_`env'}  ${Hmx_`env'} ${Hmx2_`env'})) vsquish 
marginsplot, recast(line) xdim(mx_`env') noci  ///
ytitle(" ") xtitle("Peer Parental Education") ///
scheme(s1mono) title("GPA") xlab(${Lmx2_`env'} "-2SD" ${Lmx_`env'} "-1SD" ${Mmx_`env'} "Mean" ${Hmx_`env'} "+1SD"  ${Hmx2_`env'} "+2SD", angle(45))  ///
legend(order(1 "3rd+ Gen" 2 "2/2.5 Gen" 3 "1/1.5 Gen") rows(1) size(small) ) ///
saving(Figure2_`out'_`env'.eps, replace) name(Figure2_`out'_`env', replace) 
graph export $dir\Figure2_`out'.png, as(png) replace
graph export $dir\Figure2_`out'.eps, as(eps) replace
graph export $dir\Figure2_`out'.pdf, as(pdf) replace
}
}

grc1leg2  Figure2_academic4_patedu_bal Figure2_gpa_patedu_bal, scheme(s1mono) 
graph export $dir\Figure2_combine.png, as(png) replace
graph export $dir\Figure2_combine.eps, as(eps) replace
graph export $dir\Figure2_combine.pdf, as(pdf) replace

********************************************************************************
***** Table 5. three-way interaction model
********************************************************************************
clonevar hispanic= s4 
recode hispanic 8=. 9=. 
clonevar white= s6a 
clonevar black= s6b 
clonevar asian= s6c 
replace white=0 if black==1
replace white=0 if hispanic==1
replace white=0 if asian==1
replace hispanic=0 if black==1
replace asian=0 if hispanic==1
replace asian=0 if black==1
gen otherrace=1-white-black-asian-hispanic

g raceA=.
replace raceA=0 if white==1
replace raceA=1 if black==1
replace raceA=2 if hispanic==1
replace raceA=3 if asian==1
replace raceA=4 if otherrace==1
recode raceA (.=4)

ta raceA, g(raceA)
label var raceA1 "White"
label var raceA2 "Black"
label var raceA3 "Hispanic"
label var raceA4 "Asian"
label var raceA5 "Other race"

g im_raceA=. 
replace im_raceA=raceA if immigRi==1

ta im_raceA, g(im_raceA)
label var im_raceA1 "White"
label var im_raceA2 "Black"
label var im_raceA3 "Hispanic"
label var im_raceA4 "Asian"
label var im_raceA5 "Other race"

foreach var of varlist im_raceA1 im_raceA2 im_raceA3 im_raceA4 im_raceA5  {
egen s_`var'=mean(`var'), by(schoolidw1)
}

g s_im_share=.
foreach var of varlist raceA1 raceA2 raceA3 raceA4 raceA5   {
replace s_im_share=s_im_`var' if `var'==1
}
replace s_im_share=0 if s_im_share==.

foreach var in s_im_share mx_patedu_bal {
sum `var' 
global Mmx_`var'=r(mean)
global Hmx_`var'=r(mean)+r(sd)
global Lmx_`var'=r(mean)-r(sd)
global Hmx2_`var'=r(mean)+2*r(sd)
global Lmx2_`var'=r(mean)-2*r(sd)
global Hmx3_`var'=r(mean)+3*r(sd)
global Lmx3_`var'=r(mean)-3*r(sd)
}

estimates clear
foreach env in patedu_bal   {
foreach cat in   immigi  {
foreach out in  academic4 gpa  {
eststo: xtreg `out' c.mx_`env'##i.`cat' gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei   ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize  ///
sx_`env' ///
, i(schoolidw1) fe robust cluster(schoolidw1)

eststo: xtreg `out' c.mx_`env'##i.`cat'##c.s_im_share gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei   ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize ///
sx_`env' ///
, i(schoolidw1) fe robust cluster(schoolidw1)
}
}
}
esttab, p compress
esttab using "$dir\Table5_three_way_interaction.rtf",  ///
star(+ 0.10 * 0.05 ** 0.01 *** 0.001)  b(3) pa se(3) ///
label stats(N r2, fmt(0 3) labels("N" "R-squared")) ///
replace nogaps compress dep nobase ///
o(mx_* *#* ) ///
addnote("{\i Note.} + {\i p} < 0.10, * {\i p} < 0.05, ** {\i p} < 0.01, *** {\i p} < 0.001" ) nonote 

********************************************************************************
***** Figure 3.three-way interaction figures
********************************************************************************
estimates clear
foreach env in patedu_bal   {
foreach cat in   immigi  {
foreach out in  academic4   {
eststo: xtreg `out' c.mx_`env'##i.`cat'##c.s_im_share gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei  ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize ///
sx_`env' ///
, i(schoolidw1) fe robust cluster(schoolidw1)
}
}
}
esttab, p compress
margins , at(immigi=(0 1 2) mx_patedu_bal=( $Lmx2_mx_patedu_bal $Lmx_mx_patedu_bal $Mmx_mx_patedu_bal $Hmx2_mx_patedu_bal $Hmx_mx_patedu_bal  ) s_im_share=($Lmx_s_im_share $Hmx_s_im_share) ) vsquish 

marginsplot, recast(line) x(mx_patedu_bal) noci ///
byopt(title ("Academic Effort")) ///
ytitle(" ") xtitle("Peer Parental Education") scheme(s1mono) ///
bydim(s_im_share, elabel(1 "More diverse (-1SD)" 2 "More similar (+1SD)" )) ///
legend(order(1 "3rd+ Gen" 2 "2/2.5 Gen" 3 "1/1.5 Gen")  rows(1) size(small)) ///
 xlab($Lmx2_mx_patedu_bal "-2SD" $Lmx_mx_patedu_bal "-1SD" $Mmx_mx_patedu_bal "Mean"  $Hmx_mx_patedu_bal "+1SD"  $Hmx2_mx_patedu_bal "+2SD" , angle(45))  
graph export $dir\R_figure_academic.png, as(png) replace
graph export $dir\R_figure_academic.eps, as(eps) replace
graph export $dir\R_figure_academic.pdf, as(pdf) replace
 
estimates clear
foreach env in patedu_bal   {
foreach cat in  immigi  {
foreach out in  gpa  {
eststo: xtreg `out'  c.mx_`env'##i.`cat'##c.s_im_share gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei  ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize ///
sx_`env' ///
, i(schoolidw1) fe robust cluster(schoolidw1)
}
}
}
esttab, p compress
margins , at(immigi=(0 1 2) mx_patedu_bal=( $Lmx2_mx_patedu_bal $Lmx_mx_patedu_bal $Mmx_mx_patedu_bal $Hmx_mx_patedu_bal $Hmx2_mx_patedu_bal  ) s_im_share=($Lmx_s_im_share $Hmx_s_im_share) ) vsquish 

marginsplot, recast(line) x(mx_patedu_bal) noci ///
byopt(title ("GPA")) ///
ytitle(" ") xtitle("Peer Parental Education") scheme(s1mono) ///
bydim(s_im_share, elabel(1 "More diverse (-1SD)" 2 "More similar (+1SD)" )) ///
legend(order(1 "3rd+ Gen" 2 "2/2.5 Gen" 3 "1/1.5 Gen") rows(1) size(small)) ///
 xlab($Lmx2_mx_patedu_bal "-2SD" $Lmx_mx_patedu_bal "-1SD" $Mmx_mx_patedu_bal "Mean"  $Hmx_mx_patedu_bal "+1SD"  $Hmx2_mx_patedu_bal "+2SD" , angle(45))  
graph export $dir\R_figure_gpa.png, as(png) replace
graph export $dir\R_figure_gpa.eps, as(eps) replace
graph export $dir\R_figure_gpa.pdf, as(pdf) replace
 
********************************************************************************
***** Robustness: different operationalization of peer parental education 
********************************************************************************
estimates clear
foreach out in academic4 gpa {
foreach env in patedu_bal momedu_bal dadedu_bal patcol momcol dadcol   {
foreach cat in  immigi   {
eststo: xtreg `out' c.mx_`env' i.`cat' gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei   ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize  ///
sx_`env' ///
, i(schoolidw1) fe cluster(schoolidw1)
}
}
esttab using "$dir\R_operationalization_main_`out'.rtf",  ///
star(+ 0.10 * 0.05 ** 0.01 *** 0.001)  b(3) pa se(3) ///
label stats(N r2, fmt(0 3) labels("N" "R-squared")) ///
replace nogaps compress dep nobase ///
o(mx_* ) ///
addnote("{\i Note.} + {\i p} < 0.10, * {\i p} < 0.05, ** {\i p} < 0.01, *** {\i p} < 0.001" ) nonote 
estimates clear
}

estimates clear
foreach out in academic4 gpa  {
foreach env in patedu_bal momedu_bal dadedu_bal patcol momcol dadcol   {
foreach cat in  immigi   {
eststo: xtreg `out' c.mx_`env'##i.`cat' gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei  ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize  ///
sx_`env' ///
, i(schoolidw1) fe robust cluster(schoolidw1)
}
}
esttab using "$dir\R_operationalization_int_`out'.rtf",  ///
star(+ 0.10 * 0.05 ** 0.01 *** 0.001)  b(3) pa se(3) ///
label stats(N r2, fmt(0 3) labels("N" "R-squared")) ///
replace nogaps compress dep nobase ///
o(mx_* *#* ) ///
addnote("{\i Note.} + {\i p} < 0.10, * {\i p} < 0.05, ** {\i p} < 0.01, *** {\i p} < 0.001" ) nonote 
estimates clear
}

********************************************************************************
***** Robustness: combining 1/1.5 and 2/2.5 or foreignborn
********************************************************************************
g immigR = immig
recode immigR (2=1)
g immigRi=immigR
replace immigRi=2 if immigR==.  & foreignborn!=. 
ta immigRi, g(immigRi)

estimates clear
foreach cat in  immigRi foreignborn  {
foreach env in patedu_bal   {
foreach out in  academic4 gpa  {
xtreg `out' c.mx_`env' i.`cat' gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei   ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize  ///
sx_`env' ///
, i(schoolidw1) fe robust cluster(schoolidw1)

eststo: reg `out' c.mx_`env' gradew12 gradew13 gradew14 gradew15 gradew16  sx_`env' ///
if e(sample), robust cluster(schoolidw1)

eststo: xtreg `out' c.mx_`env' gradew12 gradew13 gradew14 gradew15 gradew16  sx_`env' ///
if e(sample), i(schoolidw1) fe robust cluster(schoolidw1)

eststo: xtreg `out' c.mx_`env' i.`cat' gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei   ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize  ///
sx_`env' ///
, i(schoolidw1) fe robust cluster(schoolidw1)

eststo: xtreg `out' c.mx_`env'##i.`cat' gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei  ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize ///
sx_`env' ///
, i(schoolidw1) fe robust cluster(schoolidw1)
}
}
esttab using "$dir\R_`cat'.rtf",  ///
star(+ 0.10 * 0.05 ** 0.01 *** 0.001)  b(3) pa se(3) ///
label stats(N r2, fmt(0 3) labels("N" "R-squared")) ///
replace nogaps compress dep nobase ///
o(mx_* *#*) ///
addnote("{\i Note.} + {\i p} < 0.10, * {\i p} < 0.05, ** {\i p} < 0.01, *** {\i p} < 0.001" ) nonote 
estimates clear
}

********************************************************************************
***** Robustness: three way interation (race/ethnicity)
********************************************************************************
estimates clear
foreach race in whiteN blackN asianN americanN otherN hispanicN2 {
foreach env in patedu_bal   {
foreach cat in   immigi  {
foreach out in  academic4 gpa   {
eststo: xtreg `out' c.mx_`env' i.`cat' gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei  ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize  ///
sx_`env' ///
if `race'==1, i(schoolidw1) fe robust cluster(schoolidw1)

eststo: xtreg `out'  c.mx_`env'##i.`cat' gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei    ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize  ///
sx_`env' ///
if `race'==1, i(schoolidw1) fe robust cluster(schoolidw1)

}
}
}
esttab using "$dir\R_Table3_Table4_main_interaction_`race'.rtf",  ///
star(+ 0.10 * 0.05 ** 0.01 *** 0.001)  b(3) pa se(3) ///
label stats(N r2, fmt(0 3) labels("N" "R-squared")) ///
replace nogaps compress dep nobase ///
o(mx_* *#*) ///
addnote("{\i Note.} + {\i p} < 0.10, * {\i p} < 0.05, ** {\i p} < 0.01, *** {\i p} < 0.001" ) nonote 
estimates clear
}

g White=whiteN
g Black=blackN
g Asian=asianN
g American_Indian=americanN
g Other=otherN
g Hispanic=hispanicN2

estimates clear
foreach race in White Black Asian American_Indian Other Hispanic {
foreach env in patedu_bal   {
foreach cat in   immigi  {
foreach out in  academic4   {
eststo: xtreg `out' c.mx_`env'##i.`cat' gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei   ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize ///
sx_`env' ///
if `race'==1, i(schoolidw1) fe robust cluster(schoolidw1)
margins , at(immigi=(0 1 2) mx_`env'=(${Lmx2_`env'} ${Lmx_`env'} ${Mmx_`env'} ${Mmx_`env'}  ${Hmx_`env'} ${Hmx2_`env'})) vsquish saving(`out'_`race', replace)

marginsplot, recast(line) xdim(mx_`env') noci  ///
ytitle(" ") xtitle(" ") ///
scheme(s1mono) title(`race') xlab(${Lmx2_`env'} "-2SD" ${Lmx_`env'} "-1SD" ${Mmx_`env'} "Mean" ${Hmx_`env'} "+1SD"  ${Hmx2_`env'} "+2SD", angle(45))  ///
legend(order(1 "3rd+ Gen" 2 "2/2.5 Gen" 3 "1/1.5 Gen") rows(1) size(small) ) ///
saving(`out'_`race'.eps, replace) name(`out'_`race', replace) 
}
}
}
}
grc1leg  academic4_White academic4_Black academic4_Asian academic4_American_Indian academic4_Other academic4_Hispanic, title("Academic Effort", color(black)) b2title("Peer Prental Education",size(small)) /*l1title("Price")*/ ring(2) legendfrom(academic4_White) graphregion(color(white))
graph export $dir\fig_combine_academic_race.png, as(png) replace
graph export $dir\fig_combine_academic_race.eps, as(eps) replace
graph export $dir\fig_combine_academic_race.pdf, as(pdf) replace

estimates clear
foreach race in White Black Asian American_Indian Other Hispanic {
foreach env in patedu_bal   {
foreach cat in   immigi  {
foreach out in  gpa    {

eststo: xtreg `out' c.mx_`env'##i.`cat' gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN2 miss_hispanicN2  ///
femalei agei momcollegei dadcollegei livebothi hhsizei  ///
miss_female miss_age miss_momcollege miss_dadcollege miss_liveboth miss_hhsize  ///
sx_`env' ///
if `race'==1, i(schoolidw1) fe robust cluster(schoolidw1)
margins , at(immigi=(0 1 2) mx_`env'=(${Lmx2_`env'} ${Lmx_`env'} ${Mmx_`env'} ${Mmx_`env'}  ${Hmx_`env'} ${Hmx2_`env'})) vsquish saving(`out'_`race', replace)

marginsplot, recast(line) xdim(mx_`env') noci  ///
ytitle(" ") xtitle(" ") ///
scheme(s1mono) title(`race') xlab(${Lmx2_`env'} "-2SD" ${Lmx_`env'} "-1SD" ${Mmx_`env'} "Mean" ${Hmx_`env'} "+1SD"  ${Hmx2_`env'} "+2SD", angle(45))  ///
legend(order(1 "3rd+ Gen" 2 "2/2.5 Gen" 3 "1/1.5 Gen") rows(1) size(small) ) ///
saving(`out'_`race'.eps, replace) name(`out'_`race', replace) 
}
}
}
}
grc1leg  gpa_White gpa_Black gpa_Asian gpa_American_Indian gpa_Other gpa_Hispanic, title("GPA", color(black)) b2title("Peer Prental Education",size(small)) ring(2) legendfrom(gpa_White) graphregion(color(white))
graph export $dir\fig_combine_gpa_race.png, as(png) replace
graph export $dir\fig_combine_gpa_race.eps, as(eps) replace
graph export $dir\fig_combine_gpa_race.pdf, as(pdf) replace

********************************************************************************
***** Robustness: listwise deletion
********************************************************************************
estimates clear
foreach env in patedu_bal   {
foreach cat in   immigi  {
foreach out in  academic4 gpa   {
xtreg `out'  c.mx_`env' i.`cat' gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN  ///
female age momcollege dadcollege liveboth hhsize  ///
sx_`env' ///
, i(schoolidw1) fe robust cluster(schoolidw1)

eststo: reg `out' c.mx_`env' gradew12 gradew13 gradew14 gradew15 gradew16  sx_`env' ///
if e(sample), robust cluster(schoolidw1)

eststo: xtreg `out' c.mx_`env' gradew12 gradew13 gradew14 gradew15 gradew16  sx_`env' ///
if e(sample), i(schoolidw1) fe robust cluster(schoolidw1)

eststo: xtreg `out' c.mx_`env' i.`cat' gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN  ///
female age momcollege dadcollege liveboth hhsize ///
sx_`env' ///
, i(schoolidw1) fe robust cluster(schoolidw1)

eststo: xtreg `out' c.mx_`env'##i.`cat' gradew12 gradew13 gradew14 gradew15 gradew16 ///
whiteN blackN asianN americanN otherN hispanicN  ///
female age momcollege dadcollege liveboth hhsize  ///
sx_`env' ///
, i(schoolidw1) fe robust cluster(schoolidw1)
}
}
}
esttab using "$dir\R_listwise_deletion.rtf",  ///
star(+ 0.10 * 0.05 ** 0.01 *** 0.001)  b(3) pa se(3) ///
label stats(N r2, fmt(0 3) labels("N" "R-squared")) ///
replace nogaps compress dep nobase ///
o(mx_* *#*) ///
addnote("{\i Note.} + {\i p} < 0.10, * {\i p} < 0.05, ** {\i p} < 0.01, *** {\i p} < 0.001" ) nonote 
estimates clear

