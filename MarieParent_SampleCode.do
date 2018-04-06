********************************************************************************
*           		          Work Sample
*              		 	    By Marie Parent 
*								Mar 2018
********************************************************************************

set more off
* Set the path to your data folder here
global path="set your path here" 


********************************************************************************
* Power Calculations ***********************************************************

/* Demo of power calculations.  Data set is taken from Paul Gertler's Applied
Impact Evaluation class offered at UC Berkeley in Spring 2018.
Conduct power calculations with income as the variable of interest.
For all calculations, assume a power of 90% and a significance level of 5%. */

use $path\PowerCalculations.dta, clear
* Confirm that all observations are unique households
codebook hogid iid
* Compute the mean and standard deviation of income
sum IncomeLabHH1
* MDE for N=1500, power of 90%, significance level of 5%
power twomeans 2585, sd(1402) alpha(0.05) power (0.9) n(1500)
* Sample size to detect effect of +300, power of 90%, significance level of 5%
power twomeans 2585 2885, sd(1402) alpha(0.05) power(0.9)
* Sample size to detect effect of +300, power of 80%, significance level of 1%
power twomeans 2585 2885, sd(1402) alpha(0.01) power(0.8)
* Sample size to detect a .2 std dev effect, power of 90%, significance level of 5%
power twomeans 0 0.2, sd(1) alpha(0.05) power(0.9)
* Graph of required sample sizes for the above with varying power
power twomeans 0 0.2, sd(1) alpha(0.05) power(0.5(0.1)0.90) graph(y(power))
********************************************************************************



********************************************************************************
* RCT Analysis *****************************************************************

/* Demo of analysis of RCT data.  Data set is a modified version of PROGRESA RCT
data taken from Paul Gertler's Applied Impact Evaluation class offered at UC 
Berkeley in Spring 2018 and modified by Marie Parent for the purposes of this 
work sample.  
Analysis: Look for a difference in child labor outcomes among children 8-12
between treatment and control groups. */

* Prepare Data *****************************************************************
use $path\RCTAnalysis.dta, clear

desc
br
sum
graph twoway scatter agehead IncomeLab_HH
hist age

* Create a Unique Village Identifier 
destring entidad munici locali, replace
codebook entidad munici locali

gen str3 entidad_temp = string(entidad,"%02.0f")
gen str3 munici_temp = string(munici,"%03.0f")
gen str3 locali_temp = string(locali,"%03.0f")

egen villid = concat(entidad_temp munici_temp locali_temp)
label var villid "Unique Village Identifier"
drop entidad* munici* locali* 
drop if year==1998

* Clean up other variables
rename hogid hhid
label define D 0 "Control Village" 1 "Treatment Village"
label val D D
order year iid hhid villid

* Merge in Attrition Data
merge m:1 iid using $path/attrition_details.dta
drop _merge
bysort D D_HH: tab attri_path if year==1997
bysort D D_HH: tab attri_path if year==1999
gen attrition = 0
	replace attrition = 1 if attri_path != "1.1" & attri_path != "111" 
	label var attrition "Individual is present in '97 but missing in '99"
drop present*
* Check for Differential Attrition
regress attrition D famsize agehead sexhead if (pov_HH_bl==1 & year==1997), ///
	cluster(villid)

* Regression *****************************************************************
gen child = 0
	replace child = 1 if age > 7 & age < 13
* Check baseline characteristics
ttable2 age sex labor IncomeLab_HH famsize agehead sexhead if year==1997, by D
* Regression with all children 8-12 (ITT - Intent to Treat)
reg labor D famsize agehead sexhead if year==1999 & child==1, cluster(villid)
* Regression with female children 8-12 (ITT)
gen fem8_12 = 0
	replace fem8_12 = 1 if sex==0 & age<=12 & age>=8
reg labor D famsize agehead sexhead if year==1999 & fem8_12==1, cluster(villid)
* Estimate LATE using village treatment assignment as an IV  
ivregress 2sls labor famsize agehead sexhead (D_HH=D) ///
	if year==1999 & fem8_12==1, cluster(villid)
********************************************************************************



********************************************************************************
* Matching Methods *************************************************************

/* Demo of proprensity score matching. */

use $path\Matching.dta, clear
set seed 111
	gen rnd = runiform()
	sort rnd
ssc install psmatch2 
psmatch2 treat bpl_hhs num_hp IsPW IsReach, logit
pstest bpl_hhs num_hp IsPW IsReach, t(treat) both graph
psgraph, treated(treat) pscore(_pscore) bin(100) 
********************************************************************************




