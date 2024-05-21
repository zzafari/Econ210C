
********************************************************************************
*Zorah Zafari 
*May 15, 2023 
*ECONC PSET 2  VAR 
********************************************************************************
clear all
set scheme s1color 
graph close _all
********************************************************************************
*FILE PATHS
********************************************************************************
local datain "~/Desktop/210C_discussion/210C_psets/hw2/" 
local dataout "~/Desktop/210C_discussion/210C_psets/hw2/"
local results "~/Desktop/210C_discussion/210C_psets/hw2/"
********************************************************************************
{ 
********************************************************************************
* RECESSION SHADING 
********************************************************************************
*MONTHLY
********************************************************************************
local rsm1 "1929m8 1929m9 1929m10 1929m11 1929m12 1930m1 1930m2 1930m3 1930m4 1930m5 1930m6 1930m7 1930m8 1930m9 1930m10 1930m11 1930m12 1931m1 1931m2 1931m3 1931m4 1931m5 1931m6 1931m7 1931m8 1931m9 1931m10 1931m11 1931m12 1932m1 1932m2 1932m3 1932m4 1932m5 1932m6 1932m7 1932m8 1932m9 1932m10 1932m11 1932m12 1933m1 1933m2 1933m3 1937m5 1937m6 1937m7 1937m8 1937m9 1937m10 1937m11 1937m12 1938m1 1938m2 1938m3 1938m4 1938m5 1938m6"
local rsm2 "1945m2 1945m3 1945m4 1945m5 1945m6 1945m7 1945m8 1945m9 1945m10 1948m11 1948m12 1949m1 1949m2 1949m3 1949m4 1949m5 1949m6 1949m7 1949m8 1949m9 1949m10 1953m7 1953m8 1953m9 1953m10 1953m11 1953m12 1954m1 1954m2 1954m3 1954m4 1954m5 1957m8 1957m9 1957m10 1957m11 1957m12 1958m1 1958m2 1958m3 1958m4"
local rsm3 "1960m4 1960m5 1960m6 1960m7 1960m8 1960m9 1960m10 1960m11 1960m12 1961m1 1961m2 1969m12 1970m1 1970m2 1970m3 1970m4 1970m5 1970m6 1970m7 1970m8 1970m9 1970m10 1970m11 1973m11 1973m12 1974m1 1974m2 1974m3 1974m4 1974m5 1974m6 1974m7 1974m8 1974m9 1974m10 1974m11 1974m12 1975m1 1975m2 1975m3"
local rsm4 "1980m1 1980m2 1980m3 1980m4 1980m5 1980m6 1980m7 1981m7 1981m8 1981m9 1981m10 1981m11 1981m12 1982m1 1982m2 1982m3 1982m4 1982m5 1982m6 1982m7 1982m8 1982m9 1982m10 1982m11 1990m7 1990m8 1990m9 1990m10 1990m11 1990m12 1991m1 1991m2 1991m3"
local rsm5 "2001m3 2001m4 2001m5 2001m6 2001m7 2001m8 2001m9 2001m10 2001m11 2007m12 2008m1 2008m2 2008m3 2008m4 2008m5 2008m6 2008m7 2008m8 2008m9 2008m10 2008m11 2008m12 2009m1 2009m2 2009m3 2009m4 2009m5 2009m6 2020m2 2020m3 2020m4"
********************************************************************************
*Quarterly
********************************************************************************
local rsq1 "1929q3 1929q4 1930q1 1930q2 1930q3 1930q4 1931q1 1931q2 1931q3 1931q4 1932q1 1932q2 1932q3 1932q4 1933q1 1937q2 1937q3 1937q4 1938q1 1938q2"
local rsq2 "1945q1 1945q2 1945q3 1948q4 1949q1 1949q2 1949q3 1953q3 1953q4 1954q1 1957q3 1957q4 1958q1"
local rsq3 "1960q2 1960q3 1960q4 1961q1 1969q4 1970q1 1970q2 1970q3 1973q4 1974q1 1974q2 1974q3 1974q4 1975q1"
local rsq4 "1980q1 1980q2 1980q3 1981q3 1981q4 1982q1 1982q2 1982q3 1982q4 1990q3 1990q4 1991q1"
local rsq5 "2001q1 2001q2 2001q3 2001q4 2007q4"
local rsq6 "2008q1 2008q2 2008q3 2008q4 2009q1 2009q2 2020q1"
********************************************************************************
}

********************************************************************************
********************************************************************************


*SET UP ROMER DATA 
**JOHN: ALL YOU HAVE TO DO IS UPDATE THE LOCAL MACRO LABELED "datain" -- I added the data file you need in the repository!!! --ZZ 
use `datain'RR_monetary_shock_quarterly.dta, clear 

rename date qdate 
format qdate %tq 

tempfile romer
save `romer', replace 
	
	
	
 
	**JOHN: Hopefully you can just use import fred if not here are the steps I took to use it: 
	/*Preliminary steps: 
	1. install freduse
	2. create an account in st louis fed portal for fred data: zzafari@ucsd.edu
	3. requeset an api key 
	4. save the key in stata
	5. from now on all you have to do is "import fred `var'" 
	
	ssc install freduse, replace
	set fredkey 14f129a9671b6e891132cde7e9739593, permanently
	import fred UNRATE, clear
	*/
	
*lOAD DATA FROM FRED
local var "FEDFUNDS UNRATE GDPDEF USRECM"
	
import fred `var' , clear 


*GENERATE MDATE 	
gen mdate = mofd(daten)
format mdate %tm 
		
*GENERATE QDATE 
gen qdate = qofd(daten)
format qdate %tq

keep mdate qdate `var'
drop if mdate <= tm(1959m12) | mdate > tm(2023m12)

tsset mdate 

*GENERATE YOY INFLATION
gen INFL = ((GDPDEF - l12.GDPDEF)/l12.GDPDEF)*100
		

* LABEL VARS
lab var FEDFUNDS "Federal Funds Effective Rate"
lab var UNRATE   "Unemployment Rate"
lab var GDPDEF   "GDP Deflator"
lab var INFL	 "Inflation Rate"

		
		
*GRAPH MONTHLY SERIES
tw (tsline FEDFUNDS UNRATE INFL, lc(navy midblue blue%90) lp(solid solid solid)), ///
plotregion(style(none)) ylab(, labsize(small) angle(h)) tlab(1960m1(126)2023m1, labsize(small)) ///
ttitle("") title("Monthly U.S. Macroeconomic Indicators, 1960-2023", size(medsmall)) ///
caption("Source: FRED." "Note: Shaded regions denote recessions.", size(small)) ///
ytitle("Percent", size(small)) tline(`rsm3' `rsm4' `rsm5', lcolor(gs14) lwidth(thick) lpattern(solid)) ///
legend(order(1 "Fed Funds Rate" 2 "Unemployment Rate" 3 "Inflation Rate") cols(3) size(vsmall) symxsize(*0.5) region(lstyle(none))) ///
name(raw_data_monthly, replace)
graph export `results'rawdata_monthly.pdf, replace

*AGGREGATE ALL SERIES TO QUARTERLY FREQ BY TAKING AVERAGE OVER MONTHS
collapse (mean) FEDFUNDS UNRATE INFL, by(qdate)


*MERGE IN ROMER SHOCKS
merge 1:1 qdate using `romer' , nogen 
*REPLACE ROMER SHOCKS WITH 0 FOR QUARTERS BEFORE 1969
foreach x in  resid resid_romer resid_full{
	replace `x' = 0 if qdate <= tq(1969q1)
}


tsset qdate 

*GRAPH QUARTERLY SERIES 
tw (tsline FEDFUNDS UNRATE INFL, lc(navy midblue blue%90) lp(solid solid solid)), ///
plotregion(style(none)) ylab(, labsize(small) angle(h)) tlab(1960q1(42)2023q1, labsize(small)) ttitle("") ///
title("Quarterly U.S. Macroeconomic Indicators, 1960-2023", size(medsmall)) caption("Source: FRED." "Note: Shaded regions denote recessions.", size(small)) ///
ytitle("Percent", size(small)) tline(`rsq3' `rsq4' `rsq5', lcolor(gs14) lwidth(thick) lpattern(solid)) ///
legend(order(1 "Fed Funds Rate" 2 "Unemployment Rate" 3 "Inflation Rate") cols(3) size(vsmall) symxsize(*0.5) region(lstyle(none))) ///
name(raw_data_qdate, replace)
graph export `results'rawdata_quarterly.pdf, replace



********************************************************************************
*NOTE TO SELF: 
*var fits a multivariate time-series regression of each dependent variable on lags of itself and on lags of all the other dependent variables. 
*An IRF measures the effect of a shock to an endogenous variable on itself or on another endogenous variable
*irf set without arguments reports the identity of the active IRF file, if there is one. irf set with a filename specifies that the file be created and set as the active file. irf set, clear specifies that, if any IRF file is set, it be unset and that there be no active IRF file.
*irf create estimates multiple sets of impulse–response functions (IRFs), dynamic-multiplier functions, and forecast-error variance decompositions (FEVDs). All of these estimates and their standard errors are known collectively as IRF results and are saved in an IRF file under a specified filename. Once you have created a set of IRF results, you can use the other irf commands to analyze them.
*step # : set forecast horizon to #; default is step(8)
********************************************************************************

*Restrict analysis to before the Great Recession (1C)
keep if qdate >= tq(1960q1) & qdate <= tq(2007q4)

************************************************
*Question 1B
*Estimate VAR with 4 lags from 1960Q1 to 2007Q4
************************************************
var INFL UNRATE FEDFUNDS, lags(1/4)
irf set var_results
irf create var_result, step(20) set(var_results) replace
irf graph irf, impulse(INFL UNRATE FEDFUNDS) response(INFL UNRATE FEDFUNDS) byopts(title(VAR with 4 lags) yrescale) /// INFL UNRATE 
yline(0,  lcolor(black) lp(dash) lw(*2)) legend(col(2) order(1 "95% CI" 2 "IRF") symx(*.5) size(vsmall)) ///
name(var_results, replace )
graph export `results'var_irf.pdf, replace

**************************************************
*Question 1D
*Estimate SVAR with 4 lags from 1960Q1 to 2007Q4
**************************************************
* Manual Choleshy Decomp 
matrix A = (.,0,0 \ .,.,0 \ .,.,.)
matrix B = (1,0,0 \ 0,1,0 \ 0,0,1)
	
* Estimate SVAR model with 4 lags
svar INFL UNRATE FEDFUNDS, lags(1/4) aeq(A) beq(B)
irf create mysirf, set(mysirfs) step(20) replace
irf graph sirf, irf(mysirf) impulse(INFL UNRATE FEDFUNDS) response(INFL UNRATE FEDFUNDS)  ///
byopts(title(SVAR with 4 lags) yrescale) yline(0, lcolor(black) lp(dash) lw(*2))  /// 
legend(col(2) order(1 "95% CI" 2 "IRF") symx(*.5) size(vsmall)) ///
name(svar_irf, replace)
graph export `results'svar_irf.pdf, replace

	
/* An alternative way to do this?  -- ASK JOHN
var INFL UNRATE FEDFUNDS, lags(1/4)
irf create myirf, set(myirfs) step(20) replace
irf graph oirf, impulse(INFL UNRATE FEDFUNDS) response(INFL UNRATE FEDFUNDS) ///
yline(0, lcolor(black) lp(dash)) ///
name(svar_results_oirf, replace )
graph export `results'svar_irf_oirf.pdf, replace	
*/


*****************************************************
*Question 1F
*Plot time series of your identified monetary shocks 
*****************************************************
* Recover estimated residuals from federal fund rate -- this is the monetary shock? 
predict resid_monetary, residuals equation(FEDFUNDS)
	
* Plot residuals and save graph
tw (tsline resid_monetary, lc(navy) lp(solid)), ///
plotregion(style(none)) ylab(, labsize(small) angle(h)) tlab(1960q1(32)2007q1, labsize(small)) ttitle("") ///
title("Identified Monetary Shock", size(medsmall)) caption("Source: FRED." "Note: Shaded regions denote recessions.", size(small)) ///
ytitle("Residuals", size(small)) tline(`rsq3' `rsq4' `rsq5', lcolor(gs14) lwidth(thick) lpattern(solid)) ///
legend(order(1 "Estimated Monetary Shock") cols(3) size(vsmall) symxsize(*0.5) region(lstyle(none))) ///
name(question1f, replace)
graph export `results'monetary_shock_q1f.pdf, replace


********************************************************************************
*Question 2B
*Following Romer-Romer, construct the IRF from the esimation equation
********************************************************************************
* Estimate VAR model with RR shocks as exogenous variables
tsset qdate 
var INFL UNRATE FEDFUNDS, lags(1/8) exog(L(0/12).resid_full)
irf create myrirf, step(20) replace
irf graph dm, impulse(resid_full) irf(myrirf) byopts(title(VAR with 8 Lags and RR Shocks) yrescale) /// INFL UNRATE 
yline(0,  lcolor(black) lp(dash) lw(*2)) legend(col(2) order(1 "95% CI" 2 "IRF") symx(*.5) size(vsmall))  ///
name(var_results, replace )
graph export `results'var_irf_RRshock.pdf, replace


********************************************************************************
*Question 2C
*Estimate SVAR 
********************************************************************************

* Create matrices for ordering
matrix AA = (.,0,0,0 \ .,.,0,0 \ .,.,.,0 \ .,.,.,.)
matrix BB = (1,0,0,0 \ 0,1,0,0 \ 0,0,1,0 \ 0,0,0,1)

* Estimate SVAR model with 4 lags
svar resid_full INFL UNRATE FEDFUNDS, lags(1/4) aeq(AA) beq(BB)
irf create svarrr, step(20) replace
irf graph sirf, irf(svarrr) impulse(resid_full INFL UNRATE FEDFUNDS) response(resid_full INFL UNRATE FEDFUNDS) ///
byopts(title(SVAR with 8 Lags and RR Shocks ) yrescale) /// 
yline(0,  lcolor(black) lp(dash) lw(*2)) legend(col(2) order(1 "95% CI" 2 "IRF") symx(*.5) size(vsmall))  ///
name(svar_rrshcok, replace )
graph export `results'svar_RRshock.pdf, replace 






	
	

stop 
