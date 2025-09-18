global bases "C:\Users\gusta\Documents\Student-Lottery"

clear all

use "$bases\base_lotteries.dta", clear
set seed 0309

global balance sex white mother_high_school father_high_school ever_failed_d distance

replace sex = 0 if sex == 2 

gen white =.
replace white = 1 if race == 1
replace white = 0 if race !=. & race !=1


foreach var in mother father{
    replace `var'_schooling =. if `var'_schooling == 7
    gen `var'_high_school =.
	replace `var'_high_school = 1 if `var'_schooling >= 4
	replace `var'_high_school = 0 if `var'_schooling <=3
}

gen ever_failed_d =.
replace ever_failed_d = 1 if ever_failed_school >=2
replace ever_failed_d = 0 if ever_failed_school == 1

replace won_lotteryA =. if lotteryA == 0

*comparing treatment and control

tempfile results
postfile handle str20 covar str5 lottery double cmean csd coef se p using `results', replace

foreach var in A B {
    foreach covar in $balance {

        quietly summarize `covar' if won_lottery`var' == 0
        local cmean = r(mean)
        local csd   = r(sd)

        quietly reg `covar' won_lottery`var'
        local coef = _b[won_lottery`var']
        local se   = _se[won_lottery`var']
        local df   = e(df_r)
        local t    = `coef'/`se'
        local p    = 2*ttail(`df', abs(`t'))

        post handle ("`covar'") ("`var'") (`cmean') (`csd') (`coef') (`se') (`p')
    }
}


* Close the postfile and open the results
postclose handle
preserve
use `results', clear

foreach var in cmean csd coef se p{
	replace `var' = round(`var', 0.001)
}

listtex covar lottery cmean csd coef se p using balance.tex, replace ///
    rstyle(tabular) ///
    head("\begin{tabular}{l l c c c c c} \toprule Covariate & Lottery & Control Mean & SD & Coef & SE & p-value \\ \midrule") ///
    foot("\bottomrule \end{tabular}")

restore


**how can we adjust the test to multiple hypothesis testing? 
*** Young (2019) ***
local k : word count $balance 

foreach lot in A B{
	*matrix with the real estimates
	matrix bE = J(`k',1,.)
	matrix VE = J(`k',`k',0)

	*this command makes a regression with each var in $balance ~ won_lotteryA
	mvreg $balance = won_lottery`lot'

	matrix bfull = e(b)'
	matrix Vfull = e(V)

	forvalues i = 1/`k'{
		local col = 2*`i' - 1 
		matrix bE[`i',1] = bfull[`col', 1]
		forvalues j = 1/`k'{
			local col2 = 2*`j' - 1
			matrix VE[`i', `j'] = Vfull[`col', `col2']
		}
	}


	matrix wald = bE' * invsym(VE) * bE
	scalar QE = wald[1,1]
	display QE

	matrix QE_it = J(1000, 1,.)
	matrix QE_it[1,1] = QE

	forvalues r = 2/1000{
		gen double won_lottery`lot'_r = runiform() < 0.3
		
		quietly mvreg $balance = won_lottery`lot'_r
		
		matrix bfull = e(b)'
		matrix Vfull = e(V)
		
		forvalues i = 1/`k'{
		local col = 2*`i' - 1 
		matrix bE[`i',1] = bfull[`col', 1]
		forvalues j = 1/`k'{
			local col2 = 2*`j' - 1
			matrix VE[`i', `j'] = Vfull[`col', `col2']
			}
		}
		matrix wald = bE' * invsym(VE) * bE
		scalar QE_r = wald[1,1]
		display QE_r

		matrix QE_it[`r',1] = QE_r
		
		drop won_lottery`lot'_r
	}

	*calculating p-value 

	local greater = 0
	forvalues i = 1/1000{
		if QE_it[`i',1] >= QE{
			local greater = `greater' + 1
		}
	}

	gen pvalue_`lot' = `greater'/1000
	display pvalue_`lot'
}



****comparing participants in the first and second lotteries

tempfile balance2 
postfile handle str20 covar double cmean csd coef_a se_a p_a coef_b se_b p_b p_equal using `balance2', replace

local lotteries lotteryA lotteryB

foreach covar in $balance{
	quietly summarize `covar' if lotteryA == 0 & lotteryB == 0
	local cmean = r(mean)
	local csd = r(sd)
	
	quietly reg `covar' `lotteries'
	local coef_a = _b[lotteryA]
	local se_a = _se[lotteryA]
    local t_a    = `coef_a'/`se_a'
    local p_a    = 2*ttail(e(df_r), abs(`t_a'))
	
	local coef_b = _b[lotteryB]
	local se_b = _se[lotteryB]
    local t_b   = `coef_b'/`se_b'
    local p_b    = 2*ttail(e(df_r), abs(`t_b'))
	
	
	test lotteryA = lotteryB
    local p_equal = r(p)
    
    post handle ("`covar'") (`cmean') (`csd') (`coef_a') (`se_a') (`p_a') ///
                 (`coef_b') (`se_b') (`p_b') (`p_equal')
}

postclose handle

preserve 
use `balance2', clear

foreach var in cmean csd coef_a se_a p_a coef_b se_b p_b p_equal{
	replace `var' = round(`var', 0.001)
}

listtex covar cmean csd coef_a se_a p_a coef_b se_b p_b p_equal using balance2.tex, replace ///
    rstyle(tabular) ///
    head("\begin{tabular}{l l c c c c c} \toprule Covariate & Control Mean & SD & Coef_A & SE_A & p-value_A & Coef_B & SE_B & p-value_B & p-value A=B \\ \midrule") ///
    foot("\bottomrule \end{tabular}")

restore

*randomization inference (again) 

local k : word count $balance 
local lotteries lotteryA lotteryB

matrix bE = J(`k', 1,.)
matrix VE = J(`k', `k',0)

mvreg $balance = `lotteries'

matrix bfull = e(b)'
matrix Vfull = e(V)

forvalues i = 1/`k'{
    local colA_i = `i'*3 - 2
	local colB_i = `i'*3 - 1
	matrix bE[`i', 1] = bfull[`colA_i',1] - bfull[`colB_i',1]
	forvalues j = 1/`k' {
    local colA_j = 3*`j' - 2
    local colB_j = 3*`j' - 1

    matrix VE[`i',`j'] = Vfull[`colA_i',`colA_j'] + Vfull[`colB_i',`colB_j'] ///
                         - Vfull[`colA_i',`colB_j'] - Vfull[`colB_i',`colA_j']
}
}

matrix wald = bE' * invsym(VE) * bE
scalar QE_ab = wald[1,1]
display QE_ab

matrix QE_ab_it = J(1000,1,.)
matrix QE_ab_it[1,1] = QE_ab

forvalues r = 2/1000 {
    gen double lotteryA_r = runiform() < 0.3
    gen double lotteryB_r = runiform() < 0.3

    quietly mvreg $balance = lotteryA_r lotteryB_r
    matrix bfull = e(b)'
    matrix Vfull = e(V)
	
	matrix bE = J(`k',1,.)
	matrix VE = J(`k',`k',0)

    forvalues i = 1/`k' {
        local colA_i = 3*`i' - 2
        local colB_i = 3*`i' - 1
        matrix bE[`i',1] = bfull[`colA_i',1] - bfull[`colB_i',1]
        forvalues j = 1/`k' {
            local colA_j = 3*`j' - 2
            local colB_j = 3*`j' - 1
            matrix VE[`i',`j'] = Vfull[`colA_i',`colA_j'] + Vfull[`colB_i',`colB_j'] ///
                                 - Vfull[`colA_i',`colB_j'] - Vfull[`colB_i',`colA_j']
        }
    }

    matrix wald = bE' * invsym(VE) * bE
    scalar QE_r = wald[1,1]
    display QE_r

    matrix QE_ab_it[`r',1] = QE_r
    drop lotteryA_r lotteryB_r
}

local greater = 0
forvalues i = 1/1000{
    if QE_ab_it[`i',1] >= QE_ab{
	    local greater = `greater' + 1
	}
}

scalar pvalue_ab = `greater'/1000
display pvalue_ab 


****Question3 ****

foreach grade in 8 9 {
	foreach subject in mt lp{
		quietly summarize score_`subject'_`grade' if won_lotteryA == 0
		scalar `subject'_`grade'_cmean = r(mean)
		scalar `subject'_`grade'_sd = r(sd)
		gen score_`subject'_`grade'_st =.
		replace score_`subject'_`grade'_st = (score_`subject'_`grade' - `subject'_`grade'_cmean)/`subject'_`grade'_sd
}
	
}

reg score_mt_8_st won_lotteryA 
eststo model_nocov

reg score_mt_8_st won_lotteryA score_mt_7
eststo model_cov

esttab model_nocov model_cov, se star(* 0.10 ** 0.05 *** 0.01)

esttab model_nocov model_cov using regressions_3.tex, se star(* 0.10 ** 0.05 *** 0.01) replace label ///
title("Treatment Effects: $\mathbb{E}\big(Y_i^8(1) - Y_i^8(0)\big)$") booktabs



** permutation test

local nocov "won_lotteryA" 
local cov "won_lotteryA score_mt_7"

foreach model in cov nocov{
		local vars = "`" + "`model'" + "'"
		
		matrix QE_`model' = J(1000,1,.)
		matrix QE_`model'_se = J(1000,1,.)
		
		regress score_mt_8_st `vars'
		scalar beta_`model' = e(b)[1,1]
		scalar se_`model' = e(V)[1,1]
		
		matrix QE_`model'[1,1] = abs(beta_`model')
		matrix QE_`model'_se[1,1] = abs(beta_`model'/se_`model')
		
		forvalues r = 2/1000{
		    gen double lotteryA_r = runiform() < 0.5
			if "`model'" == "cov"{
			    quietly regress score_mt_8 lotteryA_r score_mt_7
			}
			
			if "`model'" == "nocov"{
			    quietly regress score_mt_8 lotteryA_r
			} 

			scalar beta_r = e(b)[1,1]
			scalar se_r = e(V)[1,1]
			
			matrix QE_`model'[`r',1] = abs(beta_r)
			matrix QE_`model'_se[`r',1] = abs(beta_r/se_r)
			
			drop lotteryA_r
			display `r'
		}
}


foreach model in nocov cov {
    local greater_`model' = 0
    forvalues i = 1/1000 {
        if QE_`model'[`i',1] >= abs(beta_`model') {
            local greater_`model' = `= `greater_`model'' + 1'
        }
    }
    local pvalue_`model' = `= `greater_`model'' / 1000'
    
    display "`model' p-value = " `pvalue_`model''
}

foreach model in nocov cov {
    local greater_`model' = 0
	forvalues i = 1/1000{
	    if QE_`model'_se[`i',1] >= abs(beta_`model'/se_`model'){
		    local greater_`model' = `= `greater_`model'' + 1'
		}
	}
	local pvalue_`model' = `= `greater_`model'' / 1000'
    
    display "`model' p-value (with se) = " `pvalue_`model''
}

*estimator for E(Y(1,1) - Y(0,0)|L=1)



program define gamma_est, rclass
    quietly summarize score_mt_9_st if lotteryA == 1 & won_lotteryA == 1
    scalar mu11 = r(mean)

    quietly summarize score_mt_9_st if lotteryA == 1 & won_lotteryA == 0 & lotteryB == 0
    scalar mu00 = r(mean)

    quietly summarize score_mt_9_st if lotteryA == 1 & won_lotteryA == 0 & lotteryB == 1 & won_lotteryB == 0
    scalar mu01 = r(mean)

    quietly summarize lotteryB if lotteryA == 1 & won_lotteryA == 0
    scalar p1 = r(mean)            // P(Ltilde==1 | L==1 & Z==0)
    scalar p0 = 1 - p1

    scalar EY00 = p0*mu00 + p1*mu01
    scalar gamma = mu11 - EY00

    return scalar gamma = gamma
end

* 2) bootstrap the returned scalar
bootstrap r(gamma), reps(2000) nodots: gamma_est

eststo gamma_model
esttab gamma_model using "regression_3d.tex", ///
    se ar2 label replace ///
    title("Bootstrap Estimate of the Treatment Effect $\big(Y_i^9(1,1) - Y_i^9(0,0)\big)|L=1$") ///
    mtitles("Estimate")
	
	



** Question 4

forvalues r= 0/3{
    regress score_mt_8_st won_lotteryA if distance == `r'
	eststo model`r'
}
suest model0 model1 model2 model3
test [model0_mean]won_lotteryA = [model1_mean]won_lotteryA = ///
     [model2_mean]won_lotteryA = [model3_mean]won_lotteryA

local p_equal = r(p) 

esttab model0 model1 model2 model3 using reg_distance.tex, se star(* 0.10 ** 0.05 *** 0.01) replace label ///
title("Treatment Effects by Distance to School") mtitles("Bin 0" "Bin 1" "Bin 2" "Bin 3") addnotes("Joint equality test p-value = `p_equal'") booktabs


** Questão 5
regress score_mt_8_st i.distance i.father_schooling i.mother_schooling i.distance sex i.race i.ever_failed_school if won_lotteryA == 0


* we have some treated observations with missing values for the covariates. We could substitute missing to median or mode on each of the missing variables. I won't do that.
predict y0_hat if won_lotteryA == 1
replace y0_hat = score_mt_8_st if won_lotteryA == 0

xtile score_tercile = y0_hat, n(3) 
forvalues x = 1/3{
    quietly regress score_mt_8_st won_lotteryA if score_tercile == `x'
	eststo model`x'
}
esttab model1 model2 model3 using reg_terciles.tex, se star(* 0.10 ** 0.05 *** 0.01) replace label ///
	title("Treatment Effects by $\hat{Y}^8(0)$") mtitles("$1^{st}$ tercile" "$2^{nd} tercile$" "$3^{rd} tercile$") booktabs
	

*Question 6	
gen score_avg_8st =.
replace score_avg_8st = (score_mt_8_st + score_lp_8_st)/2

regress score_avg_8st won_lotteryA
eststo model1

gen score_avg_8 =.
replace score_avg_8 = (score_mt_8 + score_lp_8)/2

regress score_avg_8 won_lotteryA
eststo model2

label variable score_avg_8st "Average of Std. Scores" 
esttab model1 model2 using reg_pt_mat.tex, se star(* 0.10 ** 0.05 *** 0.01) replace label ///
	title("Treatment Effects considering Portuguese and Math Test Scores") 
	
	
*Question 7
merge 1:1 student_code using "$bases/voluntary_exam.dta", keep(master match) gen(vol_exam_d)
recode vol_exam_d (1 = 0) (3 = 1) 

regress vol_exam_d won_lotteryA
esttab . using reg_7a.tex, se star(* 0.10 ** 0.05 *** 0.01) replace label ///
	title("Effect of Full-time School on the Prob. of taking Optional Exam") 

*Balance table for non-attriters

preserve 

keep if vol_exam_d == 1

tempfile balance3
postfile handle str20 covar double cmean csd coef se p using `balance3', replace

foreach covar in $balance{
    summarize `covar' if won_lotteryA == 0
	local cmean = r(mean)
	local csd = r(sd)
	
	regress `covar' won_lotteryA
	local coef = _b[won_lotteryA]
    local se   = _se[won_lotteryA]
    local df   = e(df_r)
    local t    = `coef'/`se'
    local p    = 2*ttail(`df', abs(`t'))

    post handle ("`covar'") (`cmean') (`csd') (`coef') (`se') (`p')
	
}

postclose handle

use `balance3', clear

foreach x in cmean csd coef se p{
    replace `x' = round(`x', 0.001)
}

listtex covar cmean csd coef se p using balance3.tex, replace ///
    rstyle(tabular) ///
    head("\begin{table}[ht]\centering \caption{Balance Table for Non-Attriters} \begin{tabular}{l c c c c c} \toprule  Covariate & Control Mean & SD & Coef & SE & p-value \\ \midrule") ///
    foot("\bottomrule \end{tabular}\end{table}")

restore 

eststo clear
preserve 

keep if won_lotteryA !=.
leebounds voluntary_exam_score won_lotteryA
eststo model_lee

esttab model_lee using lee_bounds.tex, replace label se star(* 0.10 ** 0.05 *** 0.01) title("Lee Bounds for Treatment Effect on Voluntary Exam Score")
restore