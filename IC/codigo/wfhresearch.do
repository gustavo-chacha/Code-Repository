global base "C:\Users\Chacha\Desktop\FGV\WFH\data"
global figures "C:\Users\Chacha\Desktop\FGV\WFH\figures" 
{
clear all 
import delimited "$base/WFHdata_March24.csv", clear

*cleaning data

recode female (100 = 1)
recode wfhcovid (100=1)
recode wfhcovid_ever (100=1)
recode live_children (1=0)
local dates_attn "2021m11 2021m12 2022m1 2022m2 2022m3 2022m4 2022m5 2022m6 2022m7 2022m8 2022m9 2022m10 2022m11 2022m12 2023m1 2023m2 2023m3 2023m4 2023m5 2023m6 2023m7 2023m8 2023m9 2023m10 2023m11 2023m12 2023m12 2024m1 2024m2 2024m3"
drop if grass_color_attnfull != 3 & strpos("`dates_attn'", date) > 0
local dates_attn_c "2021m12 2022m1 2022m2 2022m3 2022m4 2022m5 2022m6 2022m7 2022m8 2022m9 2022m10 2022m11 2022m12 2023m1 2023m2 2023m3 2023m4 2023m5 2023m6 2023m7 2023m8 2023m9 2023m10 2023m11 2023m12 2023m12 2024m1 2024m2 2024m3"
drop if cities_attn != 33 & strpos("`dates_attn_c'", date) > 0

keep income female age_quant date educ_years education race_ethnicity_s wfhcovid wfhcovid_ever wfhcovid_frac wfh_able wfh_able_qual numwfh_days_postcovid_s_u wfh_feel_quant region logpop_den_job_precovid haschildren live_children hours_cc_you hours_cc_partner hours_cc_other hours_cc_partner_precovid hours_cc_you_precovid hours_cc_other_precovid wfh_eff_covid_quant wfh_extraeff_comm_qual extratime_childcare wfh_eff_nocovid_qual wfh_eff_covid_qual lesseff_reasons_kidsinterr moreeff_reasons_quieter infection_labsearch_wfhpref labsearch_qual workstatus_current_d hours_training hours_mentored hourly_wage work_industry occupation wfh_able_qual_may21 offer_unemployed_wfh1days offer_unemployed_wfh23days offer_unemployed_wfh45days offer_employed_wfh1days offer_employed_wfh23days offer_employed_wfh45days wbp_react_qual who_decides_wfhdays employer_arr_qual nilf_mainreason_hps_clean howlong_resi_current extratime_1stjob commutetime_quant cratio100 workhours_precovid workhours_duringcovid self_employment wfh_invest_quant wfh_hoursinvest

gen hours_cc_not_you = 0 
replace hours_cc_not_you = hours_cc_partner + hours_cc_other
gen hours_cc_not_you_precovid = 0
replace hours_cc_not_you_precovid = hours_cc_partner_precovid + hours_cc_other_precovid

rename wfh_feel_quant wtp_wfh_23
rename numwfh_days_postcovid_s_u desired_wfh
la var income "2019 earnings"
la var wfhcovid_frac "Share of WFH days this week"
la var desired_wfh "Desired share of wfh days after covid"
la var wfhcovid_frac "Share wfh days this week"
la var wtp_wfh_23 "Pay raise/cut value WFH 2/3 days per week"
la var logpop_den_job_precovid " Log(pop density precovid job city)"
la var wfh_eff_covid_quant "How efficient are you WFH during covid?"
la var wfh_extraeff_comm_qual "Time saved by not commuting part of extra efficiency of WFH?"
la var wfh_able_qual_may "Do you need to be phisically present to perform your job (or most recent)?"
la var employer_arr_qual "Employer plans for working arrangements after Covid"
la var extratime_childcare "Percent of commute time savings spent on childcare"
la var extratime_1stjob "Percent of commute time savings spent working on primary or current job"
la var wfh_eff_nocovid_qual "How does your efficiency working from home compare to your efficiency working on business premises?"
la var lesseff_reasons_kidsinterr "Why are you less efficient when working from home? - I am frequently interrupted by my kids"
la var moreeff_reasons_quieter "Apart from saving time by not commuting, why are you more efficient when working from home? - My home is quieter and has fewer interruptions"
la var infection_labsearch_wfhpref "In my job search, I prefer jobs allowing me to work from home"
la var labsearch_qual "Which of the following best describes your job search (with respect to WFH)?"
la var offer_unemployed_wfh1days "Would you be more or less likely to take the job if it let you work from home 1 day a week?"
la var wbp_react_qual "How would you respond if your employer announced that all employees must return to worksite 5+ days a week starting [month-after-next]?"
la var employer_arr_qual "What plans does your employer have for working arrangements of full-time employees after COVID, in 2022 or later?"
la var nilf_mainreason_hps_clean "What is your main reason for not working for pay or profit?"
la var howlong_resi_current "How long have you lived in your current residence?"
la var commutetime_quant "Time spent commuting (minutes)"


label def wfh_able_lab 1 "100%+ efficient" 2 " 80-90% efficient" 3 " 50-70% efficient" 4 "less than 50% efficient" 5 " I cannot do my job at home"
la values wfh_able wfh_able_lab
la def educ_la 1 "Less than high-school" 2 "High-school diploma" 3 "1-3 years of college" 4 "4 years of college" 5 "Masters or professional degree" 6 "phd"
la values education educ_la 
la def workstatus_la 1 "Working on my business premises" 2 "Working from home" 3 "Employed e paid, but not working" 4 "Unemployed" 5 "Not working of looking for work"
la values workstatus_current_d workstatus_current_d
la def live_children_la 0 "No" 2 "Yes, youngest in pre/primary" 3 "Yes, youngest in elementary" 4 "Yes, youngest in middle school" 5 "Yes, youngest in high school"
la values live_children live_children_la 
gen live_young_child = 0 
replace live_young_child = 1 if live_children == 2 | live_children == 3 | live_children ==  4
la var live_young_child "Lives with young child? (Primary or elementary school)"
la def occ_la 1 "Armed Forces" 2 "Construction and extraction" 3 "Farming, fishing and forestry" 4 "installation, maintenance and repair" 5 "management, business and financial" 6 "office and administrative support" 7 "production" 8 "professional and related" 9 "sales and related" 10 "service" 11 "transportation and material moving" 12 "other" 
la values occupation occ_la
la def race_la 1 "Black" 2 "Hispanic" 3 "Other" 4 "White"
la values race_ethnicity_s race_la
la def industry_la 1 "Agriculture" 2 "Arts and entertainment" 3 "Finance and insurance" 4 "Construction" 5 "Education" 6 "Health care and social assistance" 7 "Hospitality and food services" 8 "Information" 9 "Manufacturinh" 10 "Mining" 11 "Profssional and business services" 12 "Real estate" 13 "Retail trade" 14 "Transportation and warehousing" 15 "Utilities" 16 "Wholesale trade" 17 "Government" 18 "Other"
la values work_industry industry_la
la def wfh_able_qual_la 1 "Yes, for all my job" 2 "Yes, for part of my job" 3 "No"
la values wfh_able_qual_may21 wfh_able_qual_la
la def offer_la 1 "More likely to consider" 2 "No effect" 3 "Less likely to consider"
la values offer_unemployed_wfh1days offer_la
la values offer_unemployed_wfh23days offer_la
la values offer_unemployed_wfh45days offer_la
la values offer_employed_wfh1days offer_la
la values offer_employed_wfh23days offer_la
la values offer_employed_wfh45days offer_la
la def wbp_la 1 "Comply and return" 2 "Return and start looking for wfh job" 3 "quit, regardless of getting another job"
la values wbp_react_qual wbp_la
la def whodecides_la 1 "Each employee" 2 "Each team" 3 "Company-wide common schedule" 4 "Company-wide varying schedule" 5 "No clear plans from employer" 6 "Other" 
la values who_decides_wfhdays whodecides_la
la def employer_arr_la 1 "Fully on-site" 2 "Hybrid" 3 "Fully remote" 4 "No clear plans" 5 "Other" 
la values employer_arr_qual employer_arr_la
la def labsearch_qual_la 1 "Only considering wfh jobs" 2 "Prefer jobs allowing wfh" 3 "No preference about wfh"
la values labsearch_qual labsearch_qual_la
la def nilf_la 1 "Don't want to be employed" 2 "Sick/caring for someone w covid" 3 "Caring for children" 4 "Caring for elderly person" 5 "concern about catching/spreading covid" 6 "sick but not w covid" 7 "Retired" 8 "Laid off because of the pandemic" 9 "Emplyer out of business because of pandemic" 10 "No transportation to work" 11 "Other reason"
la values nilf_mainreason_hps_clean nilf_la
la def resi_la 1 "Under 2 months" 2 "2 to 6 months" 3 "7 to 12 months" 4 "1 to 5 years" 5 "Over 5 years"
la values howlong_resi_current resi_la
la def wfh_eff_qual_la 1 "Better" 2 "About the same" 3 "Worse"
la values wfh_eff_covid_qual wfh_eff_qual_la
la values wfh_eff_nocovid_qual wfh_eff_qual_la

gen hours_cc_all = hours_cc_you + hours_cc_other + hours_cc_partner
gen hours_cc_all_precovid = hours_cc_you_precovid + hours_cc_other_precovid + hours_cc_partner_precovid
foreach x in hours_cc_you hours_cc_other hours_cc_partner{
	gen share_`x' = `x'/hours_cc_all
	gen share_`x'_precovid = `x'_precovid/hours_cc_all_precovid
}

gen year =.
replace year = real(substr(date,1,4))

gen experience = age_quant - educ_years - 6
replace desired_wfh = desired_wfh/100 
sum wfh_eff_covid_quant
replace wfh_eff_covid_quant = wfh_eff_covid_quant/100 
gen wfh_eff_covid_quant_st = (wfh_eff_covid_quant -r(mean))/r(sd)

gen live_young_female =.
replace live_young_female = 1 if live_young_child == 1 & female == 1 
replace live_young_female = 0 if live_young_child == 0 | female == 0


xtile wfhfrac_abov_median = wfhcovid_frac, n(2)
recode wfhfrac_abov_median (1=0) (2=1)

recode wfh_able_qual (1=0) (2=1)
gen wfh_able_all =.
replace wfh_able_all = 0 if wfh_able_qual_may21 !=. | wfh_able_qual !=.
replace wfh_able_all = 1 if wfh_able_qual_may21 == 1 | wfh_able_qual_may21 == 2 | wfh_able_qual == 1

gen wfh_any = .
replace wfh_any = 0 if wfhcovid_frac !=.
replace wfh_any = 1 if wfhcovid_frac >= 30 & wfhcovid_frac !=.

encode region, gen(region_num)

egen id = seq(), from(1) to(`=_N')

svyset id [pweight=cratio100]
xtile wage_tiles = hourly_wage, n(100)
drop if wage_tiles >= 95 | wage_tiles <= 5 
save "$base/clean_wfhdata.dta", replace
}

* Load cleaned WFH dataset
use "$base/clean_wfhdata.dta", clear

* Run regressions separately for subsamples: above vs. below median WFH fraction
foreach x in 1 0 {

    * Regression of desired WFH days on covariates
	svy: regress desired_wfh experience female live_young_child live_young_female commutetime_quant i.region_num i.occupation i.work_industry i.race_ethnicity_s income educ_years if wfhfrac_abov_median == `x'

	* Store and export results
	eststo des_wfh_reg
	esttab des_wfh_reg using "$figures/desired_wfh_regression_`x'.tex", ///
	    title("Desired Share of WFH days") keep(female live_young_child live_young_female commutetime_quant educ_years ) tex b(%9.5f) se(%9.4fc) wide replace  

    * Regression of willingness-to-pay for WFH on covariates
	svy: regress wtp_wfh_23 female live_young_child income live_young_female experience commutetime_quant i.region_num i.occupation i.work_industry i.race_ethnicity_s educ_years wfh_able_all if wfhfrac_abov_median == `x' 

	* Store and export results
	eststo wtp_wfh
	esttab wtp_wfh using "$figures/wtp_wfh_regression_`x'.tex", keep(female live_young_child live_young_female commutetime_quant educ_years income) tex b(%9.5f) se(%9.4fc) wide replace 

    * Regression of WFH productivity (relative to office)
	svy: regress wfh_eff_covid_quant income female live_young_child experience live_young_female i.race_ethnicity_s educ_years i.region_num i.occupation i.work_industry extratime_1stjob if wfhfrac_abov_median == `x'

	* Store and export results
	eststo productivity_wfh_reg
	esttab productivity_wfh_reg using "$figures/productivity_wfh_reg_`x'.tex", ///
	    title("Relative productivity of Working from Home (Standardized)") ///
	    keep(female live_young_child live_young_female educ_years income) tex b(%9.5f) se(%9.4fc) wide replace 
	
	* Extra time spent on first job (drop self-employed missing values first)
	drop if self_employment == .
	svy: regress extratime_1stjob experience female live_young_child live_young_female income educ_years i.race_ethnicity_s commutetime_quant i.region_num i.occupation i.work_industry if wfhfrac_abov_median == `x'
	eststo extra_first
	esttab extra_first using "$figures/times_on_first_job_`x'.tex", ///
	    keep(_cons female live_young_child live_young_female) tex b(%9.5f) se(%9.4fc) wide replace 
	
	* Extra childcare time (drop missing young child values first)
	drop if live_young_child == . 
	svy: regress extratime_childcare female income educ_years i.race_ethnicity_s commutetime_quant i.region_num i.occupation i.work_industry if wfhfrac_abov_median == `x'
	eststo extra_childcare 
	esttab extra_childcare using "$figures/time_on_childcare_`x'.tex", ///
	    keep(_cons female educ_years income) ///
	    title("Time saved by commuting spent on childcare") b(%9.5f) se(%9.4fc) wide tex replace 
}

* --------------------------------------------------------------------------
* Construct dataset: mean WFH by occupation x industry x year
* --------------------------------------------------------------------------
clear 
gen occupation = .
save "$base/wfh_rate_usa_year.dta", replace

foreach w in 2020 2021 2022 2023 {
	foreach x in 1 2 3 4 5 6 10 12 13 14 15 16 18 19 20 22 25 {
		foreach y in 0 1 2 4 5 6 7 8 {

			use "$base/clean_wfhdata.dta", clear

			* Recode industry and occupation categories to harmonize with US classifications
			recode work_industry (2=22) (3=14) (4=5) (5=19) (6=20) (7=12) (8=13) (9=3) (10=2) (11=16) (12=15) (13=6) (14=10) (15=4) (16=6) (17=18) (18=25)
			recode occupation (1=0) (5=1) (9=5) (10=5) (3=6) (4=8) (6=4) (11=4) (2=7) (8=2)

			* Keep sample for occupation-industry-year cell
			if `w' != 2023 {
				keep if occupation == `y' & work_industry == `x' & year == `w'
			}
			if `w' == 2023 {
				keep if occupation == `y' & work_industry == `x' & year >= 2023
			}

			* Estimate mean WFH share (survey-weighted)
			capture svy: mean wfhcovid
			if _rc == 0 {
				ereturn list
				local mean_wfh_cov = e(b)[1,1]

				* Save results into dataset
				clear 
				set obs 1
				gen year = `w'
				gen occupation = `y'
				gen work_industry = `x'
				gen mean_wfh = `mean_wfh_cov'
				append using "$base/wfh_rate_usa_year.dta", nolabel
				save "$base/wfh_rate_usa_year.dta", replace
			}
		}
	}
}

* --------------------------------------------------------------------------
* Construct dataset: mean WFH by occupation x industry (no year split)
* --------------------------------------------------------------------------
clear 
gen occupation = .
save "$base/wfh_rate_usa.dta", replace

foreach x in 1 2 3 4 5 6 10 12 13 14 15 16 18 19 20 22 25 {
	foreach y in 0 1 2 4 5 6 7 8 {

		use "$base/clean_wfhdata.dta", clear

		* Harmonize industry & occupation codes (based on brazilian classifications)
		recode work_industry (2=22) (3=14) (4=5) (5=19) (6=20) (7=12) (8=13) (9=3) (10=2) (11=16) (12=15) (13=6) (14=10) (15=4) (16=6) (17=18) (18=25)
		recode occupation (1=0) (5=1) (9=5) (10=5) (3=6) (4=8) (6=4) (11=4) (2=7) (8=2)

		* Keep sample for given occupation-industry cell
		keep if occupation == `y' & work_industry == `x'

		* Compute mean WFH
		capture svy: mean wfhcovid
		if _rc == 0 {
			ereturn list
			local mean_wfh_cov = e(b)[1,1]

			* Save results
			clear 
			set obs 1
			gen occupation = `y'
			gen work_industry = `x'
			gen mean_wfh = `mean_wfh_cov'
			append using "$base/wfh_rate_usa.dta", nolabel
			save "$base/wfh_rate_usa.dta", replace
		}
	}
}

* --------------------------------------------------------------------------
* Summary stats and descriptive tables
* --------------------------------------------------------------------------
use "$base/clean_wfhdata.dta", clear

* Average WFH rate by low education
svy: mean wfhcovid if education <= 2

* Tabulate education
svy: tab education
outreg2 using "$figures/education_table.tex", replace tex

* Tabulate race/ethnicity
svy: tab race_ethnicity_s
outreg2 using "$figures/etnia.tex", replace tex

* Tabulate gender
svy: tab female 

* Wage stats for non-self-employed
keep if self_employment != .
svy: mean hourly_wage 

* Restrict to low education subsample
keep if education <= 2

* More descriptive stats
svy: mean age
svy: tab female
svy: tab race_ethnicity_s
svy: mean hourly_wage

* WFH rate for non-self-employed
keep if self_employment != .
svy: mean wfhcovid
