/********************************************************************
Project : Job Market Paper – Inflation Inequality
Author  : Sabyasachi C. Panda
Purpose : Data preparation, construction of expenditure variables,
          sector classification, diagnostics, and IV estimation
*********************************************************************/

*---------------------------------------------------------------*
* 1. Load data
*---------------------------------------------------------------*
cd "D:\Papers_and_Books\AUD\Dissertation\Data"
use "main_filtered.dta", clear

*---------------------------------------------------------------*
* 2. Keep relevant item codes
*---------------------------------------------------------------*
keep if inlist(item_code, ///
129,139,159,179,169,219,239,249,199,189,269,279,289,299, ///
349,459,479,409,419,429,519,499,439,529,539,899, ///
309,319,329,379,399,389,629,609,099,619,599,579,559,569,639,649)

*---------------------------------------------------------------*
* 3. Construct standardized monthly consumption
*---------------------------------------------------------------*
gen monthly_cons = .

* 30-day reference items
replace monthly_cons = consumption_value if inlist(item_code, ///
129,139,159,179,169,219,239,249,199,189,269,279,289,299, ///
349,459,479,429,519,499,439,529,539)

* 7-day reference: multiply by 30/7
replace monthly_cons = consumption_value * (30/7) if inlist(item_code, 309,319,329)

* 365-day reference: convert to monthly
replace monthly_cons = consumption_value * (30/365) if inlist(item_code, ///
409,419,899,379,399,389,629,609,099,619,599,579,559,569,639,649)

* Total expenditure per household
bysort hhid: egen tot_monthly_exp = total(monthly_cons)
gen ln_tot_monthly_exp = log(tot_monthly_exp)

label variable monthly_cons        "Monthly Consumption (Standardized)"
label variable tot_monthly_exp     "Total Monthly Household Expenditure"

*---------------------------------------------------------------*
* 4. Sector classification (Agriculture / Manufacturing / Services)
*---------------------------------------------------------------*
recode item_code ///
(129 139 159 169 219 239 249 199 269 309 319 179 189 = 1 "Agriculture") ///
(299 459 479 329 379 399 389 609 099 599 579 559 569 649 = 2 "Manufacturing") ///
(289 349 409 419 429 519 499 439 529 539 899 639 629 619 = 3 "Services"), ///
gen(sector_num)

label define sector_lbl 1 "Agriculture" 2 "Manufacturing" 3 "Services"
label values sector_num sector_lbl

*---------------------------------------------------------------*
* 5. Expenditure by sector per household
*---------------------------------------------------------------*
bysort hhid: egen food_exp = total(monthly_cons * (sector_num==1))
bysort hhid: egen manu_exp = total(monthly_cons * (sector_num==2))
bysort hhid: egen serv_exp = total(monthly_cons * (sector_num==3))

*---------------------------------------------------------------*
* 6. Collapse to household level dataset
*---------------------------------------------------------------*
collapse (max) food_exp manu_exp serv_exp tot_monthly_exp ln_tot_monthly_exp ///
(firstnm) survey_name year fsu sector state nss_region district ///
stratum sub_stratum panel sub_sample ///
b1q1pt7 b1q1pt10 b4q4pt3 b1q1pt11 b1q1pt12 questionaire_no mult ///
item_code consumption_value hhsize monthly_cons sector_num, by(hhid)

*---------------------------------------------------------------*
* 7. Construct occupation-based work category
*---------------------------------------------------------------*
gen work = .
replace work = 1 if inlist(b4q4pt3, 611,612,613,621,622,631,632,633,634,921)   // Agriculture
replace work = 2 if inlist(b4q4pt3, 711,712,713,721,722,723,731,732,741,742, ///
751,752,753,754,811,812,813,814,815,816,817,818,821,831,832,833,834,835,932,933)
replace work = 3 if missing(work) & !missing(b4q4pt3)

label define work_lbl 1 "Agriculture" 2 "Manufacturing" 3 "Services"
label values work work_lbl

*---------------------------------------------------------------*
* 8. Outlier removal based on 3 SD rule
*---------------------------------------------------------------*
egen z_tot = std(tot_monthly_exp)
drop if abs(z_tot) > 3

*---------------------------------------------------------------*
* 9. Expenditure shares (log-relative)
*---------------------------------------------------------------*
gen rel_food = log(food_exp / manu_exp)
gen rel_serv = log(serv_exp / manu_exp)

*---------------------------------------------------------------*
* 10. Leave-one-out diagnostic regressions
*---------------------------------------------------------------*
gen y_loo_food = tot_monthly_exp - food_exp
gen ln_y_loo_food = log(y_loo_food)
gen ln_food_exp  = log(food_exp)

reg ln_food_exp ln_y_loo_food
predict res_food, resid

gen y_loo_serv = tot_monthly_exp - serv_exp
gen ln_y_loo_serv = log(y_loo_serv)
gen ln_serv_exp  = log(serv_exp)

reg ln_serv_exp ln_y_loo_serv
predict res_serv, resid

gen y_loo_manu = tot_monthly_exp - manu_exp
gen ln_y_loo_manu = log(y_loo_manu)
gen ln_manu_exp  = log(manu_exp)

reg ln_manu_exp ln_y_loo_manu
predict res_manu, resid

*---------------------------------------------------------------*
* 11. Fixed effects construction
*---------------------------------------------------------------*
egen pi_rk = group(state district sector), label
egen hh_char = group(hhsize gender_head Religion social_group)

*---------------------------------------------------------------*
* 12. First stage (instrument relevance)
*---------------------------------------------------------------*
reg ln_tot_monthly_exp b4q4pt6 food_exp hh_char pi_rk

*---------------------------------------------------------------*
* 13. IV estimation (2SLS): Relative Food Share
*---------------------------------------------------------------*

* Baseline weights
foreach w in mult exp_weight {
    di "--------------- Using weight: `w' ---------------"
    foreach g in 1 2 3 {
        ivregress 2sls rel_food (ln_tot_monthly_exp = b4q4pt6) ///
            i.sector pi_rk hh_char if work == `g' [aweight=`w']
    }
}

*---------------------------------------------------------------*
* 14. IV estimation (2SLS): Relative Service Share
*---------------------------------------------------------------*
foreach w in mult exp_weight {
    di "--------------- Using weight: `w' ---------------"
    foreach g in 1 2 3 {
        ivregress 2sls rel_serv (ln_tot_monthly_exp = b4q4pt6) ///
            i.sector pi_rk hh_char if work == `g' [aweight=`w']
    }
}

*****************************************************************
* End of Code
*****************************************************************
