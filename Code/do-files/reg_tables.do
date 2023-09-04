clear all
set more off

*ssc install estout
*********************
* FOLLOW THESE STEPS
*********************

dis "`c(username)'"

// Set WD

if "`c(username)'" == "HDC" {
	cd "C:\Users\kingd\OneDrive\uon-msc-thesis"
}

if "`c(username)'" == "LLR User" {
	cd "C:\Users\LLR User\OneDrive\3_Work_Research\uon-msc-thesis"
}

**# 1. Call do files

//qui do "Code/do-files/reg_panel_sampled_fips.do"

**# 2. Make tables

// Nếu đã chạy xong do file ở trên rồi mà muốn chỉnh sửa lại bảng thì các lần chạy sau chỉ cần chạy phần esttab thôi cho nhẹ.

**# --- Baseline

esttab bl1 bl2 bl3 bl_ir1 bl_ir2 bl_ir3 ///
	using "Writing/Results/tab-baseline-State.rtf", ///
	eqlabels("") ///
	title(Baseline results) ///
		label b(3) long se nogaps nodep interaction(" * ") star(* 0.10 ** 0.05 *** 0.01) ///
		s(yfe sfe ysfe lfe N r2_a, label("Year FE" "State FE" "Year*State FE" "Lender FE" "No. of Obs."  "Adj R2" ) fmt(%3s %3s %3s %3s %12.0gc 3 3 3 %5.0fc)) ///
		keep(ln_carc_releases 3.purpose 1.bank loan_to_value_ratio age c.age#c.age ln_income c.ln_income#c.ln_income 2.gender 2.race 3.race 4.race ln_property_value 2.metro_dummy aland_cou houden_cou cnty_unemp_rate _cons) ///
		order(ln_carc_releases 3.purpose 1.bank loan_to_value_ratio age c.age#c.age ln_income c.ln_income#c.ln_income 2.gender 2.race 3.race 4.race ln_property_value 2.metro_dummy aland_cou houden_cou cnty_unemp_rate _cons) ///
		nonote addnote("All continuous variables are log-transformed." "Standard errors in parentheses." "* p $<$ 0.10, ** p $<$ 0.05, *** p $<$ 0.01.") ///
		replace
// 		coeflabels(ln_carc_releases "XXX" ln_carc_onsite "YYY" 3.purpose 1.bank loan_to_value_ratio age c.age#c.age ln_income 2.gender 3.gender 2.race 3.race 4.race ln_property_value 2.metro_dummy ln_total_releases aland_cou houden_cou cnty_unemp_rate _cons)
		
**# --- Robustness
esttab rb1 rb2 rb3 rb_ir1 rb_ir2 rb_ir3 ///
	using "Writing/Results/tab-robust-State.rtf", ///
	eqlabels("") ///
	title(Robustness check results) ///
		label b(3) long se nogaps nodep interaction(" X ") star(* 0.10 ** 0.05 *** 0.01) ///
		s(yfe sfe ysfe lfe N r2_a, label("Year FE" "State FE" "Year*State FE" "Lender FE" "No. of Obs."  "Adj R2" ) fmt(%3s %3s %3s %3s %12.0gc 3 3 3 %5.0fc)) ///		
		keep(ln_carc_air 3.purpose 1.bank loan_to_value_ratio age c.age#c.age ln_income 2.gender 3.gender 2.race 3.race 4.race ln_property_value 2.metro_dummy ln_total_releases aland_cou houden_cou cnty_unemp_rate) ///
		order(ln_carc_air 3.purpose 1.bank loan_to_value_ratio age c.age#c.age ln_income 2.gender 3.gender 2.race 3.race 4.race ln_property_value 2.metro_dummy ln_total_releases aland_cou houden_cou cnty_unemp_rate) ///
		nonote addnote("All continuous variables are log-transformed." "Standard errors in parentheses." "* p $<$ 0.10, ** p $<$ 0.05, *** p $<$ 0.01.") ///
		replace 
		///
// 		coeflabels(ln_carc_releases "XXX" 3.purpose 1.bank loan_to_value_ratio age c.age#c.age ln_income 2.gender 3.gender 2.race 3.race 4.race ln_property_value 2.metro_dummy ln_total_releases aland_cou houden_cou cnty_unemp_rate _cons)
		
**# --- Additional regs
**### Hi-Lo Carc Exposure
// esttab hilo1 hilo2 hilo3 hilo4 ///
// 	using "Writing/Results/tab-addition-State-hilo.rtf", ///
// 	eqlabels("") ///
// 	title(Additional regression results - Interaction with Purpose) ///
// 		label b(3) long se nogaps nodep interaction(" * ") star(* 0.10 ** 0.05 *** 0.01) ///
// 		s(ysfe lfe N r2_a, label("Year*State FE" "Lender FE" "No. of Obs." "Adj R2" ) fmt(%3s %3s %12.0gc 3)) ///
// 		keep(1.carc_dummy 1.carc_dummy 3.purpose 1.bank loan_to_value_ratio age c.age#c.age ln_income c.ln_income#c.ln_income 2.gender  2.race 3.race 4.race ln_property_value 2.metro_dummy aland_cou houden_cou cnty_unemp_rate) ///
// 		order(1.carc_dummy 1.carc_dummy 3.purpose 1.bank loan_to_value_ratio age c.age#c.age ln_income c.ln_income#c.ln_income 2.gender  2.race 3.race 4.race ln_property_value 2.metro_dummy aland_cou houden_cou cnty_unemp_rate ) ///
// 		nonote addnote("All continuous variables are log-transformed." "Standard errors in parentheses." "* p $<$ 0.10, ** p $<$ 0.05, *** p $<$ 0.01.") ///
// 		replace

**### Purpose
esttab itr_purp1 itr_purp2 itr_purp3 itr_purp4 ///
	using "Writing/Results/tab-addition-State-purpose.rtf", ///
	eqlabels("") ///
	title(Additional regression results - Interaction with Purpose) ///
		label b(3) long se nogaps nodep interaction(" * ") star(* 0.10 ** 0.05 *** 0.01) ///
		s(ysfe lfe N r2_a, label("Year*State FE" "Lender FE" "No. of Obs." "Adj R2" ) fmt(%3s %3s %12.0gc 3)) ///
		keep(ln_carc_releases c.ln_carc_releases#3.purpose 3.purpose 1.bank loan_to_value_ratio age c.age#c.age ln_income c.ln_income#c.ln_income 2.gender  2.race 3.race 4.race ln_property_value 2.metro_dummy aland_cou houden_cou cnty_unemp_rate ) ///
		order(ln_carc_releases c.ln_carc_releases#3.purpose  3.purpose 1.bank loan_to_value_ratio age c.age#c.age ln_income c.ln_income#c.ln_income 2.gender  2.race 3.race 4.race ln_property_value 2.metro_dummy aland_cou houden_cou cnty_unemp_rate ) ///
		nonote addnote("All continuous variables are log-transformed." "Standard errors in parentheses." "* p $<$ 0.10, ** p $<$ 0.05, *** p $<$ 0.01.") ///
		replace
		
**### Bank
esttab itr_bank1 itr_bank2 itr_bank3 itr_bank4 ///
	using "Writing/Results/tab-addition-State-bank.rtf", ///
	eqlabels("") ///
	title(Additional regression results - Interaction with Bank) ///
		label b(3) long se nogaps nodep interaction(" * ") star(* 0.10 ** 0.05 *** 0.01) ///
		s(ysfe lfe N r2_a, label("Year*State FE" "Lender FE" "No. of Obs." "Adj R2" ) fmt(%3s %3s %12.0gc 3)) ///
		keep(ln_carc_releases c.ln_carc_releases#1.bank  1.bank 3.purpose loan_to_value_ratio age c.age#c.age ln_income c.ln_income#c.ln_income 2.gender  2.race 3.race 4.race ln_property_value 2.metro_dummy  aland_cou houden_cou cnty_unemp_rate ) ///
		order(ln_carc_releases c.ln_carc_releases#1.bank 1.bank 3.purpose loan_to_value_ratio age c.age#c.age ln_income c.ln_income#c.ln_income 2.gender  2.race 3.race 4.race ln_property_value 2.metro_dummy  aland_cou houden_cou cnty_unemp_rate ) ///
		nonote addnote("All continuous variables are log-transformed." "Standard errors in parentheses." "* p $<$ 0.10, ** p $<$ 0.05, *** p $<$ 0.01.") ///
		replace
		
**### Black
esttab itr_race1 itr_race2 itr_race3 itr_race4 ///
	using "Writing/Results/tab-addition-State-race.rtf", ///
	eqlabels("") ///
	title(Additional regression results - Interaction with Bank) ///
		label b(3) long se nogaps nodep interaction(" * ") star(* 0.10 ** 0.05 *** 0.01) ///
		s(yfe sfe ysfe N r2_a, label("Year FE" "State FE" "Year*State FE" "No. of Obs." "Adj R2" ) fmt(%3s %3s %3s %12.0gc 3)) ///
		keep(ln_carc_releases c.ln_carc_releases#1.black 1.black 3.purpose 1.bank loan_to_value_ratio age c.age#c.age ln_income c.ln_income#c.ln_income 2.gender 2.race 4.race ln_property_value 2.metro_dummy aland_cou houden_cou cnty_unemp_rate _cons) ///
		order(ln_carc_releases c.ln_carc_releases#1.black 1.black 3.purpose 1.bank loan_to_value_ratio age c.age#c.age ln_income c.ln_income#c.ln_income 2.gender 2.race 4.race ln_property_value 2.metro_dummy aland_cou houden_cou cnty_unemp_rate _cons) ///
		nonote addnote("All continuous variables are log-transformed." "Standard errors in parentheses." "* p $<$ 0.10, ** p $<$ 0.05, *** p $<$ 0.01.") ///
		replace

**# --- Matching

// run matching.do manually!!!

esttab psm nnm ra ///
	using "Writing/Results/tab-matching.rtf", ///
	eqlabels("") ///
	title(Matching results) ///
		label b(3) long se nogaps nodep interaction(" X ") star(* 0.10 ** 0.05 *** 0.01) ///
		s(yfe sfe ysfe lfe N n_treat Mean, label("Year FE" "State FE" "Year X State FE" "Lender FE" "No. of Obs." "No. of Treated" "Outcome mean") fmt(%3s %3s %3s %3s %12.0gc %12.0gc 3)) ///
		keep(r1vs0.carc_on2level) ///
		order(r1vs0.carc_on2level) ///
		nonote addnote("All continuous variables are log-transformed." "Standard errors in parentheses." "* p $<$ 0.10, ** p $<$ 0.05, *** p $<$ 0.01.") ///
		replace ///
// 		coeflabels(1.carc_dummy "XXX" 3.purpose "" 1.carc_dummy#3.purpose 1.carc_dummy#1.bank 1.bank loan_to_value_ratio age c.age#c.age ln_income 2.gender 3.gender 2.race 3.race 4.race ln_property_value 2.metro_dummy ln_total_releases aland_cou houden_cou cnty_unemp_rate _cons)
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		