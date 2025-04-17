/*---------------------------------------------------------*/
* Example Code for combining Matching with Eventstudy design
* Author: Johannes F. Schmieder
* do matching_eventstudy_example.do
* This version: April 17, 2025
* Version 1.0
/*---------------------------------------------------------*/
set more off
program drop _all
graph drop _all
clear

set seed 190424

/*===========================================================================================*/
/*                                      Main Program                                         */
/*===========================================================================================*/
capture program drop main
program define main
	set matsize 4000
	// set li 120 // 200
	set trace on
	set tracedepth 1

    global log ../log/
	global data ../data/
	cap mkdir $log
	cap mkdir $data

	// confirm file ./ado/
	adopath + ./ado/

	cap log close _all        // make sure log files are closed
	global logfile ${log}matching_eventstudy_example.log
	log using $logfile , text replace name(log)

	// =====================================
	// Simulate Yearly Data
	// =====================================
	simulate_yearly_data, out(${data}person_year_data.dta)

	// =====================================
	// Matching Algorithm to create treatment and control group
	// =====================================

	generate_treatment_control, ///
		in(${data}person_year_data.dta) ///
		out(${data}analysis_data.dta)

	// =====================================
	// Descriptive Analysis
	// =====================================
	descriptive_analysis, in(${data}analysis_data)

	// =====================================
	// Eventstudy Analysis
	// =====================================
	eventstudy_analysis, in(${data}analysis_data)
	

	cap log close log

end // main program

/*===========================================================================================*/
/*                                    Sub Programs                                           */
/*===========================================================================================*/

/*---------------------------------------------*/
/* Simulate Clean Spell Level Data   */
/*---------------------------------------------*/
cap program drop simulate_yearly_data
program define simulate_yearly_data
	syntax, out(str) ///
		[ ///
		startyear(int 1990) ///
		endyear(int 2005) ///
		number_firms(int 100) ///
		number_workers(int 20000) ///
		]

	// Generate firm data - assume firms are constant over time
	clear
	set obs `number_firms'
	g estabid = _n
	  g industry= ceil(uniform()*7)
	  label define industry  ///
		1 "Mining"         ///
		2 "Construction"   ///
		3 "Manufacturing"  ///
		4 "Health"         ///
		5 "Finance"        ///
		6 "FCSL"           ///
		7 "Professional Services"
	  label values industry industry

	  g industryeff = .0*rnormal()
	  replace industryeff = -.2 if industry==6
	  replace industryeff = 0 if industry==7

		g firmsize = rexponential(500)
		g firmeff = industryeff + 0.01 * log(firmsize) + .01*rnormal()

	  isid estabid

	label var estabid "Firm ID within industry - only for simulation"
	label var industry "Industry"
	label var firmsize "Firm size"
	label var firmeff "Cominbed firm effect (ind + size + rand)"
	label var industryeff "Industry effect"

	tempfile firmdata
	save `firmdata'

	// Person data
	clear
	set obs `number_workers'
	g persid = _n
	g female = uniform()<.4
	g edyrs = 8 + floor(uniform()* 8)
	g personfe = rnormal(0,1.5)
	g stateid = round(uniform()*20)

	g byear = 1900 + round(uniform()*100)

	label var female "Female"
	label var edyrs "Years of Schooling"
	label var personfe "Persone Effect"
	label var stateid "State ID"
	label var byear "Birth Year"

	g years_in_data = `endyear' - `startyear' + 1
	expand years_in_data
	bys persid : g year = `startyear' + _n - 1
	g time = year
	label var time "Year"

	isid persid time

	g age = year - byear
	drop if age<25 | age>65

	label var age "Age"

	// Random Firm Assignments
	g estabid = ceil(uniform()*`number_firms')

	// Simulate Plant closings
	g leave_firm = 0
	g mlf = uniform()<.0
	bys estabid time (persid): replace mlf = mlf[1]
	replace leave_firm = uniform()<.5 if mlf==1

	g pcl = uniform()<.02
	bys estabid time (persid): replace pcl = pcl[1]
	replace mlf = 1 if pcl==1 // every PCL is also a MLF
	replace leave_firm = 1 if pcl==1

	label var mlf "Mass lay-off between t-1 and t"
	label var pcl "Plant closing between t-1 and t"
	label var leave_firm "Workers leavs firm between t-1 and t"

	// Set estab to missing to simulate unemployment
	// high chance of unemployment after leaving firm
	replace estabid = . if uniform()<.40 & leave_firm==1 & (mlf==1 | pcl==1)

	// Fix firm transitions to fewer transitions per year
	replace leave_firm = uniform()<.1 if leave_firm==0

	// Job finding rate:
	bys persid (time): replace leave_firm = uniform()<.4 if estabid[_n-1]==.

	bys persid (time): replace estabid  = estabid[_n-1]   if leave_firm==0 & _n>1

	merge m:1  estabid using `firmdata' , keep(master matched) nogen

	g employed = !missing(estabid)
	label var employed "Employed"
	// drop  leave_firm

	g displaced = pcl
	bys persid (time): replace displaced = displaced[_n-1] if _n>1 & displaced[_n]==0

	order persid time estabid pcl leave_firm displaced
	sort persid time

	// Simulate Wage Process
	g logearn = 8.0 + 0.02*(year-2000) + (0.001*(year-1993))^2 ///
		- 0.05 * (year==1993) ///
		- 0.05 * (year==2003) ///
		- female*0.2  ///
		+ edyrs * 0.1  ///
		+ (age-edyrs) * 0.0005 ///
		- (age-edyrs)^2 * 0.0001 ///
		+ rnormal()*0.02*(year-1995) ///
			+ rnormal()*0.03 ///
		- 0.2 * displaced
		

	g earn = exp(logearn)
	replace earn = 0 if employed==. | employed==0

	drop displaced

	label var earn "Earnings"
	label var logearn "Log Earnings"

	bys persid estabid (time): g tenure = (_n-1)
	label var tenure "Tenure with firm"

	order persid year estabid industry

	tsset persid time
	g fpcl = f.pcl
	g fmlf = f.mlf


	g displaced = pcl
	bys persid (time): replace displaced = displaced[_n-1] if _n>1 & displaced[_n]==0

	tsset persid time
	bys persid (displaced) : g sep = displaced[_N]
	
	tsset persid time
	replace displaced = f.displaced if f.displaced!=.

	label var displaced "Post-displacement"
	label var sep "Displaced Worker"
	label define sep1 1 "Displaced worker" 2 "Non-displaced worker"
	label values sep sep1



	bys persid (time): g leave_labor_force = runiform()<.03 
	bys persid (time): replace leave_labor_force = leave_labor_force[_n-1] if _n>1 & leave_labor_force==0
	replace employed = 0 if leave_labor_force==1
	replace earn = 0 if leave_labor_force==1
	replace logearn = . if leave_labor_force==1
	replace displaced = 0 if leave_labor_force==1
	
	
	tsset persid time
	g disp_event = displaced==1 & l.displaced==0

	order persid time estabid industry displaced sep disp_event

	g baselinetime = time if f.disp_event==1

	bys persid (baselinetime) : replace baselinetime = baselinetime[1]
	g timesince = time-baselinetime-1

	// cellgraph logearn  if inrange(timesince,-5,10), by(timesince)
	// cellgraph employed if inrange(timesince,-5,10), by(timesince)


	rename timesince timesince1
	rename displaced displaced1
	rename disp_event disp_event1
	rename sep sep1
	rename baselinetime baselinetime1

	save `out' , replace

end // simulate_yearly_data


/*-------------------------------------------------------*/
/* Create Analysis File with Treatment and Control Group - RAM efficient */
/*-------------------------------------------------------*/
cap program drop generate_treatment_control
program define generate_treatment_control
	syntax , [ ///
    in(str)                   /// Person / year data
    out(str)                  /// Name of Analysis data to be produced
    from(integer 1990)        /// First year for displacement events
    to(integer 2004)          ///  Last year for displacement events
    PREPeriods(integer 5)     ///
    POSTPeriods(integer 10)   ///
    ]

	local totperiods = `postperiods' + `preperiods' + 1

	local matchround 1 // counter for psmatch round, used to create unique match ID

	local cell_vars industry female  // cells within we match treatment and control, we also always match within baseline year / time
	// Add county, use more detailed industry

	// Code for using quarter time units
	// local from = qofd(mdy(1,1,`from'))
	// local to   = qofd(mdy(1,1,`to'))

	// loop over baseline time units; t is the time unit before displacement
	forval t = `from'/`to' {

		// create cells within we us PS matching
		use  persid time displaced `cell_vars' using `in' if time==`t', clear
		// keep if industry==1
		egen cellID = group(`cell_vars')
		tempfile cells`t'
		save `cells`t''

		levelsof cellID , local(cellIDs)

		// Loop over industry  --> could also loop over more cells here to keep data smaller, e.g. states

		foreach cell in `cellIDs' {

			local matchround = `matchround'+1

			use persid time cellID using `cells`t'' if time==`t' & cellID==`cell' , clear

			bys persid: keep if _n==1
			keep persid

			merge 1:m persid using `in' , keep(matched) nogen

			keep if inrange(time,`=`t'-`preperiods'+1',`=`t'+`postperiods'+1')

			tsset persid time

			// -------

			// Displacement Definition
			// Note that fpcl and fmlf are variables that indicate that there is a
			// mass-layoff / plantclosing between t and t+1 in the estab the worker is at t

			// g byte leave_estab = ///
			// estabid<=. ///
			// & estabid != f.estabid 
			// g byte displaced = leave_estab & (fpcl==1 | fmlf==1)
						// Make it so that "displaced" turns on after first displacement
			bys persid (time): replace displaced = displaced[_n-1] if _n>1 & displaced[_n]==0

			// Define displacement_event in quarter before worker moves
			g byte displacement_event = l.displaced==0 & displaced==1

			// we don't want people in control group who move firms from t=-1 to 0:
			g byte mover = estabid != f.estabid & (!missing(estabid)) 
			g byte baselinetime_dummy = time==`t'

			g byte baselinerestrictions = employed==1 & baselinetime_dummy==1 & firmsize>=50 & tenure>=3 & inrange(age,20,55) & l.displaced==0
			replace baselinerestrictions = 0 if baselinerestrictions>=.

			bys persid (baselinerestrictions): replace baselinerestrictions = baselinerestrictions[_N]
			drop if baselinerestrictions==0 // can drop workers who don't satisfy baseline restrictions
			drop baselinerestrictions

			count
			if r(N)==0 continue

			tsset
			g byte sep = (baselinetime_dummy & (displacement_event==1))
			bys persid (sep): replace sep = sep[_N]

			
			// throw out individuals who are not displaced in event, but move firms
			replace sep = 9 if baselinetime_dummy==1 & displacement_event==0 & mover==1 
			
			bys persid (sep): replace sep = sep[_N]
			drop if sep==9

			g uniform = uniform()
			bys persid: replace uniform=uniform[1]
			// drop if uniform>.10 & sep==0 // if few displaced can drop some controls
			drop baselinetime_dummy

			// Create Balanced Panel
			tsset persid time
			tsfill
			g byte noleavers = 1
			tsfill, full

			g byte baselinetime_dummy = time==`t'

			// Have to fill in empty rows now
			foreach v in earn employed  {
				replace `v'=0 if `v'==.
			}
			foreach v in sep byear edyrs female uniform   {
				bys persid (`v'): replace `v'=`v'[1] if `v'==.
			}

			// Generate variables that contain baseline characteristics
			foreach v in earn logearn age edyrs firmsize tenure {
				g baseline`v' = `v' if baselinetime_dummy==1
				bys persid (baseline`v'): replace baseline`v' = baseline`v'[1]
			}

			tsset persid time
			foreach v in logearn {
				tsset
				g baseline`v'1 = `v' if f1.baselinetime_dummy==1
				bys persid (baseline`v'1): replace baseline`v'1 = baseline`v'1[1]
			}

			foreach v in logearn  {
				tsset
				g baseline`v'2 = `v' if f2.baselinetime_dummy==1
				bys persid (baseline`v'2): replace baseline`v'2 = baseline`v'2[1]
			}

			local matchlist baselinelogearn1 baselinelogearn2  baselineage   baselineedyrs  baselinetenure ///
							baselinefirmsize

			// Matching within industries
			
			// Create empty variables that are filled in the following block: 			
			g byte weight1 = .
			g int matchround = .
			g int psmatchid = .

			// This is the matching part within cells.
			// Note that this is encapusalted in 'capture' block since sometimes
			// cells are too small to find match and we don't want the loop to stop
			cap noi {
				psmatch2 sep `matchlist' ///
					if baselinetime_dummy==1 , noreplacement   common

				replace weight1 = 1 if _weight==1
				drop _weight

				replace matchround = `matchround' if _treated==0 | _treated==1
				replace psmatchid = _id if _treated==0

				replace psmatchid = _n1 if _treated==1
				bys persid (psmatchid): replace psmatchid = psmatchid[1]


				bys persid (weight1): replace weight1 = weight1[1]
				replace weight1 = 0 if weight1==.
				keep if weight1==1

				g timesince = time - `t' - 1
				g timesincemov = timesince if sep
				sort persid time
				g baselinetime = `t'

				tempfile matched`matchround'
				save `matched`matchround''
			}
		}
	}

	// Merge matching cells
	local j 1
	forval m = 2/`matchround' {
		cap noi {
			if `j'==1 use `matched`m'', clear
			else append using `matched`m''
			local j 2
		}
	}

	tab matchround
	rename psmatchid psmatchid_temp
	egen psmatchid = group( matchround psmatchid_temp)

	bys psmatchid timesince: g NumMatches = _N

	tab NumMatches

	drop if psmatchid_temp>=.
	drop psmatchid_temp


	label define sep 0 "Non-displaced" 1 "displaced"
	label values sep sep

	save `out' , replace

	cellgraph logearn, by(timesince sep)
	
	// 	err

	// err

end // generate_treatment_control

/*---------------------------------------------*/
/* Descriptive Analysis */
/*---------------------------------------------*/
cap program drop descriptive_analysis
program define descriptive_analysis
	syntax, in(str)

	use `in' , clear

	local file ${log}descriptive_analysis.tex
	latexlog `file': open

	latexlog `file': title "Descriptives for Displacement Analysis Sample"

	latexlog `file': section "Summary Table"
	
	// ====== Summary Tables using Stata's Table Command ======
	
	local vars byear age female edyrs earn logearn  employed firmsize // firmeff
	g N = 1
	label var N "Number of Observations"
	// ====== Summary Table ======
	// Create summary table using Stata's table command
	
		// table (var) (sep) if timesince==-1, ///
		// 	statistic(mean `vars') ///
		// 	statistic(sd `vars') ///
		// 	statistic(count N) ///
		// 	nformat(%9.2f) nototals 
		
		// collect style header result, level(hide)
		// collect style cell result[sd], sformat("[%s]")
		// collect style cell result[count], nformat("%8.0gc")
		// collect style cell result, halign(center)
		// collect style cell var[N], border(top)
	
		// collect label levels sep 0 "Non-displaced" 1 "Displaced", modify
		// collect style header sep, title(hide)
		// // collect style header sep, level(hide)
		// collect preview
		// latexlog `file': collect export , ///
		// 	booktabs novert three  ///
		// 	title(Summary Statistics by Displacement Status) 
		
	// ======  Summary Table with overlapping columns ======
	
	collect clear 	
	
	foreach column in All  Non_Disp Disp {
		if "`column'"=="All"      local cond 1
		if "`column'"=="Non_Disp" local cond sep==0
		if "`column'"=="Disp"     local cond sep==1

		
		egen N_estab = tag(estabid) if timesince==-1 & `cond'
		table (var) if timesince==-1 , ///
			statistic(mean `vars') ///
			statistic(sd `vars') ///
			statistic(count N ) /// 
			statistic(total N_estab) /// 
			nformat(%9.1f) nototals  ///
			name(`column')
		collect addtag group[`column']
		drop N_estab 
	}
		
	// combine collections
	collect combine comb = All Non_Disp Disp

	// specify the order of the group levels (columns)
	collect style autolevels group All Non_Disp Disp
	
	* hide the result labels
	collect style header result[mean sd] title(hide) level(hide)

	// Hide the column group title 
	collect style header group, title(hide) 
	// collect style header group, level(hide)

	// Hide the result labels ("Mean", ...)
	collect style header result, level(hide)

	// Format cells 
	collect style cell result[sd], sformat("[%s]")
	collect style cell result[count], nformat("%8.0gc")
	collect style cell result[total], nformat("%8.0gc")
	collect style cell result, halign(center)
	collect style cell var[N], border(top)

	collect label levels var N_estab "Number of Establishments", modify
	// column labels
	collect label levels group ///
		All "All workers" ///
		Non_Disp "Non-Displaced" ///
		Disp "Displaced" 

	
	// Describe table layout
	collect layout (var#result) (group)
 	collect preview
	latexlog `file': collect export , ///
		booktabs novert three  ///
		title(Summary Statistics by Displacement Status) ///
		notes(Average characteristics of individuals. Standard deviations in brackets.)


	// ====== Tables Summarize Variables with Percentiles =====
	table (var) if timesince==-1, ///
		statistic(p10 `vars') ///
		statistic(p25 `vars') ///
		statistic(p50 `vars') ///
		statistic(p75 `vars') ///
		statistic(p90 `vars') ///
		statistic(mean `vars') ///
		statistic(sd `vars') ///
		statistic(count `vars') ///
		nformat(%9.2f) 

	collect label levels result p10 "10th pct", modify
	collect label levels result p25 "25th pct", modify
	collect label levels result p50 "50th pct", modify
	collect label levels result p75 "75th pct", modify
	collect label levels result p90 "90th pct", modify
	collect label levels result mean "Mean", modify
	collect label levels result sd "SD", modify
	collect label levels result count "N", modify

	collect style cell result[sd], sformat("[%s]")
	collect style cell result[count], nformat("%8.0gc")
	collect style cell result, halign(center)
	collect style cell var[N], border(top)

	collect preview
	latexlog `file': collect export , ///
		booktabs novert three  ///
		title(Summary Statistics with Percentiles) 

	// ====== Table Industry Composition =====
	table (industry) (sep) if timesince==-1, ///
		statistic(percent, across(industry)) ///
		nformat(%9.3f) ///
		totals(sep)
	
	collect style cell result, nformat("%8.1f")
	collect style cell result, halign(center)
	collect style cell var[N], border(top)

	collect label levels sep 0 "Non-displaced" 1 "Displaced", modify
	collect style header sep, title(hide)
	collect style header industry, title(hide)
	// collect style header sep, level(hide)
	collect preview
	latexlog `file': collect export , ///
		booktabs novert three  ///
		title(Industry Distribution by Displacement Status) 

		

	// ====== Tabulate Industry vs. Year =====
	table (industry) (year) if timesince==-1, nototals
	collect style header year, title(hide)
	collect style header industry, title(hide)
	latexlog `file': collect export , ///
		booktabs novert three ///
		title(Number of Workers by Industry and Year) ///
		notes(Each cell shows number of workers per cell)

	// ====== Flexible Summary Table ======
	
	
	latexlog `file': section "Consistency Checks"
	cap mkdir ${log}Consistency/
		
	cellgraph N , by(time) nonotes stat(sum)
	latexlog `file': addfig, file(Consistency/counts_by_time.pdf) eol
	
	cellgraph earn , by(time) nonotes stat(mean)
	latexlog `file': addfig, file(Consistency/earn_by_time.pdf) eol
	
	cellgraph logearn , by(time) nonotes stat(mean)
	latexlog `file': addfig, file(Consistency/logearn_by_time.pdf) eol
	
	cellgraph logearn , by(time) nonotes stat(p10 p25 p50 p75 p90)
	latexlog `file': addfig, file(Consistency/logearn_by_year_pct.pdf) eol
	
	cellgraph employed , by(time) nonotes
	latexlog `file': addfig, file(Consistency/employed_by_time.pdf) eol
	
	cellgraph displaced , by(time) nonotes
	latexlog `file': addfig, file(Consistency/displaced_by_time.pdf) eol


	latexlog `file': section "Treatment and Control around Displacement Event"
	cap mkdir ${log}Disp_event_raw/

	cellgraph N , by(timesince sep) nonotes stat(sum)
	latexlog `file': addfig, file(Disp_event_raw/counts_by_timesince.pdf) eol

	cellgraph earn , by(timesince sep) nonotes
	latexlog `file': addfig, file(Disp_event_raw/logearn_by_timesince.pdf) eol

	cellgraph logearn , by(timesince sep) nonotes
	latexlog `file': addfig, file(Disp_event_raw/earn_by_timesince.pdf) eol

	cellgraph employed , by(timesince sep) nonotes
	latexlog `file': addfig, file(Disp_event_raw/employed_by_timesince.pdf) eol

	cellgraph displaced , by(timesince sep) nonotes
	latexlog `file': addfig, file(Disp_event_raw/displaced_by_timesince.pdf) eol


	latexlog `file': close
	latexlog `file': pdf, view

end // descriptive_analysis


/*-------------------------------------------------------*/
/* Create Analysis File with Treatment and Control Group */
/*-------------------------------------------------------*/
cap program drop eventstudy_analysis
program define eventstudy_analysis
	syntax , [ ///
	in(str)                   /// Person / year data
	from(int -5)  ///  Range of Eventstudy
	to(int 10)   ///
		omit(int -1) /// Omitted category
	]

	use `in' if timesince>=`from', clear

	egen persid2 = group(baselinetime persid)

	qui tab year, gen(_Dyear)
	drop _Dyear1

	qui tab timesince, gen(_DtimesinceTreat)

	// The following is not obvious, but otherwise the values are missing for ctrl group
	foreach v of varlist _Dtime* {
		replace `v' = 0 if sep==0
	}
	local omitdum = `omit'-`from'+1
	drop _DtimesinceTreat`omitdum'
	local omitcat omit(`omit')

	qui tab timesince, gen(_DtimesinceAll)
	// drop _DtimesinceAll1

	g age2 = age^2
	g age3 = age^3
	g age4 = age^4

	local controls  age age2 age3 age4

	local i 100
	local treatment_effect -0.2

	local file ${log}eventstudy_analysis.tex
	cap mkdir ${log}Eventstudy/

	latexlog `file': open

	latexlog `file': title "Eventstudy Analysis for Displacement Events"

	cellgraph logearn, by(timesince sep) title(Raw Means for Disp and Non-Disp Workers)
	latexlog `file': addfig, file(Eventstudy/eventstudy_RawMeans.pdf) eol

	local opt ysize(8) xsize(12) xline(-0.5, lcol(gray)) legend(off)

	reg logearn _DtimesinceTreat* _DtimesinceAll* `controls',
	eventstudy_figure, from(`from') to(`to') treatment_effect(`treatment_effect') name(g`i++') `omitcat' ///
		tit(OLS - Rel. Year Specification) subtit(Control. for Person FE; Year Since Event (for Disp and Non-Disp)) ///
		ytitle(Log Earnings) ///
		`opt'
	latexlog `file': addfig, file(Eventstudy/eventstudy_OLS.pdf) eol

	// --- JLS Specification - DOES NOT WORK ---
	xtreg logearn _DtimesinceTreat* _Dyear* `controls', fe i(persid2)
	eventstudy_figure, from(`from') to(`to') treatment_effect(`treatment_effect') name(g`i++') `omitcat' ///
		tit(JLS Specification) subtit(Controlling for Year and Person FE) ///
		ytitle(Log Earnings) ///
		`opt'
	latexlog `file': addfig, file(Eventstudy/eventstudy_FE_JLS.pdf) eol

	// --- Rel Year Specification ---
	xtreg logearn _DtimesinceTreat* _DtimesinceAll* `controls',  fe i(persid2)
	eventstudy_figure, from(`from') to(`to') treatment_effect(`treatment_effect') name(g`i++') `omitcat' ///
		tit(Rel. Year Specification) subtit(Control. for Person FE; Year Since Event (for Disp and Non-Disp)) ///
		ytitle(Log Earnings) ///
		`opt'
	latexlog `file': addfig, file(Eventstudy/eventstudy_FE_RelYear.pdf) eol

	// --- Schmieder / von Wachter / Heining Specification ---
	xtreg logearn _DtimesinceTreat* _Dyear* _DtimesinceAll* `controls', fe i(persid2)
	eventstudy_figure, from(`from') to(`to') treatment_effect(`treatment_effect') name(g`i++') `omitcat' ///
		tit(SWH Specification) subtit(Control. for Person FE; Year Since Event (for Disp and Non-Disp) and Year FE) ///
		ytitle(Log Earnings) ///
		`opt'
	latexlog `file': addfig, file(Eventstudy/eventstudy_FE_Full.pdf) eol

	// --- Schmieder / von Wachter / Heining Specification ---
	xtreg employed _DtimesinceTreat* _Dyear* _DtimesinceAll* `controls', fe i(persid2)
	eventstudy_figure, from(`from') to(`to')  name(g`i++') `omitcat' ///
		tit(SWH Specification) subtit(Outcome: Employment) ///
		ytitle(Employed) ///
		`opt'
	latexlog `file': addfig, file(Eventstudy/eventstudy_FE_Full_employed.pdf) eol

	latexlog `file': close
	latexlog `file': pdf, view

end // eventstudy_analysis

/*-------------------------------------------------------*/
/* General Code for Eventstudy Figure */
/*-------------------------------------------------------*/
cap program drop eventstudy_figure
program define eventstudy_figure
  syntax , * ///
		from(integer) /// Range of Eventstudy
		to(integer)   ///
		[ ///
		OMITcat(str) ///
		treatment_effect(str)  ]


	if "`omitcat'"=="" local omitcat = `from'

	preserve

	local totperiods = `to'-(`from') + 1

	di `totperiods'

	local ndummies = `totperiods'-1
	capture matrix drop coef coef_ev
	capture matrix drop cov cov_ev
	matrix coef = e(b)'
	matrix cov = vecdiag(e(V))'
	matrix coef_ev = coef[1..`ndummies',1]
	matrix cov_ev = cov[1..`ndummies',1]

	matrix coef = coef_ev, cov_ev

	drop _all
	svmat double coef
	keep in 1/`ndummies'

	gen timesince = (_n+`from')

	// insert omitted category
	expand 2 if _n==1
	replace coef1  = 0 if _n==1
	replace coef2  = 0 if _n==1
	replace timesince = `omitcat' if _n==1
	if `omitcat'>`from' {
		replace timesince = timesince-1 if timesince<=`omitcat' & _n>1
	}

	rename coef1 coef_ev
	rename coef2 cov_ev
	gen ci_hi = coef_ev + 2*sqrt(cov_ev)
	gen ci_lo = coef_ev - 2*sqrt(cov_ev)

	sort timesince


	// check if "true" treatment effect (in case of simulated data) is provided:
	if `"`treatment_effect'"'!="" {
		local xpos = 0.2 * `to'
		local ypos = 1.2 * `treatment_effect'
		local treatment_text text(`ypos' `xpos'  "True Treatment Eff. according to DGP", placement(right) col(gray))

		g true_eff = 0 if timesince<0
		replace true_eff = `treatment_effect' if timesince>=0

		local true_eff_line    ///
		  (line true_eff timesince if timesince<0, lcol(gray) lpat(-)) ///
		  (line true_eff timesince if timesince>=0, lcol(gray) lpat(-))
	}

	local col dknavy 
	local ciopacity 20
	twoway /// 
		(rarea ci_hi ci_lo timesince, color("`col' % `ciopacity'")) ///
		(line coef_ev timesince, color("`col'")) ///
		`true_eff_line' ///
		, ///
		legend(label(1 "Treatment effect in year since treatment")  order(1)) ///
		legend(region(lcolor(white))) legend(pos(6)) ///
		graphr(color(white)) xtitle(Year relative to treatment) ///
		`treatment_text' ///
		`options'

	restore

end // eventstudy_figure


/*---------------------------------------------------------*/
/* Run Main Program */
/*---------------------------------------------------------*/
main // run main program


/*========================================= END ==============================================*/
