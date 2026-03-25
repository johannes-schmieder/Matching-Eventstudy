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
	// simulate_yearly_data, out(${data}person_year_data.dta)

	// =====================================
	// Matching Algorithm to create treatment and control group
	// =====================================
	// generate_treatment_control, ///
	// 	in(${data}person_year_data.dta) ///
	// 	out(${data}analysis_data.dta)

	// =====================================
	// Descriptive Analysis
	// =====================================
	// descriptive_analysis, in(${data}analysis_data)

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
		number_workers(int 40000) ///
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

	// Spouse demographics (assigned once per person, constant over time)
	g spouse_female = 1 - female
	g spouse_byear = byear + round(rnormal(0, 3))
	g spouse_edyrs = edyrs + round(rnormal(0, 2))
	replace spouse_edyrs = 8 if spouse_edyrs < 8
	replace spouse_edyrs = 16 if spouse_edyrs > 16
	g spouse_personfe = rnormal(0, 1.5)
	label var spouse_female "Spouse Female"
	label var spouse_byear "Spouse Birth Year"
	label var spouse_edyrs "Spouse Years of Schooling"
	label var spouse_personfe "Spouse Person Effect"

	g years_in_data = `endyear' - `startyear' + 1
	expand years_in_data
	bys persid : g year = `startyear' + _n - 1
	g time = year
	label var time "Year"

	isid persid time

	g age = year - byear
	drop if age<25 | age>65

	label var age "Age"

	g spouse_age = year - spouse_byear
	label var spouse_age "Spouse Age"

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

	label var earn "Earnings"
	label var logearn "Log Earnings"

	// Simulate Spouse Earnings Process
	// Spouse earnings have a similar structure but are independent of the main worker
	// Note: displaced is still available here (created above, dropped after spouse code)

	// Dynamic spouse employment process with entry/exit each period
	// Steady-state: entry/(entry+exit) = 0.08/0.13 ~ 0.62 employed
	sort persid time
	bys persid (time): g spouse_employed = (uniform() > 0.40) if _n == 1
	// Transition each period: exit rate 5%, entry rate 8% (+ 12% AWE for displaced)
	bys persid (time): replace spouse_employed = ///
		cond(spouse_employed[_n-1] == 1, ///
			(uniform() > 0.05), ///
			(uniform() < 0.08 + 0.12 * displaced)) ///
		if _n > 1

	// Generate potential spouse log earnings (for all periods)
	g spouse_logearn = 7.5 + 0.02*(year-2000) + (0.001*(year-1993))^2 ///
		- spouse_female*0.2  ///
		+ spouse_edyrs * 0.1  ///
		+ (spouse_age - spouse_edyrs) * 0.0005 ///
		- (spouse_age - spouse_edyrs)^2 * 0.0001 ///
		+ spouse_personfe * 0.3 ///
		+ rnormal()*0.02*(year-1995) ///
		+ rnormal()*0.03

	// AWE intensive margin: employed spouses of displaced workers earn slightly more
	replace spouse_logearn = spouse_logearn + 0.05 * displaced

	// Set earnings based on employment status
	g spouse_earn = exp(spouse_logearn)
	replace spouse_earn = 0 if spouse_employed == 0
	replace spouse_logearn = . if spouse_employed == 0

	label var spouse_logearn "Spouse Log Earnings"
	label var spouse_earn "Spouse Earnings"
	label var spouse_employed "Spouse Employed"

	drop displaced

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

	local cell_vars industry female spouse_baseline_employed // cells within we match treatment and control, we also always match within baseline year / time
	// Add county, use more detailed industry

	// Code for using quarter time units
	// local from = qofd(mdy(1,1,`from'))
	// local to   = qofd(mdy(1,1,`to'))

	// loop over baseline time units; t is the time unit before displacement
	forval t = `from'/`to' {

		// Create spouse_baseline_employed at time t (the matching baseline)
		// This ensures exact matching on spouse employment at the actual baseline period
		use  persid time displaced industry female spouse_employed using `in' if time==`t', clear
		g spouse_baseline_employed = spouse_employed
		replace spouse_baseline_employed = 0 if missing(spouse_baseline_employed)
		// keep if industry==1
		egen cellID = group(`cell_vars')
		tempfile cells`t'
		save `cells`t''

		levelsof cellID , local(cellIDs)

		// Loop over industry  --> could also loop over more cells here to keep data smaller, e.g. states

		foreach cell in `cellIDs' {

			local matchround = `matchround'+1

			use persid time cellID spouse_baseline_employed using `cells`t'' if time==`t' & cellID==`cell' , clear

			bys persid: keep if _n==1
			keep persid spouse_baseline_employed
			tempfile cellpersons
			save `cellpersons'

			keep persid
			merge 1:m persid using `in' , keep(matched) nogen
			// Bring in spouse_baseline_employed from the cell file
			merge m:1 persid using `cellpersons', keep(matched) nogen

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

			count
			if r(N)==0 continue

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
			foreach v in earn employed spouse_earn spouse_employed {
				replace `v'=0 if `v'==.
			}
			foreach v in sep byear edyrs female uniform spouse_female spouse_byear spouse_edyrs spouse_baseline_employed {
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

			// Generate baseline spouse log earnings for matching
			g baseline_spouse_logearn = spouse_logearn if baselinetime_dummy==1
			bys persid (baseline_spouse_logearn): replace baseline_spouse_logearn = baseline_spouse_logearn[1]
			replace baseline_spouse_logearn = 0 if missing(baseline_spouse_logearn)

			local matchlist baselinelogearn1 baselinelogearn2  baselineage   baselineedyrs  baselinetenure ///
							baselinefirmsize baseline_spouse_logearn

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
				bys persid (matchround): replace matchround = matchround[1] 

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


	egen double pers_matchround_id = group(persid matchround)
	label var pers_matchround_id "Person-Matchround ID"

	label define sep 0 "Non-displaced" 1 "Displaced"
	label values sep sep

	label var timesince "Years relative to displacement"

	g baseline_earnings = earn if timesince==-1
	bys pers_matchround_id (baseline_earnings): replace baseline_earnings = baseline_earnings[1]
    g earnings_growth = (earn - baseline_earnings) / baseline_earnings
	label var earnings_growth "Earnings Growth (%)"

	// Spouse outcome variables
	g spouse_baseline_earnings = spouse_earn if timesince==-1
	bys pers_matchround_id (spouse_baseline_earnings): replace spouse_baseline_earnings = spouse_baseline_earnings[1]
	g spouse_earnings_growth = (spouse_earn - spouse_baseline_earnings) / spouse_baseline_earnings
	label var spouse_baseline_earnings "Spouse Baseline Earnings"
	label var spouse_earnings_growth "Spouse Earnings Growth (%)"

	// Spouse earnings growth relative to household income (defined even when spouse baseline earnings are zero)
	g baseline_hh_earnings = baseline_earnings + spouse_baseline_earnings
	g spouse_earnings_growth_hh = (spouse_earn - spouse_baseline_earnings) / baseline_hh_earnings
	label var spouse_earnings_growth_hh "Spouse Earnings Growth (% of HH Income)"

	// Spouse earnings growth relative to partner (displaced worker) baseline earnings
	g spouse_earnings_growth_partner = (spouse_earn - spouse_baseline_earnings) / baseline_earnings
	label var spouse_earnings_growth_partner "Spouse Earnings Growth (% of Partner Baseline Earnings)"

	// Log transformations with different constants (to illustrate Chen and Roth, QJE 2024)
	g spouse_logearn_001 = log(spouse_earn + 0.01)
	label var spouse_logearn_001 "Spouse log(earnings + 0.01)"

	g spouse_logearn_1 = log(spouse_earn + 1)
	label var spouse_logearn_1 "Spouse log(earnings + 1)"

	g spouse_logearn_100 = log(spouse_earn + 100)
	label var spouse_logearn_100 "Spouse log(earnings + 100)"

	save `out' , replace

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

	latexlog `file': section "Summary Statistics"
	
	// ====== Summary Tables using Stata's Table Command ======
	
	local vars byear age female edyrs earn logearn  employed firmsize // firmeff
	g N = 1
	label var N "Number of Observations"
	// ====== Summary Table ======
	latexlog `file': writeln "Start by clearly defininng your sample and summarizing the baseline characteristics."
	latexlog `file': writeln "Stata's table command is used to create the tables in this section.\\\\"
	latexlog `file': writeln "Here is a standard summary table, showing sample characteristics in"
	latexlog `file': writeln "the baseline year (year prior to event, both treatment and control) over time."
	
	table (var) (year) if timesince==-1 & inlist(year,1993,1999, 2004), ///
		statistic(mean `vars') ///
		statistic(sd `vars') ///
		statistic(count N) ///
		nformat(%9.2f) nototals 
	
	collect style header result, level(hide)
	collect style cell result[sd], sformat("[%s]")
	collect style cell result[count], nformat("%8.0gc")
	collect style cell result, halign(center)
	collect style cell var[N], border(top)

	// collect label levels sep 0 "Non-displaced" 1 "Displaced", modify
	collect style header year, title(hide)
	// collect style header sep, level(hide)
	collect preview
	latexlog `file': collect export , ///
		booktabs novert three  ///
		title(Summary Statistics in Baseline Year over Time) 
		
	// ======  Summary Table with overlapping columns ======
	latexlog `file': writeln "\clearpage"
	latexlog `file': writeln "Next, we create a summary table with overlapping columns (that is not mutually exclusive), showing"
	latexlog `file': writeln "the sample characteristics of the all workers as well as the treatment and control groups."
	
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
	latexlog `file': writeln "\\\\ Next, we create a table summarizing the distribution of the variables of interest."
	latexlog `file': writeln "We report the 10th, 25th, 50th, 75th, and 90th percentiles, min, max, as well as the mean and standard deviation."
	latexlog `file': writeln "Looking at details like this can often catch problems with the data cleaning or the data itself."
	table (var) if timesince==-1, ///
		statistic(p10 `vars') ///
		statistic(p25 `vars') ///
		statistic(p50 `vars') ///
		statistic(p75 `vars') ///
		statistic(p90 `vars') ///
		statistic(min `vars') ///
		statistic(max `vars') ///
		statistic(mean `vars') ///
		statistic(sd `vars') ///
		statistic(count `vars') ///
		nformat(%9.2f) 

	collect label levels result p10 "10th pct", modify
	collect label levels result p25 "25th pct", modify
	collect label levels result p50 "50th pct", modify
	collect label levels result p75 "75th pct", modify
	collect label levels result p90 "90th pct", modify
	collect label levels result min "Min", modify
	collect label levels result max "Max", modify
	collect label levels result mean "Mean", modify
	collect label levels result sd "SD", modify
	collect label levels result count "N", modify

	collect style cell result[sd], sformat("[%s]")
	collect style cell result[count], nformat("%8.0gc")
	collect style cell result, halign(center)
	collect style cell var[N], border(top)

	collect preview
	latexlog `file': collect export , ///
		booktabs novert three fontsize(small) ///
		title(Summary Statistics with Percentiles) 

	// ====== Table Industry Composition =====	
	latexlog `file': writeln "\\\\ Next, we create a table showing the industry composition of the sample."
	latexlog `file': writeln "This is a simple table showing the percentage of workers in each industry for the treatment and control groups."
	latexlog `file': writeln "This should be perfectly balanced by construction but it is a good check to make sure that the matching is working."
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
	latexlog `file': writeln "\\\\ Next, we create descriptive figures showing the evolution of some key variables by year."
	
	latexlog `file': section "Consistency Checks"
	cap mkdir ${log}Consistency/
		
	latexlog `file': subfigure, open title("Consistency Checks")

	cellgraph N , by(time) nonotes stat(sum) title(" ")
	latexlog `file': subfigure, addfig file(Consistency/counts_by_time.pdf) ///
		caption("Counts by Year") width(0.45)

	cellgraph earn , by(time) nonotes stat(mean) title(" ") legend(off)
	latexlog `file': subfigure, addfig file(Consistency/earn_by_time.pdf) ///
		caption("Earnings by Year") width(0.45) eol

	cellgraph logearn , by(time) nonotes stat(mean) title(" ") legend(off)
	latexlog `file': subfigure, addfig file(Consistency/logearn_by_time.pdf) ///
		caption("Log Earnings by Year") width(0.45)

	cellgraph logearn , by(time) nonotes stat(p10 p25 p50 p75 p90) title(" ")
	latexlog `file': subfigure, addfig file(Consistency/logearn_by_year_pct.pdf) ///
		caption("Log Earnings Percentiles by Year") width(0.45) eol

	cellgraph employed , by(time) nonotes title(" ") legend(off)
	latexlog `file': subfigure, addfig file(Consistency/employed_by_time.pdf) ///
		caption("Employment by Year") width(0.45)

	cellgraph displaced , by(time) nonotes title(" ") legend(off)
	latexlog `file': subfigure, addfig file(Consistency/displaced_by_time.pdf) ///
		caption("Displaced by Year") width(0.45) eol

	latexlog `file': subfigure, close

	latexlog `file': section "Treatment and Control around Displacement Event"

	latexlog `file': writeln "Now we create a set of figures showing the treatment and control groups around the displacement event."
	latexlog `file': writeln "\\\\ Given the matching design, this already allows us to get a good sense of the treatment effect."
	latexlog `file': writeln "\\\\ Looking at the raw means like this is often a much better way to understand "
	latexlog `file': writeln "what is happening then going straight to the eventstudy."

	cap mkdir ${log}Disp_event_raw/

	latexlog `file': subfigure, open title("Treatment and Control around Displacement Event")

	cellgraph N , by(timesince sep) nonotes stat(sum) legend(pos(7) ring(0) col(1)) title(" ")
	latexlog `file': subfigure, addfig file(Disp_event_raw/counts_by_timesince.pdf) ///
		caption("Counts") width(0.45)

	cellgraph displaced , by(timesince sep) nonotes legend(pos(3) ring(0) col(1)) title(" ")
	latexlog `file': subfigure, addfig file(Disp_event_raw/displaced_by_timesince.pdf) ///
		caption("Displacement Status") width(0.45)

	cellgraph earn , by(timesince sep) nonotes legend(pos(7) ring(0) col(1)) title(" ")
	latexlog `file': subfigure, addfig file(Disp_event_raw/logearn_by_timesince.pdf) ///
		caption("Earnings") width(0.45) eol

	cellgraph earnings_growth , by(timesince sep) nonotes legend(pos(7) ring(0) col(1)) title(" ")
	latexlog `file': subfigure, addfig file(Disp_event_raw/earnings_growth_by_timesince.pdf) ///
		caption("Earnings Growth $\frac{y_{i,t} - y_{i,-1}}{y_{i,-1}}$") width(0.45)

	cellgraph logearn , by(timesince sep) nonotes legend(pos(7) ring(0) col(1)) title(" ")
	latexlog `file': subfigure, addfig file(Disp_event_raw/earn_by_timesince.pdf) ///
		caption("Log Earnings $\log (y_{i,t} | y_{i,t}\geq 0)$") width(0.45)

	cellgraph employed , by(timesince sep) nonotes legend(pos(7) ring(0) col(1)) title(" ")
	latexlog `file': subfigure, addfig file(Disp_event_raw/employed_by_timesince.pdf) ///
		caption("Employment") width(0.45) eol

	latexlog `file': subfigure, close

	// ====== Spousal Outcomes ======
	latexlog `file': writeln "\clearpage \newpage"
	latexlog `file': section "Spousal Outcomes around Displacement Event"

	latexlog `file': writeln "This section shows spousal earnings and employment trajectories for displaced versus non-displaced workers' spouses."
	latexlog `file': writeln "\\\\ The added worker effect (AWE) refers to the labor supply response of spouses"
	latexlog `file': writeln "when the primary worker experiences job displacement."
	latexlog `file': writeln "\\\\ Matching is done exactly on whether the spouse is employed at baseline, and"
	latexlog `file': writeln "within cells we also match on spouse baseline log earnings using propensity score matching."
	latexlog `file': writeln "\\\\"
	latexlog `file': writeln "\textbf{Note on zero baseline earnings:} About 40\% of spouses have zero earnings at baseline."
	latexlog `file': writeln "For these spouses, earnings growth ($\frac{y_{i,t} - y_{i,-1}}{y_{i,-1}}$) is undefined."
	latexlog `file': writeln "Log earnings figures condition on positive spouse earnings."
	latexlog `file': writeln "Spouse employment and earnings in levels include all observations."

	// Summary statistics for spouse variables
	local spouse_vars spouse_age spouse_female spouse_edyrs spouse_earn spouse_logearn spouse_employed

	collect clear
	foreach column in All  Non_Disp Disp {
		if "`column'"=="All"      local cond 1
		if "`column'"=="Non_Disp" local cond sep==0
		if "`column'"=="Disp"     local cond sep==1

		table (var) if timesince==-1 & `cond', ///
			statistic(mean `spouse_vars') ///
			statistic(sd `spouse_vars') ///
			statistic(count N ) ///
			nformat(%9.1f) nototals  ///
			name(`column')
		collect addtag group[`column']
	}

	collect combine comb = All Non_Disp Disp
	collect style autolevels group All Non_Disp Disp
	collect style header result[mean sd] title(hide) level(hide)
	collect style header group, title(hide)
	collect style header result, level(hide)
	collect style cell result[sd], sformat("[%s]")
	collect style cell result[count], nformat("%8.0gc")
	collect style cell result, halign(center)
	collect style cell var[N], border(top)
	collect label levels group ///
		All "All workers" ///
		Non_Disp "Non-Displaced" ///
		Disp "Displaced"
	collect layout (var#result) (group)
 	collect preview
	latexlog `file': collect export , ///
		booktabs novert three  ///
		title(Spouse Summary Statistics by Displacement Status) ///
		notes(Average characteristics of spouses at baseline. Standard deviations in brackets.)

	// Cellgraph figures for spousal outcomes
	cap mkdir ${log}Spouse_raw/

	latexlog `file': subfigure, open title("Spousal Outcomes around Displacement Event")

	cellgraph spouse_earn , by(timesince sep) nonotes legend(pos(7) ring(0) col(1)) title(" ")
	latexlog `file': subfigure, addfig file(Spouse_raw/spouse_earn_by_timesince.pdf) ///
		caption("Spouse Earnings") width(0.45)

	cellgraph spouse_employed , by(timesince sep) nonotes legend(pos(7) ring(0) col(1)) title(" ")
	latexlog `file': subfigure, addfig file(Spouse_raw/spouse_employed_by_timesince.pdf) ///
		caption("Spouse Employment") width(0.45) eol

	cellgraph spouse_logearn , by(timesince sep) nonotes legend(pos(7) ring(0) col(1)) title(" ")
	latexlog `file': subfigure, addfig file(Spouse_raw/spouse_logearn_by_timesince.pdf) ///
		caption("Spouse Log Earnings (cond. on positive)") width(0.45)

	cellgraph spouse_earnings_growth , by(timesince sep) nonotes legend(pos(7) ring(0) col(1)) title(" ")
	latexlog `file': subfigure, addfig file(Spouse_raw/spouse_earnings_growth_by_timesince.pdf) ///
		caption("Spouse Earnings Growth") width(0.45) eol

	latexlog `file': subfigure, close


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

	use `in' if timesince>=`from' & timesince<=`to', clear

	egen persid2 = group(baselinetime persid)

	qui tab year, gen(_Dyear)
	drop _Dyear1

	qui tab timesince, gen(_DtimesinceTreat)

	// set all treatment time dummies to 0 for control group
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

	latexlog `file': section "Comparing Eventstudy Specifications"
	latexlog `file': writeln "Next, we compare the eventstudy estimates across different specifications."
	latexlog `file': writeln "\begin{itemize}"
	latexlog `file': writeln "\item Raw Means (always good to compare to the eventstudy estimates)"
	latexlog `file': writeln "\item OLS - Rel. year effects, no person FE"
	latexlog `file': writeln "\item JLS Specification: Cal. year and person FE - since we are not cotrolling for relative year effects, the estimates are not consistent"
	latexlog `file': writeln "\item Rel. year effects, person FE"
	latexlog `file': writeln "\item Relative year, calendar year and person FE"
	latexlog `file': writeln "\item Controlling for fully interacted calendar and relative year effects, as well as person FE"
	latexlog `file': writeln "\end{itemize}"
	latexlog `file': writeln "\\\\ The matching design does the heavy lifting, that's why these specification yield very similar results."
    latexlog `file': writeln "Standard errors are clustered at the matched pair level (Abadie and Spiess, 2022)."

	local opt ysize(8) xsize(12) xline(-0.5, lcol(gray)) legend(off) yscale(range(-.3 .1)) ylabel(-.3(.1).1) scale(1.2)

	latexlog `file': subfigure, open title("Comparing Eventstudy Specifications")

	cellgraph logearn, by(timesince sep) title(" ") legend(pos(7) ring(0) col(1))
	latexlog `file': subfigure, addfig file(Eventstudy/eventstudy_RawMeans.pdf) ///
		caption("Raw Means") width(0.45)

	reg logearn _DtimesinceTreat* _DtimesinceAll* `controls',
	eventstudy_figure, from(`from') to(`to') treatment_effect(`treatment_effect') name(g`i++') `omitcat' ///
		/// tit(OLS - Rel. Year Specification) subtit(Control. for Person FE; Year Since Event (for Disp and Non-Disp)) ///
		ytitle(Log Earnings) ///
		`opt'
	latexlog `file': subfigure, addfig file(Eventstudy/eventstudy_OLS.pdf) ///
		caption("Rel. year effects, no person FE") width(0.45) eol

	// --- JLS Specification - DOES NOT WORK ---
	xtreg logearn _DtimesinceTreat* _Dyear* `controls', fe i(persid2)
	eventstudy_figure, from(`from') to(`to') treatment_effect(`treatment_effect') name(g`i++') `omitcat' ///
		/// tit(JLS Specification) subtit(Controlling for Year and Person FE) ///
		ytitle(Log Earnings) ///
		`opt'
	latexlog `file': subfigure, addfig file(Eventstudy/eventstudy_FE_JLS.pdf) ///
		caption("Cal. year and person FE - does not work!") width(0.45)

	// --- Rel Year Specification ---
	xtreg logearn _DtimesinceTreat* _DtimesinceAll* `controls',  fe i(persid2)
	eventstudy_figure, from(`from') to(`to') treatment_effect(`treatment_effect') name(g`i++') `omitcat' ///
		///tit(Rel. Year Specification) subtit(Control. for Person FE; Year Since Event (for Disp and Non-Disp)) ///
		ytitle(Log Earnings) ///
		`opt'
	latexlog `file': subfigure, addfig file(Eventstudy/eventstudy_FE_RelYear.pdf) ///
		caption("Rel. year effects, person FE") width(0.45) eol

	// --- Schmieder / von Wachter / Heining Specification ---
	xtreg logearn _DtimesinceTreat* _Dyear* _DtimesinceAll* `controls', fe i(persid2)
	eventstudy_figure, from(`from') to(`to') treatment_effect(`treatment_effect') name(g`i++') `omitcat' ///
		/// tit(SWH Specification) subtit(Control. for Person FE; Year Since Event (for Disp and Non-Disp) and Year FE) ///
		ytitle(Log Earnings) ///
		`opt'
	latexlog `file': subfigure, addfig file(Eventstudy/eventstudy_FE_Full.pdf) ///
		caption("Rel. year, cal. year and person FE") width(0.45)

	// --- Schmieder / von Wachter / Heining Specification ---
	reghdfe logearn _DtimesinceTreat* `controls', absorb(persid2 timesince#year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') treatment_effect(`treatment_effect') name(g`i++') `omitcat' ///
		/// tit(SWH Specification) subtit(Control. for Person FE; Year Since Event (for Disp and Non-Disp) and Year FE) ///
		ytitle(Log Earnings) ///
		`opt'
	latexlog `file': subfigure, addfig file(Eventstudy/eventstudy_FE_Full.pdf) ///
		caption("Rel. year $\times$ cal. year and person FE") width(0.45)

	latexlog `file': subfigure, close

	latexlog `file': writeln "\clearpage \newpage"
	latexlog `file': section "Different Outcomes"
	latexlog `file': writeln "Next, we create eventstudy estimates for different outcomes."
	latexlog `file': writeln "\begin{itemize}"
	latexlog `file': writeln "\item Earnings in Levels"
	latexlog `file': writeln "\item Earnings Growth"
	latexlog `file': writeln "\item Log Earnings conditional on positive earnings"
	latexlog `file': writeln "\item Employment"
	latexlog `file': writeln "\end{itemize}"

	local opt2 ysize(8) xsize(12) xline(-0.5, lcol(gray)) legend(off) scale(1.2)

	latexlog `file': subfigure, open title("Event Study Estimates for Different Outcomes")

	// --- Earnings in Levels ---
	reghdfe earn _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to')  name(g`i++') `omitcat' ///
		ytitle(Earnings) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Eventstudy/eventstudy_earn.pdf) ///
		caption("Earnings in Levels") width(0.45)

	// --- Employment ---
	reghdfe employed _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' ///
		ytitle(Employed) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Eventstudy/eventstudy_employed.pdf) ///
		caption("Employment") width(0.45) eol

	
	// --- Log Earnings ---
	reghdfe logearn _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to')  name(g`i++') `omitcat' ///
		ytitle(Log Earnings) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Eventstudy/eventstudy_logearn.pdf) ///
		caption("Log Earnings cond. on pos. Earnings") width(0.45)

	// --- Earnings Growth ---
	reghdfe earnings_growth _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' ///
		ytitle(Earnings Growth) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Eventstudy/eventstudy_earnings_growth.pdf) ///
		caption("Earnings Growth") width(0.45) eol

	latexlog `file': subfigure, close



	latexlog `file': writeln "\clearpage \newpage"
	latexlog `file': section "Different Outcomes"
	latexlog `file': writeln "Next, we create eventstudy estimates for different outcomes."
	latexlog `file': writeln "\begin{itemize}"
	latexlog `file': writeln "\item Scale Raw Mean Difference by Control Mean: "
	latexlog `file': writeln "\begin{itemize}"
	latexlog `file': writeln "\item Outcome: $\frac{\bar{y}_{t}^{D} - \bar{y}_{t}^{ND}}{\bar{y}_{t}^{ND}}$"
	latexlog `file': writeln "\end{itemize}"
	latexlog `file': writeln "\item Scale Regression Coefficient from level regression by Control Mean: "
	latexlog `file': writeln "\begin{itemize}"
	latexlog `file': writeln "\item Outcome: $ y_{it}$"
	latexlog `file': writeln "\item Regression: $ y_{it} = \alpha_i + \gamma_t + \sum_k \beta_k D_{it}^k + \varepsilon_{it}$"
	latexlog `file': writeln "\item Scale: $\frac{\hat{\beta}_t}{E[y_{it}|X_{it},Non-Displaced]}$"
	latexlog `file': writeln "\end{itemize}"
	latexlog `file': writeln "\item Poisson QMLE: "
	latexlog `file': writeln "\begin{itemize}"
	latexlog `file': writeln "\item Outcome: $y_{it}$ (same as Approaches 1 and 2)"
	latexlog `file': writeln "\item Regression model: $ E[y_{it}|X_{it}] = \exp(\alpha_i + \gamma_t + \sum_k \beta_k D_{it}^k), $"
	latexlog `file': writeln "\item Scale: $\exp(\hat{\beta}_t) - 1$"
	latexlog `file': writeln "\end{itemize}"
	latexlog `file': writeln "\item Earnings Growth in percent: "
	latexlog `file': writeln "\begin{itemize}"
	latexlog `file': writeln "\item Outcome: $ \Delta y_{i,t} = \frac{y_{i,t} - y_{i,-1}}{y_{i,-1}}$"
	latexlog `file': writeln "\item Regression: $ \Delta y_{i,t} = \alpha_i + \gamma_t + \sum_k \beta_k D_{it}^k + \varepsilon_{it}$"
	latexlog `file': writeln "\item Scale: $\beta_t$"
	latexlog `file': writeln "\end{itemize}"

	latexlog `file': writeln "\subsection*{Discussion: Comparing Approaches for Percentage Interpretation}"
	latexlog `file': writeln "Each of these approaches expresses earnings losses as a percentage, but they differ in their"
	latexlog `file': writeln "assumptions, what they estimate, and how they handle zero earnings."
	latexlog `file': writeln "\\\\"
	latexlog `file': writeln "\textbf{Scaling by the control group mean (Approaches 1 and 2).}"
	latexlog `file': writeln "With a matching design, the control group mean is a valid estimate of the counterfactual"
	latexlog `file': writeln "$ E[Y(0)|D=1]$, so dividing the treatment--control difference by the control mean yields a proper"
	latexlog `file': writeln "estimate of the ATT in percentage terms: $\text{ATT} / E[Y(0)|D=1]$."
	latexlog `file': writeln "Approach 1 (raw mean difference) is fully nonparametric---transparent and easy to compute."
	latexlog `file': writeln "Approach 2 adds person fixed effects and controls, which may improve precision and absorb"
	latexlog `file': writeln "remaining imbalance after matching, but should yield similar point estimates if matching quality"
	latexlog `file': writeln "is high. Both approaches handle zero earnings naturally since they operate in levels."
	latexlog `file': writeln "A minor disadvantage is that confidence intervals from dividing $\hat{\beta}_t$ by $\bar{y}_t^{ND}$"
	latexlog `file': writeln "are approximate---strictly, one would need the delta method or bootstrap to account for"
	latexlog `file': writeln "estimation error in the denominator."
	latexlog `file': writeln "This is the classic approach used in the job displacement literature"
	latexlog `file': writeln "(Jacobson, LaLonde, and Sullivan, 1993, \textit{AER})."
	latexlog `file': writeln "\\\\"
	latexlog `file': writeln "\textbf{Poisson QMLE (Approach 3).}"
	latexlog `file': writeln "Like Approaches 1 and 2, Poisson QMLE uses earnings in levels $ y_{it}$ as the outcome"
	latexlog `file': writeln "and estimates essentially the same parameter---the percentage treatment effect---but"
	latexlog `file': writeln "does so directly via the multiplicative model"
	latexlog `file': writeln "$ E[y_{it}|X] = \exp(\alpha_i + \gamma_t + \sum_k \beta_k D_{it}^k)$,"
	latexlog `file': writeln "so $\exp(\hat{\beta}_t) - 1$ gives the percentage effect without requiring post-estimation rescaling."
	latexlog `file': writeln "Key properties:"
	latexlog `file': writeln "\begin{itemize}"
	latexlog `file': writeln "\item \textit{Handles zeros naturally:} Unlike $\log(y)$, Poisson PML is well-defined for $ y=0$"
	latexlog `file': writeln "since it only requires correct specification of the conditional mean, not that $ y$ follows"
	latexlog `file': writeln "a Poisson distribution (Santos Silva and Tenreyro, 2006, \textit{REStat};"
	latexlog `file': writeln "Gourieroux, Monfort, and Trognon, 1984, \textit{Econometrica})."
	latexlog `file': writeln "High-dimensional fixed effects can be included via \texttt{ppmlhdfe}"
	latexlog `file': writeln "(Correia, Guimar\~{a}es, and Zylkin, 2020, \textit{Stata Journal})."
	latexlog `file': writeln "\item \textit{Functional form difference vs.\ OLS:}"
	latexlog `file': writeln "OLS in levels assumes \textbf{additive separability}:"
	latexlog `file': writeln "$ E[y|i,t,D] = \alpha_i + \gamma_t + \beta D$---the treatment effect is the same"
	latexlog `file': writeln "\textit{absolute} amount for everyone."
	latexlog `file': writeln "Poisson assumes \textbf{multiplicative separability}:"
	latexlog `file': writeln "$ E[y|i,t,D] = \exp(\alpha_i) \cdot \exp(\gamma_t) \cdot \exp(\beta D)$---the treatment"
	latexlog `file': writeln "effect is the same \textit{proportional} amount for everyone."
	latexlog `file': writeln "When the goal is a percentage interpretation, the multiplicative assumption is arguably"
	latexlog `file': writeln "more natural: a 20\% earnings loss means higher-earning workers lose more in levels,"
	latexlog `file': writeln "which is typically what we observe empirically."
	latexlog `file': writeln "\item \textit{Same estimand as Approaches 1 and 2:}"
	latexlog `file': writeln "With few controls, $\exp(\hat{\beta}^{\text{Poisson}}_t) - 1 \approx \hat{\beta}^{\text{OLS}}_t / \bar{y}$,"
	latexlog `file': writeln "so all three approaches estimate essentially the same percentage effect."
	latexlog `file': writeln "Differences emerge when effects are large or when the outcome distribution has many zeros."
	latexlog `file': writeln "\end{itemize}"
	latexlog `file': writeln "\\\\"
	latexlog `file': writeln "\textbf{Earnings growth (Approach 4).}"
	latexlog `file': writeln "This measures the individual-level percentage change $(y_{it} - y_{i,-1}) / y_{i,-1}$,"
	latexlog `file': writeln "which is directly interpretable at the individual level. However, it is \textit{undefined}"
	latexlog `file': writeln "when baseline earnings are zero, so it drops the extensive margin entirely."
	latexlog `file': writeln "It can also be extremely noisy or ill-behaved when the denominator is small."
	latexlog `file': writeln "This is a serious limitation whenever non-employment spells are prevalent."
	latexlog `file': writeln "\\\\"
	latexlog `file': writeln "\textbf{Summary.}"
	latexlog `file': writeln "When zeros are rare (as for main worker earnings before displacement), all four approaches"
	latexlog `file': writeln "should yield similar results. Differences emerge when zeros are prevalent---precisely"
	latexlog `file': writeln "the case for spousal outcomes and the added worker effect."

	latexlog `file': subfigure, open title("Expressing Earnings Losses in Percent (incl. zero earnings)")

	// --- Difference in means between treatment and control scaled by control mean---
	reghdfe earn _DtimesinceTreat* , absorb( timesince ) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' pctcontrol(earn) ///
		ytitle(Earnings Loss (% of control)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Eventstudy/eventstudy_mean_diff.pdf) ///
		caption("Scale Raw Mean Difference: $\frac{\bar{y}_{t}^{D} - \bar{y}_{t}^{ND}}{\bar{y}_{t}^{ND}}$") width(0.45)

	// --- Percent relative to control ---
	reghdfe earn _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' pctcontrol(earn) ///
		ytitle(Earnings Loss (% of control)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Eventstudy/eventstudy_pct_control.pdf) ///
		caption("Scale Regression Coefficient: $\frac{\hat{\beta}_t}{\bar{y}_{t}^{ND}}$") width(0.45)

    // --- Poisson QMLE rescaled to percent effect ---
	ppmlhdfe earn _DtimesinceTreat*  `controls', vce(cluster persid2) absorb(persid2 timesince year)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' pct ///
		ytitle(Earnings Loss in percent) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Eventstudy/eventstudy_poisson_pct.pdf) ///
		caption("Poisson QMLE, scaled to \%: $\exp(\hat{\beta}_t) - 1$") width(0.45) eol

	// --- Earnings Growth ---
	reghdfe earnings_growth _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' ///
		ytitle(Earnings Growth) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Eventstudy/eventstudy_earnings_growth.pdf) ///
		caption("Earnings Growth: $\frac{y_{i,t} - y_{i,-1}}{y_{i,-1}}$") width(0.45) eol



	latexlog `file': subfigure, close

	// ====== Added Worker Effect: Spousal Outcomes ======
	latexlog `file': writeln "\clearpage \newpage"
	latexlog `file': section "Added Worker Effect: Spousal Outcomes"
	latexlog `file': writeln "This section estimates the \textbf{added worker effect} (AWE) --- the change in spousal labor market outcomes"
	latexlog `file': writeln "in response to the primary worker's job displacement."
	latexlog `file': writeln "\\\\ We estimate event study regressions using the same treatment x relative-time dummies"
	latexlog `file': writeln "as for the main worker outcomes, but with spouse earnings, employment, and various transformations as dependent variables."
	latexlog `file': writeln "\\\\"
	latexlog `file': writeln "\textbf{Note on zero baseline earnings:} About 40\% of spouses have zero earnings at baseline."
	latexlog `file': writeln "This makes expressing the AWE in percentage terms considerably more challenging than for the"
	latexlog `file': writeln "displaced worker's own earnings."

	latexlog `file': writeln "\\\\"
	latexlog `file': writeln "We present the following approaches:"
	latexlog `file': writeln "\begin{itemize}"
	latexlog `file': writeln "\item \textbf{Levels and Employment} (panels a--d): Raw means and event study estimates for"
	latexlog `file': writeln "spouse earnings in levels and employment. Well-defined for all observations."
	latexlog `file': writeln "\item \textbf{Scale by Control Mean} (panels e--f): Raw mean difference and regression coefficient"
	latexlog `file': writeln "divided by the control group mean at each event time."
	latexlog `file': writeln "\item \textbf{Poisson QMLE} (panel g): $ \exp(\hat{\beta}_t) - 1$ handles zeros naturally."
	latexlog `file': writeln "\item \textbf{Earnings Growth as \% of HH Income} (panel h): $ (y_{it}^S - y_{i,-1}^S) / (y_{i,-1} + y_{i,-1}^S)$."
	latexlog `file': writeln "\item \textbf{Earnings Growth as \% of Partner Earnings} (panel i): $ (y_{it}^S - y_{i,-1}^S) / y_{i,-1}$."
	latexlog `file': writeln "\item \textbf{Log with additive constant} (panels j--l): $ \log(y^S + c)$ for $ c \in \{0.01, 1, 100\}$."
	latexlog `file': writeln "\end{itemize}"

	latexlog `file': writeln "\subsection*{Discussion: Percentage Interpretation for the Added Worker Effect}"
	latexlog `file': writeln "\textbf{The zero-baseline problem.}"
	latexlog `file': writeln "Earnings growth $ (y_{it}^S - y_{i,-1}^S) / y_{i,-1}^S$ is undefined when baseline spouse"
	latexlog `file': writeln "earnings are zero. Conditioning on positive baseline earnings creates a selected sample:"
	latexlog `file': writeln "the extensive margin response---spouses \textit{entering} the labor force in response to"
	latexlog `file': writeln "displacement---is missed entirely, even though it may be an important component"
	latexlog `file': writeln "of the AWE (Lundberg, 1985, \textit{JPE}; Stephens, 2002, \textit{JLE})."
	latexlog `file': writeln "\textbf{Dividing by spouse baseline income does not work} for the full sample since it"
	latexlog `file': writeln "drops the approximately 40\% of spouses who are not employed at baseline."
	latexlog `file': writeln "\\\\"
	latexlog `file': writeln "\textbf{Scaling by the control mean.}"
	latexlog `file': writeln "Dividing the level estimate by the control group mean works and includes all observations"
	latexlog `file': writeln "(zeros contribute to both numerator and denominator). However, when the control mean is"
	latexlog `file': writeln "small---as it is for initially non-employed spouses---small level changes can produce"
	latexlog `file': writeln "large and potentially misleading percentages."
	latexlog `file': writeln "\\\\"
	latexlog `file': writeln "\textbf{Poisson QMLE targets the same parameter as scaling by the control mean.}"
	latexlog `file': writeln "Like Approaches 1 and 2, Poisson QMLE uses spouse earnings in levels $ y_{it}^S$ as the outcome."
	latexlog `file': writeln "With few controls, $ \exp(\hat{\beta}^{\text{Poisson}}_t) - 1 \approx \hat{\beta}^{\text{OLS}}_t / \bar{y}^S$,"
	latexlog `file': writeln "so all three approaches estimate essentially the same percentage effect."
	latexlog `file': writeln "The advantage of Poisson is that it handles zeros without any sample restriction or ad-hoc rescaling,"
	latexlog `file': writeln "gives a coherent percentage interpretation via $ \exp(\hat{\beta}_t) - 1$, and captures both extensive and"
	latexlog `file': writeln "intensive margin responses in a single estimate. The multiplicative structure is especially"
	latexlog `file': writeln "appropriate here: when baseline spouse earnings are zero, the model implies zero expected"
	latexlog `file': writeln "contribution from the intensive margin, which is correct."
	latexlog `file': writeln "\\\\"
	latexlog `file': writeln "\textbf{Earnings growth relative to household income.}"
	latexlog `file': writeln "Normalizing by total household baseline earnings"
	latexlog `file': writeln "$ (y_{i,-1} + y_{i,-1}^S)$ instead of spouse baseline earnings alone"
	latexlog `file': writeln "is always defined (since the primary worker has positive earnings by construction)"
	latexlog `file': writeln "and has a natural interpretation: it measures the spousal earnings response as a share of"
	latexlog `file': writeln "total household resources at risk."
	latexlog `file': writeln "\\\\"
	latexlog `file': writeln "\textbf{Earnings growth relative to partner (displaced worker) income.}"
	latexlog `file': writeln "Normalizing by the displaced worker's baseline earnings $ y_{i,-1}$ uses the same"
	latexlog `file': writeln "denominator as the displaced worker's own earnings growth measure"
	latexlog `file': writeln "$ (y_{it} - y_{i,-1})/y_{i,-1}$."
	latexlog `file': writeln "This makes the two directly additive: the displaced worker's earnings growth"
	latexlog `file': writeln "plus the spouse's earnings growth (both normalized by the displaced worker's"
	latexlog `file': writeln "pre-displacement earnings) gives the total household income change as a fraction"
	latexlog `file': writeln "of the displaced worker's baseline earnings."
	latexlog `file': writeln "This is always well-defined since displaced workers have positive baseline earnings by construction."
	latexlog `file': writeln "\\\\"
	latexlog `file': writeln "\textbf{Log transformation with additive constant: $ \log(y^S + c)$.}"
	latexlog `file': writeln "A common approach to handle zero earnings is to estimate $ \log(y + c)$ for some"
	latexlog `file': writeln "constant $ c > 0$. However, Chen and Roth (\textit{QJE}, 2024) show that the"
	latexlog `file': writeln "estimated treatment effect is highly sensitive to the choice of $ c$ and that"
	latexlog `file': writeln "there is no principled way to select it."
	latexlog `file': writeln "We illustrate this with $ c \in \{0.01, 1, 100\}$."
	latexlog `file': writeln "The results vary dramatically across choices of $ c$, confirming that this approach"
	latexlog `file': writeln "is unreliable for outcomes with many zeros."


	cap mkdir ${log}Spouse_eventstudy/

	// ====== Figure 4: AWE for All Spouses (12 panels) ======
	latexlog `file': subfigure, open title("Added Worker Effect: All Spouses")

	// (a) Raw Means: Earnings
	cellgraph spouse_earn, by(timesince sep) title(" ") legend(pos(7) ring(0) col(1))
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_earn_RawMeans_all.pdf) ///
		caption("Raw Means: Earnings") width(0.24)

	// (b) Raw Means: Employment
	cellgraph spouse_employed, by(timesince sep) title(" ") legend(pos(7) ring(0) col(1))
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_employed_RawMeans_all.pdf) ///
		caption("Raw Means: Employment") width(0.24)

	// (c) Event Study: Earnings
	reghdfe spouse_earn _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' ///
		ytitle(Spouse Earnings) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_earn_all.pdf) ///
		caption("Event Study: Earnings") width(0.24)

	// (d) Event Study: Employment
	reghdfe spouse_employed _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' ///
		ytitle(Spouse Employed) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_employed_all.pdf) ///
		caption("Event Study: Employment") width(0.24) eol

	// (e) Raw Mean Diff. / Control Mean
	reghdfe spouse_earn _DtimesinceTreat* , absorb( timesince ) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' pctcontrol(spouse_earn) yrange(-1 1) ///
		ytitle(Spouse Earnings (% of control)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_mean_diff_all.pdf) ///
		caption("Raw Mean Diff. / Control Mean") width(0.24)

	// (f) Reg Coef. / Control Mean
	reghdfe spouse_earn _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' pctcontrol(spouse_earn) yrange(-1 1) ///
		ytitle(Spouse Earnings (% of control)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_pct_control_all.pdf) ///
		caption("Reg. Coeff. / Control Mean") width(0.24)

	// (g) Poisson QMLE (scaled to %)
	ppmlhdfe spouse_earn _DtimesinceTreat* `controls', vce(cluster persid2) absorb(persid2 timesince year)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' pct yrange(-1 1) ///
		ytitle(Spouse Earnings Change in percent) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_poisson_all.pdf) ///
		caption("Poisson QMLE: $ \exp(\hat{\beta}_t) - 1$") width(0.24)

	// (h) Earnings Growth as % of HH baseline income
	reghdfe spouse_earnings_growth_hh _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' yrange(-1 1) ///
		ytitle(Spouse Earnings Growth (% HH Income)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_growth_hh_all.pdf) ///
		caption("Earnings Growth (\% of HH Income)") width(0.24) eol

	// (i) Earnings Growth as % of partner baseline earnings
	reghdfe spouse_earnings_growth_partner _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' yrange(-1 1) ///
		ytitle(Spouse Earnings Growth (% Partner)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_growth_partner_all.pdf) ///
		caption("Earnings Growth (\% of Partner Earnings)") width(0.24)

	// (j) log(earnings + 0.01)
	reghdfe spouse_logearn_001 _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' yrange(-1 1) ///
		ytitle(log(spouse earnings + 0.01)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_logearn_001_all.pdf) ///
		caption("$ \log(\text{earn} + 0.01)$") width(0.24)

	// (k) log(earnings + 1)
	reghdfe spouse_logearn_1 _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' yrange(-1 1) ///
		ytitle(log(spouse earnings + 1)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_logearn_1_all.pdf) ///
		caption("$ \log(\text{earn} + 1)$") width(0.24)

	// (l) log(earnings + 100)
	reghdfe spouse_logearn_100 _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' yrange(-1 1) ///
		ytitle(log(spouse earnings + 100)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_logearn_100_all.pdf) ///
		caption("$ \log(\text{earn} + 100)$") width(0.24) eol

	latexlog `file': subfigure, close

	latexlog `file': writeln "\textbf{Notes:} Panels (a)--(b) show raw means for displaced and non-displaced workers' spouses."
	latexlog `file': writeln "Panels (c)--(d) show event study regression coefficients in levels."
	latexlog `file': writeln "Panels (e)--(f) scale by the control group mean at each event time to express effects in percent."
	latexlog `file': writeln "Panel (g) uses Poisson QMLE, which handles zeros naturally and gives a direct percentage interpretation."
	latexlog `file': writeln "Panel (h) normalizes spouse earnings changes by total household baseline income."
	latexlog `file': writeln "Panel (i) normalizes by the displaced worker's baseline earnings---same denominator as the"
	latexlog `file': writeln "displaced worker's own earnings growth, so the two are directly additive."
	latexlog `file': writeln "Panels (j)--(l) illustrate that $ \log(y + c)$ is highly sensitive to the choice of $ c$"
	latexlog `file': writeln "(Chen and Roth, \textit{QJE}, 2024)."

	// ====== Figure 5: AWE for Baseline Employed Spouses (12 panels) ======
	latexlog `file': writeln "\clearpage \newpage"
	latexlog `file': section "AWE for Spouses Employed at Baseline"
	latexlog `file': writeln "This section shows the added worker effect for spouses who were employed at baseline."
	latexlog `file': writeln "\\\\ Since these spouses have positive baseline earnings, all measures including earnings growth are well defined."

	latexlog `file': subfigure, open title("AWE for Baseline Employed Spouses")
	local yrange_opt yrange(-1 2.5)
	// (a) Raw Means: Earnings
	cellgraph spouse_earn if spouse_baseline_employed == 1, by(timesince sep) title(" ") legend(pos(7) ring(0) col(1))
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_earn_RawMeans_emp.pdf) ///
		caption("Raw Means: Earnings") width(0.24)

	// (b) Raw Means: Employment
	cellgraph spouse_employed if spouse_baseline_employed == 1, by(timesince sep) title(" ") legend(pos(7) ring(0) col(1))
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_employed_RawMeans_emp.pdf) ///
		caption("Raw Means: Employment") width(0.24)

	preserve
	keep if spouse_baseline_employed == 1

	// (c) Event Study: Earnings
	reghdfe spouse_earn _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat'  ///
		ytitle(Spouse Earnings) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_earn_emp.pdf) ///
		caption("Event Study: Earnings") width(0.24)

	// (d) Event Study: Employment
	reghdfe spouse_employed _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat'  ///
		ytitle(Spouse Employed) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_employed_emp.pdf) ///
		caption("Event Study: Employment") width(0.24) eol

	// (e) Raw Mean Diff. / Control Mean
	reghdfe spouse_earn _DtimesinceTreat* , absorb( timesince ) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' pctcontrol(spouse_earn) `yrange_opt' ///
		ytitle(Spouse Earnings (% of control)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_mean_diff_emp.pdf) ///
		caption("Raw Mean Diff. / Control Mean") width(0.24)

	// (f) Reg Coef. / Control Mean
	reghdfe spouse_earn _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' pctcontrol(spouse_earn) `yrange_opt' ///
		ytitle(Spouse Earnings (% of control)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_pct_control_emp.pdf) ///
		caption("Reg. Coeff. / Control Mean") width(0.24)

	// (g) Poisson QMLE (scaled to %)
	ppmlhdfe spouse_earn _DtimesinceTreat* `controls', vce(cluster persid2) absorb(persid2 timesince year)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' pct `yrange_opt' ///
		ytitle(Spouse Earnings Change in percent) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_poisson_emp.pdf) ///
		caption("Poisson QMLE: $ \exp(\hat{\beta}_t) - 1$") width(0.24)

	// (h) Earnings Growth as % of HH baseline income
	reghdfe spouse_earnings_growth_hh _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' `yrange_opt' ///
		ytitle(Spouse Earnings Growth (% HH Income)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_growth_hh_emp.pdf) ///
		caption("Earnings Growth (\% of HH Income)") width(0.24) eol

	// (i) Earnings Growth as % of partner baseline earnings
	reghdfe spouse_earnings_growth_partner _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' `yrange_opt' ///
		ytitle(Spouse Earnings Growth (% Partner)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_growth_partner_emp.pdf) ///
		caption("Earnings Growth (\% of Partner Earnings)") width(0.24)

	// (j) log(earnings + 0.01)
	reghdfe spouse_logearn_001 _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' `yrange_opt' ///
		ytitle(log(spouse earnings + 0.01)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_logearn_001_emp.pdf) ///
		caption("$ \log(\text{earn} + 0.01)$") width(0.24)

	// (k) log(earnings + 1)
	reghdfe spouse_logearn_1 _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' `yrange_opt' ///
		ytitle(log(spouse earnings + 1)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_logearn_1_emp.pdf) ///
		caption("$ \log(\text{earn} + 1)$") width(0.24)

	// (l) log(earnings + 100)
	reghdfe spouse_logearn_100 _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' `yrange_opt' ///
		ytitle(log(spouse earnings + 100)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_logearn_100_emp.pdf) ///
		caption("$ \log(\text{earn} + 100)$") width(0.24) eol

	restore

	latexlog `file': subfigure, close

	latexlog `file': writeln "\textbf{Notes:} Conditional on spouse being employed at baseline. All percentage measures are well-defined"
	latexlog `file': writeln "since baseline spouse earnings are positive."
	latexlog `file': writeln "Panels (j)--(l) show that $ \log(y + c)$ estimates vary with $ c$ even in this subsample"
	latexlog `file': writeln "(Chen and Roth, \textit{QJE}, 2024)."

	// ====== Figure 6: AWE for Baseline Non-Employed Spouses (12 panels) ======
	latexlog `file': writeln "\clearpage \newpage"
	latexlog `file': section "AWE for Spouses Not Employed at Baseline"
	latexlog `file': writeln "This section shows the added worker effect for spouses who were not employed at baseline."
	latexlog `file': writeln "\\\\ Since baseline spouse earnings are zero, earnings growth $ (y_{it}^S - y_{i,-1}^S)/y_{i,-1}^S$ is undefined."
	latexlog `file': writeln "Dividing by spouse baseline income does not work here since it requires dividing by zero."
	latexlog `file': writeln "\\\\ However, normalizing by household income or by the displaced worker's baseline earnings"
	latexlog `file': writeln "remains well-defined, as does Poisson QMLE."

	latexlog `file': subfigure, open title("AWE for Baseline Non-Employed Spouses")

	local yrange_opt yrange(-1 3)
	// (a) Raw Means: Earnings
	cellgraph spouse_earn if spouse_baseline_employed == 0, by(timesince sep) title(" ") legend(pos(7) ring(0) col(1))
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_earn_RawMeans_nonemp.pdf) ///
		caption("Raw Means: Earnings") width(0.24)

	// (b) Raw Means: Employment
	cellgraph spouse_employed if spouse_baseline_employed == 0, by(timesince sep) title(" ") legend(pos(7) ring(0) col(1))
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_employed_RawMeans_nonemp.pdf) ///
		caption("Raw Means: Employment") width(0.24)

	preserve
	keep if spouse_baseline_employed == 0

	// (c) Event Study: Earnings
	reghdfe spouse_earn _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' ///
		ytitle(Spouse Earnings) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_earn_nonemp.pdf) ///
		caption("Event Study: Earnings") width(0.24)

	// (d) Event Study: Employment
	reghdfe spouse_employed _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' ///
		ytitle(Spouse Employed) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_employed_nonemp.pdf) ///
		caption("Event Study: Employment") width(0.24) eol

	// (e) Raw Mean Diff. / Control Mean
	reghdfe spouse_earn _DtimesinceTreat* , absorb( timesince ) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' pctcontrol(spouse_earn) hollowomit `yrange_opt' ///
		ytitle(Spouse Earnings (% of control)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_mean_diff_nonemp.pdf) ///
		caption("Raw Mean Diff. / Control Mean") width(0.24)

	// (f) Reg Coef. / Control Mean
	reghdfe spouse_earn _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' pctcontrol(spouse_earn) hollowomit `yrange_opt' ///
		ytitle(Spouse Earnings (% of control)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_pct_control_nonemp.pdf) ///
		caption("Reg. Coeff. / Control Mean") width(0.24)

	// (g) Poisson QMLE (scaled to %)
	ppmlhdfe spouse_earn _DtimesinceTreat* `controls', vce(cluster persid2) absorb(persid2 timesince year)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' pct hollowomit `yrange_opt' ///
		ytitle(Spouse Earnings Change in percent) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_poisson_nonemp.pdf) ///
		caption("Poisson QMLE: $ \exp(\hat{\beta}_t) - 1$") width(0.24)

	// (h) Earnings Growth as % of HH baseline income
	reghdfe spouse_earnings_growth_hh _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' `yrange_opt' ///
		ytitle(Spouse Earnings Growth (% HH Income)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_growth_hh_nonemp.pdf) ///
		caption("Earnings Growth (\% of HH Income)") width(0.24) eol

	// (i) Earnings Growth as % of partner baseline earnings
	reghdfe spouse_earnings_growth_partner _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' `yrange_opt' ///
		ytitle(Spouse Earnings Growth (% Partner)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_growth_partner_nonemp.pdf) ///
		caption("Earnings Growth (\% of Partner Earnings)") width(0.24)

	// (j) log(earnings + 0.01)
	reghdfe spouse_logearn_001 _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' `yrange_opt' ///
		ytitle(log(spouse earnings + 0.01)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_logearn_001_nonemp.pdf) ///
		caption("$ \log(\text{earn} + 0.01)$") width(0.24)

	// (k) log(earnings + 1)
	reghdfe spouse_logearn_1 _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
	eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' `yrange_opt' ///
		ytitle(log(spouse earnings + 1)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_logearn_1_nonemp.pdf) ///
		caption("$ \log(\text{earn} + 1)$") width(0.24)

	// (l) log(earnings + 100)
	reghdfe spouse_logearn_100 _DtimesinceTreat* `controls', absorb(persid2 timesince year) cluster(persid2)
		eventstudy_figure, from(`from') to(`to') name(g`i++') `omitcat' `yrange_opt' ///
		ytitle(log(spouse earnings + 100)) ///
		`opt2'
	latexlog `file': subfigure, addfig file(Spouse_eventstudy/eventstudy_spouse_logearn_100_nonemp.pdf) ///
		caption("$ \log(\text{earn} + 100)$") width(0.24) eol

	restore

	latexlog `file': subfigure, close

	latexlog `file': writeln "\textbf{Notes:} Conditional on spouse being \textit{not} employed at baseline."
	latexlog `file': writeln "Panels (a)--(d) are in levels and well-defined."
	latexlog `file': writeln "Panels (e)--(g) use hollow circles at the omitted category. Scaled outcomes are undefined (division by zero)"
	latexlog `file': writeln "and Poisson coefficients are not identified for groups where outcomes are precise zeros."
	latexlog `file': writeln "Panels (h)--(i) normalize by the displaced worker's (positive) baseline earnings or total"
	latexlog `file': writeln "household income and are well-defined even for baseline non-employed spouses."
	latexlog `file': writeln "Panels (j)--(l) illustrate the sensitivity of $ \log(y + c)$ to the choice of $ c$"
	latexlog `file': writeln "(Chen and Roth, \textit{QJE}, 2024)."

	latexlog `file': writeln "\clearpage \newpage"
	latexlog `file': writeln "\begin{thebibliography}{99}"
	latexlog `file': writeln ""
	latexlog `file': writeln "\bibitem{AbadieSpiess2022}"
	latexlog `file': writeln "Abadie, Alberto and Jann Spiess (2022)."
	latexlog `file': writeln "\textit{Robust Post-Matching Inference.}"
	latexlog `file': writeln "Journal of the American Statistical Association, 117, 983--995."
	latexlog `file': writeln ""
	latexlog `file': writeln "\bibitem{ChenRoth2024}"
	latexlog `file': writeln "Chen, Jiafeng and Jonathan Roth (2024)."
	latexlog `file': writeln "\textit{Logs with Zeros? Some Problems and Solutions.}"
	latexlog `file': writeln "Quarterly Journal of Economics, 139(2), 891--936."
	latexlog `file': writeln ""
	latexlog `file': writeln "\bibitem{CorreiaGuimaraesZylkin2020}"
	latexlog `file': writeln "Correia, Sergio, Paulo Guimar\~{a}es, and Tom Zylkin (2020)."
	latexlog `file': writeln "\textit{Fast Poisson Estimation with High-Dimensional Fixed Effects.}"
	latexlog `file': writeln "Stata Journal, 20(1), 95--115."
	latexlog `file': writeln ""
	latexlog `file': writeln "\bibitem{GourierouxMontfortTrognon1984}"
	latexlog `file': writeln "Gourieroux, Christian, Alain Monfort, and Alain Trognon (1984)."
	latexlog `file': writeln "\textit{Pseudo Maximum Likelihood Methods: Theory.}"
	latexlog `file': writeln "Econometrica, 52(3), 681--700."
	latexlog `file': writeln ""
	latexlog `file': writeln "\bibitem{HallaSchmiederWeber2020}"
	latexlog `file': writeln "Halla, Martin, Johannes Schmieder, and Andrea Weber (2020)."
	latexlog `file': writeln "\textit{Job Displacement, Family Dynamics, and Spousal Labor Supply.}"
	latexlog `file': writeln "American Economic Journal: Applied Economics, 12(4), 253--287."
	latexlog `file': writeln ""
	latexlog `file': writeln "\bibitem{JacobsonLaLondeSullivan1993}"
	latexlog `file': writeln "Jacobson, Louis S., Robert J. LaLonde, and Daniel G. Sullivan (1993)."
	latexlog `file': writeln "\textit{Earnings Losses of Displaced Workers.}"
	latexlog `file': writeln "American Economic Review, 83(4), 685--709."
	latexlog `file': writeln ""
	latexlog `file': writeln "\bibitem{Lundberg1985}"
	latexlog `file': writeln "Lundberg, Shelly (1985)."
	latexlog `file': writeln "\textit{The Added Worker Effect.}"
	latexlog `file': writeln "Journal of Political Economy, 93(1), 11--37."
	latexlog `file': writeln ""
	latexlog `file': writeln "\bibitem{SantosSilvaTenreyro2006}"
	latexlog `file': writeln "Santos Silva, J.M.C. and Silvana Tenreyro (2006)."
	latexlog `file': writeln "\textit{The Log of Gravity.}"
	latexlog `file': writeln "Review of Economics and Statistics, 88(4), 641--658."
	latexlog `file': writeln ""
	latexlog `file': writeln "\bibitem{SchmiederVonWachterHeining2023}"
	latexlog `file': writeln "Schmieder, Johannes F., Till von Wachter, and J\""org Heining (2023)."
	latexlog `file': writeln "\textit{The Costs of Job Displacement over the Business Cycle and Its Sources:}"
	latexlog `file': writeln "\textit{Evidence from Germany.}"
	latexlog `file': writeln "American Economic Review, 113(5), 1208--1254."
	latexlog `file': writeln ""
	latexlog `file': writeln "\bibitem{Stephens2002}"
	latexlog `file': writeln "Stephens, Melvin Jr. (2002)."
	latexlog `file': writeln "\textit{Worker Displacement and the Added Worker Effect.}"
	latexlog `file': writeln "Journal of Labor Economics, 20(3), 504--537."
	latexlog `file': writeln ""
	latexlog `file': writeln "\end{thebibliography}"

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
		treatment_effect(str) ///
		PCT ///
		pctcontrol(str) ///
		HOLLOWomit ///
		YRANge(str) ]


	if "`omitcat'"=="" local omitcat = `from'

	preserve

	// Compute control group means by timesince for pctcontrol rescaling
	if "`pctcontrol'" != "" {
		collapse (mean) _control_mean = `pctcontrol' if sep == 0, by(timesince)
		tempfile control_means
		save `control_means'
		restore
		preserve
	}

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

	// Rescale to exp(beta)-1 if pct option is specified
	if "`pct'" != "" {
		replace coef_ev = exp(coef_ev) - 1
		replace ci_hi = exp(ci_hi) - 1
		replace ci_lo = exp(ci_lo) - 1
	}

	// Rescale as percent of control group mean
	if "`pctcontrol'" != "" {
		merge 1:1 timesince using `control_means', nogen
		replace coef_ev = coef_ev / _control_mean
		replace ci_hi = ci_hi / _control_mean
		replace ci_lo = ci_lo / _control_mean
		drop _control_mean
	}

	sort timesince

	// Apply y-axis range capping: cap CIs and mark capped regions for dashed display
	local yrange_opt
	local yrange_lines
	if `"`yrange'"' != "" {
		local ylo : word 1 of `yrange'
		local yhi : word 2 of `yrange'
		// Save uncapped CI bounds for dashed line overlay where capped
		g ci_hi_raw = ci_hi
		g ci_lo_raw = ci_lo
		// Cap CI and coefficient at range boundaries (both bounds clamped to [ylo, yhi])
		replace ci_hi = min(max(ci_hi, `ylo'), `yhi')
		replace ci_lo = min(max(ci_lo, `ylo'), `yhi')
		replace coef_ev = min(max(coef_ev, `ylo'), `yhi')
		// Create dashed line variables: only where original CI exceeded boundary
		g ci_hi_dash = `yhi' if ci_hi_raw > `yhi'
		g ci_lo_dash = `ylo' if ci_lo_raw < `ylo'
		local yrange_opt yscale(range(`ylo' `yhi')) ylabel(`ylo'(0.5)`yhi', nogrid)
	}

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

	// Build dashed-line overlays for capped CI boundaries (used with yrange)
	if `"`yrange'"' != "" {
		local yrange_lines ///
			(line ci_hi_dash timesince, color("`col' % 50") lpattern(dash) lwidth(medthin)) ///
			(line ci_lo_dash timesince, color("`col' % 50") lpattern(dash) lwidth(medthin))
	}

	// Hollow circle at omitted category if requested
	// Split line into pre and post segments so Stata doesn't connect across the gap
	if "`hollowomit'" != "" {
		g omit_point = 0 if timesince == `omitcat'
		local omit_scatter (scatter omit_point timesince, msymbol(Oh) mcolor("`col'") msize(medlarge))
		local pre_line  (rarea ci_hi ci_lo timesince if timesince < `omitcat', color("`col' % `ciopacity'")) ///
						(line coef_ev timesince if timesince < `omitcat', color("`col'"))
		local post_line (rarea ci_hi ci_lo timesince if timesince > `omitcat', color("`col' % `ciopacity'")) ///
						(line coef_ev timesince if timesince > `omitcat', color("`col'"))
	}
	else {
		local omit_scatter
		local pre_line  (rarea ci_hi ci_lo timesince, color("`col' % `ciopacity'")) ///
						(line coef_ev timesince, color("`col'"))
		local post_line
	}

	twoway ///
		`pre_line' ///
		`post_line' ///
		`yrange_lines' ///
		`omit_scatter' ///
		`true_eff_line' ///
		, ///
		legend(label(1 "Treatment effect in year since treatment")  order(1)) ///
		legend(region(lcolor(white))) legend(pos(6)) ///
		graphr(color(white)) xtitle(Year relative to treatment) ///
		`yrange_opt' ///
		`treatment_text' ///
		`options'

	restore

end // eventstudy_figure


/*---------------------------------------------------------*/
/* Run Main Program */
/*---------------------------------------------------------*/
main // run main program


/*========================================= END ==============================================*/
