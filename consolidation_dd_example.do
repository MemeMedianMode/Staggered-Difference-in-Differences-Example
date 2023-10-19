********************************************************************************
*
*	This paper draws on a subset of data from 
*	Chin, M. J. (2023). School district consolidation in North Carolina: 
*		Impacts on school composition and finance, crime outcomes, and 
*		educational attainment. Economics of Education Review, 95, 102432.
*
*	It runs different difference-in-difference estimators, including
*	those that are robust to recent advances in the DD literature.
*
*	Date updated: 10/19/23
*	Author: Mark J. Chin
*
*
********************************************************************************
	
	cd "~/Desktop/staggered_dd"

********************************************************************************
* Load the data from Chin (2023) and keep a subset
********************************************************************************

	/*
	use "~/Dropbox/consolidation/clean/lea_analysis_file.dta", clear
	keep year first_year cfips tcurssvc treat
	rename tcurssvc district_services_exp_pp
	label var year "Academic Year - Spring"
	label var first_year "First Year post consolidation of districts; 0 for never treated"
	label var cfips "County FIPS code"
	label var district_services_exp_pp "Current Expenditures on District Services Per Pupil CPI Adjusted"
	label var treat "County with consolidated districts"
	save "consolidation_dd_example.dta", replace
	*/
	
********************************************************************************
* Load the data and prep it
********************************************************************************

	use "consolidation_dd_example.dta", clear
	
		// Create a post indicator and event time variable
		gen diff = year-first_year
		gen post = diff>=0
		replace post = 0 if treat==0
		gen posdiff = diff+3
		replace posdiff = . if diff<-3 | diff > 12
		replace posdiff = 100 if treat==0
		
		// Will focus primarily on time periods from -3 to 12 because of balance of data
		// time period 0 is treatment period 1

********************************************************************************
* Simple TWFE
********************************************************************************

	// DD
	reghdfe district_services_exp_pp post if inrange(diff,-3,12)|treat==0, absorb(cfips year) vce(cluster cfips)

	// Event Study (ES)
	reghdfe district_services_exp_pp ib2.posdiff, absorb(cfips year) vce(cluster cfips)
	coefplot, keep(*posdiff) 

	
********************************************************************************
* Gardner (2022) approach, two-stage DD
********************************************************************************

	//ssc install did2s
	
	preserve
		keep if inrange(diff,-3,12) | treat==0
		
		// Set up
		replace first_year = . if first_year == 0 	
		assert post == 0 if mi(first_year)
			
		// DD
		did2s district_services_exp_pp, ///
			first_stage(i.cfips i.year) second_stage(post) treatment(post) cluster(cfips)
			
		// ES
		did2s district_services_exp_pp, ///
			first_stage(i.cfips i.year) second_stage(ib100.posdiff) treatment(post) cluster(cfips)	
	restore


********************************************************************************
* Callway & Sant'Anna (2021) approach
********************************************************************************
		
	//ssc install csdid
	
	preserve
		
		// Set up
		assert first_year==0 if treat==0
		
		// Base model code
		csdid district_services_exp_pp, i(cfips) t(year) g(first_year) 
		
		// DD
		estat simple
		
		// ES
		estat event
		
		csdid_plot // CSDID code to plot results
		
	restore
		
		
********************************************************************************
* Josh Bleiberg's Stacked DD code
********************************************************************************
	
	//ssc install stackedev
	
	preserve
		
		// Set up
		replace first=. if treat==0
		gen never=treat==0
		
		assert post==0 if never

		replace posdiff=2 if treat==0
		tab posdiff, gen(rel_)
		
		rename rel_3 ref // Reference year
		
		// DD
		stackedev district_services_exp_pp post, cohort(first_year) time(year) ///
			never_treat(never) unit_fe(cfips) clust_unit(cfips) 
			
		// ES
		stackedev district_services_exp_pp rel_* ref, cohort(first_year) time(year) ///
			never_treat(never) unit_fe(cfips) clust_unit(cfips) 
			
	restore

********************************************************************************
* Mark Chin's Stacked DD code
********************************************************************************
	
	// Setup
	drop diff post posdiff 
	
		// Create a Group ID for treatment units that is sequential
		// levelsof could also be used if you want to skip this step
		preserve
			keep if treat==1
			egen group =group(cfips)
			keep cfips group
			duplicates drop
			tempfile temp
			save `temp'
		restore
		merge m:1 cfips using `temp', assert(1 3) nogen

		// Loop over all Group IDs
			// Keep the treated unit
			// Keep all non-treated units
				// Assign them the treatment year of the treated unit
			// Append the two subdatasets together
			
		sum group
		local max = r(max)
		forvalues n=1/`max' {
			preserve
				keep if group==`n'
				sum first_year
				local trtyr = r(max)
				tempfile temp
				save `temp'
			restore
			preserve
				keep if treat==0
				replace first_year = `trtyr'
				replace group = `n'
				append using `temp'
				tempfile n`n'
				save `n`n''
			restore
		}
		
		// Append the stacked data together
		use `n1', clear
		forvalues n=2/`max' {
			append using `n`n''
		}
	
	gen diff = year-first_year
	keep if inrange(diff,-3,12) 
	gen post = diff>=0
	gen diffpl = diff+3
	
	// DD
	reghdfe district_services_exp_pp post##treat, absorb(i.cfips i.year) vce(cluster cfips)	

	// ES
	reghdfe district_services_exp_pp ib2.diffpl##treat, absorb(cfips year) vce(cluster cfips)	
	
