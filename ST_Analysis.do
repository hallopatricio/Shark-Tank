cd "YOUR PATH"
clear all
cls

use "ST_cleaned_data_STATA.dta", clear 

**Variable creation

*competition between sharks variable
g shark_compet=	.
replace shark_compet = 1 if OfSharksWhoWon < OfSharksWhoBid & OfSharksWhoWon > 0
replace shark_compet = 0 if OfSharksWhoWon == OfSharksWhoBid & OfSharksWhoBid > 0
replace shark_compet = 0 if OfSharksWhoBid==1
*missing values are offer received but rejected (no deal)

*bid dynamics
g bid_dynamics =.
replace bid_dynamics = 0 if Offer==0
replace bid_dynamics = 1 if Offer==1 & MoreThan1SharkBid ==0
replace bid_dynamics = 2 if Offer==1 & MoreThan1SharkBid ==1 & shark_compet ==0 
replace bid_dynamics = 3 if Offer==1 & MoreThan1SharkBid ==1 & shark_compet ==1 

*TABLE 1
estpost summarize , detail
esttab  , ///
	nonumbers replace  ///
	cell("count(label(N)) mean(label(Mean) fmt(2)) p50(label(Median) fmt(2)) sd(label(Std. Dev.) fmt(2)) skewness(label(Skewness) fmt(2)) kurtosis(label(Kurtosis) fmt(2)) min(label(Min) fmt(2)) max(label(Max) fmt(2))") ///
	label noobs gaps 	
	
tabulate bid_dynamics, generate(bid_dum)
sum bid_dum*, detail

tabulate Education, generate(educ_dum)
sum educ_dum*, detail
	

** gender difference
ttest Offer if mixed_team==0, by(female) unequal
ttest Accepted if mixed_team==0, by(female) unequal

g gender_cat = .
replace gender_cat = 1 if female==0 & mixed_team==0
replace gender_cat = 2 if female==1 & mixed_team==0
replace gender_cat = 3 if mixed_team==1
total AcceptedInvestment_mio , over(gender_cat)
	
**** Likelihood to receive an offer
***********************************

* FIGURE 1 	
graph twoway (histogram OpenEquity, width(0.05) fcolor(bluishgray) lcolor(ltbluishgray)) ///
	(kdensity OpenEquity if Offer ==0, lwidth(medthick) lpattern(dash) lcolor(navy)) ///
	(kdensity OpenEquity if Offer ==1, lwidth(medthick) lcolor(midblue)) , ///
	xtitle("Open equity % offered to sharks", margin(medium) size(small)) ///
	ytitle("Density", margin(medium) size(small)) ///
	graphregion(fcolor(white)) xmtick(##2) xlabel(#10, valuelabel)   ///
	legend(order( 1 "Histogram" 2 "No offer" 3 "Offer") position(1) ring(0) col(1))
	

*Kolmogorov-Smirnov test 	
ksmirnov OpenEquity, by(Offer) exact


*variables of interest (from CART)
global controls OpenEquity OpenInvestment_mio i.PatentStatus i.TimeDevoted female mixed_team TeamSize i.Education
global season_FE i.season
global episode_FE i.episode_season


** Estimation
probit Offer OpenEquity OpenInvestment_mio, vce(cluster episode_general)
	estadd scalar r2p = e(r2_p)
	estadd scalar clust = e(N_clust)
	estat ic
	matrix IC = r(S)
	estadd scalar aic = IC[1,5]
	estadd scalar bic = IC[1,6]
	capt drop y_aag2
	predict y_aag2, p
	quietly su y_aag2
	estadd scalar pr = r(mean)
	quietly roctab Offer  y_aag2  , nograph
	estadd scalar roc = r(area)
	estpost margins , dydx(*)
	est store mfx_probit_offer0	

probit Offer $controls $season_FE $episode_FE if Badidea ==0, vce(cluster episode_general)
	estadd scalar r2p = e(r2_p)
	estadd scalar clust = e(N_clust)
	estat ic
	matrix IC = r(S)
	estadd scalar aic = IC[1,5]
	estadd scalar bic = IC[1,6]
	capt drop y_aag2
	predict y_aag2, p
	quietly su y_aag2
	estadd scalar pr = r(mean)
	quietly roctab Offer  y_aag2  , nograph
	estadd scalar roc = r(area)
	estpost margins , dydx(*)
	est store mfx_probit_offer_BM	
	
probit Offer $controls $season_FE $episode_FE Duration if Badidea ==0, vce(cluster episode_general)
	estadd scalar r2p = e(r2_p)
	estadd scalar clust = e(N_clust)
	estat ic
	matrix IC = r(S)
	estadd scalar aic = IC[1,5]
	estadd scalar bic = IC[1,6]
	capt drop y_aag2
	predict y_aag2, p
	quietly su y_aag2
	estadd scalar pr = r(mean)
	quietly roctab Offer  y_aag2  , nograph
	estadd scalar roc = r(area)
	estpost margins , dydx(*)
	est store mfx_probit_offerBM2
	

*robustness check with # sharks
reg OfSharksWhoBid OpenEquity OpenInvestment_mio i.PatentStatus i.TimeDevoted ///
	female mixed_team TeamSize i.Education $season_FE $episode_FE if Badidea==0, vce(cluster episode_general)
xi: collin OpenEquity OpenInvestment_mio i.PatentStatus i.TimeDevoted ///
	female mixed_team TeamSize i.Education $season_FE $episode_FE if !missing(OfSharksWhoBid) & Badidea==0
	estadd scalar VIF = r(m_vif)
	estadd scalar clust = e(N_clust)
est store reg_sharks_BM	
	

** TABLE 2 (part I)	
esttab mfx_probit_offer0 mfx_probit_offer_BM mfx_probit_offerBM2 reg_sharks_BM ///
	, replace  ///
	starlevels(* 0.1 ** 0.05 *** 0.01) ///
	cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) label ///
	stats(N N_cens nb_rep clust  r2_a r2p aic bic roc chi2, fmt(0 0 0 0 2 2 2 2 2 2 ) ///
	labels(`"Observations"' `"Number censored"' `"# Bootstrap reps"' `"Nb. clusters"' `"Adjusted R-squared"' `"Pseudo R-squared"' `"AIC"' `"BIC"' `"ROC"' `"Chi2"')) ///
	varlabels(_cons "Constant") drop(0.PatentStatus 0.TimeDevoted *.season *.episode_season) ///
	indicate("Season FE = 2.season" "Episode FE = 2.episode_season")	

	
**** Negotiation
****************

** Stat desc.
sum best_offertoopen, detail
ttest best_offertoopen if Offer==1, by(shark_compet)

*valuation change
g value_change = .
replace value_change = best_accepted_value_mio - OpenValue_mio if Offer ==1

*equity and investment change
g equity_change = .
replace equity_change = best_accepted_equity - OpenEquity if Offer ==1
g invest_change = .
replace invest_change = best_accepted_invest - OpenInvestment_mio if Offer ==1	

*TABLE 3
sum value_change, det
	sum value_change if Accepted ==1, det

sum equity_change	invest_change , detail
bysort Accepted: sum equity_change	invest_change , detail

tab equity_change Accepted , column
ttest equity_change ==0
	ttest equity_change ==0 if Accepted ==1
	
sum invest_change if invest_change<1 , detail
tab invest_change Accepted, col
ttest invest_change ==0
	ttest invest_change ==0 if Accepted ==1 	
	

*FIGURE 3
graph twoway (scatter equity_change invest_change if invest_change<10 & Accepted==1, msize(large) msymbol(circle_hollow) mcolor(ltblue)) ///
	(scatter equity_change invest_change if invest_change<10 & Accepted==0 , msymbol(x) msize(medlarge) mcolor(cranberry)), ///
	xtitle("Change in investment money (mio USD)", margin(medium) size(small)) ///
	ytitle("Change in equity % (percentage point)", margin(medium) size(small)) ///
	graphregion(fcolor(white)) xmtick(##2) xlabel(-0.2(0.1)1, valuelabel) xscale(reverse) ///
	ymtick(##2) ylabel(-0.4(0.1)1, valuelabel) ///
	legend(order( 1 "Accepted deal" 2 "Rejected by entrepreneur") rows(1) region(fcolor(none)) ) 	

	*Number of cases per quadrant:
	*entrepreneur gain
	tab Accepted if equity_change <=0 & invest_change>0  & invest_change<. | equity_change <0 & invest_change>=0  & invest_change<.
		tab Accepted if equity_change ==0 & invest_change==0 
	*investor gain
	tab Accepted if equity_change >=0 & invest_change<0  | equity_change >0 & invest_change<=0

	*top right:
	tab Accepted if equity_change >0 & invest_change>0	 & invest_change<.
	*top left:
	tab Accepted if equity_change <0 & invest_change<0		
	
	
* FIGURE 4 (hisogram removing outliers + densities for open equity +/- 15% (the median))
graph twoway (histogram value_change if value_change <5 & value_change>-5, width(0.1) fcolor(bluishgray) lcolor(ltbluishgray)) ///
	(kdensity value_change if OpenEquity>0.15 & value_change <5 & value_change>-5, lwidth(medthick) lpattern(dash) lcolor(navy)) ///
	(kdensity value_change if OpenEquity <=0.15 & value_change <5 & value_change>-5, lwidth(medthick) lcolor(midblue)) , ///
	xtitle("Valuation change in million USD (best offer from sharks - initial)", margin(medium) size(small)) ///
	ytitle("Density", margin(medium) size(small)) ///
	graphregion(fcolor(white)) xmtick(##2) xlabel(#10, valuelabel)   ///
	legend(order( 1 "Histogram" 2 "Open equity >0.15" 3 "Open equity <=0.15") size(small) col(3))

* FIGURE 5
graph twoway (histogram best_offertoopen if best_offertoopen <3, width(0.05) fcolor(bluishgray) lcolor(ltbluishgray)) ///
	(kdensity best_offertoopen if bid_dynamics <3 & best_offertoopen <3, lwidth(medthick) lpattern(dash) lcolor(sand)) ///
	(kdensity best_offertoopen if bid_dynamics ==3 & best_offertoopen <3, lwidth(medthick) lcolor(cranberry)) , ///
	xtitle("Best offer to open ratio", margin(medium) size(small)) ///
	ytitle("Density", margin(medium) size(small)) ///
	graphregion(fcolor(white)) xmtick(##2) xlabel(#10, valuelabel)   ///
	legend(order( 1 "Histogram" 2 "No competition" 3 "Competition") positio(1) ring(0) col(1)) 
	
ksmirnov best_offertoopen, by(shark_compet) exact // signif
	

* Estimation
set seed 1010
scalar nb_boot = 1000
	
heckman best_offertoopen i.bid_dynamics $controls $season_FE $episode_FE ///
	if Badidea==0 , ///
	select(Offer = OpenEquity OpenInvestment_mio i.PatentStatus i.TimeDevoted female mixed_team TeamSize i.Education $season_FE $episode_FE) twostep ///
	nolog vce(bootstrap, reps(`=nb_boot') seed(1010))
	estadd scalar nb_rep = e(N_reps)
est store ratio_BM	

preserve
keep if best_offertoopen==. | best_offertoopen<=2
heckman best_offertoopen OpenEquity i.bid_dynamics TeamSize i.Education $controls $season_FE $episode_FE ///
	if Badidea==0 , ///
	select(Offer = OpenEquity OpenInvestment_mio i.PatentStatus i.TimeDevoted female mixed_team TeamSize i.Education $season_FE $episode_FE) twostep ///
	nolog vce(bootstrap, reps(`=nb_boot') seed(1010))
	estadd scalar nb_rep = e(N_reps)
est store ratio_BM2_out
restore


** TABLE 2 (part II)	
esttab ratio_BM ratio_BM2_out  ///
	, replace  ///
	starlevels(* 0.1 ** 0.05 *** 0.01) ///
	cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) label ///
	stats(N N_cens nb_rep clust  r2_a r2p aic bic roc chi2 rho, fmt(0 0 0 0 2 2 2 2 2 2 2) ///
	labels(`"Observations"' `"Number censored"' `"# Bootstrap reps"' `"Nb. clusters"' `"Adjusted R-squared"' `"Pseudo R-squared"' `"AIC"' `"BIC"' `"ROC"' `"Chi2"')) ///
	varlabels(_cons "Constant") drop(0.PatentStatus 0.TimeDevoted *.season *.episode_season 1.bid_dynamics 0.Education) ///
	indicate("Season FE = 2.season" "Episode FE = 2.episode_season")	
 