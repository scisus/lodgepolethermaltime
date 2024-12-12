# meta

This analysis models the mean forcing accummulation required for lodgepole pine to shed pollen and receive pollen.



- `doy.R`
  

- `simulatefrommodel.R`
- `*simulations*` simulate from model with known pars and then see if you can fit the model and recover pars.
- `cloneeffectexplore.R` exploratory analysis of phenology & provenance vars - graphs of genotype effects vs. all clim vars
- `provclimatecorr.R` pearson correlations of provenance effects and provenance climates [obsolete - only 6 provenances and not true provenances]
- `cloneinvest.R` how many genotypes are from locations where multiple genotypes were sourced from?

Order of analysis is

- conceptual

Climate data is from `../processed/PNWNAmet_adjusted.csv` in the `lodgepole_climate` project. Phenology data is from the `flowers` package.

#####

Modular analysis scripts write out objects needed in other scripts or for graphs and tables in `objects` folder
- `calc_forcing.R` calculate forcing and sum forcing for daily weather data
- `model_dev/conceptualanalysis.*` goes thru the first few steps of Betancourt's workflow, outlining the problem and determining what domain specific knowledge can be brought to bear on the priors
- `phenology_functions.R` are helper functions for `thermaltimemodel.R`
- `thermaltimemodel.R` thermal time models of flowering events in stan
  - stan code in `[sex]_[event].stan`
  - model output in `[sex]_[event].rds`
- `diagnositics.R` calculates model diagnostics like Rhat and ESS
- `censoring.R` end vs interval censoring: what proportion of data is censored end vs. interval?
- `factororder.R` order factors for making good graphs
- `replication_points.R` levels of replication in data. used to build graph.
- `obsVSretro.R` compare observations to retrodictions
- `modelparameters.R` extract parameter values from thermal time model
- `effectinvestigation_genotype.R` check the relationship between genotype effects estimated in the model and site MATs to make sure site climate/latitude isn't getting picked up in the genotype effect par
- `predict.R` predict thermal time for events from models 
- `dayofyear_translation.R` translate predictions into day of year 
- `doyanalysis.R` investigate patterns of flowering across sites and between provs
- `model_check_independentdata.R` check model predictions against data in O'Reilly 1988 and Nilsson 1981
- `owens2005comp.R` calculate GDD for dates reported in Owens 2005
- `site_climate_change.R` calculate monthly temperature over time. used for graph in supplemental materials
- `floweringlength.R` [DEFUNCT] length of flowering period
- `overlap.R` [DEFUNCT] calculate historical and future overlap of phenological periods [defunct]
- `variation.R` [DEFUNCT] variation in flowering period 

? genotypeinvest.R and matchgenotypeparent I think are trying to figure out which genotypes are associated with which breeding zone?

`graphsandtables.R` contains code for graphs and tables. relies on objects in `objects` folder created by modular analysis scripts
