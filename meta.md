# meta

This analysis models the mean forcing accummulation required for lodgepole pine to shed pollen and receive pollen.

- `conceptualanalysis.*` goes thru the first few steps of Betancourt's workflow, outlining the problem and determining what domain specific knowledge can be brought to bear on the priors.
- `diagnositics.R` calculates model diagnostics like Rhat and ESS
- `doy.R`
- `phenology.R` runs the stan model `phenology.stan` four times with different inputs for male start, female start, male end, and female end.
- `phenology_functions.R` are helper functions for `phenology.R`
- `ppc.R`  
- `retrodiction_prediction.R`
- `simulatefrommodel.R`
- `*simulations*` simulate from model with known pars and then see if you can fit the model and recover pars.
- `cloneeffectexplore.R` exploratory analysis of phenology & provenance vars - graphs of genotype effects vs. all clim vars
- `provclimatecorr.R` pearson correlations of provenance effects and provenance climates [obsolete - only 6 provenances and not true provenances]
- `cloneinvest.R` how many clones are from locations where multiple clones were sourced from?

Order of analysis is

- conceptual

Data is prepared with `combine_phenology_and_heatsum.R`. Climate data is from `PCIC_all_seed_orchard_sites_adjusted.csv` in the `lodgepole_climate` project. Phenology data is from the `flowers` project.

#####

Modular analysis scripts write out objects needed in other scripts or for graphs and tables in `objects` folder
- `thermaltimemodel.R` thermal time models of flowering events in stan
  - stan code in `[sex]_[event].stan`
  - model output in `[sex]_[event].rds`
- `obsVSretro.R` compare observations to retrodictions
- `censoring.R` end vs interval censoring: what proportion of data is censored end vs. interval?
- `factororder.R` order factors for making good graphs
- `modelparameters.R` extract parameter values from models
- `retrodictandpredict.R` predict thermal time for events from models and translate into day of year historical and future - possibly defunct? See `obsVSretro.R` & `predict.R`
- `floweringlength.R` length of flowering period
- `overlap.R` calculate historical and future overlap of phenological periods
- `variation.R` variation in flowering period

- `provenancemodel.R` model genotype effect as function of genotype provenance climate
- `provenancemodelanalysis.R` extracts and saves parameters from genotype provenance climate model 

`graphsandtables.R` contains code for graphs and tables. relies on objects in `objects` folder created by modular analysis scripts
