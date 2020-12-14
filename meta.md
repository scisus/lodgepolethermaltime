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

Order of analysis is

- conceptual
- simulations

Data is prepared with `combine_phenology_and_heatsum.R`. Climate data is from `PCIC_all_seed_orchard_sites_adjusted.csv` in the `lodgepole_climate` project. Phenology data is from the `flowers` project.