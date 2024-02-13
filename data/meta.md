# meta for lodgepolethermaltime/data

This folder contains climate data needed to model phenology, parent tree data, and distribution data for mapping.
Phenology data is in the "flowers" R package

## forcing
- `dailyforc_1945-2012.csv` 
contains daily temperatures for all seed orchard and comparison sites.
- `LodgepoleSPUs.csv`
Summarizes info on lodgepole SPUs from the BC orchard summary. Units for elevation are meters. Information from Forest Genetics Council of British Columbia Business Plan & BC orchard summary (Jan 07 11).xlsx compiled by Jack Woods in 2011. See also `phd/data/OrchardInfo`
- `normalforc_1901-2100.csv`
forcing and mean temperatures for all seed orchard and comparison sites over 30 year normal periods

## latifoliaDistribution
distribution data for mapping

`parents.csv`
Information on Parent Trees extracted from SPAR in May 2014.

Got information on parent trees from SPAR with Susan Zedel's help. 
The file with the geographic information is the ParentTreeExtractReport_ParentTrees. When I connect orchard trees to parent trees, I want to use the Parent Tree Number.

`typical_year_forc.csv`
forcing and mean temperatures for all seed orchard and comparison sites averaged over 1945-2011
