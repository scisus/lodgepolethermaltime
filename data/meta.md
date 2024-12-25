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

# Nilsson1981, oreilly-and-owens1988, and owens2005
All contain lodgepole flowering phenology data copied from other papers

Nilsson, J.-E., 1981. Flowering in Pinus contorta: a study of flowering abundance and flowering phenology of provenances from latitude 48⁰N to 63⁰N in a Swedish field trial. Swedish University of Agricultural Sciences, Dept. of Forest Genetics and Plant Physiology, Umeå.


O’Reilly, C., Owens, J.N., 1988. Reproductive growth and development in seven provenances of lodgepole pine. Can. J. For. Res. 18, 43–53. https://doi.org/10.1139/x88-008

Owens, J.N., Bennett, J., L’Hirondelle, S., 2005. Pollination and cone morphology affect cone and seed production in lodgepole pine seed orchards. Canadian Journal of Forest Research 35, 383–400. https://doi.org/10.1139/x04-176
