# how much does phenology vary year to year?
#
doyvar <- specific_doy_preds %>%
group_by(Sex, event, Site, Year, Provenance, Genotype, Tree) %>%
  summarise(sd = sd(newdoycol)) %>%
  median_hdci(sd, .width = c( 0.5, 0.89))
