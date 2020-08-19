# convert predictions to DoY

gclim <- read.csv("../phenolology/data/all_clim_PCIC.csv", header=TRUE, stringsAsFactors=FALSE) %>%
  filter(forcing_type=="ristos") %>%
  group_by(Site, Year) %>%
  arrange(Site, Year, DoY)

#THIS WORKS. This compares forcing units in tpars to sum_forcing in the climate dataset and finds the closest sum_forcing that's greater than a given tpars. Then it extracts they Day of Year that sum_forcing occured on. The additional "+ 1" that follows the findInterval call adds one to the index returned: findInterval by default returns the index of the left-hand (low) side of an interval, and we want the right-hand (high) side of the interval, which is the next index in the interval-vector from the indexes returned.


pf30

 <- filter(gclim, Site == "Kalamalka", Year==1997)
foo[findInterval(pf30$forcing[1], foo$sum_forcing +1), ]

gclim %>%
  summarise(rstart = DoY[findInterval(pf30$forcing[1], sum_forcing +1)])

splitclim <- split(clim, clim$siteyear)

ifinder <-  function(x) {
  index <- findInterval(predictphen$sum_forcing, x$sum_forcing)
  doy <- x$DoY[index]
  df <- data.frame(iter=predictphen$iter, DoY=doy, siteyear=x$siteyear[1],
                   side=predictphen$side, Sex=predictphen$Sex)
  return(df)
}

predictdoy <- purrr::map_df(splitclim, ifinder) %>%
  pivot_wider(names_from = side, values_from = DoY) %>% #expensive operation
  mutate(lengthdays = end - begin) %>%
  pivot_longer(cols=c(end, begin), names_to = "side", values_to = "DoY") %>%
  left_join(predictphen)