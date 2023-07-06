# Order factors for plotting
#
# depends
library(dplyr)

siteclim <- read.csv("../lodgepole_climate/processed/PNWNAmet_adjusted.csv")
provclim <- read.csv("../phd/data/OrchardInfo/lodgepole_SPU_climsum.csv")
spudat <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv", header = TRUE, stringsAsFactors = FALSE)

phenf <- readRDS("objects/phenf.rds")

# order site, prov, and year levels by MAT
siteMAT <- siteclim %>%
  mutate(Year = lubridate::year(Date)) %>%
  group_by(Site) %>%
  summarise(MAT = mean(mean_temp_corrected)) %>%
  arrange(MAT)

sitefactororder <- siteMAT$Site

# provMAT <- provclim %>% select(SPU_Number, MAT) %>%
#   full_join(spudat) %>%
#   filter(SPU_Name %in% unique(phenf$Provenance)) %>%
#   select(MAT, SPU_Name) %>%
#   distinct() %>%
#   arrange(MAT)
#
# provfactororder <- provMAT$SPU_Name %>%
#   stringr::str_replace_all(pattern = "\\s", replacement = "\\.")  # format names to work with stan output

yearMAT <- siteclim %>%
  mutate(Year = lubridate::year(Date)) %>%
  right_join(data.frame(Year = as.numeric(unique(phenf$Year)))) %>%
  group_by(Year) %>%
  summarise(MAT = mean(mean_temp_corrected)) %>%
  arrange(MAT)

yearfactororder <- yearMAT$Year

factororder <- list(site = sitefactororder, year = yearfactororder)
saveRDS(factororder, "objects/factororder.rds")
saveRDS(factororder, "../flowering-cline/objects/factororder.rds")

