library(flowers)
library(dplyr)
library(brms)
library(bayesplot)

source('phenology_functions.R')

# add start and end censorship to phenology data

phen <- flowers::phenology %>%
  distinct()


# infer phenophases 1 and 3 for walsh
walsh <- filter(phen, Source == "Chris Walsh")

## build a dataframe with all possible dates for Walsh data given Site/Year/Orchard observations
doys <- walsh %>%
  select(Site, Year, Orchard, DoY, Date) %>%
  distinct()

individualmeta <- walsh %>%
  select(-DoY, -Date, -contains("Phenophase")) %>%
  distinct()

walsh_grid <- full_join(doys, individualmeta) %>%
  arrange(Index, DoY, Date)

inferred_walsh <- walsh_grid %>%
  mutate(Phenophase_Inferred = case_when(DoY < First_RF ~ 1,
                                         First_RF <= DoY & DoY <= Last_RF ~ 2,
                                         Last_RF < DoY ~ 3)) %>%
  select(-First_RF, -Last_RF)

## merge in observations

full_walsh <- left_join(inferred_walsh, walsh)

### test proper merge

#did i drop or add data?
full_walsh <- left_join(inferred_walsh, walsh)
samefl <- walsh %>% filter(First_RF==Last_RF) %>% # which trees have the same first and last recorded flowering date
  group_by(Index) %>%
  summarise(phases = n()) %>% # recorded under different phenophases
  filter(phases > 1)
nrow(full_walsh) == nrow(inferred_walsh) + nrow(samefl)

# did I overwrite correct phenophase?
full_walsh %>%
  filter(Phenophase_Derived == 2) %>%
  summarise(comp = Phenophase_Inferred == Phenophase_Derived) %>%
  summarise(unique(comp))

# rename columns
full_walsh <- full_walsh %>%
  select(-Phenophase_Derived) %>%
  rename(Phenophase_Derived = Phenophase_Inferred)

## merge back into full phenology dataset
phen <- full_join(phen, full_walsh)

## filter dataset so that only the following 4 kinds of observations are retained
## 1) last recorded obs before flowering
## 2) first recorded flowering obs
## 3) last recorded flowering obs
## 4) first recorded finished flowering obs
phen_trim <- phen %>%
  group_by(Index, Phenophase_Derived, Year) %>%
  summarise(First = min(DoY), Last = max(DoY) ) %>%
  select(Index, Phenophase_Derived, First, Last) %>%
  tidyr::pivot_longer(cols = c(First, Last), names_to = "firstorlast", values_to = "DoY") %>%
  ungroup() %>%
  group_by(Index) %>%
  filter(Phenophase_Derived == 1 & firstorlast == "Last" |
           Phenophase_Derived == 2 |
           Phenophase_Derived ==3 & firstorlast == "First") %>%
  select(-firstorlast)

## Test - should have 0 rows since max 4 obs (see phentrim)
phen_trim %>%
  group_by(Index) %>%
  summarize(count = n()) %>%
  filter(count > 4)

### merge back in full dataset

phenbe <- phen %>%
  select(-DoY, -Phenophase, -Date, -Phenophase_Derived, -contains("_RF"), -Source) %>%
  distinct() %>%
  right_join(phen_trim)


#####
censorind <- add_censor_indicator(phenbe) # add a label for censorship type. all data is either end or interval censored

# add a y and y2 variable for interval data for brms
# for start data, y is the last date the tree was observed not flowering (in phase 1) and y2 is the observation date
# for end data, y is the last day the tree was observed flowering and y2 is the first day the tree was observed in phase 3





foo <- censorind %>%
  #filter(begin_censored == "interval" | end_censored == "interval")
  group_by(Index, Phenophase_Derived) %>%
  mutate(Last = max(DoY), First = min(DoY)) %>%
  filter(Phenophase_Derived == 2)



phenbe <- filter_start_end() # filter phenology data for only start and end dates

factors <- c("Site", "Provenance", "Year", "Clone")

fbdat <- select_data(phendat = phenbe, censordat = censorbegin, factors = factors, sex = "FEMALE", event = "begin") %>%
  mutate(sum_forcing_centered = sum_forcing - mean(sum_forcing))

# fit the simplest model possible (mean only)

mo <- brm(sum_forcing_centered ~ 1, data = fbdat)
summary(mo)
plot(mo)
mo_yrep <- posterior_predict(mo, draws = 500)

color_scheme_set("brightblue")
ppc_dens_overlay(fbdat$sum_forcing_centered, mo_yrep[1:50,])

loo_mo <- loo(mo)

# fit a model with censorship

moc <- brm(sum_forcing_centered | cens(censored) ~ 1, data = fbdat)
summary(moc)
plot(moc)
moc_yrep <- posterior_predict(moc, draws = 500)

color_scheme_set("purple")
ppc_dens_overlay(fbdat$sum_forcing_centered, moc_yrep[1:50,])

# compare
loo_moc <- loo(moc)

# fit a model with effects
msypc <- brm(sum_forcing_centered ~ 1 + (1|Site) + (1|Provenance) + (1|Year) + (1|Clone), data = fbdat, cores = 5)
summary(msypc)
plot(msypc, pars = )

loo_msypc <- loo(msypc)

# fit a model with censorship and effects
msypc_c <- brm(sum_forcing_centered | cens(censored) ~ 1 + (1|Site) + (1|Provenance) + (1|Year) + (1|Clone), data = fbdat, cores = 5)
summary(msypc_c)
msypc_c_yrep <- posterior_predict(msypc_c, draws = 500)

loo_msypc_c <- loo(msypc_c)
color_scheme_set("green")
ppc_dens_overlay(fbdat$sum_forcing_centered, msypc_c_yrep[1:50,])

# fit a model with left censorship AND interval censorship
#
brms::loo_compare(loo_msypc_c, loo_msypc, loo_moc, loo_mo)
