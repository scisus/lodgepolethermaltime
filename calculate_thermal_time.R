# Calculate forcing unit data for seed orchard sites using PCIC weather data data adjusted with ClimateNA data in lodgepole_climate project

# Calculate forcing unit data. Writes weather and forcing unit data to `all_clim_PCIC.csv`

# combine phenology and PCIC data
library(tidyverse)
library(lubridate)


forcingtype = "ristos" # choose forcing type for use in phendf output for modeling with stan


# Data --------------------------------------------------------------------


climdat <- read.csv("../lodgepole_climate/processed/PCIC_all_seed_orchard_sites_adjusted.csv", header = TRUE, stringsAsFactors=FALSE) %>%
    mutate(DoY = yday(Date)) %>%
    rename(mean_temp = mean_temp_corrected) # use mean temps that PCIC raw with a ClimateNA correction

# Functions ----------------------------------

#function to calculate heat sum using a linear threshold model of heat sum accumulation. If mean temp for the day is below a given threshold, no heat is accumulated. If mean temp for the day is above a given threshold, the heat accumulated = mean temp - threshold temp. This requires a dataframe with MeanTempC columns (Mean temperature, numeric) and Year and DoY (Day of Year) columns. threshold temp is a number. Returns a df with year, day of year, daily heat and heatsum columns

calculate_GDD <- function(climate_df, threshold_temp) {
    # calculate heat added on a given day
    # days where no heat is added bc threshold temp not reached
    no_heat <- climate_df %>%
        filter(mean_temp < threshold_temp) %>%
        dplyr::mutate(Heat = 0)
    # days where heat is added bc threshold temp is reached
    heat <- climate_df %>%
        filter(mean_temp >= threshold_temp) %>%
        dplyr::mutate(Heat = mean_temp - threshold_temp)

    clim <- rbind(no_heat, heat)
    clim <- clim %>%
        dplyr::arrange(Site, Year, DoY) %>%
        dplyr::group_by(Site, Year) %>%
        dplyr::mutate(Heatsum = cumsum(Heat)) %>% # add heatsum
        dplyr::select(Site, Year, DoY, Heat, Heatsum)

    return(clim)
}

#calculate forcing units according to Sarvas 1972/4/Hanninen 1990/Chuine ForcSar 1999
calculate_ristos <- function(climate_df) { #climatedf is a dataframe with a column named "mean_temp" containing mean temperatures in Celcius. columns named Site, Year, and DoY must also exist
    #calculate the amount of forcing any mean daily temperature provices
    climate_df$ristos <- 28.4/(1+exp(-0.185*(climate_df$mean_temp-18.4)))
    #sum the forcing units
    clim <- climate_df
    clim <- clim %>%
        dplyr::arrange(Site, Year, DoY) %>%
        dplyr::group_by(Site, Year) %>%
        dplyr::mutate(sum_ristos = cumsum(ristos)) %>%
        dplyr::select(Site, Year, DoY, mean_temp, ristos, sum_ristos)
    return(clim)
}

#calculate forcing units according to Sarvas 1972/4/Hanninen 1990/Chuine ForcSar 1999, but with a numerator of 1 since forcing units are arbitrary
# calculate_scaled_ristos <- function(climate_df) {
#     #calculate the amount of forcing any mean daily temperature provices
#     climate_df$ristos_scaled <- 1/(1+exp(-0.185*(climate_df$mean_temp-18.4)))
#     #sum the forcing units
#     clim <- climate_df
#     clim <- clim %>%
#         dplyr::arrange(Site, Year, DoY) %>%
#         dplyr::group_by(Site, Year) %>%
#         dplyr::mutate(sum_scaled_ristos = cumsum(ristos_scaled)) %>%
#         dplyr::select(Site, Year, DoY, mean_temp, ristos_scaled, sum_scaled_ristos)
#     return(clim)
# }

# Calculate heatsum -----------------


# Climate data processing
clim <- mutate(climdat, Year = year(Date))

# Calculate Forcing Units and Forcing sums

risto <- calculate_ristos(climate_df = clim)
clim5 <- calculate_GDD(clim, threshold_temp = 5)

clim <- dplyr::full_join(risto, clim5)

#check nothing weird happened
#very slow
# ggplot(clim, aes(x=DoY, y=Heatsum, color = Year)) +
#     geom_point() +
#     facet_wrap("Site") +
#     theme_bw(base_size=20) +
#     ggtitle("Heatsum Accumulation 1997-2012") +
#     theme(legend.position="none")
# 
# ggplot(filter(clim, DoY<180), aes(x=DoY, y=sum_scaled_ristos, color=Year)) +
#     geom_point() +
#     facet_wrap("Site") +
#     theme_bw(base_size=18) +
#     ggtitle("Risto Accumulation Jan-June 1997-2012") +
#     theme(legend.position="none")
# 
# ggplot(clim, aes(x=ristos, y=Heat)) +
#     geom_point() +
#     geom_abline(slope=1, intercept=0)
# 
# #present - how heat and gdd compare
# ggplot(clim, aes(x=mean_temp, y=ristos, color="ristos")) +
#     geom_line(size=2) +
#     geom_line(size=1.1, alpha=0.9, aes(x=mean_temp, y=Heat, color="GDD")) +
#     theme_bw(base_size=20) +
#     scale_color_viridis_d(option="D", end=0.6) +
#     ylab("Forcing Units") +
#     xlab("Mean Daily Temperature") +
#     theme(legend.position = "top")

# Tidy forcing unit df ##########

#first split into three data frames

ristoframe <- clim %>%
    dplyr::select(-Heat, -Heatsum) %>%
    mutate(forcing_type = "ristos")
colnames(ristoframe)[c(5,6)] <- c("forcing", "sum_forcing")


gddframe <- clim %>%
    dplyr::select(-contains("ristos")) %>%
    mutate(forcing_type = "gdd")
colnames(gddframe)[c(5,6)] <- c("forcing", "sum_forcing")

#recombine

gclim <- rbind(ristoframe, gddframe)

#tests
nrow(gclim) == nrow(ristoframe) + nrow(gddframe)


# Plot climate data ####################
climdat$Year <- lubridate::year(climdat$Date)
climdat$Month <- lubridate::month(climdat$Date)

#by site
# ggplot(climdat, aes(x=as.factor(Month), y=mean_temp, fill=Site, color=Site)) +
#     geom_violin(alpha=0.8) +
#     xlab("Month") +
#     theme_bw(base_size=20) +
#     #facet_wrap("Site") +
#     theme(legend.position = "top") +
#     scale_fill_viridis_d(option="A", end=0.9)+
#     scale_color_viridis_d(option="A", end=0.9) +
#     ggtitle("Mean Temperature 1997-2011") +
#     ylab("Temperature (°C)")
#
#overall
# ggplot(climdat, aes(x=DoY, y=mean_temp)) +
#     geom_point(pch=1, alpha=0.1)+
#     theme_bw(base_size=20) +
#     ylab("Temperature (°C)") +
#     ggtitle("Mean Temperatures 1997-2011")


write.csv(gclim, "data/all_clim_PCIC.csv", row.names=FALSE)

## Checks ################

