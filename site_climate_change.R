# check for trends in temperature over time

library(dplyr)
library(purrr)
library(stringr)


file_list <- list.files(path = "../lodgepole_climate/data/climateBC/historical normals/", pattern = "*.csv", full.names = TRUE)
factororder <- readRDS("objects/factororder.rds")


# Read each file, adding a 'source_file' column with only the year range
climate_normals_data <- map_dfr(file_list, function(file) {
  # Extract the year range from the filename
  year_range <- str_extract(basename(file), "\\d{4}_\\d{4}") %>% str_replace("_", "-")
  data <- read.csv(file)
  data %>% mutate(normal_period = year_range)
})

climate_normals <- climate_normals_data %>%
  filter(id == "site") %>%
  select(-starts_with("PPT"), -starts_with("Rad"), -starts_with("Tmin"), -starts_with("Tmax")) %>%
  mutate(enddate = str_extract(normal_period, "\\d{4}$")) %>%
  pivot_longer(cols = starts_with("Tave"), names_to = "Month", values_to = "Tave") %>%
  arrange(enddate) %>%
  mutate(season = ifelse(Month %in% c("Tave03", "Tave04", "Tave05", "Tave06"), "March-June", "other"),
         enddate = as.numeric(enddate),
         normal_period = fct_reorder(normal_period, enddate),
         Site = factor(Site, levels = factororder$site))

saveRDS(climate_normals, "objects/monthly_climate_normals.rds")

