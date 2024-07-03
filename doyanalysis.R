results <- doy_annual_pp_sum %>%
  group_by(Site, Sex, event) %>%
  arrange(DoY) %>%
  summarise(
    Earliest_Years = list(Year[1:10]),  # List of years with the 10 lowest DoYs
    Latest_Years = list(Year[(n()-9):n()]),  # List of years with the 10 highest DoYs
    .groups = 'drop'  # Drop the grouping structure afterward
  ) %>%
  unnest(c(Earliest_Years, Latest_Years)) %>%
  arrange(Earliest_Years) %>%
  print()


rank_years <- doy_annual_pp_sum %>%
  filter(MAT == -0.7) %>% #coldest mat only
  group_by(Sex, event, Site) %>%
  mutate(DoY_Rank = rank(DoY, ties.method = "min")) %>%
  ungroup() %>%
  filter(MAT %in% c(-0.7)) %>%
  select(-starts_with("."), -MAT_label, -MAT, -DoY)

wide_df <- rank_years %>%
  pivot_wider(names_from = Site, values_from = DoY_Rank)

library(corrr)
# Step 3: Calculate Kendall’s Tau correlation coefficient between sites
# We'll use the `correlate()` function from the `corrr` package which simplifies correlation matrix creation
correlation_matrix <- wide_df %>%
  select(-c(Sex, event, Year)) %>%  # Remove non-numeric columns
  correlate(method = "kendall")  # Use Kendall's tau for non-parametric rank correlation

# To see the correlation matrix
print(correlation_matrix)

# Optional: Focus on correlations for a specific Sex and Event
# For example, for FEMALE and begin event
female_begin_corr <- wide_df %>%
  filter(Sex == "FEMALE", event == "begin") %>%
  select(-c(Sex, event, Year)) %>%
  correlate(method = "kendall")

print(female_begin_corr)

rank_years <- doy_annual_pp_sum %>%
  filter(MAT == -0.7) %>%  # Filtering for coldest MAT only if still needed
  group_by(Sex, event, Site) %>%
  mutate(DoY_Rank = rank(DoY, ties.method = "min")) %>%
  ungroup() %>%
  select(Sex, event, Site, Year, DoY_Rank)  # Ensure these are the only columns

# Create the wide data frame
wide_df <- rank_years %>%
  pivot_wider(names_from = Site, values_from = DoY_Rank) %>%
  drop_na()

#Calculate Kendall’s Tau correlation for each combination of Sex and event
correlation_matrices <- wide_df %>%
  group_by(Sex, event) %>%
  nest() %>%
  mutate(correlation_matrix = map(data, ~ correlate(.x %>% select(-Year), method = "kendall", use = "pairwise.complete.obs"))) %>%
  select(-data)
# Print correlation matrices
correlation_matrices %>% pull(correlation_matrix)

longcorr <- correlation_matrices %>%
  unnest(correlation_matrix) %>%
  pivot_longer(cols = -c(Sex, event, term),
               names_to = "Site2",
               values_to = "Correlation") %>%
  rename(Site1 = "term") %>%
  ungroup() %>%
  mutate(Site1 = forcats::fct_relevel(Site1, seedorchardsites), Site2 = forcats::fct_relevel(Site2, seedorchardsites))

  # Create the heatmap
ggplot(longcorr, aes(x = Site1, y = Site2, fill = Correlation)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C", direction = -1) +
  labs(fill = "Kendall's Tau") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(Sex ~ event) +
  xlab("") +
  ylab("")

# variation in day of year ####

doysd <- doy_annual_pp_sum %>%
  group_by(MAT, Site, event, Sex) %>%
  summarise(sd = sd(DoY))

ggplot(doysd, aes(x = Site, y = sd, colour = MAT)) +
  geom_point() +
  facet_grid(Sex ~ event)

## lower for warmer provs at all sites for male begin, but no consistent patterns, not even in gaps (basically 0 to huge)

# avg difference in doy for flowering between coldest and warmest prov at each site

warmvscold <- doy_annual_pp_sum %>%
  select(-starts_with("."), -MAT_label) %>%
  pivot_wider(names_from = MAT, names_prefix = "MAT", values_from = DoY) %>%
  mutate(diff = MAT6.8 - `MAT-0.7`) %>%
  select(-starts_with("MAT")) %>%
  group_by(Site, event, Sex) %>%
  summarise(mean_doy_diff = mean(diff), sd_diff = sd(diff))
saveRDS(warmvscold, "objects/warmvscold.rds")

ggplot(warmvscold, aes(x = Site, y = mean_doy_diff, colour = Sex, shape = event)) +
  geom_point(position = position_dodge(0.2), size = 3) +
  geom_errorbar(aes(ymin = mean_doy_diff - sd_diff, ymax = mean_doy_diff + sd_diff),
                width = 0.2,   # Controls the width of the horizontal lines at the top and bottom of the error bars
                position = position_dodge(0.2)) +
  theme_bw() +
  ylab("Mean difference (days)")
#mean difference between coldest and warmest provenance
