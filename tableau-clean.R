library(dplyr)
library(readr)

# Tableau can only show what's in the data... 
# If a coral only has one row in 2015 for example, then it disappears after 2015. By creating 6 rows per coral we force Tableau to keep drawing it for 6 years, 
#... it fades out with time


# Raw coral survey data
df <- read_csv("~/MEDS/capstone/eds-coral-data-storage-management/analysisdata/volume_corals.csv")
view(df)
# # of years coral remains visible after recruitment
fade_window <- 6

# Find first year each coral was observed 
# coral appears when it has a length measurement OR is flagged as recruited (dyn_recruitment = 1)
# take earliest such year as its first_seen_year
first_seen <- df %>%
  filter(!is.na(length) | dyn_recruitment == 1) %>%
  group_by(coral_number) %>%
  summarise(first_seen_year = min(year))

# Get one row per coral with its position and identity (we dont instead of 11 repeated rows)
# Each coral has 11 rows in raw data (one per survey year) but 
# position (x, y_m, z_m) and identity never change...need to deduplicate before
# expanding so don't accidentally multiply rows later
static <- df %>%
  select(coral_number, site, habitat, transect, transect_id,
         taxa, x, y_m, z_m) %>%
  distinct(coral_number, .keep_all = TRUE) %>% 
  left_join(first_seen, by = "coral_number")

# For each coral, create 6 rows (expand)... one per year it should be visible...with decreasing opacity each year
# ex. For a coral recruited in 2015, this creates rows for display_year =
# 2015, 2016, 2017, 2018, 2019, 2020. When Tableau's Pages shelf is on
# any of those years, the coral is present in the data and gets drawn.
# Without expansion, each coral would only appear in its recruitment
# year and vanish immediately (this is a big issue)
expanded <- static %>%
  rowwise() %>%
  do(data.frame(
    .,
    years_since = 0:(fade_window - 1)
  )) %>%
  ungroup() %>%
  mutate(
    display_year = first_seen_year + years_since,
    opacity_pct  = round(100 * (1 - years_since / FADE_WINDOW)),
    fade_label   = paste0("Year +", years_since)
  ) %>%
  # Drop rows beyond the last survey year
  filter(display_year <= 2024)

# Attach actual size measurement for whichever year coral was measured
# Match on both coral_number and display_year so that coral's size
# in 2016 reflects its actual 2016 measurement
# Should we use actual volume instead of size_t0 since size_t0 is  volume at  start of each transition period?
# Use true volume instead?
year_data <- df %>%
  select(coral_number, year, length, width, height,
         dyn_death, size_t0) %>%
  rename(display_year   = year,
         died_this_year = dyn_death,
         volume_cm3     = size_t0)

expanded <- expanded %>%
  left_join(year_data, by = c("coral_number", "display_year"))

# Label each coral as recruited, surviving, or died to color in Tableau
expanded <- expanded %>%
  mutate(status = case_when(
    died_this_year == 1 ~ "Died this year",
    years_since == 0    ~ "Recruited",
    TRUE                ~ "Surviving"
  ))

# Export for tableau 
write_csv(expanded, "~/MEDS/capstone/eds-coral-figs-storage/corals_tableau_ready.csv")



