### The purpose of this script is to create a reproducible function for creating visualizations 
## What is needed: cleaned, standardized dataset

## thought process: incorporate total recruits in each transect per 5 m^2 (/5)
# - Exclude "P" transects 
# - Sum up total recruits BY TRANSECT, then divide by 5 m^2 to standardize 



# the function will take:


##### STEP 1: Load in cleaned dataset and filter 
# Read in data and converting missing data to NA values
library(tidyverse)
clean_coral <- read_csv(here::here("vedika-r-files", "data", "coral_tidy_2024_with_dynamics.csv"), na = c("UK", "MK", "?", "MD", "US", "NS", "MissingObs", "Na", "NA"))

# Coral color way - consistent across all visualizations

coral_colors <- c(
  Acr = "blue", 
  Poc = "red", 
  Por = "darkgreen", 
  Mil = "darkorange"
)

clean_coral <- clean_coral |> 
  # Removing plots from backreef surveys 
  filter(str_detect(transect, "T")) |> 
  # Change volume variables to numeric
  mutate(
    height = as.numeric(height),
    width  = as.numeric(width),
    length = as.numeric(length),
    # Create volume (cm^2), z (m), and y (m) columns  
    y_m = y / 100, 
    z_m = z /100, 
    # Acropora were input as "A" in 2024, converting to "Acr" 
    taxa = case_when(taxa == "A" ~ "Acr", 
                     TRUE ~ taxa)) 

clean_coral_lter1 <- clean_coral %>% filter(site == "LTER1", habitat == "BR") 

unique(clean_coral_lter1$habitat)

###_______________ Step 2: Create the function for transects at LTER1 only 
transect_recruitment <- function(coral_data, habitat = "BR")






