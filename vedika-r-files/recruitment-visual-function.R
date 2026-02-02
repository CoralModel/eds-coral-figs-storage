### The purpose of this script is to create a reproducible function for creating visualizations 
## What is needed: cleaned, standardized dataset

## thought process: incorporate total recruits in each transect per 5 m^2 (/5)
# - Exclude "P" transects 
# - Sum up total recruits BY TRANSECT, then divide by 5 m^2 to standardize 



# the function will take:


##### STEP 1: Load in cleaned dataset and filter 
# Read in data and converting missing data to NA values

###_______________ Step 2: Create the function for transects at LTER1 only 
transect_recruitment <- function(coral_data, site_name, habitat_name = "BR") {
  
  # Establish shared colors per taxa
  coral_colors <- c(
    Acr = "blue", 
    Poc = "red", 
    Por = "darkgreen", 
    Mil = "darkorange"
  )
  
  # 1. Filter and summarize recruits
  plot_data <- coral_data %>%
    filter(site == site_name,
           habitat == habitat_name) %>%
    group_by(transect, taxa, year) %>%
    summarize(
      n_recruits = sum(dyn_recruitment, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      recruits_std = n_recruits / 5   # Standardize per 5 m^2
    )
  
  # 2. Plot the standardized recruitment per year
  ggplot(plot_data, aes(x = factor(year), y = recruits_std, color = taxa, group = taxa)) +
    geom_line() +
    geom_point() +
    facet_grid(transect ~ taxa) + 
    scale_color_manual(values = coral_colors) + 
    labs(
      x = "Year",
      y = "Total Recruits per 5 m²",
      title = paste("Standardized Coral Recruitment at", site_name),
      color = "Taxa"
    ) +
    theme_minimal()
}

transect_recruitment(coral_data = clean_coral, site_name = "LTER1")






