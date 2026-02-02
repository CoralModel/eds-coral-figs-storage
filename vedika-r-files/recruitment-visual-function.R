### The purpose of this script is to create a reproducible function for creating visualizations 
## What is needed: cleaned, standardized dataset

## thought process: incorporate total recruits in each transect per 5 m^2 (/5)
# - Exclude "P" transects 
# - Sum up total recruits BY TRANSECT, then divide by 5 m^2 to standardize 

###_______________ Step 2: Create the function for transects at LTER1 only 
transect_recruitment <- function(coral_data, site_name = NULL, habitat_name) {
  
  # Establish shared colors per taxa
  coral_colors <- c(
    Acr = "blue", 
    Poc = "red", 
    Por = "darkgreen", 
    Mil = "darkorange"
  )
  
  # Filter and summarize recruits
  plot_data <- coral_data %>%
    filter(site %in% site_name,
           habitat == habitat_name) %>%
    group_by(transect, taxa, year) %>%
    summarize(
      n_recruits = sum(dyn_recruitment, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      recruits_std = n_recruits / 5   # Standardize per 5 m^2
    )
  
  # Plot standardized recruitment per year
  plot_transect <- ggplot(plot_data, aes(x = factor(year), y = recruits_std, color = taxa, group = taxa)) +
    geom_line() +
    geom_point() +
    facet_grid(transect ~ taxa) + 
    scale_color_manual(values = coral_colors) + 
    labs(
      x = "Year",
      y = "Total Recruits per 5 m²",
      title = paste("Standardized Coral Recruitment at", site_name, "(", habitat_name, ")"),
      color = "Taxa"
    ) +
    theme_igray() + 
    scale_x_discrete(breaks = c(2014,2016, 2018, 2020, 2022, 2024)) + 
    theme(axis.text.x = element_text(angle = 45, vjust = .5))
  
  
  
  ## Save individual plots 
 # ggsave(plot_transect, file = "recruitment-figs/transect-recruitment/transect-recruitment.png")
  print(unique(plot_data$transect))
  plot_transect
}

########## recruitment vs year for sites (based on habitat)
### The purpose of this script is to create a reproducible function for creating visualizations 
## What is needed: cleaned, standardized dataset

## thought process: incorporate total recruits in each transect per 5 m^2 (/5)
# - Exclude "P" transects 
# - Sum up total recruits BY TRANSECT, then divide by 5 m^2 to standardize 

###_______________ Step 2: Create the function for transects at LTER1 only 
site_recruitment <- function(coral_data, habitat_name) {
  
  # Establish shared colors per taxa
  coral_colors <- c(
    Acr = "blue", 
    Poc = "red", 
    Por = "darkgreen", 
    Mil = "darkorange"
  )
  
  # 1. Filter and summarize recruits
  plot_data <- coral_data %>% 
    filter(habitat == habitat_name) %>%
    group_by(site, taxa, year) %>%
    summarize(
      n_recruits = sum(dyn_recruitment, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
     # habitat = factor(habitat, levels = c("BR" = "Backreef", "OR" = "Outer Reef")), 
      recruits_std = n_recruits / 5   # Standardize per 5 m^2
    )
  
  # 2. Plot the standardized recruitment per year
  plot_transect <- ggplot(plot_data, aes(x = factor(year), y = recruits_std, color = taxa, group = taxa)) +
    geom_line() +
    geom_point() +
    facet_grid(site ~ taxa) + 
    scale_color_manual(values = coral_colors) + 
    labs(
      x = "Year",
      y = "Total Recruits per 5 m²",
     title = paste("Standardized Coral Recruitment at Each Site in", habitat_name),
      color = "Taxa"
    )  +
  theme_igray() + 
  scale_x_discrete(breaks = c(2014,2016, 2018, 2020, 2022, 2024)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = .5))



## Save individual plots 
# ggsave(plot_transect, file = "recruitment-figs/transect-recruitment/transect-recruitment.png")
#print(unique(plot_data$site))
plot_transect
  
}

#########
habitat_recruitment <- function(coral_data) {
  
  # Establish shared colors per taxa
  coral_colors <- c(
    Acr = "blue", 
    Poc = "red", 
    Por = "darkgreen", 
    Mil = "darkorange"
  )
  
  # 1. Filter and summarize recruits
  plot_data <- coral_data %>% 
    group_by(habitat, taxa, year) %>%
    summarize(
      n_recruits = sum(dyn_recruitment, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # habitat = factor(habitat, levels = c("BR" = "Backreef", "OR" = "Outer Reef")), 
      recruits_std = n_recruits / 5   # Standardize per 5 m^2
    )
  
  # 2. Plot the standardized recruitment per year
  plot_transect <- ggplot(plot_data, aes(x = factor(year), y = recruits_std, color = taxa, group = taxa)) +
    geom_line() +
    geom_point() +
    facet_grid(habitat ~ taxa) + 
    scale_color_manual(values = coral_colors) + 
    labs(
      x = "Year",
      y = "Total Recruits per 5 m²",
      title = "Standardized Coral Recruitment at Each Habitat",
      color = "Taxa"
    )  +
    theme_igray() + 
    scale_x_discrete(breaks = c(2014,2016, 2018, 2020, 2022, 2024)) + 
    theme(axis.text.x = element_text(angle = 45, vjust = .5))
  
  
  
  ## Save individual plots 
  # ggsave(plot_transect, file = "recruitment-figs/transect-recruitment/transect-recruitment.png")
  #print(unique(plot_data$site))
  plot_transect
  
}








