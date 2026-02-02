### This is a rough draft of a function that would allow you to compile all sites, transects, and habitats together based on recruitment


############ aggregated function
recruitment_visual <- function(coral_data,
                               level = "transect",        # "transect", "site", or "habitat"
                               site_name = NULL,
                               habitat_name = NULL,
                               transects = NULL,
                               area_m2 = 5) {
  
  # 1. Filter data if site/habitat specified
  plot_data <- coral_data
  
  if (!is.null(site_name)) {
    plot_data <- plot_data %>% filter(site == site_name)
  }
  
  if (!is.null(habitat_name)) {
    plot_data <- plot_data %>% filter(habitat == habitat_name)
  }
  
  
  # 2. Group and summarize based on chosen level
  plot_data <- plot_data %>%
    { 
      if (level == "transect") {
        group_by(., transect, taxa, year)
      } else if (level == "site") {
        group_by(., site, taxa, year)
      } else if (level == "habitat") {
        group_by(., habitat, taxa, year)
      } else {
        stop("level must be one of 'transect', 'site', or 'habitat'")
      }
    } %>%
    summarize(
      n_recruits = sum(dyn_recruitment, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      recruits_std = n_recruits / area_m2
    )
  
  # 3. Determine facet and grouping for plotting
  facet_formula <- switch(level,
                          "transect" = transect ~ taxa,
                          "site"     = site ~ taxa,
                          "habitat"  = habitat ~ taxa)
  
  # 4. Plot
  p <- ggplot(plot_data, aes(x = factor(year), y = recruits_std, color = taxa, group = taxa)) +
    geom_line() +
    geom_point() +
    facet_grid(facet_formula) +
    labs(
      x = "Year",
      y = paste("Recruits per", area_m2, "m²"),
      title = paste("Standardized Coral Recruitment:", level),
      color = "Taxa"
    ) +
    theme_minimal()
  
  return(p)
}

recruitment_visual(clean_coral, level = "transect", site_name = "LTER1")

#per transect: transect_recruitment(clean_coral, level = "transect", site_name = "LTER1", transects = c("T01","T02"))
# aggregate per site: transect_recruitment(clean_coral, level = "site", site_name = "LTER1")
# aagregate across habitat: transect_recruitment(clean_coral, level = "habitat")