plot_recruitment <- function(coral_data) {
  
  coral_colors <- c(
    Acr = "blue", 
    Poc = "red", 
    Por = "darkgreen", 
    Mil = "darkorange"
  )
  
  plot_data <- coral_data %>%
    dplyr::group_by(transect, year, taxa, habitat) %>%
    dplyr::summarize(
      n_recruits = sum(dyn_recruitment, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      recruits_std = n_recruits / 5,
      year = factor(year, levels = 2013:2024),
      habitat = factor(habitat, levels = c("BR", "OR"))
    )
  
  taxa_labels <- c(
    Acr = "italic('Acropora')",
    Mil = "italic('Millepora')",
    Poc = "italic('Pocillopora')",
    Por = "italic('Porites')"
  )
  
  habitat_labels <- c(
    BR = "Back Reef",
    OR = "Fore Reef"
  )
  
  ggplot(plot_data, aes(x = year, y = n_recruits, fill = taxa)) +
    geom_col(width = 0.9) +
    facet_grid(
      habitat ~ taxa,
      labeller = labeller(
        taxa = as_labeller(taxa_labels, label_parsed),
        habitat = habitat_labels
      )
    ) +
    scale_fill_manual(values = coral_colors) +
    theme_minimal(base_size = 12) +  # keeps grid lines
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(color = "grey80"),  # major grid lines
      panel.grid.minor = element_line(color = "grey90"),  # minor grid lines
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background  = element_rect(fill = "transparent", color = NA),
      strip.background = element_rect(fill = "black"),
      strip.text = element_text(color = "white")
    ) +
    scale_x_discrete(breaks = seq(from = 2014, to = 2024, by = 2)) +
    labs(
      title = "Standardized Recruitment Through Time (2013-2024)",
      x = "Year",
      y = "Recruits per 5 m²"
    )
}











# library(dplyr)
# library(ggplot2)
# library(ggthemes)
# 
# # Unified recruitment plotting function
# plot_recruitment <- function(coral_data,
#                              site_name = NULL,
#                              habitat_name = NULL,
#                              facet_level = c("transect", "site", "habitat")) {
#   
#   # Match facet level argument
#   facet_level <- match.arg(facet_level)
#   
#   # Define colors per taxa
#   coral_colors <- c(
#     Acr = "blue",
#     Poc = "red",
#     Por = "darkgreen",
#     Mil = "darkorange"
#   )
#   habitat_labels <- c("BR" = "Back Reef", "OR" = "Fore Reef")
#   taxa_labels <- c("Acr" = "*Acropora*", "Mil" = "*Millepora*",
#                    "Poc" = "*Pocillopora*", "Por" = "*Porites*")
#   transect_labels <- c("T01" = "T1", "T02" = "T2", "T03" = "T3", "T04" = "T4")
#   
#   
#   # Filter data based on site and habitat
#   plot_data <- coral_data %>%
#     {if (!is.null(site_name)) filter(., site %in% site_name) else .} %>%
#     {if (!is.null(habitat_name)) filter(., habitat == habitat_name) else .} %>%
#     # Optional: exclude "P" transects if needed
#     filter(!grepl("^P", transect)) %>%
#     group_by(transect, site, habitat, taxa, year) %>%
#     summarize(n_recruits = sum(dyn_recruitment, na.rm = TRUE),
#               .groups = "drop") %>%
#     mutate(recruits_std = n_recruits / 5) # Standardize per 5 m^2
#   
#   # Determine faceting
#   facet_formula <- switch(facet_level,
#                           "transect" = transect ~ taxa,
#                           "site" = site ~ taxa,
#                           "habitat" = habitat ~ taxa)
#   
#   # Choose theme (example)
#   theme_fn <- if (facet_level == "site") theme_igray() else theme_light()
#   
#   # Create plot
#   p <- ggplot(plot_data, aes(x = factor(year), y = recruits_std, fill = taxa, group = taxa)) +
#     geom_col() +
#     facet_grid(facet_formula, scales = "free") +
#     scale_fill_manual(values = coral_colors) +
#     labs(
#       x = "Year",
#       y = "Recruits per 5 m²",
#       title = paste("Standardized Coral Recruitment",
#                     if(!is.null(site_name)) paste("at", site_name) else "",
#                     if(!is.null(habitat_name)) paste("(", habitat_name, ")") else ""),
#       fill = "Taxa"
#     ) +
#     scale_x_discrete(breaks = c(2014,2016,2018,2020,2022,2024)) +
#     theme_fn +
#     theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
#           legend.position = "none", 
#           panel.background = element_rect(fill = "transparent", color = NA), # Transparent panel background
#           plot.background = element_rect(fill = "transparent", color = NA),  # Transparent plot background
#     )
#   
#   print(unique(plot_data$transect)) # Optional: see which transects were included
#   return(p)
# }
