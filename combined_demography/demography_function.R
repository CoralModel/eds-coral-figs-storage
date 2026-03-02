# Function to create coral demography profile plots
# Args:
#   data: cleaned dataframe
#   profile_filter: "growth" or "survival"
#   site_filter: LTER site to filter (e.g. "LTER1", "LTER2")
#   habitat_filter: Optional habitat to filter ("OR" or "BR"), NULL shows all
#   by_transect: If TRUE, facets by both taxa and transect; if FALSE, facets by taxa only
library(ggtext)

demography_plot <- function(data, profile_filter, site_filter, habitat_filter = NULL, by_transect = FALSE) {
  coral_colors <- c(
    Acr = "blue", 
    Poc = "red", 
    Por = "darkgreen", 
    Mil = "darkorange"
  )
  # Labels
  habitat_labels <- c("BR" = "Back Reef", "OR" = "Fore Reef")
  taxa_labels <- c("Acr" = "*Acropora*", "Mil" = "*Millepora*",
                   "Poc" = "*Pocillopora*", "Por" = "*Porites*")
  transect_labels <- c("T01" = "T1", "T02" = "T2", "T03" = "T3", "T04" = "T4")
  
  # Filter data by site
  data_clean <- data |> filter(site == site_filter)
  
  # Add habitat filter if specified
  if (!is.null(habitat_filter)) {
    data_clean <- data_clean |> filter(habitat == habitat_filter)
  }
  
  # GROWTH PLOT
  if (profile_filter == "growth") {
    
    p <- data_clean |>
      ggplot(aes(x = size_t0, y = size_t1, color = taxa)) +
      geom_point(alpha = 0.5) +
      scale_color_manual(values = coral_colors) +
      scale_x_log10(breaks = 10^(-1:5), labels = scales::label_log()) + 
      scale_y_log10(breaks = 10^(-1:5), labels = scales::label_log()) +
      coord_equal() +
      theme_classic() +
      theme(legend.position = "none",
            strip.background = element_rect(fill = "gray40"),
            strip.text.x = element_markdown(color = "white"),
            strip.text.y = element_markdown(color = "white"),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent"))
    
    # Apply faceting
    if (by_transect) {
      p <- p + facet_grid(taxa ~ transect,
                          labeller = labeller(transect = transect_labels, taxa = taxa_labels),
                          drop = FALSE)
    } else {
      p <- p + facet_grid(~ taxa,
                          labeller = labeller(taxa = taxa_labels),
                          drop = FALSE)
    }
    
    # Add labels
    p + labs(
      x = "Initial coral volume (cm³)",
      y = "Final coral volume (cm³)",
      title = paste(site_filter, habitat_filter)
    )
  }
  
  # SURVIVAL PLOT
  else if (profile_filter == "survival") {
    
    coral_death <- data_clean %>% 
      group_by(coral_number) %>%
      arrange(year) %>% 
      mutate(survival = case_when(
        dyn_death == 1 ~ 0,
        is.na(length) ~ NA,
        TRUE ~ 1
      ), volume = case_when(
        is.na(size_t0) ~ size_t_minus1,
        TRUE~ size_t0
      )
      ) %>% 
      ungroup()
    
    p <- coral_death |>
      ggplot(aes(x = volume, y = survival, color = taxa)) +
      geom_point(alpha = 0.5) +
      scale_color_manual(values = coral_colors) +
      scale_x_log10(breaks = 10^(-1:5), labels = scales::label_log()) +
      theme_classic() +
      theme(legend.position = "none",
            strip.background = element_rect(fill = "gray40"),
            strip.text.x = element_markdown(color = "white"),
            strip.text.y = element_markdown(color = "white"),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent"))
    
    # Apply faceting
    if (by_transect) {
      p <- p + facet_grid(taxa ~ transect,
                          labeller = labeller(transect = transect_labels, taxa = taxa_labels),
                          drop = FALSE)
    } else {
      p <- p + facet_grid(~ taxa,
                          labeller = labeller(taxa = taxa_labels),
                          drop = FALSE)
    }
    
    # Add labels
    p + labs(
      x = "Initial coral volume (cm³)",
      y = "Survival probability",
      title = paste(site_filter, habitat_filter)
    )
  }

  ## RECRUITMENT PLOT
  else if (profile_filter == "recruitment") {
    
    plot_data <- data_clean %>%
      # filter(!grepl("^P", transect)) %>%
      group_by(transect, taxa, year) %>%
      summarize(n_recruits = sum(dyn_recruitment, na.rm = TRUE), .groups = "drop") %>%
      mutate(recruits_std = n_recruits / 5)
    
    p <- ggplot(plot_data, aes(x = factor(year), y = recruits_std, fill = taxa)) +
      geom_col() +
      scale_fill_manual(values = coral_colors) +
      theme_classic() + scale_x_discrete(breaks = seq(2014, 2024, by = 2)) + 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            legend.position = "none", 
                  strip.background = element_rect(fill = "gray40"),
                  strip.text.x = element_markdown(color = "white"),
                  strip.text.y = element_markdown(color = "white"),
                  panel.background = element_rect(fill = "transparent"),
                  plot.background = element_rect(fill = "transparent")) 
    
    if (by_transect) {
      p <- p + facet_grid(taxa ~ transect,
                          labeller = labeller(transect = transect_labels, taxa = taxa_labels),
                          drop = F)
    } else {
      p <- p + facet_grid(~ taxa,
                          labeller = labeller(taxa = taxa_labels),
                          drop = F)
    }
    
    return(
      p + labs(
        x = "Year",
        y = "Recruits per 5 m²",
        title = paste(site_filter, habitat_filter)
      )
    )
  }
  

  else {
    stop("profile_filter must be 'growth', 'survival', or 'recruitment'.")
  }
}
