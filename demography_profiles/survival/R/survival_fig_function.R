survival_plot <- function(data, site = NULL, habitat = NULL){
  
  if(is.null(site)){
    habitat_survival <- data %>%
      ggplot(aes(volume, survival, color = taxa)) +
      geom_point(alpha = 0.4) +
      scale_color_manual(values = coral_colors) +
      facet_grid(habitat~taxa, scales = "free") +
      labs(title = paste("Coral survival across habitats"),
           y = "",
           x = "Initial volume (cm^3)") +
      theme_light() +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 6, angle = 45, hjust = 1))
    print(habitat_survival)
  }
else if(is_null(habitat)){
  data %>% 
    dplyr::filter(!!enquo(site) == site) %>%
    ggplot(aes(volume, survival, color = taxa)) +
    geom_point(alpha = 0.4) +
    scale_color_manual(values = coral_colors) +
    facet_grid(habitat~taxa, scales = "free") +
    labs(title = paste("Coral survival at", site),
         y = "",
         x = "Initial volume (cm^3)") +
    theme_light() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 6))
  
}else{
  data %>% 
    dplyr::filter(!!enquo(site) == site,
                  !!enquo(habitat) == habitat) %>%
    ggplot(aes(volume, survival, color = taxa)) +
    geom_point(size = 1, alpha = 0.4) +
    scale_color_manual(values = coral_colors) +
    facet_grid(transect~taxa, scales = "free") +
    labs(title = paste(data$taxa, "survival on", habitat, "at", site),
         y = "",
         x = "Initial volume (cm^3)") +
    theme_classic() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 6))
}

  
}
