gif_maker <- function(data, lter_site, habitat_code, transect_number){
  plot <- data %>% 
    filter(site == lter_site &
             habitat == habitat_code & 
             transect == transect_number &
             !is.na(current_volume)
    ) %>% 
    mutate(y_m = y / 100) %>% 
    ggplot(aes(x, y_m)) +
    #geom_point(aes(color = taxa_class, size = current_volume * 2, group = coral_id)) + # Add rings for recruitment and death
    geom_point(shape = 21, aes(color = taxa_class, fill = taxa, size = current_volume, group = coral_id)) +
    scale_size(range = c(0.01, 5), 
               name = "Volume (cm³)") +
    scale_color_manual(values = coral_colors) +
    scale_fill_manual(values = coral_colors) +
    theme_light() +
    coord_fixed(ratio = 1) +
    labs(title = paste0(lter_site, " ", habitat_code, " ", transect_number, " ", 'population year: {closest_state}'),
         x = "Transect length (m)",
         y = "Transect width (m)") +
    transition_states(year,
                      transition_length = 4,
                      state_length = 2) +
    theme_bw() +
    theme(legend.position = "none",
          title = element_text(size = 8),
          axis.title = element_text(size = 6))
  
  anim <- animate(plot,
                  duration = 30,
                  fps = 25, height = 500, width = 2500, res = 300,
                  renderer = gifski_renderer())
  
  magick::image_read(anim) %>%
    magick::image_trim() %>%          # auto-trims white borders
    magick::image_write_gif(paste0(lter_site, habitat_code, transect_number, ".gif"))
}
