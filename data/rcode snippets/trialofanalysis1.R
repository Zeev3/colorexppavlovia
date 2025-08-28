library(ggplot2)
library(grid)

output_dir <- "/Users/zeevbenamos/Documents/GitHub/RttM_project/amits_code/img"


# Function to generate and save gradient scales
generate_gradient_scale <- function(color_start, color_end, name) {
  shades <- colorRampPalette(c(color_start, color_end))(50)
  
  # Create data for the gradient scale
  df <- data.frame(x = 1:50, y = 1, color = shades)
  
  # Plot and save the full gradient scale
  p <- ggplot(df, aes(x = x, y = y, fill = x)) +
    geom_raster() +  # Ensures smooth gradient without gaps
    scale_fill_gradientn(colors = shades) +
    theme_void() +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, paste0(name, "_scale.jpg")), 
         plot = p, width = 10, height = 2, dpi = 300, device = "jpeg")
  
  # Save each individual shade as a separate image
  for (i in 1:50) {
    shade_color <- shades[i]
    
    single_plot <- ggplot(data.frame(x = 1, y = 1), aes(x = x, y = y, fill = factor(1))) +
      geom_tile() +
      scale_fill_manual(values = shade_color) +
      theme_void() +
      theme(legend.position = "none")
    
    ggsave(file.path(output_dir, paste0(name, i, ".jpg")), 
           plot = single_plot, width = 141 / 72, height = 181 / 72, dpi = 72, device = "jpeg")
  }
}

# Generate gradient images
generate_gradient_scale("pink", "red", "red")
generate_gradient_scale("lightblue", "blue", "blue")

