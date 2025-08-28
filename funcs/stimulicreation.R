library(ggplot2)
library(grid)

output_dir <- "/Users/zeevbenamos/Documents/GitHub/RttM_project/amits_code/img"

# Create a data frame to store filenames and hex codes
color_data <- data.frame(filename = character(), hex_code = character(), stringsAsFactors = FALSE)

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
  
  scale_filename <- paste0(name, "_scale.jpg")
  ggsave(file.path(output_dir, scale_filename), 
         plot = p, width = 10, height = 2, dpi = 300, device = "jpeg")
  
  # Add scale image to the color data
  color_data <<- rbind(color_data, data.frame(
    filename = scale_filename,
    hex_code = paste(shades, collapse = ";"),  # Store all gradient colors
    stringsAsFactors = FALSE
  ))
  
  # Save each individual shade as a separate image
  for (i in 1:50) {
    shade_color <- shades[i]
    
    single_plot <- ggplot(data.frame(x = 1, y = 1), aes(x = x, y = y, fill = factor(1))) +
      geom_tile() +
      scale_fill_manual(values = shade_color) +
      theme_void() +
      theme(legend.position = "none")
    
    individual_filename <- paste0(name, i, ".jpg")
    ggsave(file.path(output_dir, individual_filename), 
           plot = single_plot, width = 141 / 72, height = 181 / 72, dpi = 72, device = "jpeg")
    
    # Add individual image to the color data
    color_data <<- rbind(color_data, data.frame(
      filename = individual_filename,
      hex_code = shade_color,
      stringsAsFactors = FALSE
    ))
  }
  
  return(shades)
}

# Generate gradient images
red_shades <- generate_gradient_scale("pink", "red", "red")
blue_shades <- generate_gradient_scale("lightblue", "blue", "blue")

# Export the color data to a CSV file
csv_path <- file.path(output_dir, "color_data.csv")
write.csv(color_data, file = csv_path, row.names = FALSE, quote = TRUE)

# Print confirmation message
cat("CSV file created at:", csv_path, "\n")
cat("Total images created:", nrow(color_data), "\n")