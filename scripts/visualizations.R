###########################
#
#
#
#
#
###############################

library(tidyverse)
library(ggplot2)

data = read.csv("data/clean/transformed_cleaned_data.csv")

#########################################
# Scatter plot showing structural breaks
# when in a recession
# U3 vs FFR
#########################################
scatter_levels = ggplot(data %>% drop_na(u3_unemployment, fedfunds, recession), 
  # Notice we swapped 'color' for 'fill' here because of the new point shape
  aes(y = fedfunds, x = u3_unemployment, fill = factor(recession))) +
  
  # Shape 21 allows for filled dots with a white outline (stroke). 
  geom_point(shape = 21, color = "white", size = 3, stroke = 0.4, alpha = 0.5) +
  
  # Updated to scale_fill_manual to match the shape 21 geometry
  scale_fill_manual(values = c("0" = "#7f8c8d", "1" = "#e74c3c"), 
    labels = c("Expansion", "Recession")) +
  
  theme_minimal(base_size = 12) +
  
  labs(title = "Federal Funds vs. U3 Unemployment",
    subtitle = "Assessing geometric relationship across regimes",
    y = "Federal Funds Rate (%)",
    x = "U3 Unemployment Rate (%)",
    fill = "Economic State:") +
  
  # Stupid theme stuff
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 12, color = "#555555", margin = margin(b = 15)),
    axis.title = element_text(face = "bold", color = "#333333"),
    axis.text = element_text(color = "#666666"),
    panel.grid.major = element_line(color = "#eaeaea", linewidth = 0.5),
    panel.grid.minor = element_blank(), # Drops the cluttering minor lines
    legend.position = "top",            # Moves legend to the top for a wider plot
    legend.title = element_text(face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
  )

print(scatter_levels)

