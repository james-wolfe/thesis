library(tidyverse)
library(viridis)
library(ggpubr)
library(gridExtra)
library(grid)

# Given a tibble x with the resident strategy's values for each rep, plot_reactive
# plots the frequency of the resident strategy on the reactive strategy space.

# bwidth determines how big to make the 2d bins.

plot_reactive <- function(x, bwidth = 0.05){
  y = x %>%
    ggplot(aes(x = p, y = q)) + 
    geom_bin2d(aes(fill = after_stat(count/sum(count))), 
               binwidth = bwidth) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    scale_fill_viridis(name = "Frequency",trans = "log") +
    theme_minimal()
  return(y)
}

# reactive_path plots the dynamics of the resident strategy given a tibble x,
# which has the value of the resident strategy at each rep. 

# it gives a line path on the unit square.

reactive_path <- function(x, slice_min = 1, slice_max = 100000){
  y = x %>%
    mutate(row = row_number()) %>%
    slice(slice_min:slice_max) %>%
    ggplot(aes(x = p, y = q)) + 
    geom_path(aes(color = row)) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    scale_color_viridis(name = "Time") +
    theme_minimal()
  return(y)
}

# Takes a tibble x which has the count (number of visits) for each memory-one subset.
# Plots the count on a 10 x 10 grid of plots, each of which is a 10 x 10 surface plot,
# with fill representing frequency of visits.

freq_m1 <- function(x){
  y = x %>%
    mutate(p2 = p2 + 0.05, p3 = p3 + 0.05)
  
  y$p4 <- factor(y$p4, levels = c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0))
  
  y %>%
    ggplot(aes(x = p2, y = p3)) + 
    geom_raster(aes(fill = count), interpolate = FALSE) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    facet_grid(cols = vars(p1), rows = vars(p4)) +
    scale_fill_viridis(name = "Count", trans="pseudo_log",
                       breaks = c(10^3,10^5,10^7)) +
    theme_minimal() +                               
    theme(strip.text.x = element_blank(),
          strip.text.y = element_blank(),
          panel.spacing.x=unit(0.15, "lines"),
          panel.spacing.y=unit(0.15, "lines"))
}
