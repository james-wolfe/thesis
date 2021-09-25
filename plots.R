library(tidyverse)
library(viridis)
library(ggpubr)
library(gridExtra)
library(grid)

plot_reactive <- function(x){
  y = x %>%
    ggplot(aes(x = p, y = q)) + 
    geom_bin2d(aes(fill = after_stat(count/sum(count))), 
               binwidth = 0.05) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    scale_fill_viridis(name = "Frequency") +
    theme_minimal()
  return(y)
}

reactive_path <- function(x, slice_min = 1, slice_max = 100000){
  y = x %>%
    mutate(row = row_number()) %>%
    slice(slice_min:slice_max) %>%
    ggplot(aes(x = p, y = q)) + 
    geom_path(aes(color = row)) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    scale_color_viridis(name = "Time", trans = "log", breaks = c(1, 20, 400, 8000)) +
    theme_minimal()
  return(y)
}

# creates 100 frequency plots, to basically create 4d graph in 2d

for (i in 1:10){
  name = paste("plot_", i, sep = "")
  for (j in 10:-1:1){
    new_name = paste(name, j, sep = "_")
    assign(new_name, mem1_s2 %>% 
             filter(i/10 - 0.1 < p1 & p1 <= i/10) %>% 
             filter(j/10 - 0.1 < p3 & p3 <= j/10) %>%
             ggplot(aes(x = p2, y = p4)) +
             geom_bin2d(aes(fill = stat(count/sum(count))), binwidth = 0.05) +
             coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
             scale_fill_viridis(limits=c(0,1), name = "Frequency") +
             theme_minimal() +
             theme(axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   legend.position="none"))
  }
}
