# Running the reactive plots

plot_reactive(reac_l0) 
plot_reactive(reac_l05)
plot_reactive(reac_l1)
plot_reactive(reac_s2)

# Running the memory-one plots

# Adding these rows because these subsets were not visited at all
# but I still want them showing up on the plot.

m1_s2 = m1_s2 %>%
  add_row(p1=0,p2=0.7,p3=0.9,p4=0,count=0) %>%
  add_row(p1=0,p2=0.8,p3=0.8,p4=0,count=0) %>%
  add_row(p1=0,p2=0.8,p3=0.9,p4=0,count=0) %>%
  add_row(p1=0.1,p2=0.9,p3=0.6,p4=0,count=0) %>%
  add_row(p1=0.1,p2=0.9,p3=0.7,p4=0,count=0) %>%
  add_row(p1=0.1,p2=0.9,p3=0.9,p4=0,count=0) %>%
  add_row(p1=0.1,p2=0.8,p3=0.8,p4=0,count = 0)

freq_m1(m1_l0) +
  labs(title = "lambda = 0")
freq_m1(m1_l05) +
  labs(title = "lambda = 0.5")
freq_m1(m1_l1) +
  labs(title = "lambda = 1")
freq_m1(m1_s2) +
  labs(title = "sigma = 2")
