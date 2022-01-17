library(tidyverse)

# Finds the local minima and maxima of the datasets using Moore neighborhood.

# df is the dataframe with the frequency for each 2d or 4d subset.
# func is either min or max.

# output will be the rows that are minima or maxima. 

# note that this does NOT find the subsets with the lowest or highest frequency,
# but this can easily be achieved with the following code with df as input:

# lowest freq:

df %>%
  arrange(count)

# highest freq:

df %>%
  arrange(desc(count))

# if looking at mem-one data, the input dataframe should have 5 columns, one 
# for each parameter and one with the frequency the strategy appeared in that
# subset, in the order (p1,p2,p3,p4,count)

# if looking at reactive data, the input dataframe should have 3 columns, in the
# order (p1,q,count)

find_minmax_m1 <- function(df, func){
  vec = c()
  for (i in 1:nrow(df)){
    p_1 = df[i,1]
    p_2 = df[i,2]
    p_3 = df[i,3]
    p_4 = df[i,4]
    
    df2 = df %>% 
      filter(p1 %in% c(p_1-0.1, p_1, p_1+0.1),
             p2 %in% c(p_2-0.1, p_2, p_2+0.1),
             p3 %in% c(p_3-0.1, p_3, p_3+0.1),
             p4 %in% c(p_4-0.1, p_4, p_4+0.1))
    
    if (df[i,5] == func(df2$count)){
      vec = c(vec, i)
    }
  }
  return(df[vec,])
}

# example:

m1_s2_max = find_minmax_m1(m1_s2, max)



find_minmax_r <- function(df, func){
  vec = c()
  for (i in 1:nrow(df)){
    p_1 = df[i,1]
    p_2 = df[i,2]
    
    df2 = df %>% 
      filter(p %in% c(p_1-0.1, p_1, p_1+0.1),
             q %in% c(p_2-0.1, p_2, p_2+0.1))
    
    if (df[i,3] == func(df2$count)){
      vec = c(vec, i)
    }
  }
  return(df[vec,])
}

# ex:

r_s2_max = find_minmax_r(reac_s2, max)

