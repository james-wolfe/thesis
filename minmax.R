# Finds the local minima and maxima of the datasets using Moore neighborhood.

# df is the dataframe with the frequency for each 2d or 4d subset.
# func is either min or max.

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
  return(vec)
}


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
  return(vec)
}
