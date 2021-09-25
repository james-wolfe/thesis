library(tidyverse)
library(viridis)
library(ggpubr)

pr <- function(A, B, b, c){
  r_1 = A[1] - A[2]
  r_2 = B[1] - B[2]
  s_1 = (B[2]*r_1 + A[2])/(1-r_1*r_2)
  s_2 = (A[2]*r_2 + B[2])/(1-r_1*r_2)
  P = b*s_2 - c*s_1
  return(P)
}

pmo <- function(A, B, b, c){
  payoffs = c(b-c, -c, b, 0)
  p1_matrix = matrix(c(-1 + A[1]*B[1], -1 + A[1], -1 + B[1], payoffs[1],
                       A[2]*B[3], -1 + A[2], B[3], payoffs[2],
                       A[3]*B[2], A[3], -1 + B[2], payoffs[3],
                       A[4]*B[4], A[4], B[4], payoffs[4]),
                     byrow = TRUE, nrow = 4)
  ones_mx = matrix(c(-1 + A[1]*B[1], -1 + A[1], -1 + B[1], 1,
                     A[2]*B[3], -1 + A[2], B[3], 1,
                     A[3]*B[2], A[3], -1 + B[2], 1,
                     A[4]*B[4], A[4], B[4], 1),
                   byrow = TRUE, nrow = 4)
  exp_payoff = det(p1_matrix)/det(ones_mx)
  return(exp_payoff)
}
  
condition_1 <- function(vector, lambda){
  if (vector[1] + lambda*vector[2] <= vector[3] + lambda*vector[4]){
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

condition_2 <- function(vector, sigma){
  if (sigma*vector[1] + vector[2] <= vector[3] + sigma*vector[4]){
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

reactive <- function(u = 0.5, b = 3, c = 1, measure, condition, reps = 10^5, 
                     epsilon = 0.05){
  SR = c(runif(1), runif(1))
  df = tibble(p = SR[1], q = SR[2])
  for (i in 1:reps) {
    x = runif(1)
    if (x < u) {
      SM = c(runif(1), runif(1))
    }
    else {
      i = 0
      while (i < 1) {
        x_ep = runif(1, -epsilon, epsilon)
        y_ep = runif(1, -epsilon, epsilon)
        if (SR[1] + x_ep < 1 & SR[1] + x_ep > 0 &
            SR[2] + y_ep < 1 & SR[2] + y_ep > 0) {
        SM = c(SR[1] + x_ep, SR[2] + y_ep)
        i = 1
        }
        else {
          i = 0
        }
      }
    }
    if (condition(c(pr(SR, SR, b, c),
                    pr(SR, SM, b, c),
                    pr(SM, SR, b, c),
                    pr(SM, SM, b, c)), measure)){
      SR = SM
    }
    df = df %>% add_row(p = SR[1], q = SR[2])
  }
  return(df)
}

memory_one <- function(u = 0.5, b = 3, c = 1, measure, condition, reps = 10^5, 
                       epsilon = 0.05){
  SR = c(runif(1), runif(1), runif(1), runif(1))
  df = tibble(p1 = SR[1], p2 = SR[2], p3 = SR[3], p4 = SR[4])
  for (i in 1:reps) {
    x = runif(1)
    if (x < u) {
      SM = c(runif(1), runif(1), runif(1), runif(1))
    }
    else {
      i = 0
      while (i < 1) {
        for (i in 1:4){
          name = paste("ep_", i, sep = "")
          assign(name, runif(1, -epsilon, epsilon))
        }
        if (SR[1] + ep_1 < 1 & SR[1] + ep_1 > 0 &
            SR[2] + ep_2 < 1 & SR[2] + ep_2 > 0 &
            SR[3] + ep_3 < 1 & SR[3] + ep_3 > 0 &
            SR[4] + ep_4 < 1 & SR[4] + ep_4 > 0) {
          SM = c(SR[1] + ep_1, SR[2] + ep_2, SR[3] + ep_3, SR[4] + ep_4)
          i = 1
        }
        else {
          i = 0
        }
      }
    }
    if (condition(vector = c(pmo(A = SR, B = SR, b, c),
                             pmo(A = SR, B = SM, b, c),
                             pmo(A = SM, B = SR, b, c),
                             pmo(A = SM, B = SM, b, c)), 
                  measure) == TRUE){
      SR = SM
    }
    df = df %>% add_row(p1 = SR[1], p2 = SR[2], p3 = SR[3], p4 = SR[4])
  }
  return(df)
}



counting <- function(u = 0.5, b = 3, c = 1, measure, condition, reps = 10^5, 
                       epsilon = 0.05){
  SR = c(runif(1), runif(1), runif(1))
  df = tibble(p1 = SR[1], p2 = SR[2], p3 = SR[3])
  for (i in 1:reps) {
    x = runif(1)
    if (x < u) {
      SM = c(runif(1), runif(1), runif(1))
    }
    else {
      i = 0
      while (i < 1) {
        for (i in 1:3){
          name = paste("ep_", i, sep = "")
          assign(name, runif(1, -epsilon, epsilon))
        }
        if (SR[1] + ep_1 < 1 & SR[1] + ep_1 > 0 &
            SR[2] + ep_2 < 1 & SR[2] + ep_2 > 0 &
            SR[3] + ep_3 < 1 & SR[3] + ep_3 > 0) {
          SM = c(SR[1] + ep_1, SR[2] + ep_2, SR[3] + ep_3)
          i = 1
        }
        else {
          i = 0
        }
      }
    }
    if (condition(vector = c(pmo(A = c(SR[1], SR[2], SR[2], SR[3]), 
                                 B = c(SR[1], SR[2], SR[2], SR[3]), b, c),
                             pmo(A = c(SR[1], SR[2], SR[2], SR[3]), 
                                 B = c(SM[1], SM[2], SM[2], SM[3]), b, c),
                             pmo(A = c(SM[1], SM[2], SM[2], SM[3]), 
                                 B = c(SR[1], SR[2], SR[2], SR[3]), b, c),
                             pmo(A = c(SM[1], SM[2], SM[2], SM[3]), 
                                 B = c(SM[1], SM[2], SM[2], SM[3]), b, c)), 
                  measure) == TRUE){
      SR = SM
    }
    df = df %>% add_row(p1 = SR[1], p2 = SR[2], p3 = SR[3])
  }
  return(df)
}

repeat_sim = function(func, sets = 10, u = 0.5, b = 3, c = 1, measure, condition, 
                      reps = 5000, epsilon = 0.025){
  df <- func(u, b, c, measure, condition, reps, epsilon)
  for (i in 1:sets){
    df_new = func(u, b, c, measure, condition, reps, epsilon)
    df = rbind(df, df_new)
  }
  return(df)
}
