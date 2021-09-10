library(tidyverse)

pr <- function(A, B, b, c){
  r_1 = A[1] - A[2]
  r_2 = B[1] - B[2]
  s_1 = (B[2]*r_1 + A[2])/(1-r_1*r_2)
  s_2 = (A[2]*r_2 + B[2])/(1-r_1*r_2)
  P_AB = b*s_2 - c*s_1
  P_BA = b*s_1 - c*s_2
  s_AA = (A[2]*r_1 + A[2])/(1-(r_1)^2)
  s_BB = (B[2]*r_2 + B[2])/(1-(r_2)^2)
  P_AA = s_AA*(b - c)
  P_BB = s_BB*(b - c)
  return(c(P_AA, P_AB, P_BA, P_BB))
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

reactive <- function(u, b, c, lambda, condition, reps){
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
        rand_angle = runif(1, 0, 2*pi)
        if (SR[1] + cos(rand_angle) <= 1 & SR[1] + cos(rand_angle) >= 0 &
            SR[2] + sin(rand_angle) <= 1 & SR[2] + sin(rand_angle) >= 0) {
          SM = c(SR[1] + cos(rand_angle), SR[2] + sin(rand_angle))
          i = i + 1
        }
        else {
          i = 0
        }
      }
    }
    if (condition(pr(SR, SM, b, c), lambda)){
      SR = SM
    }
    df = df %>% add_row(p = SR[1], q = SR[2])
  }
}

results = reactive(u = 0.02, b = 3, c = 1, lambda = 0.2, condition = condition_1, reps = 100000)
