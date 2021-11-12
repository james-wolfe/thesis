using Distributions
using DataFrames
using Random
using CSV
using LinearAlgebra
using Parquet

# This function is the condition a + Lb <= c + Ld. Returns true or false.
# If true, mutant is accepted.

function condition_1(
    vector::Vector{Float64},
    lambda::Float64)
  if vector[1] + lambda*vector[2] <= vector[3] + lambda*vector[4]
    return true
  else
    return false
end
end

# This function is the condition Sa + b <= c + Sd. Returns true or false.
# If true, mutant is accepted.

function condition_2(
    vector::Vector{Float64},
    sigma::Float64)
  if sigma*vector[1] + vector[2] <= vector[3] + sigma*vector[4]
    return true
  else
    return false
end
end

# Given reactive strategy A plays reactive strategy B in an infinitely repeated
# donation game with benefit b and cost c, pr returns the expected average
# payoff of A playing B.

function pr(
    A::Vector{Float64},
    B::Vector{Float64},
    b::Int64,
    c::Int64)
  r_1 = A[1] - A[2]
  r_2 = B[1] - B[2]
  s_1 = (B[2]*r_1 + A[2])/(1-r_1*r_2)
  s_2 = (A[2]*r_2 + B[2])/(1-r_1*r_2)
  P = b*s_2 - c*s_1
  return P
end

# Given memory-one strategy A plays strategy B in an infinitely repeated
# donation game with benefit b and cost c, pmo returns the expected average
# payoff of A playing B.

function pmo(;
    A::Vector{Float64},
    B::Vector{Float64},
    b::Float64,
    c::Float64)
  payoffs = [b-c, -c, b, 0.0]
  p1_matrix = [-1.0+A[1]*B[1] -1.0+A[1] -1.0+B[1] payoffs[1];
  A[2]*B[3] -1.0+A[2] B[3] payoffs[2];
  A[3]*B[2] A[3] -1.0+B[2] payoffs[3];
  A[4]*B[4] A[4] B[4] payoffs[4]]
  ones = [-1.0+A[1]*B[1] -1.0+A[1] -1.0+B[1] 1.0;
  A[2]*B[3] -1.0+A[2] B[3] 1.0;
  A[3]*B[2] A[3] -1.0+B[2] 1.0;
  A[4]*B[4] A[4] B[4] 1.0]
  det_p1 = det(p1_matrix)
  det_1 = det(ones)
  exp_payoff = det_p1/det_1
  return exp_payoff
end

# reactive runs stochastic evolutionary dynamics with the following
# parameters:
#
# u         - prob. mutant is taken from uniform dist of strategy space
# b         - benefit
# c         - cost
# measure   - value of either sigma or lambda, depending on condition
# condition - function that gives whether or not mutant is accepted
# payoff    - payoff function. in this case, pr
# reps      - how many times we find a mutant and have it play resident
# epsilon   - neighborhood around resident where mutant is selected from
#
# It returns a DataFrame documenting the resident strategy for each rep

function reactive(;
    u::Float64 = 0.5,
    b::Int64 = 3,
    c::Int64 = 1,
    measure::Float64,
    condition::Function,
    reps::Int64 = 10^5,
    epsilon::Float64 = 0.05)

  ep_samp = Uniform(-epsilon, epsilon)
  unif = Uniform(0,1)
  SR = [rand(unif),rand(unif)]
  df = DataFrame(p = SR[1], q = SR[2])
  SM = [0.0,0.0]
  for i in 2:reps
    x = rand(unif)
    if x < u
      SM = [rand(unif),rand(unif)]
    else
        x_ep = rand(ep_samp)
        y_ep = rand(ep_samp)
        if (0.0 < SR[1] + x_ep && SR[1] + x_ep < 1.0)
            SM[1] = SR[1] + x_ep
        else
            SM[1] = SR[1]
        end
        if (0.0 < SR[2] + y_ep && SR[2] + y_ep < 1.0)
            SM[2] = SR[2] + y_ep
        else
            SM[2] = SR[2]
        end
    end
    if (condition([pr(SR, SR, b, c),
                    pr(SR, SM, b, c),
                    pr(SM, SR, b, c),
                    pr(SM, SM, b, c)], measure) == true)
      SR = SM
    end
    push!(df, [SR[1], SR[2]])
end
  return df
end

# memory_one runs stochastic evolutionary dynamics with the following
# parameters:
#
# u         - prob. mutant is taken from uniform dist of strategy space
# b         - benefit
# c         - cost
# measure   - value of either sigma or lambda, depending on condition
# condition - function that gives whether or not mutant is accepted
# payoff    - payoff function. in this case, pmo
# reps      - how many times we find a mutant and have it play resident
# epsilon   - neighborhood around resident where mutant is selected from
#
# It returns a DataFrame documenting the resident strategy for each rep

function memory_one(;
    u::Float64 = 0.5,
    b::Float64 = 3.0,
    c::Float64 = 1.0,
    measure::Float64,
    condition::Function,
    payoff::Function,
    reps::Int64 = 10^8,
    epsilon::Float64 = 0.05)
    ep_samp = Uniform(-epsilon, epsilon)
    unif = Uniform(0,1)
    SR = [rand(unif), rand(unif), rand(unif), rand(unif)]
    df = DataFrame(p1 = SR[1], p2 = SR[2], p3 = SR[3], p4 = SR[4])
    SM = [0.0,0.0,0.0,0.0]
    for i in 2:reps
        x = rand(unif)
        if x < u
            SM = [rand(unif),rand(unif),rand(unif),rand(unif)]
        else
            p1_ep = rand(ep_samp)
            p2_ep = rand(ep_samp)
            p3_ep = rand(ep_samp)
            p4_ep = rand(ep_samp)
            if (0.0 < SR[1] + p1_ep && SR[1] + p1_ep < 1.0)
                SM[1] = SR[1] + p1_ep
            else
                SM[1] = SR[1]
            end
            if (0.0 < SR[2] + p2_ep && SR[2] + p2_ep < 1.0)
                SM[2] = SR[2] + p2_ep
            else
                SM[2] = SR[2]
            end
            if (0.0 < SR[3] + p3_ep && SR[3] + p3_ep < 1.0)
                SM[3] = SR[3] + p3_ep
            else
                SM[3] = SR[3]
            end
            if (0.0 < SR[4] + p4_ep && SR[4] + p4_ep < 1.0)
                SM[4] = SR[4] + p4_ep
            else
                SM[4] = SR[4]
            end
        end
        if (condition([payoff(A = SR, B = SR, b = b, c = c),
            payoff(A = SR, B = SM, b = b, c = c),
            payoff(A = SM, B = SR, b = b, c = c),
            payoff(A = SM, B = SM, b = b, c = c)], measure) == true)
            SR = SM
        end
        push!(df, [SR[1],SR[2],SR[3],SR[4]])
    end
    return df
end

# m1_summary is the same as memory_one, except instead of returning a DataFrame
# of values for the resident strategy, it returns the frequency of each combo
# of values for the four probability parameters.

# I could easily make the summary its own function, but felt I could save a
# bit of time.

function m1_summary(;
    u::Float64 = 0.5,
    b::Float64 = 3.0,
    c::Float64 = 1.0,
    measure::Float64,
    condition::Function,
    payoff::Function,
    reps::Int64 = 10^8,
    epsilon::Float64 = 0.05)
    ep_samp = Uniform(-epsilon, epsilon)
    unif = Uniform(0,1)
    SR = [rand(unif), rand(unif), rand(unif), rand(unif)]
    df = DataFrame(p1 = SR[1], p2 = SR[2], p3 = SR[3], p4 = SR[4])
    SM = [0.0,0.0,0.0,0.0]
    for i in 2:reps
        x = rand(unif)
        if x < u
            SM = [rand(unif),rand(unif),rand(unif),rand(unif)]
        else
            p1_ep = rand(ep_samp)
            p2_ep = rand(ep_samp)
            p3_ep = rand(ep_samp)
            p4_ep = rand(ep_samp)
            if (0.0 < SR[1] + p1_ep && SR[1] + p1_ep < 1.0)
                SM[1] = SR[1] + p1_ep
            else
                SM[1] = SR[1]
            end
            if (0.0 < SR[2] + p2_ep && SR[2] + p2_ep < 1.0)
                SM[2] = SR[2] + p2_ep
            else
                SM[2] = SR[2]
            end
            if (0.0 < SR[3] + p3_ep && SR[3] + p3_ep < 1.0)
                SM[3] = SR[3] + p3_ep
            else
                SM[3] = SR[3]
            end
            if (0.0 < SR[4] + p4_ep && SR[4] + p4_ep < 1.0)
                SM[4] = SR[4] + p4_ep
            else
                SM[4] = SR[4]
            end
        end
        if (condition([payoff(A = SR, B = SR, b = b, c = c),
            payoff(A = SR, B = SM, b = b, c = c),
            payoff(A = SM, B = SR, b = b, c = c),
            payoff(A = SM, B = SM, b = b, c = c)], measure) == true)
            SR = SM
        end
        push!(df, [floor(SR[1]*10)/10,floor(SR[2]*10)/10,floor(SR[3]*10)/10,floor(SR[4]*10)/10])
    end
    df = combine(groupby(df, [:p1, :p2, :p3, :p4]), nrow => :count)
    return df
end
