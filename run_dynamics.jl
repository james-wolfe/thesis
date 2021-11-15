using Random

reac_l0 = reactive(measure = 0.0,
                    condition = condition_1,
                    reps = 10^8,
                    epsilon = .05)
reac_l05 = reactive(measure = 0.5,
                    condition = condition_1,
                    reps = 10^8,
                    epsilon = .05)
reac_l1 = reactive(measure = 1.0,
                    condition = condition_1,
                    reps = 10^8,
                    epsilon = .05)
reac_s2 = reactive(measure = 2.0,
                    condition = condition_2,
                    reps = 10^8,
                    epsilon = .05)

CSV.write("reac_l0.csv", reac_l0)
CSV.write("reac_l05.csv", reac_l05)
CSV.write("reac_l1.csv", reac_l1)
CSV.write("reac_s2.csv", reac_l0)
                    

Random.seed!(1)
l0 = m1_summary(measure = 0.0, condition = condition_1, payoff = pmo, reps = 40*10^7)
CSV.write("m1_l0.csv", l0[1])

Random.seed!(1)
l05 = m1_summary(measure = 0.5, condition = condition_1, payoff = pmo, reps = 40*10^7)
CSV.write("m1_l05.csv", l05[1])

Random.seed!(1)
l1 = m1_summary(measure = 1.0, condition = condition_1, payoff = pmo, reps = 40*10^7)
CSV.write("m1_l1.csv", l1[1])

Random.seed!(1)
s2 = m1_summary(measure = 2.0, condition = condition_2, payoff = pmo, reps = 40*10^7)
CSV.write("m1_s2.csv", s2[1])




