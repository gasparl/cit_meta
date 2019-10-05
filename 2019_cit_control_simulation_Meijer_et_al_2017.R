set.seed(100)
simulated_sds = c()
for (i in 1:1000) {
    all_preds = c()
    for (i in 1:100) {
        rands = rnorm(n = 5, mean = 0, sd = 1)
        scaled_probe = (rands[1] - mean(rands)) / sd(rands)
        all_preds = c(all_preds, scaled_probe)
    }
    simulated_sds = c(simulated_sds, sd(all_preds))
}
mean(simulated_sds)
# mean_ci(simulated_sds, distance_only = F)
min(simulated_sds)
max(simulated_sds)

plot(density(all_preds))