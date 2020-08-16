# The codes below show possible very simple ways to simulate control data in RT-CIT experiments
# All codes should be run in R - e.g., using RStudio. However, there are even many online websites where R codes can be run from a browser without any additional software, although typically without library support (e.g. https://repl.it/languages/rlang)

# first, let's say we have a liar data SD of 30.45

liar_SD = 30.45

# now, with the formula from the paper you can get the presumed control data SD for the simulation

control_SD_sim = liar_SD * 0.52 + 6.86 # (more precisely 0.5186245 + 6.856039)

# in the simplest case, base R gives the rnorm functions, that gives a random sample from normal distribution
# this may be used simply as follows

control_data_sim = rnorm(n = 10000, mean = 0, sd = control_SD_sim)

# ALTERNATIVELY (1): for more precise results, near-perfect normal distribution can be generating using the distribution_normal function from the bayestestR library

install.packages('bayestestR') # install, if not yet installed
library('bayestestR') # load library

control_data_sim = bayestestR::distribution_normal(n = 10000, mean = 0, sd = control_SD_sim)

# ALTERNATIVELY (2): in fact this distribution_normal function is so simple, it can just be assigned in base R without library
# this way it can be easily used in online websites too

distr_norm = function(n, mean = 0, sd = 1) {
    stats::qnorm(seq(1 / n, 1 - 1 / n, length.out = n), mean, sd)
}

control_data_sim = distr_norm(n = 10000, mean = 0, sd = control_SD_sim)

####

# whichever of the three options you used, now you have the control_data_sim with all simulated datapoints
# this can be used in various ways
# to use it outside R, it can be, for example, copied to clipboard:

writeClipboard(as.character(control_data_sim))

# in R, the AUC can be calculated e.g. using the pROC library (or using the t_neat function of the neatStats library, with auc_added = TRUE) - this would require loading the liar data as well, and then calculating AUC between that and the simulated control data

