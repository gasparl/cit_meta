library('neatStats')
setwd(path_neat())

load("prevs/cit_meta_data_aggregated_EQUAL_SIM_SAMPLE.Rda")
load("prevs/cit_meta_data_aggregated_10000_SIM_SAMPLE.Rda")

names(out_full_10000)

out_full_10000 = out_full_10000[out_full_10000$version == "simulated", ]
out_full_10000 = out_full_10000[colSums(!is.na(out_full_10000)) > 0]
out_full_10000$multiple_single = NULL
out_full_10000$crowdsourced = NULL

names(out_full_10000)[4:11] = paste0(names(out_full_10000), "_n10000")[4:11]

names(out_full)

out_full_both = merge(out_full, out_full_10000, all = TRUE)

out_full_both[out_full_both == 'p_vs_i_scaled_items'] = 'p_standard'
out_full_both[out_full_both == 'd_cit_pooled'] = 'p_vs_i_standard'
out_full_both[out_full_both == 'simulated'] = 'p_vs_i_simulated'
out_full_both[out_full_both == 'inducer'] = 'filler'

out_full_both$predictor = out_full_both$version
out_full_both$protocol = out_full_both$multiple_single

unique(out_full_both$predictor)

cit_meta_data_aggregated = out_full_both[c(
    'dataset',
    # simply an assigned number of the given data from each particular experimental design with both liar and control data, based on the alphabetic order of the study reference names
    'study',
    # study reference
    'protocol',
    # multiple: multiple-probe protocol, single: single-probe protocol, filler: single-probe protocol with fillers,
    'crowdsourced',
    # participants crowdsourced or not
    'predictor',
    # the type of predictor; p_vs_i: mean probe-irrelevant difference, p_standard: standardized probe RT, p_vs_i_standard: standardized probe-irrelevant difference, p_vs_i_simulated: mean probe-irrelevant difference with simulated control data
    'n_g',
    # number of liar participants
    'n_i',
    # number of truthteller participants
    'cohens_d',
    # liar-truthteller effect size used for meta analysis
    'variance_d',
    #  variance used for meta analysis
    'sed',
    #  standard error of the difference used for meta analysis
    'm_g',
    # predictor mean for liar
    'm_i',
    # predictor mean for truthteller
    'sd_g',
    # predictor SD for liar
    'sd_i',
    # predictor SD for truthteller
    'aucs',
    # AUC
    'auc_lower',
    # AUC 95% CI lower boundarity
    'auc_upper',
    # AUC 95% CI upper boundarity
    'thresholds',
    # optimal threshold for AUC
    'acc_orig',
    # overall classification accuracy using optimal threshold
    'TPs_orig',
    # TNRs using optimal threshold
    'TNs_orig',
    # TPRs using optimal threshold
    'acc_cv_mean',
    # overall classification accuracy using the MEAN of optimal threshold of other studies
    'TPs_cv_mean',
    'TNs_cv_mean',
    'acc_cv_med',
    # overall classification accuracy using the MEDIAN of optimal threshold of other studies
    'TPs_cv_med',
    'TNs_cv_med',
    'aucs_n10000',
    # same as above, but with 10000 simulated datapoints (the previous columns contain simulated datapoints equal to the real control sample size in the given experimental design)
    'accuracies_n10000',
    'thresholds_n10000',
    'TNs_n10000',
    'TPs_n10000'
)]

cat(names(cit_meta_data_aggregated), sep = "', '")

save(cit_meta_data_aggregated, file = "cit_meta_data_aggregated.Rda")

##########

load("OSF/cit_meta_data_aggregated.Rda")
wryt = cit_meta_data_aggregated
wryt$protocol[wryt$protocol == 'multiple'] = 'MP'
wryt$protocol[wryt$protocol == 'single'] = 'SP'
wryt$protocol[wryt$protocol == 'filler'] = 'SPF'
wryt$study = paste(wryt$study, wryt$protocol)
wryt$aucs[wryt$predictor == 'p_vs_i_simulated'] = wryt$aucs_n10000[wryt$predictor == 'p_vs_i_simulated']

cat(names(wryt), sep = "', '")
cat(names(wryt), sep = "\n")

wryt$cohens_d = ro(wryt$cohens_d, round_to = 2)
wryt$variance_d = ro(wryt$variance_d, round_to = 2)

wryt$cohens_d = ro(wryt$cohens_d , round_to = 2)
wryt$variance_d = ro(wryt$variance_d , round_to = 2)
wryt$sed = ro(wryt$sed , round_to = 2)
for (pred in unique(wryt$predictor)) {
    if (pred %in% c("p_vs_i", "p_vs_i_simulated")) {
        r_to = 1
    } else {
        r_to = 2
    }
    wryt$m_g[wryt$predictor == pred] = neatStats::ro(wryt$m_g[wryt$predictor == pred], round_to = r_to)
    wryt$m_i[wryt$predictor == pred] = neatStats::ro(wryt$m_i[wryt$predictor == pred], round_to = r_to)
    wryt$sd_g[wryt$predictor == pred] = neatStats::ro(wryt$sd_g[wryt$predictor == pred], round_to = r_to)
    wryt$sd_i[wryt$predictor == pred] = neatStats::ro(wryt$sd_i[wryt$predictor == pred], round_to = r_to)
    wryt$thresholds[wryt$predictor == pred] = neatStats::ro(wryt$thresholds[wryt$predictor == pred], round_to = r_to)
}
wryt$aucs = substring(neatStats::ro(wryt$aucs , round_to = 2), 2)
wryt$auc_lower = substring(neatStats::ro(wryt$auc_lower , round_to = 2), 2)
wryt$auc_upper = substring(neatStats::ro(wryt$auc_upper , round_to = 2), 2)

wryt$acc_orig = neatStats::ro(wryt$acc_orig , round_to = 2)
wryt$TPs_orig = neatStats::ro(wryt$TPs_orig , round_to = 2)
wryt$TNs_orig = neatStats::ro(wryt$TNs_orig , round_to = 2)
wryt$acc_cv_mean = neatStats::ro(wryt$acc_cv_mean , round_to = 2)
wryt$TPs_cv_mean = neatStats::ro(wryt$TPs_cv_mean , round_to = 2)
wryt$TNs_cv_mean = neatStats::ro(wryt$TNs_cv_mean , round_to = 2)

table1long = wryt[wryt$predictor != 'p_vs_i_simulated',
                  c('dataset',
                    'study',
                    'predictor',
                    'n_g',
                    'n_i',
                    'm_g',
                    'm_i',
                    'sd_g',
                    'sd_i')]

table1 = reshape(
    table1long,
    timevar = "predictor",
    idvar = c("dataset", "study", 'n_g', 'n_i'),
    direction = "wide",
    sep = "|"
)
