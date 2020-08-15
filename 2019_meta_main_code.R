

########################
#  load the packages   #
#                      #
#                      #
########################

library(weights)
library(tidyverse)
library(haven)
options(scipen=999)
library(metafor)
library(schoolmath)
library(MBESS)
# library(nsROC)
library(bayestestR)
library(BayesFactor)
library(pROC)
library(neatStats)
library(esc)
library("openxlsx")


# Set working directory
setwd(path_neat())

# set the functions
source("2019_meta_functions.R")

##### USING cit_meta_data_trial_level.Rda

setwd(path_neat('data'))
load("cit_meta_data_trial_level.Rda")
names(cit_meta_data_trial_level) = c('study', 'cond','multiple_single', 'id','block','dataset','trial','type','corr','rt','stim', 'isi','age','gender')
Data_joined = cit_meta_data_trial_level

temp1 = aggr_neat(Data_joined, 'trial', method = length, group_by = 'id')
temp2 = aggr_neat(old, 'trial', method = length, group_by = c('id', 'dataset', 'cond'))
setdiff(temp1, temp2)
histogram(temp1$aggr_value)
histogram(temp2$aggr_value)

aggr_neat(temp1, 'aggr_group', method = length, group_by = 'aggr_value')
aggr_neat(temp2, 'aggr_group', method = length, group_by = 'aggr_value')

# now we loop through the Data
set.seed(100)
metdat <- tibble()
roc_metdat <- tibble()
count <- 0
l_fig_real = list()
l_fig_sim = list()

add_figs = TRUE # takes time
add_figs = FALSE

for (i in dsets[order(nchar(dsets), dsets)]) {
    # i = "dataset 12"
  dat_i <- filter(Data_joined, dataset == i) # select the current data set

  study_i <- dat_i$study[1]
  cat("\n------------ started ", i, ": ", study_i, fill = T)

  #prep the data
  dat_i_prep <- dat_prep(dat_i$id,dat_i$gender,dat_i$age,dat_i$stim,dat_i$trial,
            dat_i$cond,dat_i$rt,dat_i$corr,dat_i$type,dat_i$multiple_single,dat_i$study)

  ## NOTE: predictors listed below

  ## 0
  # conventional overall probe RT mean minus irrelevant RT mean
  # dat_i_prep$p_vs_i

  ## 1
  # Cohen's d for CIT: dCIT =  (MRT(probes) -  MRT(irrelevants) )/SDRT(irrelevants)
  # so, on trial level, (all_probe_RT_mean minus all_irrelevant_RT_mean) divided by SD_of_all_irrelevants
  # ## // or perhaps pooled SD of probes and irrelevants: sd_pooled(probevector, irrelevantvector)
  #  dat_i_prep$d_cit
  #  dat_i_prep$d_cit_pooled

  ## 2
  # ZScore ("scale") the entire valid RT range, on trial level
  # then calculate "normal" probe-irrelevant mean
  #  dat_i_prep$p_i_diff_pertrial_scaled

  ## 3
  # Aggregate RT for each items, then calculate "Cohen's d for CIT" for these aggregated values
  # we get one or few PROBE means, and at least four IRRELEVANT means
  # Then calculate the difference dCIT:
  # (PROBESmean minus IRRELEVANTSmean) divided by IRRELEVANTsd
  #  dat_i_prep$d_cit_perstim

  ## 4
  # Aggregate RT for each items, then ZScore these aggregated values
  # we get one or few PROBE means, and at least four IRRELEVANT means
  # Then calculate the difference of the "mean of all PROBE means" vs "mean of all IRRELEVANT  means":
  # mean(PROBESmeans) - mean(IRRELEVANTSmeans)
  # dat_i_prep$p_vs_i_scaled_items

  # dat_i_prep$p_vs_i = dat_i_prep$p_vs_i_scaled_items # for quick testing of standardized probe RT

  datnum = as.numeric(strsplit(i, split = " ", fixed = TRUE)[[1]][2])
  sd_met_dat_i = get_sd_info(
      dat_i_prep,
      c(
          'p_vs_i',
          'p_vs_i_scaled_items',
          'd_cit_pooled'
      ),
      study_i,
      datnum
  )


  preds_met_dat_i = data.frame(
      study = study_i,
      dataset = datnum,
      p_vs_i = dat_i_prep$p_vs_i,
      p_vs_i_scaled_items = dat_i_prep$p_vs_i_scaled_items,
      d_cit = dat_i_prep$d_cit,
      d_cit_pooled = dat_i_prep$d_cit_pooled,
      cond = dat_i_prep$cond
  )

  p_i_guilty = filter(dat_i_prep, cond == 1)$p_vs_i
  sd_i <- sd(p_i_guilty) * 0.51 + 7.08
  # sd_i <- 0.894

  # Get the cohens d and stuff
  eff_data <-
    effectsize_data(
      dat_i_prep$id,
      dat_i_prep$p_vs_i,
      dat_i_prep$d_cit,
      dat_i_prep$d_cit_pooled,
      dat_i_prep$p_i_diff_pertrial_scaled,
      dat_i_prep$d_cit_perstim,
      dat_i_prep$p_vs_i_scaled_items,
      dat_i_prep$cond,
      dat_i_prep$multiple_single,
      dat_i_prep$study,
      sd_i,
      equal_n = TRUE # true for equal N, false for 10000
    )
  
  met_dat_i = eff_data[[1]]
  met_dat_i$dataset = datnum
  l_fig_real[[datnum]] = eff_data[[2]]
  l_fig_sim[[datnum]] = eff_data[[3]]

  probe_rt_inn = filter(dat_i_prep, cond == 0)$rt_probe
  irr_rt_inn = filter(dat_i_prep, cond == 0)$rt_irr
  met_dat_i$eff_p_i_control = t_neat(probe_rt_inn, irr_rt_inn, bf_added = F)$stats['d']

  # get ROC data

  roc_met_dat_i <-
      roc_data(
          dat_i_prep$id,
          dat_i_prep$p_vs_i,
          dat_i_prep$cond,
          dat_i_prep$multiple_single,
          dat_i_prep$study,
          sd_i
      )
  roc_met_dat_i$dataset <- i

  if (count > 0) {
      metdat <- rbind(metdat, met_dat_i)
      roc_metdat <- rbind(roc_metdat, roc_met_dat_i)
      sd_metdat <- rbind(sd_metdat, sd_met_dat_i)
      preds_metdat = rbind(preds_metdat, preds_met_dat_i)

  } else
  {
      metdat <- met_dat_i
      roc_metdat <- roc_met_dat_i
      sd_metdat <- sd_met_dat_i
      preds_metdat = preds_met_dat_i
  }
  count = count + 1
  cat("finished", i, ": ", study_i, fill = T)
}


## --- FIGURES

library("ggpubr")
fig_lists = c(l_fig_real, l_fig_sim)
bigfig = ggpubr::ggarrange(plotlist = fig_lists, common.legend = TRUE, ncol = 12, nrow = 2, labels = c(paste(1:12, 'real'), paste(1:12, 'sim')), label.x = 0.17 )
annotate_figure(
    bigfig,
    top = text_grob("Density plots per each dataset, real and simulated", face = "bold", size = 14),
    bottom = text_grob("probe-irrelevant differences (ms)"),
    left = text_grob("density estimate", rot = 90)
)


## --- Standard Deviations
# unique(sd_metdat$version): p_vs_i p_vs_i_scaled_items d_cit_pooled

stat_dat = sd_metdat[sd_metdat$version == "p_vs_i",]

mean(stat_dat$m_g)
mean(stat_dat$sd_g)
neatStats::mean_ci(stat_dat$sd_g, distance_only = F)
g_sd_max = max(stat_dat$sd_g)
g_sd_min = min(stat_dat$sd_g)
#mean(stat_dat$m_i)
mean(stat_dat$sd_i)
neatStats::mean_ci(stat_dat$sd_i, distance_only = F)
i_sd_max = max(stat_dat$sd_i)
i_sd_min = min(stat_dat$sd_i)

# simulated standardized probe RT's SD mean: 0.894
# t.test(stat_dat$sd_i, mu = 0.894)

#stat_dat = stat_dat[stat_dat$dataset != 9, ]
t_neat(stat_dat$sd_g, stat_dat$sd_i, pair = T, round_descr = 1)
corr_neat(stat_dat$sd_g, stat_dat$sd_i)
weights::wtd.cor(stat_dat$sd_g, stat_dat$sd_i, weight = (stat_dat$n_g+stat_dat$n_i))

model = lm(sd_i~sd_g, data = stat_dat)
summary(model)
model = lm(sd_i~sd_g, data = stat_dat, weights = (stat_dat$n_g+stat_dat$n_i))
summary(model)


stat_dat$sd_sim2 = stat_dat$sd_g * model$coefficients[2] + model$coefficients[1]
# weighted: 0.5112 + 7.0784
# unweighed: 0.4528 + 8.1529
#stat_dat$sd_sim2 = stat_dat$sd_g * 0.52479 +0.16970
stat_dat$sd_sim3 = stat_dat$sd_g

t_neat( stat_dat$sd_i, stat_dat$sd_sim2, pair = T )
t_neat( stat_dat$sd_i, stat_dat$sd_sim3, pair = T )
t_neat( stat_dat$sd_i, stat_dat$sd_sim3, pair = T )

ggplot(stat_dat, aes(
    x = sd_g,
    y = sd_i,
    weight = (stat_dat$n_g + stat_dat$n_i)
)) +
    theme_bw() +
    geom_point(aes(y = stat_dat$sd_sim3), shape = 3,
               size = 3) +
    geom_smooth(
        method = lm,
        fullrange = TRUE,
        level = .95,
        color = "#111111",
        size = 0.7
    ) +
    geom_point(
        aes(y = stat_dat$sd_sim2),
        shape = 16,
        size = 4,
        color = "#FFFFFF"
    ) +
    geom_text(label = stat_dat$dataset,
              color = "#000000",
              size = 4) +
    ylim(0, 50) +
    xlim(0, 50) + xlab("\nLiar data SD") + ylab("Control data SD\n") +
    theme(text = element_text(family = "serif", size = 15))

# SD and mean diff

met_means_sds = sd_metdat
met_means_sds$mean_diff = met_means_sds$m_g - met_means_sds$m_i

met_means_sds = met_means_sds[met_means_sds$version == 'p_vs_i',] # p_vs_i, d_cit_pooled, p_vs_i_scaled_items
corr_neat(met_means_sds$mean_diff, met_means_sds$sd_g)
weights::wtd.cor(met_means_sds$mean_diff, met_means_sds$sd_g, weight = (met_means_sds$n_g+met_means_sds$n_i))

ggplot(stat_dat, aes(x = sd_g, y = sd_i, weight = (stat_dat$n_g+stat_dat$n_i) )) +
    theme_bw() +
    geom_point(aes(y = stat_dat$sd_sim3), shape = 3) +
    geom_smooth(
        method = lm,
        fullrange = TRUE,
        level = .95,
        color = "#111111",
        size = 0.5
    ) +
    geom_point(aes(y = stat_dat$sd_sim2), shape = 16, size = 3, color = "#FFFFFF") +
    geom_text(label = stat_dat$dataset, color = "#000000")


## -- Accuracies - cross-validated

accs_cv = NULL
for (pred_type in c("p_vs_i", "d_cit", "d_cit_pooled", "p_vs_i_scaled_items")) {
    met_thres = metdat[metdat$version == pred_type, ]

    for (stud in unique(met_thres$dataset)) {
        thres_orig = met_thres$thresholds[met_thres$dataset == stud]
        thres_mean = mean(met_thres$thresholds[met_thres$dataset != stud])
        thres_median = median(met_thres$thresholds[met_thres$dataset != stud])
        preds_stud = preds_metdat[preds_metdat$dataset == stud, ]
        preds_guilty = preds_stud[[pred_type]][preds_stud$cond == 1]
        preds_innocent = preds_stud[[pred_type]][preds_stud$cond == 0]
        tpr = length(preds_guilty[preds_guilty > thres_orig]) / length(preds_guilty)
        tnr = length(preds_innocent[preds_innocent < thres_orig]) / length(preds_innocent)
        acc = (tpr + tnr) / 2

        tpr_mean = length(preds_guilty[preds_guilty > thres_mean]) / length(preds_guilty)
        tnr_mean = length(preds_innocent[preds_innocent < thres_mean]) / length(preds_innocent)
        acc_mean = (tpr_mean + tnr_mean) / 2

        tpr_med = length(preds_guilty[preds_guilty > thres_median]) / length(preds_guilty)
        tnr_med = length(preds_innocent[preds_innocent < thres_median]) / length(preds_innocent)
        acc_med = (tpr_med + tnr_med) / 2
        new_accs_cv = data.frame(
            dataset = stud,
            version = pred_type,
            acc_orig = acc,
            acc_cv_mean = acc_mean,
            acc_cv_med = acc_med,
            TPs_orig = tpr,
            TNs_orig = tnr,
            TPs_cv_mean = tpr_mean,
            TNs_cv_mean = tnr_mean,
            TPs_cv_med = tpr_med,
            TNs_cv_med = tnr_med
        )
        if (is.null(accs_cv)) {
            accs_cv = new_accs_cv
        } else {
            accs_cv = rbind(accs_cv, new_accs_cv)
        }
    }
}

aggr_neat(accs_cv, values = "acc_orig", group_by = c("version"), method = 'mean+sd')
aggr_neat(accs_cv, values = "acc_cv_mean", group_by = c("version"), method = 'mean+sd')
aggr_neat(accs_cv, values = "acc_cv_med", group_by = c("version"), method = 'mean+sd')

aggr_neat(accs_cv, values = "TPs_orig", group_by = c("version"), method = 'mean+sd')
aggr_neat(accs_cv, values = "TPs_cv_mean", group_by = c("version"), method = 'mean+sd')
aggr_neat(accs_cv, values = "TPs_cv_med", group_by = c("version"), method = 'mean+sd')

aggr_neat(accs_cv, values = "TNs_orig", group_by = c("version"), method = 'mean+sd')
aggr_neat(accs_cv, values = "TNs_cv_mean", group_by = c("version"), method = 'mean+sd')
aggr_neat(accs_cv, values = "TNs_cv_med", group_by = c("version"), method = 'mean+sd')

accs_cv_for_aov = accs_cv
accs_cv_for_aov$version = as.character(accs_cv_for_aov$version)
accs_cv_for_aov$version[accs_cv_for_aov$version == 'p_vs_i'] = 'p_vs_i_basic'
accs_cv_wide = reshape(
    as.data.frame(accs_cv_for_aov[accs_cv_for_aov$version %in% c("p_vs_i_basic", "d_cit_pooled", "p_vs_i_scaled_items"), c(
        "dataset",
        'version',
        'acc_orig',
        'acc_cv_mean',
        'acc_cv_med',
        'TPs_orig',
        'TPs_cv_mean',
        'TPs_cv_med',
        'TNs_orig',
        'TNs_cv_mean',
        'TNs_cv_med'
    )]),
    idvar = "dataset",
    timevar = "version",
    direction = "wide"
)
# cat(names(accs_cv_wide), sep = "', '", fill = T)

##
anova_neat(
    accs_cv_wide,
    values = c(
        'TPs_orig.p_vs_i_basic',
        'TPs_cv_mean.p_vs_i_basic',
        'TPs_orig.d_cit_pooled',
        'TPs_cv_mean.d_cit_pooled',
        'TPs_orig.p_vs_i_scaled_items',
        'TPs_cv_mean.p_vs_i_scaled_items',
        'TNs_orig.p_vs_i_basic',
        'TNs_cv_mean.p_vs_i_basic',
        'TNs_orig.d_cit_pooled',
        'TNs_cv_mean.d_cit_pooled',
        'TNs_orig.p_vs_i_scaled_items',
        'TNs_cv_mean.p_vs_i_scaled_items'
    ),
    within_ids = list(
        Cutoff = c('_orig', '_mean'),
        Condition = c('TPs_', 'TNs_'),
        Predictor = c('p_vs_i_basic', 'd_cit_pooled', 'p_vs_i_scaled_items')
    )
)

plot_neat(
    accs_cv_wide,
    values = c(
        'TPs_orig.p_vs_i_basic',
        'TPs_cv_mean.p_vs_i_basic',
        'TPs_orig.d_cit_pooled',
        'TPs_cv_mean.d_cit_pooled',
        'TPs_orig.p_vs_i_scaled_items',
        'TPs_cv_mean.p_vs_i_scaled_items',
        'TNs_orig.p_vs_i_basic',
        'TNs_cv_mean.p_vs_i_basic',
        'TNs_orig.d_cit_pooled',
        'TNs_cv_mean.d_cit_pooled',
        'TNs_orig.p_vs_i_scaled_items',
        'TNs_cv_mean.p_vs_i_scaled_items'
    ),
    within_ids = list(
        orig_vs_cv = c('_orig', '_mean'),
        acc_type = c('TPs_', 'TNs_'),
        pred_type = c('p_vs_i_basic', 'p_vs_i_scaled_items', 'd_cit_pooled')
    ),
    eb_method = sd,
    type = "bar",
    panel = 'acc_type',
    factor_names = c(orig_vs_cv = 'Cutoff', pred_type = ''),
    value_names = c(
        p_vs_i_basic = 'MPID',
        p_vs_i_scaled_items = 'SPRT',
        d_cit_pooled = 'SPID',
        '_orig' = 'Optimal',
        '_mean' = 'Inferred',
        TPs_ = 'True positive rate',
        TNs_ = 'True negative rate'
    )
) + theme(text = element_text(size = 21)) + scale_y_continuous(breaks = c(
    "0" = 0,
    ".25" = 0.25,
    ".50" = 0.5,
    ".75" = 0.75
))


anova_neat(
    accs_cv_wide,
    values = c(
        'TPs_orig.p_vs_i_basic',
        'TPs_cv_med.p_vs_i_basic',
        'TPs_orig.d_cit_pooled',
        'TPs_cv_med.d_cit_pooled',
        'TPs_orig.p_vs_i_scaled_items',
        'TPs_cv_med.p_vs_i_scaled_items',
        'TNs_orig.p_vs_i_basic',
        'TNs_cv_med.p_vs_i_basic',
        'TNs_orig.d_cit_pooled',
        'TNs_cv_med.d_cit_pooled',
        'TNs_orig.p_vs_i_scaled_items',
        'TNs_cv_med.p_vs_i_scaled_items'
    ),
    within_ids = list(
        Cutoff = c('_orig', '_med'),
        Condition = c('TPs_', 'TNs_'),
        Predictor = c('p_vs_i_basic', 'd_cit_pooled', 'p_vs_i_scaled_items')
    )
)
plot_neat(
    accs_cv_wide,
    values = c(
        'TPs_orig.p_vs_i_basic',
        'TPs_cv_med.p_vs_i_basic',
        'TPs_orig.d_cit_pooled',
        'TPs_cv_med.d_cit_pooled',
        'TPs_orig.p_vs_i_scaled_items',
        'TPs_cv_med.p_vs_i_scaled_items',
        'TNs_orig.p_vs_i_basic',
        'TNs_cv_med.p_vs_i_basic',
        'TNs_orig.d_cit_pooled',
        'TNs_cv_med.d_cit_pooled',
        'TNs_orig.p_vs_i_scaled_items',
        'TNs_cv_med.p_vs_i_scaled_items'
    ),
    within_ids = list(
        orig_vs_cv = c('_orig', '_med'),
        acc_type = c('TPs_', 'TNs_'),
        pred_type = c('p_vs_i_basic', 'd_cit_pooled', 'p_vs_i_scaled_items')
    ), eb_method = sd, type = "bar", panel = 'acc_type',
    factor_names = c(orig_vs_cv = 'Cutoff', pred_type = ''),
    value_names = c(
        p_vs_i_basic = 'MPID',
        p_vs_i_scaled_items = 'SPRT',
        d_cit_pooled = 'SPID',
        '_orig' = 'Optimal',
        '_med' = 'Inferred',
        TPs_ = 'True positive rate',
        TNs_ = 'True negative rate'
    )
) + theme(text = element_text(size = 21)) + scale_y_continuous(breaks = c(
    "0" = 0,
    ".25" = 0.25,
    ".50" = 0.5,
    ".75" = 0.75
))
# follow-up for medians
anova_neat(
    accs_cv_wide,
    values = c(
        'acc_orig.d_cit_pooled',
        'acc_cv_med.d_cit_pooled',
        'acc_orig.p_vs_i_scaled_items',
        'acc_cv_med.p_vs_i_scaled_items'
    ),
    within_ids = list(
        orig_vs_cv = c('acc_orig', 'acc_cv_med'),
        pred_type = c('d_cit_pooled', 'p_vs_i_scaled_items')
    )
)
anova_neat(
    accs_cv_wide,
    values = c(
        'acc_orig.p_vs_i_basic',
        'acc_cv_med.p_vs_i_basic',
        'acc_orig.p_vs_i_scaled_items',
        'acc_cv_med.p_vs_i_scaled_items'
    ),
    within_ids = list(
        orig_vs_cv = c('acc_orig', 'acc_cv_med'),
        pred_type = c('p_vs_i_basic', 'p_vs_i_scaled_items')
    )
)
anova_neat(
    accs_cv_wide,
    values = c(
        'acc_orig.p_vs_i_basic',
        'acc_cv_med.p_vs_i_basic',
        'acc_orig.d_cit_pooled',
        'acc_cv_med.d_cit_pooled'
    ),
    within_ids = list(
        orig_vs_cv = c('acc_orig', 'acc_cv_med'),
        pred_type = c('p_vs_i_basic', 'd_cit_pooled')
    )
)
plot_neat(
    accs_cv_wide,
    values = c(
        'acc_orig.p_vs_i_basic',
        'acc_cv_med.p_vs_i_basic',
        'acc_orig.d_cit_pooled',
        'acc_cv_med.d_cit_pooled',
        'acc_orig.p_vs_i_scaled_items',
        'acc_cv_med.p_vs_i_scaled_items'
    ),
    within_ids = list(
        acc_type = c('acc_orig', 'acc_cv_med'),
        pred_type = c('p_vs_i_basic', 'd_cit_pooled', 'p_vs_i_scaled_items')
    ), eb_method = sd, type = "bar"
)

## -- Meta-analysis

## sim Fig
# p_vs_i
fig_dat = metdat[metdat$version %in% c("p_vs_i", "simulated"), c("version", "dataset", "aucs", "auc_lower", "auc_upper")]
fig_dat$dataset = as.factor(fig_dat$dataset)
fig_dat$Simulated = 'Real'
fig_dat$Simulated[fig_dat$version == "simulated"] = 'Simulated'
ggplot2::ggplot(data = fig_dat, aes(x = dataset,
                                    y = aucs,
                                    fill = Simulated)) +
    geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
    scale_fill_manual(values = c('#AAAAAA', '#333333'), name = NULL) +
    geom_errorbar(aes(
        ymin = fig_dat$auc_lower,
        ymax = fig_dat$auc_upper,
        width = 0.2
    ),
    position = position_dodge(0.9)) + theme_bw() +
    theme(panel.grid.major.x = element_blank())  +
    scale_y_continuous(breaks = c(
        "0" = 0,
        ".25" = 0.25,
        ".50" = 0.5,
        ".75" = 0.75
    )) +
    ylab("Area under the curve") + xlab("Dataset (individual experimental design)") +
    theme(
        panel.grid.major.y = element_line(color = "#d5d5d5"),
        panel.grid.minor.y = element_line(color = "#d5d5d5"),
        legend.position = "bottom",
        text = element_text(family = "serif", size = 17)
    )


### META-ANALYSES

aggr_neat(metdat, cohens_d, method = "mean+sd", group_by = 'version')
aggr_neat(metdat, aucs, method = "mean+sd", group_by = 'version')

met_stat = metdat[metdat$version %in% c("p_vs_i", "d_cit_pooled", "p_vs_i_scaled_items"),]
met_stat = metdat[metdat$version %in% c("p_vs_i","simulated"),] # this is to easily change the examined variable

met_stat = met_stat[order(met_stat$dataset, met_stat$version),]

met_stat$multiple_single[met_stat$multiple_single == 'multiple'] = 'MP'
met_stat$multiple_single[met_stat$multiple_single == 'single'] = 'SP'
met_stat$multiple_single[met_stat$multiple_single == 'inducer'] = 'SPF'
met_stat$simulated = 'No'
met_stat$simulated[met_stat$version == 'simulated'] = 'Yes'

met_stat$crowdsourced = "Yes"
met_stat$crowdsourced[grepl( "Noordraven & Verschuere", met_stat$study )] = "No"
met_stat$crowdsourced[grepl( "Verschuere & Kleinberg (2015)", met_stat$study, fixed = T )] = "No"

# reshape(
#     as.data.frame(met_stat[, c("study", 'version', 'aucs')]),
#     idvar = "study",
#     timevar = "version",
#     direction = "wide"
# )
# thresholds = reshape(
#     as.data.frame(met_stat[, c("study", 'version', 'thresholds')]),
#     idvar = "study",
#     timevar = "version",
#     direction = "wide"
# )

# met_stat$multiple_single[met_stat$multiple_single == "inducer"] = "multiple"

REML_multi <-
    rma(
        cohens_d,
        variance_d,
        data = met_stat,
        method = "REML",
        mods = ~ relevel(factor(version), ref = "p_vs_i") + relevel(factor(multiple_single), ref = "SP") + crowdsourced #, level = 0.9
    )
REML_multi
## this is to compare pairwise the third pair
REML_multi <-
    rma(
        cohens_d,
        variance_d,
        data = met_stat,
        method = "REML",
        mods = ~ relevel(factor(version), ref = "d_cit_pooled") + relevel(factor(multiple_single), ref = "MP") + crowdsourced #, level = 0.9
    )
REML_multi
## this is to compare pairwise the third pair
REML_multi <-
    rma(
        cohens_d,
        variance_d,
        data = met_stat,
        method = "REML",
        mods = ~ relevel(factor(version), ref = "p_vs_i") + relevel(factor(multiple_single), ref = "MP") + crowdsourced #, level = 0.9
    )
REML_multi
# here the tests for multi-level factors
anova(REML_multi, btt=2:3)
anova(REML_multi, btt=4:5)
anova(REML_multi, btt=3:4)

forest(
    REML_multi,
    psize = 1,
    xlab = NULL,
    xlim = c(-10, 5.9),
    slab = met_stat$study,
    ilab = cbind(
        met_stat$multiple_single,
        met_stat$crowdsourced,
        met_stat$simulated
    ),
    ilab.xpos = c(-4.0, -2.6, -1.3),
    cex = 0.75,
    fonts = 'serif'
)  # plot it

text(
    x = c(-9.1, -4.1, -2.7, -1.2, 3.0),
    y = 24,
    labels = c(
        "Dataset title",
        "Protocol",
        "Crowdsourced",
        "Simulated",
        "Effect Size (Cohen's d) and 95% CI"
    ),
    font = 2,
    family = 'serif',
    cex = .9
)



## ANOVA for AUC across different predictors

met_stat_wide = data.frame(met_stat)
met_stat_wide$version[met_stat_wide$version == 'p_vs_i'] = 'p_vs_i_basic'
met_stat_wide = reshape(
    as.data.frame(met_stat_wide[met_stat_wide$version %in% c("p_vs_i_basic", "d_cit_pooled", "p_vs_i_scaled_items", "simulated"), c(
        "dataset",
        'version',
        'aucs',
        'auc_lower',
        'auc_upper'
    )]),
    idvar = "dataset",
    timevar = "version",
    direction = "wide"
)

aggr_neat(met_stat, aucs, method = "mean+sd", group_by = 'version')
anova_neat(
    met_stat_wide,
    values = c(
        'aucs.p_vs_i_basic',
        'aucs.d_cit_pooled',
        'aucs.p_vs_i_scaled_items'
    ),
    within_ids = 'predictor_version'
)

## t-test for AUC between simulated and real

t_neat(metdat$aucs[metdat$version == 'simulated'],
       metdat$aucs[metdat$version == 'p_vs_i'],
       pair = T,
       round_descr = 3)

corr_neat(metdat$aucs[metdat$version == "p_vs_i"], metdat$aucs[metdat$version == "simulated"])

weights::wtd.cor(metdat$aucs[metdat$version == "p_vs_i"], metdat$aucs[metdat$version == "simulated"], weight = (sd_metdat$n_g[sd_metdat$version == "p_vs_i"]+sd_metdat$n_i[sd_metdat$version == "p_vs_i"]))


## ROC

t_neat(metdat$aucs[metdat$version %in% c("simulated")],
       metdat$aucs[metdat$version %in% c("p_vs_i")],
       plot_densities = T,
       pair = T)

roc_met = roc_metdat

roc_met$Author = paste(roc_met$study, roc_met$multiple_single) # to separate studies
roc_met$crowdsourced = "yes"
roc_met$crowdsourced[grepl( "Noordraven & Verschuere", roc_met$study )] = "no"
roc_met$crowdsourced[grepl( "Verschuere & Kleinberg (2015)", roc_met$study, fixed = T )] = "no"
roc_met$multiple_single[roc_met$multiple_single == "inducer"] = "multiple"

unique(roc_metdat$study)
unique(met_stat$study)
unique(roc_met$Author)

roc_met = roc_met[,c('TP', 'FP', 'FN', 'TN', 'simulated', 'multiple_single', 'Author', 'crowdsourced')]

# these below take forever; sth must be modified

# metaROC(roc_met, plot.Author=TRUE)

# roc_results = metaROC(roc_met, plot.Author=TRUE, model="random-effects")


### JOINT TABLES

unique(out_aucs$version)
unique(out_descriptives$version)
unique(out_main$version)
names(out_aucs)

out_main = metdat
out_descriptives = sd_metdat
out_aucs = accs_cv
out_main = out_main[out_main$version %in% c("d_cit_pooled", "p_vs_i_scaled_items", "p_vs_i", "simulated"), !(names(out_main) %in% c("eff_p_i_control"))]
out_aucs = out_aucs[out_aucs$version %in% c("d_cit_pooled", "p_vs_i_scaled_items", "p_vs_i", "simulated"),  !(names(out_aucs) %in% c("eff_p_i_control"))]

out_main$crowdsourced = "Yes"
out_main$crowdsourced[grepl( "Noordraven & Verschuere", out_main$study )] = "No"
out_main$crowdsourced[grepl( "Verschuere & Kleinberg (2015)", out_main$study, fixed = T )] = "No"

out_full = merge(out_main, out_descriptives, all = TRUE)
out_full = merge(out_full, out_aucs, all = TRUE)

# save(out_full, file="cit_meta_data_aggregated_EQUAL_SIM_SAMPLE.Rda")

out_full_10000 = out_full

# save(out_full_10000, file="cit_meta_data_aggregated_10000_SIM_SAMPLE.Rda")
