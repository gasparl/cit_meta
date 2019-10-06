

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
library(nsROC)
library(bayestestR)
library(BayesFactor)
library(pROC)
library(neatStats)
library(esc)

add_figs = TRUE # takes time
add_figs = FALSE

# Set working directory
setwd(path_neat())

# set the functions
source("2019_meta_functions.R")


########################
#  Read in the data    #
#                      #
########################

setwd(path_neat("data"))

Data_VKT <- read_sav("Experiment2_Data_SPSSformat.sav") # Verschuere, Kleinberg, & Theocharidou (2015) SP/MP https://osf.io/kgum2/
Data_KV1 <- read_sav("MemoryDetection2_0_Study1_rawdata_SPSS.sav") # Kleinberg & Verschuere (2015) Study 1 https://osf.io/5htyr/
Data_KV2 <- read_sav("MemoryDetection2_0_Study2_rawdata_SPSS.sav") # Kleinberg & Verschuere (2015) Study 2 https://osf.io/5htyr/
Data_VK <-  read_sav("rawdata_online-id-check.sav") # Verschuere & Kleinberg (2015) https://osf.io/cg5es/
Data_LKV <- read_tsv("Experiment1_data.txt") # Lukács, Kleinberg, & Verschuere (2017) MP/SP/SPF https://osf.io/kv65n/
Data_NV <- read_tsv("Oddball Ernst PP 1- 43 FINAL.txt") # Noordraven & Verschuere (2013)
Data_KV21 <- read_tsv("rawdata_Exp1.txt") # Kleinberg & Verschuere (2016) Study 1 https://osf.io/c7w5v/
Data_KV22 <- read_tsv("rawdata_Exp2.txt") # Kleinberg & Verschuere (2016) Study 2 https://osf.io/c7w5v/


########################
#  Unify &             #
#  combine data        #
#                      #
########################

#this one has multiple and sinle probes
# We want to seperate the files one for single and one for multiple probes
Data_VKT <- Data_VKT %>%
    mutate(
        multiple_single = ifelse(cond < 2, 1, 0),
        # 1 = multiple, 0 = single
        cond = ifelse(is.even(cond), 1, 0),
        # here we set 0 & 2 to guilty (=1) and 1 & 3 to innocent (=0),
        study = "Verschuere, Kleinberg, & Theocharidou (2015) Exp2",
        dataset = ifelse(multiple_single == 0, "dataset 11", "dataset 10")
    )


Data_KV1 <- Data_KV1 %>%
            mutate(multiple_single = 1, # 1 = multiple, 0 = single
            study = "Kleinberg & Verschuere (2015) Exp1",
            dataset = "dataset 1",
            cohd = as.numeric(cohd)) #for some reason cohens d is a charachter here which leads to issues with joining

Data_KV2 <- Data_KV2 %>%
             mutate(multiple_single = 1, # 1 = multiple, 0 = single
             study = "Kleinberg & Verschuere (2015) Exp2",
             dataset = "dataset 2")

Data_VK <- Data_VK %>%
           mutate(multiple_single = 1, # 1 = multiple, 0 = single
           stim = paste(cat, type, sep = "_"),
           study = "Verschuere & Kleinberg (2015)",
           dataset = "dataset 9")

Data_LKV <- Data_LKV %>%
    mutate(
        multiple_single = ifelse(cond == 1 |
                                     cond == 4, 1, 0),
        # 1 = multiple, 0 = single
        multiple_single = ifelse(cond == 2 |
                                     cond == 5, 2, multiple_single),
        # 2 means inducer
        cond = ifelse(cond < 3, 1, 0),
        # here we set 0, 1, 2 to guilty (=1) and 3, 4, 5 to innocent (=0),
        study = 'Lukács, Kleinberg, & Verschuere (2017) Exp1',
        dataset = ifelse(multiple_single == 0, "dataset 6", "dataset 5"),
        dataset = ifelse(multiple_single == 2, "dataset 7", dataset)
    )

required_cols = c("id", "date", "cond", "rt", "type", "corr", "multiple_single", "study", "dataset", "age", "gender", "stim", "trial") # this is all the data we need

# below Gaspar-style filter, sorry
NV_guilty = c( 1, 2, 3, 8, 9, 11, 12, 14, 15, 19, 20, 22, 25, 26, 29, 31, 32, 34, 37, 39, 41, 42 )
NV_innocent = c( 4, 5, 6, 7, 10, 13, 16, 17, 18, 21, 23, 24, 27, 28, 30, 33, 35, 36, 38, 40, 43 )
Data_NV = Data_NV[! is.na(Data_NV$rt ),] # remove irrelevant lines
Data_NV = Data_NV[! is.na(Data_NV$blockcode != "CIT_lr_oefen" ),] # remove practice
Data_NV = Data_NV[grepl("_crime", Data_NV$trialcode),] # remove non-crime part
Data_NV$cond = NA
Data_NV$cond[Data_NV$id %in% NV_guilty] = 1
Data_NV$cond[Data_NV$id %in% NV_innocent] = 0
Data_NV$type = NA
Data_NV$type[grepl('^irrelevant', Data_NV$trialcode)] = "irrelevant"
Data_NV$type[grepl('^probe', Data_NV$trialcode)] = "probe"
Data_NV$type[grepl('^target', Data_NV$trialcode)] = "target"

Data_NV <- Data_NV %>%
    mutate(multiple_single = 1, # 1 = multiple, 0 = single
           study = "Noordraven & Verschuere (2013)",
           dataset = "dataset 8",
           trial = as.numeric(trialnum),
           age = 99, # just filler, no need for this now
           gender = 9, # just filler, no need for this now
           id = as.numeric(id),
           rt = as.numeric(rt),
           corr = as.numeric(corr))

Data_NV = Data_NV[,required_cols]

Data_KV21 <- Data_KV21 %>%
    mutate(multiple_single = 1, # 1 = multiple, 0 = single
           cond = ifelse(cond == 1, 0, 1 ), # here we set 0 & 2 to guilty (=1) and 1 to innocent (=0),
           study = "Kleinberg & Verschuere (2016) Exp1",
           dataset = "dataset 3")
Data_KV21 = Data_KV21[,required_cols]

Data_KV22$trial = as.numeric(seq(nrow(Data_KV22))) # has no trial column, so i create it artificially
Data_KV22 <- Data_KV22 %>%
    mutate(multiple_single = 1, # 1 = multiple, 0 = single
           cond = ifelse(cond == 0, 0, 1 ), # here we set 1 & 2 & 3 to guilty (=1) and 0 to innocent (remains 0), ,
           id = paste0(date, age, lang, cohd), # for some reason it has no ids; so i create one
           study = "Kleinberg & Verschuere (2016) Exp2",
           dataset = "dataset 4")
Data_KV22 = Data_KV22[,required_cols]

# join them all together
# joining unfortunately only takes 2 arguments but it has other benefits so its nicer this way

Data_joined <- full_join(Data_VKT, Data_KV1)
Data_joined <- full_join(Data_joined, Data_KV2)
Data_joined <- full_join(Data_joined, Data_VK)
Data_joined <- full_join(Data_joined, select(Data_LKV,-date)) # the date column leads to an issue so we take it out because we dont need it
Data_joined <- full_join(Data_joined, Data_NV)
Data_joined <- full_join(Data_joined, select(Data_KV21,-date)) # date problem again
Data_joined$id = as.character(Data_joined$id) # gotto convert it because last one is char
Data_joined <- full_join(Data_joined, select(Data_KV22,-date)) # date problem again
dsets = unique(Data_joined$dataset)

# saveRDS(Data_joined, file="2019_meta_data_trial_level.Rds")

# length(unique(paste(Data_joined$dataset, Data_joined$id, Data_joined$age, Data_joined$cond)))
# Data_joined_probe = Data_joined[Data_joined$type == 'probe',]
# length(unique(paste(Data_joined_probe$dataset, Data_joined_probe$id, Data_joined_probe$age, Data_joined_probe$cond, Data_joined_probe$id, Data_joined_probe$stim)))

# now we loop through the Data
set.seed(100)
metdat <- tibble()
roc_metdat <- tibble()
count <- 0
l_fig_real = list()
l_fig_sim = list()

for (i in dsets[order(nchar(dsets), dsets)]) {
    # i = "dataset 7" # filler
    # i = "dataset 5"
  dat_i <- filter(Data_joined, dataset == i) # select the current data set

  study_i <- dat_i$study[1]
  cat("------------ started ", i, ": ", study_i, fill = T)

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
  sd_i <- sd(p_i_guilty) #* 0.5077 + 7.1245
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
          sd_i
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

# library("ggpubr")
# fig_lists = c(l_fig_real, l_fig_sim)
# bigfig = ggpubr::ggarrange(plotlist = fig_lists, common.legend = TRUE, ncol = 11, nrow = 2)
# annotate_figure(
#     bigfig,
#     top = text_grob("Density plots per each study", face = "bold", size = 14),
#     bottom = text_grob("probe-irrelevant differences (ms)"),
#     left = text_grob("density estimate", rot = 90)
# )


## --- Standard Deviations
# unique(sd_metdat$version): p_vs_i p_vs_i_scaled_items d_cit_pooled

stat_dat = sd_metdat[sd_metdat$version == "p_vs_i",]

mean(stat_dat$m_g)
mean(stat_dat$sd_g)
mean_ci(stat_dat$sd_g, distance_only = F)
g_sd_max = max(stat_dat$sd_g)
g_sd_min = min(stat_dat$sd_g)
#mean(stat_dat$m_i)
mean(stat_dat$sd_i)
mean_ci(stat_dat$sd_i, distance_only = F)
i_sd_max = max(stat_dat$sd_i)
i_sd_min = min(stat_dat$sd_i)

# simulated standardized probe RT's SD mean: 0.894
# t.test(stat_dat$sd_i, mu = 0.894)

#stat_dat = stat_dat[stat_dat$dataset != 9, ]
corr_neat(stat_dat$sd_g, stat_dat$sd_i)
t_neat(stat_dat$sd_g, stat_dat$sd_i, pair = T)
t.test(stat_dat$sd_g, stat_dat$sd_i, paired = T)
weights::wtd.cor(stat_dat$sd_g, stat_dat$sd_i, weight = (stat_dat$n_g+stat_dat$n_i))

model = lm(sd_i~sd_g, data = stat_dat)
summary(model)
model = lm(sd_i~sd_g, data = stat_dat, weights = (stat_dat$n_g+stat_dat$n_i))
summary(model)

stat_dat$sd_sim2 = stat_dat$sd_g * model$coefficients[2] + model$coefficients[1]
# weighted: 0.5077 + 7.1245
# unweighed: 0.4418+8.3898
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

    for (stud in unique(met_thres$study)) {
        thres_orig = met_thres$thresholds[met_thres$study == stud]
        thres_mean = mean(met_thres$thresholds[met_thres$study != stud])
        thres_median = median(met_thres$thresholds[met_thres$study != stud])
        preds_stud = preds_metdat[preds_metdat$study == stud, ]
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
            study = stud,
            version = pred_type,
            acc_orig = acc,
            acc_cv_mean = acc_mean,
            acc_cv_med = acc_med,
            TPs_orig = tnr,
            TNs_orig = tpr,
            TPs_cv_mean = tnr_mean,
            TNs_cv_mean = tpr_mean,
            TPs_cv_med = tnr_med,
            TNs_cv_med = tpr_med
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
        "study",
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
    idvar = "study",
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
        orig_vs_cv = c('_orig', '_mean'),
        acc_type = c('TPs_', 'TNs_'),
        pred_type = c('p_vs_i_basic', 'd_cit_pooled', 'p_vs_i_scaled_items')
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
        orig_vs_cv = c('_orig', '_med'),
        acc_type = c('TPs_', 'TNs_'),
        pred_type = c('p_vs_i_basic', 'd_cit_pooled', 'p_vs_i_scaled_items')
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
    ), eb_method = sd, type = "bar", panel = 'acc_type'
)
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
        "study",
        'version',
        'aucs',
        'auc_lower',
        'auc_upper'
    )]),
    idvar = "study",
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

t_neat(metdat$aucs[metdat$version == 'p_vs_i'],
       metdat$aucs[metdat$version == 'simulated'],
       pair = T,
       round_descr = 3)
t.test(metdat$aucs[metdat$version == 'p_vs_i'],
       metdat$aucs[metdat$version == 'simulated'], paired = T)

corr_neat(metdat$aucs[metdat$version == "p_vs_i"], metdat$aucs[metdat$version == "simulated"])

weights::wtd.cor(metdat$aucs[metdat$version == "p_vs_i"], metdat$aucs[metdat$version == "simulated"], weight = (sd_metdat$n_g[sd_metdat$version == "p_vs_i"]+sd_metdat$n_i[sd_metdat$version == "p_vs_i"]))


## BF META

#install.packages('metaBMA')

bayes_model = metaBMA::meta_random(
    y = cohens_d~version+multiple_single+crowdsourced,
    SE = sed,
    labels = study,
    data = met_stat
)

bayes_model$stanfit_dstudy
plot_posterior

bayes_model_without = metaBMA::meta_random(
    y = cohens_d~multiple_single+crowdsourced,
    SE = sed,
    labels = study,
    data = met_stat
)
bayes_model_without$BF

metaBMA::plot_forest(bayes_model)

metaBMA::inclusion(list(bayes_model, bayes_model_without))

bayes_model$posterior_models


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


