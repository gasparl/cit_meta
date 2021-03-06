

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

# Set working directory
setwd(path_neat())

# set the functions
source("2019_meta_functions.R")

add_figs = FALSE # no figs in this script, no use for it


########################
#  Read in the data    #
#                      #
########################

setwd(path_neat("data/original_predictors"))

Data_VKT <- read_tsv("endfile_VerschuereKleinbergTheocharidou2015_JARMAC_Exp2.txt") # Verschuere, Kleinberg, & Theocharidou (2015) SP/MP https://osf.io/kgum2/
Data_VKT = Data_VKT[Data_VKT$ER_all_pro < 0.5 &
                        Data_VKT$ER_all_irr < 0.5 & Data_VKT$ER_all_tar < 0.5, ]

Data_KV1 <- read_tsv("endfile_KleinbergVerschuere2015_PLOSONE_Exp1.txt") # Kleinberg & Verschuere (2015) Study 1 https://osf.io/5htyr/
Data_KV1 = Data_KV1[Data_KV1$ER_all_pro < 0.5 &
                        Data_KV1$ER_all_irr < 0.5 & Data_KV1$ER_all_tar < 0.5, ]

Data_KV2 <- read_tsv("endfile_KleinbergVerschuere2015_PLOSONE_Exp2.txt") # Kleinberg & Verschuere (2015) Study 2 https://osf.io/5htyr/
Data_KV2 = Data_KV2[Data_KV2$ER_all_pro < 0.5 &
                        Data_KV2$ER_all_irr < 0.5 & Data_KV2$ER_all_tar < 0.5, ]

Data_VK <-  read_tsv("endfile_means_online-id-check.txt") # Verschuere & Kleinberg (2015) https://osf.io/cg5es/
Data_VK = Data_VK[Data_VK$ER_all_pro < 0.5 &
                      Data_VK$ER_all_irr < 0.5 & Data_VK$ER_all_tar < 0.5, ]

Data_LKV <- read_tsv("Experiment1_end_data_all.txt") # Luk�cs, Kleinberg, & Verschuere (2017) MP/SP/SPF https://osf.io/kv65n/
Data_LKV = Data_LKV[Data_LKV$ProbeAccuracy > 0.5 &
                        Data_LKV$IrrelevantAccuracy > 0.5 &
                        Data_LKV$TargetAccuracy > 0.5 &
                        Data_LKV$AccuracyAll > 0.5 ,]

Data_NV <- read_sav("End File Noordraven and Verschuere 2013_Forsharing.sav") # Noordraven & Verschuere (2013)
Data_NV = Data_NV[Data_NV$ERR_Crime_Irrel < 0.5 &
                      Data_NV$ERR_Crime_Probe < 0.5 & Data_NV$ERR_Crime_Target < 0.5, ]

Data_KV21 <- read_tsv("collapsed_endfile_Exp1.txt") # Kleinberg & Verschuere (2016) Study 1 https://osf.io/c7w5v/
Data_KV21 = Data_KV21[Data_KV21$ER_all_pro < 0.5 &
                          Data_KV21$ER_all_irr < 0.5 & Data_KV21$ER_all_tar < 0.5, ]
Data_KV22 <- read_tsv("collapsed_endfile_Exp2.txt") # Kleinberg & Verschuere (2016) Study 2 https://osf.io/c7w5v/
Data_KV22 = Data_KV22[Data_KV22$ER_all_pro < 0.5 &
                          Data_KV22$ER_all_irr < 0.5 & Data_KV22$ER_all_tar < 0.5, ]

Data_HU <- read_tsv("hu_et_al_2013.txt")

# Data_GBKV <- read_sav("Inquisit_Final.sav") # no conditions for this one


# all types error < 50% (50% or more error excluded)

########################
#  Unify &             #
#  combine data        #
#                      #
########################

#this one has multiple and sinle probes
# We want to seperate the files one for single and one for multiple probes
Data_VKT = data.frame(p_vs_i = Data_VKT$RT_all_pro - Data_VKT$RT_all_irr,
                      cond_base = Data_VKT$cond)
Data_VKT <- Data_VKT %>%
    mutate(
        multiple_single = ifelse(cond_base < 2, 1, 0),
        # 1 = multiple, 0 = single
        cond = ifelse(is.even(cond_base), 1, 0),
        # here we set 0 & 2 to guilty (=1) and 1 & 3 to innocent (=0),
        cond_base = NULL,
        study = ifelse(
            multiple_single == 0,
            "Verschuere, Kleinberg, & Theocharidou (2015) SP",
            "Verschuere, Kleinberg, & Theocharidou (2015) MP"
        ),
        dataset = ifelse(multiple_single == 0, "dataset 11", "dataset 10")
    )

Data_KV1 = data.frame(p_vs_i = Data_KV1$RT_all_pro - Data_KV1$RT_all_irr,
                      cond_base = Data_KV1$cond)
Data_KV1 <- Data_KV1 %>%
    mutate(
        multiple_single = 1,
        # 1 = multiple, 0 = single
        cond = cond_base,
        cond_base = NULL,
        study = "Kleinberg & Verschuere (2015) Study 1",
        dataset = "dataset 1"
    )

Data_KV2 = data.frame(p_vs_i = Data_KV2$RT_all_pro - Data_KV2$RT_all_irr,
                      cond_base = Data_KV2$cond)
Data_KV2 <- Data_KV2 %>%
    mutate(
        multiple_single = 1,
        # 1 = multiple, 0 = single
        cond = cond_base,
        cond_base = NULL,
        study = "Kleinberg & Verschuere (2015) Study 2",
        dataset = "dataset 2"
    )

Data_VK = data.frame(
    p_vs_i = Data_VK$RT_all_pro - Data_VK$RT_all_irr,
    cond_base = Data_VK$cond
)
Data_VK <- Data_VK %>%
    mutate(multiple_single = 1,
           # 1 = multiple, 0 = single
           cond = cond_base,
           cond_base = NULL,
           study = "Verschuere & Kleinberg (2015)",
           dataset = "dataset 9")

Data_LKV = data.frame(
    p_vs_i = Data_LKV$RTmeanDif,
    cond_base = Data_LKV$condition
)
Data_LKV <- Data_LKV %>%
    mutate(
        multiple_single = ifelse(cond_base == 1 |
                                     cond_base == 4, 1, 0),
        # 1 = multiple, 0 = single
        multiple_single = ifelse(cond_base == 2 |
                                     cond_base == 5, 2, multiple_single),
        # 2 means inducer
        cond = ifelse(cond_base < 3, 1, 0),
        # here we set 0, 1, 2 to guilty (=1) and 3, 4, 5 to innocent (=0),
        cond_base = NULL,
        study = ifelse(
            multiple_single == 0,
            "Luk�cs, Kleinberg, & Verschuere (2017) SP",
            "Luk�cs, Kleinberg, & Verschuere (2017) MP"
        ),
        study = ifelse(
            multiple_single == 2,
            "Luk�cs, Kleinberg, & Verschuere (2017) SPF",
            study
        ),
        dataset = ifelse(multiple_single == 0, "dataset 6", "dataset 5"),
        dataset = ifelse(multiple_single == 2, "dataset 7", dataset)
    )


Data_NV = data.frame(
    p_vs_i = Data_NV$Probe_Irrel_RT_Difference_CRIME,
    cond_base = Data_NV$Condition_1guilt_2innocent
)
Data_NV <- Data_NV %>%
    mutate(
           multiple_single = 1, # 1 = multiple, 0 = single
           cond = ifelse(cond_base == 2, 0, 1),
           cond_base = NULL,
           study = "Noordraven & Verschuere (2013)",
           dataset = "dataset 8")


Data_KV21 = data.frame(
    p_vs_i = Data_KV21$RT_all_pro - Data_KV21$RT_all_irr,
    cond_base = Data_KV21$cond
)
Data_KV21 <- Data_KV21 %>%
    mutate(multiple_single = 1, # 1 = multiple, 0 = single
           cond = ifelse(cond_base == 1, 0, 1 ), # here we set 0 & 2 to guilty (=1) and 1 to innocent (=0),
           cond_base = NULL,
           study = "Kleinberg & Verschuere (2016) Study 1",
           dataset = "dataset 3")

Data_KV22 = data.frame(
    p_vs_i = Data_KV22$RT_all_pro - Data_KV22$RT_all_irr,
    cond_base = Data_KV22$cond
)
Data_KV22 <- Data_KV22 %>%
    mutate(multiple_single = 1, # 1 = multiple, 0 = single
           cond = ifelse(cond_base == 0, 0, 1 ), # here we set 1 & 2 & 3 to guilty (=1) and 0 to innocent (remains 0),
           cond_base = NULL,
           study = "Kleinberg & Verschuere (2016) Study 2",
           dataset = "dataset 4")

hu1_pvi_g = Data_HU$G_probe_standard - Data_HU$G_irrelevant_standard
hu1_pvi_i = Data_HU$I_probe_standard - Data_HU$I_irrelevant_standard
hu1_pvi_g = hu1_pvi_g[!is.na(hu1_pvi_g)]
hu1_pvi_i = hu1_pvi_i[!is.na(hu1_pvi_i)]

Data_HU1 = data.frame(p_vs_i = c(hu1_pvi_g, hu1_pvi_i),
                      cond_base = c(rep(1, length(hu1_pvi_g)), rep(0, length(hu1_pvi_i))))

Data_HU1 <- Data_HU1 %>%
    mutate(
        multiple_single = 1,
        # 1 = multiple, 0 = single
        cond = cond_base,
        # here we set 0 & 2 to guilty (=1) and 1 & 3 to innocent (=0),
        cond_base = NULL,
        study = "Hu et al. (2013) Standard",
        dataset = "dataset 12"
    )


hu2_pvi_g = Data_HU$G_probe_dot - Data_HU$G_irrelevant_dot
hu2_pvi_i = Data_HU$I_probe_dot - Data_HU$I_irrelevant_dot
hu2_pvi_g = hu2_pvi_g[!is.na(hu2_pvi_g)]
hu2_pvi_i = hu2_pvi_i[!is.na(hu2_pvi_i)]

Data_HU2 = data.frame(p_vs_i = c(hu2_pvi_g, hu2_pvi_i),
                      cond_base = c(rep(1, length(hu2_pvi_g)), rep(0, length(hu2_pvi_i))))

Data_HU2 <- Data_HU2 %>%
    mutate(
        multiple_single = 1,
        # 1 = multiple, 0 = single
        cond = cond_base,
        # here we set 0 & 2 to guilty (=1) and 1 & 3 to innocent (=0),
        cond_base = NULL,
        study = "Hu et al. (2013) Dot",
        dataset = "dataset 13"
    )


# join them all together
# joining unfortunately only takes 2 arguments but it has other benefits so its nicer this way

Data_joined <- full_join(Data_VKT, Data_KV1)
Data_joined <- full_join(Data_joined, Data_KV2)
Data_joined <- full_join(Data_joined, Data_VK)
Data_joined <- full_join(Data_joined, Data_LKV)
Data_joined <- full_join(Data_joined, Data_NV)
Data_joined <- full_join(Data_joined, Data_KV21)
Data_joined <- full_join(Data_joined, Data_KV22)
Data_joined <- full_join(Data_joined, Data_HU1)
Data_joined <- full_join(Data_joined, Data_HU2)
Data_joined = Data_joined[!is.na(Data_joined$p_vs_i),]
dsets = unique(Data_joined$dataset)

# now we loop through the Data
set.seed(100)
metdat <- tibble()
roc_metdat <- tibble()
count <- 0

for (i in dsets[order(nchar(dsets), dsets)]) {
    # i = "dataset 7" # filler
    # i = "dataset 1"
  dat_i <- filter(Data_joined, dataset == i) # select the current data set

  study_i <- dat_i$study[1]
  cat("------------ started ", i, ": ", study_i, fill = T)

  dat_i_prep = dat_i
  datnum = as.numeric(strsplit(i, split = " ", fixed = TRUE)[[1]][2])


  p_i_guilty = filter(dat_i,  cond == 1)$p_vs_i

  sd_met_dat_i = get_sd_info(dat_i_prep,
                             c('p_vs_i'),
                             study_i,
                             i)

  preds_met_dat_i = data.frame(
      study = study_i,
      dataset = i,
      p_vs_i = dat_i_prep$p_vs_i,
      cond = dat_i_prep$cond
  )

  sd_i <- sd(p_i_guilty)*0.5077 + 7.1245

  # Get the cohens d and stuff
  eff_data <-
      effectsize_data(
          seq(nrow(dat_i_prep)),
          dat_i_prep$p_vs_i,
          dat_i_prep$p_vs_i*0.1, # these are just dummy data
          dat_i_prep$p_vs_i*0.1, # so that i can use same function
          dat_i_prep$p_vs_i*0.1, # as in the full meta
          dat_i_prep$p_vs_i*0.1, # these can all be ignored
          dat_i_prep$p_vs_i*0.1, # in the final analysis
          dat_i_prep$cond,
          dat_i_prep$multiple_single,
          dat_i_prep$study,
          sd_i
      )

  met_dat_i = eff_data[[1]]
  met_dat_i$dataset = datnum
  # get ROC data

  roc_met_dat_i <-
      roc_data(
          seq(nrow(dat_i_prep)),
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

  } else {
      metdat <- met_dat_i
      roc_metdat <- roc_met_dat_i
      sd_metdat <- sd_met_dat_i
      preds_metdat = preds_met_dat_i
  }
  count = count + 1
  cat("finished", i, ": ", study_i, fill = T)
}

## META

met_stat = metdat[metdat$version %in% c("p_vs_i","simulated"),] # this is to easily change the examined variable

met_stat = met_stat[order(met_stat$study, met_stat$version),]

met_stat$multiple_single[met_stat$multiple_single == 'multiple'] = 'MP'
met_stat$multiple_single[met_stat$multiple_single == 'single'] = 'SP'
met_stat$multiple_single[met_stat$multiple_single == 'inducer'] = 'SPF'
met_stat$simulated = 'No'
met_stat$simulated[met_stat$version == 'simulated'] = 'Yes'

met_stat$crowdsourced = "Yes"
met_stat$crowdsourced[grepl( "Noordraven & Verschuere", met_stat$study )] = "No"
met_stat$crowdsourced[grepl( "Verschuere & Kleinberg (2015)", met_stat$study, fixed = T )] = "No"
met_stat$crowdsourced[grepl( "Hu et", met_stat$study, fixed = T )] = "No"

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

# t test

t_neat(metdat$aucs[metdat$version == 'p_vs_i'],
       metdat$aucs[metdat$version == 'simulated'],
       pair = T,
       round_descr = 3)
t.test(metdat$aucs[metdat$version == 'p_vs_i'],
       metdat$aucs[metdat$version == 'simulated'], paired = T)

corr_neat(metdat$aucs[metdat$version == "p_vs_i"], metdat$aucs[metdat$version == "simulated"])
