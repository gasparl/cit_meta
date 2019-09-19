

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


########################
#  Read in the data    #
#                      #
########################

setwd(path_neat("data"))

Data_VKT <- read_sav("Experiment2_Data_SPSSformat.sav")
Data_KV1 <- read_sav("MemoryDetection2_0_Study1_rawdata_SPSS.sav")
Data_KV2 <- read_sav("MemoryDetection2_0_Study2_rawdata_SPSS.sav")
Data_VK <-  read_sav("rawdata_online-id-check.sav")
Data_LKV <- read_tsv("Experiment1_data.txt")
Data_NV <- read_tsv("Oddball Ernst PP 1- 43 FINAL.txt")
Data_KV21 <- read_tsv("rawdata_Exp1.txt") # 10.1016j.jarmac.2015.11.004
Data_KV22 <- read_tsv("rawdata_Exp2.txt")


########################
#  Unify &             #
#  combine data        #
#                      #
########################

#this one has multiple and sinle probes
# We want to seperate the files one for single and one for multiple probes
Data_VKT <- Data_VKT %>%
            mutate(multiple_single = ifelse(cond <2, 1, 0), # 1 = multiple, 0 = single
                   cond = ifelse(is.even(cond), 1,0 ), # here we set 0 & 2 to guilty (=1) and 1 & 3 to innocent (=0),
                   study = "Verschuere, Kleinberg, & Theocharidou (2015)",
                   dataset = ifelse(multiple_single==0, "dataset 1", "dataset 2"))


Data_KV1 <- Data_KV1 %>%
            mutate(multiple_single = 1, # 1 = multiple, 0 = single
            study = "Kleinberg & Verschuere (2015) Study 1",
            dataset = "dataset 3",
            cohd = as.numeric(cohd)) #for some reason cohens d is a charachter here which leads to issues with joining

Data_KV2 <- Data_KV2 %>%
             mutate(multiple_single = 1, # 1 = multiple, 0 = single
             study = "Kleinberg & Verschuere (2015) Study 2",
             dataset = "dataset 4")

Data_VK <- Data_VK %>%
           mutate(multiple_single = 1, # 1 = multiple, 0 = single
           study = "Verschuere & Kleinberg (2015)",
           dataset = "dataset 5")

Data_LKV <- Data_LKV %>%
            mutate(multiple_single = ifelse(cond == 1 | cond == 4, 1, 0), # 1 = multiple, 0 = single
                   multiple_single = ifelse(cond == 2 | cond == 5, 2, multiple_single), # 2 means inducer
                   cond = ifelse(cond <3, 1, 0 ), # here we set 0, 1, 2 to guilty (=1) and 3, 4, 5 to innocent (=0),
                   study = "Lukács, Kleinberg, & Verschuere (2017)",
                   dataset = ifelse(multiple_single==0, "dataset 6", "dataset 7"),
                   dataset = ifelse(multiple_single==2,"dataset 8", dataset))

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
           dataset = "dataset 9",
           stim = stimulusitem1,
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
           study = "Kleinberg & Verschuere (2016) Study 1",
           dataset = "dataset 10")
Data_KV21 = Data_KV21[,required_cols]

Data_KV22$trial = as.numeric(seq(nrow(Data_KV22))) # has no trial column, so i create it artificially
Data_KV22 <- Data_KV22 %>%
    mutate(multiple_single = 1, # 1 = multiple, 0 = single
           cond = ifelse(cond == 0, 0, 1 ), # here we set 1 & 2 & 3 to guilty (=1) and 0 to innocent (remains 0), ,
           id = paste0(date, age, lang, cohd), # for some reason it has no ids; so i create one
           study = "Kleinberg & Verschuere (2016) Study 2",
           dataset = "dataset 11")
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

# now we loop through the Data
metdat <- tibble()
roc_metdat <- tibble()
study_prev <- "none"
sd_prev <- 0
count <- 0

for (i in unique(Data_joined$dataset)) {
    # i = "dataset 8" # filler
  dat_i <- filter(Data_joined, dataset == i) # select the current data set

  study_i <- dat_i$study[1]
  print("-----------------------")
  print(study_i)

  #prep the data
  dat_i_prep <- dat_prep(dat_i$id,dat_i$gender,dat_i$age,dat_i$stim,dat_i$trial,
            dat_i$cond,dat_i$rt,dat_i$corr,dat_i$type,dat_i$multiple_single,dat_i$study)


  ## NOTE: predictors listed below

  ## 0
  # conventional overall probe RT mean minus irrelevant RT mean
  # dat_i_prep$probe_irrelevant_diff

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
  # dat_i_prep$p_i_diff_perstim_scaled


  p_i_guilty = filter(dat_i_prep,  cond == 1)$probe_irrelevant_diff
  p_i_innocent = filter(dat_i_prep,  cond == 0)$probe_irrelevant_diff

  sd_met_dat_i = get_bf_info(p_i_guilty, p_i_innocent, study_i, i)
  #t_neat(p_i_guilty, p_i_innocent, plot_densities = T, )

  sd_i <- sd(p_i_guilty)

  # Get the cohens d and shit
  met_dat_i <-
      effectsize_data(
          dat_i_prep$id,
          dat_i_prep$probe_irrelevant_diff,
          dat_i_prep$cond,
          dat_i_prep$multiple_single,
          dat_i_prep$study,
          sd_i
      )

  # get ROC data

  roc_met_dat_i <-
      roc_data(
          dat_i_prep$id,
          dat_i_prep$probe_irrelevant_diff,
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
  } else
  {
      metdat <- met_dat_i
      roc_metdat <- roc_met_dat_i
      sd_metdat <- sd_met_dat_i
  }
  count = count + 1
}

stat_dat = sd_metdat[order(as.character(sd_metdat$study)),]
rownames(stat_dat) = seq(nrow(stat_dat))

#stat_dat = stat_dat[stat_dat$study_num != "dataset 9", ]
corr_neat(stat_dat$sd_g, stat_dat$sd_i)
t_neat(stat_dat$sd_g, stat_dat$sd_i, pair = T)
wtd.cor(stat_dat$sd_g, stat_dat$sd_i, weight = (stat_dat$n_g+stat_dat$n_i))

model = lm(sd_i~sd_g, data = stat_dat)
summary(model)
model = lm(sd_i~sd_g, data = stat_dat, weights = (stat_dat$n_g+stat_dat$n_i))
summary(model)

stat_dat$sd_sim2 = stat_dat$sd_g * 0.5077 + 7.1245 # unweighed: 0.4418+8.3898
stat_dat$sd_sim3 = stat_dat$sd_g

t_neat( stat_dat$sd_i, stat_dat$sd_sim2, pair = T )
t_neat( stat_dat$sd_i, stat_dat$sd_sim3, pair = T )

ggplot(stat_dat, aes(x = sd_g, y = sd_i, weight = (stat_dat$n_g+stat_dat$n_i) )) +
    theme_bw() +
    geom_point(aes(y = stat_dat$sd_sim3), shape = 3) +
    geom_smooth(
        method = lm,
        fullrange = TRUE,
        level = .95,
        color = "#444444",
        size = 0.5
    ) +
    geom_point(aes(y = stat_dat$sd_sim2), shape = 16, color = "#FFFFFF") +
    geom_text(label = rownames(stat_dat), color = "#000000") +
    ylim(0, 50) +
    xlim(0, 50)

met_stat = metdat # this is to easily change the examined variable
#met_stat = metdat_alt

met_stat$study = paste(met_stat$study, met_stat$multiple_single) # to separate studies

mean(met_stat$cohens_d[met_stat$simulated == "no"])
mean(met_stat$cohens_d[met_stat$simulated == "yes"])

met_stat$crowdsourced = "yes"
met_stat$crowdsourced[grepl( "Noordraven & Verschuere", met_stat$study )] = "no"
met_stat$crowdsourced[grepl( "Verschuere & Kleinberg (2015)", met_stat$study, fixed = T )] = "no"
met_stat$multiple_single[met_stat$multiple_single == "inducer"] = "multiple"

# since the - in cohens D just gives the direction we get rid of it cause normally you report cohens d not with + or - signs.
met_stat$cohens_d <-met_stat$cohens_d *-1

### fixed-effects model
REML1 <- rma(cohens_d, variance_d,data=met_stat, method="REML")
REML1
forest(REML1, slab = met_stat$study, mlab="Summary effect size", xlab = "Effect Size (Cohen's d)")  # plot it
# so still al lot of variance left (75%)

# Check for moderators
REML2 <-rma(cohens_d, variance_d,data=met_stat, method="REML", mods = ~ simulated + multiple_single + crowdsourced)
REML2# so in this case you can see that multiple or single probe influences the effect size but not simulated or no
forest(REML2, slab = met_stat$study,  xlab = "Effect Size (Cohen's d)")  #

# You can fit it again with just multiple single to show that this can alone explain all variance in effect sizes

# why not control for each study??

REML3 <-rma(cohens_d, variance_d,data=met_stat, method="REML", mods = ~ simulated )
REML3
forest(REML3, slab = met_stat$study,  xlab = "Effect Size (Cohen's d)")  #

## ROC
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


