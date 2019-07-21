

########################
#  load the packages   #
#                      #
#                      #
########################

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
setwd(script_path())

# set the functions
source("functions_lukacs&specker.R")


########################
#  Read in the data    #
#                      #
########################

setwd(script_path("data"))

Data_VKT <- read_sav("Experiment2_Data_SPSSformat.sav") 
Data_KV1 <- read_sav("MemoryDetection2_0_Study1_rawdata_SPSS.sav")
Data_KV2 <- read_sav("MemoryDetection2_0_Study2_rawdata_SPSS.sav")
Data_VK <-  read_sav("rawdata_online-id-check.sav")
Data_LKV <- read_tsv("Experiment1_data.txt")
Data_NV <- read_tsv("Oddball Ernst PP 1- 43 FINAL.txt")

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
                   study = "Verschuere, B., Kleinberg, B., & Theocharidou, K. (2015)",
                   dataset = ifelse(multiple_single==0, "dataset 1", "dataset 2")) 


Data_KV1 <- Data_KV1 %>%
            mutate(multiple_single = 1, # 1 = multiple, 0 = single
            study = "Kleinberg, B. & Verschuere, B. (2015) Study 1",
            dataset = "dataset 3",
            cohd = as.numeric(cohd)) #for some reason cohens d is a charachter here which leads to issues with joining

Data_KV2 <- Data_KV2 %>%
             mutate(multiple_single = 1, # 1 = multiple, 0 = single
             study = "Kleinberg, B. & Verschuere, B. (2015) Study 2",
             dataset = "dataset 4")

Data_VK <- Data_VK %>%
           mutate(multiple_single = 1, # 1 = multiple, 0 = single
           study = "Verschuere, B. & Kleinberg, B. (2015)",
           dataset = "dataset 5")

Data_LKV <- Data_LKV %>%
            mutate(multiple_single = ifelse(cond == 1 | cond == 4, 1, 0), # 1 = multiple, 0 = single
                   multiple_single = ifelse(cond == 2 | cond == 5, 2, multiple_single), # 2 means inducer
                   cond = ifelse(cond <3, 1, 0 ), # here we set 0, 1, 2 to guilty (=1) and 3, 4, 5 to innocent (=0), 
                   study = "Lukács, G. Kleinberg, B., & Verschuere (2017)",
                   dataset = ifelse(multiple_single==0, "dataset 6", "dataset 7"),
                   dataset = ifelse(multiple_single==2,"dataset 8", dataset))
                  
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
           study = "Noordraven, E. & Verschuere, B. (2013)",
           dataset = "dataset 9")


# join them all together
# joining unfortunately only takes 2 arguments but it has other benefits so its nicer this way

Data_joined <- full_join(Data_VKT, Data_KV1)
Data_joined <- full_join(Data_joined, Data_KV2)
Data_joined <- full_join(Data_joined, Data_VK)
Data_joined <- full_join(Data_joined, select(Data_LKV,-date)) # the date column leads to an issue so we take it out because we dont need it

# now we loop through the Data 
metdat <- tibble()
roc_metdat <- tibble()
study_prev <- "none"
sd_prev <- 0
count <- 0

for (i in unique(Data_joined$dataset)) {

  dat_i <- filter(Data_joined, dataset == i) # select the current data set
  
  study_i <- dat_i$study[1]
  
  #prep the data 
  dat_i_prep <- dat_prep(dat_i$id,dat_i$gender,dat_i$age,dat_i$stim,dat_i$trial,
            dat_i$cond,dat_i$rt,dat_i$corr,dat_i$type,dat_i$multiple_single,dat_i$study)
  
  
  #If you take this out of the comments it does it with the wrong sd
  
  # if the current study is the same as the previous study then take the sd of the previous study, otherwise take the sd of the guilty group
  #sd_i <- ifelse(study_i == prev_study,sd_prev,sd(filter(dat_i_prep,  cond == 1)$probe_irrelevant_diff)) 
  #print(sd_i)
  
  p_i_guilty = filter(dat_i_prep,  cond == 1)$probe_irrelevant_diff
  p_i_innocent = filter(dat_i_prep,  cond == 0)$probe_irrelevant_diff
  
  sd_met_dat_i = get_bf_info(p_i_guilty, p_i_innocent, study_i, i)
  
  sd_i <- sd(p_i_guilty)
  
  # Get the cohens d and shit 
  met_dat_i <- effectsize_data(dat_i_prep$id,dat_i_prep$probe_irrelevant_diff,
                               dat_i_prep$cond,dat_i_prep$multiple_single,dat_i_prep$study,sd_i)
  
  roc_met_dat_i <- roc_data(dat_i_prep$id,dat_i_prep$probe_irrelevant_diff,
                               dat_i_prep$cond,dat_i_prep$multiple_single,dat_i_prep$study,sd_i)
  
  roc_met_dat_i$dataset <- i
  
  # this needs to be out of the comments to make the 
  #study_prev <- study_i
  #sd_prev <- sd_i
  
  
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

cor.test(sd_metdat$sd_g, sd_metdat$sd_i)
plot(sd_metdat$sd_g, sd_metdat$sd_i)
model = lm(sd_g~sd_i, data = sd_metdat)
summary(model)
model$coefficients

# since the - in cohens D just gives the direction we get rid of it cause normally you report cohens d not with + or - signs. 
metdat$cohens_d <-metdat$cohens_d *-1

### fixed-effects model
REML1 <- rma(cohens_d, variance_d,data=metdat, method="REML")
REML1
forest(REML1, slab = metdat$study, mlab="Summary effect size", xlab = "Effect Size (Cohen's d)")  # plot it
# so still al lot of variance left (75%)

# Check for moderators
REML2 <-rma(cohens_d, variance_d,data=metdat, method="REML", mods = ~ simulated + multiple_single)
REML2# so in this case you can see that multiple or single probe influences the effect size but not simulated or no
forest(REML2, slab = metdat$study,  xlab = "Effect Size (Cohen's d)")  #

# You can fit it again with just multiple single to show that this can alone explain all variance in effect sizes
