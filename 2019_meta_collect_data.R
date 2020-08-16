

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


########################
#  Read in the data    #
#                      #
########################


##### the code below records the full original data processing code
##### to use the uploaded data, skip to the section "USING cit_meta_data_trial_level.Rda"

setwd(path_neat("data"))

Data_VKT <- read_sav("Experiment2_Data_SPSSformat.sav") # Verschuere, Kleinberg, & Theocharidou (2015) SP/MP https://osf.io/kgum2/
Data_KV1 <- read_sav("MemoryDetection2_0_Study1_rawdata_SPSS.sav") # Kleinberg & Verschuere (2015) Study 1 https://osf.io/5htyr/
Data_KV2 <- read_sav("MemoryDetection2_0_Study2_rawdata_SPSS.sav") # Kleinberg & Verschuere (2015) Study 2 https://osf.io/5htyr/
Data_VK <-  read_sav("rawdata_online-id-check.sav") # Verschuere & Kleinberg (2015) https://osf.io/cg5es/
Data_LKV <- read_tsv("Experiment1_data.txt") # Luk?cs, Kleinberg, & Verschuere (2017) MP/SP/SPF https://osf.io/kv65n/
Data_NV <- read_tsv("Oddball Ernst PP 1- 43 FINAL.txt") # Noordraven & Verschuere (2013)
Data_KV21 <- read_tsv("rawdata_Exp1.txt") # Kleinberg & Verschuere (2016) Study 1 https://osf.io/c7w5v/
Data_KV22 <- read_tsv("rawdata_Exp2.txt") # Kleinberg & Verschuere (2016) Study 2 https://osf.io/c7w5v/

# Geven, Ben-Shakhar, Kindt, & Verschuere (2018)
Data_GBKV1 <- read.xlsx("Geven_CheatingRT_1.xlsx", sheet = 1)
Data_GBKV2 <- read.xlsx("Geven_CheatingRT_2.xlsx", sheet = 1)
Data_GBKV3 <- read_tsv("Geven_CheatingRT_3.iqdat")
Data_GBKV4 <- read_tsv("Geven_CheatingRT_4.iqx")

Data_GBKVbase = rbind(Data_GBKV1, Data_GBKV2, Data_GBKV3, Data_GBKV4)
Data_GBKVbase = Data_GBKVbase[, c(
    'subject',
    'blocknum',
    'trialnum',
    'blockcode',
    'trialcode',
    'correct',
    'latency',
    'stimulusitem1'
)]

Data_GBKV = Data_GBKVbase[sapply(strsplit(Data_GBKVbase$stimulusitem1, " "), length) == 2 &
                              (!grepl('leerfase', Data_GBKVbase$blockcode, fixed = TRUE)) &
                              (!grepl('practice', Data_GBKVbase$blockcode, fixed = TRUE)),]


Data_GBKV_excl <- read_sav("Geven_Inquisit_Final.sav")
Data_GBKV_excl = Data_GBKV_excl[Data_GBKV_excl$Exclusions_CIT == 0 &
                                    Data_GBKV_excl$Exclusions_HEXACO == 0 &
                                    Data_GBKV_excl$Exclusions_News_CarrieFisher == 0 &
                                    Data_GBKV_excl$ExclusionGround == "" &
                                    Data_GBKV_excl$Condition %in% c(1,2,4), ]
Data_GBKV_excl$subject = substring(Data_GBKV_excl$ParticipantNumber, 2)
Data_GBKV_dems = Data_GBKV_excl[, c(
    'subject',
    'Age',
    'Gender',
    'Condition'
)]
Data_GBKV = Data_GBKV[Data_GBKV$subject %in% Data_GBKV_excl$subject,]
Data_GBKV = merge(Data_GBKV, Data_GBKV_dems, by = "subject")

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
        dataset = ifelse(multiple_single == 0, "dataset 12", "dataset 11")
    )


Data_KV1 <- Data_KV1 %>%
            mutate(multiple_single = 1, # 1 = multiple, 0 = single
            study = "Kleinberg & Verschuere (2015) Exp1",
            dataset = "dataset 2",
            cohd = as.numeric(cohd)) #for some reason cohens d is a charachter here which leads to issues with joining

Data_KV2 <- Data_KV2 %>%
             mutate(multiple_single = 1, # 1 = multiple, 0 = single
             study = "Kleinberg & Verschuere (2015) Exp2",
             dataset = "dataset 3")

Data_VK <- Data_VK %>%
           mutate(multiple_single = 1, # 1 = multiple, 0 = single
           stim = paste(cat, type, sep = "_"),
           study = "Verschuere & Kleinberg (2015)",
           dataset = "dataset 10")

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
        study = 'Luk?cs, Kleinberg, & Verschuere (2017) Exp1',
        dataset = ifelse(multiple_single == 0, "dataset 7", "dataset 6"),
        dataset = ifelse(multiple_single == 2, "dataset 8", dataset)
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
           dataset = "dataset 9",
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
           dataset = "dataset 4")
Data_KV21 = Data_KV21[,required_cols]

Data_KV22 <- Data_KV22 %>%
    mutate(multiple_single = 1, # 1 = multiple, 0 = single
           cond = ifelse(cond == 0, 0, 1 ), # here we set 1 & 2 & 3 to guilty (=1) and 0 to innocent (remains 0), ,
           id = paste0(date, age, lang, cohd), # for some reason it has no ids; so i create one
           study = "Kleinberg & Verschuere (2016) Exp2",
           dataset = "dataset 5")
Data_KV22 = Data_KV22[,required_cols]

Data_GBKV <- Data_GBKV %>%
    mutate(id = subject,
           date = NA,
           multiple_single = 1, # 1 = multiple, 0 = single
           cond = ifelse(Condition == 2, 0, 1 ), # here we set 1 & 4 to guilty (=1) and 2 to innocent
           rt = latency,
           type = trialcode,
           corr = correct,
           study = "Geven, Ben-Shakhar, Kindt, & Verschuere (2018)",
           dataset = "dataset 1",
           age = Age,
           gender = Gender,
           stim = stimulusitem1,
           trial = trialnum)
Data_GBKV = Data_GBKV[,required_cols]
Data_GBKV$type[Data_GBKV$type == "real_PROBE"] = "probe"
Data_GBKV$type[Data_GBKV$type == "real_TARGET"] = "target"
Data_GBKV$type[Data_GBKV$type == "real_IRRELEVANT"] = "irrelevant"

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
Data_joined <- full_join(Data_joined, select(Data_GBKV,-date))

#######################

# reorganize and save
# cit_meta_data_trial_level = Data_joined
# cit_meta_data_trial_level = cit_meta_data_trial_level[c('study', 'cond','multiple_single', 'id','block','dataset','trial','type','corr','rt','stim', 'isi','age','gender')]
# names(cit_meta_data_trial_level) = c('study', 'condition','multiple_single', 'subject_id','block','dataset','trial','type','correct','rt','stimulus', 'isi','age','gender')
#
# cit_meta_data_trial_level$subject_id = paste0(cit_meta_data_trial_level$subject_id, cit_meta_data_trial_level$dataset, cit_meta_data_trial_level$condition, cit_meta_data_trial_level$multiple_single, cit_meta_data_trial_level$age)
# anon <- function(x) {
#     rl <- rle(x)$lengths
#     ans<- paste0("id", rep(seq_along(rl), rl))
#     return(ans)
# }
# cit_meta_data_trial_level$subject_id <- anon(cit_meta_data_trial_level$subject_id)
# # unique(full_out$subject_id)
#
# save(cit_meta_data_trial_level, file="cit_meta_data_trial_level.Rda")


##### USING cit_meta_data_trial_level.Rda

setwd(path_neat('data'))
load("cit_meta_data_trial_level.Rda")
names(cit_meta_data_trial_level) = c('study', 'cond','multiple_single', 'id','block','dataset','trial','type','corr','rt','stim', 'isi','age','gender')
Data_joined = cit_meta_data_trial_level




