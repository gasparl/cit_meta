cohen_d_between = function(outcome, categ, for_table = T){
  e = t.test(outcome ~ categ)
  f = as.vector(e$statistic)
  df = as.vector(e$parameter)
  pvalue = e$p.value
  table_categ = as.data.frame(table(categ))
  table_categ$Freq[table_categ$Freq == 0] = NA
  table_categ = na.omit(table_categ)
  n1 = table_categ[1,2]
  n2 = table_categ[2,2]
  g = sqrt(1/n1 + 1/n2)
  d = f*g
  l = ci.smd(ncp = f, n.1 = n1, n.2 = n2, conf.level = .95)
  if (for_table == T) {
    out = paste("t(", ro(df,1), ")", " = ", ro(f), ", p = ", ro(pvalue, 3), ", dbetween = ", ro(d, 2), " [", ro(l$Lower.Conf.Limit.smd, 2), ", ", ro(l$Upper.Conf.Limit.smd, 2), "].", sep="")
  } else {
    out = paste("t(", ro(df,1), ")", " = ", ro(f), ", p = ", ro(pvalue, 3), ", dbetween = ", ro(d, 2), ", 95% CI [", ro(l$Lower.Conf.Limit.smd, 2), ", ", ro(l$Upper.Conf.Limit.smd, 2), "].", sep="")
  }
  v = (n1 + n2) / (n1*n2) + (d**2) / ( 2 *  (n1 + n2) )
  print(v)
  stats <- cbind(d,v,out) # so I changed it here so its also outputting the cohens D
  print(stats)
}

get_bf_info = function(dat_guilt, dat_innocent, study, study_num) {
    #t = t.test(dat_guilt, dat_innocent)$statistic
    n_g = length(dat_guilt)
    n_i = length(dat_innocent)
    sd_g = sd(dat_guilt)
    sd_i = sd(dat_innocent)
    m_g = mean(dat_guilt)
    m_i = mean(dat_innocent)
    myrow = data.frame(
        study = study,
        study_num = study_num,
        #t = t,
        sd_g = sd_g,
        sd_i = sd_i,
        n_g = n_g,
        n_i = n_i,
        m_g = m_g,
        m_i = m_i
    )
    return(myrow)
}

# test: get_bf_info(c(1,2,3,4,23,2),c(1,2,13,24,23,2), "stud", 88)

ro = function(value, round_to = 2) {
  return(format(round(value, round_to), nsmall = round_to))
}

sd_pooled = function(var1, var2) {
    n1 = length(var1)
    n2 = length(var2)
    nom = (n1 - 1) * (sd(var1) ** 2) + (n2 - 1) * (sd(var2) ** 2)
    sd_p = sqrt(nom / (n1 + n2 - 2))
    return(sd_p)
}


# this data takes the "raw" data and prepares it for analysis
dat_prep = function(id,
                    gender,
                    age,
                    stim,
                    trial,
                    cond,
                    rt,
                    corr,
                    type,
                    multiple_single,
                    study) {
    Data_raw <-
        tibble(
            id = id,
            gender = gender,
            age = age,
            stim = stim,
            trial = trial,
            cond = cond,
            rt = rt,
            corr = corr,
            type = type,
            multiple_single = multiple_single,
            study = study
        )
    Data_raw_test  <<- Data_raw
    # Data_raw  = Data_raw_test

    testx <- Data_raw %>%
        filter(rt > 150 &
                   corr != 9999) %>% # so it filters out too fast trials and invalid trials
        mutate (acc_type = ifelse(type %in% c("probe", "irrelevant"), "p_and_i", type)) %>% # I create a column that says if its a target or not cause I wanna use that in my accuracy calculations
        group_by(id, acc_type) %>% # this groups it by id and target that means that for each individual participant and for each level of target (target or no target) it will compute the following seperately, thus for  count_corr it calculates the nr of correct trials for all target trials and all non-target trials seperately for each participant
        mutate(
            count_corr = sum(corr == 1),
            # nr of trials that were correct
            count_incorr = sum (corr == 0),
            # nr of trials that were incorrect
            count_slow  = sum(rt > 800),
            # nr of trials that were too slow
            accuracy = count_corr / (count_corr + count_incorr + count_slow)
        ) %>% # accuracy
        ungroup() %>% # here I undo the grouping because in the next line I wanna start a new grouping

        group_by (id, stim) %>% # here I calculate means value per stimuli
        mutate(mean_stim = c(mean(rt), rep(NA, length(rt) - 1))) %>%
        ungroup() %>%
        group_by(id) %>%
        mutate(
            mean_probe_stim = mean(mean_stim[type == "probe"], na.rm = TRUE),
            mean_irr_stim = mean(mean_stim[type == "irrelevant"], na.rm = TRUE),
            sd_irr_stim = sd(mean_stim[type == "irrelevant"], na.rm = TRUE),
            d_cit_perstim = (mean_probe_stim-mean_irr_stim)/sd_irr_stim,
            mean_stim_scaled = scale(mean_stim),
            p_i_diff_perstim_scaled = mean(mean_stim_scaled[type == "probe"], na.rm = TRUE) - mean(mean_stim_scaled[type == "irrelevant"], na.rm = TRUE)
        ) %>%
        ungroup() %>%

        # here i'm just making the data more wide, cause I create new columns
        spread(key = acc_type, value = accuracy, sep = "_") %>% # so I split up accuracy in 2 columns

        select(-c(stim, type)) %>% # we drop the stim and type columns because we wanna average over it, and im dropping the charachter variables cause I'm throwing a mean over everything
        group_by(id) %>%
        mutate(
            rt_probe = mean(rt[type == "probe"]),
            rt_irr = mean(rt[type == "irrelevant"]),
            # SDs
            sd_irr_trials = sd(rt[type == "irrelevant"]),
            sd_pooled_trials = sd_pooled(rt[type == "probe"], rt[type == "irrelevant"]),
            # scaled RTs
            rt_scaled_trials = scale(rt),
            rt_probe_scaled = mean(rt_scaled_trials[type == "probe"]),
            rt_irr_scaled = mean(rt_scaled_trials[type == "irrelevant"])
        ) %>%

        summarise_all(funs(ifelse((is.numeric(.)), mean(., na.rm = TRUE), first(.)
        ))) %>% # aggregrate
            mutate(
                probe_irrelevant_diff = rt_probe - rt_irr,
                d_cit = (rt_probe - rt_irr) / sd_irr_trials,
                d_cit_pooled = (rt_probe - rt_irr) / sd_pooled_trials,
                p_i_diff_pertrial_scaled = rt_probe_scaled - rt_irr_scaled
            ) %>% # create the difference scores
        filter(
            acc_type_target > .50 &
                acc_type_p_and_i > .75
        ) %>% # kick people out who are not accurate enough
        {
            if ("acc_type_itarget" %in% names(.)) filter(., acc_type_itarget  > .50 & acc_type_nontarget  > .50) else .
        }
}


effectsize_data = function(id,probe_irrelevant_diff,cond,multiple_single,study,sd) {

  Data_Real <- tibble (id=id,probe_irrelevant_diff=probe_irrelevant_diff,cond=cond, multiple_single=multiple_single,study=study)

  multiple_single <- ifelse(Data_Real$multiple_single[1] == 1, "multiple", "single")
  multiple_single <- ifelse(Data_Real$multiple_single[1] == 2, "inducer", multiple_single)
  study <- Data_Real$study[1]


  ## We simulate a normal distribution with the same SD but with a mean of zero.
  # Then we simulate like what 100.000 observations?

  # Because of the way the cohens_d works I add them in a data frame with the guilty ones immediately
  # set.seed(100) # unnecessary; we'll always create perfect normal distributions
  Data_Sim <- full_join (tibble(cond = 0, probe_irrelevant_diff = bayestestR::distribution_normal(n=10000,  mean=0, sd=sd)),filter(Data_Real,cond == 1))  # multiple

  # compare the guilty with the innocent

  d1 <- cohen_d_between(Data_Real$probe_irrelevant_diff,Data_Real$cond) #real
  # compare the guilty with the stimulated
  d2 <- cohen_d_between(Data_Sim$probe_irrelevant_diff,Data_Sim$cond) # simulate

  output <- tibble(cohens_d = c (as.numeric(d1[1]), as.numeric(d2[1])),
                   variance_d = c(as.numeric(d1[2]), as.numeric(d2[2])),
                   simulated = c ("no", "yes"),
                   multiple_single = c (multiple_single,multiple_single),
                   study = c (study,study))

  return(output)
}


### ROc stimulation etc

roc_data = function(id,probe_irrelevant_diff,cond,multiple_single,study,sd) {

  Data_Real <- tibble (id=id,probe_irrelevant_diff=probe_irrelevant_diff,cond=cond, multiple_single=multiple_single,study=study)

  study <- Data_Real$study[1]
  multiple_single <- ifelse(Data_Real$multiple_single[1] == 1, "multiple", "single")


  # make the real roc
  r1 <- roc(Data_Real$cond,Data_Real$probe_irrelevant_diff)

  #save in a data frame
  r1_w <-  tibble( TP = r1$sensitivities,
                   FP = (1- r1$specificities),
                   FN = (1- r1$sensitivities),
                   TN = r1$specificities,
                   simulated = "no")

  #make simulated data
  #set.seed(100)
  Data_Sim <- full_join(tibble(cond = 0, probe_irrelevant_diff = bayestestR::distribution_normal(n=10000,  mean=0, sd=sd)), filter(Data_Real,cond == 1))

  # roc over simulated data
  r2 <- roc(Data_Sim$cond,Data_Sim$probe_irrelevant_diff)

  # save in a data frame
  r2_w <-  tibble( TP = r2$sensitivities,
                   FP = (1- r2$specificities),
                   FN = (1- r2$sensitivities),
                   TN = r2$specificities,
                   simulated = "yes")

  # join the data together
  roc_metdat <- full_join(r1_w,r2_w)

  # add study info
  roc_metdat$study <- study
  roc_metdat$multiple_single <- multiple_single

  return(roc_metdat)

}
