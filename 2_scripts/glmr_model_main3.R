
df_E_apl$day <- day(df_E_apl$COLLECTION_DATE)
df_E_apl$week <- week(df_E_apl$COLLECTION_DATE)
df_E_apl$month <- month(df_E_apl$COLLECTION_DATE)


start_time <- Sys.time()
fit_E_apl_m <- stan_glm(interp_roche_n ~ age_group + Sex + year_month,
                        family = binomial,
                        data = df_E_apl,
                        cores = 4,
                        seed = 1010, adapt_delta = 0.9)
end_time <- Sys.time()
end_time - start_time
print(fit_E_apl_m)
waic(fit_E_apl_m)

start_time <- Sys.time()
fit_E_apl <- stan_glm(interp_roche_n ~ age_group + Sex,
                        family = binomial,
                        data = df_E_apl,
                        cores = 4,
                        seed = 1010, adapt_delta = 0.9)
end_time <- Sys.time()
end_time - start_time
print(fit_E_apl)
waic(fit_E_apl)

start_time <- Sys.time()
fit_C_apl <- stan_glm(interp_roche_n ~ age_group + Sex,
                      family = binomial,
                      data = df_C_apl,
                      cores = 4,
                      seed = 1010, adapt_delta = 0.9)
end_time <- Sys.time()
end_time - start_time
print(fit_C_apl)
waic(fit_C_apl)
############################## Consistent Code Begins Here Setting G Alberta #####################
census_all <- census_all[census_all$age_group != "< 18 years", ]

poststrat_cbs <-
  census_all %>%
  group_by(regpro,age_group,Sex,Urban) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_abc <-
  census_all %>%
  group_by(regpro,age_group,Sex,Urban) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_apl <-
  census_all %>%
  group_by(regpro,age_group,Sex, Urban) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

df_G_abc$age_group <- age_groups_fun(df_G_abc$age)
df_G_cbs$age_group <- age_groups_fun(df_G_cbs$age)
df_G_apl$age_group <- age_groups_fun(df_G_apl$age)

df_G_abc <- df_G_abc[!is.na(df_G_abc$age_group),]
df_G_cbs <- df_G_cbs[!is.na(df_G_cbs$age_group),]
df_G_apl <- df_G_apl[!is.na(df_G_apl$age_group),]

poststrat_cbs$composite_strata <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, sep = "_")
poststrat_abc$composite_strata <- paste(poststrat_abc$age_group, poststrat_abc$Sex, sep = "_")
poststrat_apl$composite_strata <- paste(poststrat_apl$age_group, poststrat_apl$Sex, sep = "_")
df_G_abc$composite_strata <- paste(df_G_abc$age_group, df_G_abc$Sex, sep = "_")
df_G_cbs$composite_strata <- paste(df_G_cbs$age_group, df_G_cbs$Sex, sep = "_")
df_G_apl$composite_strata <- paste(df_G_apl$age_group, df_G_apl$Sex, sep = "_")

poststrat_cbs$composite_strata_1 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$Urban, sep = "_")
poststrat_abc$composite_strata_1 <- paste(poststrat_abc$age_group, poststrat_abc$Sex, poststrat_abc$Urban, sep = "_")
poststrat_apl$composite_strata_1 <- paste(poststrat_apl$age_group, poststrat_apl$Sex, poststrat_apl$Urban, sep = "_")
df_G_abc$composite_strata_1 <- paste(df_G_abc$age_group, df_G_abc$Sex, df_G_abc$Urban, sep = "_")
df_G_cbs$composite_strata_1 <- paste(df_G_cbs$age_group, df_G_cbs$Sex, df_G_cbs$Urban, sep = "_")
df_G_apl$composite_strata_1 <- paste(df_G_apl$age_group, df_G_apl$Sex, df_G_apl$Urban, sep = "_")

library(survey)
# By province/region
regpro_df <- data.frame(
  regpro = c("AB"), 
  mrp_G_cbs = NA,
  mrp_G_cbs_se = NA,
  mrp_G_abc = NA,
  mrp_G_abc_se = NA,
  mrp_G_apl = NA,
  mrp_G_apl_se = NA,
  ua_G_cbs = NA,
  ua_G_cbs_se = NA,
  ua_G_abc = NA,
  ua_G_abc_se = NA,
  ua_G_apl = NA,
  ua_G_apl_se = NA,
  rake_G_cbs = NA,
  rake_G_cbs_se = NA,
  rake_G_abc = NA,
  rake_G_abc_se = NA,
  rake_G_apl = NA,
  rake_G_apl_se = NA,
  post_G_cbs = NA,
  post_G_cbs_se = NA,
  post_G_abc = NA,
  post_G_abc_se = NA,
  post_G_apl = NA,
  post_G_apl_se = NA,
  n_sample = NA,
  n_full = NA
)

for(i in 1:nrow(regpro_df)) {
  
  # MRP estimate abc
  epred_mat <- posterior_epred(fit_G_abc, newdata = poststrat_abc, draws = 4000)
  filtering_condition <- which(poststrat_abc$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_abc[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_G_abc[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_G_abc_se[i] <- sd(mrp_estimates_vector)
  
  # MRP estimate cbs
  epred_mat <- posterior_epred(fit_G_cbs, newdata = poststrat_cbs, draws = 4000)
  filtering_condition <- which(poststrat_cbs$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_cbs[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_G_cbs[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_G_cbs_se[i] <- sd(mrp_estimates_vector)
  
  # MRP estimate apl
  epred_mat <- posterior_epred(fit_G_apl, newdata = poststrat_apl, draws = 4000)
  filtering_condition <- which(poststrat_abc$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_abc[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_G_apl[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_G_apl_se[i] <- sd(mrp_estimates_vector)
  
  # Unadjusted estimate abc
  regpro_df$ua_G_abc[i] <- mean(filter(df_G_abc, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_G_abc, regpro==regpro_df$regpro[i]))
  regpro_df$ua_G_abc_se[i] <- get_se_bernoulli(regpro_df$ua_G_abc[i], regpro_df$n_sample[i])
  
  # Unadjusted estimate cbs
  regpro_df$ua_G_cbs[i] <- mean(filter(df_G_cbs, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_G_cbs, regpro==regpro_df$regpro[i]))
  regpro_df$ua_G_cbs_se[i] <- get_se_bernoulli(regpro_df$ua_G_cbs[i], regpro_df$n_sample[i])
  
  # Unadjusted estimate apl
  regpro_df$ua_G_apl[i] <- mean(filter(df_G_apl, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_G_apl, regpro==regpro_df$regpro[i]))
  regpro_df$ua_G_apl_se[i] <- get_se_bernoulli(regpro_df$ua_G_apl[i], regpro_df$n_sample[i])
  
  # Raking on age/sex abc (RAKE)
  data <- df_G_abc[df_G_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_G_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_G_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex cbs (RAKE)
  data <- df_G_cbs[df_G_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_G_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_G_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex apl (RAKE)
  data <- df_G_apl[df_G_apl$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_apl[poststrat_apl$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_apl[poststrat_apl$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_G_apl[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_G_apl_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A abc (POST)
  data <- df_G_abc[df_G_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_G_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_G_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A cbs (POST)
  data <- df_G_cbs[df_G_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_G_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_G_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A apl (POST)
  data <- df_G_apl[df_G_apl$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_apl[poststrat_apl$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_apl[poststrat_apl$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_G_apl[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_G_apl_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
}

options(survey.lonely.psu="adjust")

write.csv(regpro_df, "/home/yuanyu/projects/covid-donor-v-survey-antibody/4_output/regpro_df_G.csv")

# reload the dataframe in long format
regpro_df_G <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_G_new.csv')

desired_order <- c("UA","RAKE","POST","MRP")
#desired_order1 <- c("BC","AB","MB","SK","ON","ATL")
desired_order2 <- c("CBS","ABC","APL")

# Reorder the levels of the 'category' variable
regpro_df_G$model <- factor(regpro_df_G$model, levels = desired_order)
#regpro_df_C_vac$regpro <- factor(regpro_df_C_vac$regpro, levels = desired_order1)
regpro_df_G$source <- factor(regpro_df_G$source, levels = desired_order2)

compare_G <- ggplot(regpro_df_G, aes(x = regpro, y = mrp_mean, colour=source, shape = model)) +
  theme_bw()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 2) +
  scale_color_manual(values = c("#F8766D", "#00BFC4","#619CFF","#00BA38","#F564E3")) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Seropositivity") +
  xlab("Province/Region") +
  ggtitle("Setting G (Jan- Apr 22), CBS vs AbC vs APL, Alberta") +
  theme(plot.title = element_text(vjust = 0))
compare_G


##### [Apparent Prevalence + (Specificity − 1)]/[Specificity + (Sensitivity − 1)]
(x+0.998-1)/(0.995+0.998-1) #-0.002 /0.993
(x+0.99-1)/(0.92+0.99-1) #-0.01 /0.91


#####
start_time <- Sys.time()
fit_E_apl <- stan_glm(interp_roche_n ~ age_group + Sex + Urban,
                      family = binomial,
                      data = df_E_apl,
                      cores = 4,
                      seed = 1010, adapt_delta = 0.8)
end_time <- Sys.time()
end_time - start_time
print(fit_E_apl)

start_time <- Sys.time()
fit_E_cbs_glm <- stan_glm(interp_roche_n ~ age_group + Sex + Urban,
                      family = binomial,
                      data = df_E_cbs,
                      cores = 4,
                      seed = 1010, adapt_delta = 0.8)
end_time <- Sys.time()
end_time - start_time
print(fit_E_cbs_glm)

############################## Consistent Code Begins Here Setting E Alberta #####################
census_all <- census_all[census_all$age_group != "< 18 years", ]
poststrat_cbs <-
  census_all %>%
  group_by(regpro,age_group,Sex,Urban) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_apl <-
  census_all %>%
  group_by(regpro,age_group,Sex, Urban) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

df_E_abc$age_group <- age_groups_fun(df_E_abc$age)
df_E_cbs$age_group <- age_groups_fun(df_E_cbs$age)
df_E_apl$age_group <- age_groups_fun(df_E_apl$age)

df_E_abc <- df_E_abc[!is.na(df_E_abc$age_group),]
df_E_cbs <- df_E_cbs[!is.na(df_E_cbs$age_group),]
df_E_apl <- df_E_apl[!is.na(df_E_apl$age_group),]

poststrat_cbs$composite_strata <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, sep = "_")
poststrat_abc$composite_strata <- paste(poststrat_abc$age_group, poststrat_abc$Sex, sep = "_")
poststrat_apl$composite_strata <- paste(poststrat_apl$age_group, poststrat_apl$Sex, sep = "_")
df_E_abc$composite_strata <- paste(df_E_abc$age_group, df_E_abc$Sex, sep = "_")
df_E_cbs$composite_strata <- paste(df_E_cbs$age_group, df_E_cbs$Sex, sep = "_")
df_E_apl$composite_strata <- paste(df_E_apl$age_group, df_E_apl$Sex, sep = "_")

poststrat_cbs$composite_strata_1 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$Urban, sep = "_")
poststrat_abc$composite_strata_1 <- paste(poststrat_abc$age_group, poststrat_abc$Sex, poststrat_abc$Urban, sep = "_")
poststrat_apl$composite_strata_1 <- paste(poststrat_apl$age_group, poststrat_apl$Sex, poststrat_apl$Urban, sep = "_")
df_E_abc$composite_strata_1 <- paste(df_E_abc$age_group, df_E_abc$Sex, df_E_abc$Urban, sep = "_")
df_E_cbs$composite_strata_1 <- paste(df_E_cbs$age_group, df_E_cbs$Sex, df_E_cbs$Urban, sep = "_")
df_E_apl$composite_strata_1 <- paste(df_E_apl$age_group, df_E_apl$Sex, df_E_apl$Urban, sep = "_")

library(survey)
# By province/region
regpro_df <- data.frame(
  regpro = c("AB"), 
  mrp_E_apl = NA,
  mrp_E_apl_se = NA,
  ua_E_apl = NA,
  ua_E_apl_se = NA,
  rake_E_apl = NA,
  rake_E_apl_se = NA,
  rake1_E_apl = NA,
  rake1_E_apl_se = NA,
  post_E_apl = NA,
  post_E_apl_se = NA,
  n_sample = NA,
  n_full = NA
)

for(i in 1:nrow(regpro_df)) {
  
  # MRP estimate apl
  epred_mat <- posterior_epred(fit_E_apl, newdata = poststrat_apl, draws = 4000)
  filtering_condition <- which(poststrat_abc$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_abc[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_E_apl[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_E_apl_se[i] <- sd(mrp_estimates_vector)

  # Unadjusted estimate apl
  regpro_df$ua_E_apl[i] <- mean(filter(df_E_apl, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_E_apl, regpro==regpro_df$regpro[i]))
  regpro_df$ua_E_apl_se[i] <- get_se_bernoulli(regpro_df$ua_E_apl[i], regpro_df$n_sample[i])

  # Raking on age/sex apl (RAKE)
  data <- df_E_apl[df_E_apl$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_apl[poststrat_apl$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_apl[poststrat_apl$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_E_apl[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_E_apl_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A apl (POST)
  data <- df_E_apl[df_E_apl$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_apl[poststrat_apl$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_apl[poststrat_apl$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_E_apl[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_E_apl_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
}

options(survey.lonely.psu="adjust")

write.csv(regpro_df, "/home/yuanyu/projects/covid-donor-v-survey-antibody/4_output/regpro_df_E_new.csv")

# reload the dataframe in long format
regpro_df_E <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_E_new.csv')

desired_order <- c("UA","RAKE","POST","MRP")
#desired_order1 <- c("BC","AB","MB","SK","ON","ATL")
desired_order2 <- c("CBS","ABC","APL")

# Reorder the levels of the 'category' variable
regpro_df_E$model <- factor(regpro_df_E$model, levels = desired_order)
#regpro_df_C_vac$regpro <- factor(regpro_df_C_vac$regpro, levels = desired_order1)
regpro_df_E$source <- factor(regpro_df_E$source, levels = desired_order2)

compare_E <- ggplot(regpro_df_E, aes(x = regpro, y = mrp_mean, colour=source, shape = model)) +
  theme_bw()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 2) +
  scale_color_manual(values = c("#F8766D", "#00BFC4","#619CFF","#00BA38","#F564E3")) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Seropositivity") +
  xlab("Province/Region") +
  ggtitle("Setting G (Jan- Apr 22), CBS vs AbC vs APL, Alberta") +
  theme(plot.title = element_text(vjust = 0))
compare_E

regpro_df_E[regpro_df_E$model=="MRP" & regpro_df_E$source=="CBS",]$mrp_mean -
regpro_df_E[regpro_df_E$model=="MRP" & regpro_df_E$source=="CCAHS",]$mrp_mean

regpro_df_E[regpro_df_E$model=="NO_Adjust" & regpro_df_E$source=="CBS",]$mrp_mean -
regpro_df_E[regpro_df_E$model=="NO_Adjust" & regpro_df_E$source=="CCAHS",]$mrp_mean

############################## Consistent Code Begins Here Setting A & B CanPath #####################
census_all <- census_all[census_all$age_group != "< 18 years", ]

start_time <- Sys.time()
fit_A_cbs <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race,
                        family = binomial,
                        data = df_A_cbs,
                        cores = 4,
                        seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_A_cbs)

df_A_canpaths$Sex <- df_A_canpaths$sex
df_A_canpaths$ethnic1 <- df_A_canpaths$race
df_A_canpaths <- df_A_canpaths[,-17]
start_time <- Sys.time()
fit_A_canpaths <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race,
                        family = binomial,
                        data = df_A_canpaths,
                        cores = 4,
                        seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_A_canpaths)


poststrat_cbs <-
  census_all %>%
  group_by(regpro,age_group,Sex,Race) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_abc <-
  census_all %>%
  group_by(regpro,age_group,Sex,Race) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_canpaths <-
  census_all %>%
  group_by(regpro,age_group,Sex, Race) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_cbs[poststrat_cbs$regpro == 'Atlantic',]$regpro <- 'ATL'
poststrat_abc[poststrat_abc$regpro == 'Atlantic',]$regpro <- 'ATL'
poststrat_canpaths[poststrat_canpaths$regpro == 'Atlantic',]$regpro <- 'ATL'

df_A_abc$age_group <- age_groups_fun(df_A_abc$age)
df_A_cbs$age_group <- age_groups_fun(df_A_cbs$age)
df_A_canpaths$age_group <- age_groups_fun(df_A_canpaths$age)

colnames(poststrat_canpaths)[4] <- "Race1"

df_A_canpaths$age_group <- droplevels(df_A_canpaths$age_group)
df_B_canpaths$age_group <- droplevels(df_B_canpaths$age_group)

df_A_abc <- df_A_abc[!is.na(df_A_abc$age_group),]
df_A_cbs <- df_A_cbs[!is.na(df_A_cbs$age_group),]
df_A_canpaths <- df_A_canpaths[!is.na(df_A_canpaths$age_group),]
df_A_canpaths <- df_A_canpaths[!is.na(df_A_canpaths$Race),]

df_A_canpaths <- fit_A_canpaths$data
df_A_canpaths$Race <- as.numeric(as.character(df_A_canpaths$Race))
poststrat_canpaths <- poststrat_canpaths[poststrat_canpaths$age_group != "18-26 years", ]

poststrat_cbs$composite_strata <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, sep = "_")
poststrat_abc$composite_strata <- paste(poststrat_abc$age_group, poststrat_abc$Sex, sep = "_")
poststrat_canpaths$composite_strata <- paste(poststrat_canpaths$age_group, poststrat_canpaths$Sex, sep = "_")
df_A_abc$composite_strata <- paste(df_A_abc$age_group, df_A_abc$Sex, sep = "_")
df_A_cbs$composite_strata <- paste(df_A_cbs$age_group, df_A_cbs$Sex, sep = "_")
df_A_canpaths$composite_strata <- paste(df_A_canpaths$age_group, df_A_canpaths$Sex, sep = "_")

poststrat_cbs$composite_strata_1 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$Race, sep = "_")
poststrat_abc$composite_strata_1 <- paste(poststrat_abc$age_group, poststrat_abc$Sex, poststrat_abc$Race, sep = "_")
poststrat_canpaths$composite_strata_1 <- paste(poststrat_canpaths$age_group, poststrat_canpaths$Sex, poststrat_canpaths$Race, sep = "_")
df_A_abc$composite_strata_1 <- paste(df_A_abc$age_group, df_A_abc$Sex, df_A_abc$Race, sep = "_")
df_A_cbs$composite_strata_1 <- paste(df_A_cbs$age_group, df_A_cbs$Sex, df_A_cbs$Race, sep = "_")
df_A_canpaths$composite_strata_1 <- paste(df_A_canpaths$age_group, df_A_canpaths$Sex, df_A_canpaths$Race, sep = "_")

library(survey)
# By province/region
regpro_df <- data.frame(
  regpro = c("BC","AB","ON", "ATL"), 
  mrp_A_cbs = NA,
  mrp_A_cbs_se = NA,
  mrp_A_abc = NA,
  mrp_A_abc_se = NA,
  mrp_A_canpaths = NA,
  mrp_A_canpaths_se = NA,
  ua_A_cbs = NA,
  ua_A_cbs_se = NA,
  ua_A_abc = NA,
  ua_A_abc_se = NA,
  ua_A_canpaths = NA,
  ua_A_canpaths_se = NA,
  rake_A_cbs = NA,
  rake_A_cbs_se = NA,
  rake_A_abc = NA,
  rake_A_abc_se = NA,
  rake_A_canpaths = NA,
  rake_A_canpaths_se = NA,
  post_A_cbs = NA,
  post_A_cbs_se = NA,
  post_A_abc = NA,
  post_A_abc_se = NA,
  post_A_canpaths = NA,
  post_A_canpaths_se = NA,
  n_sample = NA,
  n_full = NA
)

for(i in 1:nrow(regpro_df)) {
  
  # MRP estimate abc
  epred_mat <- posterior_epred(fit_A_abc, newdata = poststrat_abc, draws = 4000)
  filtering_condition <- which(poststrat_abc$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_abc[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_A_abc[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_A_abc_se[i] <- sd(mrp_estimates_vector)
  
  # MRP estimate cbs
  epred_mat <- posterior_epred(fit_A_cbs, newdata = poststrat_cbs, draws = 4000)
  filtering_condition <- which(poststrat_cbs$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_cbs[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_A_cbs[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_A_cbs_se[i] <- sd(mrp_estimates_vector)
  
  # MRP estimate apl
  epred_mat <- posterior_epred(fit_A_canpaths, newdata = poststrat_canpaths, draws = 4000)
  filtering_condition <- which(poststrat_canpaths$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_abc[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_A_canpaths[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_A_canpaths_se[i] <- sd(mrp_estimates_vector)
  
  # Unadjusted estimate abc
  regpro_df$ua_A_abc[i] <- mean(filter(df_A_abc, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_A_abc, regpro==regpro_df$regpro[i]))
  regpro_df$ua_A_abc_se[i] <- get_se_bernoulli(regpro_df$ua_A_abc[i], regpro_df$n_sample[i])
  
  # Unadjusted estimate cbs
  regpro_df$ua_A_cbs[i] <- mean(filter(df_A_cbs, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_A_cbs, regpro==regpro_df$regpro[i]))
  regpro_df$ua_A_cbs_se[i] <- get_se_bernoulli(regpro_df$ua_A_cbs[i], regpro_df$n_sample[i])
  
  # Unadjusted estimate apl
  regpro_df$ua_A_canpaths[i] <- mean(filter(df_A_canpaths, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_A_canpaths, regpro==regpro_df$regpro[i]))
  regpro_df$ua_A_canpaths_se[i] <- get_se_bernoulli(regpro_df$ua_A_canpaths[i], regpro_df$n_sample[i])
  
  # # Raking on age/sex abc (RAKE)
  # data <- df_A_abc[df_A_abc$regpro==regpro_df$regpro[i],]
  # data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  # poststrat.dist <- data.frame(composite_strata = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata,
  #                              Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  # 
  # data.svy.rake <- rake(design = data.svy.unweighted,
  #                       sample.margins = list(~composite_strata),
  #                       population.margins = list(poststrat.dist))
  # 
  # regpro_df$rake_A_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  # regpro_df$rake_A_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  # 
  # # Raking on age/sex cbs (RAKE)
  # data <- df_A_cbs[df_A_cbs$regpro==regpro_df$regpro[i],]
  # data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  # poststrat.dist <- data.frame(composite_strata = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata,
  #                              Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  # 
  # data.svy.rake <- rake(design = data.svy.unweighted,
  #                       sample.margins = list(~composite_strata),
  #                       population.margins = list(poststrat.dist))
  # 
  # regpro_df$rake_A_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  # regpro_df$rake_A_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  # 
  # # Raking on age/sex apl (RAKE)
  # data <- df_A_canpaths[df_A_canpaths$regpro==regpro_df$regpro[i],]
  # data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  # poststrat.dist <- data.frame(composite_strata = poststrat_canpaths[poststrat_canpaths$regpro==regpro_df$regpro[i],]$composite_strata,
  #                              Freq = poststrat_canpaths[poststrat_canpaths$regpro==regpro_df$regpro[i],]$n)
  # 
  # data.svy.rake <- rake(design = data.svy.unweighted,
  #                       sample.margins = list(~composite_strata),
  #                       population.margins = list(poststrat.dist))
  # 
  # regpro_df$rake_A_canpaths[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  # regpro_df$rake_A_canpaths_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A abc (POST with composite_strata )
  data <- df_A_abc[df_A_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$rake_A_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_A_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A cbs (POST)
  data <- df_A_cbs[df_A_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$rake_A_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_A_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A apl (POST)
  data <- df_A_canpaths[df_A_canpaths$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_canpaths[poststrat_canpaths$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_canpaths[poststrat_canpaths$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$rake_A_canpaths[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_A_canpaths_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A abc (POST)
  data <- df_A_abc[df_A_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_A_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_A_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A cbs (POST)
  data <- df_A_cbs[df_A_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_A_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_A_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A apl (POST)
  data <- df_A_canpaths[df_A_canpaths$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_canpaths[poststrat_canpaths$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_canpaths[poststrat_canpaths$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_A_canpaths[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_A_canpaths_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
}

options(survey.lonely.psu="adjust")

write.csv(regpro_df, "/home/yuanyu/projects/covid-donor-v-survey-antibody/4_output/regpro_df_A_new.csv")

# reload the dataframe in long format
regpro_df_A <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_A_new.csv')

desired_order <- c("UA","RAKE","POST","MRP")
#desired_order1 <- c("BC","AB","MB","SK","ON","ATL")
desired_order2 <- c("CBS","ABC","APL")

# Reorder the levels of the 'category' variable
regpro_df_A$model <- factor(regpro_df_A$model, levels = desired_order)
#regpro_df_C_vac$regpro <- factor(regpro_df_C_vac$regpro, levels = desired_order1)
regpro_df_A$source <- factor(regpro_df_A$source, levels = desired_order2)

compare_A <- ggplot(regpro_df_A, aes(x = regpro, y = mrp_mean, colour=source, shape = model)) +
  theme_bw()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 2) +
  scale_color_manual(values = c("#F8766D", "#00BFC4","#619CFF","#00BA38","#F564E3")) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Seropositivity") +
  xlab("Province/Region") +
  ggtitle("Setting G (Jan- Apr 22), CBS vs AbC vs APL, Alberta") +
  theme(plot.title = element_text(vjust = 0))
compare_A

###### B Canpath   ###
df_B_canpaths$Race <- as.numeric(as.character(df_B_canpaths$Race))
df_B_canpaths$Sex <- df_B_canpaths$sex

start_time <- Sys.time()
fit_B_canpaths <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race,
                             family = binomial,
                             prior = normal(0,2),
                             data = df_B_canpaths,
                             cores = 4,
                             seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_B_canpaths)

df_D_clsa$age_group <- age_groups_fun(df_D_clsa$age)

start_time <- Sys.time()
fit_D_clsa <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race,
                             family = binomial,
                             data = df_D_clsa,
                             prior = normal(0,1),
                             cores = 4, control=list(adapt_delta=0.9),
                             seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_D_clsa)

start_time <- Sys.time()
fit_D_clsa <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race,
                         family = binomial,
                         data = df_D_clsa,
                         prior = normal(0,1),
                         cores = 4, control=list(adapt_delta=0.9),
                         seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_D_clsa)

df_clsa$month <- month(df_clsa$date)
df_A_clsa <- df_clsa[df_clsa$month <= 4,]
df_B_clsa <- df_clsa[df_clsa$month %in% c(7,8),]

start_time <- Sys.time()
fit_A_clsa <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race,
                         family = binomial,
                         data = df_A_clsa,
                         prior = normal(0,1),
                         cores = 4, control=list(adapt_delta=0.9),
                         seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_A_clsa)

start_time <- Sys.time()
fit_B_clsa <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race,
                         family = binomial,
                         data = df_B_clsa,
                         prior = normal(0,1),
                         cores = 4, control=list(adapt_delta=0.9),
                         seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_B_clsa)


############################## Consistent Code Begins Here Setting A CLSA #####################
#census_all <- census_all[census_all$age_group != "< 18 years", ]
census_all[census_all$regpro == 'Atlantic',]$regpro <- 'ATL'

poststrat_clsa <-
  census_all %>%
  group_by(regpro,age_group,Sex, Race) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  filter(age_group %in% as.character(unique(df_A_clsa$age_group))) %>%
  drop_na()

df_A_clsa <- df_A_clsa[!is.na(df_A_clsa$age_group),]

poststrat_clsa$composite_strata <- paste(poststrat_clsa$age_group, poststrat_clsa$Sex, sep = "_")
df_A_clsa$composite_strata <- paste(df_A_clsa$age_group, df_A_clsa$Sex, sep = "_")

poststrat_clsa$composite_strata_1 <- paste(poststrat_clsa$age_group, poststrat_clsa$Sex, poststrat_clsa$Race, sep = "_")
df_A_clsa$composite_strata_1 <- paste(df_A_clsa$age_group, df_A_clsa$Sex, df_A_clsa$Race, sep = "_")

library(survey)
# By province/region
regpro_df <- data.frame(
  regpro = c("BC","AB","MB","SK","ON","ATL"), 
  mrp_A_clsa = NA,
  mrp_A_clsa_se = NA,
  ua_A_clsa = NA,
  ua_A_clsa_se = NA,
  rake_A_clsa = NA,
  rake_A_clsa_se = NA,
  post_A_clsa = NA,
  post_A_clsa_se = NA,
  n_sample = NA,
  n_full = NA
)

for(i in 1:nrow(regpro_df)) {
  
  # MRP estimate apl
  epred_mat <- posterior_epred(fit_A_clsa, newdata = poststrat_clsa, draws = 4000)
  filtering_condition <- which(poststrat_clsa$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_abc[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_A_clsa[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_A_clsa_se[i] <- sd(mrp_estimates_vector)
  
  # Unadjusted estimate apl
  regpro_df$ua_A_clsa[i] <- mean(filter(df_A_clsa, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_A_clsa, regpro==regpro_df$regpro[i]))
  regpro_df$ua_A_clsa_se[i] <- get_se_bernoulli(regpro_df$ua_A_clsa[i], regpro_df$n_sample[i])
  
  # Raking on age/sex apl (RAKE)
  data <- df_A_clsa[df_A_clsa$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_clsa[poststrat_clsa$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_clsa[poststrat_clsa$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_A_clsa[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_A_clsa_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A apl (POST)
  data <- df_A_clsa[df_A_clsa$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_clsa[poststrat_clsa$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_clsa[poststrat_clsa$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_A_clsa[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_A_clsa_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
}

options(survey.lonely.psu="adjust")

write.csv(regpro_df, "/home/yuanyu/projects/covid-donor-v-survey-antibody/4_output/regpro_df_A_clsa.csv")


############################## Consistent Code Begins Here Setting B #####################
df_B_canpaths <- canpaths[canpaths$sampledate >= '2021-07-01' & canpaths$sampledate <= '2021-08-31' & canpaths$regpro != 'MB', ]

census_all <- census_all[census_all$age_group != "< 18 years", ]

start_time <- Sys.time()
fit_B_cbs <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race,
                        family = binomial,
                        data = df_B_cbs,
                        cores = 4,
                        seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_B_cbs)

poststrat_cbs <-
  census_all %>%
  group_by(regpro,age_group,Sex,Race) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_abc <-
  census_all %>%
  group_by(regpro,age_group,Sex,Race) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_canpaths <-
  census_all %>%
  group_by(regpro,age_group,Sex, Race) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  filter(age_group!= "18-26 years") %>%
  drop_na()

poststrat_clsa <-
  census_all %>%
  group_by(regpro,age_group,Sex, Race) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  filter(age_group %in% as.character(unique(df_B_clsa$age_group))) %>%
  drop_na()

df_B_abc <- df_B_abc %>% filter(!is.na(age_group), !is.na(Sex), !is.na(Race))
df_B_cbs <- df_B_cbs %>% filter(!is.na(age_group), !is.na(Sex), !is.na(Race))
df_B_canpaths <- df_B_canpaths %>% filter(!is.na(age_group), !is.na(Sex), !is.na(Race))
df_B_clsa <- df_B_clsa %>% filter(!is.na(age_group), !is.na(Sex), !is.na(Race))

poststrat_cbs$composite_strata <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, sep = "_")
poststrat_abc$composite_strata <- paste(poststrat_abc$age_group, poststrat_abc$Sex, sep = "_")
poststrat_canpaths$composite_strata <- paste(poststrat_canpaths$age_group, poststrat_canpaths$Sex, sep = "_")
poststrat_clsa$composite_strata <- paste(poststrat_clsa$age_group, poststrat_clsa$Sex, sep = "_")
df_B_abc$composite_strata <- paste(df_B_abc$age_group, df_B_abc$Sex, sep = "_")
df_B_cbs$composite_strata <- paste(df_B_cbs$age_group, df_B_cbs$Sex, sep = "_")
df_B_canpaths$composite_strata <- paste(df_B_canpaths$age_group, df_B_canpaths$Sex, sep = "_")
df_B_clsa$composite_strata <- paste(df_B_clsa$age_group, df_B_clsa$Sex, sep = "_")

poststrat_cbs$composite_strata_1 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$Race, sep = "_")
poststrat_abc$composite_strata_1 <- paste(poststrat_abc$age_group, poststrat_abc$Sex, poststrat_abc$Race, sep = "_")
poststrat_canpaths$composite_strata_1 <- paste(poststrat_canpaths$age_group, poststrat_canpaths$Sex, poststrat_canpaths$Race, sep = "_")
poststrat_clsa$composite_strata_1 <- paste(poststrat_clsa$age_group, poststrat_clsa$Sex, poststrat_clsa$Race, sep = "_")
df_B_abc$composite_strata_1 <- paste(df_B_abc$age_group, df_B_abc$Sex, df_B_abc$Race, sep = "_")
df_B_cbs$composite_strata_1 <- paste(df_B_cbs$age_group, df_B_cbs$Sex, df_B_cbs$Race, sep = "_")
df_B_canpaths$composite_strata_1 <- paste(df_B_canpaths$age_group, df_B_canpaths$Sex, df_B_canpaths$Race, sep = "_")
df_B_clsa$composite_strata_1 <- paste(df_B_clsa$age_group, df_B_clsa$Sex, df_B_clsa$Race, sep = "_")

library(survey)
# By province/region
regpro_df <- data.frame(
  regpro = c("BC","AB","MB","SK","ON","ATL"), 
  mrp_B_cbs = NA,
  mrp_B_cbs_se = NA,
  mrp_B_abc = NA,
  mrp_B_abc_se = NA,
  mrp_B_canpaths = NA,
  mrp_B_canpaths_se = NA,
  mrp_B_clsa = NA,
  mrp_B_clsa_se = NA,
  ua_B_cbs = NA,
  ua_B_cbs_se = NA,
  ua_B_abc = NA,
  ua_B_abc_se = NA,
  ua_B_canpaths = NA,
  ua_B_canpaths_se = NA,
  ua_B_clsa = NA,
  ua_B_clsa_se = NA,
  rake_B_cbs = NA,
  rake_B_cbs_se = NA,
  rake_B_abc = NA,
  rake_B_abc_se = NA,
  rake_B_canpaths = NA,
  rake_B_canpaths_se = NA,
  rake_B_clsa = NA,
  rake_B_clsa_se = NA,
  post_B_cbs = NA,
  post_B_cbs_se = NA,
  post_B_abc = NA,
  post_B_abc_se = NA,
  post_B_canpaths = NA,
  post_B_canpaths_se = NA,
  post_B_clsa = NA,
  post_B_clsa_se = NA,
  n_sample = NA,
  n_full = NA
)

for(i in 1:nrow(regpro_df)) {
  
  # MRP estimate abc
  epred_mat <- posterior_epred(fit_B_abc, newdata = poststrat_abc, draws = 4000)
  filtering_condition <- which(poststrat_abc$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_abc[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_B_abc[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_B_abc_se[i] <- sd(mrp_estimates_vector)
  
  # MRP estimate cbs
  epred_mat <- posterior_epred(fit_B_cbs, newdata = poststrat_cbs, draws = 4000)
  filtering_condition <- which(poststrat_cbs$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_cbs[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_B_cbs[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_B_cbs_se[i] <- sd(mrp_estimates_vector)
  
  # MRP estimate canpaths
  epred_mat <- posterior_epred(fit_B_canpaths, newdata = poststrat_canpaths, draws = 4000)
  filtering_condition <- which(poststrat_canpaths$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_abc[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_B_canpaths[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_B_canpaths_se[i] <- sd(mrp_estimates_vector)
  
  # MRP estimate clsa
  epred_mat <- posterior_epred(fit_B_clsa, newdata = poststrat_clsa, draws = 4000)
  filtering_condition <- which(poststrat_clsa$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_abc[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_B_clsa[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_B_clsa_se[i] <- sd(mrp_estimates_vector)
  
  # Unadjusted estimate abc
  regpro_df$ua_B_abc[i] <- mean(filter(df_B_abc, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_B_abc, regpro==regpro_df$regpro[i]))
  regpro_df$ua_B_abc_se[i] <- get_se_bernoulli(regpro_df$ua_B_abc[i], regpro_df$n_sample[i])
  
  # Unadjusted estimate cbs
  regpro_df$ua_B_cbs[i] <- mean(filter(df_B_cbs, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_B_cbs, regpro==regpro_df$regpro[i]))
  regpro_df$ua_B_cbs_se[i] <- get_se_bernoulli(regpro_df$ua_B_cbs[i], regpro_df$n_sample[i])
  
  # Unadjusted estimate canpaths
  regpro_df$ua_B_canpaths[i] <- mean(filter(df_B_canpaths, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_B_canpaths, regpro==regpro_df$regpro[i]))
  regpro_df$ua_B_canpaths_se[i] <- get_se_bernoulli(regpro_df$ua_B_canpaths[i], regpro_df$n_sample[i])
  
  # Unadjusted estimate clsa
  regpro_df$ua_B_clsa[i] <- mean(filter(df_B_clsa, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_B_clsa, regpro==regpro_df$regpro[i]))
  regpro_df$ua_B_clsa_se[i] <- get_se_bernoulli(regpro_df$ua_B_clsa[i], regpro_df$n_sample[i])

  # Raking on age/sex A abc (POST)
  data <- df_B_abc[df_B_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$rake_B_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_B_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A cbs (POST)
  data <- df_B_cbs[df_B_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$rake_B_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_B_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A canpaths (POST)
  data <- df_B_canpaths[df_B_canpaths$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_canpaths[poststrat_canpaths$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_canpaths[poststrat_canpaths$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$rake_B_canpaths[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_B_canpaths_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
 
  # Raking on age/sex A clsa (POST)
  data <- df_B_clsa[df_B_clsa$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_clsa[poststrat_clsa$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_clsa[poststrat_clsa$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$rake_B_clsa[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_B_clsa_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
   
  # Raking on age/sex A abc (POST)
  data <- df_B_abc[df_B_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_B_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_B_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A cbs (POST)
  data <- df_B_cbs[df_B_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_B_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_B_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A apl (POST)
  data <- df_B_canpaths[df_B_canpaths$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_canpaths[poststrat_canpaths$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_canpaths[poststrat_canpaths$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_B_canpaths[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_B_canpaths_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A apl (POST)
  data <- df_B_clsa[df_B_clsa$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_clsa[poststrat_clsa$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_clsa[poststrat_clsa$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_B_clsa[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_B_clsa_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
}

options(survey.lonely.psu="adjust")

write.csv(regpro_df, "/home/yuanyu/projects/covid-donor-v-survey-antibody/4_output/regpro_df_B_new.csv")

# reload the dataframe in long format
regpro_df_B <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_B_new.csv')


epred_mat <- posterior_epred(fit_A_canpaths, newdata = poststrat_canpaths, draws = 4000)
filtering_condition <- which(poststrat_canpaths$regpro == 'SK')
regpro_epred_mat <- epred_mat[ ,filtering_condition]
k_filtered <- poststrat_abc[filtering_condition, ]$n
mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
# regpro_df$mrp_A_canpaths[i] <- mean(mrp_estimates_vector)
regpro_df$mrp_A_canpaths_se[i] <- sd(mrp_estimates_vector)


regpro_df$ua_A_canpaths[i] <- mean(filter(df_A_canpaths, regpro=='MB')$interp_roche_n)
regpro_df$n_sample[i] <- nrow(filter(df_A_canpaths, regpro=='MB'))
regpro_df$ua_A_canpaths_se[i] <- get_se_bernoulli(regpro_df$ua_A_canpaths[i], regpro_df$n_sample[i])



############################## Consistent Code Begins Here Setting C #####################

start_time <- Sys.time()
fit_C_cbs <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race,
                        family = binomial,
                        data = df_C_cbs,
                        cores = 4,
                        seed = 1010, adapt_delta = 0.99)
end_time <- Sys.time()
end_time - start_time
print(fit_C_cbs)

############################## Consistent Code Begins Here Setting C Jan-Mar #####################
census_all <- census_all[census_all$age_group != "< 18 years", ]

poststrat_cbs <-
  census_all %>%
  group_by(regpro,age_group,Sex,Race) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_abc <-
  census_all %>%
  group_by(regpro,age_group,Sex,Urban) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_apl <-
  census_all %>%
  group_by(regpro,age_group,Sex, Urban) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

df_G_abc$age_group <- age_groups_fun(df_G_abc$age)
df_G_cbs$age_group <- age_groups_fun(df_G_cbs$age)
df_G_apl$age_group <- age_groups_fun(df_G_apl$age)

df_G_abc <- df_G_abc[!is.na(df_G_abc$age_group),]
df_G_cbs <- df_G_cbs[!is.na(df_G_cbs$age_group),]
df_G_apl <- df_G_apl[!is.na(df_G_apl$age_group),]

poststrat_cbs$composite_strata <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, sep = "_")
poststrat_abc$composite_strata <- paste(poststrat_abc$age_group, poststrat_abc$Sex, sep = "_")
poststrat_apl$composite_strata <- paste(poststrat_apl$age_group, poststrat_apl$Sex, sep = "_")
df_G_abc$composite_strata <- paste(df_G_abc$age_group, df_G_abc$Sex, sep = "_")
df_G_cbs$composite_strata <- paste(df_G_cbs$age_group, df_G_cbs$Sex, sep = "_")
df_G_apl$composite_strata <- paste(df_G_apl$age_group, df_G_apl$Sex, sep = "_")

poststrat_cbs$composite_strata_1 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$Urban, sep = "_")
poststrat_abc$composite_strata_1 <- paste(poststrat_abc$age_group, poststrat_abc$Sex, poststrat_abc$Urban, sep = "_")
poststrat_apl$composite_strata_1 <- paste(poststrat_apl$age_group, poststrat_apl$Sex, poststrat_apl$Urban, sep = "_")
df_G_abc$composite_strata_1 <- paste(df_G_abc$age_group, df_G_abc$Sex, df_G_abc$Urban, sep = "_")
df_G_cbs$composite_strata_1 <- paste(df_G_cbs$age_group, df_G_cbs$Sex, df_G_cbs$Urban, sep = "_")
df_G_apl$composite_strata_1 <- paste(df_G_apl$age_group, df_G_apl$Sex, df_G_apl$Urban, sep = "_")

library(survey)
# By province/region
regpro_df <- data.frame(
  regpro = c("AB"), 
  mrp_G_cbs = NA,
  mrp_G_cbs_se = NA,
  mrp_G_abc = NA,
  mrp_G_abc_se = NA,
  mrp_G_apl = NA,
  mrp_G_apl_se = NA,
  ua_G_cbs = NA,
  ua_G_cbs_se = NA,
  ua_G_abc = NA,
  ua_G_abc_se = NA,
  ua_G_apl = NA,
  ua_G_apl_se = NA,
  rake_G_cbs = NA,
  rake_G_cbs_se = NA,
  rake_G_abc = NA,
  rake_G_abc_se = NA,
  rake_G_apl = NA,
  rake_G_apl_se = NA,
  post_G_cbs = NA,
  post_G_cbs_se = NA,
  post_G_abc = NA,
  post_G_abc_se = NA,
  post_G_apl = NA,
  post_G_apl_se = NA,
  n_sample = NA,
  n_full = NA
)

for(i in 1:nrow(regpro_df)) {
  
  # MRP estimate abc
  epred_mat <- posterior_epred(fit_G_abc, newdata = poststrat_abc, draws = 4000)
  filtering_condition <- which(poststrat_abc$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_abc[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_G_abc[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_G_abc_se[i] <- sd(mrp_estimates_vector)
  
  # MRP estimate cbs
  epred_mat <- posterior_epred(fit_G_cbs, newdata = poststrat_cbs, draws = 4000)
  filtering_condition <- which(poststrat_cbs$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_cbs[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_G_cbs[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_G_cbs_se[i] <- sd(mrp_estimates_vector)
  
  # MRP estimate apl
  epred_mat <- posterior_epred(fit_G_apl, newdata = poststrat_apl, draws = 4000)
  filtering_condition <- which(poststrat_abc$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_abc[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_G_apl[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_G_apl_se[i] <- sd(mrp_estimates_vector)
  
  # Unadjusted estimate abc
  regpro_df$ua_G_abc[i] <- mean(filter(df_G_abc, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_G_abc, regpro==regpro_df$regpro[i]))
  regpro_df$ua_G_abc_se[i] <- get_se_bernoulli(regpro_df$ua_G_abc[i], regpro_df$n_sample[i])
  
  # Unadjusted estimate cbs
  regpro_df$ua_G_cbs[i] <- mean(filter(df_G_cbs, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_G_cbs, regpro==regpro_df$regpro[i]))
  regpro_df$ua_G_cbs_se[i] <- get_se_bernoulli(regpro_df$ua_G_cbs[i], regpro_df$n_sample[i])
  
  # Unadjusted estimate apl
  regpro_df$ua_G_apl[i] <- mean(filter(df_G_apl, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_G_apl, regpro==regpro_df$regpro[i]))
  regpro_df$ua_G_apl_se[i] <- get_se_bernoulli(regpro_df$ua_G_apl[i], regpro_df$n_sample[i])
  
  # Raking on age/sex abc (RAKE)
  data <- df_G_abc[df_G_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_G_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_G_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex cbs (RAKE)
  data <- df_G_cbs[df_G_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_G_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_G_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex apl (RAKE)
  data <- df_G_apl[df_G_apl$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_apl[poststrat_apl$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_apl[poststrat_apl$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_G_apl[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_G_apl_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A abc (POST)
  data <- df_G_abc[df_G_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_G_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_G_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A cbs (POST)
  data <- df_G_cbs[df_G_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_G_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_G_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A apl (POST)
  data <- df_G_apl[df_G_apl$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_apl[poststrat_apl$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_apl[poststrat_apl$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_G_apl[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_G_apl_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
}

options(survey.lonely.psu="adjust")

write.csv(regpro_df, "/home/yuanyu/projects/covid-donor-v-survey-antibody/4_output/regpro_df_G.csv")

# reload the dataframe in long format
regpro_df_G <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_G_new.csv')