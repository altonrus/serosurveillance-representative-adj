# Analysis with new poststratification table on age, sex, race, urban
census_all <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/1_data/private/CENSUS/census_w_counts_race_urban.csv')
census_all <- rename(census_all, regpro = province)
census_all <- rename(census_all, Urban = urban)


age_groups_fun <- function(variable){
  age_group = cut(variable,
                  breaks = c(18,26,36,46,56,Inf),
                  labels = c("18-26 years",
                             "27-36 years","37-46 years","47-56 years",
                             "56+ years"),
                  right = FALSE)
  return(age_group)
}

df_cbs$age_group <- age_groups_fun(df_cbs$age)

df_E_cbs <- df_cbs[df_cbs$sampledate >= '2022-04-04' & df_cbs$sampledate <= '2022-08-31',] #132927 

df_E_cbs_1 <- df_E_cbs_1 %>% mutate(year_month = format(df_E_cbs_1$sampledate, "%Y-%m"))
start_time <- Sys.time()
fit_E_cbs <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race + Urban + year_month,
                        family = binomial,
                        data = df_E_cbs_1,
                        cores = 4,
                        seed = 1010, adapt_delta = 0.99)
end_time <- Sys.time()
end_time - start_time
print(fit_E_cbs)

df_cbs <- df_cbs %>% mutate(year_month = format(df_cbs$sampledate, "%Y-%m"))
table(df_cbs$year_month)


########################### Consistent Code Begins Here Setting E #####################
poststrat_cbs <-
  census_all %>%
  group_by(regpro,age_group,Sex,Race,Urban) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_cbs <- poststrat_cbs[poststrat_cbs$age_group != "< 18 years", ]
poststrat_cbs[poststrat_cbs$regpro == 'Atlantic', ]$regpro <- 'ATL'

df_E_cbs <- df_E_cbs[!is.na(df_E_cbs$age_group),]

poststrat_cbs$composite_strata <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, sep = "_")
df_E_cbs$composite_strata <- paste(df_E_cbs$age_group, df_E_cbs$Sex, sep = "_")

poststrat_cbs$composite_strata_1 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$Race, sep = "_")
df_E_cbs$composite_strata_1 <- paste(df_E_cbs$age_group, df_E_cbs$Sex, df_E_cbs$Race, sep = "_")

poststrat_cbs$composite_strata_2 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$Race, poststrat_cbs$Urban, sep = "_")
df_E_cbs$composite_strata_2 <- paste(df_E_cbs$age_group, df_E_cbs$Sex, df_E_cbs$Race, df_E_cbs$Urban, sep = "_")

# By province/region
regpro_df <- data.frame(
  regpro = c("BC","AB","MB","SK","ON","ATL"), 
  mrp_E_cbs = NA,
  mrp_E_cbs_se = NA,
  ua_E_cbs = NA,
  ua_E_cbs_se = NA,
  rake_E_cbs = NA,
  rake_E_cbs_se = NA,
  rake1_E_cbs = NA,
  rake1_E_cbs_se = NA,
  rake2_E_cbs = NA,
  rake2_E_cbs_se = NA,
  n_sample = NA,
  n_full = NA
)

for(i in 1:nrow(regpro_df)) {
  
  # MRP estimate B cbs
  epred_mat <- posterior_epred(fit_E_cbs, newdata = poststrat_cbs, draws = 4000)
  filtering_condition <- which(poststrat_cbs$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_cbs[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_E_cbs[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_E_cbs_se[i] <- sd(mrp_estimates_vector)
  
  # Unadjusted estimate B cbs
  regpro_df$ua_E_cbs[i] <- mean(filter(df_E_cbs, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_E_cbs, regpro==regpro_df$regpro[i]))
  regpro_df$ua_E_cbs_se[i] <- get_se_bernoulli(regpro_df$ua_E_cbs[i], regpro_df$n_sample[i])
  
  # Raking on age/sex B cbs (RAKE)
  data <- df_E_cbs[df_E_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_E_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_E_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex B cbs (RAKE1)
  data <- df_E_cbs[df_E_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata_1),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake1_E_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake1_E_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A cbs (RAKE2)
  data <- df_E_cbs[df_E_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_2, data = data)
  poststrat.dist <- data.frame(composite_strata_2 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_2,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata_2),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake2_E_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake2_E_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
}

write.csv(regpro_df, "/home/yuanyu/projects/covid_donors_3waves/1_data/regpro_df_E.csv")

regpro_df_E <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_E.csv')

regpro_df_E[regpro_df_E$model == 'UA', ]$model <- 'NO_Adjust'
regpro_df_E[regpro_df_E$model == 'RAKE', ]$model <- 'Age_Sex'
regpro_df_E[regpro_df_E$model == 'RAKE1', ]$model <- 'Age_Sex_Race'
regpro_df_E[regpro_df_E$model == 'RAKE2', ]$model <- 'Age_Sex_Race_Urban'
regpro_df_E[regpro_df_E$model == 'MRP', ]$model <- 'MRP'

desired_order <- c('NO_Adjust','Age_Sex','Age_Sex_Race','Age_Sex_Race_Urban',"MRP")
desired_order1 <- c("BC","AB","MB","SK","ON","ATL")
desired_order2 <- c("CBS","CCAHS")

# Reorder the levels of the 'category' variable
regpro_df_E$model <- factor(regpro_df_E$model, levels = desired_order)
regpro_df_E$regpro <- factor(regpro_df_E$regpro, levels = desired_order1)
regpro_df_E$source <- factor(regpro_df_E$source, levels = desired_order2)

compare_E <- ggplot(regpro_df_E, aes(x = regpro, y = mrp_mean, colour=source, shape = model)) +
  theme_bw()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 2) +
  scale_color_manual(name="Study", values = c("#F8766D", "#00BA38")) +
  #  scale_color_manual(values = my_colors) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_shape_manual(
    name = "Adjustment",
    values = c("MRP" = 7, 
               "NO_Adjust" = 16 ,
               "Age_Sex" = 17, 
               "Age_Sex_Race" = 15,
               "Age_Sex_Race_Urban" = 3),
    labels = c("MRP" = "MRP", 
               "NO_Adjust" = "No adjustment" ,
               "Age_Sex" = "Raking (Age-Sex)", 
               "Age_Sex_Race" = "Raking (Age-Sex-Race)",
               "Age_Sex_Race_Urban" = "Poststratification (Age-Sex-Race-Urban)")) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Seropositivity") +
  xlab("Province/Region") +
#  ggtitle("Setting E (Apr-Oct, 2022), CBS vs CCAHS") +
  theme(plot.title = element_text(vjust = 0)) +
  theme(legend.position = "bottom")
compare_E


df_F_cbs <- df_cbs[df_cbs$sampledate >= '2020-12-01' & df_cbs$sampledate < '2021-04-30',] #55156

start_time <- Sys.time()
fit_F_cbs <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race + Urban,
                        family = binomial,
                        data = df_F_cbs,
                        cores = 4,
                        seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_F_cbs)

df_A1_cbs <- df_A_cbs
df_A_cbs <- df_cbs[df_cbs$sampledate >= '2021-01-01' & df_cbs$sampledate < '2021-05-01',] #54492
start_time <- Sys.time()
fit_A_cbs <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race + Urban,
                        family = binomial,
                        data = df_A_cbs,
                        cores = 4,
                        seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_A_cbs)

#df_A_abc
df_A_abc$age_group <- age_groups_fun(df_A_abc$age)
start_time <- Sys.time()
fit_A_abc <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race + Urban,
                        family = binomial,
                        data = df_A_abc,
                        cores = 4,
                        seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_A_abc)

#df_A_canpath

start_time <- Sys.time()
fit_A_canpath <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race + Urban,
                        family = binomial,
                        data = df_A_canpath,
                        cores = 4,
                        seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_A_canpath)


df_A_abc$age
#################### Setting A ##########
df_A_cbs <- df_A_cbs[!is.na(df_A_cbs$age_group),]

poststrat_cbs$composite_strata <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, sep = "_")
df_A_cbs$composite_strata <- paste(df_A_cbs$age_group, df_A_cbs$Sex, sep = "_")

poststrat_cbs$composite_strata_1 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$Race, sep = "_")
df_A_cbs$composite_strata_1 <- paste(df_A_cbs$age_group, df_A_cbs$Sex, df_A_cbs$Race, sep = "_")

poststrat_cbs$composite_strata_2 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$Race, poststrat_cbs$Urban, sep = "_")
df_A_cbs$composite_strata_2 <- paste(df_A_cbs$age_group, df_A_cbs$Sex, df_A_cbs$Race, df_A_cbs$Urban, sep = "_")

# By province/region
regpro_df <- data.frame(
  regpro = c("BC","AB","MB","SK","ON","ATL"), 
  mrp_A_cbs = NA,
  mrp_A_cbs_se = NA,
  ua_A_cbs = NA,
  ua_A_cbs_se = NA,
  rake_A_cbs = NA,
  rake_A_cbs_se = NA,
  rake1_A_cbs = NA,
  rake1_A_cbs_se = NA,
  rake2_A_cbs = NA,
  rake2_A_cbs_se = NA,
  n_sample = NA,
  n_full = NA
)

for(i in 1:nrow(regpro_df)) {
  
  # MRP estimate B cbs
  epred_mat <- posterior_epred(fit_A_cbs, newdata = poststrat_cbs, draws = 4000)
  filtering_condition <- which(poststrat_cbs$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_cbs[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_A_cbs[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_A_cbs_se[i] <- sd(mrp_estimates_vector)
  
  # Unadjusted estimate B cbs
  regpro_df$ua_A_cbs[i] <- mean(filter(df_A_cbs, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_A_cbs, regpro==regpro_df$regpro[i]))
  regpro_df$ua_A_cbs_se[i] <- get_se_bernoulli(regpro_df$ua_A_cbs[i], regpro_df$n_sample[i])
  
  # Raking on age/sex B cbs (RAKE)
  data <- df_A_cbs[df_A_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_A_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_A_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex B cbs (RAKE1)
  data <- df_A_cbs[df_A_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata_1),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake1_A_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake1_A_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A cbs (RAKE2)
  data <- df_A_cbs[df_A_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_2, data = data)
  poststrat.dist <- data.frame(composite_strata_2 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_2,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata_2),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake2_A_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake2_A_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
}

write.csv(regpro_df, "/home/yuanyu/projects/covid_donors_3waves/1_data/regpro_df_A.csv")

regpro_df_A <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_A.csv')

regpro_df_A[regpro_df_A$model == 'UA', ]$model <- 'NO_Adjust'
regpro_df_A[regpro_df_A$model == 'RAKE', ]$model <- 'Age_Sex'
regpro_df_A[regpro_df_A$model == 'RAKE1', ]$model <- 'Age_Sex_Race*'
regpro_df_A[regpro_df_A$model == 'RAKE2', ]$model <- 'Age_Sex_Race_Urban*'
regpro_df_A[regpro_df_A$model == 'MRP', ]$model <- 'MRP'

desired_order <- c('NO_Adjust','Age_Sex','Age_Sex_Race*','Age_Sex_Race_Urban*',"MRP")
desired_order1 <- c("BC","AB","MB","SK","ON","ATL")
desired_order2 <- c("CBS","ABC","CCAHS")



# Reorder the levels of the 'category' variable
regpro_df_A$model <- factor(regpro_df_A$model, levels = desired_order)
regpro_df_A$regpro <- factor(regpro_df_A$regpro, levels = desired_order1)
regpro_df_A$source <- factor(regpro_df_A$source, levels = desired_order2)

compare_A <- ggplot(regpro_df_A, aes(x = regpro, y = mrp_mean, colour=source, shape = model)) +
  theme_bw()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 1.5) +
  scale_color_manual(name= "Study", values = c("#F8766D", "#00BFC4","#00BA38")) +
  #  scale_color_manual(values = my_colors) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  scale_shape_manual(
    name = "Adjustment",
    values = c("MRP" = 7, 
               "NO_Adjust" = 16 ,
               "Age_Sex" = 17, 
               "Age_Sex_Race*" = 15,
               "Age_Sex_Race_Urban*" = 3),
    labels = c("MRP" = "MRP", 
               "NO_Adjust" = "No adjustment" ,
               "Age_Sex" = "Raking (Age-Sex)", 
               "Age_Sex_Race*" = "Raking (Age-Sex-Race)",
               "Age_Sex_Race_Urban*" = "Poststratification (Age-Sex-Race-Urban)")) +
  ylab("Seropositivity") +
  xlab("Province/Region") +
  guides(
    color = guide_legend(order = 2),  # Color legend second
    shape = guide_legend(order = 1)   # Shape legend first
  ) +
#  ggtitle("Setting A (Jan - Apr, 2021), CBS vs AbC vs CCAHS") +
  theme(plot.title = element_text(vjust = 0)) +
  theme(legend.position = "bottom")
compare_A

########################### Consistent Code Begins Here Setting A #####################
df_A_cbs <- df_A_cbs[!is.na(df_A_cbs$age_group),]

poststrat_cbs$composite_strata <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, sep = "_")
df_A_cbs$composite_strata <- paste(df_A_cbs$age_group, df_A_cbs$Sex, sep = "_")

poststrat_cbs$composite_strata_1 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$Race, sep = "_")
df_A_cbs$composite_strata_1 <- paste(df_A_cbs$age_group, df_A_cbs$Sex, df_A_cbs$Race, sep = "_")

poststrat_cbs$composite_strata_2 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$Race, poststrat_cbs$Urban, sep = "_")
df_A_cbs$composite_strata_2 <- paste(df_A_cbs$age_group, df_A_cbs$Sex, df_A_cbs$Race, df_A_cbs$Urban, sep = "_")

df_A_abc$composite_strata <- paste(df_A_abc$age_group, df_A_abc$Sex, sep = "_")
df_A_abc$composite_strata_1 <- paste(df_A_abc$age_group, df_A_abc$Sex, df_A_abc$Race, sep = "_")
df_A_abc$composite_strata_2 <- paste(df_A_abc$age_group, df_A_abc$Sex, df_A_abc$Race, df_A_abc$Urban, sep = "_")


# By province/region
regpro_df <- data.frame(
  regpro = c("BC","AB","MB","SK","ON","ATL"), 
  mrp_A_cbs = NA,
  mrp_A_cbs_se = NA,
  mrp_A_abc = NA,
  mrp_A_abc_se = NA,
  ua_A_cbs = NA,
  ua_A_cbs_se = NA,
  ua_A_abc = NA,
  ua_A_abc_se = NA,
  rake_A_cbs = NA,
  rake_A_cbs_se = NA,
  rake_A_abc = NA,
  rake_A_abc_se = NA,
  rake1_A_cbs = NA,
  rake1_A_cbs_se = NA,
  rake1_A_abc = NA,
  rake1_A_abc_se = NA,
  rake2_A_cbs = NA,
  rake2_A_cbs_se = NA,
  rake2_A_abc = NA,
  rake2_A_abc_se = NA,
  n_sample = NA,
  n_full = NA
)

for(i in 1:nrow(regpro_df)) {
  
  # MRP estimate B cbs
  epred_mat <- posterior_epred(fit_A_abc, newdata = poststrat_cbs, draws = 4000)
  filtering_condition <- which(poststrat_cbs$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_cbs[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_A_abc[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_A_abc_se[i] <- sd(mrp_estimates_vector)
  
  # Unadjusted estimate B cbs
  regpro_df$ua_A_abc[i] <- mean(filter(df_A_abc, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_A_abc, regpro==regpro_df$regpro[i]))
  regpro_df$ua_A_abc_se[i] <- get_se_bernoulli(regpro_df$ua_A_abc[i], regpro_df$n_sample[i])
  
  # Raking on age/sex B cbs (RAKE)
  data <- df_A_abc[df_A_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_A_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_A_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex B cbs (RAKE1)
  data <- df_A_abc[df_A_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$rake1_A_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake1_A_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A cbs (RAKE2)
  data <- df_A_abc[df_A_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_2, data = data)
  poststrat.dist <- data.frame(composite_strata_2 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_2,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_2,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$rake2_A_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake2_A_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  ####
  # MRP estimate B cbs
  epred_mat <- posterior_epred(fit_A_cbs, newdata = poststrat_cbs, draws = 4000)
  filtering_condition <- which(poststrat_cbs$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_cbs[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_A_cbs[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_A_cbs_se[i] <- sd(mrp_estimates_vector)
  
  # Unadjusted estimate B cbs
  regpro_df$ua_A_cbs[i] <- mean(filter(df_A_cbs, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_A_cbs, regpro==regpro_df$regpro[i]))
  regpro_df$ua_A_cbs_se[i] <- get_se_bernoulli(regpro_df$ua_A_cbs[i], regpro_df$n_sample[i])
  
  # Raking on age/sex B cbs (RAKE)
  data <- df_A_cbs[df_A_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_A_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_A_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex B cbs (RAKE1)
  data <- df_A_cbs[df_A_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$rake1_A_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake1_A_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A cbs (RAKE2)
  data <- df_A_cbs[df_A_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_2, data = data)
  poststrat.dist <- data.frame(composite_strata_2 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_2,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_2,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$rake2_A_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake2_A_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
}
options(survey.lonely.psu="certainty")
write.csv(regpro_df, "/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_A.csv")


############ Setting A Canpath Jan-Apr, 2021 ##########
canpaths$Urban <- factor(case_when(as.character(substr(canpaths$fsa,start = 2,stop = 2)) != "0" ~ 1, 
                                   as.character(substr(canpaths$fsa,start = 2,stop = 2)) == "0" ~ 0,
                                   TRUE~NA))

canpaths$Race <- factor(case_when(canpaths$race == "White" ~ 1, 
                                  canpaths$race == "Racialized minority" ~ 0,
                                   TRUE~NA))

df_A_canpaths <- canpaths[canpaths$sampledate >= '2021-01-01' & canpaths$sampledate <= '2021-04-30', ]
start_time <- Sys.time()
fit_A_canpaths <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + sex + Race,
                        family = binomial,
                        data = df_A_canpaths,
                        cores = 4,
                        seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_A_canpaths)

############ Setting B Canpath Jul-Aug, 2021 ##########
df_B_canpaths <- canpaths[canpaths$sampledate >= '2021-07-01' & canpaths$sampledate <= '2021-08-31' & canpaths$regpro != 'MB', ]

canpaths$Race <- as.numeric(as.character(canpaths$Race))
canpaths$Sex <- canpaths$sex


start_time <- Sys.time()
fit_B_canpaths <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + sex + Race,
                             family = binomial,
                             data = df_B_canpaths,
                             # prior = c(prior(normal(0, 5), class = Intercept),
                             #                      prior(normal(0, 1), class = b),
                             #                      prior(exponential(0.5), class = sd)),
                             # prior = normal(location = c(0, 5), scale = c(5, 2)),
                             prior_intercept = normal(-3,1, autoscale = FALSE),
#                             prior = normal(0,1, autoscale = FALSE),
#                             prior_covariance = decov(4),
                             control = list(adapt_delta = 0.99, max_treedepth  = 10),
                             prior_aux = normal(0,1, autoscale = FALSE),
                             iter = 4000,
                             cores = 4,
                             seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_B_canpaths)

start_time <- Sys.time()
fit_B_canpaths <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + sex + Race,
                             family = binomial,
                             data = df_B_canpaths,
                             prior_intercept = normal(-3,10, autoscale = FALSE),
#                             prior = normal(0,1, autoscale = FALSE),
#                             prior_covariance = decov(4),
                             control = list(adapt_delta = 0.99, max_treedepth  = 10),
#                             prior_aux = normal(0,1, autoscale = FALSE),
                             iter = 4000,
                             cores = 4,
                             seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_B_canpaths)

prior_summary(fit_B_canpaths)

############ Setting A AbC Jan-Apr, 2021 ##########
start_time <- Sys.time()
fit_A_abc <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race,
                             family = binomial,
                             data = df_A_abc,
                             cores = 4,
                             seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_A_abc)
#waic(fit_A_abc)

############ Setting B AbC Jul-Oct, 2021 ##########
start_time <- Sys.time()
fit_B_abc <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race,
                        family = binomial,
                        data = df_B_abc,
                        cores = 4,
                        seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_B_abc)
waic(fit_B_abc)

############ Setting C AbC Jan-Mar, 2022 ##########
start_time <- Sys.time()
fit_C_abc <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race,
                        family = binomial,
                        data = df_C_abc,
                        iter = 3000,
                        cores = 4,
                        seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_C_abc)
waic(fit_C_abc)
