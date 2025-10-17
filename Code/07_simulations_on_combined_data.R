#Fig S6 & Tab S4
#Simulations on the biased data for CBS
df_E_cbs_biased <- df_E_cbs_1 %>%
  filter(!is.na(age_group)) %>% 
  sample_n(16000, weight = I((age_group=="age_group18-26 years")*0.5 + 
                               (age_group=="age_group27-36 years")*1 + 
                               (age_group=="age_group37-46 years")*2 + 
                               (age_group=="age_group47-56 years")*4 + 
                               (age_group=="age_group56+ years")*6 + 
                               (Sex==0)*8 + 
                               (Race==1)*20))

start_time <- Sys.time()
fit_E_cbs_biased <- stan_gamm4(interp_roche_n ~ s(day, by=regpro, k=10) + age_group + Sex + Race, 
                        random = ~ (1 | regpro) , 
                        family = binomial, 
                        data = df_E_cbs_biased, iter = 2000, cores = 4, 
                        prior = normal(0,10), prior_covariance = decov(shape = 1, scale = 1), 
                        prior_smooth=normal(location=4), 
                        control=list(adapt_delta=0.95))
end_time <- Sys.time()
end_time - start_time
print(fit_E_cbs_biased)


ndays = max(df_E_cbs_biased$day)-min(df_E_cbs_biased$day) + 1 #79
i=0
poststrat_t= NULL
while (i < ndays){
  poststrat_t <- rbind(poststrat_t,poststrat_cbs_1)
  i = i+1
}
day_col <- c()
for(i in 1:ndays){
  day_col <- c(day_col,rep(min(df_E_cbs_biased$day)-1+i, nrow(poststrat_cbs_1)))
}
poststrat_t$day <- day_col

#ndays = 150
output_mrp <- matrix(ncol=4, nrow=6*ndays)
regpros <- c("BC","AB","MB","SK","ON","ATL")
epred_mat <- posterior_epred(fit_E_cbs_biased, newdata = poststrat_t, draws = 4000)
for(j in 1:6){
  for(i in 1:ndays) {
    # MRP estimate A cbs
    filtering_condition <- which(poststrat_t$day == (min(df_E_cbs_biased$day)-1 + i) & poststrat_t$regpro == regpros[j])
    regpro_epred_mat <- epred_mat[ ,filtering_condition]
    k_filtered <- poststrat_t[filtering_condition, ]$n
    mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
    output_mrp[i + (j - 1) * ndays, ] <- c(regpros[j], mean(mrp_estimates_vector), sd(mrp_estimates_vector), min(df_E_cbs_biased$day)-1+i)
  }
}

##[Apparent Prevalence + (Specificity − 1)]/
##[Specificity + (Sensitivity − 1)]
## For abc : [x+(0.99-1)]/[0.99+0.92-1]
## x/0.91-0.01/0.91
df_time <- as.data.frame(output_mrp)
colnames(df_time) <- c('regpro','mrp_mean', 'mrp_sd', 'day')
df_time$mrp_mean <- as.numeric(df_time$mrp_mean)/0.993-0.002/0.993
df_time$mrp_sd <- as.numeric(df_time$mrp_sd)/0.993
df_time$day <- as.integer(df_time$day)

df_time$lower <- df_time$mrp_mean - 2*df_time$mrp_sd
df_time$upper <- df_time$mrp_mean + 2*df_time$mrp_sd

#####
library(dplyr)
library(tidyverse)
output_ua <- matrix(ncol=4, nrow=6*ndays)
regpros <- c("BC","AB","MB","SK","ON","ATL")
poststrat_ua <-
  df_E_cbs_biased %>%
  group_by(regpro,age_group,Sex,Race,day) %>%
  dplyr::summarize(n = n()) %>%
  drop_na()
epred_mat <- posterior_epred(fit_E_cbs_biased, newdata = poststrat_ua, draws = 4000)
for(j in 1:6){
  for(i in 1:ndays) {
    # MRP estimate A cbs
    filtering_condition <- which(poststrat_ua$day == (min(df_E_cbs_biased$day)-1 + i) & poststrat_ua$regpro == regpros[j])
    regpro_epred_mat <- as.matrix(epred_mat[ ,filtering_condition])
    k_filtered <- poststrat_ua[filtering_condition, ]$n
    mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
    output_ua[i + (j - 1) * ndays, ] <- c(regpros[j], mean(mrp_estimates_vector), sd(mrp_estimates_vector), min(df_E_cbs_biased$day)-1+i)
  }
}
##[Apparent Prevalence + (Specificity − 1)]/
##[Specificity + (Sensitivity − 1)]
## For Roche : [x+(0.998-1)]/[0.998+0.995-1]
## x/0.993-0.002/0.993
df_time_ua <- as.data.frame(output_ua)
colnames(df_time_ua) <- c('regpro','mrp_mean', 'mrp_sd', 'day')
df_time_ua$mrp_mean <- as.numeric(df_time_ua$mrp_mean)/0.993-0.002/0.993
df_time_ua$mrp_sd <- as.numeric(df_time_ua$mrp_sd)/0.993
df_time_ua$day <- as.integer(df_time_ua$day)

df_time_ua$lower <- df_time_ua$mrp_mean - 2*df_time_ua$mrp_sd
df_time_ua$upper <- df_time_ua$mrp_mean + 2*df_time_ua$mrp_sd

df_time$model <- 'MRP'
df_time_ua$model <- 'MR'
df_time_cbs_bias_low <- rbind(df_time, df_time_ua)

df_time_cbs_bias_low$source <- 'CBS_BIAS_L'

df_time_cbs_sim <- rbind(df_time_cbs, df_time_cbs_bias_low)

ggplot(df_time_cbs_sim, aes(x = day, y = mrp_mean, color = source)) +
  geom_line(aes(linetype=model, color=source)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  labs(title = "Seropositivity Estimation during Omicron for biased sample, Apr-Aug 2022 (Lower)",
       x = "Year-Day",
       y = "Seropositivity") +
  theme_bw()

##############
df_E_cbs_biased2 <- df_E_cbs_1 %>%
  filter(!is.na(age_group)) %>% 
  sample_n(16000, weight = I((age_group=="age_group18-26 years")*6 + 
                               (age_group=="age_group27-36 years")*4 + 
                               (age_group=="age_group37-46 years")*2 + 
                               (age_group=="age_group47-56 years")*1 + 
                               (age_group=="age_group56+ years")*0.5 + 
                               (Sex==1)*8 + 
                               (Race==0)*20))

start_time <- Sys.time()
fit_E_cbs_biased2 <- stan_gamm4(interp_roche_n ~ s(day, by=regpro, k=10) + age_group + Sex + Race, 
                               random = ~ (1 | regpro) , 
                               family = binomial, 
                               data = df_E_cbs_biased2, iter = 2000, cores = 4, 
                               prior = normal(0,10), prior_covariance = decov(shape = 1, scale = 1), 
                               prior_smooth=normal(location=4), 
                               control=list(adapt_delta=0.95))
end_time <- Sys.time()
end_time - start_time
print(fit_E_cbs_biased2)

ndays = max(df_E_cbs_biased2$day)-min(df_E_cbs_biased2$day) + 1 #150
i=0
poststrat_t= NULL
while (i < ndays){
  poststrat_t <- rbind(poststrat_t,poststrat_cbs_1)
  i = i+1
}
day_col <- c()
for(i in 1:ndays){
  day_col <- c(day_col,rep(min(df_E_cbs_biased2$day)-1+i, nrow(poststrat_cbs_1)))
}
poststrat_t$day <- day_col

#ndays = 150
output_mrp <- matrix(ncol=4, nrow=6*ndays)
regpros <- c("BC","AB","MB","SK","ON","ATL")
epred_mat <- posterior_epred(fit_E_cbs_biased2, newdata = poststrat_t, draws = 4000)
for(j in 1:6){
  for(i in 1:ndays) {
    # MRP estimate A cbs
    filtering_condition <- which(poststrat_t$day == (min(df_E_cbs_biased2$day)-1 + i) & poststrat_t$regpro == regpros[j])
    regpro_epred_mat <- epred_mat[ ,filtering_condition]
    k_filtered <- poststrat_t[filtering_condition, ]$n
    mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
    output_mrp[i + (j - 1) * ndays, ] <- c(regpros[j], mean(mrp_estimates_vector), sd(mrp_estimates_vector), min(df_E_cbs_biased2$day)-1+i)
  }
}

##[Apparent Prevalence + (Specificity − 1)]/
##[Specificity + (Sensitivity − 1)]
## For abc : [x+(0.99-1)]/[0.99+0.92-1]
## x/0.91-0.01/0.91
df_time <- as.data.frame(output_mrp)
colnames(df_time) <- c('regpro','mrp_mean', 'mrp_sd', 'day')
df_time$mrp_mean <- as.numeric(df_time$mrp_mean)/0.993-0.002/0.993
df_time$mrp_sd <- as.numeric(df_time$mrp_sd)/0.993
df_time$day <- as.integer(df_time$day)

df_time$lower <- df_time$mrp_mean - 2*df_time$mrp_sd
df_time$upper <- df_time$mrp_mean + 2*df_time$mrp_sd

#####
library(dplyr)
library(tidyverse)
output_ua <- matrix(ncol=4, nrow=6*ndays)
regpros <- c("BC","AB","MB","SK","ON","ATL")
poststrat_ua <-
  df_E_cbs_biased2 %>%
  group_by(regpro,age_group,Sex,Race,day) %>%
  dplyr::summarize(n = n()) %>%
  drop_na()
epred_mat <- posterior_epred(fit_E_cbs_biased2, newdata = poststrat_ua, draws = 4000)
for(j in 1:6){
  for(i in 1:ndays) {
    # MRP estimate A cbs
    filtering_condition <- which(poststrat_ua$day == (min(df_E_cbs_biased2$day)-1 + i) & poststrat_ua$regpro == regpros[j])
    regpro_epred_mat <- as.matrix(epred_mat[ ,filtering_condition])
    k_filtered <- poststrat_ua[filtering_condition, ]$n
    mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
    output_ua[i + (j - 1) * ndays, ] <- c(regpros[j], mean(mrp_estimates_vector), sd(mrp_estimates_vector), min(df_E_cbs_biased2$day)-1+i)
  }
}
##[Apparent Prevalence + (Specificity − 1)]/
##[Specificity + (Sensitivity − 1)]
## For Roche : [x+(0.998-1)]/[0.998+0.995-1]
## x/0.993-0.002/0.993
df_time_ua <- as.data.frame(output_ua)
colnames(df_time_ua) <- c('regpro','mrp_mean', 'mrp_sd', 'day')
df_time_ua$mrp_mean <- as.numeric(df_time_ua$mrp_mean)/0.993-0.002/0.993
df_time_ua$mrp_sd <- as.numeric(df_time_ua$mrp_sd)/0.993
df_time_ua$day <- as.integer(df_time_ua$day)

df_time_ua$lower <- df_time_ua$mrp_mean - 2*df_time_ua$mrp_sd
df_time_ua$upper <- df_time_ua$mrp_mean + 2*df_time_ua$mrp_sd

df_time$model <- 'MRP'
df_time_ua$model <- 'MR'
df_time_cbs_bias_high <- rbind(df_time, df_time_ua)

df_time_cbs_bias_high$source <- 'CBS_BIAS_H'

df_time_cbs_sim <- rbind(df_time_cbs, df_time_cbs_bias_low, df_time_cbs_bias_high)

ggplot(df_time_cbs_sim, aes(x = day, y = mrp_mean, color = source)) +
  geom_line(aes(linetype=model, color=source)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
  #ylim(0.15, 0.8) +
  labs(linetype = "Adjustment") +
  scale_fill_manual(
    name = "Dataset",
    values = c("CBS" = "#F8766D", 
               "CBS_BIAS_H" = "#00BA38" ,
               "CBS_BIAS_L" = "#619CFF"),
    labels = c("CBS" = "CBS", 
               "CBS_BIAS_H" = "CBS biased towards \n higher seroposotivity" ,
               "CBS_BIAS_L" = "CBS biased towards \n lower seroposotivity")) +
  scale_color_manual(
    name = "Dataset",
    values = c("CBS" = "#F8766D", 
               "CBS_BIAS_H" = "#00BA38" ,
               "CBS_BIAS_L" = "#619CFF"),
    labels = c("CBS" = "CBS", 
               "CBS_BIAS_H" = "CBS biased towards \n higher seroposotivity" ,
               "CBS_BIAS_L" = "CBS biased towards \n lower seroposotivity")) +
  facet_wrap(~ regpro) +
  labs(#title = "Seropositivity Estimation during Omicron for biased sample, Apr-Aug 2022",
       x = "Year-Day",
       y = "Seropositivity") +
  theme(
#    legend.position = "right",             # Position the legend
    legend.key.height = unit(2, "cm"),     # Increase height between legend keys
#    legend.key.width = unit(2, "cm"),      # Adjust width between legend keys
 #   legend.spacing.y = unit(3, "cm")       # Increase vertical spacing between labels
  )+
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm"))
  




# Compare the distance of the Adjusted vs Unadjusted curves
# CBS vs CBS_BIAS_L
install.packages('dtw')
library(dtw)
time_index <- seq(min(df_time_cbs_sim$day),max(df_time_cbs_sim$day),1)
output_dtw <- matrix(ncol=2, nrow=6)
regpros <- c("BC","AB","MB","SK","ON","ATL")
for(j in 1:6){
    # MRP estimate A cbs
    filtering_condition <- which(df_time_cbs_sim$regpro == regpros[j])
    df_sub <- df_time_cbs_sim[filtering_condition,]
    ts1 <- df_sub[df_sub$source =='CBS' & df_sub$model == 'MR',]$mrp_mean
    ts2 <- df_sub[df_sub$source =='CBS_BIAS_L' & df_sub$model == 'MR',]$mrp_mean
    ts1_interp <- na.approx(ts1)
    ts2_interp <- na.approx(ts2)
    alignment <- dtw(ts1_interp, ts2_interp)
    output_dtw[j, 1] <- alignment$distance
    ts1 <- df_sub[df_sub$source =='CBS' & df_sub$model == 'MRP',]$mrp_mean
    ts2 <- df_sub[df_sub$source =='CBS_BIAS_L' & df_sub$model == 'MRP',]$mrp_mean
    ts1_interp <- na.approx(ts1)
    ts2_interp <- na.approx(ts2)
    alignment <- dtw(ts1_interp, ts2_interp)
    output_dtw[j, 2] <- alignment$distance
}
output_dtw

# CBS vs CBS_BIAS_H
for(j in 1:6){
  # MRP estimate A cbs
  filtering_condition <- which(df_time_cbs_sim$regpro == regpros[j])
  df_sub <- df_time_cbs_sim[filtering_condition,]
  ts1 <- df_sub[df_sub$source =='CBS' & df_sub$model == 'MR',]$mrp_mean
  ts2 <- df_sub[df_sub$source =='CBS_BIAS_H' & df_sub$model == 'MR',]$mrp_mean
  ts1_interp <- na.approx(ts1)
  ts2_interp <- na.approx(ts2)
  alignment <- dtw(ts1_interp, ts2_interp)
  output_dtw[j, 1] <- alignment$distance
  ts1 <- df_sub[df_sub$source =='CBS' & df_sub$model == 'MRP',]$mrp_mean
  ts2 <- df_sub[df_sub$source =='CBS_BIAS_H' & df_sub$model == 'MRP',]$mrp_mean
  ts1_interp <- na.approx(ts1)
  ts2_interp <- na.approx(ts2)
  alignment <- dtw(ts1_interp, ts2_interp)
  output_dtw[j, 2] <- alignment$distance
}
output_dtw

# CBS_BIAS_L vs CBS_BIAS_H
for(j in 1:6){
  # MRP estimate A cbs
  filtering_condition <- which(df_time_cbs_sim$regpro == regpros[j])
  df_sub <- df_time_cbs_sim[filtering_condition,]
  ts1 <- df_sub[df_sub$source =='CBS_BIAS_L' & df_sub$model == 'MR',]$mrp_mean
  ts2 <- df_sub[df_sub$source =='CBS_BIAS_H' & df_sub$model == 'MR',]$mrp_mean
  ts1_interp <- na.approx(ts1)
  ts2_interp <- na.approx(ts2)
  alignment <- dtw(ts1_interp, ts2_interp)
  output_dtw[j, 1] <- alignment$distance
  ts1 <- df_sub[df_sub$source =='CBS_BIAS_L' & df_sub$model == 'MRP',]$mrp_mean
  ts2 <- df_sub[df_sub$source =='CBS_BIAS_H' & df_sub$model == 'MRP',]$mrp_mean
  ts1_interp <- na.approx(ts1)
  ts2_interp <- na.approx(ts2)
  alignment <- dtw(ts1_interp, ts2_interp)
  output_dtw[j, 2] <- alignment$distance
}
output_dtw

#df_time_all_ccahs2
time_index <- seq(min(df_time_all_ccahs2$day),max(df_time_all_ccahs2$day),1)
output_dtw <- matrix(ncol=2, nrow=6)
for(j in 1:6){
  # MRP estimate A cbs
  filtering_condition <- which(df_time_all_ccahs2$regpro == regpros[j])
  df_sub <- df_time_all_ccahs2[filtering_condition,]
  ts1 <- df_sub[df_sub$source =='CBS' & df_sub$model == 'MR',]$mrp_mean
  ts2 <- df_sub[df_sub$source =='CCAHS' & df_sub$model == 'MR',]$mrp_mean
  ts1_interp <- na.approx(ts1)
  ts2_interp <- na.approx(ts2)
  alignment <- dtw(ts1_interp, ts2_interp)
  output_dtw[j, 1] <- alignment$distance
  ts1 <- df_sub[df_sub$source =='CBS' & df_sub$model == 'MRP',]$mrp_mean
  ts2 <- df_sub[df_sub$source =='CCAHS' & df_sub$model == 'MRP',]$mrp_mean
  ts1_interp <- na.approx(ts1)
  ts2_interp <- na.approx(ts2)
  alignment <- dtw(ts1_interp, ts2_interp)
  output_dtw[j, 2] <- alignment$distance
}
output_dtw

#df_time_all_abc3
time_index <- seq(min(df_time_all_abc3$day),max(df_time_all_abc3$day),1)
output_dtw <- matrix(ncol=2, nrow=6)
for(j in 1:6){
  # MRP estimate A cbs
  filtering_condition <- which(df_time_all_abc3$regpro == regpros[j])
  df_sub <- df_time_all_abc3[filtering_condition,]
  ts1 <- df_sub[df_sub$source =='CBS' & df_sub$model == 'MR',]$mrp_mean
  ts2 <- df_sub[df_sub$source =='AbC' & df_sub$model == 'MR',]$mrp_mean
  ts1_interp <- na.approx(ts1)
  ts2_interp <- na.approx(ts2)
  alignment <- dtw(ts1_interp, ts2_interp)
  output_dtw[j, 1] <- alignment$distance
  ts1 <- df_sub[df_sub$source =='CBS' & df_sub$model == 'MRP',]$mrp_mean
  ts2 <- df_sub[df_sub$source =='AbC' & df_sub$model == 'MRP',]$mrp_mean
  ts1_interp <- na.approx(ts1)
  ts2_interp <- na.approx(ts2)
  alignment <- dtw(ts1_interp, ts2_interp)
  output_dtw[j, 2] <- alignment$distance
}
output_dtw




df_time_cbs_sim <- rbind(df_time_cbs, df_time_cbs_bias_low, df_time_cbs_bias_high)

ggplot(df_time_cbs_sim[df_time_cbs_sim$source %in% c("CBS",'CBS_BIAS_H'),], aes(x = day, y = mrp_mean, color = source)) +
  geom_line(aes(linetype=model, color=source)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  labs(title = "Seropositivity Estimation during Omicron for biased sample, Apr-Aug 2022 (Higher)",
       x = "Year-Day",
       y = "Seropositivity") +
  theme_bw()

ggplot(df_time_cbs_sim[df_time_cbs_sim$regpro %in% c("AB",'ON','BC'),], aes(x = day, y = mrp_mean, color = source)) +
  geom_line(aes(linetype=model, color=source)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
  #ylim(0.15, 0.8) +
  labs(linetype = "Adjustment") +
  scale_fill_manual(
    name = "Dataset",
    values = c("CBS" = "#F8766D", 
               "CBS_BIAS_H" = "#00BA38" ,
               "CBS_BIAS_L" = "#619CFF"),
    labels = c("CBS" = "CBS", 
               "CBS_BIAS_H" = "CBS biased towards \n higher seroposotivity" ,
               "CBS_BIAS_L" = "CBS biased towards \n lower seroposotivity")) +
  scale_color_manual(
    name = "Dataset",
    values = c("CBS" = "#F8766D", 
               "CBS_BIAS_H" = "#00BA38" ,
               "CBS_BIAS_L" = "#619CFF"),
    labels = c("CBS" = "CBS", 
               "CBS_BIAS_H" = "CBS biased towards \n higher seroposotivity" ,
               "CBS_BIAS_L" = "CBS biased towards \n lower seroposotivity")) +
  facet_wrap(~ regpro) +
  labs(#title = "Seropositivity Estimation during Omicron for biased sample, Apr-Aug 2022",
    x = "Year-Day",
    y = "Seropositivity") +
  theme(
    #    legend.position = "right",             # Position the legend
    legend.key.height = unit(2, "cm"),     # Increase height between legend keys
    #    legend.key.width = unit(2, "cm"),      # Adjust width between legend keys
    #   legend.spacing.y = unit(3, "cm")       # Increase vertical spacing between labels
  )+
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm"))
