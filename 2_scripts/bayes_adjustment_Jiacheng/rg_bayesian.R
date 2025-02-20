## Bayesian Rogan-Gladen for Jiacheng's assay adjustment paper
## Wilson CI ####
CI_Wilson2 <- function(AP, Sp, Se, Splower, Selower, z.value, n1.backup, n1){
  if(AP==0){n1 <- n1.backup}
    n2 <- Sp*(1-Sp)*z.value^2/(Sp-Splower)^2
    n3 <- Se*(1-Se)*z.value^2/(Se-Selower)^2
    grid <- -4.04 + 0.05*(1:100)
    ngrid <- dnorm(grid)/12.5
    APval <- rep(0, 100)
    Spval <- rep(0, 100)
    Seval <- rep(0, 100)
  for(i in 1:50){
    center <- (AP+ 0.5*grid[i]^2/n1)/(1+grid[i]^2/n1)
    dev <- grid[i]*sqrt(AP*(1-AP)/n1+grid[i]^2/(4*n1^2))/
      (1+grid[i]^2/n1)
    APval[i] <- center + dev
    APval[101-i] <- center - dev
    center <- (Sp + 0.5*grid[i]^2/n2)/(1+grid[i]^2/n2)
    dev <- grid[i]*sqrt(Sp*(1-Sp)/n2+grid[i]^2/(4*n2^2))/
      (1+grid[i]^2/n2)
    Spval[i] <- center + dev
    Spval[101-i] <- center - dev
    center <- (Se + 0.5*grid[i]^2/n3)/(1+grid[i]^2/n3)
    dev <- grid[i]*sqrt(Se*(1-Se)/n3+grid[i]^2/(4*n3^2))/
      (1+grid[i]^2/n3)
    Seval[i] <- center + dev
    Seval[101-i] <- center - dev
  }  
  Pval <- rep(0, 1000000)
  probval <- rep(0, 1000000) 
  for(i in 1:100){
    for(j in 1:100){
      Pval[(100*i-99+(j-1)*10000):(100*i+(j-1)*10000)] <-
      (APval[i]+Spval[j]-1)/(Seval+Spval[j]-1)
      probval[(100*i-99+(j-1)*10000):(100*i+(j-1)*10000)] <-
      ngrid[i]*ngrid[j]*ngrid}}
  results <- data.frame(Pval=Pval, probval=probval)
  results <- results[order(results$Pval), ]
  results$probval <- cumsum(results$probval)
  cut1 <- which.min(abs(results$probval-0.025))
  cut2 <- which.min(abs(results$probval-0.975))
  if(results$probval[cut1]>0.025)cut1 <- cut1-1
  if(results$probval[cut2]<0.975)cut2 <- cut2+1
  return(c(results$Pval[cut1], results$Pval[cut2], (AP+(Sp-1)/Se+Sp-1)))
#  return(as.data.frame(t(c(k, results$Pval[cut1], results$Pval[cut2],
#                           (AP+(Sp-1)/Se+Sp-1)))))
}


RFD4682_e$Province = province_fun(RFD4682_e$PAT_FSA)
#df_apl <- df_apl %>% 
#  filter(Province != ("YT") & Province !=("NU/NT") & Province != ("QC")) #4178

RFD4682_e$age = RFD4682_e$AGE_AT_COLLECTION
RFD4682_e$age_group <- age_groups_fun(RFD4682_e$age)

RFD4682_e <- RFD4682_e[!is.na(RFD4682_e$GENDER),] 
RFD4682_e$Sex = with(RFD4682_e,ifelse(GENDER == "Male",1,0))

RFD4682_e <- RFD4682_e[!is.na(RFD4682_e$PAT_FSA),]
RFD4682_e$Urban <- with(RFD4682_e,ifelse(substr(PAT_FSA,start = 2,stop = 2) != "0",1,0))

RFD4682_e$region = factor(case_when(as.character(substr(RFD4682_e$PAT_FSA,1,1)) %in% c("A", "B", "C", "E")~"Atlantic", 
                                 as.character(substr(RFD4682_e$PAT_FSA,1,1)) %in% c("K", "L", "M", "N", "P")~"Ontario",
                                 as.character(substr(RFD4682_e$PAT_FSA,1,1)) %in% c("R", "S", "T")~"Prairies",
                                 as.character(substr(RFD4682_e$PAT_FSA,1,1)) %in% c("V")~"BC",
                                 TRUE~NA), levels = c("Atlantic","BC",  "Ontario", "Prairies"))

RFD4682_e$regpro = factor(case_when(as.character(substr(RFD4682_e$PAT_FSA,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                                 as.character(substr(RFD4682_e$PAT_FSA,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                                 as.character(substr(RFD4682_e$PAT_FSA,1,1)) %in% c("T")~"AB",
                                 as.character(substr(RFD4682_e$PAT_FSA,1,1)) %in% c("R")~"MB",
                                 as.character(substr(RFD4682_e$PAT_FSA,1,1)) %in% c("S")~"SK",
                                 as.character(substr(RFD4682_e$PAT_FSA,1,1)) %in% c("V")~"BC",
                                 TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK"))
RFD4682_e$regpro = 'AB'
RFD4682_e$regpro = factor(RFD4682_e$regpro)

RFD4682_e <- RFD4682_e[!is.na(RFD4682_e$'N-IgG_INTERP'),]
RFD4682_e$interp_roche_n <- with(RFD4682_e,ifelse(RFD4682_e$'N-IgG_INTERP' == "Positive",1,0))
library(lubridate)
RFD4682_e$day <- yday(RFD4682_e$COLLECTION_DATE)
RFD4682_e$week <- week(RFD4682_e$COLLECTION_DATE)

library(zoo)
RFD4682_e$yearmon <- zoo::as.yearmon(RFD4682_e$COLLECTION_DATE)
RFD4682_e <- RFD4682_e[!is.na(RFD4682_e$age_group),] #37009
library(tidyverse)
library(dplyr)
df_time_raw_apl <-
  RFD4682_e %>%
  group_by(regpro, yearmon) %>%
  dplyr::summarize(mrp_mean = mean(interp_roche_n), 'Total Analyzed Sample Size'=n(), 
                   n_seropos = sum(interp_roche_n), lab = "Abbott", ab_target = "N", seroprev_est = mean(interp_roche_n)) %>%
  drop_na()


df_cbs$yearmon <- zoo::as.yearmon(df_cbs$sampledate)


df_time_raw_cbs_alberta <-
  df_cbs %>%
  filter(regpro == "AB") %>%
  group_by(regpro, yearmon) %>%
  dplyr::summarize(mrp_mean = mean(interp_roche_n), 'Total Analyzed Sample Size'=n(), 
                   n_seropos = sum(interp_roche_n), lab = "Roche", ab_target = "N", seroprev_est = mean(interp_roche_n)) %>%
  drop_na()

#sens: 0.81[.66,.91]; spec:0.98[.89,1]
#sens: 0.927[0.902, 0.948]; spec:	0.999[0.994, 1] Abbott
#97.2 (95.4, 98.4)	99.8 (99.3, 100) Roche
sscrnls <- data.frame(
  lab = c("MAL", "MAL", "MAL", "ACG", "ACG", "ACG", "Abbott","Roche"),
  ab_target = c("S", "RBD", "N", "S", "RBD", "N", "N", "N"),
  sens = c(0.9163, 0.8507, 0.7109, 0.980, 0.968, 0.835, 0.927, 0.972),
  sens_lo95 = c(0.8891, 0.8167, 0.6493, 0.960, 0.943, 0.786, 0.902, 0.954),
  sens_hi95 = c(0.9412, 0.8824, 0.7678, 0.996, 0.988, 0.879, 0.948, 0.984),
  spec = c(0.8996, 0.8996, 0.9013, 0.989, 1.00, 0.964, 0.999, 0.998),
  spec_lo95 = c(0.873, 0.8709, 0.879, 0.980, 0.995, 0.946, 0.994, 0.993),
  spec_hi95 = c(0.9242, 0.9262, 0.9221, 0.996, 1.00, 0.978, 1,1)
) %>% 
  mutate(
    sens_simSampleSize = round(sens * (1 - sens) * 
                                 (1.96 / ((sens_hi95 - sens_lo95) / 2)^2), 0)
    # I think alpha, beta parameters not necessary if use 
    ## mean, concentration parameters
    #, sens_simpos = round(sens * sens_simN, 0),
    #sens_simneg = round((1 - sens) * sens_simN, 0)
    , spec_simSampleSize = ifelse(
      round(spec, 2) == 1.00, 
      round(0.995 * (1 - 0.995) * 
              (1.96 / ((spec_hi95 - spec_lo95) / 2)^2), 0),
      round(spec * (1 - spec) * 
              (1.96 / ((spec_hi95 - spec_lo95) / 2)^2), 0))
  )

lf3 <- df_time_raw_apl %>%
  left_join(sscrnls, 
            by = c("lab", "ab_target")) %>% 
  mutate(
    seroprev_est_RGadj = (seroprev_est + (spec - 1)) /
      (spec + (sens - 1))
  )

lf4 <- df_time_raw_cbs_alberta %>%
  left_join(sscrnls, 
            by = c("lab", "ab_target")) %>% 
  mutate(
    seroprev_est_RGadj = (seroprev_est + (spec - 1)) /
      (spec + (sens - 1))
  )

# R-JAGS interface
library(rjags)
library(runjags)
library(R2jags)
options(mc.cores = parallel::detectCores())
# Post-estimation helper packages
library(coda)
library(tidybayes)
library(bayesplot)

testjags()

jags_dat <- list(
  Ntotal = nrow(lf3),
  x = lf3$n_seropos, 
  n = lf3$`Total Analyzed Sample Size`, 
  pi_prior = c(1, 1),
  se_mu_prior = lf3$sens,
  se_conc_prior = lf3$sens_simSampleSize,
  sp_mu_prior = ifelse(lf3$spec == 1.000, 0.999, lf3$spec),
  sp_conc_prior = lf3$spec_simSampleSize)

jags_model_string <- function() {
  for(i in 1:Ntotal){
    pi[i] ~  dbeta(pi_prior[1], pi_prior[2])    # prevalence
    se[i] ~  dbeta(se_mu_prior[i] * se_conc_prior[i], 
                   (1 - se_mu_prior[i]) * se_conc_prior[i]) # sensitivity
    sp[i] ~  dbeta(sp_mu_prior[i] * sp_conc_prior[i], 
                   (1 - sp_mu_prior[i]) * sp_conc_prior[i])#; T(0.001,0.999) # specificity
    ap[i] <- pi[i]*se[i] + (1-pi[i])*(1-sp[i])              # apparent prevalence
    x[i]  ~  dbin(ap[i], n[i])                        # number of positive samples
  }
}

 # run model -------------
start_time <- Sys.time()
m1 <- jags.parallel( # 
  data = jags_dat, 
  model.file = jags_model_string, 
  #inits = jags_inits_function,
  parameters.to.save = c("ap", "pi", "se", "sp")
  , n.chains = 5, n.iter = 60000, n.burnin = 10000, n.thin = 100
)
end_time <- Sys.time()
end_time - start_time
m1
mcmc.m1 <- as.mcmc(m1)

# Specify the parameters you want to monitor
params <- c("ap[1]") # replace these with your actual parameter names

# Extract posterior samples
samples <- coda.samples(m1, variable.names = params, n.iter = 5000)

samples_array <- as.array(mcmc.m1)

# Convert `mcmc.list` to a data frame
samples_df <- as.data.frame(as.matrix(mcmc.m1))

jags_model_string <- function() {
  for(i in 1:Ntotal){
    x[i]  ~  dbin(ap[i], n[i])                        # number of positive samples
  }
}


quantile(samples_df[,1], probs = 0.025)
quantile(samples_df[,1], probs = 0.975)

#samples_df <- as.data.frame(as.matrix(mcmc.m1))[c("ap[1]", "ap[2]", "ap[3]","ap[4]","ap[5]","ap[6]","ap[7]","ap[8]","ap[9]","ap[10]")]

m1_df <- as.data.frame(as.matrix(mcmc.m1))
for (j in 1:Ntotal) {
  m1_df[,paste0("tp[",j,sep="]")]<-(m1_df[,paste0("ap[",j,sep="]")] + (m1_df[,paste0("sp[",j,sep="]")] - 1)) /
    (m1_df[,paste0("sp[",j,sep="]")] + (m1_df[,paste0("se[",j,sep="]")] - 1))
}

samples_df <- m1_df[,(ncol(m1_df)-Ntotal+1):ncol(m1_df)]

for (i in 1:Ntotal){
  df_time_raw_apl[i, 'rg_est'] <- lf3[i, 'seroprev_est_RGadj']
  df_time_raw_apl[i, 'rg_lower'] <- quantile(samples_df[,i], probs = 0.025)
  df_time_raw_apl[i, 'rg_upper'] <- quantile(samples_df[,i], probs = 0.975)
  df_time_raw_apl[i, 'rg_mean'] <- mean(samples_df[,i])
  a <- sapply(samples_df[,i], function(x) {sum(rbinom(n=df_time_raw_apl$`Total Analyzed Sample Size`[i], size=1, prob=x))/df_time_raw_apl$`Total Analyzed Sample Size`[i]})
  df_time_raw_apl[i, 'fppe_lower'] <- quantile(a, probs = 0.025)
  df_time_raw_apl[i, 'fppe_upper'] <- quantile(a, probs = 0.975)
  df_time_raw_apl[i, 'fppe_mean'] <- mean(a)
}

#df_time_raw_apl <- df_time_raw_apl[1:(length(df_time_raw_apl)-4)]

fppe <- function(x) {sum(rbinom(n=10, size=1, prob=x))/10}

a <- sapply(samples_df[,1], function(x) {sum(rbinom(n=10, size=1, prob=x))/10})


jags_dat <- list(
  Ntotal = nrow(lf4),
  x = lf4$n_seropos, 
  n = lf4$`Total Analyzed Sample Size`, 
  pi_prior = c(1, 1),
  se_mu_prior = lf4$sens,
  se_conc_prior = lf4$sens_simSampleSize,
  sp_mu_prior = ifelse(lf4$spec == 1.000, 0.999, lf4$spec),
  sp_conc_prior = lf4$spec_simSampleSize)

jags_model_string <- function() {
  for(i in 1:Ntotal){
    pi[i] ~  dbeta(pi_prior[1], pi_prior[2])    # prevalence
    se[i] ~  dbeta(se_mu_prior[i] * se_conc_prior[i], 
                   (1 - se_mu_prior[i]) * se_conc_prior[i]) # sensitivity
    sp[i] ~  dbeta(sp_mu_prior[i] * sp_conc_prior[i], 
                   (1 - sp_mu_prior[i]) * sp_conc_prior[i])#; T(0.001,0.999) # specificity
    ap[i] <- pi[i]*se[i] + (1-pi[i])*(1-sp[i])              # apparent prevalence
    x[i]  ~  dbin(ap[i], n[i])                        # number of positive samples
  }
}

# run model -------------
start_time <- Sys.time()
m3 <- jags.parallel( # 
  data = jags_dat, 
  model.file = jags_model_string, 
  #inits = jags_inits_function,
  parameters.to.save = c("ap", "pi", "se", "sp")
  , n.chains = 5, n.iter = 60000, n.burnin = 10000, n.thin = 100
)
end_time <- Sys.time()
end_time - start_time
m3
mcmc.m3 <- as.mcmc(m3)
# Convert `mcmc.list` to a data frame
samples_df <- as.data.frame(as.matrix(mcmc.m3))[c("ap[1]", "ap[2]", "ap[3]","ap[4]","ap[5]","ap[6]","ap[7]","ap[8]","ap[9]","ap[10]",
                                                  "ap[11]","ap[12]","ap[13]", "ap[14]","ap[15]","ap[16]","ap[17]","ap[18]","ap[19]" , 
                                                  "ap[20]","ap[21]","ap[22]")]

Ntotal <- nrow(lf4)
m3_df <- as.data.frame(as.matrix(mcmc.m3))
for (j in 1:Ntotal) {
  m3_df[,paste0("tp[",j,sep="]")]<-(m3_df[,paste0("ap[",j,sep="]")] + (m3_df[,paste0("sp[",j,sep="]")] - 1)) /
    (m3_df[,paste0("sp[",j,sep="]")] + (m3_df[,paste0("se[",j,sep="]")] - 1))
}

samples_df <- m3_df[,(ncol(m3_df)-Ntotal+1):ncol(m3_df)]

for (i in 1:Ntotal){
  df_time_raw_cbs_alberta[i, 'rg_est'] <- lf4[i, 'seroprev_est_RGadj']
  df_time_raw_cbs_alberta[i, 'rg_lower'] <- quantile(samples_df[,i], probs = 0.025)
  df_time_raw_cbs_alberta[i, 'rg_upper'] <- quantile(samples_df[,i], probs = 0.975)
  df_time_raw_cbs_alberta[i, 'rg_mean'] <- mean(samples_df[,i])
}

for (i in 1:Ntotal){
  df_time_raw_cbs_alberta[i, 'rg_est'] <- lf4[i, 'seroprev_est_RGadj']
  df_time_raw_cbs_alberta[i, 'rg_lower'] <- quantile(samples_df[,i], probs = 0.025)
  df_time_raw_cbs_alberta[i, 'rg_upper'] <- quantile(samples_df[,i], probs = 0.975)
  df_time_raw_cbs_alberta[i, 'rg_mean'] <- mean(samples_df[,i])
  a <- sapply(samples_df[,i], function(x) {sum(rbinom(n=df_time_raw_cbs_alberta$`Total Analyzed Sample Size`[i], size=1, prob=x))/df_time_raw_cbs_alberta$`Total Analyzed Sample Size`[i]})
  df_time_raw_cbs_alberta[i, 'fppe_lower'] <- quantile(a, probs = 0.025)
  df_time_raw_cbs_alberta[i, 'fppe_upper'] <- quantile(a, probs = 0.975)
  df_time_raw_cbs_alberta[i, 'fppe_mean'] <- mean(a)
}


df_time_raw_apl$source <- "APL"
df_time_raw_cbs_alberta$source <- "CBS"
df_time_raw_canpath_alberta$source <- "CanPath"

df_time_rg <- rbind(df_time_raw_apl, df_time_raw_cbs_alberta, df_time_raw_canpath_alberta)
df_time_rg <- df_time_raw_all
ggplot(df_time_rg_all, aes(x = yearmon, y = rg_mean, color = source)) +
#  geom_point(data = df_time_rg, aes(x=yearmon, y=seroprev_est), position=pd_1, size = 1) +
  geom_point(aes(shape = model)) + #, position= position_dodge(width = 0.1)) +
  geom_line(aes(linetype=model, color=source)) +
  geom_errorbar(aes(ymin=rg_lower, ymax=rg_upper), width = 0.02) + 
  geom_errorbar(data = df_time_rg_all[df_time_rg_all$model == 'RG_fppe', ], aes(ymin=rg_lower, ymax=rg_upper), width = 0.02) + 
#  geom_point(data = df_time_rg, aes(x=yearmon, y=seroprev_est), size = 1) +
#  geom_line(data = df_time_rg, aes(x=yearmon, y=seroprev_est, color = source)) +
#  geom_smooth(aes(color=source),method = "loess", se = FALSE) +
#  geom_ribbon(data= df_time_rg, aes(ymin = rg_lower, ymax = rg_upper, fill = source), alpha = 0.5, colour = NA) +
  labs(title = "Seropositivity Estimation",
       x = "Year_month",
       y = "Seropositivity") +
  theme_bw() +
  scale_color_manual(values = c("#1CE37B","#00BFC4","#F8766D"))


df_time_rg_1 <- df_time_rg
df_time_rg_1[] <- NA
df_time_rg_1$region <- df_time_rg$region
df_time_rg_1$yearmon <- df_time_rg$yearmon
df_time_rg_1$rg_mean <- df_time_rg$seroprev_est
df_time_rg_1$source <- df_time_rg$source
df_time_rg_1$model <- 'RAW'

df_time_rg_2 <- df_time_rg
df_time_rg_2$model <- 'RG_Bayes'

df_time_rg_3 <- df_time_rg
df_time_rg_3[] <- NA
df_time_rg_3$yearmon <- df_time_rg$yearmon
df_time_rg_3$rg_mean <- df_time_rg$rg_est
df_time_rg_3$source <- df_time_rg$source
df_time_rg_3$model <- 'RG_point'

df_time_rg_4 <- df_time_rg
df_time_rg_4[] <- NA
df_time_rg_4$region <- df_time_rg$region
df_time_rg_4$yearmon <- df_time_rg$yearmon
df_time_rg_4$rg_mean <- df_time_rg$fppe_mean
df_time_rg_4$rg_lower <- df_time_rg$fppe_lower
df_time_rg_4$rg_upper <- df_time_rg$fppe_upper
df_time_rg_4$source <- df_time_rg$source
df_time_rg_4$model <- 'RG_fppe'


df_time_rg_all <- rbind(df_time_rg_1, df_time_rg_2, df_time_rg_3, df_time_rg_4)
df_time_rg_all <- rbind(df_time_rg_1,  df_time_rg_4)

df_time_rg_2$rg_est - df_time_rg_2$seroprev_est


CI_Wilson2 <- function(AP, Sp, Se, Splower, Selower, z.value, n1_backup)

CI_Wilson2(lf3$seroprev_est[1], lf3$spec[1], lf3$sens, lf3$spec_lo95, lf3$sens_lo95, 1.96, 10, 398)




for (j in 1:3) {
  df[,paste0("MktCap",j,sep="")]<-df$Shares*df[,paste0("Price",j,sep="")]
}

for (j in 1:3) {
 print(paste0("ap[",j,sep="]"))
}

m1_df <- as.data.frame(as.matrix(mcmc.m1))
for (j in 1:Ntotal) {
  m1_df[,paste0("tp[",j,sep="]")]<-(m1_df[,paste0("ap[",j,sep="]")] + (m1_df[,paste0("sp[",j,sep="]")] - 1)) /
    (m1_df[,paste0("sp[",j,sep="]")] + (m1_df[,paste0("se[",j,sep="]")] - 1))
}


##### Canpath ########

canpaths <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/1_data/private/CANPATH/Canpaths.csv')

canpaths$province = province_fun(canpaths$fsa)
canpaths$age_group <- age_groups_fun(canpaths$age)

#canpaths$sex

canpaths <- canpaths[!is.na(canpaths$province),]
canpaths <- canpaths[canpaths$province !='QC',]
canpaths$Urban <- with(canpaths,ifelse(substr(fsa,start = 2,stop = 2) != "0",1,0))

canpaths$region = factor(case_when(as.character(substr(canpaths$fsa,1,1)) %in% c("A", "B", "C", "E")~"Atlantic", 
                                    as.character(substr(canpaths$fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"Ontario",
                                    as.character(substr(canpaths$fsa,1,1)) %in% c("R", "S", "T")~"Prairies",
                                    as.character(substr(canpaths$fsa,1,1)) %in% c("V")~"BC",
                                    TRUE~NA), levels = c("Atlantic","BC",  "Ontario", "Prairies"))

canpaths$regpro = factor(case_when(as.character(substr(canpaths$fsa,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                                    as.character(substr(canpaths$fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                                    as.character(substr(canpaths$fsa,1,1)) %in% c("T")~"AB",
                                    as.character(substr(canpaths$fsa,1,1)) %in% c("R")~"MB",
                                    as.character(substr(canpaths$fsa,1,1)) %in% c("S")~"SK",
                                    as.character(substr(canpaths$fsa,1,1)) %in% c("V")~"BC",
                                    TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK"))

canpaths$region1 = factor(case_when(as.character(substr(canpaths$fsa,1,1)) %in% c("A", "B", "C", "E")~"Atlantic", 
                                   as.character(substr(canpaths$fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"Ontario",
                                   as.character(substr(canpaths$fsa,1,1)) %in% c("R", "S")~"SK & MB",
                                   as.character(substr(canpaths$fsa,1,1)) %in% c("T")~"Ablerta",
                                   as.character(substr(canpaths$fsa,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("Atlantic","BC",  "Ontario", "SK & MB", "Alberta"))

table(canpaths$region)
canpaths <- canpaths[!is.na(canpaths$region),]
canpaths <- canpaths[!as.character(substr(canpaths$fsa,1,1)) %in% c("S"),]

canpaths$regpro = factor(case_when(canpaths$region %in% c("Atlantic")~"ATL", 
                                   canpaths$region %in% c("Ontario")~"ON",
                                   canpaths$region %in% c("Alberta")~"AB",
                                   canpaths$region %in% c("SK & MB")~"MB",
                                   canpaths$region %in% c("BC")~"BC",
                                   TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB"))

canpaths <- canpaths[canpaths$pos != 3,]
canpaths$interp_roche_n <- with(canpaths,ifelse(canpaths$pos == 1,1,0))
library(lubridate)
library(zoo)
canpaths$sampledate <- as.Date(canpaths$date_col)
canpaths$Month <- floor_date(canpaths$sampledate, unit = "month")
canpaths$yearmon <- zoo::as.yearmon(as.Date(canpaths$date_col))
canpaths <- canpaths[!is.na(canpaths$yearmon),]

## Drop 9 samples tested by Gingras Lab in Alberta and 2 samples tested by Abbott in BC
dim(canpaths)
canpaths <- canpaths[canpaths$region == "Alberta" & canpaths$assay_id == '20010-03' |
                     canpaths$region != "Alberta" & canpaths$assay_id == '10010-00', ]

library(tidyverse)
library(dplyr)
df_time_raw_canpath <-
  canpaths %>%
#  filter(region == "Alberta", assay_id == '20010-03')  %>% #3052
  mutate(lab = if_else(region == "Alberta", "Abbott", "ACG"), ab_target = "N")  %>%
  group_by(region, yearmon, lab, ab_target) %>%
  dplyr::summarize(mrp_mean = mean(interp_roche_n), 'Total Analyzed Sample Size'=n(), 
                   n_seropos = sum(interp_roche_n), seroprev_est = mean(interp_roche_n)) %>%
  drop_na()



lf5 <- df_time_raw_canpath_alberta %>%
  left_join(sscrnls, 
            by = c("lab", "ab_target")) %>% 
  mutate(
    seroprev_est_RGadj = (seroprev_est + (spec - 1)) /
      (spec + (sens - 1))
  )

# R-JAGS interface
library(rjags)
library(runjags)
library(R2jags)
options(mc.cores = parallel::detectCores())
# Post-estimation helper packages
library(coda)
library(tidybayes)
library(bayesplot)

testjags()

jags_dat <- list(
  Ntotal = nrow(lf5),
  x = lf5$n_seropos, 
  n = lf5$`Total Analyzed Sample Size`, 
  pi_prior = c(1, 1),
  se_mu_prior = lf5$sens,
  se_conc_prior = lf5$sens_simSampleSize,
  sp_mu_prior = ifelse(lf5$spec == 1.000, 0.999, lf5$spec),
  sp_conc_prior = lf5$spec_simSampleSize)

jags_model_string <- function() {
  for(i in 1:Ntotal){
    pi[i] ~  dbeta(pi_prior[1], pi_prior[2])    # prevalence
    se[i] ~  dbeta(se_mu_prior[i] * se_conc_prior[i], 
                   (1 - se_mu_prior[i]) * se_conc_prior[i]) # sensitivity
    sp[i] ~  dbeta(sp_mu_prior[i] * sp_conc_prior[i], 
                   (1 - sp_mu_prior[i]) * sp_conc_prior[i])#; T(0.001,0.999) # specificity
    ap[i] <- pi[i]*se[i] + (1-pi[i])*(1-sp[i])              # apparent prevalence
    x[i]  ~  dbin(ap[i], n[i])                        # number of positive samples
  }
}

# run model -------------
start_time <- Sys.time()
m5 <- jags.parallel( # 
  data = jags_dat, 
  model.file = jags_model_string, 
  #inits = jags_inits_function,
  parameters.to.save = c("ap", "pi", "se", "sp")
  , n.chains = 5, n.iter = 60000, n.burnin = 10000, n.thin = 100
)
end_time <- Sys.time()
end_time - start_time
m5
#mcmc.m5 <- as.mcmc(m5)

m5_df <- as.data.frame(as.matrix(as.mcmc(m5)))
for (j in 1:Ntotal) {
  m5_df[,paste0("tp[",j,sep="]")]<-(m5_df[,paste0("ap[",j,sep="]")] + (m5_df[,paste0("sp[",j,sep="]")] - 1)) /
    (m5_df[,paste0("sp[",j,sep="]")] + (m5_df[,paste0("se[",j,sep="]")] - 1))
}

samples_df <- m5_df[,(ncol(m5_df)-Ntotal+1):ncol(m5_df)]

for (i in 1:Ntotal){
  df_time_raw_canpath_alberta[i, 'rg_est'] <- lf5[i, 'seroprev_est_RGadj']
  df_time_raw_canpath_alberta[i, 'rg_lower'] <- quantile(samples_df[,i], probs = 0.025)
  df_time_raw_canpath_alberta[i, 'rg_upper'] <- quantile(samples_df[,i], probs = 0.975)
  df_time_raw_canpath_alberta[i, 'rg_mean'] <- mean(samples_df[,i])
  a <- sapply(samples_df[,i], function(x) {sum(rbinom(n=df_time_raw_apl$`Total Analyzed Sample Size`[i], size=1, prob=x))/df_time_raw_apl$`Total Analyzed Sample Size`[i]})
  df_time_raw_canpath_alberta[i, 'fppe_lower'] <- quantile(a, probs = 0.025)
  df_time_raw_canpath_alberta[i, 'fppe_upper'] <- quantile(a, probs = 0.975)
  df_time_raw_canpath_alberta[i, 'fppe_mean'] <- mean(a)
}


View(canpaths %>% 
     group_by(yearmon) %>% 
     summarize(n=n()))


### APL ###


### CanPath 2020 - 2021 ###
df_time_raw_canpath <-
  canpaths %>%
  #  filter(region == "Alberta", assay_id == '20010-03')  %>% #3052
  mutate(lab = if_else(region == "Alberta", "Abbott", "ACG"), ab_target = "N")  %>%
  group_by(region, yearmon, lab, ab_target) %>%
  dplyr::summarize(mrp_mean = mean(interp_roche_n), 'Total Analyzed Sample Size'=n(), 
                   n_seropos = sum(interp_roche_n), seroprev_est = mean(interp_roche_n)) %>%
  drop_na()


lf6 <- df_time_raw_canpath %>%
  left_join(sscrnls, 
            by = c("lab", "ab_target")) %>% 
  mutate(
    seroprev_est_RGadj = (seroprev_est + (spec - 1)) /
      (spec + (sens - 1))
  )

Ntotal = nrow(lf6)

# R-JAGS interface
library(rjags)
library(runjags)
library(R2jags)
options(mc.cores = parallel::detectCores())
# Post-estimation helper packages
library(coda)
library(tidybayes)
library(bayesplot)

testjags()

jags_dat <- list(
  Ntotal = nrow(lf6),
  x = lf6$n_seropos, 
  n = lf6$`Total Analyzed Sample Size`, 
  pi_prior = c(1, 1),
  se_mu_prior = lf6$sens,
  se_conc_prior = lf6$sens_simSampleSize,
  sp_mu_prior = ifelse(lf6$spec == 1.000, 0.999, lf6$spec),
  sp_conc_prior = lf6$spec_simSampleSize)

jags_model_string <- function() {
  for(i in 1:Ntotal){
    pi[i] ~  dbeta(pi_prior[1], pi_prior[2])    # prevalence
    se[i] ~  dbeta(se_mu_prior[i] * se_conc_prior[i], 
                   (1 - se_mu_prior[i]) * se_conc_prior[i]) # sensitivity
    sp[i] ~  dbeta(sp_mu_prior[i] * sp_conc_prior[i], 
                   (1 - sp_mu_prior[i]) * sp_conc_prior[i])#; T(0.001,0.999) # specificity
    ap[i] <- pi[i]*se[i] + (1-pi[i])*(1-sp[i])              # apparent prevalence
    x[i]  ~  dbin(ap[i], n[i])                        # number of positive samples
  }
}

# run model -------------
start_time <- Sys.time()
m6 <- jags.parallel( # 
  data = jags_dat, 
  model.file = jags_model_string, 
  #inits = jags_inits_function,
  parameters.to.save = c("ap", "pi", "se", "sp")
  , n.chains = 5, n.iter = 60000, n.burnin = 10000, n.thin = 100
)
end_time <- Sys.time()
end_time - start_time
m6
#mcmc.m5 <- as.mcmc(m5)

m6_df <- as.data.frame(as.matrix(as.mcmc(m6)))
for (j in 1:Ntotal) {
  m6_df[,paste0("tp[",j,sep="]")]<-(m6_df[,paste0("ap[",j,sep="]")] + (m6_df[,paste0("sp[",j,sep="]")] - 1)) /
    (m6_df[,paste0("sp[",j,sep="]")] + (m6_df[,paste0("se[",j,sep="]")] - 1))
}

samples_df <- m6_df[,(ncol(m6_df)-Ntotal+1):ncol(m6_df)]

for (i in 1:Ntotal){
  df_time_raw_canpath[i, 'rg_est'] <- lf6[i, 'seroprev_est_RGadj']
  df_time_raw_canpath[i, 'rg_lower'] <- quantile(samples_df[,i], probs = 0.025)
  df_time_raw_canpath[i, 'rg_upper'] <- quantile(samples_df[,i], probs = 0.975)
  df_time_raw_canpath[i, 'rg_mean'] <- mean(samples_df[,i])
  a <- sapply(samples_df[,i], function(x) {sum(rbinom(n=df_time_raw_canpath$`Total Analyzed Sample Size`[i], size=1, prob=x))/df_time_raw_canpath$`Total Analyzed Sample Size`[i]})
  df_time_raw_canpath[i, 'fppe_lower'] <- quantile(a, probs = 0.025)
  df_time_raw_canpath[i, 'fppe_upper'] <- quantile(a, probs = 0.975)
  df_time_raw_canpath[i, 'fppe_mean'] <- mean(a)
}

df_time_raw_canpath$source <- "CanPath"
sum(df_time_raw_canpath$`Total Analyzed Sample Size`)

###########################################################
### CBS Abbott Sep-Nov 2020 + CBS Roche Dec 2020 - 2022 ###

# df_time_raw_cbs_all <-
#   data_all %>%
#   #  filter(region == "Alberta", assay_id == '20010-03')  %>% #3052
#   mutate(region = factor(case_when(as.character(substr(fsa,1,1)) %in% c("A", "B", "C", "E")~"Atlantic", 
#                               as.character(substr(fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"Ontario",
#                               as.character(substr(fsa,1,1)) %in% c("R", "S")~"SK & MB",
#                               as.character(substr(fsa,1,1)) %in% c("T")~"Ablerta",
#                               as.character(substr(fsa,1,1)) %in% c("V")~"BC",
#                               TRUE~NA), levels = c("Atlantic","BC",  "Ontario", "SK & MB", "Alberta")),
#          yearmon = zoo::as.yearmon(sampledate), 
#          lab = if_else(sampledate <= "2020-05-09", "Abbott", "ACG"), ab_target = "N")  %>%
#   group_by(region, yearmon, lab, ab_target) %>%
#   dplyr::summarize(mrp_mean = mean(interp_roche_n), 'Total Analyzed Sample Size'=n(), 
#                    n_seropos = sum(interp_roche_n), seroprev_est = mean(interp_roche_n)) %>%
#   drop_na()
# 
# data_all_abbott <-  data_all[!is.na(data_all$abbott_n) & !is.na(data_all$interp_abbott_n), ]
# max(data_all_abbott$sample)

df_time_raw_cbs_all_abb <-
  data_all_abbott %>%
  filter(sampledate < '2020-12-01') %>%
  mutate(region = factor(case_when(as.character(substr(fsa,1,1)) %in% c("A", "B", "C", "E")~"Atlantic", 
                                             as.character(substr(fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"Ontario",
                                             as.character(substr(fsa,1,1)) %in% c("R", "S")~"SK & MB",
                                             as.character(substr(fsa,1,1)) %in% c("T")~"Ablerta",
                                             as.character(substr(fsa,1,1)) %in% c("V")~"BC",
                                             TRUE~NA), levels = c("Atlantic","BC",  "Ontario", "SK & MB", "Alberta")),
         yearmon = zoo::as.yearmon(sampledate), 
         interp_abbott_n = case_when(interp_abbott_n == 'Negative' ~ 0, 
                                    interp_abbott_n == 'Positive' ~ 1,
                                    TRUE~NA),
         lab = "Abbott", ab_target = "N")  %>%
  group_by(region, yearmon, lab, ab_target) %>%
  dplyr::summarize(mrp_mean = mean(interp_abbott_n), 'Total Analyzed Sample Size'=n(), 
                   n_seropos = sum(interp_abbott_n), seroprev_est = mean(interp_abbott_n)) %>%
  drop_na()


df_time_raw_cbs_all <-
  data_all %>%
  filter(sampledate >= '2020-12-14',
         !is.na(interp_roche_n)) %>%
  mutate(region = factor(case_when(as.character(substr(fsa,1,1)) %in% c("A", "B", "C", "E")~"Atlantic", 
                                   as.character(substr(fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"Ontario",
                                   as.character(substr(fsa,1,1)) %in% c("R", "S")~"SK & MB",
                                   as.character(substr(fsa,1,1)) %in% c("T")~"Alberta",
                                   as.character(substr(fsa,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("Atlantic","BC",  "Ontario", "SK & MB", "Alberta")),
         yearmon = zoo::as.yearmon(sampledate), 
#         interp_abbott_n = case_when(interp_abbott_n == 'Negative' ~ 0, 
#                                     interp_abbott_n == 'Positive' ~ 1,
#                                     TRUE~NA),
         lab = "Roche", ab_target = "N")  %>%
  group_by(region, yearmon, lab, ab_target) %>%
  dplyr::summarize(mrp_mean = mean(interp_roche_n), 'Total Analyzed Sample Size'=n(), 
                   n_seropos = sum(interp_roche_n), seroprev_est = mean(interp_roche_n)) %>%
  drop_na()

df_time_raw_cbs_all <- rbind(df_time_raw_cbs_all_abb, df_time_raw_cbs_all)

df_time_raw_cbs_all <- df_time_raw_cbs_all[order(df_time_raw_cbs_all$region, df_time_raw_cbs_all$yearmon),]

lf7 <- df_time_raw_cbs_all %>%
  left_join(sscrnls, 
            by = c("lab", "ab_target")) %>% 
  mutate(
    seroprev_est_RGadj = (seroprev_est + (spec - 1)) /
      (spec + (sens - 1))
  )

Ntotal = nrow(lf7)

# R-JAGS interface
library(rjags)
library(runjags)
library(R2jags)
options(mc.cores = parallel::detectCores())
# Post-estimation helper packages
library(coda)
library(tidybayes)
library(bayesplot)

testjags()

jags_dat <- list(
  Ntotal = nrow(lf7),
  x = lf7$n_seropos, 
  n = lf7$`Total Analyzed Sample Size`, 
  pi_prior = c(1, 1),
  se_mu_prior = lf7$sens,
  se_conc_prior = lf7$sens_simSampleSize,
  sp_mu_prior = ifelse(lf7$spec == 1.000, 0.999, lf7$spec),
  sp_conc_prior = lf7$spec_simSampleSize)

jags_model_string <- function() {
  for(i in 1:Ntotal){
    pi[i] ~  dbeta(pi_prior[1], pi_prior[2])    # prevalence
    se[i] ~  dbeta(se_mu_prior[i] * se_conc_prior[i], 
                   (1 - se_mu_prior[i]) * se_conc_prior[i]) # sensitivity
    sp[i] ~  dbeta(sp_mu_prior[i] * sp_conc_prior[i], 
                   (1 - sp_mu_prior[i]) * sp_conc_prior[i])#; T(0.001,0.999) # specificity
    ap[i] <- pi[i]*se[i] + (1-pi[i])*(1-sp[i])              # apparent prevalence
    x[i]  ~  dbin(ap[i], n[i])                        # number of positive samples
  }
}

# run model -------------
start_time <- Sys.time()
m7 <- jags.parallel( # 
  data = jags_dat, 
  model.file = jags_model_string, 
  #inits = jags_inits_function,
  parameters.to.save = c("ap", "pi", "se", "sp")
  , n.chains = 5, n.iter = 60000, n.burnin = 10000, n.thin = 100
)
end_time <- Sys.time()
end_time - start_time
m7
#mcmc.m5 <- as.mcmc(m5)

m7_df <- as.data.frame(as.matrix(as.mcmc(m7)))
for (j in 1:Ntotal) {
  m7_df[,paste0("tp[",j,sep="]")]<-(m7_df[,paste0("ap[",j,sep="]")] + (m7_df[,paste0("sp[",j,sep="]")] - 1)) /
    (m7_df[,paste0("sp[",j,sep="]")] + (m7_df[,paste0("se[",j,sep="]")] - 1))
}

samples_df <- m7_df[,(ncol(m7_df)-Ntotal+1):ncol(m7_df)]

for (i in 1:Ntotal){
  df_time_raw_cbs_all[i, 'rg_est'] <- lf7[i, 'seroprev_est_RGadj']
  df_time_raw_cbs_all[i, 'rg_lower'] <- quantile(samples_df[,i], probs = 0.025)
  df_time_raw_cbs_all[i, 'rg_upper'] <- quantile(samples_df[,i], probs = 0.975)
  df_time_raw_cbs_all[i, 'rg_mean'] <- mean(samples_df[,i])
  a <- sapply(samples_df[,i], function(x) {sum(rbinom(n=df_time_raw_cbs_all$`Total Analyzed Sample Size`[i], size=1, prob=x))/df_time_raw_cbs_all$`Total Analyzed Sample Size`[i]})
  df_time_raw_cbs_all[i, 'fppe_lower'] <- quantile(a, probs = 0.025)
  df_time_raw_cbs_all[i, 'fppe_upper'] <- quantile(a, probs = 0.975)
  df_time_raw_cbs_all[i, 'fppe_mean'] <- mean(a)
}

df_time_raw_cbs_all$source <- "CBS"
sum(df_time_raw_cbs_all$`Total Analyzed Sample Size`)

df_time_raw_apl$region <- 'Alberta'
df_time_raw_apl <- df_time_raw_apl[,colnames(df_time_raw_canpath)]

colnames(df_time_raw_apl[,colnames(df_time_raw_canpath)])
#options(max.print=1000000)

df_time_raw_all <- rbind(df_time_raw_cbs_all, df_time_raw_canpath, df_time_raw_apl)


ggplot(df_time_rg_all, aes(x = yearmon, y = rg_mean, color = source)) +
  #  geom_point(data = df_time_rg, aes(x=yearmon, y=seroprev_est), position=pd_1, size = 1) +
  geom_point(aes(shape = model)) + #, position= position_dodge(width = 0.1)) +
  geom_line(aes(linetype=model, color=source)) +
  geom_errorbar(aes(ymin=rg_lower, ymax=rg_upper), width = 0.02) + 
  geom_errorbar(data = df_time_rg_all[df_time_rg_all$model == 'RG_fppe', ], aes(ymin=rg_lower, ymax=rg_upper), width = 0.02) + 
  #  geom_point(data = df_time_rg, aes(x=yearmon, y=seroprev_est), size = 1) +
  #  geom_line(data = df_time_rg, aes(x=yearmon, y=seroprev_est, color = source)) +
  #  geom_smooth(aes(color=source),method = "loess", se = FALSE) +
  #  geom_ribbon(data= df_time_rg, aes(ymin = rg_lower, ymax = rg_upper, fill = source), alpha = 0.5, colour = NA) +
  facet_wrap(~ region) +
  labs(title = "Seropositivity Estimation",
       x = "Year_month",
       y = "Seropositivity") +
  theme_bw() +
  scale_color_manual(values = c("#1CE37B","#00BFC4","#F8766D"))

df_time_rg_all <- df_time_rg_all[c("region","yearmon","rg_lower", "rg_upper","rg_mean", "source","model")]

write.csv(df_time_rg_all,df_time_rg_all)
write.csv(df_time_rg_all, "/home/yuanyu/projects/covid-donor-v-survey-antibody/data_all.csv")

