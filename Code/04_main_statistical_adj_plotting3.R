#Fig 2 & 4, Fig S1-S5
library(tidyverse)

# Age
age_sample <- cces_df %>% mutate(age = factor(age, ordered = FALSE)) %>% group_by(age) %>% summarise(n = n()) %>% mutate(Sample = n/sum(n))
age_post <- poststrat_df %>% mutate(age = factor(age, ordered = FALSE)) %>% group_by(age) %>% summarise(n_post = sum(n)) %>% mutate(Population = n_post/sum(n_post))
age <- inner_join(age_sample, age_post, by = "age") %>% select(age, Sample, Population)
age_plot <- ggplot() + 
  ylab("") + xlab("Proportion") + theme_bw() + coord_flip()  + 
  geom_dumbbell(data = age, aes(y = age, x = Sample, xend = Population)) +
  geom_point(data = melt(age, id = "age"), aes(y = age, x = value, color = variable), size = 2) +
  scale_x_continuous(limits = c(0, 0.35), breaks = c(0, .1, .2, .3)) + theme(legend.position = "none") + ggtitle("Age")

# Gender
male_sample <- cces_df %>% group_by(male) %>% summarise(n = n()) %>% mutate(Sample = n/sum(n))
male_post <- poststrat_df %>% group_by(male) %>% summarise(n_post = sum(n)) %>% mutate(Population = n_post/sum(n_post))
male <- inner_join(male_sample, male_post, by = "male") %>% select(male, Sample, Population) %>% 
  mutate(male = factor(male, levels = c(-0.5, 0.5), labels = c("Female", "Male")))
male_plot <- ggplot() + 
  ylab("") + xlab("") + theme_bw() + coord_flip()  + 
  geom_dumbbell(data = male, aes(y = male, x = Sample, xend = Population)) +
  geom_point(data = melt(male, id = "male"), aes(y = male, x = value, color = variable), size = 2) +
  scale_x_continuous(limits = c(0, 0.6), breaks = c(0, .2, .4, .6)) + theme(legend.position = "none") + ggtitle("Gender")

# Ethnicity
ethnicity_sample <- cces_df %>% group_by(eth) %>% summarise(n = n()) %>% mutate(Sample = n/sum(n))
ethnicity_post <- poststrat_df %>% group_by(eth) %>% summarise(n_post = sum(n)) %>% mutate(Population = n_post/sum(n_post))
ethnicity <- inner_join(ethnicity_sample, ethnicity_post, by = "eth") %>% select(eth, Sample, Population)
ethnicity$eth <- factor(ethnicity$eth, 
                        levels = c("Black", "Hispanic", "Other", "White"),
                        labels = c("Black", "Hispanic", "Other", "White"))
ethnicity_plot <- ggplot() + 
  ylab("") + xlab("") + theme_bw() + coord_flip()  + 
  geom_dumbbell(data = ethnicity, aes(y = eth, x = Sample, xend = Population)) +
  geom_point(data = melt(ethnicity, id = "eth"), aes(y = eth, x = value, color = variable), size = 2) +
  scale_x_continuous(limits = c(0, 0.8), breaks = c(0, .2, .4, .6, 0.8)) + theme(legend.position = "none") + ggtitle("Ethnicity")

# Education
educ_sample <- cces_df %>% mutate(educ = factor(educ, ordered = FALSE)) %>% group_by(educ) %>% summarise(n = n()) %>% mutate(Sample = n/sum(n))
educ_post <- poststrat_df %>% mutate(educ = factor(educ, ordered = FALSE)) %>% group_by(educ) %>% summarise(n_post = sum(n)) %>% mutate(Population = n_post/sum(n_post))
educ <- inner_join(educ_sample, educ_post, by = "educ") %>% select(educ, Sample, Population)
educ$educ <- factor(educ$educ, 
                    levels = c("No HS", "HS", "Some college", "4-Year College", "Post-grad"),
                    labels = c("No HS", "HS", "Some\nCollege", "4-year\nCollege", "Post-grad"))
educ_plot <- ggplot() + 
  ylab("") + xlab("") + theme_bw() + coord_flip()  + 
  geom_dumbbell(data = educ, aes(y = educ, x = Sample, xend = Population)) +
  geom_point(data = melt(educ, id = "educ"), aes(y = educ, x = value, color = variable), size = 2) +
  scale_x_continuous(limits = c(0, 0.33), breaks = c(0, .1, .2, .3)) + theme(legend.position = "none") + ggtitle("Education")

grid.arrange(age_plot, male_plot, ethnicity_plot, educ_plot,
             widths = c(1.5, 0.75, 1.5, 1.5))



state_sample <- cces_df %>% group_by(state) %>% summarise(n = n()) %>% mutate(Sample = n/sum(n))
state_post <- poststrat_df %>% group_by(state) %>% summarise(n_post = sum(n)) %>% mutate(Population = n_post/sum(n_post))
state <- left_join(state_sample, state_post, by = "state") %>% select(state, Sample, Population) %>% left_join(statelevel_predictors_df, by = "state")
states_order <- state$repvote
state$state <- fct_reorder(state$state, states_order)
state <- state %>% select(state, Sample, Population)

ggplot() + 
  ylab("") + xlab("Proportion") + theme_bw() + coord_flip()  + 
  geom_dumbbell(data = state, aes(y = state, x = Sample, xend = Population)) +
  geom_point(data = melt(state, id = "state"), aes(y = state, x = value, color = variable), size = 2) +
  scale_x_continuous(limits = c(0, 0.13), breaks = c(0, .025, .05, .075, .1, .125)) + ggtitle("State") + 
  theme(legend.position = "bottom", legend.title=element_blank())


# a function called get_se_bernoulli to obtain the SE based on p and n
get_se_bernoulli <- function(p, n){
  return(sqrt(p*(1-p)/n))
}
# National
poststrat_cbs <-
  census_race %>%
  group_by(regpro,age_group,Sex,Race,QuintMat,QuintSoc) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()
poststrat_cbs$composite_strata <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$Race, 
                                        poststrat_cbs$QuintMat, poststrat_cbs$QuintSoc, sep = "_")

poststrat_df <- poststrat_cbs
epred_mat <- posterior_epred(fit2, newdata = poststrat_df, draws = 4000)
mrp_estimates_vector <- epred_mat %*% poststrat_df$n / sum(poststrat_df$n)
mrp_estimate_A_cbs <- c(mean = mean(mrp_estimates_vector), sd = sd(mrp_estimates_vector))

poststrat_abc <-
  census_race %>%
  group_by(regpro,age_group,Sex,Race) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_abc$composite_strata <- paste(poststrat_abc$age_group, poststrat_abc$Race, poststrat_abc$Sex, sep = "_")

poststrat_df <- poststrat_abc
epred_mat <- posterior_epred(fit2, newdata = poststrat_df, draws = 4000)
mrp_estimates_vector <- epred_mat %*% poststrat_df$n / sum(poststrat_df$n)
mrp_estimate_A_abc <- c(mean = mean(mrp_estimates_vector), sd = sd(mrp_estimates_vector))

A_cbs_estimate <- c(mean = mean(df_A_cbs$interp_roche_n), se = get_se_bernoulli(mean(df_A_cbs$interp_roche_n), nrow(df_A_cbs)))
A_abc_estimate <- c(mean = mean(df_A_abc$interp_roche_n), se = get_se_bernoulli(mean(df_A_abc$interp_roche_n), nrow(df_A_abc)))

# Create your composite stratification variable
df_A_abc$composite_strata <- paste(df_A_abc$age_group, df_A_abc$Sex, df_A_abc$Race, sep = "_")
df_A_cbs$composite_strata <- paste(df_A_cbs$age_group, df_A_cbs$Sex, df_A_cbs$Race, 
                                   df_A_cbs$QuintMat, df_A_cbs$QuintSoc, sep = "_")

# By province/region
regpro_df <- data.frame(
  regpro = c("BC","AB","MB","SK","ON","ATL"), 
  mrp_estimate_A_cbs = NA,
  mrp_estimate_A_cbs_se = NA,
  mrp_estimate_A_abc = NA,
  mrp_estimate_A_abc_se = NA,
  A_cbs_estimate = NA,
  A_cbs_estimate_se = NA,
  A_abc_estimate = NA,
  A_abc_estimate_se = NA,
  n_sample = NA,
  n_full = NA
)

for(i in 1:nrow(regpro_df)) {
  
  # MRP estimate A abc
  epred_mat <- posterior_epred(fit1, newdata = poststrat_abc, draws = 4000)
  filtering_condition <- which(poststrat_abc$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_abc[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_estimate_A_abc[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_estimate_A_abc_se[i] <- sd(mrp_estimates_vector)
  
  # MRP estimate A cbs
  epred_mat <- posterior_epred(fit2, newdata = poststrat_cbs, draws = 4000)
  filtering_condition <- which(poststrat_cbs$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_cbs[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_estimate_A_cbs[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_estimate_A_cbs_se[i] <- sd(mrp_estimates_vector)
  
  # Unadjusted estimate A cbs
  regpro_df$A_cbs_estimate[i] <- mean(filter(df_A_cbs, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_A_cbs, regpro==regpro_df$regpro[i]))
  regpro_df$A_cbs_estimate_se[i] <- get_se_bernoulli(regpro_df$A_cbs_estimate[i], regpro_df$n_sample[i])

  # Unadjusted estimate A abc
  regpro_df$A_abc_estimate[i] <- mean(filter(df_A_abc, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_A_abc, regpro==regpro_df$regpro[i]))
  regpro_df$A_abc_estimate_se[i] <- get_se_bernoulli(regpro_df$A_abc_estimate[i], regpro_df$n_sample[i])
  
  # Direct adjusted estimate with raking A cbs
  data <- df_A_cbs[df_A_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$da_estimate_A_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$da_estimate_A_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Direct adjusted estimate with raking A abc
  data <- df_A_abc[df_A_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$da_estimate_A_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$da_estimate_A_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
}


### Compare Plot
colors <- c("MRP abc" = "#1CE37B", "OBS abc" = "#69b3a2", "OBS cbs" = "#7B1CE3", "MRP cbs" = "#E37B1C")
compare1 <- ggplot(data=regpro_df) +
  geom_point(aes(x=regpro, y=A_cbs_estimate, color = "OBS cbs"), position=position_dodge(width=0.3)) +
  geom_errorbar(aes(ymin=A_cbs_estimate - 2*A_cbs_estimate_se,
                    ymax=A_cbs_estimate + 2*A_cbs_estimate_se,
                    x=regpro, color = "OBS cbs"), alpha=.5, width = 0, position=position_dodge(width=0.3)) +
  geom_point(data=regpro_df, aes(x=regpro, y=mrp_estimate_A_cbs, color = "MRP cbs"), position=pd) +
  geom_errorbar(data=regpro_df, aes(ymin=mrp_estimate_A_cbs - 2*mrp_estimate_A_cbs_se, 
                                    ymax=mrp_estimate_A_cbs + 2*mrp_estimate_A_cbs_se, 
                                    x=regpro, color = "MRP cbs"), alpha=.5, width = 0, position=pd) +
  
  geom_point(aes(x=regpro, y=A_abc_estimate, color = "OBS abc"), position=pd) +
  geom_errorbar(aes(ymin=A_abc_estimate - 2*A_abc_estimate_se,
                    ymax=A_abc_estimate + 2*A_abc_estimate_se,
                    x=regpro, color = "OBS abc"), alpha=.5, width = 0, position=pd) +
  geom_point(data=regpro_df, aes(x=regpro, y=mrp_estimate_A_abc, color = "MRP abc"), position=pd) +
  geom_errorbar(data=regpro_df, aes(ymin=mrp_estimate_A_abc - 2*mrp_estimate_A_abc_se, 
                                    ymax=mrp_estimate_A_abc + 2*mrp_estimate_A_abc_se, 
                                    x=regpro, color = "MRP abc"), alpha=.5, width = 0, position=pd) +
  scale_y_continuous(breaks=c(0,.025,.05,.075), 
                     labels=c("0%","2.5%","5%","7.5%"), 
                     expand=c(0,0))+
  coord_cartesian(ylim=c(0, .1)) +
  theme_bw()+
  labs(x="Province/Region",y="Anti-N Seropositivity",color = "Legend")+
  
  scale_color_manual(values = colors,
                     name = "type",
                     labels = c("MRP abc", "OBS abc", "OBS cbs", "MRP cbs"), guide='legend') + 
  
  theme(legend.position="bottom",
        axis.title=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(angle=90,size=8, vjust=0.3),
        legend.title=element_text(size=10),
        legend.text=element_text(size=10))

compare1

compare2 <- ggplot(data = data.frame()) +
  geom_point(aes(y=sample_cces_estimate[1], x = .25), color = "#E37B1C") +
  geom_errorbar(data=data.frame(), aes(y = sample_cces_estimate[1], 
                                       x = .25,
                                       ymin = sample_cces_estimate[1] - 2*sample_cces_estimate[2],
                                       ymax = sample_cces_estimate[1] + 2*sample_cces_estimate[2]),
                width = 0, color = "#E37B1C") +
  geom_text(aes(x = Inf, y = sample_cces_estimate[1]+0.06, label = "Unadjusted Sample"), 
            hjust = -.05, size = 4, color = "#E37B1C") +
  geom_point(aes(y = mrp_estimate[1], x = .75), color = "#7B1CE3") +
  geom_errorbar(aes(y = mrp_estimate[1], 
                    x = .75, 
                    ymin = mrp_estimate[1] - 2*mrp_estimate[2],
                    ymax = mrp_estimate[1] + 2*mrp_estimate[2]),
                width = 0, color = "#7B1CE3") +
  geom_text(data = data.frame(), aes(x = Inf, y = mrp_estimate[1]+.0, label = "Sample with MRP"), 
            hjust = -.05, size = 4, color = "#7B1CE3") +
  scale_y_continuous(breaks=c(0,.25,.5,.75,1),
                     labels=c("0%","25%","50%","75%","100%"),
                     limits=c(0,1),expand=c(0,0)) +
  geom_point(data = data.frame(), aes(y=full_cces_estimate[1], x = .5), color = "#1CE37B") +
  geom_errorbar(data = data.frame(), aes(y = full_cces_estimate[1], 
                                         x = .5,
                                         ymin = full_cces_estimate[1] - 2*full_cces_estimate[2],
                                         ymax = full_cces_estimate[1] + 2*full_cces_estimate[2]),
                width = 0, color = "#1CE37B") +
  geom_text(data = data.frame(), aes(x = Inf, y = full_cces_estimate-0.06, label = "Complete Survey"), 
            hjust = -.06, size = 4, color = "#1CE37B") +
  scale_y_continuous(breaks=c(0,.25,.5,.75,1),
                     labels=c("0%","25%","50%","75%","100%"),
                     limits=c(0,1),expand=c(0,0))+
  scale_x_continuous(limits=c(0,1),expand=c(0,0), breaks=c(.25, .75)) +
  coord_cartesian(clip = 'off') +
  theme_bw() +
  labs(x="Population",y="")+
  theme(legend.position="none",
        axis.title.y=element_blank(),
        axis.title.x=element_text(size=10, margin = margin(t = 19, r = 0, b = , l = 0)),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        plot.margin = margin(5.5, 105, 5.5, 5.5, "pt")
  )

bayesplot_grid(compare1,compare2, 
               grid_args = list(nrow=1, widths = c(5,1.4)))



regpro_df %>% 
  pivot_longer(
    cols = c(),
    names_to = ,
    values
  )


colors <- c("Sepal Width" = "blue", "Petal Length" = "red", "Petal Width" = "orange")

ggplot(iris, aes(x = Sepal.Length)) +
  geom_line(aes(y = Sepal.Width, color = "Sepal Width"), size = 1.5) +
  geom_line(aes(y = Petal.Length, color = "Petal Length"), size = 1.5) +
  geom_line(aes(y = Petal.Width, color = "Petal Width"), size = 1.5) +
  labs(x = "Year",
       y = "(%)",
       color = "Legend") +
  scale_color_manual(values = colors)


# reload the dataframe in long format
regpro_df3 <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df3.csv')
p <- ggplot(regpro_df3, aes(x = regpro, y = mrp_mean, colour=source, shape = model))
  p +
  theme_bw()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 1.2) +
  #  scale_color_manual(values = my_colors) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Seropositivity") +
  xlab("Province/Region") +
  ggtitle("Setting A, CBS vs AbC vs CCAHS") +
  theme(plot.title = element_text(vjust = 0)) 
  + 
  scale_color_manual(values = c("MRP abc" = "#1CE37B", "OBS abc" = "#69b3a2", "OBS cbs" = "#7B1CE3", "MRP cbs" = "#E37B1C"),
                     name = "type",
                     labels = c( "GLM", "Obs", "MLM spatial"), guide='legend')


########################### Consistent Code Begins Here Setting A #####################
poststrat_cbs$composite_strata_1 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, sep = "_")
poststrat_abc$composite_strata_1 <- paste(poststrat_abc$age_group, poststrat_abc$Sex, sep = "_")
df_A_abc$composite_strata_1 <- paste(df_A_abc$age_group, df_A_abc$Sex, sep = "_")
df_A_cbs$composite_strata_1 <- paste(df_A_cbs$age_group, df_A_cbs$Sex, sep = "_")

poststrat_cbs$composite_strata_2 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$Race, sep = "_")
poststrat_abc$composite_strata_2 <- paste(poststrat_abc$age_group, poststrat_abc$Sex, poststrat_abc$Race, sep = "_")
df_A_abc$composite_strata_2 <- paste(df_A_abc$age_group, df_A_abc$Sex, df_A_abc$Race, sep = "_")
df_A_cbs$composite_strata_2 <- paste(df_A_cbs$age_group, df_A_cbs$Sex, df_A_cbs$Race, sep = "_")

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
  rake_race_A_cbs = NA,
  rake_race_A_cbs_se = NA,
  rake_race_A_abc = NA,
  rake_race_A_abc_se = NA,
  n_sample = NA,
  n_full = NA
)

for(i in 1:nrow(regpro_df)) {
  
  # MRP estimate A abc
  epred_mat <- posterior_epred(fit1_stan, newdata = poststrat_abc, draws = 4000)
  filtering_condition <- which(poststrat_abc$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_abc[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_A_abc[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_A_abc_se[i] <- sd(mrp_estimates_vector)
  
  # MRP estimate A cbs
  epred_mat <- posterior_epred(fit2_stan, newdata = poststrat_cbs, draws = 4000)
  filtering_condition <- which(poststrat_cbs$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_cbs[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_A_cbs[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_A_cbs_se[i] <- sd(mrp_estimates_vector)
  
  # Unadjusted estimate A abc
  regpro_df$ua_A_abc[i] <- mean(filter(df_A_abc, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_A_abc, regpro==regpro_df$regpro[i]))
  regpro_df$ua_A_abc_se[i] <- get_se_bernoulli(regpro_df$ua_A_abc[i], regpro_df$n_sample[i])
  
  # Unadjusted estimate A cbs
  regpro_df$ua_A_cbs[i] <- mean(filter(df_A_cbs, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_A_cbs, regpro==regpro_df$regpro[i]))
  regpro_df$ua_A_cbs_se[i] <- get_se_bernoulli(regpro_df$ua_A_cbs[i], regpro_df$n_sample[i])

  # Raking on age/sex A abc (RAKE)
  data <- df_A_abc[df_A_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata_1),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_A_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_A_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A cbs (RAKE)
  data <- df_A_cbs[df_A_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata_1),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_A_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_A_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A cbs (RAKE_RACE)
  data <- df_A_cbs[df_A_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_2, data = data)
  poststrat.dist <- data.frame(composite_strata_2 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_2,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata_2),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_race_A_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_race_A_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]

  # Raking on age/sex A abc (RAKE_RACE)
  data <- df_A_abc[df_A_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_2, data = data)
  poststrat.dist <- data.frame(composite_strata_2 = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata_2,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata_2),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_race_A_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_race_A_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
}

write.csv(regpro_df, "/home/yuanyu/projects/covid_donors_3waves/1_data/regpro_df3.csv")


########################### Consistent Code Begins Here Setting F #####################
census_urban <- census_urban %>% rename("QuintMat" = "QUINTMAT")
census_urban <- census_urban %>% rename("QuintSoc" = "QUINTSOC")
census_urban <- census_urban %>% rename("Urban" = "urban")
census_urban <- census_urban %>% rename("regpro" = "province")
census_urban[census_urban$regpro == 'Atlantic', ]$regpro <- 'ATL'
census_urban$age_group = case_when(census_urban$age_group == '18-39 years' ~ "18-39", 
                                  census_urban$age_group == '40-54 years' ~"40-54",
                                  census_urban$age_group == '55+ years' ~"55+",
                                  TRUE~NA)

poststrat_cbs <-
  census_urban %>%
  group_by(regpro,age_group,Sex,Urban,QuintMat,QuintSoc) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_cbs$composite_strata <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, sep = "_")
df_F_cbs$composite_strata <- paste(df_F_cbs$age_group, df_F_cbs$Sex, sep = "_")

poststrat_cbs$composite_strata_1 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$QuintMat, sep = "_")
df_F_cbs$composite_strata_1 <- paste(df_F_cbs$age_group, df_F_cbs$Sex, df_F_cbs$QuintMat, sep = "_")

poststrat_cbs$composite_strata_2 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$QuintMat, 
                                          poststrat_cbs$Urban, sep = "_")
df_F_cbs$composite_strata_2 <- paste(df_F_cbs$age_group, df_F_cbs$Sex, df_F_cbs$QuintMat,df_F_cbs$Urban, sep = "_")

# By province/region
regpro_df <- data.frame(
  regpro = c("BC","AB","MB","SK","ON","ATL"), 
  mrp_F_cbs = NA,
  mrp_F_cbs_se = NA,
  ua_F_cbs = NA,
  ua_F_cbs_se = NA,
  rake_F_cbs = NA,
  rake_F_cbs_se = NA,
  rake1_F_cbs = NA,
  rake1_F_cbs_se = NA,
  rake2_F_cbs = NA,
  rake2_F_cbs_se = NA,
  n_sample = NA,
  n_full = NA
)

options(survey.lonely.psu="certainty")

for(i in 1:nrow(regpro_df)) {

  # MRP estimate A cbs
  epred_mat <- posterior_epred(fit_F_cbs, newdata = poststrat_cbs, draws = 4000)
  filtering_condition <- which(poststrat_cbs$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_cbs[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_F_cbs[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_F_cbs_se[i] <- sd(mrp_estimates_vector)
  
  # Unadjusted estimate A cbs
  regpro_df$ua_F_cbs[i] <- mean(filter(df_F_cbs, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_F_cbs, regpro==regpro_df$regpro[i]))
  regpro_df$ua_F_cbs_se[i] <- get_se_bernoulli(regpro_df$ua_F_cbs[i], regpro_df$n_sample[i])
  
  # Raking on age/sex A cbs (RAKE)
  data <- df_F_cbs[df_F_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_F_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_F_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A cbs (RAKE1)
  data <- df_F_cbs[df_F_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata_1),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake1_F_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake1_F_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  

  # Raking on age/sex A cbs (RAKE2)
  data <- df_F_cbs[df_F_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_2, data = data)
  poststrat.dist <- data.frame(composite_strata_2 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_2,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                        ~composite_strata_2,
                        poststrat.dist, partial=TRUE)
  
  regpro_df$rake2_F_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake2_F_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
}

write.csv(regpro_df, "/home/yuanyu/projects/covid_donors_3waves/1_data/regpro_df_F_urban.csv")

# reload the dataframe in long format
regpro_df3 <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df3.csv')

desired_order <- c("UA","RAKE","RAKE_RACE","POST4","MRP")
desired_order1 <- c("BC","AB","MB","SK","ON","ATL")
desired_order2 <- c("CBS","ABC","CCAHS")

# Reorder the levels of the 'category' variable
regpro_df3$model <- factor(regpro_df3$model, levels = desired_order)
regpro_df3$regpro <- factor(regpro_df3$regpro, levels = desired_order1)
regpro_df3$source <- factor(regpro_df3$source, levels = desired_order2)

compare_A <- ggplot(regpro_df3, aes(x = regpro, y = mrp_mean, colour=source, shape = model)) +
  theme_bw()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 2) +
  scale_color_manual(values = c("#F8766D", "#00BFC4","#00BA38")) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Seropositivity") +
  xlab("Province/Region") +
  ggtitle("Setting A (Jan-Apr, 2021), CBS vs AbC vs CCAHS") +
  theme(plot.title = element_text(vjust = 0))
compare_A

# reload the dataframe in long format
regpro_df_F <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_F.csv')

desired_order <- c("UA","RAKE","RAKE_1","RAKE_2","POST4","MRP")
desired_order1 <- c("BC","AB","MB","SK","ON","ATL")
desired_order2 <- c("CBS","CCAHS")

# Reorder the levels of the 'category' variable
regpro_df_F$model <- factor(regpro_df_F$model, levels = desired_order)
regpro_df_F$regpro <- factor(regpro_df_F$regpro, levels = desired_order1)
regpro_df_F$source <- factor(regpro_df_F$source, levels = desired_order2)

compare_F <- ggplot(regpro_df_F, aes(x = regpro, y = mrp_mean, colour=source, shape = model)) +
  theme_bw()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 2) +
  scale_color_manual(name="Study", values = c("#F8766D","#00BA38","#F564E3")) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_shape_manual(
    name = "Adjustment",
    values = c("MRP" = 7, 
               "UA" = 16 ,
               "RAKE" = 17, 
               "RAKE_1" = 15,
               "RAKE_2" = 8,
               "POST4" = 3),
    labels = c("MRP" = "MRP", 
               "UA" = "No adjustment" ,
               "RAKE" = "Raking (Age-Sex)", 
               "RAKE_1" = "Raking (Age-Sex-Race)",
               "RAKE_2" = "Raking (Age-Sex-Race-Urban)",
               "POST4" = "Poststratification (Age-Sex-Race-Urban)")) +
  guides(
    shape = guide_legend(order = 1),  # Set the order of the color legend
    color = guide_legend(order = 2)   # Set the order of the shape legend
  ) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Seropositivity") +
  xlab("Province/Region") +
#  ggtitle("Setting F (Dec 20- Apr 21), CBS vs CCAHS") +
  theme(plot.title = element_text(vjust = 0)) +
  theme(legend.position = "bottom")
compare_F


########################### Consistent Code Begins Here Setting B #####################
poststrat_cbs <-
  census_race %>%
  group_by(regpro,age_group,Sex,Race,QuintMat,QuintSoc) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_cbs$composite_strata <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, sep = "_")
poststrat_abc$composite_strata <- paste(poststrat_abc$age_group, poststrat_abc$Sex, sep = "_")
df_B_abc$composite_strata <- paste(df_B_abc$age_group, df_B_abc$Sex, sep = "_")
df_B_cbs$composite_strata <- paste(df_B_cbs$age_group, df_B_cbs$Sex, sep = "_")

poststrat_cbs$composite_strata_1 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$Race, sep = "_")
poststrat_abc$composite_strata_1 <- paste(poststrat_abc$age_group, poststrat_abc$Sex, poststrat_abc$Race, sep = "_")
df_B_abc$composite_strata_1 <- paste(df_B_abc$age_group, df_B_abc$Sex, df_B_abc$Race, sep = "_")
df_B_cbs$composite_strata_1 <- paste(df_B_cbs$age_group, df_B_cbs$Sex, df_B_cbs$Race, sep = "_")

# By province/region
regpro_df <- data.frame(
  regpro = c("BC","AB","MB","SK","ON","ATL"), 
  mrp_B_cbs = NA,
  mrp_B_cbs_se = NA,
  mrp_B_abc = NA,
  mrp_B_abc_se = NA,
  ua_B_cbs = NA,
  ua_B_cbs_se = NA,
  ua_B_abc = NA,
  ua_B_abc_se = NA,
  rake_B_cbs = NA,
  rake_B_cbs_se = NA,
  rake_B_abc = NA,
  rake_B_abc_se = NA,
  rake1_B_cbs = NA,
  rake1_B_cbs_se = NA,
  rake1_B_abc = NA,
  rake1_B_abc_se = NA,
  post_B_cbs = NA,
  post_B_cbs_se = NA,
  post_B_abc = NA,
  post_B_abc_se = NA,
  n_sample = NA,
  n_full = NA
)

for(i in 1:nrow(regpro_df)) {
  
  # MRP estimate B abc
  epred_mat <- posterior_epred(fit_B_abc, newdata = poststrat_abc, draws = 4000)
  filtering_condition <- which(poststrat_abc$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_abc[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_B_abc[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_B_abc_se[i] <- sd(mrp_estimates_vector)
  
  # MRP estimate B cbs
  epred_mat <- posterior_epred(fit_B_cbs, newdata = poststrat_cbs, draws = 4000)
  filtering_condition <- which(poststrat_cbs$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_cbs[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_B_cbs[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_B_cbs_se[i] <- sd(mrp_estimates_vector)
  
  # Unadjusted estimate B abc
  regpro_df$ua_B_abc[i] <- mean(filter(df_B_abc, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_B_abc, regpro==regpro_df$regpro[i]))
  regpro_df$ua_B_abc_se[i] <- get_se_bernoulli(regpro_df$ua_B_abc[i], regpro_df$n_sample[i])
  
  # Unadjusted estimate B cbs
  regpro_df$ua_B_cbs[i] <- mean(filter(df_B_cbs, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_B_cbs, regpro==regpro_df$regpro[i]))
  regpro_df$ua_B_cbs_se[i] <- get_se_bernoulli(regpro_df$ua_B_cbs[i], regpro_df$n_sample[i])
  
  # Raking on age/sex B abc (RAKE)
  data <- df_B_abc[df_B_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_B_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_B_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex B cbs (RAKE)
  data <- df_B_cbs[df_B_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_B_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_B_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # # Raking on age/sex B abc (RAKE1)
  # data <- df_B_abc[df_B_abc$regpro==regpro_df$regpro[i],]
  # data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  # poststrat.dist <- data.frame(composite_strata_1 = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata_1,
  #                              Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  # 
  # data.svy.rake <- rake(design = data.svy.unweighted,
  #                       sample.margins = list(~composite_strata_1),
  #                       population.margins = list(poststrat.dist))
  # 
  # regpro_df$rake1_B_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  # regpro_df$rake1_B_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  # 
  # # Raking on age/sex B cbs (RAKE1)
  # data <- df_B_cbs[df_B_cbs$regpro==regpro_df$regpro[i],]
  # data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  # poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_1,
  #                              Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  # 
  # data.svy.rake <- rake(design = data.svy.unweighted,
  #                       sample.margins = list(~composite_strata_1),
  #                       population.margins = list(poststrat.dist))
  # 
  # regpro_df$rake1_B_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  # regpro_df$rake1_B_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
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
}

write.csv(regpro_df, "/home/yuanyu/projects/covid_donors_3waves/1_data/regpro_df_B.csv")


########################### Consistent Code Begins Here Setting C #####################
poststrat_cbs <-
  census_race %>%
  group_by(regpro,age_group,Sex,Race,QuintMat,QuintSoc) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_abc <-
  census_race %>%
  group_by(regpro,age_group,Sex,Race) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

df_C_abc <- df_C_abc[!is.na(df_C_abc$Race),]
df_C_cbs <- df_C_cbs[!is.na(df_C_cbs$age_group),]

poststrat_cbs$composite_strata <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, sep = "_")
poststrat_abc$composite_strata <- paste(poststrat_abc$age_group, poststrat_abc$Sex, sep = "_")
df_C_abc$composite_strata <- paste(df_C_abc$age_group, df_C_abc$Sex, sep = "_")
df_C_cbs$composite_strata <- paste(df_C_cbs$age_group, df_C_cbs$Sex, sep = "_")

poststrat_cbs$composite_strata_1 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$Race, sep = "_")
poststrat_abc$composite_strata_1 <- paste(poststrat_abc$age_group, poststrat_abc$Sex, poststrat_abc$Race, sep = "_")
df_C_abc$composite_strata_1 <- paste(df_C_abc$age_group, df_C_abc$Sex, df_C_abc$Race, sep = "_")
df_C_cbs$composite_strata_1 <- paste(df_C_cbs$age_group, df_C_cbs$Sex, df_C_cbs$Race, sep = "_")

# By province/region
regpro_df <- data.frame(
  regpro = c("BC","AB","MB","SK","ON","ATL"), 
  mrp_C_cbs = NA,
  mrp_C_cbs_se = NA,
  mrp_C_abc = NA,
  mrp_C_abc_se = NA,
  ua_C_cbs = NA,
  ua_C_cbs_se = NA,
  ua_C_abc = NA,
  ua_C_abc_se = NA,
  rake_C_cbs = NA,
  rake_C_cbs_se = NA,
  rake_C_abc = NA,
  rake_C_abc_se = NA,
  rake1_C_cbs = NA,
  rake1_C_cbs_se = NA,
  rake1_C_abc = NA,
  rake1_C_abc_se = NA,
  post_C_cbs = NA,
  post_C_cbs_se = NA,
  post_C_abc = NA,
  post_C_abc_se = NA,
  n_sample = NA,
  n_full = NA
)

for(i in 1:nrow(regpro_df)) {
  
  # MRP estimate B abc
  epred_mat <- posterior_epred(fit_C_abc, newdata = poststrat_abc, draws = 4000)
  filtering_condition <- which(poststrat_abc$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_abc[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_C_abc[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_C_abc_se[i] <- sd(mrp_estimates_vector)
  
  # MRP estimate B cbs
  epred_mat <- posterior_epred(fit_C_cbs, newdata = poststrat_cbs, draws = 4000)
  filtering_condition <- which(poststrat_cbs$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_cbs[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_C_cbs[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_C_cbs_se[i] <- sd(mrp_estimates_vector)
  
  # Unadjusted estimate B abc
  regpro_df$ua_C_abc[i] <- mean(filter(df_C_abc, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_C_abc, regpro==regpro_df$regpro[i]))
  regpro_df$ua_C_abc_se[i] <- get_se_bernoulli(regpro_df$ua_C_abc[i], regpro_df$n_sample[i])
  
  # Unadjusted estimate B cbs
  regpro_df$ua_C_cbs[i] <- mean(filter(df_C_cbs, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_C_cbs, regpro==regpro_df$regpro[i]))
  regpro_df$ua_C_cbs_se[i] <- get_se_bernoulli(regpro_df$ua_C_cbs[i], regpro_df$n_sample[i])
  
  # Raking on age/sex B abc (RAKE)
  data <- df_C_abc[df_C_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_C_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_C_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex B cbs (RAKE)
  data <- df_C_cbs[df_C_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_C_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_C_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex B abc (RAKE1)
  data <- df_C_abc[df_C_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)

  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata_1),
                        population.margins = list(poststrat.dist))

  regpro_df$rake1_C_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake1_C_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]

  # Raking on age/sex B cbs (RAKE1)
  data <- df_C_cbs[df_C_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)

  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata_1),
                        population.margins = list(poststrat.dist))

  regpro_df$rake1_C_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake1_C_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]

  # Raking on age/sex A abc (POST)
  data <- df_C_abc[df_C_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_C_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_C_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A cbs (POST)
  data <- df_C_cbs[df_C_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_C_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_C_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
}

write.csv(regpro_df, "/home/yuanyu/projects/covid_donors_3waves/1_data/regpro_df_C.csv")

# reload the dataframe in long format
regpro_df_B <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_B.csv')

desired_order <- c("UA","RAKE","POST","MRP")
desired_order1 <- c("BC","AB","MB","SK","ON","ATL")
desired_order2 <- c("CBS","ABC")

# Reorder the levels of the 'category' variable
regpro_df_B$model <- factor(regpro_df_B$model, levels = desired_order)
regpro_df_B$regpro <- factor(regpro_df_B$regpro, levels = desired_order1)
regpro_df_B$source <- factor(regpro_df_B$source, levels = desired_order2)

compare_B <- ggplot(regpro_df_B, aes(x = regpro, y = mrp_mean, colour=source, shape = model)) +
  theme_bw()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 2) +
  scale_color_manual(name="Study",values = c("#F8766D", "#00BFC4","#B79F00","#619CFF","#00BA38","#F564E3")) +
  #  scale_color_manual(values = my_colors) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  scale_shape_manual(
    name="Adjustment",
    values = c("MRP" = 7, 
               "UA" = 16 ,
               "RAKE" = 17, 
               "POST" = 15),
    labels = c("MRP" = "MRP", 
               "UA" = "No adjustment",
               "RAKE" = "Raking (Age-Sex)", 
               "POST" = "Poststratification (Age-Sex-Race)")) +
  guides(
    shape = guide_legend(order = 1),  # Set the order of the color legend
    color = guide_legend(order = 2)   # Set the order of the shape legend
  ) +
  ylab("Seropositivity") +
  xlab("Province/Region") +
  #ggtitle("Setting B (July-August, 2021), CBS vs AbC") +
  theme(plot.title = element_text(vjust = 0)) +
  theme(legend.position = "bottom")
compare_B


# reload the dataframe in long format
regpro_df_C <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_C.csv')

desired_order <- c("MRP","UA","RAKE","RAKE1","POST")
desired_order1 <- c("BC","AB","MB","SK","ON","ATL")
desired_order2 <- c("CBS","ABC")

# Reorder the levels of the 'category' variable
regpro_df_C$model <- factor(regpro_df_C$model, levels = desired_order)
regpro_df_C$regpro <- factor(regpro_df_C$regpro, levels = desired_order1)
regpro_df_C$source <- factor(regpro_df_C$source, levels = desired_order2)

compare_C <- ggplot(regpro_df_C, aes(x = regpro, y = mrp_mean, colour=source, shape = model)) +
  theme_bw()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 1.2) +
  scale_color_manual(name= "Study", values = c("#F8766D", "#00BFC4","#619CFF","#00BA38","#F564E3")) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  scale_shape_manual(
    name = "Adjustment",
    values = c("MRP" = 7, 
               "UA" = 16 ,
               "RAKE" = 17, 
               "RAKE1" = 15,
               "POST" = 3),
    labels = c("MRP" = "MRP", 
               "UA" = "No adjustment" ,
               "RAKE" = "Raking (Age-Sex)", 
               "RAKE1" = "Raking (Age-Sex-Race)",
               "POST" = "Poststratification (Age-Sex-Race-Urban)")) +
  guides(
    shape = guide_legend(order = 1),  # Set the order of the color legend
    color = guide_legend(order = 2)   # Set the order of the shape legend
  ) +
  ylab("Seropositivity") +
  xlab("Province/Region") +
#  ggtitle("Setting C (Jan- Apr 22), CBS vs AbC") +
  theme(plot.title = element_text(vjust = 0)) +
  theme(legend.position = "bottom")
compare_C

##### [Apparent Prevalence + (Specificity − 1)]/[Specificity + (Sensitivity − 1)]
(x+0.998-1)/(0.995+0.998-1) #-0.002 /0.993
(x+0.99-1)/(0.92+0.99-1) #-0.01 /0.91



############################## Consistent Code Begins Here Setting G Alberta #####################
poststrat_cbs <-
  census_urban %>%
  group_by(regpro,age_group,Sex,QuintMat,QuintSoc,Urban) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_abc <-
  census_urban %>%
  group_by(regpro,age_group,Sex,Urban) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_apl <-
  census_urban %>%
  group_by(regpro,age_group,Sex,Urban) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

df_G_abc <- df_G_abc[!is.na(df_G_abc$Race),]
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
  rake1_G_cbs = NA,
  rake1_G_cbs_se = NA,
  rake1_G_abc = NA,
  rake1_G_abc_se = NA,
  rake1_G_apl = NA,
  rake1_G_apl_se = NA,
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
  
  # Raking on age/sex B abc (RAKE1)
  data <- df_G_abc[df_G_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_apl[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata_1),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake1_G_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake1_G_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex B cbs (RAKE1)
  data <- df_G_cbs[df_G_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata_1),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake1_G_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake1_G_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]

  # Raking on age/sex B apl (RAKE1)
  data <- df_G_apl[df_G_apl$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_apl[poststrat_apl$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_apl[poststrat_apl$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata_1),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake1_G_apl[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake1_G_apl_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
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


write.csv(regpro_df, "/home/yuanyu/projects/covid_donors_3waves/1_data/regpro_df_G.csv")

# reload the dataframe in long format
regpro_df_G <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_G.csv')

desired_order <- c("UA","RAKE","RAKE1","POST","MRP")
#desired_order1 <- c("BC","AB","MB","SK","ON","ATL")
desired_order2 <- c("CBS","ABC","APL")

# Reorder the levels of the 'category' variable
regpro_df_G$model <- factor(regpro_df_G$model, levels = desired_order)
#regpro_df_C_vac$regpro <- factor(regpro_df_C_vac$regpro, levels = desired_order1)
regpro_df_G$source <- factor(regpro_df_G$source, levels = desired_order2)

compare_G <- ggplot(regpro_df_G1, aes(x = regpro, y = mrp_mean, colour=source, shape = model)) +
  theme_bw()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 2) +
  scale_color_manual(name="Study", values = c("#F8766D", "#00BFC4","#619CFF","#00BA38","#F564E3")) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  scale_shape_manual(
    name = "Adjustment",
    values = c("MRP" = 7, 
               "UA" = 16 ,
               "RAKE" = 17, 
               "RAKE1" = 15,
               "POST" = 3),
    labels = c("MRP" = "MRP", 
               "UA" = "No adjustment" ,
               "RAKE" = "Raking (Age-Sex)", 
               "RAKE1" = "Raking (Age-Sex-Race)",
               "POST" = "Poststratification (Age-Sex-Race-Urban)")) +
  guides(
    shape = guide_legend(order = 1),  # Set the order of the color legend
    color = guide_legend(order = 2)   # Set the order of the shape legend
  ) +
  ylab("Seropositivity") +
  xlab("Province/Region") +
#  ggtitle("Setting G (Jan- Apr 22), CBS vs AbC vs APL, Alberta") +
  theme(plot.title = element_text(vjust = 0)) +
  theme(legend.position = "bottom")
compare_G


# reload the dataframe in long format
regpro_df_C <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_C.csv')
compare_C <- ggplot(regpro_df_C, aes(x = regpro, y = mrp_mean, colour=source, shape = model)) +
  theme_bw()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 1.2) +
  #  scale_color_manual(values = my_colors) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Seropositivity") +
  xlab("Province/Region") +
  ggtitle("Setting C (Jan- Apr 22), CBS vs AbC") +
  theme(plot.title = element_text(vjust = 0)) 
compare_C

# library
library(latticeExtra) 

# create data
set.seed(1) 
data <- data.frame(x = rnorm(100), y = rnorm(100)) 
data$z <- with(data, x * y + rnorm(100, sd = 1)) 

# showing data points on the same color scale 
levelplot(z ~ x * y, data, 
          panel = panel.levelplot.points, cex = 1.2
) + 
  layer_(panel.2dsmoother(..., n = 200))

# Load the ggplot2 library
library(ggplot2)

# Sample data (replace with your data)
cell_counts <- data.frame(
  Condition = c("Condition A", "Condition B", "Condition C"),
  Cell_Type = c("Cell Type 1", "Cell Type 1", "Cell Type 2", "Cell Type 2", "Cell Type 3", "Cell Type 3"),
  Count = c(10, 20, 15, 25, 30, 35)
)

# Create a heatmap using ggplot2
ggplot(cell_counts, aes(x = Cell_Type, y = Condition, fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +  # Color scale
  labs(title = "Aggregated Cell Counts Heatmap",
       x = "Cell Types",
       y = "Conditions") +
  theme_minimal()  # Customize the theme as needed

agg_C_abc <-
  df_C_abc %>%
  group_by(regpro,age_group,Sex,Race) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

agg_C_abc <-
  df_C_abc %>%
  group_by(regpro,age_group,Sex,Race) %>%
  summarize(case_counts = sum(interp_roche_n), n = n(), rates = mean(interp_roche_n)) %>%
  drop_na()
#  summarise(n = n()) %>% mutate(rates = sum(interp_roche_n)/sum(n))

agg_C_cbs <-
  df_C_cbs %>%
  group_by(regpro,age_group,Sex,Race) %>%
  summarize(case_counts = sum(interp_roche_n), n = n(), rates = mean(interp_roche_n)) %>%
  drop_na()

agg_C <- rbind(agg_C_abc,agg_C_cbs)

agg_C$source <- c(rep('ABC', nrow(agg_C_abc)), rep('CBS', nrow(agg_C_cbs)))


agg_C$Race <- with(agg_C,ifelse(Race ==1,'White','Non-white'))

lbl <- c("Non-white", "White")
Race = rep(c(0,1),nrow(agg_C_abc))

ggplot(agg_C, aes(x = age_group, y = regpro, fill = rates)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +  # Color scale
  facet_wrap(~ source + Race) +
#  facet_wrap(~ source + Race, labeller = as_labeller(setNames(lbl, sort(unique(Race))))) + 
#  facet_grid(cols = vars(Sex), rows = vars(Year),labeller = labeller(Sex = col_labs))+
  labs(title = "Anti-N Posotivity, Setting C",
       x = "age_group",
       y = "Region/Province") +
  theme_minimal()  # Customize the theme as needed

#### Heatmap Setting A (no CCAHS)
agg_A_abc <-
  df_A_abc %>%
  group_by(regpro,age_group,Sex,Race) %>%
  summarize(case_counts = sum(interp_roche_n), n = n(), rates = mean(interp_roche_n)) %>%
  drop_na()
#  summarise(n = n()) %>% mutate(rates = sum(interp_roche_n)/sum(n))

agg_A_cbs <-
  df_A_cbs %>%
  group_by(regpro,age_group,Sex,Race) %>%
  summarize(case_counts = sum(interp_roche_n), n = n(), rates = mean(interp_roche_n)) %>%
  drop_na()

agg_A <- rbind(agg_A_abc,agg_A_cbs)

agg_A$source <- c(rep('ABC', nrow(agg_A_abc)), rep('CBS', nrow(agg_A_cbs)))


agg_A$Race <- with(agg_A,ifelse(Race ==1,'White','Non-white'))

#lbl <- c("Non-white", "White")
#Race = rep(c(0,1),nrow(agg_C_abc))

ggplot(agg_A, aes(x = age_group, y = regpro, fill = rates)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +  # Color scale
  facet_wrap(~ source + Race) +
  #  facet_wrap(~ source + Race, labeller = as_labeller(setNames(lbl, sort(unique(Race))))) + 
  #  facet_grid(cols = vars(Sex), rows = vars(Year),labeller = labeller(Sex = col_labs))+
  labs(title = "Anti-N Posotivity, Setting A",
       x = "age_group",
       y = "Region/Province") +
  theme_minimal()  # Customize the theme as needed


#### Heatmap Setting B
agg_B_abc <-
  df_B_abc %>%
  group_by(regpro,age_group,Sex,Race) %>%
  summarize(case_counts = sum(interp_roche_n), n = n(), rates = mean(interp_roche_n)) %>%
  drop_na()
#  summarise(n = n()) %>% mutate(rates = sum(interp_roche_n)/sum(n))

agg_B_cbs <-
  df_B_cbs %>%
  group_by(regpro,age_group,Sex,Race) %>%
  summarize(case_counts = sum(interp_roche_n), n = n(), rates = mean(interp_roche_n)) %>%
  drop_na()

agg_B <- rbind(agg_B_abc,agg_B_cbs)

agg_B$source <- c(rep('ABC', nrow(agg_B_abc)), rep('CBS', nrow(agg_B_cbs)))


agg_B$Race <- with(agg_B,ifelse(Race ==1,'White','Non-white'))

#lbl <- c("Non-white", "White")
#Race = rep(c(0,1),nrow(agg_C_abc))

ggplot(agg_B, aes(x = age_group, y = regpro, fill = rates)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +  # Color scale
  facet_wrap(~ source + Race) +
  #  facet_wrap(~ source + Race, labeller = as_labeller(setNames(lbl, sort(unique(Race))))) + 
  #  facet_grid(cols = vars(Sex), rows = vars(Year),labeller = labeller(Sex = col_labs))+
  labs(title = "Anti-N Posotivity, Setting B",
       x = "age_group",
       y = "Region/Province") +
  theme_minimal()  # Customize the theme as needed


#### Heatmap Setting G Ablerta
agg_G_abc <-
  df_G_abc %>%
  group_by(age_group,Sex,Urban) %>%
  summarize(case_counts = sum(interp_roche_n), n = n(), rates = mean(interp_roche_n)) %>%
  drop_na()
#  summarise(n = n()) %>% mutate(rates = sum(interp_roche_n)/sum(n))

agg_G_cbs <-
  df_G_cbs %>%
  group_by(age_group,Sex,Urban) %>%
  summarize(case_counts = sum(interp_roche_n), n = n(), rates = mean(interp_roche_n)) %>%
  drop_na()

agg_G_apl <-
  df_G_apl %>%
  group_by(age_group,Sex,Urban) %>%
  summarize(case_counts = sum(interp_roche_n), n = n(), rates = mean(interp_roche_n)) %>%
  drop_na()

agg_G <- rbind(agg_G_abc,agg_G_cbs,agg_G_apl)

agg_G$source <- c(rep('ABC', nrow(agg_G_abc)), rep('CBS', nrow(agg_G_cbs)), rep('APL', nrow(agg_G_apl)))

agg_G$Urban <- with(agg_G,ifelse(Urban ==1,'Urban','Rural'))
agg_G$Sex <- with(agg_G,ifelse(Sex ==1,'Male','Female'))

#lbl <- c("Non-white", "White")
#Race = rep(c(0,1),nrow(agg_C_abc))

ggplot(agg_G, aes(x = age_group, y = Sex, fill = rates)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +  # Color scale
  facet_wrap(~ source + Urban,  nrow = 3,ncol = 2) +
  #  facet_wrap(~ source + Race, labeller = as_labeller(setNames(lbl, sort(unique(Race))))) + 
  #  facet_grid(cols = vars(Sex), rows = vars(Year),labeller = labeller(Sex = col_labs))+
  labs(title = "Anti-N Posotivity, Setting G, Alberta",
       x = "age_group",
       y = "Sex") +
  theme_minimal()  # Customize the theme as needed

#### Heatmap Setting H #######
agg_H_abc <-
  df_H_abc %>%
  group_by(regpro,age_group,Sex,Urban) %>%
  summarize(case_counts = sum(interp_roche_n), n = n(), rates = mean(interp_roche_n)) %>%
  drop_na()
#  summarise(n = n()) %>% mutate(rates = sum(interp_roche_n)/sum(n))

agg_H_cbs <-
  df_H_cbs %>%
  group_by(regpro,age_group,Sex,Urban) %>%
  summarize(case_counts = sum(interp_roche_n), n = n(), rates = mean(interp_roche_n)) %>%
  drop_na()

agg_H_clsa <-
  df_H_clsa %>%
  group_by(regpro,age_group,Sex,Urban) %>%
  summarize(case_counts = sum(interp_roche_n), n = n(), rates = mean(interp_roche_n)) %>%
  drop_na()

agg_H <- rbind(agg_H_abc,agg_H_cbs,agg_H_clsa)

agg_H$source <- c(rep('ABC', nrow(agg_H_abc)),rep('CBS', nrow(agg_H_cbs)),rep('CLSA', nrow(agg_H_clsa)))


agg_H$Urban <- with(agg_H,ifelse(Urban ==1,'Urban','Rural'))
agg_H$Sex <- with(agg_H,ifelse(Sex ==1,'Male','Female'))

#lbl <- c("Non-white", "White")
#Race = rep(c(0,1),nrow(agg_C_abc))

agg_H <- rbind(agg_H[agg_H$Urban == 'Rural',],agg_H[agg_H$Urban == 'Urban',])

ggplot(agg_H, aes(x = age_group, y = regpro, fill = rates)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +  # Color scale
  facet_wrap(~ Urban + factor(source, levels=c('ABC', 'CBS', 'CLSA')), nrow = 2, ncol=3) +
  #  facet_wrap(~ source + Race, labeller = as_labeller(setNames(lbl, sort(unique(Race))))) + 
  #  facet_grid(cols = vars(Sex), rows = vars(Year),labeller = labeller(Sex = col_labs))+
  labs(title = "Anti-N Posotivity, Setting H",
       x = "age_group",
       y = "Region/Province") +
  theme_minimal()  # Customize the theme as needed


### ########################### Consistent Code Begins Here Setting H Elder Group (Urban) #####################
poststrat_cbs <-
  census_urban[census_urban$age_group != "18-39", ] %>%
  group_by(regpro,age_group,Sex,QuintMat,QuintSoc,Urban) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_abc <-
  census_urban[census_urban$age_group != "18-39", ] %>%
  group_by(regpro,age_group,Sex,Urban) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_clsa <-
  census_urban[census_urban$age_group != "18-39", ] %>%
  group_by(regpro,age_group,Sex,Urban) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

df_H_abc <- df_H_abc[!is.na(df_H_abc$Urban),]
df_H_cbs <- df_H_cbs[!is.na(df_H_cbs$age_group),]
df_H_clsa <- df_H_clsa[!is.na(df_H_clsa$age_group),]

poststrat_cbs$composite_strata <- paste(poststrat_cbs$age_group)
poststrat_abc$composite_strata <- paste(poststrat_abc$age_group)
poststrat_clsa$composite_strata <- paste(poststrat_clsa$age_group)
df_H_abc$composite_strata <- paste(df_H_abc$age_group, sep = "_")
df_H_cbs$composite_strata <- paste(df_H_cbs$age_group, sep = "_")
df_H_clsa$composite_strata <- paste(df_H_clsa$age_group, sep = "_")

poststrat_cbs$composite_strata_1 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$Urban, sep = "_")
poststrat_abc$composite_strata_1 <- paste(poststrat_abc$age_group, poststrat_abc$Sex, poststrat_abc$Urban, sep = "_")
poststrat_clsa$composite_strata_1 <- paste(poststrat_clsa$age_group, poststrat_clsa$Sex, poststrat_clsa$Urban, sep = "_")
df_H_abc$composite_strata_1 <- paste(df_H_abc$age_group, df_H_abc$Sex, df_H_abc$Urban, sep = "_")
df_H_cbs$composite_strata_1 <- paste(df_H_cbs$age_group, df_H_cbs$Sex, df_H_cbs$Urban, sep = "_")
df_H_clsa$composite_strata_1 <- paste(df_H_clsa$age_group, df_H_clsa$Sex, df_H_clsa$Urban, sep = "_")

# By province/region
regpro_df <- data.frame(
  regpro = c("BC","AB","MB","SK","ON","ATL"), 
  mrp_H_cbs = NA,
  mrp_H_cbs_se = NA,
  mrp_H_abc = NA,
  mrp_H_abc_se = NA,
  mrp_H_clsa = NA,
  mrp_H_clsa_se = NA,
  ua_H_cbs = NA,
  ua_H_cbs_se = NA,
  ua_H_abc = NA,
  ua_H_abc_se = NA,
  ua_H_clsa = NA,
  ua_H_clsa_se = NA,
  rake_H_cbs = NA,
  rake_H_cbs_se = NA,
  rake_H_abc = NA,
  rake_H_abc_se = NA,
  rake_H_clsa = NA,
  rake_H_clsa_se = NA,
  post_H_cbs = NA,
  post_H_cbs_se = NA,
  post_H_abc = NA,
  post_H_abc_se = NA,
  post_H_clsa = NA,
  post_H_clsa_se = NA,
  n_sample = NA,
  n_full = NA
)

for(i in 1:nrow(regpro_df)) {
  
  # MRP estimate abc
  epred_mat <- posterior_epred(fit_H_abc, newdata = poststrat_abc, draws = 4000)
  filtering_condition <- which(poststrat_abc$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_abc[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_H_abc[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_H_abc_se[i] <- sd(mrp_estimates_vector)
  
  # MRP estimate cbs
  epred_mat <- posterior_epred(fit_H_cbs, newdata = poststrat_cbs, draws = 4000)
  filtering_condition <- which(poststrat_cbs$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_cbs[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_H_cbs[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_H_cbs_se[i] <- sd(mrp_estimates_vector)
  
  # MRP estimate clsa
  epred_mat <- posterior_epred(fit_H_clsa, newdata = poststrat_clsa, draws = 4000)
  filtering_condition <- which(poststrat_clsa$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_clsa[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_H_clsa[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_H_clsa_se[i] <- sd(mrp_estimates_vector)
  
  # Unadjusted estimate abc
  regpro_df$ua_H_abc[i] <- mean(filter(df_H_abc, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_H_abc, regpro==regpro_df$regpro[i]))
  regpro_df$ua_H_abc_se[i] <- get_se_bernoulli(regpro_df$ua_H_abc[i], regpro_df$n_sample[i])
  
  # Unadjusted estimate cbs
  regpro_df$ua_H_cbs[i] <- mean(filter(df_H_cbs, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_H_cbs, regpro==regpro_df$regpro[i]))
  regpro_df$ua_H_cbs_se[i] <- get_se_bernoulli(regpro_df$ua_H_cbs[i], regpro_df$n_sample[i])
  
  # Unadjusted estimate clsa
  regpro_df$ua_H_clsa[i] <- mean(filter(df_H_clsa, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_H_clsa, regpro==regpro_df$regpro[i]))
  regpro_df$ua_H_clsa_se[i] <- get_se_bernoulli(regpro_df$ua_H_clsa[i], regpro_df$n_sample[i])
  
  # Raking on age/sex abc (RAKE)
  data <- df_H_abc[df_H_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_H_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_H_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex cbs (RAKE)
  data <- df_H_cbs[df_H_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_H_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_H_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex clsa (RAKE)
  data <- df_H_clsa[df_H_clsa$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_clsa[poststrat_clsa$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_clsa[poststrat_clsa$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_H_clsa[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_H_clsa_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A abc (POST)
  data <- df_H_abc[df_H_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_H_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_H_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A cbs (POST)
  data <- df_H_cbs[df_H_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_H_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_H_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A clsa (POST)
  data <- df_H_clsa[df_H_clsa$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_clsa[poststrat_clsa$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_clsa[poststrat_clsa$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_H_clsa[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_H_clsa_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
}

write.csv(regpro_df, "/home/yuanyu/projects/covid_donors_3waves/1_data/regpro_df_H1.csv")

# reload the dataframe in long format
regpro_df_H <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_H.csv')

desired_order <- c("UA","RAKE","RAKE1","POST","MRP")
desired_order1 <- c("BC","AB","MB","SK","ON","ATL")
desired_order2 <- c("CBS","ABC","CLSA")

# Reorder the levels of the 'category' variable
regpro_df_H$model <- factor(regpro_df_H$model, levels = desired_order)
regpro_df_H$regpro <- factor(regpro_df_H$regpro, levels = desired_order1)
regpro_df_H$source <- factor(regpro_df_H$source, levels = desired_order2)


compare_H <- ggplot(regpro_df_H, aes(x = regpro, y = mrp_mean, colour=source, shape = model)) +
  theme_bw()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 2) +
  scale_color_manual(name= "Study", values = c("#F8766D", "#00BFC4","#B79F00","#619CFF","#00BA38","#F564E3")) +
  #  scale_color_manual(values = my_colors) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  scale_shape_manual(
    name = "Adjustment",
    values = c("MRP" = 7, 
               "UA" = 16 ,
               "RAKE" = 17, 
               "POST" = 3),
    labels = c("MRP" = "MRP", 
               "UA" = "No adjustment" ,
               "RAKE" = "Raking (Age-Sex)", 
               "POST" = "Poststratification (Age-Sex-Race)")) +
  guides(
    shape = guide_legend(order = 1),  # Set the order of the color legend
    color = guide_legend(order = 2, nrow = 1)   # Set the order of the shape legend
  ) +
  ylab("Seropositivity") +
  xlab("Province/Region") +
#  ggtitle("Setting H (Mar-Apr & Jul-Aug, 2021), CBS vs AbC vs CLSA") +
  theme(plot.title = element_text(vjust = 0)) +
  theme(legend.position = "bottom") 
compare_H


######################## Consistent Code Begins Here Setting C (state level covariates) #####################
poststrat_cbs_race <-
  census_race %>%
  group_by(regpro,age_group,Sex,Race,QuintMat,QuintSoc) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na() %>%
  left_join(df_C_vac, by='regpro')

poststrat_abc_race <-
  census_race %>%
  group_by(regpro,age_group,Sex,Race) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na() %>%
  left_join(df_C_vac, by='regpro')

df_C_abc <- df_C_abc[!is.na(df_C_abc$Race),]
df_C_cbs <- df_C_cbs[!is.na(df_C_cbs$age_group),]

poststrat_cbs <- poststrat_cbs_race
poststrat_abc <- poststrat_abc_race

poststrat_cbs$composite_strata <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, sep = "_")
poststrat_abc$composite_strata <- paste(poststrat_abc$age_group, poststrat_abc$Sex, sep = "_")
df_C_abc$composite_strata <- paste(df_C_abc$age_group, df_C_abc$Sex, sep = "_")
df_C_cbs$composite_strata <- paste(df_C_cbs$age_group, df_C_cbs$Sex, sep = "_")

poststrat_cbs$composite_strata_1 <- paste(poststrat_cbs$age_group, poststrat_cbs$Sex, poststrat_cbs$Race, sep = "_")
poststrat_abc$composite_strata_1 <- paste(poststrat_abc$age_group, poststrat_abc$Sex, poststrat_abc$Race, sep = "_")
df_C_abc$composite_strata_1 <- paste(df_C_abc$age_group, df_C_abc$Sex, df_C_abc$Race, sep = "_")
df_C_cbs$composite_strata_1 <- paste(df_C_cbs$age_group, df_C_cbs$Sex, df_C_cbs$Race, sep = "_")

# By province/region
regpro_df <- data.frame(
  regpro = c("BC","AB","MB","SK","ON","ATL"), 
  mrp_C_cbs = NA,
  mrp_C_cbs_se = NA,
  mrp_C_abc = NA,
  mrp_C_abc_se = NA,
  ua_C_cbs = NA,
  ua_C_cbs_se = NA,
  ua_C_abc = NA,
  ua_C_abc_se = NA,
  rake_C_cbs = NA,
  rake_C_cbs_se = NA,
  rake_C_abc = NA,
  rake_C_abc_se = NA,
  rake1_C_cbs = NA,
  rake1_C_cbs_se = NA,
  rake1_C_abc = NA,
  rake1_C_abc_se = NA,
  post_C_cbs = NA,
  post_C_cbs_se = NA,
  post_C_abc = NA,
  post_C_abc_se = NA,
  n_sample = NA,
  n_full = NA
)

for(i in 1:nrow(regpro_df)) {
  
  # MRP estimate B abc
  epred_mat <- posterior_epred(fit2_C_abc, newdata = poststrat_abc, draws = 4000)
  filtering_condition <- which(poststrat_abc$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_abc[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_C_abc[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_C_abc_se[i] <- sd(mrp_estimates_vector)
  
  # MRP estimate B cbs
  epred_mat <- posterior_epred(fit2_C_cbs, newdata = poststrat_cbs, draws = 4000)
  filtering_condition <- which(poststrat_cbs$regpro == regpro_df$regpro[i])
  regpro_epred_mat <- epred_mat[ ,filtering_condition]
  k_filtered <- poststrat_cbs[filtering_condition, ]$n
  mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
  regpro_df$mrp_C_cbs[i] <- mean(mrp_estimates_vector)
  regpro_df$mrp_C_cbs_se[i] <- sd(mrp_estimates_vector)
  
  # Unadjusted estimate B abc
  regpro_df$ua_C_abc[i] <- mean(filter(df_C_abc, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_C_abc, regpro==regpro_df$regpro[i]))
  regpro_df$ua_C_abc_se[i] <- get_se_bernoulli(regpro_df$ua_C_abc[i], regpro_df$n_sample[i])
  
  # Unadjusted estimate B cbs
  regpro_df$ua_C_cbs[i] <- mean(filter(df_C_cbs, regpro==regpro_df$regpro[i])$interp_roche_n)
  regpro_df$n_sample[i] <- nrow(filter(df_C_cbs, regpro==regpro_df$regpro[i]))
  regpro_df$ua_C_cbs_se[i] <- get_se_bernoulli(regpro_df$ua_C_cbs[i], regpro_df$n_sample[i])
  
  # Raking on age/sex B abc (RAKE)
  data <- df_C_abc[df_C_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_C_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_C_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex B cbs (RAKE)
  data <- df_C_cbs[df_C_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
  poststrat.dist <- data.frame(composite_strata = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake_C_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake_C_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex B abc (RAKE1)
  data <- df_C_abc[df_C_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata_1),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake1_C_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake1_C_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex B cbs (RAKE1)
  data <- df_C_cbs[df_C_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- rake(design = data.svy.unweighted,
                        sample.margins = list(~composite_strata_1),
                        population.margins = list(poststrat.dist))
  
  regpro_df$rake1_C_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$rake1_C_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A abc (POST)
  data <- df_C_abc[df_C_abc$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_abc[poststrat_abc$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_C_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_C_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
  
  # Raking on age/sex A cbs (POST)
  data <- df_C_cbs[df_C_cbs$regpro==regpro_df$regpro[i],]
  data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
  poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata_1,
                               Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)
  
  data.svy.rake <- postStratify(design = data.svy.unweighted,
                                ~composite_strata_1,
                                poststrat.dist, partial=TRUE)
  
  regpro_df$post_C_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
  regpro_df$post_C_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]
}

####### Adding this part for the result from all Canada ##########
poststrat_abc_all <-
  poststrat_abc_race %>%
  group_by(age_group,Sex,Race,vac) %>%
  dplyr::summarize(n = sum(n)) %>%
  drop_na()

poststrat_cbs_all <-
  poststrat_cbs_race %>%
  group_by(age_group,Sex,Race,QuintMat,QuintSoc,vac) %>%
  dplyr::summarize(n = sum(n))

df_C_abc <- df_C_abc[!is.na(df_C_abc$Race),]
df_C_cbs <- df_C_cbs[!is.na(df_C_cbs$age_group),]

poststrat_cbs <- poststrat_cbs_race
poststrat_abc <- poststrat_abc_race

poststrat_cbs_all$composite_strata <- paste(poststrat_cbs_all$age_group, poststrat_cbs$Sex, sep = "_")
poststrat_abc_all$composite_strata <- paste(poststrat_abc_all$age_group, poststrat_abc$Sex, sep = "_")
df_C_abc$composite_strata <- paste(df_C_abc$age_group, df_C_abc$Sex, sep = "_")
df_C_cbs$composite_strata <- paste(df_C_cbs$age_group, df_C_cbs$Sex, sep = "_")

poststrat_cbs_all$composite_strata_1 <- paste(poststrat_cbs_all$age_group, poststrat_cbs$Sex, poststrat_cbs$Race, sep = "_")
poststrat_abc_all$composite_strata_1 <- paste(poststrat_abc_all$age_group, poststrat_abc$Sex, poststrat_abc$Race, sep = "_")
df_C_abc$composite_strata_1 <- paste(df_C_abc$age_group, df_C_abc$Sex, df_C_abc$Race, sep = "_")
df_C_cbs$composite_strata_1 <- paste(df_C_cbs$age_group, df_C_cbs$Sex, df_C_cbs$Race, sep = "_")

# MRP estimate B abc
i=7
regpro_df[nrow(regpro_df) + 1 , ] <- NA
regpro_df[7, 1] <- 'CAN'
epred_mat <- posterior_epred(fit2_C_abc, newdata = poststrat_abc, draws = 4000) #fit2 denotes the model with vac added
filtering_condition <- seq(1,nrow(poststrat_abc))
regpro_epred_mat <- epred_mat[ ,filtering_condition]
k_filtered <- poststrat_abc[filtering_condition, ]$n
mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
regpro_df$mrp_C_abc[i] <- mean(mrp_estimates_vector)
regpro_df$mrp_C_abc_se[i] <- sd(mrp_estimates_vector)

# MRP estimate B cbs
epred_mat <- posterior_epred(fit2_C_cbs, newdata = poststrat_cbs, draws = 4000)
filtering_condition <- seq(1,nrow(poststrat_cbs))
regpro_epred_mat <- epred_mat[ ,filtering_condition]
k_filtered <- poststrat_cbs[filtering_condition, ]$n
mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
regpro_df$mrp_C_cbs[i] <- mean(mrp_estimates_vector)
regpro_df$mrp_C_cbs_se[i] <- sd(mrp_estimates_vector)

# Unadjusted estimate B abc
regpro_df$ua_C_abc[i] <- mean(df_C_abc$interp_roche_n)
regpro_df$n_sample[i] <- nrow(df_C_abc)
regpro_df$ua_C_abc_se[i] <- get_se_bernoulli(regpro_df$ua_C_abc[i], regpro_df$n_sample[i])

# Unadjusted estimate B cbs
regpro_df$ua_C_cbs[i] <- mean(df_C_cbs$interp_roche_n)
regpro_df$n_sample[i] <- nrow(df_C_cbs)
regpro_df$ua_C_cbs_se[i] <- get_se_bernoulli(regpro_df$ua_C_cbs[i], regpro_df$n_sample[i])

# Raking on age/sex B abc (RAKE)
data <- df_C_abc
data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
poststrat.dist <- data.frame(composite_strata = poststrat_abc_all$composite_strata,
                             Freq = poststrat_abc_all$n)

data.svy.rake <- rake(design = data.svy.unweighted,
                      sample.margins = list(~composite_strata),
                      population.margins = list(poststrat.dist))

regpro_df$rake_C_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
regpro_df$rake_C_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]

# Raking on age/sex B cbs (RAKE)
data <- df_C_cbs
data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
poststrat.dist <- data.frame(composite_strata = poststrat_cbs_all$composite_strata,
                             Freq = poststrat_cbs_all$n)

data.svy.rake <- rake(design = data.svy.unweighted,
                      sample.margins = list(~composite_strata),
                      population.margins = list(poststrat.dist))

regpro_df$rake_C_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
regpro_df$rake_C_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]

# Raking on age/sex B abc (RAKE1)
data <- df_C_abc
data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
poststrat.dist <- data.frame(composite_strata_1 = poststrat_abc_all$composite_strata_1,
                             Freq = poststrat_abc_all$n)

data.svy.rake <- rake(design = data.svy.unweighted,
                      sample.margins = list(~composite_strata_1),
                      population.margins = list(poststrat.dist))

regpro_df$rake1_C_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
regpro_df$rake1_C_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]

# Raking on age/sex B cbs (RAKE1)
data <- df_C_cbs
data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs_all$composite_strata_1,
                             Freq = poststrat_cbs_all$n)

data.svy.rake <- rake(design = data.svy.unweighted,
                      sample.margins = list(~composite_strata_1),
                      population.margins = list(poststrat.dist))

regpro_df$rake1_C_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
regpro_df$rake1_C_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]

# Raking on age/sex A abc (POST)
data <- df_C_abc
data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
poststrat.dist <- data.frame(composite_strata_1 = poststrat_abc_all$composite_strata_1,
                             Freq = poststrat_abc_all$n)

data.svy.rake <- postStratify(design = data.svy.unweighted,
                              ~composite_strata_1,
                              poststrat.dist, partial=TRUE)

regpro_df$post_C_abc[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
regpro_df$post_C_abc_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]

# Raking on age/sex A cbs (POST)
data <- df_C_cbs
data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata_1, data = data)
poststrat.dist <- data.frame(composite_strata_1 = poststrat_cbs_all$composite_strata_1,
                             Freq = poststrat_cbs_all$n)

data.svy.rake <- postStratify(design = data.svy.unweighted,
                              ~composite_strata_1,
                              poststrat.dist, partial=TRUE)

regpro_df$post_C_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
regpro_df$post_C_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]

####################################################################
write.csv(regpro_df, "/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_C_vac.csv")

# reload the dataframe in long format
regpro_df_C_vac <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_C_vac.csv')

## Reshape the dataframe from long to wide
regpro_df_C_vac <- data.frame(
  regpro = rep(c("BC","AB","MB","SK","ON","ATL","CAN"),10), 
  mrp_mean = NA,
  mrp_se = NA,
  lower = NA,
  upper = NA,
  model = NA,
  source = NA,
  setting = NA)

regpro_df_C_vac[, 2:3] <- rbind(as.matrix(regpro_df[1:7,c('mrp_C_cbs','mrp_C_cbs_se')]),
                                   as.matrix(regpro_df[1:7,c('ua_C_cbs','ua_C_cbs_se')]),
                                   as.matrix(regpro_df[1:7,c('rake_C_cbs','rake_C_cbs_se')]),
                                   as.matrix(regpro_df[1:7,c('rake1_C_cbs','rake1_C_cbs_se')]),
                                   as.matrix(regpro_df[1:7,c('post_C_cbs','post_C_cbs_se')]),
                                   as.matrix(regpro_df[1:7,c('mrp_C_abc','mrp_C_abc_se')]),
                                   as.matrix(regpro_df[1:7,c('ua_C_abc','ua_C_abc_se')]),
                                   as.matrix(regpro_df[1:7,c('rake_C_abc','rake_C_abc_se')]),
                                   as.matrix(regpro_df[1:7,c('rake1_C_abc','rake1_C_abc_se')]),
                                   as.matrix(regpro_df[1:7,c('post_C_abc','post_C_abc_se')]))
regpro_df_C_vac$lower <- regpro_df_C_vac$mrp_mean - regpro_df_C_vac$mrp_se*2
regpro_df_C_vac$upper <- regpro_df_C_vac$mrp_mean + regpro_df_C_vac$mrp_se*2
regpro_df_C_vac$model <- rep(c(rep("MRP",7),rep("UA",7),rep("RAKE",7),rep("RAKE1",7),rep("POST",7)),2)
regpro_df_C_vac$source <- c(rep('CBS',35),rep('ABC',35))
regpro_df_C_vac$setting <- rep('C',70)

desired_order <- c("UA","RAKE","RAKE1","POST","MRP")
desired_order1 <- c("BC","AB","MB","SK","ON","ATL")
desired_order2 <- c("CBS","ABC")

# Reorder the levels of the 'category' variable
regpro_df_C_vac$model <- factor(regpro_df_C_vac$model, levels = desired_order)
regpro_df_C_vac$regpro <- factor(regpro_df_C_vac$regpro, levels = desired_order1)
regpro_df_C_vac$source <- factor(regpro_df_C_vac$source, levels = desired_order2)

compare_C_vac <- ggplot(regpro_df_C_vac, aes(x = regpro, y = mrp_mean, colour=source, shape = model)) +
  theme_bw()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 2) +
  scale_color_manual(values = c("#F8766D", "#00BFC4","#B79F00","#619CFF","#00BA38","#F564E3")) +
  #  scale_color_manual(values = my_colors) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Seropositivity") +
  xlab("Province/Region") +
  ggtitle("Setting C (Jan-Apr, 2022) with Vaccination, CBS vs AbC") +
  theme(plot.title = element_text(vjust = 0))
compare_C_vac

regpro_df_C[regpro_df_C$source == 'CBS' & regpro_df_C$model == 'MRP',]$mrp_mean - 
regpro_df_C[regpro_df_C$source == 'ABC' & regpro_df_C$model == 'MRP',]$mrp_mean

regpro_df_C[regpro_df_C$source == 'CBS' & regpro_df_C$model == 'UA',]$mrp_mean - 
regpro_df_C[regpro_df_C$source == 'ABC' & regpro_df_C$model == 'UA',]$mrp_mean

regpro_df_C_vac[regpro_df_C_vac$source == 'CBS' & regpro_df_C_vac$model == 'MRP',]$mrp_mean - 
regpro_df_C_vac[regpro_df_C_vac$source == 'ABC' & regpro_df_C_vac$model == 'MRP',]$mrp_mean

regpro_df_C_vac[regpro_df_C_vac$source == 'CBS' & regpro_df_C_vac$model == 'UA',]$mrp_mean - 
regpro_df_C_vac[regpro_df_C_vac$source == 'ABC' & regpro_df_C_vac$model == 'UA',]$mrp_mean

write.csv(df_G_abc, "/home/yuanyu/projects/covid-donor-v-survey-antibody/df_G_abc.csv")


regpro_df3 <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df3.csv')