#Fig 6
library(rstanarm)
# Extract model parameter samples
parameter_samples <- as.matrix(fit_G_abc)
# Extract the posterior samples for predictor1
samples_Urban <- parameter_samples[, "Urban"]
# Calculate the odds ratio for predictor1
odds_ratio_Urban <- exp(samples_Urban)
mean(odds_ratio_Urban)
# Calculate credible intervals for the odds ratio of predictor1
odds_ratio_interval_Urban <- quantile(odds_ratio_Urban, c(0.025, 0.975))

# Extract model parameter samples
parameter_samples <- posterior_samples(fit_G_abc)
# Calculate the odds ratio for predictor1
odds_ratio_urban <- exp(mean(parameter_samples$Urban))

# Calculate the 95% credible interval for the odds ratio of predictor1
odds_ratio_interval_urban <- hdi(parameter_samples$Urban, hdi_prob = 0.95)



library(sjPlot)
theme_set(theme_sjplot())
plot_model(fit_G_abc)
#G
plot_model(fit_G_abc, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "Setting G (Jan- Apr 22), Alberta, AbC", , vline.color = "yellow")

plot_model(fit_G_cbs, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "Setting G (Jan- Apr 22), Alberta, CBS", , vline.color = "yellow")

plot_model(fit_G_apl, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "Setting G (Jan- Apr 22), Alberta, APL", , vline.color = "yellow")

#H
plot_model(fit_H_abc, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "Setting H (Mar-Apr & Jul-Aug, 2021), AbC", , vline.color = "yellow")

plot_model(fit_H_cbs, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "Setting H (Mar-Apr & Jul-Aug, 2021), CBS", , vline.color = "yellow")

plot_model(fit_H_clsa, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "Setting H (Mar-Apr & Jul-Aug, 2021), CLSA", , vline.color = "yellow")

#C
plot_model(fit_C_abc, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "Setting C (Jan-Apr, 2022), AbC", , vline.color = "yellow")

plot_model(fit_C_cbs, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "Setting C (Jan-Apr, 2022), CBS", , vline.color = "yellow")

#B
plot_model(fit_B_abc, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "Setting B (July-August, 2021), AbC", , vline.color = "yellow")

plot_model(fit_B_cbs, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "Setting B (July-August, 2021), CBS", , vline.color = "yellow")

#F
plot_model(fit_F_cbs, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "Setting F (Dec 20- Apr 21), CBS", , vline.color = "yellow")

#A
plot_model(fit1_stan, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "Setting A (Jan-Apr, 2021), AbC", , vline.color = "yellow")

plot_model(fit2_stan, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "Setting A (Jan-Apr, 2021), CBS", , vline.color = "yellow")

#A
plot_model(fit_A_abc, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "Setting A (Jan-Apr, 2021), AbC", , vline.color = "yellow")

plot_model(fit_A_cbs, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "Setting A (Jan-Apr, 2021), CBS", , vline.color = "yellow")

#E
plot_model(fit_E_cbs, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "Setting A (Apr-Oct, 2022), CBS", , vline.color = "yellow")

#
plot_model(fit_E_cbs, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "", , vline.color = "green")

plot_model(fit_C_abc_1, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "Setting C (Jan-Apr, 2022), AbC", vline.color = "green")
plot_model(fit_C_cbs_1, show.values = TRUE, value.offset = .3,bpe = "mean",
           prob.inner = .5, prob.outer = .95,
           bpe.style = "dot",title = "Setting C (Jan-Apr, 2022), CBS", vline.color = "green")



agg_H_abc <-
  df_H_abc %>%
  group_by(regpro) %>%
  summarize(case_counts = sum(interp_roche_n), n = n(), mrp_mean = mean(interp_roche_n)) %>%
  mutate(mrp_se = sqrt(mrp_mean*(1-mrp_mean)/n), 
         lower = mrp_mean - 2*mrp_se, upper = mrp_mean + 2*mrp_se, 
         model = 'RAW', source = 'ABC', setting = 'H') %>% 
  select (-c(case_counts, n)) %>% 
  drop_na()
#  summarise(n = n()) %>% mutate(rates = sum(interp_roche_n)/sum(n))

agg_H_cbs <-
  df_H_cbs %>%
  group_by(regpro) %>%
  summarize(case_counts = sum(interp_roche_n), n = n(), mrp_mean = mean(interp_roche_n)) %>%
  mutate(mrp_se = sqrt(mrp_mean*(1-mrp_mean)/n), 
         lower = mrp_mean - 2*mrp_se, upper = mrp_mean + 2*mrp_se, 
         model = 'RAW', source = 'CBS', setting = 'H') %>% 
  select (-c(case_counts, n)) %>% 
  drop_na()

agg_H_clsa <-
  df_H_clsa %>%
  group_by(regpro) %>%
  summarize(case_counts = sum(interp_roche_n), n = n(), mrp_mean = mean(interp_roche_n)) %>%
  mutate(mrp_se = sqrt(mrp_mean*(1-mrp_mean)/n), 
         lower = mrp_mean - 2*mrp_se, upper = mrp_mean + 2*mrp_se, 
         model = 'RAW', source = 'CLSA', setting = 'H') %>% 
  select (-c(case_counts, n)) %>% 
  drop_na()

agg_H <- rbind(agg_H_abc,agg_H_cbs,agg_H_clsa)

# reload the dataframe in long format
regpro_df_H <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_H.csv')

regpro_df_H <- regpro_df_H[1:72,]

regpro_df_H <- rbind(regpro_df_H, agg_H)

desired_order <- c("RAW","UA","RAKE","RAKE1","POST","MRP")
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
  scale_color_manual(values = c("#F8766D", "#00BFC4","#B79F00","#619CFF","#00BA38","#F564E3")) +
  #  scale_color_manual(values = my_colors) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Seropositivity") +
  xlab("Province/Region") +
  ggtitle("Setting H (Mar-Apr & Jul-Aug, 2021), CBS vs AbC vs CLSA") +
  theme(plot.title = element_text(vjust = 0))
compare_H

regpro_df_H1 <- regpro_df_H[1:72,]
regpro_df_H1$model <- factor(regpro_df_H1$model, levels = desired_order)
regpro_df_H1$regpro <- factor(regpro_df_H1$regpro, levels = desired_order1)
regpro_df_H1$source <- factor(regpro_df_H1$source, levels = desired_order2)

regpro_df_H <- regpro_df_H[order(regpro_df_H$source), ]
regpro_df_H1$relative_change <- (regpro_df_H1$mrp_mean - rep(regpro_df_H[regpro_df_H$model == 'RAW', ]$mrp_mean,4))/rep(regpro_df_H[regpro_df_H$model == 'RAW', ]$mrp_mean,4)

regpro_df_H1$cv <- regpro_df_H1$mrp_se/regpro_df_H1$mrp_mean

compare_H_cv <- ggplot(regpro_df_H1, aes(x = regpro, y = relative_change, colour=source, shape = model)) +
  theme_bw()+
#  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 2) +
  scale_color_manual(values = c("#F8766D", "#00BFC4","#B79F00","#619CFF","#00BA38","#F564E3")) +
  #  scale_color_manual(values = my_colors) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Relative Change of the Seropositivity") +
  xlab("Province/Region") +
  ggtitle("Setting H (Mar-Apr & Jul-Aug, 2021), CBS vs AbC vs CLSA") +
  theme(plot.title = element_text(vjust = 0))
compare_H_r

regpro_df_H1$cv <- regpro_df_H1$mrp_se/regpro_df_H1$mrp_mean
compare_H_cv <- ggplot(regpro_df_H1, aes(x = regpro, y = cv, colour=source, shape = model)) +
  theme_bw()+
  #  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 2) +
  scale_color_manual(values = c("#F8766D", "#00BFC4","#B79F00","#619CFF","#00BA38","#F564E3")) +
  #  scale_color_manual(values = my_colors) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Coefficient of Variation") +
  xlab("Province/Region") +
  ggtitle("Setting H (Mar-Apr & Jul-Aug, 2021), CBS vs AbC vs CLSA") +
  theme(plot.title = element_text(vjust = 0))
compare_H_cv

###### G 
agg_G_abc <-
  df_G_abc %>%
  group_by(regpro) %>%
  summarize(case_counts = sum(interp_roche_n), n = n(), mrp_mean = mean(interp_roche_n)) %>%
  mutate(mrp_se = sqrt(mrp_mean*(1-mrp_mean)/n), 
         lower = mrp_mean - 2*mrp_se, upper = mrp_mean + 2*mrp_se, 
         model = 'RAW', source = 'ABC', setting = 'G') %>% 
  select (-c(case_counts, n)) %>% 
  drop_na()
#  summarise(n = n()) %>% mutate(rates = sum(interp_roche_n)/sum(n))

agg_G_cbs <-
  df_G_cbs %>%
  group_by(regpro) %>%
  summarize(case_counts = sum(interp_roche_n), n = n(), mrp_mean = mean(interp_roche_n)) %>%
  mutate(mrp_se = sqrt(mrp_mean*(1-mrp_mean)/n), 
         lower = mrp_mean - 2*mrp_se, upper = mrp_mean + 2*mrp_se, 
         model = 'RAW', source = 'CBS', setting = 'G') %>% 
  select (-c(case_counts, n)) %>% 
  drop_na()

agg_G_apl <-
  df_G_apl %>%
  group_by(regpro) %>%
  summarize(case_counts = sum(interp_roche_n), n = n(), mrp_mean = mean(interp_roche_n)) %>%
  mutate(mrp_se = sqrt(mrp_mean*(1-mrp_mean)/n), 
         lower = mrp_mean - 2*mrp_se, upper = mrp_mean + 2*mrp_se, 
         model = 'RAW', source = 'APL', setting = 'G') %>% 
  select (-c(case_counts, n)) %>% 
  drop_na()

agg_G <- rbind(agg_G_cbs,agg_G_abc,agg_G_apl)

regpro_df_G <-  regpro_df_G[1:15,]
regpro_df_G <-  rbind(regpro_df_G, agg_G)

desired_order <- c("RAW","UA","RAKE","RAKE1","POST","MRP")
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

regpro_df_G1 <- regpro_df_G
regpro_df_G1$relative_change <- (regpro_df_G1$mrp_mean - rep(regpro_df_G[regpro_df_G$model == 'RAW', ]$mrp_mean,6))/rep(regpro_df_G[regpro_df_G$model == 'RAW', ]$mrp_mean,6)

regpro_df_G1$cv <- regpro_df_G1$mrp_se/regpro_df_G1$mrp_mean

compare_G_r <- ggplot(regpro_df_G1, aes(x = regpro, y = relative_change, colour=source, shape = model)) +
  theme_bw()+
#  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 2) +
  scale_color_manual(values = c("#F8766D", "#00BFC4","#619CFF","#00BA38","#F564E3")) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Relative Change of Seropositivity") +
  xlab("Province/Region") +
  ggtitle("Setting G (Jan- Apr 22), CBS vs AbC vs APL, Alberta") +
  theme(plot.title = element_text(vjust = 0))
compare_G_r

compare_G1_cv <- ggplot(regpro_df_G1, aes(x = regpro, y = cv, colour=source, shape = model)) +
  theme_bw()+
#  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 2) +
  scale_color_manual(values = c("#F8766D", "#00BFC4","#619CFF","#00BA38","#F564E3")) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Coefficient of Variation") +
  xlab("Province/Region") +
  ggtitle("Setting G (Jan- Apr 22), CBS vs AbC vs APL, Alberta") +
  theme(plot.title = element_text(vjust = 0))
compare_G1_cv
