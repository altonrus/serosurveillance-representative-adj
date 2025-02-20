# Provincial estimates VS CBS through BLCA
df_prev <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/Draws_prevalence.csv')
df_sens <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/Draws_sensitivity.csv')
df_spec <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/Draws_specificity.csv')

df_prev <- tail(df_prev,50000)[,2]
df_sens <- tail(df_sens,50000)
df_spec <- tail(df_spec,50000)

df_cbs_Roche_all <-
  data_all %>%
  mutate(Month = floor_date(sampledate, unit = "month"),
         Province = province_fun(fsa),
         age = 2022 - dob,
         age_group = cut(age,
                         breaks = c(18,27,37,47,57,Inf),
                         labels = c("18-26 years",
                                    "27-36 years","37-46 years","47-56 years",
                                    "56+ years"), right = FALSE),
         Sex = case_when(sex == 'F' ~ 0, 
                         sex == 'M' ~ 1,
                         TRUE~NA),
         Race = case_when(ethnic1 == "1 White" ~ 1, 
                          ethnic1 %in% c("3 Asian", "4 Others", "4 Other") ~ 0,
                          TRUE~NA),
         Urban = factor(case_when(as.character(substr(fsa,start = 2,stop = 2)) != "0" ~ 1, 
                                  as.character(substr(fsa,start = 2,stop = 2)) == "0" ~ 0,
                                  TRUE~NA)),
         regpro = factor(case_when(as.character(substr(fsa,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                                   as.character(substr(fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                                   as.character(substr(fsa,1,1)) %in% c("T")~"AB",
                                   as.character(substr(fsa,1,1)) %in% c("R")~"MB",
                                   as.character(substr(fsa,1,1)) %in% c("S")~"SK",
                                   as.character(substr(fsa,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK")),
         day = case_when(year(sampledate) == 2020 ~ yday(sampledate),
                         year(sampledate) == 2021 ~ (yday(sampledate) + 366),
                         year(sampledate) == 2022 ~ (yday(sampledate)) + 366 + 365),
         week = week(sampledate) + (year(sampledate)-2020)*53,
         month = month(sampledate) + (year(sampledate)-2020)*12,
         year = year(sampledate),
         assay = "Roche",
         serum = 1,
         study = "CBS",
         yearmon = zoo::as.yearmon(sampledate)) %>%
  filter(Province != ("YT") & Province !=("NU/NT") & Province != ("QC"), Province == ("AB"),
         sampledate <= '2021-12-31',
         !is.na(age_group), !is.na(Sex), !is.na(Race), !is.na(interp_roche_n)) %>% 
  select(all_of(column_list))



df_cbs_Abbott_all <-
  data_all %>%
  mutate(Month = floor_date(sampledate, unit = "month"),
         Province = province_fun(fsa),
         age = 2022 - dob,
         age_group = cut(age,
                         breaks = c(18,27,37,47,57,Inf),
                         labels = c("18-26 years",
                                    "27-36 years","37-46 years","47-56 years",
                                    "56+ years"), right = FALSE),
         Sex = case_when(sex == 'F' ~ 0, 
                         sex == 'M' ~ 1,
                         TRUE~NA),
         Race = case_when(ethnic1 == "1 White" ~ 1, 
                          ethnic1 %in% c("3 Asian", "4 Others", "4 Other") ~ 0,
                          TRUE~NA),
         Urban = factor(case_when(as.character(substr(fsa,start = 2,stop = 2)) != "0" ~ 1, 
                                  as.character(substr(fsa,start = 2,stop = 2)) == "0" ~ 0,
                                  TRUE~NA)),
         regpro = factor(case_when(as.character(substr(fsa,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                                   as.character(substr(fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                                   as.character(substr(fsa,1,1)) %in% c("T")~"AB",
                                   as.character(substr(fsa,1,1)) %in% c("R")~"MB",
                                   as.character(substr(fsa,1,1)) %in% c("S")~"SK",
                                   as.character(substr(fsa,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK")),
         day = case_when(year(sampledate) == 2020 ~ yday(sampledate),
                         year(sampledate) == 2021 ~ (yday(sampledate) + 366),
                         year(sampledate) == 2022 ~ (yday(sampledate)) + 366 + 365),
         week = week(sampledate) + (year(sampledate)-2020)*53,
         month = month(sampledate) + (year(sampledate)-2020)*12,
         year = year(sampledate),
         assay = "Abbott",
         serum = 1,
         study = "CBS",
         interp_roche_n = case_when(interp_abbott_n == "Negative" ~ 0, 
                                    interp_abbott_n == "Positive" ~ 1,
                                    TRUE~NA),
         yearmon = zoo::as.yearmon(sampledate)) %>%
  filter(Province != ("YT") & Province !=("NU/NT") & Province != ("QC"), Province == ("AB"), 
         #sampledate >= '2020-11-01', 
         !is.na(age_group), !is.na(Sex), !is.na(Race), !is.na(interp_abbott_n),
         interp_abbott_n != "") %>% 
  select(all_of(column_list))


df_joint_apl1 <- df_joint_apl %>% filter(regpro=='AB')


### CBS Abbott
ts_abbott_cbs <-
  df_cbs_Abbott_all %>%
  group_by(regpro,yearmon) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n), total_counts = n())

### APL
ts_abbott_apl <-
  df_joint_apl %>%
  group_by(regpro,yearmon) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n), total_counts = n()) %>% 
  filter(regpro=='AB')

### CBS Abbott
ts_roche_cbs <-
  df_cbs_Roche_all %>%
  group_by(regpro,yearmon) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n), total_counts = n())

#result_vector <- apply(df, 1, my_function)

get_prev_abbott <- function(x){
  (x + df_spec$Abbott - 1)/(df_spec$Abbott + df_sens$Abbott - 1)
}

get_prev_roche <- function(x){
  (x + df_spec$Roche - 1)/(df_spec$Roche + df_sens$Roche - 1)
}

mean(get_prev_abbott(0.05))
quantile(get_prev_abbott(0.05), probs=0.025)
quantile(get_prev_abbott(0.05), probs=0.975)


#### 1000 bootstrap samples
install.packages("boot")
library(boot)


stat_function <- function(data, indices) {
  sample_data <- data[indices, ]
  return(mean(sample_data$interp_roche_n))
}

result <- boot(data = df_cbs_Abbott_all, statistic = stat_function, R = 100)

set.seed(123)

stat_function1 <- function(data, indices) {
  resampled_data <- data[indices, ]
  aggregate(resampled_data$interp_roche_n, by = list(resampled_data$yearmon), FUN = mean)$x
}

# Run the bootstrap within each group
boot_results <- boot(data = df_cbs_Abbott_all, statistic = stat_function1, 
                     R = 100, strata = df_cbs_Abbott_all$yearmon)

# boot_results <- boot(data = df_joint_apl1, statistic = stat_function1, 
#                      R = 100, strata = df_joint_apl1$yearmon)
# 
# boot_results <- boot(data = df_cbs_Roche_all, statistic = stat_function1, 
#                      R = 100, strata = df_cbs_Roche_all$yearmon)
## Compare the distance between the two time series and evaluate if the adjusted 
## seropositivity get closer

class(boot_results$t)

mat <- boot_results$t
mat_list <- matrix(list(), nrow = nrow(mat), ncol = ncol(mat))

for (i in 1:nrow(mat)) {
  for (j in 1:ncol(mat)) {
    mat_list[[i, j]] <- get_prev_roche(mat[i, j])  # Replace with your function
  }
}

dim(mat_list)

unlisted_columns <- apply(mat_list, 2, function(col) unlist(col))


#model : obs/obs_rg/boot/boot_rg
#sample_mean,lower, upper


#obs_rg
output <- matrix(ncol=7, nrow=ncol(mat))

df_blca <- as.data.frame(output)
colnames(df_blca) <- c('yearmon','sample_mean', 'lower', 'upper', 'model', 'assay', 'source')
df_blca$yearmon <- as.character(ts_abbott_cbs$yearmon)
df_blca$sample_mean <- sapply(ts_abbott_cbs$rates, function(x) max(mean(get_prev_abbott(x)),0))
df_blca$lower <- sapply(ts_abbott_cbs$rates, function(x) max(quantile(get_prev_abbott(x), probs=0.025),0))
df_blca$upper <- sapply(ts_abbott_cbs$rates, function(x) max(quantile(get_prev_abbott(x), probs=0.975),0))
df_blca$model <- 'obs_rg'
df_blca$assay <- 'Abbott'
df_blca$source <- 'CBS'

df_blca_cbs <- df_blca

output <- matrix(ncol=7, nrow=ncol(mat))

#obs
df_blca <- as.data.frame(output)
colnames(df_blca) <- c('yearmon','sample_mean', 'lower', 'upper', 'model', 'assay', 'source')
df_blca$yearmon <- as.character(ts_abbott_cbs$yearmon)
df_blca$sample_mean <- ts_abbott_cbs$rates
df_blca$lower <- sapply(df_blca$sample_mean - 2*sqrt(ts_abbott_cbs$rates*(1-ts_abbott_cbs$rates)/ts_abbott_cbs$case_counts), function(x) max(x,0))
df_blca$upper <- sapply(df_blca$sample_mean + 2*sqrt(ts_abbott_cbs$rates*(1-ts_abbott_cbs$rates)/ts_abbott_cbs$case_counts), function(x) max(x,0))
df_blca$model <- 'obs'
df_blca$assay <- 'Abbott'
df_blca$source <- 'CBS'
df_blca_cbs <- rbind(df_blca_cbs, df_blca)

#boot
mat <- boot_results$t
mat[mat < 0] <- 0
df_blca <- as.data.frame(output)
colnames(df_blca) <- c('yearmon','sample_mean', 'lower', 'upper', 'model', 'assay', 'source')
df_blca$yearmon <- as.character(ts_abbott_cbs$yearmon)
df_blca$sample_mean <- apply(mat, 2, mean)
df_blca$lower <- apply(mat, 2, function(x) quantile(x, probs=0.025))
df_blca$upper <- apply(mat, 2, function(x) quantile(x, probs=0.975))
df_blca$model <- 'boot'
df_blca$assay <- 'Abbott'
df_blca$source <- 'CBS'
df_blca_cbs <- rbind(df_blca_cbs, df_blca)

#boot_rg

mat2 <- unlisted_columns
mat2[mat2 < 0] <- 0
df_blca <- as.data.frame(output)
colnames(df_blca) <- c('yearmon','sample_mean', 'lower', 'upper', 'model', 'assay', 'source')
df_blca$yearmon <- as.character(ts_abbott_cbs$yearmon)
df_blca$sample_mean <- apply(mat2, 2, mean)
df_blca$lower <- apply(mat2, 2, function(x) quantile(x, probs=0.025))
df_blca$upper <- apply(mat2, 2, function(x) quantile(x, probs=0.975))
df_blca$model <- 'boot_rg'
df_blca$assay <- 'Abbott'
df_blca$source <- 'CBS'
df_blca_cbs <- rbind(df_blca_cbs, df_blca)


df_blca_ab <- rbind(df_blca_cbs, df_blca_apl , df_blca_cbs_r)  
desired_order <- unique(df_blca_ab$yearmon)
desired_order1 <- c("obs", "boot", "obs_rg", "boot_rg")

# Reorder the levels of the 'category' variable
df_blca_ab$yearmon <- factor(df_blca_ab$yearmon, levels = desired_order)
df_blca_ab$model <- factor(df_blca_ab$model, levels = desired_order1)

compare_blca <- ggplot(df_blca_ab, aes(x = yearmon, y = sample_mean, colour=source, shape = model)) +
  theme_bw()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 2) +
  scale_color_manual(values = c("#F8766D", "#00BFC4","#00BA38")) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_shape_manual(
    name = "Adjustment",
    values = c("obs" = 3, 
               "boot" = 16 ,
               "obs_rg" = 17, 
               "boot_rg" = 15),
    labels = c("obs" = "No adjustment",   
               "boot" = "No adjustment, boot",
               "obs_rg" = "BLCA adjusted", 
               "boot_rg" = "BLCA adjusted, boot"))+
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Seropositivity") +
  xlab("Month") +
  ggtitle("CBS/APL,  Abbott/Roche") +
  theme(plot.title = element_text(vjust = 0))
compare_blca

df_blca_cbs[df_blca_cbs$source=='CBS',]$source <- 'CBS_Abbott'


# Run the bootstrap within each group

boot_results <- boot(data = df_joint_apl1, statistic = stat_function1, 
                     R = 100, strata = df_joint_apl1$yearmon)

## Compare the distance between the two time series and evaluate if the adjusted 
## seropositivity get closer

class(boot_results$t)

mat <- boot_results$t
mat_list <- matrix(list(), nrow = nrow(mat), ncol = ncol(mat))

for (i in 1:nrow(mat)) {
  for (j in 1:ncol(mat)) {
    mat_list[[i, j]] <- get_prev_roche(mat[i, j])  # Replace with your function
  }
}

dim(mat_list)

unlisted_columns <- apply(mat_list, 2, function(col) unlist(col))
#obs_rg
output <- matrix(ncol=7, nrow=ncol(mat))

df_blca <- as.data.frame(output)
colnames(df_blca) <- c('yearmon','sample_mean', 'lower', 'upper', 'model', 'assay', 'source')
df_blca$yearmon <- as.character(ts_abbott_apl$yearmon)
df_blca$sample_mean <- sapply(ts_abbott_apl$rates, function(x) max(mean(get_prev_abbott(x)),0))
df_blca$lower <- sapply(ts_abbott_apl$rates, function(x) max(quantile(get_prev_abbott(x), probs=0.025),0))
df_blca$upper <- sapply(ts_abbott_apl$rates, function(x) max(quantile(get_prev_abbott(x), probs=0.975),0))
df_blca$model <- 'obs_rg'
df_blca$assay <- 'Abbott'
df_blca$source <- 'APL'

df_blca_apl <- df_blca

#obs
df_blca <- as.data.frame(output)
colnames(df_blca) <- c('yearmon','sample_mean', 'lower', 'upper', 'model', 'assay', 'source')
df_blca$yearmon <- as.character(ts_abbott_apl$yearmon)
df_blca$sample_mean <- ts_abbott_apl$rates
df_blca$lower <- sapply(df_blca$sample_mean - 2*sqrt(ts_abbott_apl$rates*(1-ts_abbott_apl$rates)/ts_abbott_apl$case_counts), function(x) max(x,0))
df_blca$upper <- sapply(df_blca$sample_mean + 2*sqrt(ts_abbott_apl$rates*(1-ts_abbott_apl$rates)/ts_abbott_apl$case_counts), function(x) max(x,0))
df_blca$model <- 'obs'
df_blca$assay <- 'Abbott'
df_blca$source <- 'APL'
df_blca_apl <- rbind(df_blca_apl, df_blca)

#boot
mat <- boot_results$t
mat[mat < 0] <- 0
df_blca <- as.data.frame(output)
colnames(df_blca) <- c('yearmon','sample_mean', 'lower', 'upper', 'model', 'assay', 'source')
df_blca$yearmon <- as.character(ts_abbott_apl$yearmon)
df_blca$sample_mean <- apply(mat, 2, mean)
df_blca$lower <- apply(mat, 2, function(x) quantile(x, probs=0.025))
df_blca$upper <- apply(mat, 2, function(x) quantile(x, probs=0.975))
df_blca$model <- 'boot'
df_blca$assay <- 'Abbott'
df_blca$source <- 'APL'
df_blca_apl <- rbind(df_blca_apl, df_blca)

#boot_rg

mat2 <- unlisted_columns
mat2[mat2 < 0] <- 0
df_blca <- as.data.frame(output)
colnames(df_blca) <- c('yearmon','sample_mean', 'lower', 'upper', 'model', 'assay', 'source')
df_blca$yearmon <- as.character(ts_abbott_apl$yearmon)
df_blca$sample_mean <- apply(mat2, 2, mean)
df_blca$lower <- apply(mat2, 2, function(x) quantile(x, probs=0.025))
df_blca$upper <- apply(mat2, 2, function(x) quantile(x, probs=0.975))
df_blca$model <- 'boot_rg'
df_blca$assay <- 'Abbott'
df_blca$source <- 'APL'
df_blca_apl <- rbind(df_blca_apl, df_blca)

########### cbs roche
boot_results <- boot(data = df_cbs_Roche_all, statistic = stat_function1, 
                     R = 100, strata = df_cbs_Roche_all$yearmon)
## Compare the distance between the two time series and evaluate if the adjusted 
## seropositivity get closer

class(boot_results$t)

mat <- boot_results$t
mat_list <- matrix(list(), nrow = nrow(mat), ncol = ncol(mat))

for (i in 1:nrow(mat)) {
  for (j in 1:ncol(mat)) {
    mat_list[[i, j]] <- get_prev_roche(mat[i, j])  # Replace with your function
  }
}

dim(mat_list)

unlisted_columns <- apply(mat_list, 2, function(col) unlist(col))

#obs_rg
output <- matrix(ncol=7, nrow=ncol(mat))

df_blca <- as.data.frame(output)
colnames(df_blca) <- c('yearmon','sample_mean', 'lower', 'upper', 'model', 'assay', 'source')
df_blca$yearmon <- as.character(ts_roche_cbs$yearmon)
df_blca$sample_mean <- sapply(ts_roche_cbs$rates, function(x) max(mean(get_prev_roche(x)),0))
df_blca$lower <- sapply(ts_roche_cbs$rates, function(x) max(quantile(get_prev_roche(x), probs=0.025),0))
df_blca$upper <- sapply(ts_roche_cbs$rates, function(x) max(quantile(get_prev_roche(x), probs=0.975),0))
df_blca$model <- 'obs_rg'
df_blca$assay <- 'Roche'
df_blca$source <- 'CBS'

df_blca_cbs_r <-df_blca

output <- matrix(ncol=7, nrow=ncol(mat))

#obs
df_blca <- as.data.frame(output)
colnames(df_blca) <- c('yearmon','sample_mean', 'lower', 'upper', 'model', 'assay', 'source')
df_blca$yearmon <- as.character(ts_roche_cbs$yearmon)
df_blca$sample_mean <- ts_roche_cbs$rates
df_blca$lower <- sapply(df_blca$sample_mean - 2*sqrt(ts_roche_cbs$rates*(1-ts_roche_cbs$rates)/ts_roche_cbs$case_counts), function(x) max(x,0))
df_blca$upper <- sapply(df_blca$sample_mean + 2*sqrt(ts_roche_cbs$rates*(1-ts_roche_cbs$rates)/ts_roche_cbs$case_counts), function(x) max(x,0))
df_blca$model <- 'obs'
df_blca$assay <- 'Roche'
df_blca$source <- 'CBS'
df_blca_cbs_r <- rbind(df_blca_cbs_r, df_blca)

#boot
mat <- boot_results$t
mat[mat < 0] <- 0
#df_blca <- as.data.frame(output)
colnames(df_blca) <- c('yearmon','sample_mean', 'lower', 'upper', 'model', 'assay', 'source')
df_blca$yearmon <- as.character(ts_roche_cbs$yearmon)
df_blca$sample_mean <- apply(mat, 2, mean)
df_blca$lower <- apply(mat, 2, function(x) quantile(x, probs=0.025))
df_blca$upper <- apply(mat, 2, function(x) quantile(x, probs=0.975))
df_blca$model <- 'boot'
df_blca$assay <- 'Roche'
df_blca$source <- 'CBS'
df_blca_cbs_r <- rbind(df_blca_cbs_r, df_blca)

#boot_rg

mat2 <- unlisted_columns
mat2[mat2 < 0] <- 0
#df_blca <- as.data.frame(output)
colnames(df_blca) <- c('yearmon','sample_mean', 'lower', 'upper', 'model', 'assay', 'source')
df_blca$yearmon <- as.character(ts_roche_cbs$yearmon)
df_blca$sample_mean <- apply(mat2, 2, mean)
df_blca$lower <- apply(mat2, 2, function(x) quantile(x, probs=0.025))
df_blca$upper <- apply(mat2, 2, function(x) quantile(x, probs=0.975))
df_blca$model <- 'boot_rg'
df_blca$assay <- 'Roche'
df_blca$source <- 'CBS'
df_blca_cbs_r <- rbind(df_blca_cbs_r, df_blca)



compare_blca_1 <- ggplot(df_blca_ab[df_blca_ab$yearmon %in% head(unique(df_blca_ab$yearmon),9), ], aes(x = yearmon, y = sample_mean, colour=source, shape = model)) +
  theme_bw()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=pd_1) +
  geom_point(position=pd_1,  size = 1.6) +
  scale_color_manual(values = c("#F8766D", "#00BFC4","#00BA38")) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_shape_manual(
    name = "Adjustment",
    values = c("obs" = 16, 
               "boot" = 17 ,
               "obs_rg" = 15, 
               "boot_rg" = 3),
    labels = c("obs" = "No adjustment",   
               "boot" = "No adjustment, boot",
               "obs_rg" = "BLCA adjusted", 
               "boot_rg" = "BLCA adjusted, boot")) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Seropositivity") +
  xlab("Month") +
  ggtitle("CBS/APL,  Abbott/Roche, Alberta") +
  theme(plot.title = element_text(vjust = 0))
compare_blca_1

dim(df_blca_ab)




df_samplesize_apl <- ts_abbott_apl[,c(1,2,5)] %>%
  full_join(ts_abbott_cbs[,c(1,2,5)], by = "yearmon") %>%
  full_join(ts_roche_cbs[,c(1,2,5)], by = "yearmon")


df_samplesize_sk <- ts_abbott_sk[,c(1,2,5)] %>%
  full_join(ts_abbott_cbs[,c(1,2,5)], by = "yearmon") %>%
  full_join(ts_roche_cbs[,c(1,2,5)], by = "yearmon")

df_samplesize_mb <- ts_abbott_mcs[,c(1,2,5)] %>%
  full_join(ts_abbott_cbs[,c(1,2,5)], by = "yearmon") %>%
  full_join(ts_roche_cbs[,c(1,2,5)], by = "yearmon")



