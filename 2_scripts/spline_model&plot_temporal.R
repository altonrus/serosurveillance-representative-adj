# Fig 3 & Fig 5
library("splines")
library("rstan")
X <- seq(from=-5, to=5, by=.1) # generating inputs
B <- t(bs(X, knots=seq(-5,5,1), degree=3, intercept = TRUE)) # creating the B-splines
num_data <- length(X); num_basis <- nrow(B)
a0 <- 0.2 # intercept
a <- rnorm(num_basis, 0, 1) # coefficients of B-splines
Y_true <- as.vector(a0*X + a%*%B) # generating the output
Y <- Y_true + rnorm(length(X),0,.2) # adding noise
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
sm<-stan_model("fit_basis.stan")
fit<-sampling(sm,iter=500,control = list(adapt_delta=0.95))

####

library("splines")
library("rstan")
num_knots <- 10
spline_degree <- 3
num_basis <- num_knots + spline_degree - 1
X <- seq(from=-5, to=5, by=.1)
num_data <- length(X)
knots <- unname(quantile(X,probs=seq(from=0, to=1, length.out = num_knots)))
a0 <- 0.2
a <- rnorm(num_basis, 0, 1)
B_true <- t(bs(X, df=num_basis, degree=spline_degree, intercept = TRUE))
Y_true <- as.vector(a0*X + a%*%B_true)
Y <- Y_true + rnorm(length(X), 0, 0.2)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
spline_model <- stan_model("b_spline.stan")
fit_spline <- sampling(spline_model, iter=500, control = list(adapt_delta=0.95))




# Assuming your datetime variable is in POSIXct format
df_G_apl$COLLECTION_week <- as.numeric(df_G_apl$COLLECTION_DATE - min(df_G_apl$COLLECTION_DATE), units = "weeks")
df_G_apl$COLLECTION_day <- as.numeric(df_G_apl$COLLECTION_DATE - min(df_G_apl$COLLECTION_DATE), units = "days")
x_spline <- bs(df_G_apl$COLLECTION_week, knots = seq(0, 1.7143, 0.1428571), degree = 3)

start_time <- Sys.time()
fit_G_apl_spline <- stan_glm(interp_roche_n ~ Urban + age_group + Sex + x_spline,
                      family = binomial,
                      data = df_G_apl,
                      cores = 4,
                      seed = 1010, adapt_delta = 0.8)
end_time <- Sys.time()
end_time - start_time
print(fit_G_apl_spline)

spline_predictions <- predict(fit_G_apl_spline, newdata = df_G_apl, type = "link", allow.new.levels = TRUE)
model_predictions <- predict(fit_G_apl_spline, newdata = df_G_apl, type = "response", allow.new.levels = TRUE)
combined_data <- data.frame(df_G_apl, spline_predictions, model_predictions)

combined_data <- data.frame(weeks = df_G_apl$COLLECTION_week, predictions = model_predictions, source = rep('APL', nrow(df_G_apl)))

library(ggplot2)

ggplot(combined_data, aes(x = COLLECTION_week, y = spline_predictions)) +
#  geom_line(color = "blue", linetype = "dashed") +
  geom_line(aes(y = model_predictions), color = "red") +
#  geom_smooth(aes(y = model_predictions), method = "loess", color = "green") +
  geom_smooth(aes(y = model_predictions), method = "gam", color = "blue") +
  labs(title = "Spline and Model Predictions",
       x = "week",
       y = "Seropositivity") +
  theme_minimal()

#AbC
df_G_abc$weeks <- as.numeric(df_G_abc$p4_dbs_received_date - as.Date('2022-01-24'), units = "weeks")
df_G_abc$days <- as.numeric(df_G_abc$p4_dbs_received_date - min(df_G_abc$p4_dbs_received_date), units = "days")
x_spline <- bs(df_G_abc$weeks, knots = sort(unique(df_G_abc$weeks)), degree = 3)

start_time <- Sys.time()
fit_G_abc_spline <- stan_glm(interp_roche_n ~ Urban + age_group + Sex + x_spline,
                             family = binomial,
                             data = df_G_abc,
                             cores = 4,
                             seed = 1010, adapt_delta = 0.8)
end_time <- Sys.time()
end_time - start_time
print(fit_G_abc_spline)

#cbs
df_G_cbs$weeks <- as.numeric(df_G_cbs$sampledate - as.Date('2022-01-24'), units = "weeks")
#df_G_abc$days <- as.numeric(df_G_abc$p4_dbs_received_date - min(df_G_abc$p4_dbs_received_date), units = "days")
x_spline <- bs(df_G_cbs$weeks, knots = seq(0, 11.1428571, 0.1428571), degree = 3)

start_time <- Sys.time()
fit_G_cbs_spline <- stan_glm(interp_roche_n ~ Urban + age_group + Sex + x_spline,
                             family = binomial,
                             data = df_G_cbs,
                             cores = 4,
                             seed = 1010, adapt_delta = 0.8)
end_time <- Sys.time()
end_time - start_time
print(fit_G_cbs_spline)

##########


library(ggplot2)

ggplot(combined_data, aes(x = weeks, y = model_predictions)) +
  #  geom_line(color = "blue", linetype = "dashed") +
  geom_line(aes(y = model_predictions), color = "red") +
  #  geom_smooth(aes(y = model_predictions), method = "loess", color = "green") +
  geom_smooth(aes(y = model_predictions), method = "gam", color = "blue") +
  labs(title = "CBS Spline Predictions",
       x = "week",
       y = "Seropositivity") +
  theme_minimal()

x_spline <- bs(df_G_cbs$weeks, knots = seq(0, 11.1428571, 0.1428571), degree = 3)
model_predictions <- predict(fit_G_cbs_spline, newdata = df_G_cbs, type = "response", allow.new.levels = TRUE)
combined_data_cbs <- data.frame(weeks = df_G_cbs$weeks, predictions = model_predictions, source = rep('CBS', nrow(df_G_cbs)))

G_spline_combined <- rbind(G_spline_combined, combined_data)

ggplot(G_spline_combined, aes(x = weeks, y = predictions), color = source) +
  #  geom_line(color = "blue", linetype = "dashed") +
  #geom_line(aes(y = model_predictions), color = "red") +
  #  geom_smooth(aes(y = model_predictions), method = "loess", color = "green") +
  geom_smooth(aes(y = model_predictions, color = source), method = "loess") +
  labs(title = "Spline Predictions",
       x = "week",
       y = "Seropositivity") +
  theme_minimal()


ggplot(G_spline_combined, aes(x = weeks, color=source)) +
  # Add the first smoothed spline for x1
  geom_smooth(aes(y = predictions), method = "loess") +
  # Add the second smoothed spline for x2
#  geom_smooth(aes(y = predictions), method = "gam", color = "red") +
  # Other plot customization (titles, labels, etc.)
  labs(title = "Two Smoothed Splines",
       x = "X-axis",
       y = "Y-axis") +
  theme_minimal()

# Moving Average Time Series Plot
ts_G_apl <-
  df_G_apl %>%
  group_by(regpro,COLLECTION_DATE) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

install.packages("zoo")
install.packages("ggplot2")
library(zoo)
library(ggplot2)
# Create a zoo object from the time series data
time_series <- zoo(ts_G_apl$rates, order.by = ts_G_apl$COLLECTION_DATE)
# Apply a simple moving average with a window size of 5
smoothed_data <- rollapply(time_series, width = 5, FUN = mean, align = "center", fill = NA)
plot_data <- data.frame(date = index(smoothed_data), value = coredata(smoothed_data))
ggplot(plot_data, aes(x = date, y = value)) +
  geom_line(color = "green", size = 1) +
#  geom_line(data = ts_G_apl, aes(x = COLLECTION_DATE, y = rates), color = "gray", size = 0.7) +
  geom_smooth(data = ts_G_apl, aes(x = COLLECTION_DATE, y = rates), method = "loess") +
  labs(title = "APL, Time Series Plot with Moving Average",
       x = "Date",
       y = "Anti-N Seropositivity") +
  theme_minimal()


#abc
ts_G_abc <-
  df_G_abc %>%
  group_by(regpro,p4_dbs_received_date) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

# Create a zoo object from the time series data
time_series <- zoo(ts_G_abc$rates, order.by = ts_G_abc$p4_dbs_received_date)
# Apply a simple moving average with a window size of 5
smoothed_data <- rollapply(time_series, width = 5, FUN = mean, align = "center", fill = NA)
plot_data <- data.frame(date = index(smoothed_data), value = coredata(smoothed_data))
ggplot(plot_data, aes(x = date, y = value)) +
  geom_line(color = "green", size = 1) +
  #  geom_line(data = ts_G_apl, aes(x = COLLECTION_DATE, y = rates), color = "gray", size = 0.7) +
  geom_smooth(data = ts_G_abc, aes(x = p4_dbs_received_date, y = rates), method = "loess") +
  labs(title = "ABC, Time Series Plot with Moving Average",
       x = "Date",
       y = "Anti-N Seropositivity") +
  theme_minimal()

### ABC
df_G_abc$week <- week(df_G_abc$p4_dbs_received_date)
ts_G_abc <-
  df_G_abc %>%
  group_by(regpro,week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

# Create a zoo object from the time series data
time_series <- zoo(ts_G_abc$rates, order.by = ts_G_abc$week)
# Apply a simple moving average with a window size of 5
smoothed_data <- rollapply(time_series, width = 5, FUN = mean, align = "center", fill = NA)
plot_data <- data.frame(date = index(smoothed_data), value = coredata(smoothed_data))
ggplot(plot_data, aes(x = date, y = value)) +
  geom_line(color = "green", size = 1) +
  #  geom_line(data = ts_G_apl, aes(x = COLLECTION_DATE, y = rates), color = "gray", size = 0.7) +
  geom_smooth(data = ts_G_abc, aes(x = week, y = rates), method = "loess") +
  labs(title = "ABC, weekly time Series Plot with Moving Average",
       x = "Date",
       y = "Anti-N Seropositivity") +
  theme_minimal()

### CBS
df_G_cbs$week <- week(df_G_cbs$sampledate)
ts_G_cbs <-
  df_G_cbs %>%
  group_by(regpro,week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

# Create a zoo object from the time series data
time_series <- zoo(ts_G_cbs$rates, order.by = ts_G_cbs$week)
# Apply a simple moving average with a window size of 5
smoothed_data <- rollapply(time_series, width = 5, FUN = mean, align = "center", fill = NA)
plot_data_cbs <- data.frame(date = index(smoothed_data), value = coredata(smoothed_data))
ggplot(plot_data, aes(x = date, y = value)) +
  geom_line(color = "green", size = 1) +
  #  geom_line(data = ts_G_apl, aes(x = COLLECTION_DATE, y = rates), color = "gray", size = 0.7) +
  geom_smooth(data = ts_G_cbs, aes(x = week, y = rates), method = "loess") +
  labs(title = "CBS, weekly time Series Plot with Moving Average",
       x = "week",
       y = "Anti-N Seropositivity") +
  theme_minimal()


df_D1_apl <- RFD4682_e[RFD4682_e$COLLECTION_DATE >= "2021-01-01",]
### APL
df_G_apl$week <- week(df_G_apl$COLLECTION_DATE)
ts_G_apl <-
  df_G_apl %>%
  group_by(regpro,week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

# Create a zoo object from the time series data
time_series <- zoo(ts_G_apl$rates, order.by = ts_G_apl$week)
# Apply a simple moving average with a window size of 5
smoothed_data <- rollapply(time_series, width = 1, FUN = mean, align = "center", fill = NA)
plot_data_apl <- data.frame(date = index(smoothed_data), value = coredata(smoothed_data))
ggplot(plot_data_apl, aes(x = date, y = value)) +
    geom_line(color = "green", size = 1) +
    geom_line(data = ts_G_apl, aes(x = week, y = rates), color = "gray", size = 0.7) +
  geom_smooth(data = ts_G_abc, aes(x = week, y = rates), method = "loess", color='#00BFC4') +
  geom_smooth(data = ts_G_cbs, aes(x = week, y = rates), method = "loess", color='#F8766D') +
  geom_smooth(data = ts_G_apl, aes(x = week, y = rates), method = "loess", color='#619CFF') +
  labs(title = "Weekly time Series Plot with Moving Average",
       x = "week",
       y = "Anti-N Seropositivity") +
  theme_minimal()

ggplot(plot_data_apl, aes(x = date, y = value)) +
  geom_line(color = "green", size = 1) +
  geom_smooth(data = ts_G_apl, aes(x = week, y = rates), method = "loess", color='#619CFF') +
  labs(title = "Weekly time Series Plot with Moving Average",
       x = "week",
       y = "Anti-N Seropositivity") +
  theme_minimal()



#### Setting C
### ABC
df_C_abc$week <- week(df_C_abc$p4_dbs_received_date)
ts_C_abc <-
  df_C_abc %>%
  group_by(week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

# Create a zoo object from the time series data
time_series <- zoo(ts_C_abc$rates, order.by = ts_C_abc$week)
# Apply a simple moving average with a window size of 5
smoothed_data <- rollapply(time_series, width = 5, FUN = mean, align = "center", fill = NA)
plot_data_abc <- data.frame(date = index(smoothed_data), value = coredata(smoothed_data))

ggplot(plot_data, aes(x = date, y = value)) +
  geom_line(color = "green", size = 1) +
  #  geom_line(data = ts_C_apl, aes(x = COLLECTION_DATE, y = rates), color = "gray", size = 0.7) +
  geom_smooth(data = ts_C_abc, aes(x = week, y = rates), method = "loess") +
  labs(title = "ABC, weekly time Series Plot with Moving Average",
       x = "Date",
       y = "Anti-N Seropositivity") +
  theme_minimal()

### CBS
df_C_cbs$week <- week(df_C_cbs$sampledate)
ts_C_cbs <-
  df_C_cbs %>%
  group_by(week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

# Create a zoo object from the time series data
time_series <- zoo(ts_C_cbs$rates, order.by = ts_C_cbs$week)
# Apply a simple moving average with a window size of 5
smoothed_data <- rollapply(time_series, width = 5, FUN = mean, align = "center", fill = NA)
plot_data_cbs <- data.frame(date = index(smoothed_data), value = coredata(smoothed_data))

ggplot(plot_data, aes(x = date, y = value)) +
  geom_line(color = "green", size = 1) +
  #  geom_line(data = ts_C_apl, aes(x = COLLECTION_DATE, y = rates), color = "gray", size = 0.7) +
  geom_smooth(data = ts_C_cbs, aes(x = week, y = rates), method = "loess") +
  labs(title = "CBS, weekly time Series Plot with Moving Average",
       x = "week",
       y = "Anti-N Seropositivity") +
  theme_minimal()

ggplot(plot_data_cbs, aes(x = date, y = value)) +
#  geom_line(color = "green", size = 1) +
#  geom_line(data = ts_C_apl, aes(x = week, y = rates), color = "gray", size = 0.7) +
  geom_smooth(data = ts_C_abc, aes(x = week, y = rates), method = "loess", color='#00BFC4') +
  geom_smooth(data = ts_C_cbs, aes(x = week, y = rates), method = "loess", color='#F8766D') +
  labs(title = "Weekly time Series Plot with Moving Average, Canada, Omicron",
       x = "week",
       y = "Anti-N Seropositivity") +
  theme_minimal()

ggplot(plot_data_apl, aes(x = date, y = value)) +
  geom_line(color = "green", size = 1) +
  geom_smooth(data = ts_C_apl, aes(x = week, y = rates), method = "loess", color='#619CFF') +
  labs(title = "Weekly time Series Plot with Moving Average",
       x = "week",
       y = "Anti-N Seropositivity") +
  theme_minimal()



### Setting A & B
summary(df_A_abc$p2_received_date)
summary(df_B_abc$p3_dbs_received_date)

summary(df_A_cbs$sampledate)
summary(df_B_cbs$sampledate)

df_A_abc <- within(df_A_abc, rm("Month","composite_strata","composite_strata_1","composite_strata_2"))

### ABC
#df_AB_abc$week <- week(df_AB_abc$p4_dbs_received_date)
df_A_abc$week <- week(df_A_abc$p2_received_date)
df_B_abc$week <- week(df_B_abc$p3_dbs_received_date)
df_AB_abc <- rbind(df_A_abc, df_B_abc)
ts_AB_abc <-
  df_AB_abc %>%
  group_by(week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

ts_A_abc <-
  df_A_abc %>%
  group_by(week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

ts_B_abc <-
  df_B_abc %>%
  group_by(week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

# Create a zoo object from the time series data
time_series <- zoo(ts_AB_abc$rates, order.by = ts_AB_abc$week)
# Apply a simple moving average with a window size of 5
smoothed_data <- rollapply(time_series, width = 5, FUN = mean, align = "center", fill = NA)
plot_data_abc <- data.frame(date = index(smoothed_data), value = coredata(smoothed_data))

ggplot(plot_data, aes(x = date, y = value)) +
  geom_line(color = "green", size = 1) +
  #  geom_line(data = ts_AB_apl, aes(x = COLLECTION_DATE, y = rates), color = "gray", size = 0.7) +
  geom_smooth(data = ts_AB_abc, aes(x = week, y = rates), method = "loess") +
  labs(title = "ABC, weekly time Series Plot with Moving Average",
       x = "Date",
       y = "Anti-N Seropositivity") +
  theme_minimal()

### CBS
df_cbs <- data1
df_AB_cbs <- df_cbs[df_cbs$sampledate >= '2021-01-01' & df_cbs$sampledate <= '2021-08-26',]
df_AB_cbs$week <- week(df_AB_cbs$sampledate)
ts_AB_cbs <-
  df_AB_cbs %>%
  group_by(week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

# Create a zoo object from the time series data
time_series <- zoo(ts_AB_cbs$rates, order.by = ts_AB_cbs$week)
# Apply a simple moving average with a window size of 5
smoothed_data <- rollapply(time_series, width = 5, FUN = mean, align = "center", fill = NA)
plot_data_cbs <- data.frame(date = index(smoothed_data), value = coredata(smoothed_data))

ggplot(plot_data, aes(x = date, y = value)) +
  geom_line(color = "green", size = 1) +
  #  geom_line(data = ts_AB_apl, aes(x = COLLECTION_DATE, y = rates), color = "gray", size = 0.7) +
  geom_smooth(data = ts_AB_cbs, aes(x = week, y = rates), method = "loess") +
  labs(title = "CBS, weekly time Series Plot with Moving Average",
       x = "week",
       y = "Anti-N Seropositivity") +
  theme_minimal()

ggplot(plot_data_cbs, aes(x = date, y = value)) +
  #  geom_line(color = "green", size = 1) +
  #  geom_line(data = ts_AB_apl, aes(x = week, y = rates), color = "gray", size = 0.7) +
  geom_smooth(data = ts_AB_abc, aes(x = week, y = rates), method = "loess", color='#00BFC4') +
  geom_smooth(data = ts_AB_cbs, aes(x = week, y = rates), method = "loess", color='#F8766D') +
  labs(title = "Weekly time Series Plot with Moving Average, Canada, Pre-Omicron",
       x = "week",
       y = "Anti-N Seropositivity") +
  theme_minimal()

ggplot(plot_data_cbs, aes(x = date, y = value)) +
  #  geom_line(color = "green", size = 1) +
  #  geom_line(data = ts_AB_apl, aes(x = week, y = rates), color = "gray", size = 0.7) +
  geom_smooth(data = ts_A_abc, aes(x = week, y = rates), method = "loess", color='#00BFC4') +
  geom_smooth(data = ts_B_abc, aes(x = week, y = rates), method = "loess", color='#00BFC4') +
  geom_smooth(data = ts_AB_cbs, aes(x = week, y = rates), method = "loess", color='#F8766D') +
  labs(title = "Weekly time Series Plot with Moving Average, Canada, Pre-Omicron",
       x = "week",
       y = "Anti-N Seropositivity") +
  theme_minimal()

## df_D_clsa, df_A_abc, df_B_abc
summary(df_D_clsa$date)
summary(df_D_cbs$sampledate)

df_A50_abc <- df_A_abc[df_A_abc$age>=50,]
df_B50_abc <- df_B_abc[df_B_abc$age>=50,]


ts_A50_abc <-
  df_A50_abc %>%
  group_by(week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

ts_B50_abc <-
  df_B50_abc %>%
  group_by(week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

df_D_clsa$week <- week(df_D_clsa$date)
df_D_clsa <- df_D_clsa[!is.na(df_D_clsa$date),]
df_D1_clsa <- df_D_clsa[df_D_clsa$date >= "2021-01-01",]
ts_D1_clsa <-
  df_D1_clsa %>%
  group_by(week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()


df_D1_cbs <- df_cbs[df_cbs$sampledate >= "2021-01-01" & df_cbs$sampledate <= "2021-08-26" & df_cbs$age>=50,]
df_D1_cbs$week <- week(df_D1_cbs$sampledate)
ts_D1_cbs <-
  df_D1_cbs %>%
  group_by(week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

df_D50_cbs <- df_cbs[df_cbs$sampledate >= "2021-01-01" & df_cbs$sampledate <= "2021-08-26" & df_cbs$age>=50,]
df_D50_cbs$week <- week(df_D50_cbs$sampledate)
ts_D50_cbs <-
  df_D50_cbs %>%
  group_by(week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()


ggplot() +
  #  geom_line(color = "green", size = 1) +
  #  geom_line(data = ts_AB_apl, aes(x = week, y = rates), color = "gray", size = 0.7) +
  geom_smooth(data = ts_A50_abc, aes(x = week, y = rates), method = "loess", color='#00BFC4') +
  geom_smooth(data = ts_B50_abc, aes(x = week, y = rates), method = "loess", color='#00BFC4') +
  geom_smooth(data = ts_D50_cbs, aes(x = week, y = rates), method = "loess", color='#F8766D') +
  geom_smooth(data = ts_D1_clsa, aes(x = week, y = rates), method = "loess", color='#B79F00') +
  labs(title = "2021 weekly time Series Plot with Moving Average, Canada, Pre-Omicron, Elder Group",
       x = "week",
       y = "Anti-N Seropositivity") +
  theme_minimal()

#################
df_A_abc_AB <- df_A_abc[df_A_abc$regpro=='AB',]
df_B_abc_AB <- df_B_abc[df_B_abc$regpro=='AB',]


ts_A_abc_AB <-
  df_A_abc_AB %>%
  group_by(week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

ts_B_abc_AB <-
  df_B_abc_AB %>%
  group_by(week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

df_D_cbs_AB <- df_cbs[df_cbs$sampledate >= "2021-01-01" & df_cbs$sampledate <= "2021-08-26" & df_cbs$regpro =="AB",]
df_D_cbs_AB$week <- week(df_D_cbs_AB$sampledate)
ts_D_cbs_AB <-
  df_D_cbs_AB %>%
  group_by(week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

df_D_apl <- RFD4682_e[RFD4682_e$COLLECTION_DATE >= '2021-01-01 00:00:00.0000' & RFD4682_e$COLLECTION_DATE <= '2021-08-26 00:00:00.0000',]
df_D_apl <- df_D_apl[!is.na(df_D_apl$GENDER),] #8783
df_D_apl$week <- week(df_D_apl$COLLECTION_DATE)
df_D_apl$age = df_D_apl$AGE_AT_COLLECTION
# df_D_apl$age_group = cut(df_D_apl$age, 
#                          breaks = c(18,40,55,Inf),
#                          labels = c('18-39','40-54','55+'),
#                          right = FALSE)
df_D_apl$age_group <- age_groups_fun(df_D_apl$age)

df_D_apl$Sex = with(df_D_apl,ifelse(GENDER == "Male",1,0))

df_D_apl <- df_D_apl[!is.na(df_D_apl$PAT_FSA),]
df_D_apl$Urban <- with(df_D_apl,ifelse(substr(PAT_FSA,start = 2,stop = 2) != "0",1,0))

df_D_apl$region = factor(case_when(as.character(substr(df_D_apl$PAT_FSA,1,1)) %in% c("A", "B", "C", "E")~"Atlantic", 
                                   as.character(substr(df_D_apl$PAT_FSA,1,1)) %in% c("K", "L", "M", "N", "P")~"Ontario",
                                   as.character(substr(df_D_apl$PAT_FSA,1,1)) %in% c("R", "S", "T")~"Prairies",
                                   as.character(substr(df_D_apl$PAT_FSA,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("Atlantic","BC",  "Ontario", "Prairies"))

df_D_apl$regpro = factor(case_when(as.character(substr(df_D_apl$PAT_FSA,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                                   as.character(substr(df_D_apl$PAT_FSA,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                                   as.character(substr(df_D_apl$PAT_FSA,1,1)) %in% c("T")~"AB",
                                   as.character(substr(df_D_apl$PAT_FSA,1,1)) %in% c("R")~"MB",
                                   as.character(substr(df_D_apl$PAT_FSA,1,1)) %in% c("S")~"SK",
                                   as.character(substr(df_D_apl$PAT_FSA,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK"))

df_D_apl <- df_D_apl[!is.na(df_D_apl$'N-IgG_INTERP'),]
df_D_apl$interp_roche_n <- with(df_D_apl,ifelse(df_D_apl$'N-IgG_INTERP' == "Positive",1,0))
df_D_apl$interp_roche_n <- case_when(df_D_apl$'N-IgG_INTERP' == "Positive"~ 1,
                                     df_D_apl$'N-IgG_INTERP' == "Negative"~ 0,
                                     TRUE~NA)
df_D_apl$day <- yday(df_D_apl$COLLECTION_DATE)


ts_D_apl <-
  df_D_apl %>%
  group_by(week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

ggplot() +
  #  geom_line(color = "green", size = 1) +
  #  geom_line(data = ts_AB_apl, aes(x = week, y = rates), color = "gray", size = 0.7) +
  geom_smooth(data = ts_A_abc_AB, aes(x = week, y = rates), method = "loess", color='#00BFC4') +
  geom_smooth(data = ts_B_abc_AB, aes(x = week, y = rates), method = "loess", color='#00BFC4') +
  geom_smooth(data = ts_D_cbs_AB, aes(x = week, y = rates), method = "loess", color='#F8766D') +
  geom_smooth(data = ts_D_apl, aes(x = week, y = rates), method = "loess", color='#619CFF') +
  labs(title = "2021 weekly time Series Plot with Moving Average, Canada, Pre-Omicron, Alberta",
       x = "week",
       y = "Anti-N Seropositivity") +
  theme_minimal()


ts_A_ccahs <- data.frame (week  = seq(1,15),
                  rates = c(.009, .042, .035, .044, .035, 
                            .035, .029, .035, .024, .014, 
                            .033, .028, 0, .125, 0))

ggplot() +
  geom_smooth(data = ts_A_abc, aes(x = week, y = rates), method = "loess", color='#00BFC4') +
  geom_smooth(data = ts_B_abc, aes(x = week, y = rates), method = "loess", color='#00BFC4') +
  geom_smooth(data = ts_A_abc, aes(x = week-1, y = rates), method = "loess", color='#00BFC4',linetype = "dashed") +
  geom_smooth(data = ts_B_abc, aes(x = week-1, y = rates), method = "loess", color='#00BFC4',linetype = "dashed") +
  geom_smooth(data = ts_AB_cbs, aes(x = week, y = rates), method = "loess", color='#F8766D') +
  geom_smooth(data = ts_A_ccahs, aes(x = week, y = rates), method = "loess", color='#00BA38') +
  labs(title = "2021 weekly time Series Plot with Moving Average, Canada, Pre-Omicron",
       x = "week",
       y = "Anti-N Seropositivity") +
  theme_minimal()

ts_E_ccahs <- data.frame (week  = seq(14,35),
                          rates = c(0.328,0.318,0.286,0.285,0.365,0.338,0.354,0.353,0.324,0.338,0.373,
                                    0.391,0.428,0.443,0.542,0.597,0.444,0.469,0.395,0.313,0.439,0.286))

library(dplyr)
df_E_cbs$week <- week(df_E_cbs$sampledate)
df_E_cbs$vac <- with(df_E_cbs, covidvv == 1,1,0)
ts_E_cbs <-
  df_E_cbs %>%
  group_by(regpro,week) %>%
  dplyr::summarize(case_counts = sum(vac), rates = mean(vac)) %>%
  drop_na()

ggplot() +
  geom_smooth(data = ts_E_cbs, aes(x = week, y = rates), method = "loess", color='#F8766D') +
  geom_smooth(data = ts_E_ccahs, aes(x = week, y = rates), method = "loess", color='#00BA38') +
  labs(title = "2022 Apr-Aug weekly time Series Plot, Canada, Omicron",
       x = "week",
       y = "Anti-N Seropositivity") +
  theme_minimal()


df_E_cbs_1 <- data_all[data_all$sampledate >= '2022-04-04' & data_all$sampledate <= '2022-08-31',] #132927 
df_E_cbs_1$province = province_fun(df_E_cbs_1$fsa)
df_E_cbs_1 <- df_E_cbs_1 %>% 
  filter(province != ("YT") & province !=("NU/NT") & province != ("QC")) #5648

df_E_cbs_1 <- df_E_cbs_1[!is.na(df_E_cbs_1$sex),]
df_E_cbs_1$Sex <- with(df_E_cbs_1, ifelse(sex == "M", 1, 0))
df_E_cbs_1 <- df_E_cbs_1[!is.na(df_E_cbs_1$ethnic1),]
df_E_cbs_1$Race <- with(df_E_cbs_1, ifelse(ethnic1 == "1 White", 1, 0))
#df_E_cbs_1$cur_result_n <- with(df_E_cbs_1, ifelse(cur_result_n == "Roche Positive", 1, 0))
df_E_cbs_1$Urban <- with(df_E_cbs_1,ifelse(substr(fsa,start = 2,stop = 2) != "0",1,0))



df_E_cbs_1 <- df_E_cbs_1[!is.na(df_E_cbs_1$interp_roche_n),]

df_E_cbs_1$week <- week(df_E_cbs_1$sampledate)
df_E_cbs_1$day <- yday(df_E_cbs_1$sampledate)
df_E_cbs$vac <- with(df_E_cbs, covidvv == 1,1,0)
df_E_cbs_1$regpro = factor(case_when(as.character(substr(df_E_cbs_1$fsa,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                                as.character(substr(df_E_cbs_1$fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                                as.character(substr(df_E_cbs_1$fsa,1,1)) %in% c("T")~"AB",
                                as.character(substr(df_E_cbs_1$fsa,1,1)) %in% c("R")~"MB",
                                as.character(substr(df_E_cbs_1$fsa,1,1)) %in% c("S")~"SK",
                                as.character(substr(df_E_cbs_1$fsa,1,1)) %in% c("V")~"BC",
                                TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK"))

#df_E_cbs_1$age_group <- age_groups_fun(df_E_cbs_1$age)
df_E_cbs_1 <- df_E_cbs_1 %>% 
  mutate(age = 2022 - dob, age_group = age_groups_fun(age)) #%>% 
#  mutate(month_year = format(as.Date(data2$sampledate), "%Y-%m")) %>% 
#  mutate(province = province_fun(fsa)) %>%  
#  mutate(log_roche_s = log(roche_s))

ts_E_cbs_1 <-
  df_E_cbs_1 %>%
  group_by(regpro,week) %>%
  dplyr::summarize(case_counts = sum(interp_roche_n), rates = mean(interp_roche_n)) %>%
  drop_na()

ggplot() +
  geom_smooth(data = ts_E_cbs_1, aes(x = week, y = rates), method = "loess", color='#F8766D') +
  geom_smooth(data = ts_E_ccahs, aes(x = week, y = rates), method = "loess", color='#00BA38') +
  labs(title = "2022 Apr-Aug weekly time Series Plot, Canada, Omicron",
       x = "week",
       y = "Anti-N Seropositivity") +
  theme_minimal()


######
library(rstanarm)
#example(example_model)
rstan::get_stanmodel(fit_G_cbs$stanfit)

#df_E_cbs_1
#3:18 pm
start_time <- Sys.time()
df_test <- df_E_cbs_1[sample(nrow(df_E_cbs_1), 16000), ]
fit_test <- stan_gamm4(interp_roche_n ~ s(week, by=regpro, k=10) + age_group + Sex + Race + Urban, 
           random = ~ (1 | regpro), 
           family = binomial(link="cloglog"), 
           data = df_test, iter = 3000, cores = 4, 
           prior = normal(0,0.5), prior_covariance = decov(shape = 1, scale = 1), 
           prior_smooth=normal(location=4), control=list(adapt_delta=0.95))
end_time <- Sys.time()
end_time - start_time


start_time <- Sys.time()
fit_test_1 <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race + Urban + year_month,
                        family = binomial,
                        data = df_test,
                        cores = 4,
                        seed = 1010, adapt_delta = 0.99)
end_time <- Sys.time()
end_time - start_time
print(fit_test_1)
waic(fit_test_1)

start_time <- Sys.time()
#df_test <- df_E_cbs_1[sample(nrow(df_E_cbs_1), 16000), ]
fit_test_s1 <- stan_gamm4(interp_roche_n ~ s(week, by=regpro, k=10) + age_group + Sex + Race + Urban, 
                       random = ~ (1 | regpro), 
                       family = binomial(link="cloglog"), 
                       data = df_test, iter = 3000, cores = 4, 
                       prior = normal(0,0.5), prior_covariance = decov(shape = 1, scale = 1), 
                       prior_smooth=normal(location=4), control=list(adapt_delta=0.95))
end_time <- Sys.time()
end_time - start_time
print(fit_test_s1)


df_test$month <- as.integer(format(df_test$sampledate,"%m"))
start_time <- Sys.time()
fit_test_s2 <- stan_gamm4(interp_roche_n ~ s(month, by=regpro, k=5) + age_group + Sex + Race + Urban, 
                          random = ~ (1 | regpro), 
                          family = binomial(link="cloglog"), 
                          data = df_test, iter = 3000, cores = 4, 
                          prior = normal(0,0.5), prior_covariance = decov(shape = 1, scale = 1), 
                          prior_smooth=normal(location=4), control=list(adapt_delta=0.95))
end_time <- Sys.time()
end_time - start_time
print(fit_test_s2)

start_time <- Sys.time()
fit_test_s2 <- stan_gamm4(interp_roche_n ~ s(week, by=regpro, k=20) + age_group + Sex + Race + Urban, 
                          random = ~ (1 | regpro), 
                          family = binomial(link="cloglog"), 
                          data = df_test, iter = 3000, cores = 4, 
                          prior = normal(0,0.5), prior_covariance = decov(shape = 1, scale = 1), 
                          prior_smooth=normal(location=4), control=list(adapt_delta=0.95))
end_time <- Sys.time()
end_time - start_time
print(fit_test_s2)

start_time <- Sys.time()
fit_test_2 <- stan_glmer(interp_roche_n ~ (1 | regpro)+ age_group + Sex + Race + Urban + month,
                         family = binomial,
                         data = df_test,
                         cores = 4,
                         seed = 1010, adapt_delta = 0.95)
end_time <- Sys.time()
end_time - start_time
print(fit_test_2)

start_time <- Sys.time()
fit_test_s3 <- stan_gamm4(interp_roche_n ~ s(week, by=regpro, k=15) + age_group + Sex + Race + Urban, 
                          random = ~ (1 | regpro), 
                          family = binomial(link="cloglog"), 
                          data = df_test, iter = 3000, cores = 4, 
                          prior = normal(0,0.5), prior_covariance = decov(shape = 1, scale = 1), 
                          prior_smooth=normal(location=4), control=list(adapt_delta=0.9))
end_time <- Sys.time()
end_time - start_time
print(fit_test_s3)


start_time <- Sys.time()
fit_test_s4 <- stan_gamm4(interp_roche_n ~ s(week, by=regpro, k=10) + age_group + Sex + Race + Urban, 
                          random = ~ (1 | regpro) , 
                          family = binomial, 
                          data = df_test, iter = 3000, cores = 4, 
                          prior = normal(0,0.5), prior_covariance = decov(shape = 1, scale = 1), 
                          prior_smooth=normal(location=4), control=list(adapt_delta=0.9))
end_time <- Sys.time()
end_time - start_time
print(fit_test_s4)
waic(fit_test_s4)

#use the default prior
start_time <- Sys.time()
fit_test_s5 <- stan_gamm4(interp_roche_n ~ s(week, by=regpro, k=10) + age_group + Sex + Race + Urban, 
                          random = ~ (1 | regpro) , 
                          family = binomial, 
                          data = df_test, iter = 3000, cores = 4, 
                          prior = normal(0,10), prior_covariance = decov(shape = 1, scale = 1), 
                          prior_smooth=normal(location=4), 
                          control=list(adapt_delta=0.9))
end_time <- Sys.time()
end_time - start_time
print(fit_test_s5)

plot_nonlinear(fit_test)
plot_nonlinear(fit_test, smooths = "s(week):regproAB", alpha = 2/3)
library(rstan)
check_hmc_diagnostics(fit_test$stanfit)

library(lubridate)
df_test$day <- yday(df_test$sampledate)
start_time <- Sys.time()
fit_test_s6 <- stan_gamm4(interp_roche_n ~ s(day, by=regpro, k=10) + age_group + Sex + Race + Urban, 
                          random = ~ (1 | regpro) , 
                          family = binomial, 
                          data = df_test, iter = 3000, cores = 4, 
                          prior = normal(0,10), prior_covariance = decov(shape = 1, scale = 1), 
                          prior_smooth=normal(location=4), 
                          control=list(adapt_delta=0.9))
end_time <- Sys.time()
end_time - start_time
print(fit_test_s6)
print(fit_test_s6)

plot_nonlinear(fit_test_s6)

library(bayesplot)
library(dplyr)
# Assuming posterior_preds is the output from posterior_epred()
dim(df_test[complete.cases(df_test), ])
dim(df_test %>% drop_na())
dim(na.omit(df_test))
i=0
poststrat_t= NULL
while (i < ndays){
  poststrat_t <- rbind(poststrat_t,poststrat_cbs)
  i = i+1
}
day_col <- c()
for(i in 1:ndays){
  day_col <- c(day_col,rep(93+i, nrow(poststrat_cbs)))
}
poststrat_t$day <- day_col

poststrat_t$day <- c(rep(94, nrow(poststrat_cbs)), rep(95, nrow(poststrat_cbs)),rep(96, nrow(poststrat_cbs)),
                     rep(97, nrow(poststrat_cbs)),rep(98, nrow(poststrat_cbs)),rep(99, nrow(poststrat_cbs)),
                     rep(100, nrow(poststrat_cbs)),rep(101, nrow(poststrat_cbs)),rep(102, nrow(poststrat_cbs)),
                     rep(103, nrow(poststrat_cbs)))



posterior_preds <- posterior_epred(fit_test_s6, newdata = poststrat_t, draws = 4000)
predicted_values <- posterior_preds$fit
mcmc_areas(posterior_samples = posterior_preds, prob = c(0.025, 0.975))

install.packages("sjPlot")
library(sjPlot)
# Plotting marginal effects
plot_model(fit_test_s6, type = "eff")

####
regpro_df <- data.frame(
  regpro = c("BC","AB","MB","SK","ON","ATL"), 
  mrp_A_cbs = NA,
  mrp_A_cbs_se = NA,
  n_sample = NA,
  n_full = NA
)

ndays = 150
output <- matrix(ncol=4, nrow=6*ndays)
regpros <- c("BC","AB","MB","SK","ON","ATL")
epred_mat <- posterior_epred(fit_test_s6, newdata = poststrat_t, draws = 4000)
for(j in 1:6){
  for(i in 1:ndays) {
    # MRP estimate A cbs
    filtering_condition <- which(poststrat_t$day == (93 + i) & poststrat_t$regpro == regpros[j])
    regpro_epred_mat <- epred_mat[ ,filtering_condition]
    k_filtered <- poststrat_t[filtering_condition, ]$n
    mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
    output[i + (j - 1) * ndays, ] <- c(regpros[j], mean(mrp_estimates_vector), sd(mrp_estimates_vector), 93+i)
  }
}

df_time <- as.data.frame(output)
colnames(df_time) <- c('regpro','mrp_mean', 'mrp_sd', 'day')
df_time$mrp_mean <- as.numeric(df_time$mrp_mean)
df_time$mrp_sd <- as.numeric(df_time$mrp_sd)
df_time$day <- as.integer(df_time$day)

df_time$lower <- df_time$mrp_mean - 2*df_time$mrp_sd
df_time$upper <- df_time$mrp_mean + 2*df_time$mrp_sd

#df_time$day <- c(94:103)

ggplot(df_time, aes(x = day, y = mrp_mean, color = regpro)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = regpro), alpha = 0.2) +
  labs(title = "Time Series with Ribbon Lower and Upper Bound by Region",
       x = "Year-Day",
       y = "Seropositivity") 
  scale_color_discrete(name = "regpro") 
#  scale_fill_discrete(name = "Region")


  
library(ggplot2)
ggplot(df_time, aes(x = day, y = mrp_mean, colour = regpro)) +
  #  geom_line(color = "blue", linetype = "dashed") +
  geom_line(aes(y = mrp_mean, colour = regpro)) +
  geom_ribbon(aes(x = day, ymin = lower, ymax = upper, fill = "ribbon", colour = regpro), alpha = 0.2) +
  #  geom_smooth(aes(y = model_predictions), method = "loess", color = "green") +
#  geom_smooth(aes(y = model_predictions), method = "gam", color = "blue") +
  labs(title = "Mrp",
       x = "day",
       y = "Seropositivity") +
  theme_minimal()


ggplot(df_time, aes(x = day, y = mrp_mean, color = regpro)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = regpro), alpha = 0.2, colour = NA) +
  labs(title = "Time Series with Ribbon Lower and Upper Bound by Region",
       x = "Year-Day",
       y = "Seropositivity") 
scale_color_discrete(name = "regpro") 

### modify for unadjusted
# Unadjusted estimate A cbs
regpro_df$A_cbs_estimate[i] <- mean(filter(df_A_cbs, regpro==regpro_df$regpro[i])$interp_roche_n)
regpro_df$n_sample[i] <- nrow(filter(df_A_cbs, regpro==regpro_df$regpro[i]))
regpro_df$A_cbs_estimate_se[i] <- get_se_bernoulli(regpro_df$A_cbs_estimate[i], regpro_df$n_sample[i])

ndays=150
output1 <- matrix(ncol=4, nrow=6*ndays)
regpros <- c("BC","AB","MB","SK","ON","ATL")
#epred_mat <- posterior_epred(fit_test_s6, newdata = poststrat_t, draws = 4000)
for(j in 1:6){
  for(i in 1:ndays) {
    # MRP estimate A cbs
    ua_estimate <- mean(filter(df_test, day == (93 + i) & regpro == regpros[j])$interp_roche_n)
    n_sample <- nrow(filter(df_test, day == (93 + i) & regpro == regpros[j]))
    ua_se <- get_se_bernoulli(ua_estimate, n_sample)
    output1[i + (j - 1) * ndays, ] <- c(regpros[j], ua_estimate, ua_se, 93+i)
  }
}

df_time1 <- as.data.frame(output1)
colnames(df_time1) <- c('regpro','mrp_mean', 'mrp_sd', 'day')
df_time1$mrp_mean <- as.numeric(df_time1$mrp_mean)
df_time1$mrp_sd <- as.numeric(df_time1$mrp_sd)
df_time1$day <- as.integer(df_time1$day)

df_time1$lower <- df_time1$mrp_mean - 2*df_time1$mrp_sd
df_time1$upper <- df_time1$mrp_mean + 2*df_time1$mrp_sd

ggplot(df_time1, aes(x = day, y = mrp_mean, color = regpro)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = regpro), alpha = 0.2, colour = NA) +
  labs(title = "Time Series Unadjusted",
       x = "Year-Day",
       y = "Seropositivity") 
scale_color_discrete(name = "regpro") 

####. Rake. ####
data <- df_A_cbs[df_A_cbs$regpro==regpro_df$regpro[i],]
data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
poststrat.dist <- data.frame(composite_strata = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$composite_strata,
                             Freq = poststrat_cbs[poststrat_cbs$regpro==regpro_df$regpro[i],]$n)

data.svy.rake <- rake(design = data.svy.unweighted,
                      sample.margins = list(~composite_strata),
                      population.margins = list(poststrat.dist))

regpro_df$da_estimate_A_cbs[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
regpro_df$da_estimate_A_cbs_se[i] <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]

library(survey)
ndays=150
output2 <- matrix(ncol=4, nrow=6*ndays)
regpros <- c("BC","AB","MB","SK","ON","ATL")
#epred_mat <- posterior_epred(fit_test_s6, newdata = poststrat_t, draws = 4000)
df_test <- df_test[!is.na(df_test$age_group),]

df_test$composite_strata <- paste(df_test$age_group, df_test$Sex, sep = "_")
df_test$composite_strata <- paste(df_test$age_group, df_test$Sex, sep = "_")

df_test$composite_strata1 <- paste(df_test$age_group, df_test$Sex, df_test$Race, sep = "_")
df_test$composite_strata1 <- paste(df_test$age_group, df_test$Sex, df_test$Race, sep = "_")



for(j in 1:6){
  for(i in 1:ndays) {
    # MRP estimate A cbs
    data <- filter(df_test, day == (93 + i) & regpro == regpros[j])
    data.svy.unweighted <- svydesign(ids = ~1, strata = ~composite_strata, data = data)
    poststrat.dist <- data.frame(filter(poststrat_t, day == (93 + i) & regpro == regpros[j])$composite_strata,
                                 Freq = filter(poststrat_t, day == (93 + i) & regpro == regpros[j])$n)
    data.svy.rake <- rake(design = data.svy.unweighted,
                          sample.margins = list(~composite_strata),
                          population.margins = list(poststrat.dist))
    
    rake_estimate <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
    rake_se <- as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]

    output2[i + (j - 1) * ndays, ] <- c(regpros[j], rake_estimate, rake_se, 93+i)
  }
}



#############
df_time_asr <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/df_time_asr.csv')
df_time_asru <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/df_time_asru.csv')
df_time_asr_ua <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/df_time_asr_ua.csv')
df_time_ccahs <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/df_ccahs.csv')


ggplot(df_time_asr_ua, aes(x = day, y = mrp_mean, color = regpro)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = regpro), alpha = 0.2, colour = NA) +
  labs(title = "Time Series Unadjusted",
       x = "Year-Day",
       y = "Seropositivity") 
scale_color_discrete(name = "regpro") 

ggplot(df_time_asr, aes(x = day, y = mrp_mean, color = regpro)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = regpro), alpha = 0.2, colour = NA) +
  labs(title = "Time Series MRP Adjusted",
       x = "Year-Day",
       y = "Seropositivity") 
scale_color_discrete(name = "regpro") 

ggplot(df_time_ccahs, aes(x = day, y = mrp_mean, color = regpro)) +
  geom_line(aes(linetype=model, color=regpro)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = regpro), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  labs(title = "Seropositivity Estimation from CCAHS, Apr-Aug 2022",
       x = "Year-Day",
       y = "Seropositivity") +
  theme_bw()
scale_color_discrete(name = "regpro") 

start_time <- Sys.time()
fit_E_cbs <- stan_gamm4(interp_roche_n ~ s(day, by=regpro, k=10) + age_group + Sex + Race, 
                          random = ~ (1 | regpro) , 
                          family = binomial, 
                          data = df_E_cbs_1, iter = 2000, cores = 4, 
                          prior = normal(0,10), prior_covariance = decov(shape = 1, scale = 1), 
                          prior_smooth=normal(location=4), 
                          control=list(adapt_delta=0.9))
end_time <- Sys.time()
end_time - start_time
print(fit_E_cbs)

ndays = 150
output_mrp <- matrix(ncol=4, nrow=6*ndays)
regpros <- c("BC","AB","MB","SK","ON","ATL")
epred_mat <- posterior_epred(fit_E_cbs, newdata = poststrat_t, draws = 4000)
for(j in 1:6){
  for(i in 1:ndays) {
    # MRP estimate A cbs
    filtering_condition <- which(poststrat_t$day == (93 + i) & poststrat_t$regpro == regpros[j])
    regpro_epred_mat <- epred_mat[ ,filtering_condition]
    k_filtered <- poststrat_t[filtering_condition, ]$n
    mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
    output[i + (j - 1) * ndays, ] <- c(regpros[j], mean(mrp_estimates_vector), sd(mrp_estimates_vector), 93+i)
  }
}

df_time <- as.data.frame(output_mrp)
colnames(df_time) <- c('regpro','mrp_mean', 'mrp_sd', 'day')
df_time$mrp_mean <- as.numeric(df_time$mrp_mean)
df_time$mrp_sd <- as.numeric(df_time$mrp_sd)
df_time$day <- as.integer(df_time$day)

df_time$lower <- df_time$mrp_mean - 2*df_time$mrp_sd
df_time$upper <- df_time$mrp_mean + 2*df_time$mrp_sd

# Count Table Without Urban Indicator
library(tidyverse)
poststrat_cbs_1 <-
  census_all %>%
  group_by(regpro,age_group,Sex,Race) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

poststrat_cbs_1 <- poststrat_cbs_1[poststrat_cbs_1$age_group != "< 18 years", ]
poststrat_cbs_1[poststrat_cbs_1$regpro == 'Atlantic', ]$regpro <- 'ATL'

i=0
poststrat_t= NULL
while (i < ndays){
  poststrat_t <- rbind(poststrat_t,poststrat_cbs_1)
  i = i+1
}
day_col <- c()
for(i in 1:ndays){
  day_col <- c(day_col,rep(93+i, nrow(poststrat_cbs_1)))
}
poststrat_t$day <- day_col

ndays = 150
output_mrp <- matrix(ncol=4, nrow=6*ndays)
regpros <- c("BC","AB","MB","SK","ON","ATL")
epred_mat <- posterior_epred(fit_E_cbs, newdata = poststrat_t, draws = 4000)
for(j in 1:6){
  for(i in 1:ndays) {
    # MRP estimate A cbs
    filtering_condition <- which(poststrat_t$day == (93 + i) & poststrat_t$regpro == regpros[j])
    regpro_epred_mat <- epred_mat[ ,filtering_condition]
    k_filtered <- poststrat_t[filtering_condition, ]$n
    mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
    output_mrp[i + (j - 1) * ndays, ] <- c(regpros[j], mean(mrp_estimates_vector), sd(mrp_estimates_vector), 93+i)
  }
}

##[Apparent Prevalence + (Specificity − 1)]/
##[Specificity + (Sensitivity − 1)]
## For Roche : [x+(0.998-1)]/[0.998+0.995-1]
## x/0.993-0.002/0.993
df_time <- as.data.frame(output_mrp)
colnames(df_time) <- c('regpro','mrp_mean', 'mrp_sd', 'day')
df_time$mrp_mean <- as.numeric(df_time$mrp_mean)/0.993-0.002/0.993
df_time$mrp_sd <- as.numeric(df_time$mrp_sd)/0.993
df_time$day <- as.integer(df_time$day)

df_time$lower <- df_time$mrp_mean - 2*df_time$mrp_sd
df_time$upper <- df_time$mrp_mean + 2*df_time$mrp_sd

#####
library(dplyr)
ndays <- max(df_E_cbs_1$day) - min(df_E_cbs_1$day) + 1
output_ua <- matrix(ncol=4, nrow=6*ndays)
regpros <- c("BC","AB","MB","SK","ON","ATL")
poststrat_ua <-
  df_E_cbs_1 %>%
  group_by(regpro,age_group,Sex,Race,day) %>%
  dplyr::summarize(n = n()) %>%
  drop_na()
epred_mat <- posterior_epred(fit_E_cbs, newdata = poststrat_ua, draws = 4000)
for(j in 1:6){
  for(i in 1:ndays) {
    # MRP estimate A cbs
    filtering_condition <- which(poststrat_ua$day == (min(df_E_cbs_1$day)-1 + i) & poststrat_ua$regpro == regpros[j])
    regpro_epred_mat <- epred_mat[ ,filtering_condition]
    mrp_estimates_vector <- regpro_epred_mat
    output_ua[i + (j - 1) * ndays, ] <- c(regpros[j], mean(mrp_estimates_vector), sd(mrp_estimates_vector), min(df_E_cbs_1$day)-1+i)
  }
}
# for(j in 1:6){
#   for(i in 1:ndays) {
#     # MRP estimate A cbs
#     filtering_condition <- which(poststrat_ua$day == (93 + i) & poststrat_ua$regpro == regpros[j])
#     regpro_epred_mat <- as.matrix(epred_mat[ ,filtering_condition])
#     k_filtered <- poststrat_ua[filtering_condition, ]$n
#     mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
#     output_ua[i + (j - 1) * ndays, ] <- c(regpros[j], mean(mrp_estimates_vector), sd(mrp_estimates_vector), 93+i)
#   }
# }

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
df_time_cbs <- rbind(df_time, df_time_ua)

ggplot(df_time_cbs, aes(x = day, y = mrp_mean, color = regpro)) +
  geom_line(aes(linetype=model, color=regpro)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = regpro), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  labs(title = "",
       x = "Year-Day",
       y = "Seropositivity") +
  theme_bw()

df_time_ccahs <- within(df_time_ccahs, rm(X))
df_time_ccahs$source <- 'CCAHS'
df_time_cbs$source <- 'CBS'
df_time_all <- rbind(df_time_ccahs, df_time_cbs)

ggplot(df_time_all, aes(x = day, y = mrp_mean, color = source)) +
  geom_line(aes(linetype=model, color=source)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
#  facet_wrap(~ regpro) +
  labs(title = "Seropositivity Estimation during Omicron, Apr-Aug 2022",
       x = "Year-Day",
       y = "Seropositivity") +
  theme_bw()

ggplot(df_time_all[df_time_all$regpro == 'AB', ], aes(x = day, y = mrp_mean, color = source)) +
  geom_line(aes(linetype=model, color=source)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
  #  facet_wrap(~ regpro) +
  labs(title = "Seropositivity Estimation during Omicron, Apr-Aug 2022",
       x = "Year-Day",
       y = "Seropositivity") +
  theme_bw()

ggplot(df_time_all[df_time_all$regpro == 'BC', ], aes(x = day, y = mrp_mean, color = source)) +
  geom_line(aes(linetype=model, color=source)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
  #  facet_wrap(~ regpro) +
  labs(title = "Seropositivity Estimation during Omicron, Apr-Aug 2022",
       x = "Year-Day",
       y = "Seropositivity") +
  theme_bw()


df_C_abc <- df_abc_cb[df_abc_cb$p4_dbs_received_date >= '2022-01-24' & df_abc_cb$p4_dbs_received_date <= '2022-04-12',]
df_C_abc$Province = province_fun(df_C_abc$p4a_fsa)
df_C_abc <- df_C_abc %>% 
  filter(Province != ("YT") & Province !=("NU/NT") & Province != ("QC")) #4178

df_C_abc$age = df_C_abc$p4a_age
df_C_abc$age_group <- age_groups_fun(df_C_abc$age)

df_C_abc$Sex = 2-df_C_abc$p4a_qe2
df_C_abc <- df_C_abc[df_C_abc$Sex != -1,] #4151

df_C_abc$Urban <- with(df_C_abc,ifelse(substr(p4a_fsa,start = 2,stop = 2) != "0",1,0))

df_C_abc$Race = with(df_C_abc, ifelse(p4a_vizmin==2 & p4a_ethnicity_1 !=1, 1, 0))

df_C_abc$day = yday(df_C_abc$p4_dbs_received_date)

df_C_abc$week = week(df_C_abc$p4_dbs_received_date)

df_C_abc$regpro = factor(case_when(as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                                     as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                                     as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("T")~"AB",
                                     as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("R")~"MB",
                                     as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("S")~"SK",
                                     as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("V")~"BC",
                                     TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK"))
df_C_abc$region = factor(case_when(as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("A", "B", "C", "E")~"Atlantic", 
                                   as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"Ontario",
                                   as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("R", "S", "T")~"Prairies",
                                   as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("Atlantic","BC",  "Ontario", "Prairies"))

df_C_abc$interp_roche_n <- with(df_C_abc,ifelse(p4_np_igg_pred== "Positive",1,0))

start_time <- Sys.time()
fit_C_abc_1 <- stan_gamm4(interp_roche_n ~ s(day, by=regpro, k=10) + age_group + Sex + Race, 
                        random = ~ (1 | regpro) , 
                        family = binomial, 
                        data = df_C_abc, iter = 2000, cores = 4, 
                        prior = normal(0,1), prior_covariance = decov(shape = 1, scale = 1), 
                        prior_smooth=normal(location=4), 
                        control=list(adapt_delta=0.9))
end_time <- Sys.time()
end_time - start_time
print(fit_C_abc_1)


# cbs0
df_E_cbs_0 <- data_all[data_all$sampledate >= '2022-01-01' & data_all$sampledate <= '2022-04-03',] #132927 
df_E_cbs_0$province = province_fun(df_E_cbs_0$fsa)
df_E_cbs_0 <- df_E_cbs_0 %>% 
  filter(province != ("YT") & province !=("NU/NT") & province != ("QC")) #5648

df_E_cbs_0 <- df_E_cbs_0[!is.na(df_E_cbs_0$sex),]
df_E_cbs_0$Sex <- with(df_E_cbs_0, ifelse(sex == "M", 1, 0))
df_E_cbs_0 <- df_E_cbs_0[!is.na(df_E_cbs_0$ethnic1),]
df_E_cbs_0$Race <- with(df_E_cbs_0, ifelse(ethnic1 == "1 White", 1, 0))
#df_E_cbs_0$cur_result_n <- with(df_E_cbs_0, ifelse(cur_result_n == "Roche Positive", 1, 0))
df_E_cbs_0$Urban <- with(df_E_cbs_0,ifelse(substr(fsa,start = 2,stop = 2) != "0",1,0))



df_E_cbs_0 <- df_E_cbs_0[!is.na(df_E_cbs_0$interp_roche_n),]

df_E_cbs_0$week <- week(df_E_cbs_0$sampledate)
df_E_cbs_0$day <- yday(df_E_cbs_0$sampledate)
df_E_cbs$vac <- with(df_E_cbs, covidvv == 1,1,0)
df_E_cbs_0$regpro = factor(case_when(as.character(substr(df_E_cbs_0$fsa,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                                     as.character(substr(df_E_cbs_0$fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                                     as.character(substr(df_E_cbs_0$fsa,1,1)) %in% c("T")~"AB",
                                     as.character(substr(df_E_cbs_0$fsa,1,1)) %in% c("R")~"MB",
                                     as.character(substr(df_E_cbs_0$fsa,1,1)) %in% c("S")~"SK",
                                     as.character(substr(df_E_cbs_0$fsa,1,1)) %in% c("V")~"BC",
                                     TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK"))

#df_E_cbs_0$age_group <- age_groups_fun(df_E_cbs_0$age)
df_E_cbs_0 <- df_E_cbs_0 %>% 
  mutate(age = 2022 - dob, age_group = age_groups_fun(age))

start_time <- Sys.time()
fit_E_cbs_0 <- stan_gamm4(interp_roche_n ~ s(day, by=regpro, k=10) + age_group + Sex + Race, 
                        random = ~ (1 | regpro) , 
                        family = binomial, 
                        data = df_E_cbs_0, iter = 2000, cores = 4, 
                        prior = normal(0,1), prior_covariance = decov(shape = 1, scale = 1), 
                        prior_smooth=normal(location=4), 
                        control=list(adapt_delta=0.9))
end_time <- Sys.time()
end_time - start_time
print(fit_E_cbs_0)

###### AbC #######
ndays = max(df_C_abc$day)-min(df_C_abc$day) + 1 #79
i=0
poststrat_t_abc= NULL
while (i < ndays){
  poststrat_t_abc <- rbind(poststrat_t_abc,poststrat_cbs_1)
  i = i+1
}
day_col <- c()
for(i in 1:ndays){
  day_col <- c(day_col,rep(min(df_C_abc$day)-1+i, nrow(poststrat_cbs_1)))
}
poststrat_t_abc$day <- day_col

#ndays = 150
output_mrp <- matrix(ncol=4, nrow=6*ndays)
regpros <- c("BC","AB","MB","SK","ON","ATL")
epred_mat <- posterior_epred(fit_C_abc_1, newdata = poststrat_t_abc, draws = 4000)
for(j in 1:6){
  for(i in 1:ndays) {
    # MRP estimate A cbs
    filtering_condition <- which(poststrat_t_abc$day == (min(df_C_abc$day)-1 + i) & poststrat_t_abc$regpro == regpros[j])
    regpro_epred_mat <- epred_mat[ ,filtering_condition]
    k_filtered <- poststrat_t_abc[filtering_condition, ]$n
    mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
    output_mrp[i + (j - 1) * ndays, ] <- c(regpros[j], mean(mrp_estimates_vector), sd(mrp_estimates_vector), min(df_C_abc$day)-1+i)
  }
}

##[Apparent Prevalence + (Specificity − 1)]/
##[Specificity + (Sensitivity − 1)]
## For abc : [x+(0.99-1)]/[0.99+0.92-1]
## x/0.91-0.01/0.91
df_time <- as.data.frame(output_mrp)
colnames(df_time) <- c('regpro','mrp_mean', 'mrp_sd', 'day')
df_time$mrp_mean <- as.numeric(df_time$mrp_mean)/0.91-0.01/0.91
df_time$mrp_sd <- as.numeric(df_time$mrp_sd)/0.91
df_time$day <- as.integer(df_time$day)

df_time$lower <- df_time$mrp_mean - 2*df_time$mrp_sd
df_time$upper <- df_time$mrp_mean + 2*df_time$mrp_sd

#####
library(dplyr)
library(tidyverse)
output_ua <- matrix(ncol=4, nrow=6*ndays)
regpros <- c("BC","AB","MB","SK","ON","ATL")
poststrat_ua <-
  df_C_abc %>%
  group_by(regpro,age_group,Sex,Race,day) %>%
  dplyr::summarize(n = n()) %>%
  drop_na()
epred_mat <- posterior_epred(fit_C_abc_1, newdata = poststrat_ua, draws = 4000)
for(j in 1:6){
  for(i in 1:ndays) {
    # MRP estimate A cbs
    filtering_condition <- which(poststrat_ua$day == (min(df_C_abc$day)-1 + i) & poststrat_ua$regpro == regpros[j])
    regpro_epred_mat <- as.matrix(epred_mat[ ,filtering_condition])
    k_filtered <- poststrat_ua[filtering_condition, ]$n
    mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
    output_ua[i + (j - 1) * ndays, ] <- c(regpros[j], mean(mrp_estimates_vector), sd(mrp_estimates_vector), min(df_C_abc$day)-1+i)
  }
}
##[Apparent Prevalence + (Specificity − 1)]/
##[Specificity + (Sensitivity − 1)]
## For Roche : [x+(0.998-1)]/[0.998+0.995-1]
## x/0.993-0.002/0.993
df_time_ua <- as.data.frame(output_ua)
colnames(df_time_ua) <- c('regpro','mrp_mean', 'mrp_sd', 'day')
df_time_ua$mrp_mean <- as.numeric(df_time_ua$mrp_mean)/0.91-0.01/0.91
df_time_ua$mrp_sd <- as.numeric(df_time_ua$mrp_sd)/0.91
df_time_ua$day <- as.integer(df_time_ua$day)

df_time_ua$lower <- df_time_ua$mrp_mean - 2*df_time_ua$mrp_sd
df_time_ua$upper <- df_time_ua$mrp_mean + 2*df_time_ua$mrp_sd

df_time$model <- 'MRP'
df_time_ua$model <- 'MR'
df_time_abc <- rbind(df_time, df_time_ua)

ggplot(df_time_abc, aes(x = day, y = mrp_mean, color = regpro)) +
  geom_line(aes(linetype=model, color=regpro)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = regpro), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  labs(title = "Seropositivity Estimation from AbC, Jan-Apr 2022",
       x = "Year-Day",
       y = "Seropositivity") +
  theme_bw()

### APL #####
df_apl <- RFD4682_e[RFD4682_e$COLLECTION_DATE >= '2022-01-01 00:00:00.0000',]
df_apl$Province = province_fun(df_apl$PAT_FSA)
#df_apl <- df_apl %>% 
#  filter(Province != ("YT") & Province !=("NU/NT") & Province != ("QC")) #4178

df_apl$age = df_apl$AGE_AT_COLLECTION
df_apl$age_group <- age_groups_fun(df_apl$age)

df_apl <- df_apl[!is.na(df_apl$GENDER),] #8783
df_apl$Sex = with(df_apl,ifelse(GENDER == "Male",1,0))

df_apl <- df_apl[!is.na(df_apl$PAT_FSA),]
df_apl$Urban <- with(df_apl,ifelse(substr(PAT_FSA,start = 2,stop = 2) != "0",1,0))

df_apl$region = factor(case_when(as.character(substr(df_apl$PAT_FSA,1,1)) %in% c("A", "B", "C", "E")~"Atlantic", 
                                   as.character(substr(df_apl$PAT_FSA,1,1)) %in% c("K", "L", "M", "N", "P")~"Ontario",
                                   as.character(substr(df_apl$PAT_FSA,1,1)) %in% c("R", "S", "T")~"Prairies",
                                   as.character(substr(df_apl$PAT_FSA,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("Atlantic","BC",  "Ontario", "Prairies"))

df_apl$regpro = factor(case_when(as.character(substr(df_apl$PAT_FSA,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                                   as.character(substr(df_apl$PAT_FSA,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                                   as.character(substr(df_apl$PAT_FSA,1,1)) %in% c("T")~"AB",
                                   as.character(substr(df_apl$PAT_FSA,1,1)) %in% c("R")~"MB",
                                   as.character(substr(df_apl$PAT_FSA,1,1)) %in% c("S")~"SK",
                                   as.character(substr(df_apl$PAT_FSA,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK"))
df_apl$regpro = 'AB'
df_apl$regpro = factor(df_apl$regpro)

df_apl <- df_apl[!is.na(df_apl$'N-IgG_INTERP'),]
df_apl$interp_roche_n <- with(df_apl,ifelse(df_apl$'N-IgG_INTERP' == "Positive",1,0))
df_apl$day <- yday(df_apl$COLLECTION_DATE)
df_apl$week <- week(df_apl$COLLECTION_DATE)
df_apl <- df_apl[!is.na(df_apl$age_group),] #37009

start_time <- Sys.time()
fit_apl <- stan_gamm4(interp_roche_n ~ s(day, k=10) + age_group + Sex, 
#                          random = ~ (1 | regpro) , 
                          family = binomial, 
                          data = df_apl, iter = 2000, cores = 4, 
                          prior = normal(0,10), prior_covariance = decov(shape = 1, scale = 1), 
                          prior_smooth=normal(location=4), 
                          control=list(adapt_delta=0.9))
end_time <- Sys.time()
end_time - start_time
print(fit_apl)


ggplot(df_time_all, aes(x = day, y = mrp_mean, color = source)) +
  geom_line(aes(linetype=model, color=source)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  labs(title = "Seropositivity Estimation during Omicron, Apr-Aug 2022",
       x = "Year-Day",
       y = "Seropositivity") +
  theme_bw()

ggplot(df_time_cbs, aes(x = day, y = mrp_mean, color = source)) +
  geom_line(aes(linetype=model, color=source)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  labs(title = "Seropositivity Estimation during Omicron, Apr-Aug 2022",
       x = "Year-Day",
       y = "Seropositivity") +
  theme_bw()

time_series <- zoo(df_time_cbs[df_time_cbs$regpro=='AB' & df_time_cbs$model=='MR',]$mrp_mean, 
                   order.by = df_time_cbs[df_time_cbs$regpro=='AB' & df_time_cbs$model=='MRP',]$day)
# Apply a simple moving average with a window size of 5
smoothed_data <- rollapply(time_series, width = 10, FUN = mean, align = "center", fill = NA)
plot_data <- data.frame(date = index(smoothed_data), value = coredata(smoothed_data))
ggplot(plot_data, aes(x = date, y = value)) +
  geom_line(color = "green", size = 1) +
  #  geom_line(data = ts_G_apl, aes(x = COLLECTION_DATE, y = rates), color = "gray", size = 0.7) +
  geom_smooth(data = df_time_cbs[df_time_cbs$regpro=='AB' & df_time_cbs$model=='MR',], aes(x = day, y = mrp_mean), method = "loess") +
  labs(title = "cbs, Smoothed MRP with Moving Average",
       x = "Day",
       y = "Anti-N Seropositivity") +
  theme_minimal()

model_predictions <- posterior_predict(fit_test_s6, newdata = df_time_cbs[df_time_cbs$regpro=='AB' & df_time_cbs$model=='MR',], type = "response", allow.new.levels = TRUE)
ggplot(df_time_cbs[df_time_cbs$regpro=='AB' & df_time_cbs$model=='MR',], aes(x = day, y = mrp_mean, color = source)) +
  geom_line(aes(linetype=model, color=source)) +
  geom_smooth(aes(y = model_predictions), method = "gam", color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
#  facet_wrap(~ regpro) +
  theme_bw()

ggplot(df_time_cbs[df_time_cbs$regpro=='AB',], aes(x = day, y = mrp_mean, color = source)) +
  geom_line(aes(linetype=model, color=source)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  theme_bw()

df_time_ccahs <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/df_time_ccahs.csv')
df_time_all <- rbind(df_time_ccahs, df_time_cbs)

ggplot(df_time_all, aes(x = day, y = mrp_mean, color = source)) +
  geom_line(aes(linetype=model, color=source)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  labs(title = "Seropositivity Estimation during Omicron, Apr-Aug 2022",
       x = "Year-Day",
       y = "Seropositivity") +
  theme_bw()

# Assuming 'your_data' is your time series data frame with columns 'date' and 'value'
# Convert 'date' to a Date object
# Convert year and day_of_year to a regular Date
df_time_all$date <- as.Date(paste(2022, sprintf("%03d", df_time_all$day), sep = "-"), format = "%Y-%j")

df_time_all$date <- as.Date(df_time_all$date)

library(zoo)
# Create a zoo object with interpolation for missing values
df_time_smooth_cbs <-  df_time_all %>% filter(source == 'CBS', model=='MRP', regpro=='AB')
df_time_smooth_ccahs <-  df_time_all %>% filter(source == 'CCAHS', model=='MRP', regpro=='AB')

interpolated_data <- zoo(df_time_smooth$mrp_mean, order.by = df_time_smooth$date)
interpolated_data <- na.approx(interpolated_data)

# Create a plot
plot(df_time_smooth$date, df_time_smooth$mrp_mean, type = "l", col = "blue", lty = 1, lwd = 2, ylim = c(min(df_time_smooth$mrp_mean), max(df_time_smooth$mrp_mean)))
lines(index(interpolated_data), coredata(interpolated_data), col = "red", lty = 2, lwd = 2)


plot(df_time_smooth_cbs$date, df_time_smooth_cbs$mrp_mean, type = "l", col = "blue", lty = 1, lwd = 2, ylim = c(min(df_time_smooth_cbs$mrp_mean), max(df_time_smooth_cbs$mrp_mean)))
lines(index(interpolated_data), coredata(interpolated_data), col = "red", lty = 2, lwd = 2)

ggplot(df_time_all, aes(x = date, y = mrp_mean, color = source)) +
  geom_smooth(aes(linetype=model, color=source),method = "loess", se = FALSE) +
  geom_ribbon(data= df_time_all %>% filter(model=='MRP'), aes(ymin = lower, ymax = upper, fill = source), alpha = 0.5, colour = NA) +
  geom_ribbon(data= df_time_all %>% filter(model=='MR'), aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  labs(title = "Seropositivity Estimation during Omicron, Apr-Aug 2022",
       x = "April- August, 2022",
       y = "Seropositivity") +
  theme_bw() +
  scale_linetype_manual(values = c("dotted", "solid"))

ggplot(df_time_all, aes(x = date, y = mrp_mean, color = source)) +
  geom_smooth(aes(linetype=model, color=source),method = "gam", se = FALSE) +
  geom_ribbon(data= df_time_all %>% filter(model=='MRP'), aes(ymin = lower, ymax = upper, fill = source), alpha = 0.5, colour = NA) +
  geom_ribbon(data= df_time_all %>% filter(model=='MR'), aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  labs(title = "Seropositivity Estimation during Omicron, Apr-Aug 2022",
       x = "April- August, 2022",
       y = "Seropositivity") +
  theme_bw() +
  scale_linetype_manual(values = c("dotted", "solid"))

ggplot(df_time_all, aes(x = date, y = mrp_mean, color = source)) +
  geom_smooth(aes(linetype=model, color=source),method = "gam", se = FALSE) +
  geom_ribbon(data= df_time_all %>% filter(model=='MRP'), aes(ymin = lower, ymax = upper, fill = source), alpha = 0.5, colour = NA) +
  geom_ribbon(data= df_time_all %>% filter(model=='MR'), aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
#  facet_wrap(~ regpro) +
  labs(title = "Seropositivity Estimation during Omicron, Apr-Aug 2022",
       x = "April- August, 2022",
       y = "Seropositivity") +
  theme_bw() +
  scale_linetype_manual(values = c("dotted", "solid"))
### Mean Absolute Bias ###

sum(abs(df_time_all[df_time_all$source == 'CCAHS' & df_time_all$model == 'MRP', ]$mrp_mean -
        df_time_all[df_time_all$source == 'CBS' & df_time_all$model == 'MRP', ]$mrp_mean))

df_time_all_dropna <- df_time_all[!is.na(df_time_all$mrp_mean),]
sum(abs(df_time_all_dropna[df_time_all_dropna$source == 'CCAHS' & df_time_all_dropna$model == 'MR', ]$mrp_mean -
        df_time_all_dropna[df_time_all_dropna$source == 'CBS' & df_time_all_dropna$model == 'MR', ]$mrp_mean))

df_time_all_match <- merge(df_time_all[df_time_all$source == 'CCAHS', ], 
                           df_time_all[df_time_all$source == 'CBS', ], by = c('regpro','model','day'), all = FALSE)

df_time_all_dropna <- df_time_all_match[!is.na(df_time_all_match$mrp_mean.x) & !is.na(df_time_all_match$mrp_mean.y),]

sum(abs(df_time_all_dropna[df_time_all_dropna$source.x == 'CCAHS' & df_time_all_dropna$model == 'MR', ]$mrp_mean.x -
        df_time_all_dropna[df_time_all_dropna$source.y == 'CBS' & df_time_all_dropna$model == 'MR', ]$mrp_mean.y))


sum(abs(df_time_all_dropna[df_time_all_dropna$model == 'MRP', ]$mrp_mean.x - 
        df_time_all_dropna[df_time_all_dropna$model == 'MRP', ]$mrp_mean.y))

mean(abs(df_time_all_dropna[df_time_all_dropna$model == 'MRP', ]$mrp_mean.x - 
          df_time_all_dropna[df_time_all_dropna$model == 'MRP', ]$mrp_mean.y))

mean(abs(df_time_all_dropna[df_time_all_dropna$model == 'MR', ]$mrp_mean.x - 
           df_time_all_dropna[df_time_all_dropna$model == 'MR', ]$mrp_mean.y))

df_time_all_dropna1 <- merge(df_time_all_dropna[df_time_all_dropna$model == 'MR', ], 
                             df_time_all_dropna[df_time_all_dropna$model == 'MRP',], 
                             by = c('regpro','day'), all.x = TRUE)

df_time_all_dropna1$ind <- with(df_time_all_dropna1, ifelse(abs(mrp_mean.x.y-mrp_mean.y.y)<abs(mrp_mean.x.x-mrp_mean.y.x), 1, 0))

df_time_all_dropna2 <-
  df_time_all_dropna1 %>%
  group_by(regpro) %>%
  dplyr::summarize(counts = sum(ind), n=n(), rates = mean(ind)) %>%
  drop_na()

############ ABC Setting C ###############
library(rstanarm)
library(lubridate)
df_C_abc$age_group <- age_groups_fun(df_C_abc$age)
start_time <- Sys.time()
fit_C_abc_1 <- stan_gamm4(interp_roche_n ~ s(day, by=regpro, k=10) + age_group + Sex + Race, 
                        random = ~ (1 | regpro) , 
                        family = binomial, 
                        data = df_C_abc, iter = 2000, cores = 4, 
                        prior = normal(0,10), prior_covariance = decov(shape = 1, scale = 1), 
                        prior_smooth=normal(location=4), 
                        control=list(adapt_delta=0.95))
end_time <- Sys.time()
end_time - start_time
print(fit_C_abc_1)

age_groups_fun <- function(variable){
  age_group = cut(variable,
                  breaks = c(18,27,37,47,57,Inf),
                  labels = c("18-26 years",
                             "27-36 years","37-46 years","47-56 years",
                             "56+ years"),
                  right = FALSE)
  return(age_group)
}

df_C_cbs$age_group <- age_groups_fun(df_C_cbs$age)
summary(df_C_cbs[df_C_cbs$age_group == "27-36 years",]$age)
summary(df_C_cbs$sampledate)
df_C_cbs$day <- yday(df_C_cbs$sampledate)
start_time <- Sys.time()
fit_C_cbs_1 <- stan_gamm4(interp_roche_n ~ s(day, by=regpro, k=10) + age_group + Sex + Race, 
                          random = ~ (1 | regpro) , 
                          family = binomial, 
                          data = df_C_cbs, iter = 2000, cores = 4, 
                          prior = normal(0,10), prior_covariance = decov(shape = 1, scale = 1), 
                          prior_smooth=normal(location=4), 
                          control=list(adapt_delta=0.95))
end_time <- Sys.time()
end_time - start_time
print(fit_C_cbs_1)

####### CBS Setting C ###########
output_mrp <- matrix(ncol=4, nrow=6*ndays)
regpros <- c("BC","AB","MB","SK","ON","ATL")
epred_mat <- posterior_epred(fit_C_cbs_1, newdata = poststrat_t_abc, draws = 4000)
for(j in 1:6){
  for(i in 1:ndays) {
    # MRP estimate A cbs
    filtering_condition <- which(poststrat_t_abc$day == (min(df_C_cbs$day)-1 + i) & poststrat_t_abc$regpro == regpros[j])
    regpro_epred_mat <- epred_mat[ ,filtering_condition]
    k_filtered <- poststrat_t_abc[filtering_condition, ]$n
    mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
    output_mrp[i + (j - 1) * ndays, ] <- c(regpros[j], mean(mrp_estimates_vector), sd(mrp_estimates_vector), min(df_C_cbs$day)-1+i)
  }
}

##[Apparent Prevalence + (Specificity − 1)]/
##[Specificity + (Sensitivity − 1)]
## For Roche : [x+(0.998-1)]/[0.998+0.995-1]
## x/0.993-0.002/0.993
df_time <- as.data.frame(output_mrp)
colnames(df_time) <- c('regpro','mrp_mean', 'mrp_sd', 'day')
df_time$mrp_mean <- as.numeric(df_time$mrp_mean)/0.993-0.002/0.993
df_time$mrp_sd <- as.numeric(df_time$mrp_sd)/0.993
df_time$day <- as.integer(df_time$day)

df_time$lower <- df_time$mrp_mean - 2*df_time$mrp_sd
df_time$upper <- df_time$mrp_mean + 2*df_time$mrp_sd

#####
library(dplyr)
output_ua <- matrix(ncol=4, nrow=6*ndays)
regpros <- c("BC","AB","MB","SK","ON","ATL")
poststrat_ua <-
  df_C_cbs %>%
  group_by(regpro,age_group,Sex,Race,day) %>%
  dplyr::summarize(n = n()) %>%
  drop_na()
epred_mat <- posterior_epred(fit_C_cbs_1, newdata = poststrat_ua, draws = 4000)
for(j in 1:6){
  for(i in 1:ndays) {
    # MRP estimate A cbs
    filtering_condition <- which(poststrat_ua$day == (min(df_C_cbs$day)-1 + i) & poststrat_ua$regpro == regpros[j])
    regpro_epred_mat <- epred_mat[ ,filtering_condition]
    mrp_estimates_vector <- regpro_epred_mat
    output_ua[i + (j - 1) * ndays, ] <- c(regpros[j], mean(mrp_estimates_vector), sd(mrp_estimates_vector), min(df_C_cbs$day)-1+i)
  }
}

# for(j in 1:6){
#   for(i in 1:ndays) {
#     # MRP estimate A cbs
#     filtering_condition <- which(poststrat_ua$day == (min(df_C_cbs$day)-1 + i) & poststrat_ua$regpro == regpros[j])
#     regpro_epred_mat <- as.matrix(epred_mat[ ,filtering_condition])
#     k_filtered <- poststrat_ua[filtering_condition, ]$n
#     mrp_estimates_vector <- regpro_epred_mat %*% k_filtered / sum(k_filtered)
#     output_ua[i + (j - 1) * ndays, ] <- c(regpros[j], mean(mrp_estimates_vector), sd(mrp_estimates_vector), min(df_C_cbs$day)-1+i)
#   }
# }

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
df_time_cbs_C <- rbind(df_time, df_time_ua)

df_time_abc$source <- 'AbC'
df_time_cbs_C$source <- 'CBS'
df_time_all <- rbind(df_time_abc, df_time_cbs_C)

df_time_all$date <- as.Date(paste(2022, sprintf("%03d", df_time_all$day), sep = "-"), format = "%Y-%j")

df_time_all$date <- as.Date(df_time_all$date)

ggplot(df_time_all, aes(x = day, y = mrp_mean, color = source)) +
  geom_line(aes(linetype=model, color=source)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  labs(title = "Seropositivity Estimation during Omicron, Jan-Apr 2022",
       x = "Year-Day",
       y = "Seropositivity") +
  theme_bw()


desired_order2 <- c("CBS","AbC")

# Reorder the levels of the 'category' variable

df_time_all$source <- factor(df_time_all$source, levels = desired_order2)

ggplot(df_time_all, aes(x = date, y = mrp_mean, color = source)) +
  geom_smooth(aes(linetype=model, color=source),method = "loess", se = FALSE) +
  geom_ribbon(data= df_time_all %>% filter(model=='MRP'), aes(ymin = lower, ymax = upper, fill = source), alpha = 0.5, colour = NA) +
  geom_ribbon(data= df_time_all %>% filter(model=='MR'), aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  labs(title = "Seropositivity Estimation during Omicron, Jan-Apr 2022",
       x = "January-April, 2022",
       y = "Seropositivity") +
  theme_bw() +
#  scale_color_manual(values = c("#F8766D","#00BFC4")) +
  scale_linetype_manual(values = c("dotted", "solid"))

### Mean Absolute Bias & Proportion of closer estimates ###

sum(abs(df_time_all[df_time_all$source == 'AbC' & df_time_all$model == 'MRP', ]$mrp_mean -
          df_time_all[df_time_all$source == 'CBS' & df_time_all$model == 'MRP', ]$mrp_mean))

df_time_all_dropna <- df_time_all[!is.na(df_time_all$mrp_mean),]
sum(abs(df_time_all_dropna[df_time_all_dropna$source == 'AbC' & df_time_all_dropna$model == 'MR', ]$mrp_mean -
          df_time_all_dropna[df_time_all_dropna$source == 'CBS' & df_time_all_dropna$model == 'MR', ]$mrp_mean))

df_time_all_match <- merge(df_time_all[df_time_all$source == 'AbC', ], 
                           df_time_all[df_time_all$source == 'CBS', ], by = c('regpro','model','day'), all = FALSE)

df_time_all_dropna <- df_time_all_match[!is.na(df_time_all_match$mrp_mean.x) & !is.na(df_time_all_match$mrp_mean.y),]

sum(abs(df_time_all_dropna[df_time_all_dropna$source.x == 'AbC' & df_time_all_dropna$model == 'MR', ]$mrp_mean.x -
          df_time_all_dropna[df_time_all_dropna$source.y == 'CBS' & df_time_all_dropna$model == 'MR', ]$mrp_mean.y))


sum(abs(df_time_all_dropna[df_time_all_dropna$model == 'MRP', ]$mrp_mean.x - 
          df_time_all_dropna[df_time_all_dropna$model == 'MRP', ]$mrp_mean.y))

mean(abs(df_time_all_dropna[df_time_all_dropna$model == 'MRP', ]$mrp_mean.x - 
           df_time_all_dropna[df_time_all_dropna$model == 'MRP', ]$mrp_mean.y))

mean(abs(df_time_all_dropna[df_time_all_dropna$model == 'MR', ]$mrp_mean.x - 
           df_time_all_dropna[df_time_all_dropna$model == 'MR', ]$mrp_mean.y))

df_time_all_dropna1 <- merge(df_time_all_dropna[df_time_all_dropna$model == 'MR', ], 
                             df_time_all_dropna[df_time_all_dropna$model == 'MRP',], 
                             by = c('regpro','day'), all.x = TRUE)

df_time_all_dropna1$ind <- with(df_time_all_dropna1, ifelse(abs(mrp_mean.x.y-mrp_mean.y.y)<abs(mrp_mean.x.x-mrp_mean.y.x), 1, 0))

df_time_all_dropna2 <-
  df_time_all_dropna1 %>%
  group_by(regpro) %>%
  dplyr::summarize(counts = sum(ind), n=n(), rates = mean(ind)) %>%
  drop_na()

start_time <- Sys.time()
fit_C_abc_ab <- stan_gamm4(interp_roche_n ~ s(day, k=10) + age_group + Sex + Race, 
#                          random = ~ (1 | regpro) , 
                          family = binomial, 
                          data = df_C_abc[df_C_abc$regpro=='AB',], iter = 2000, cores = 4, 
                          prior = normal(0,1), prior_covariance = decov(shape = 1, scale = 1), 
                          prior_smooth=normal(location=4), 
                          control=list(adapt_delta=0.9))
end_time <- Sys.time()
end_time - start_time
print(fit_C_abc_ab)


df_time_all <- rbind(df_time_ccahs, df_time_cbs)

df_time_all$date <- as.Date(paste(2022, sprintf("%03d", df_time_all$day), sep = "-"), format = "%Y-%j")

df_time_all$date <- as.Date(df_time_all$date)

df_time_all$lower = with(df_time_all,ifelse(lower < 0,0,lower))


df_time_raw_abc <-
  df_C_abc %>%
  group_by(regpro, day) %>%
  dplyr::summarize(mrp_mean = mean(interp_roche_n)) %>%
  drop_na()

df_time_raw_cbs_C <-
  df_C_cbs %>%
  group_by(regpro, day) %>%
  dplyr::summarize(mrp_mean = mean(interp_roche_n)) %>%
  drop_na()

df_time_raw_cbs <-
  df_E_cbs_1 %>%
  group_by(regpro, day) %>%
  dplyr::summarize(mrp_mean = mean(interp_roche_n)) %>%
  drop_na()
desired_order2 <- c("CBS","CCAHS")

# Reorder the levels of the 'category' variable

df_time_all$source <- factor(df_time_all$source, levels = desired_order2)

df_time_all_abc3$source <- factor(df_time_all_abc3$source, levels = c("CBS","ABC"))

df_time_all_abc3[df_time_all_abc3$source=='AbC',]$source = 'ABC'
df_time_all_abc3$source <- as.character(df_time_all_abc3$source)
df_time_all_abc3[is.na(df_time_all_abc3$source),]$source = 'ABC'

ggplot(df_time_all_abc3, aes(x = date, y = mrp_mean, color = source)) +
  geom_smooth(aes(linetype=model, color=source),method = "loess", se = FALSE) +
  geom_ribbon(data= df_time_all_abc3 %>% filter(model=='MRP'), aes(ymin = lower, ymax = upper, fill = source), alpha = 0.5, colour = NA) +
  geom_ribbon(data= df_time_all_abc3 %>% filter(model=='MR'), aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  labs(#title = "Seropositivity Estimation",
       x = "January-April, 2022",
       y = "Seropositivity") +
  theme_bw() +
  scale_color_manual(name = "Study", values = c("#F8766D","#00BFC4")) +
  scale_fill_manual(name = "Study", values = c("#F8766D","#00BFC4")) +
  scale_linetype_manual(name="Adjustment",values = c("dotted", "solid", "dashed")) +
  theme(legend.position = "bottom")

ggplot(df_time_cbs_C, aes(x = day, y = mrp_mean, color = regpro)) +
  geom_line(aes(linetype=model, color=regpro)) +
#  geom_smooth(data=df_time_raw_cbs_C, aes(x = day, y = mrp_mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = regpro), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  labs(title = "CBS",
       x = "Year-Day",
       y = "Seropositivity") +
  theme_bw()

ggplot(df_time_abc, aes(x = day, y = mrp_mean, color = regpro)) +
  geom_line(aes(linetype=model, color=regpro)) +
#  geom_smooth(data=df_time_raw_abc, aes(x = day, y = mrp_mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = regpro), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  labs(title = "AbC",
       x = "Year-Day",
       y = "Seropositivity") +
  theme_bw()

df_time_raw <- df_time_abc
df_time_raw[] <- NA
df_time_raw$regpro <- df_time_raw_abc$regpro
df_time_raw$day <- df_time_raw_abc$day
df_time_raw$mrp_mean <- df_time_raw_abc$mrp_mean
df_time_raw$source <- 'AbC'


df_time_raw <- df_time_abc
df_time_raw[] <- NA
df_time_raw_1 <- merge(df_time_raw_abc, df_time_raw, by=c('regpro','day','mrp_mean'), all.x=TRUE)
df_time_raw_1$source <- 'AbC'
df_time_raw_1$model <- 'RAW'

df_time_raw <- df_time_cbs_C
df_time_raw[] <- NA
df_time_raw_2 <- merge(df_time_raw_cbs_C, df_time_raw, by=c('regpro','day','mrp_mean'), all.x=TRUE)
df_time_raw_2$source <- 'CBS'
df_time_raw_2$model <- 'RAW'

df_time_all <- rbind(df_time_abc, df_time_cbs_C, df_time_raw_1, df_time_raw_2)
df_time_all$date <- as.Date(paste(2022, sprintf("%03d", df_time_all$day), sep = "-"), format = "%Y-%j")

df_time_all$date <- as.Date(df_time_all$date)
#df_time_cbs_C$source <- 'CBS'

df_time_all <- df_time_all[!apply(df_time_all, 1, function(x) all(is.na(x))), ]

df_time_all_abc3 <- df_time_all


########

df_time_raw <- df_time_cbs
df_time_raw[] <- NA
df_time_raw_2 <- merge(df_time_raw_cbs, df_time_raw, by=c('regpro','day','mrp_mean'), all.x=TRUE)
df_time_raw_2$source <- 'CBS'
df_time_raw_2$model <- 'RAW'

df_time_all <- rbind(df_time_ccahs, df_time_cbs, df_time_raw_2)
df_time_all$date <- as.Date(paste(2022, sprintf("%03d", df_time_all$day), sep = "-"), format = "%Y-%j")

df_time_all$date <- as.Date(df_time_all$date)
#df_time_cbs_C$source <- 'CBS'

df_time_all <- df_time_all[!apply(df_time_all, 1, function(x) all(is.na(x))), ]

df_time_all_ccahs2 <- df_time_all


df_time_all_ccahs2$lower = with(df_time_all_ccahs2,ifelse(lower < 0,0,lower))
df_time_all_abc3$lower = with(df_time_all_abc3,ifelse(lower < 0,0,lower))

df_time_all_ccahs2$source <- factor(df_time_all_ccahs2$source, levels = c("CBS","CCAHS"))
ggplot(df_time_all_ccahs2, aes(x = date, y = mrp_mean, color = source)) +
  geom_smooth(aes(linetype=model, color=source),method = "loess", se = FALSE) +
  geom_ribbon(data= df_time_all_ccahs2 %>% filter(model=='MRP'), aes(ymin = lower, ymax = upper, fill = source), alpha = 0.5, colour = NA) +
  geom_ribbon(data= df_time_all_ccahs2 %>% filter(model=='MR'), aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  labs(#title = "Seropositivity Estimation",
       x = "April-August, 2022",
       y = "Seropositivity") +
  theme_bw() +
  scale_color_manual(name = "Study", values = c("#F8766D","#00BFC4")) +
  scale_fill_manual(name = "Study", values = c("#F8766D","#00BFC4")) +
  scale_linetype_manual(name="Adjustment", values = c("dotted", "solid", "dashed")) +
  theme(legend.position = "bottom")

unique(df_cbs$ethnic1)



start_time <- Sys.time()
fit_C_combine_1 <- stan_gamm4(interp_roche_n ~ s(day, by=regpro, k=10) + age_group + Sex + Race, 
                            random = ~ (1 | regpro) , 
                            family = binomial, 
                            data = df_C_abc, iter = 2000, cores = 4, 
                            prior = normal(0,1), prior_covariance = decov(shape = 1, scale = 1), 
                            prior_smooth=normal(location=4), 
                            control=list(adapt_delta=0.9))
end_time <- Sys.time()
end_time - start_time
print(fit_C_combine_1)

install.packages("xlsx")
library(xlsx)
df_time_ccahs_lab <- read.xlsx(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/4_output/df_time_ccahs_lab.xlsx', 'df_time_ccahs_lab', header=TRUE)
df_time_ccahs_lab <- df_time_ccahs_lab[ ,c("regpro","mrp_mean","mrp_sd","day","lower","upper","model")]
df_time_ccahs_lab$source <- 'CCAHS'
df_time_ccahs_lab <- 
  df_time_ccahs_lab %>%
  mutate(mrp_mean = as.numeric(mrp_mean),
         mrp_sd = as.numeric(mrp_sd),
         lower = as.numeric(lower),
         upper = as.numeric(upper))

df_time_cbs_raw <-
  df_time_all_ccahs2[df_time_all_ccahs2$model == 'RAW', 1:8] %>%
  mutate(mrp_mean = mrp_mean/0.993 - 0.002/0.993,
         mrp_sd = mrp_sd/0.993,
         lower = mrp_mean - 2*mrp_sd,
         upper = mrp_mean + 2*mrp_sd)

df_time_all_ccahs3 <- rbind(df_time_ccahs_lab[df_time_ccahs_lab$model %in% c("MRP", "MR"),], df_time_cbs,
                            df_time_cbs_raw)
df_time_all_ccahs3$source <- factor(df_time_all_ccahs3$source, levels = c("CBS","CCAHS"))

ggplot(df_time_all_ccahs3, aes(x = day, y = mrp_mean, color = source)) +
  geom_smooth(aes(linetype=model, color=source),method = "loess", se = FALSE) +
  geom_ribbon(data= df_time_all_ccahs3 %>% filter(model=='MRP'), aes(ymin = lower, ymax = upper, fill = source), alpha = 0.5, colour = NA) +
  geom_ribbon(data= df_time_all_ccahs3 %>% filter(model=='MR'), aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  labs(title = "Seropositivity Estimation",
       x = "April-August, 2022",
       y = "Seropositivity") +
  theme_bw() +
  scale_color_manual(values = c("#F8766D","#00BFC4")) +
  scale_linetype_manual(values = c("dotted", "solid", "dashed"))

#no-rg

df_time_cbs_no_rg <-
  df_time_cbs %>%
  mutate(mrp_mean = (mrp_mean + 0.002/0.993)*0.993,
         mrp_sd = mrp_sd*0.993,
         lower = mrp_mean - 2*mrp_sd,
         upper = mrp_mean + 2*mrp_sd)

df_time_ccahs_lab_no_rg <- df_time_ccahs_lab[df_time_ccahs_lab$model %in% c("MRP_no_rg", "MR_no_rg"),] %>%
  mutate(model = case_when(model=="MRP_no_rg" ~ "MRP",
                           model=="MR_no_rg" ~ "MR"))
  
df_time_all_ccahs3_no_rg <- rbind(df_time_ccahs_lab_no_rg, df_time_cbs_no_rg,
                            df_time_all_ccahs2[df_time_all_ccahs2$model == 'RAW', 1:8])

df_time_all_ccahs3_no_rg$source <- factor(df_time_all_ccahs3$source, levels = c("CBS","CCAHS"))

ggplot(df_time_all_ccahs3_no_rg, aes(x = day, y = mrp_mean, color = source)) +
  geom_smooth(aes(linetype=model, color=source),method = "loess", se = FALSE) +
  geom_ribbon(data= df_time_all_ccahs3_no_rg %>% filter(model=='MRP'), aes(ymin = lower, ymax = upper, fill = source), alpha = 0.5, colour = NA) +
  geom_ribbon(data= df_time_all_ccahs3_no_rg %>% filter(model=='MR'), aes(ymin = lower, ymax = upper, fill = source), alpha = 0.2, colour = NA) +
  facet_wrap(~ regpro) +
  labs(title = "Seropositivity Estimation without Rogan-Gladen Adjustment",
       x = "April-August, 2022",
       y = "Seropositivity") +
  theme_bw() +
  scale_color_manual(values = c("#F8766D","#00BFC4")) +
  scale_linetype_manual(values = c("dotted", "solid", "dashed"))


# DTW
#df_time_all_ccahs3
time_index <- seq(min(df_time_all_ccahs3$day),max(df_time_all_ccahs3$day),1)
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
df_dtw <- data.frame(output_dtw)
colnames(output_dtw) <- c('MR', 'MRP')
output_dtw


#df_time_all_ccahs3_no_rg
time_index <- seq(min(df_time_all_ccahs3_no_rg$day),max(df_time_all_ccahs3_no_rg$day),1)
output_dtw <- matrix(ncol=2, nrow=6)
for(j in 1:6){
  # MRP estimate A cbs
  filtering_condition <- which(df_time_all_ccahs3_no_rg$regpro == regpros[j])
  df_sub <- df_time_all_ccahs3_no_rg[filtering_condition,]
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
df_dtw <- data.frame(output_dtw)
colnames(output_dtw) <- c('MR', 'MRP')
output_dtw
