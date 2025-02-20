# Fig 1
install.packages("ggplot2")
install.packages("ggalt")  # for ggalt::geom_dumbbell
library(ggplot2)
library(ggalt)
library(dplyr)
# Create example data
data <- data.frame(
  Category = c("A", "B", "C", "D","A","B", "C", "D"),
  Year2010 = c(2.0, 3.0, 1.0, 5.0, 15, 20, 20, 40),
  Year2020 = c(4.0, 3.5, 1.5, 5.5, 30, 35, 10, 50),
  setting = c("A", "A","A","A","B","B","B","B")
)
# Load the ggalt package for geom_dumbbell
#library(ggalt)

# Create the dumbbell plot
dumbbell_plot <- ggplot(data, aes(y = Category)) +
  geom_dumbbell(aes(x = Year2010, xend = Year2020),
                size = 1, color = "skyblue", 
                size_x = 2.5, colour_x = "darkblue",
                size_xend = 2.5, colour_xend = "darkblue") +
  facet_wrap(~ setting, scales = "free") +
  labs(x = "Value", y = "Category", 
       title = "Dumbbell Plot Showing Changes from 2010 to 2020",
       subtitle = "Each line connects two years for a given category.") +
  theme_linedraw()

# Print the plot
print(dumbbell_plot)



###########
install.packages('ggtext')
install.packages('extrafont')
library(tidyverse)
library(ggtext)
library(extrafont)
# font_import()  #run once
loadfonts(device = "win") #run once per session

hbcu_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv')
hbcu_all %>%
  filter(Year >= 1990) %>% #filter the year
  select(Year, Males, Females)%>% #select columns of interest
  mutate(diff = Females - Males) %>% #calculate difference
  pivot_longer(cols = c(Males, Females)) %>% #get into long format
  rename(Gender = name, #rename columns
         Enrollments = value)-> dat_gender
head(dat_gender)
Males <- dat_gender %>%
  filter(Gender == "Males")
Females <- dat_gender %>%
  filter(Gender == "Females")
head(Females)

p <- ggplot(dat_gender)+
  
  geom_segment(data = Males,
               aes(x = Enrollments, y = Year,
                   yend = Females$Year, xend = Females$Enrollments), #use the $ operator to fetch data from our "Females" tibble
               color = "#aeb6bf",
               size = 4.5, #Note that I sized the segment to fit the points
               alpha = .5) +
  
  geom_point(aes(x = Enrollments, y = Year, color = Gender), size = 4, show.legend = TRUE)+
  
  ggtitle("Enrollment Trends at Historically Black Colleges and Universities")
p

dat_gender %>%
  group_by(Gender) %>%
  summarise(mean = mean(Enrollments),
            SE = sd(Enrollments)) %>%
  mutate(meanpos = mean + 1 *SE,
         meanneg = mean - 1 *SE)-> stats
stats_males <- stats %>%
  filter(Gender == "Males")
stats_females <- stats %>%
  filter(Gender == "Females")

diff <- dat_gender %>% 
  filter(Gender == "Males") %>% #you can chose Males of Females, doesn't matter
  mutate(x_pos = Enrollments + (diff/2)) #x position of label (Enrollment value of Males + diff/2)

p + 
  geom_text(data = diff,
            aes(label = paste("D: ",diff), x = x_pos, y = Year), #note thatI changed the greek letter Delta to "D:" because of encoding reasons
            fill = "white",
            color = "#4a4e4d",
            size = 2.5,
            family = "Segoe UI Semibold") -> p_labelled
p_labelled


p_labelled +
  
  #add facet for more control
  facet_grid(Year ~ ., scales = "free", switch = "y") +
  
  #theming
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Segoe UI Semibold", color = "#4a4e4d"),
        strip.text.y.left  = element_text(angle = 0),
        panel.background = element_rect(fill = "white", color = "white"),
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "#4a4e4d", family = "Segoe UI"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.spacing = unit(0, "lines"),
        plot.margin = margin(1,1,.5,1, "cm"))-> p_ext_facetted
p_ext_facetted


ggplot(dat_gender)+
  
  #add mean and standard deviation for both groups
  geom_rect(xmin = stats_males$meanneg, xmax = stats_males$meanpos,
            ymin = 2016, ymax = 1989, fill = "#762a83", alpha = .05)+
  geom_vline(xintercept = stats_males$mean, linetype = "solid", size = .5, alpha = .8, color = "#762a83")+
  
  geom_rect(xmin = stats_females$meanneg, xmax = stats_females$meanpos,
            ymin = 2016, ymax = 1989, fill = "#009688", alpha = .05)+  
  geom_vline(xintercept = stats_females$mean, color = "#009688", linetype = "solid",  size = .5, alpha = .8) +
  
  #add point range
  geom_segment(data = Males, aes(x = Enrollments, y = Year, yend = Females$Year, , xend = Females$Enrollments),
               color = "#aeb6bf", size = 4.5, alpha = .5) +
  
  #add points
  geom_point(aes(x = Enrollments, y = Year, color = Gender), size = 4, show.legend = FALSE) +
  
  #color points
  scale_color_manual(values = c("#009688","#762a83"))+
  #add point-range labels
  geom_text(data = diff, aes(label = paste("D: ",diff), x = x_pos, y = Year), fill = "white", color = "#4a4e4d", size = 2.5, family = "Segoe UI") +
  
  #add annotations for mean and standard deviations
  geom_text(x = stats_females$mean - 1500, y = 1990, label = "MEAN", angle = 90, size = 2.5, color = "#009688", family = "Segoe UI")+
  geom_text(x = stats_females$meanpos -1500, y = 1990, label = "STDEV", angle = 90, size = 2.5, color = "#009688", family = "Segoe UI")+
  
  #add facets for more control
  facet_grid(Year ~ ., scales = "free", switch = "y") +
  #add title
  ggtitle("Enrollment Trends at Historically Black Colleges and Universities")+
  #theming
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Segoe UI", color = "#4a4e4d"),
        strip.text.y.left  = element_text(angle = 0),
        panel.background = element_rect(fill = "white", color = "white"),
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "#4a4e4d", family = "Segoe UI"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.spacing = unit(0, "lines"),
        plot.margin = margin(1,1,.5,1, "cm")) -> p_styled
p_styled

regpro_df_ABCE <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_ABCE_v (1).csv')

# Create the dumbbell plot
dumbbell_plot <- ggplot(regpro_df_ABCE, aes(y = regpro)) +
  geom_dumbbell(aes(x = ua_mean, xend = mrp_mean),
                size = 1, color = "skyblue", 
                size_x = 2.5, colour_x = "#009688",
                size_xend = 2.5, colour_xend = "darkblue") +
  facet_wrap(~ setting + source, scales = "free") +
  labs(x = "Seropositivity", y = "Province", 
       title = "Dumbbell Plot Showing Seropositivity Changes Before & After Statistical Ajustment",
       subtitle = "Each line connects two estimates for a given region/province.") +
  theme_bw()

# Print the plot
print(dumbbell_plot)


regpro_df_ABCE_v <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/regpro_df_ABCE_v (1).csv')

setting.labs <- c("Jan-Apr, 2021", "Jul-Aug, 2021", "Jan-Apr, 2022","Apr-Sep, 2022")
names(setting.labs) <- c("A", "B", "C", "D")



ggplot(regpro_df_ABCE_v) +
  geom_point(
    aes(
      x = regpro, y = ua_mean, color = source,
      shape = Adjustment, group = source
    ), 
    position = position_dodge(width = 0.5)
  ) +
  scale_shape_manual(values = c(19, 1)) +
  geom_line(
    aes(
      x = regpro, y = ua_mean, color = source, 
      group = interaction(source, regpro)
    ),
    position = position_dodge(width = 0.5)
  ) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ setting, scales = "free",
             labeller = labeller(setting = setting.labs)) +
  coord_flip() + 
  labs(y = "Seropositivity", x = "Province/Region", 
       title = "Seropositivity Changes Before & After Statistical Ajustment",
       subtitle = "Each line connects two estimates for a given region/province.") +
  theme_bw()
  
dim(fit_A_abc$data)  
dim(fit_A_cbs$data)

dim(fit_B_abc$data)  
dim(fit_B_cbs$data)

dim(fit_C_abc$data)  
dim(fit_C_cbs$data)

summary(fit_C_cbs$data$sampledate)

summary(fit_E_cbs$data$sampledate)

dim(df_E_cbs[df_E_cbs$sampledate > '2022-04-12',])

View(regpro_df_ABCE_v)

### Full population (no clsa) shape=4

### Elder population (CLSA and CBS)

sscrnls_v2 <- data.frame(
  lab = c( "MAL", "ACG", "Abbott","Roche"),
  ab_target = c("N","N", "N", "N"),
  sens = c(0.7109, 0.835, 0.927, 0.972),
  spec = c(0.9013, 0.964, 0.999, 0.998)
) 


regpro_df_ABCE_v2 <-
regpro_df_ABCE_v %>% 
  mutate(
    sens = case_when(source =='ABC' | (source =='CANPATH' & regpro!='AB') ~ 0.835, # Gingras Lab
                     source %in% c('APL') | (source =='CANPATH' & regpro=='AB') ~ 0.927, # Abbott
                     source %in% c('CBS','CLSA') ~ 0.972, # Roche
                     source =='CCAHS' & setting =='A' ~ 1, # ccahs I 
                     TRUE~NA),
    spec = case_when(source =='ABC' | (source =='CANPATH' & regpro!='AB') ~ 0.964, # Gingras Lab
                     source %in% c('APL') | (source =='CANPATH' & regpro=='AB') ~ 0.999, # Abbott
                     source %in% c('CBS','CLSA') ~ 0.998, # Roche
                     source =='CCAHS' & setting =='A' ~ 1, # ccahs I 
                     TRUE~NA),
    ua_mean_rg = ifelse(ua_mean+(spec-1)>=0,(ua_mean+(spec-1))/(spec+sens-1),ua_mean)
  )  %>% 
  filter(Adjustment == 'MRP_Adjusted') %>% 
  mutate(ua_mean = ua_mean_rg, 
         Adjustment = 'MRP_Adjusted_rg') %>% 
  select(c("regpro","ua_mean","Adjustment","source","setting"))



lf3 <- df_time_raw_apl %>%
  left_join(sscrnls, 
            by = c("lab", "ab_target")) %>% 
  mutate(
    seroprev_est_RGadj = (seroprev_est + (spec - 1)) /
      (spec + (sens - 1))
  )

regpro_df_ABCE_v3 <- rbind(regpro_df_ABCE_v, regpro_df_ABCE_v2)
write.csv(regpro_df_ABCE_v3, "/home/yuanyu/projects/covid-donor-v-survey-antibody/4_output/regpro_df_ABCE_v3.csv")                           
regpro_df_ABCE_v3 <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/4_output/regpro_df_ABCE_v3.csv') 

regpro_df_ABCE_v3 <- filter(regpro_df_ABCE_v3, source != 'CLSA')


ggplot(regpro_df_ABCE_v3) +
  geom_point(
    aes(
      x = regpro, y = ua_mean, color = source,
      shape = Adjustment, group = source
    ), 
    position = position_dodge(width = 0.5)
  ) +
#  scale_shape_manual(values = c(4, 19, 1)) +
  geom_line(
    aes(
      x = regpro, y = ua_mean, color = source, 
      group = interaction(source, regpro)
    ),
    position = position_dodge(width = 0.5)
  ) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ setting, scales = "free",
             labeller = labeller(setting = setting.labs)) +
  coord_flip() + 
  labs(y = "Seropositivity", x = "Province/Region" 
       #title = "Seropositivity Changes Before & After Statistical Ajustment",
       #subtitle = "Each line connects two estimates for a given region/province."
       ) +
  labs(color = "Study") +
  scale_shape_manual(
    name = "Adjustment",
    values = c("MRP adjusted" = 4, 
               "MRP adjusted with RG" = 16 ,
               "Unadjusted" = 1),
    labels = c(c("MRP adjusted" = "MRP adjusted", 
                 "MRP adjusted with RG" = "MRP adjusted with Rogan-Gladen" ,
                 "Unadjusted" = "Unadjusted"))) +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.justification = 'left',
        legend.box.just = 'left') +
  guides(
    color = guide_legend(order = 2),  # Color legend second
    shape = guide_legend(order = 1))    # Shape legend first


regpro_df_ABCE_v3 <-
  regpro_df_ABCE_v3 %>%
  mutate(Adjustment = factor(case_when(Adjustment == 'MRP_Adjusted' ~ 'MRP adjusted', 
                                       Adjustment == 'MRP_Adjusted_rg' ~ 'MRP adjusted with RG',
                                       Adjustment == 'Unadjusted' ~ 'Unadjusted', 
                                       TRUE~NA)) )

regpro_df_ABCE_v3[regpro_df_ABCE_v3$source=='CANPATH',]$source = 'CanPath'
  