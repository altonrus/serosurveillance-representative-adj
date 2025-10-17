#Load CLSA data
#Load conbined table to join demo/geo graphical covariate
df_clsa_cb <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/1_data/private/CLSA/2209005_McGill_ARussell_Covid/2209005_McGill_ARussell_Covid_Combined_v1-1.csv')
dim(df_clsa_cb)
colnames(df_clsa_cb)[1]
col_list <- c("entity_id", "source_COVID", "PROV_COVID", "FSA_COVID", "CSD_COVID", "URBAN_RURAL_COVID", "POP_CNTR_COVID", 
              "POP_DENSITY_COVID", "cohort")
df_clsa_cb <- df_clsa_cb[col_list]

#Load antibody participants cohort
df_clsa_anti <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/1_data/private/CLSA/2209005_McGill_ARussell_Covid/2209005_McGill_ARussell_Covid_Antibody_Combined_NoIndigenousIdentifiers_v1.csv')
dim(df_clsa_anti)
colnames(df_clsa_anti)
col_list <- c("entity_id", "SER_ADM_COV", "SER_AGE_COV", "SER_SEX_COV", "start_datetime_COV", "SER_CURRSEX_COV", 
              "SER_CURRSEX_SP_COV", "SER_GENDER_COV", "SER_GENDER_SP_COV", "SER_ETHN_WH_COV", "SER_ETHN_SA_COV", 
              "SER_ETHN_ZH_COV", "SER_ETHN_BL_COV", "SER_ETHN_FP_COV", "SER_ETHN_LA_COV", "SER_ETHN_AR_COV", "SER_ETHN_SE_COV",
              "SER_ETHN_WA_COV", "SER_ETHN_KO_COV", "SER_ETHN_JA_COV", "SER_ETHN_OTSP_COV", "SER_ETHN_DK_NA_COV", 
              "SER_ETHN_REFUSED_COV","SER_EDU_COV", "SER_LIVH_NB_COV", "SER_BEDR_NB_COV", "SER_BATHR_NB_COV", "SER_WRK_HCW_COV", 
              "SER_WRK_FR_COV", "SER_WRK_CCW_COV", "SER_WRK_CO_COV", "SER_WRK_TC_COV", "SER_WRK_FS_COV", "SER_WRK_GS_COV", 
              "SER_WRK_PH_COV", "SER_WRK_AT_COV", "SER_WRK_FA_COV", "SER_WRK_FW_COV", "SER_WRK_TD_COV", "SER_WRK_HD_COV", 
              "SER_PG10_NB_COV", "SER_FAMPH_COV", "SER_FLUVAC_COV", "SER_MASK_COV", "SER_DIST_COV", "SER_CROWD_COV", 
              "SER_GREET_COV", "SER_LIMIT_COV", "SER_SLFISO_COV", "SER_SLFQA_COV", "SER_VAC_COV", "SER_VDOSE_COV",
              "ICQ_start_datetime_COV", "BLD_WNOB_COV", "BLD_WNOB_SP_COV", "BLD_FATT_COV", "BLD_FATT_NO_COMMT_COV", 
              "BLD_DECL_POS_COV", "BLD_TECH_REA_COV", "BLD_NEEDLE_COV", "BLD_SIT_REC_COV", "SER_VACTYPE_OTSP_COV", 
              "SER_NUCLEOCAPSID_COV", "SER_SPIKE_COV", "SER_ABRSLT_COV")
df_clsa <- df_clsa_anti[col_list]

table(df_clsa_anti$SER_ADM_COV)

df_clsa_anti[df_clsa_anti$SER_ADM_COV == 2, ][1,]
summary(df_clsa_anti$SER_AGE_COV)
table(df_clsa_anti$SER_AGE_COV)

#Join FSA
df_clsa <- merge(df_clsa, df_clsa_cb, by='entity_id', all.x = TRUE)
df_clsa[df_clsa$SER_ADM_COV == 2, ][1,]
#func_province <- function(x) {names(pruid)[which(unlist(pruid) == x)]}
#df_clsa$province <- sapply(df_clsa$PROV_COVID, func_province)
#names(pruid)[which(unlist(pruid) == 35)]
df_clsa$province <- province_fun(df_clsa$FSA_COVID)

#----Generate counts by collection type and province-----
library(tidyverse)
df_clsa %>% 
  group_by(SER_ADM_COV,province) %>% 
  dplyr::summarise(Count = n())

#Check the time range
df_clsa$date <- as.Date(df_clsa$start_datetime_COV)
summary(df_clsa$date) # "2020-10-29" ~ "2021-08-31"
summary(data6$sampledate) # "2020-12-14" ~ "2022-11-30"
#Check the age range
summary(df_clsa$SER_AGE_COV) # 51~96
summary(data6$age) # 17~93

df_clsa$month <- format(df_clsa$date,"%m")

#remove NA date
#remove non-overlap time
#remove QC & NA province
#NA age, race, sex

dim(cbs_data[cbs_data$sampledate < "2020-12-14", ])


#Load Ab-C data
#Load conbined table to join demo/geo graphical covariate
df_abc_cb <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/1_data/private/AbC/df_047_hs_jha_phases1234.csv')
dim(df_abc_cb)
colnames(df_abc_cb)
unique(df_abc_cb$p1_province)

head(df_abc_cb$p2_run_date)
df_abc_cb$p2_run_date_cln <- as.Date(df_abc_cb$p2_run_date_cln)

df_abc_cb$p2_run_month <- format(df_abc_cb$p2_run_date_cln,"%m")
table(df_abc_cb$p2_run_month)

dim(df_abc_cb[df_abc_cb$p2_received_date != "",])

col_list <- c('rseed','p1_testdate','p1_province','p1_fsa','p1_age','p1_vizmin','p1_int_month','p2_np_igg','p2_np_igg_pred',
              'p2_rbd_igg_pred','p2_smt1_igg','p2_rbd_igg_pred','p2_smt1_igg_pred','p2_voting_result','p2_voting_quality',
              'p2_suggested_status','p2_note','p2_pprovince','p2_date202101','p2_vizmin','p2_fsa','p2_age','p2_optin',
              'p2_hidstartdate_cln','p2_hidenddate_cln','p2_received_date')

#check if p2 test the blood sample from p1
dim(df_abc_cb[df_abc_cb$p2_note %in% unique(df_abc_cb[df_abc_cb$p2_run_date_cln <= "2021-05-14",]$p2_note)[3:12],])
table(df_abc_cb[df_abc_cb$p2_run_date_cln <= "2021-05-14",]$p1_int_month)


cbs_data$province = province_fun(cbs_data$fsa)
dim(cbs_data[cbs_data$province == "AB" & cbs_data$sampledate < "2021-05-14" , ])
df_ab <- cbs_data[cbs_data$province == "AB" & cbs_data$sampledate < "2021-05-14" , ]

# Change density plot line colors by groups
ggplot(df_ab[df_ab$cur_result_n=='Roche Positive', ], aes(x=roche_n, color=cur_result_n)) +
  geom_density()
# Add mean lines
p<-ggplot(df_ab, aes(x=roche_n, color=cur_result_n)) +
  geom_density()+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=cur_result_n),
             linetype="dashed")
p

ggplot(df_ab[df_ab$cur_result_n=='Roche Negative', ], aes(x=roche_n, color=cur_result_n)) +
  geom_density()

# Retrieves Vancouver and Toronto
list_census_regions('CA21') %>% 
  filter(level == "CMA", name %in% c("Vancouver","Toronto"))

##### ABC vs CBS vs CLSA ###
df_abc_cb$p3_dbs_received_date <- ymd(df_abc_cb$p3_dbs_received_date)

dim(df_abc_cb[!is.na(df_abc_cb$p4_date_specimen_collected),])

df_abc_cb$p3_dbs_received_date <- ymd(df_abc_cb$p3_dbs_received_date)
sample(df_abc_cb$p2_received_date, 50)

dim(df_abc_cb[!is.na(df_abc_cb$p4_run_date),])

#ABC date
df_abc_cb$p2_received_date <- as.Date(df_abc_cb$p2_received_date)
df_abc_cb$p3_dbs_received_date <- as.Date(df_abc_cb$p3_dbs_received_date)
df_abc_cb$p4_dbs_received_date <- as.Date(df_abc_cb$p4_dbs_received_date)

canpaths$Month <- floor_date(canpaths$sampledate, unit = "month")
df_abc_cb$p2_received_month <- floor_date(df_abc_cb$p2_received_date, unit = "month")
df_abc_cb$p3_received_month <- floor_date(df_abc_cb$p3_dbs_received_date, unit = "month")
df_abc_cb$p4_received_month <- floor_date(df_abc_cb$p4_dbs_received_date, unit = "month")

df_abc_cb$p2_received_month <- format(as.Date(df_abc_cb$p2_received_date, format="%d/%m/%Y"),"%m")
table(df_abc_cb$p2_received_month)

df_abc_cb$p2_run_date_cln <- as.Date(df_abc_cb$p2_run_date_cln)
summary(df_abc_cb$p2_run_date_cln)

summary(df_abc_cb[df_abc_cb$p2_received_month == '05',]$p2_received_date)

table(df_abc_cb[df_abc_cb$p2_received_date > '2021-05-14',]$p2_np_igg_pred)

table(df_abc_cb[df_abc_cb$p2_received_month == '01',]$p2_np_igg_pred)


df_A_abc <- df_abc_cb[!is.na(df_abc_cb$p2_received_date),]
df_A_abc$Month <- floor_date(df_A_abc$p2_received_date, unit = "month")
#df_A_abc <- df_abc_cb[df_abc_cb$p2_received_month %in% c('01', '02', '03', '04'),]
df_A_abc <- df_A_abc[df_A_abc$Month < '2021-05-01',] # 7129

table(df_A_abc$p2_province)
#AB, BC, MB, NB, NL, NT, NS, NU, ON, PIE, QC, SK, YK

df_A_abc$Province = province_fun(df_A_abc$p2_fsa)
df_A_abc <- df_A_abc %>% 
  filter(Province != ("YT") & Province !=("NU/NT") & Province != ("QC")) #5648

df_A_abc$age = df_A_abc$p2_age
df_A_abc$age_group = cut(df_A_abc$age, 
                         breaks = c(18,40,55,Inf),
                         labels = c('18-39','40-54','55+'),
                         right = FALSE)

df_A_abc$Sex = 2-df_A_abc$p2_qe2
df_A_abc <- df_A_abc[df_A_abc$Sex != -1,] #5620

df_A_abc$Urban <- with(df_A_abc,ifelse(substr(p2_fsa,start = 2,stop = 2) != "0",1,0))

df_A_abc$Race = with(df_A_abc, ifelse(p1_vizmin==2 & p1_ethnicity_1 !=1, 1, 0))
                      
#df_A_abc$Month <- floor_date(df_A_abc$p2_received_date, unit = "month")
#df_A_abc$month <- plyr::mapvalues(as.character(df_A_abc$Month), as.character(sort(unique(df_A_abc$Month))), order(sort(unique(df_A_abc$Month))))

#df_A_abc$month <- as.integer(df_A_abc$month)

                      
dim(df_A_abc[df_A_abc$p1_vizmin==2 & df_A_abc$p1_ethnicity_1 !=1,]) #4748 white

dim(df_A_abc[df_A_abc$p1_vizmin==2 & df_A_abc$p1_ethnicity_1 ==1,]) #212 nonwhite?

dim(df_A_abc[df_A_abc$p1_vizmin==1 & df_A_abc$p1_ethnicity_1 !=1,]) #539 nonwhite

dim(df_A_abc[df_A_abc$p1_vizmin==1 & df_A_abc$p1_ethnicity_1 ==1,]) #121 nonwhite

df_A_abc$region = factor(case_when(as.character(substr(df_A_abc$p2_fsa,1,1)) %in% c("A", "B", "C", "E")~"Atlantic", 
                                   as.character(substr(df_A_abc$p2_fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"Ontario",
                                   as.character(substr(df_A_abc$p2_fsa,1,1)) %in% c("R", "S", "T")~"Prairies",
                                   as.character(substr(df_A_abc$p2_fsa,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("Atlantic","BC",  "Ontario", "Prairies"))

df_A_abc$regpro = factor(case_when(as.character(substr(df_A_abc$p2_fsa,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                                    as.character(substr(df_A_abc$p2_fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                                    as.character(substr(df_A_abc$p2_fsa,1,1)) %in% c("T")~"AB",
                                    as.character(substr(df_A_abc$p2_fsa,1,1)) %in% c("R")~"MB",
                                    as.character(substr(df_A_abc$p2_fsa,1,1)) %in% c("S")~"SK",
                                    as.character(substr(df_A_abc$p2_fsa,1,1)) %in% c("V")~"BC",
                                    TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK"))
df_A_abc$interp_roche_n <- with(df_A_abc,ifelse(p2_np_igg_pred== "Positive",1,0))

#Atlantic       BC  Ontario Prairies 
#375          1250     2823     1172 
#join the cduid from unique map table
df_A_abc$fsa <- df_A_abc$p2_fsa
df_A_abc <- merge(df_A_abc, fsa_to_cd, by = 'fsa', all.x = TRUE)
df_A_abc <- df_A_abc_cp

fsa_to_cd_dup <- fsa_to_cd[duplicated(fsa_to_cd$fsa) | duplicated(fsa_to_cd$fsa, fromLast = TRUE),]

fsa_to_cd_uniq <- fsa_to_cd[!(duplicated(fsa_to_cd$fsa) | duplicated(fsa_to_cd$fsa, fromLast = TRUE)),]

df_A_abc <- df_A_abc %>% distinct(rseed, .keep_all = TRUE)


df_A_abc$interp_roche_n <- with(df_A_abc,ifelse(p2_np_igg_pred== "Positive",1,0))
df_A_abc$Sex <- df_A_abc$sex
df_A_abc$Race <- df_A_abc$race
df_A_abc$Urban <- df_A_abc$urban


dat <- df_A_abc[df_A_abc$region == 'Ontario', ]
dim(dat)
trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod_A_abc_on <- brm(interp_roche_n ~ age + Sex + Race + Urban + month + (1 | cduid), 
                    data = dat, family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, 
                    control = list(adapt_delta = 0.9, max_treedepth = 10),  prior = PRIORS, cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod_A_abc_on)


# create df_A_cbs
#Atlantic       BC  Ontario Prairies 
# 6098        8440    24241    15713 
df_A_cbs <- data1[data1$sampledate >= '2021-01-01' & data1$sampledate < '2021-05-01',] #54492
pr_name <- 'BC'
pr_name <- c('AB', 'SK', 'MB')
#### Nova Scotia, Prince Edward Island, New Brunswick, Newfoundland
pr_name <- c('NS', 'PE', 'NB', 'NL')
#pr_name <- c('NS', 'NB', 'NL')
loo_list_wave <- list()

PRIORS <- c(set_prior("normal(-1,.3)", class = "b", coef= "age"),
            set_prior("normal(0,5)",  class = "b", coef= "Sex"),
            set_prior("normal(0,5)",   class = "b", coef= "Race"),
#            set_prior("normal(0,5)",   class = "b", coef= "QuintMat"),
#            set_prior("normal(0,5)",   class = "b", coef= "QuintSoc"),
            set_prior("normal(0,5)",   class = "b", coef= "Urban"),
            set_prior("normal(0,5)",   class = "b", coef= "month"),
            set_prior("cauchy(0,10)",  class = "Intercept", coef = "" ))

pr_name <- 'ON'
dat <- df_A_cbs[df_A_cbs$region == 'Ontario', ]
dim(dat)
trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod_A_cbs_on <- brm(interp_roche_n ~ age + Sex + Race + Urban + month + (1 | cduid), 
                   data = dat, family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, 
                   control = list(adapt_delta = 0.9, max_treedepth = 10),  prior = PRIORS, cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod_A_cbs_on)



###### CLSA ########
#SER_AGE_COV,SER_SEX_COV,SER_ETHN_WH_COV,"FSA_COVID", "CSD_COVID", "URBAN_RURAL_COVID"

df_D_clsa <- df_clsa

df_D_clsa$Province = province_fun(df_D_clsa$FSA_COVID)
df_D_clsa <- df_D_clsa %>% 
  filter(Province != ("YT") & Province !=("NU/NT") & Province != ("QC")) #11946

df_D_clsa$age = df_D_clsa$SER_AGE_COV
df_D_clsa$age_group = cut(df_D_clsa$age, 
                         breaks = c(18,40,55,Inf),
                         labels = c('18-39','40-54','55+'),
                         right = FALSE)

df_D_clsa <- df_D_clsa[df_D_clsa$SER_SEX_COV %in% c('M','F'),] #11946
df_D_clsa$Sex = with(df_D_clsa, ifelse(SER_SEX_COV=='M', 1, 0))


df_D_clsa$Urban <- with(df_D_clsa,ifelse(substr(FSA_COVID,start = 2,stop = 2) != "0",1,0))

#df_D_clsa <- df_D_clsa[df_D_clsa$SER_ETHN_WH_COV >= 0,] # 11387
df_D_clsa$Race = with(df_D_clsa, ifelse(SER_ETHN_WH_COV==1, 1, NA))

df_D_clsa$Race = case_when(df_D_clsa$SER_ETHN_WH_COV==1 ~ 1,
                           df_D_clsa$SER_ETHN_SA_COV==1 ~ 0,
                           df_D_clsa$SER_ETHN_ZH_COV==1 ~ 0,
                           df_D_clsa$SER_ETHN_BL_COV==1 ~ 0,
                           df_D_clsa$SER_ETHN_FP_COV==1 ~ 0,
                           df_D_clsa$SER_ETHN_LA_COV==1 ~ 0,
                           df_D_clsa$SER_ETHN_AR_COV==1 ~ 0,
                           df_D_clsa$SER_ETHN_SE_COV==1 ~ 0,
                           df_D_clsa$SER_ETHN_WA_COV==1 ~ 0,
                           df_D_clsa$SER_ETHN_KO_COV==1 ~ 0,
                           df_D_clsa$SER_ETHN_JA_COV==1 ~ 0,
                           df_D_clsa$SER_ETHN_OTSP_COV %in% c("guianese of east indian descent","mixed black and white ancestry","indian","india","south america - biracial",
                                                              "west indian of east indian descent","300 hundreds year ago my ancestors came from great britain, prior to that we all originated from africa.",
                                                              "west  indian") ~ 0,
                                TRUE~NA)

length(unique(df_D_clsa[is.na(df_D_clsa$Race),]$SER_ETHN_OTSP_COV))

c("guianese of east indian descent","mixed black and white ancestry","indian","india","south america - biracial",
  "west indian of east indian descent","300 hundreds year ago my ancestors came from great britain, prior to that we all originated from africa.",
  "west  indian")

df_D_clsa$Month <- floor_date(df_D_clsa$date, unit = "month")
df_D_clsa$month <- plyr::mapvalues(as.character(df_D_clsa$Month), as.character(sort(unique(df_D_clsa$Month))), order(sort(unique(df_D_clsa$Month))))

df_D_clsa$month <- as.integer(df_D_clsa$month)
#df_D_clsa$Urban <- as.integer(df_D_clsa$urban)

df_D_clsa$region = factor(case_when(as.character(substr(df_D_clsa$FSA_COVID,1,1)) %in% c("A", "B", "C", "E")~"Atlantic", 
                                   as.character(substr(df_D_clsa$FSA_COVID,1,1)) %in% c("K", "L", "M", "N", "P")~"Ontario",
                                   as.character(substr(df_D_clsa$FSA_COVID,1,1)) %in% c("R", "S", "T")~"Prairies",
                                   as.character(substr(df_D_clsa$FSA_COVID,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("Atlantic","BC",  "Ontario", "Prairies"))

df_D_clsa$regpro = factor(case_when(as.character(substr(df_D_clsa$FSA_COVID,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                                    as.character(substr(df_D_clsa$FSA_COVID,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                                    as.character(substr(df_D_clsa$FSA_COVID,1,1)) %in% c("T")~"AB",
                                    as.character(substr(df_D_clsa$FSA_COVID,1,1)) %in% c("R")~"MB",
                                    as.character(substr(df_D_clsa$FSA_COVID,1,1)) %in% c("S")~"SK",
                                    as.character(substr(df_D_clsa$FSA_COVID,1,1)) %in% c("V")~"BC",
                                    TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK"))

df_D_clsa <- df_D_clsa[df_D_clsa$SER_NUCLEOCAPSID_COV >= 0,] # 10417
df_D_clsa$interp_roche_n <- df_D_clsa$SER_NUCLEOCAPSID_COV

# Setting D (CLSA)
df_D_clsa <- df_D_clsa[df_D_clsa$date >= '2021-03-01' & df_D_clsa$date < '2021-09-01',] # 7431


data1$regpro = factor(case_when(as.character(substr(data1$fsa,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                                as.character(substr(data1$fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                                as.character(substr(data1$fsa,1,1)) %in% c("T")~"AB",
                                as.character(substr(data1$fsa,1,1)) %in% c("R")~"MB",
                                as.character(substr(data1$fsa,1,1)) %in% c("S")~"SK",
                                as.character(substr(data1$fsa,1,1)) %in% c("V")~"BC",
                                TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK"))

df_cbs <- data1
df_cbs$age_group = cut(df_cbs$age, 
                          breaks = c(18,40,55,Inf),
                          labels = c('18-39','40-54','55+'),
                          right = FALSE)

# Setting D (CBS)
df_D_cbs <- df_cbs[df_cbs$sampledate >= '2021-03-01' & df_cbs$sampledate < '2021-09-01',] # 69067

# Setting A (CBS)
df_A_cbs <- df_cbs[df_cbs$sampledate >= '2021-01-01' & df_cbs$sampledate < '2021-05-01',] # 54492

# Setting A1 (CBS)

# Setting B (CBS)
df_B_cbs <- df_cbs[df_cbs$sampledate >= '2021-07-01' & df_cbs$sampledate < '2021-09-01',] # 14271
# Setting C (CBS)
df_C_cbs <- df_cbs[df_cbs$sampledate >= '2021-12-01' & df_cbs$sampledate < '2022-07-01',] # 165419


#Atlantic       BC  Ontario Prairies 
#375          1250     2823     1172 
#join the cduid from unique map table
df_D_clsa$fsa <- df_D_clsa$FSA_COVID
df_D_clsa <- merge(df_D_clsa, fsa_to_cd, by = 'fsa', all.x = TRUE)
df_D_clsa <- df_D_clsa_cp

fsa_to_cd_dup <- fsa_to_cd[duplicated(fsa_to_cd$fsa) | duplicated(fsa_to_cd$fsa, fromLast = TRUE),]

fsa_to_cd_uniq <- fsa_to_cd[!(duplicated(fsa_to_cd$fsa) | duplicated(fsa_to_cd$fsa, fromLast = TRUE)),]

df_D_clsa <- df_D_clsa %>% distinct(rseed, .keep_all = TRUE)


df_D_clsa$interp_roche_n <- with(df_D_clsa,ifelse(p2_np_igg_pred== "Positive",1,0))
df_D_clsa$Sex <- df_D_clsa$sex
df_D_clsa$Race <- df_D_clsa$race
df_D_clsa$Urban <- df_D_clsa$urban


dat <- df_D_clsa[df_D_clsa$region == 'Ontario', ]
dim(dat)
trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod_A_abc_on <- brm(interp_roche_n ~ age + Sex + Race + Urban + month + (1 | cduid), 
                    data = dat, family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, 
                    control = list(adapt_delta = 0.9, max_treedepth = 10),  prior = PRIORS, cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod_A_abc_on)


# Recode CCES
clean_clsa <- function(df, remove_nas = TRUE){
  
  ## Abortion -- dichotomous (0 - Oppose / 1 - Support)
  df$abortion <- abs(df$CC18_321d-2)
  
  ## State -- factor
  df$state <- recode_fips(df$inputstate)
  
  ## Gender -- dichotomous (coded as -0.5 Female, +0.5 Male)
  df$male <- abs(df$gender-2)-0.5
  
  ## ethnicity -- factor
  df$eth <- factor(df$race,
                   levels = 1:8,
                   labels = c("White", "Black", "Hispanic", "Asian", "Native American", 
                              "Mixed", "Other", "Middle Eastern"))
  df$eth <- fct_collapse(df$eth, "Other" = c("Asian", "Other", "Middle Eastern", 
                                             "Mixed", "Native American"))
  
  ## Age -- cut into factor
  df$age <- 2018 - df$birthyr
  df$age <- cut(as.integer(df$age), breaks = c(0, 29, 39, 49, 59, 69, 120), 
                labels = c("18-29","30-39","40-49","50-59","60-69","70+"),
                ordered_result = TRUE)
  
  ## Education -- factor
  df$educ <- factor(as.integer(df$educ), 
                    levels = 1:6, 
                    labels = c("No HS", "HS", "Some college", "Associates", 
                               "4-Year College", "Post-grad"), ordered = TRUE)
  df$educ <- fct_collapse(df$educ, "Some college" = c("Some college", "Associates"))  
  
  # Filter out unnecessary columns and remove NAs
  df <- df %>% select(abortion, state, eth, male, age, educ) 
  if (remove_nas){
    df <- df %>% drop_na()
  }
  
  return(df)
}

#################### Load post-stratification Table ##########################
census_race <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/1_data/private/CENSUS/census_w_final_counts_race.csv')
census_urban <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/1_data/private/CENSUS/census_w_final_counts_urban.csv')

#filter for blood donor population
census_race <- census_race[census_race$age_group != "< 18 years", ]
census_urban <- census_urban[census_urban$age_group != "< 18 years", ]

census_race <- census_race %>% rename("regpro" = "province")

census_race <- census_race %>% rename("QuintMat" = "QUINTMAT")
census_race <- census_race %>% rename("QuintSoc" = "QUINTSOC")

census_race[census_race$regpro == 'Atlantic', ]$regpro <- 'ATL'

census_race$region = factor(case_when(census_race$regpro == 'ATL' ~ "Atlantic", 
                                      census_race$regpro == 'ON' ~"Ontario",
                                      census_race$regpro %in% c("AB", "MB", "SK")~"Prairies",
                                      census_race$regpro == 'BC' ~"BC",
                                    TRUE~NA), levels = c("Atlantic","BC",  "Ontario", "Prairies"))

# Fit in brm
start_time <- Sys.time()
fit1 <- brm(interp_roche_n ~ (1 | regpro) + (1 | Race) + (1 | age_group) + QuintMat + QuintSoc +
            Sex + factor(region),
            family = bernoulli('logit'),
            data = df_A_cbs,
            #                    prior(normal(0, 0+), class = “whatever”),
            #                    prior(decov(regularization = 0.50), class = “cov”),
            prior = c(prior(normal(0, 5), class = Intercept),
                      prior(normal(0, 1), class = b),
                      prior(exponential(0.5), class = sd)),
            control = list(adapt_delta = 0.99, max_treedepth  = 10),
            cores = 4,
            seed = 1010)
end_time <- Sys.time()
end_time - start_time


# Fit in brm
start_time <- Sys.time()
fit1 <- brm(interp_roche_n ~ (1 | regpro) + Race + age_group +
              Sex,
            family = bernoulli('logit'),
            data = df_A_abc,
            #                    prior(normal(0, 0+), class = “whatever”),
            #                    prior(decov(regularization = 0.50), class = “cov”),
            prior = c(prior(normal(0, 5), class = Intercept),
                      prior(normal(0, 1), class = b),
                      prior(exponential(0.5), class = sd)),
            control = list(adapt_delta = 0.99, max_treedepth  = 10),
            cores = 4,
            seed = 1010)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
fit <- brm(interp_roche_n ~ (1 | regpro) + Race + age_group +
              Sex,
            family = bernoulli('logit'),
            data = df_A_abc,
            #                    prior(normal(0, 0+), class = “whatever”),
            #                    prior(decov(regularization = 0.50), class = “cov”),
            prior = c(prior(normal(0, 5), class = Intercept),
                      prior(normal(0, 1), class = b),
                      prior(exponential(0.5), class = sd)),
            control = list(adapt_delta = 0.99, max_treedepth  = 10),
            cores = 4,
            seed = 1010)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
fit2 <- brm(interp_roche_n ~ (1 | regpro) + Race + age_group + Sex +
            QuintMat + QuintSoc,
           family = bernoulli('logit'),
           data = df_A_cbs,
           #                    prior(normal(0, 0+), class = “whatever”),
           #                    prior(decov(regularization = 0.50), class = “cov”),
           prior = c(prior(normal(0, 5), class = Intercept),
                     prior(normal(0, 1), class = b),
                     prior(exponential(0.5), class = sd)),
           control = list(adapt_delta = 0.99, max_treedepth  = 10),
           cores = 4,
           seed = 1010)
end_time <- Sys.time()
end_time - start_time

census_race$age_group = case_when(census_race$age_group == '18-39 years' ~ "18-39", 
                                      census_race$age_group == '40-54 years' ~"40-54",
                                      census_race$age_group == '55+ years' ~"55+",
                                      TRUE~NA)


poststrat_D <-
  census_race %>%
  group_by(regpro,age_group,Sex,Race) %>%
  dplyr::summarize(n = sum(Final_Count))

poststrat_D <-
  census_race %>%
  group_by(regpro,age_group,Sex,Race,QuintMat,QuintSoc) %>%
  dplyr::summarize(n = sum(Final_Count)) %>%
  drop_na()

# poststrat_df dataframe.
epred_mat <- posterior_epred(fit1, newdata = poststrat_D, draws = 1000)
epred_mat <- posterior_epred(fit2, newdata = poststrat_D, draws = 1000)

mrp_estimates_vector <- epred_mat %*% poststrat_D$n / sum(poststrat_D$n)
mrp_estimate <- c(mean = mean(mrp_estimates_vector), sd = sd(mrp_estimates_vector))
cat('MRP estimate mean, sd: ', round(mrp_estimate, 3))


sort(sapply(ls(),function(x){object.size(get(x))})) 

ls(pattern="\\mod")

save("mod_bc_glm","mod_bc_p1","mod_bc_ran","mod_on_glm","mod1_atl","mod1_atl_glm","mod1_bc","mod1_bc_glm","mod1_on",
     "mod1_on_glm","mod1_prai","mod1_prai_glm","mod2_atl","mod2_atl_glm","mod2_atl_sub_glm","mod2_bc","mod2_bc_glm",
     "mod2_on","mod2_on_dens_glm", "mod2_on_glm","mod2_on_ran","mod2_prai","mod2_prai_glm","mod3", "mod3_atl", 
     "mod3_atl_glm","mod3_bc","mod3_bc_glm","mod3_on","mod3_on_glm","mod3_prai","mod3_prai_glm","modelposterior",
     "models","waic_mod","waic_mod_wave", "waic_mod_wave_1" , file = "0921models.RData")

rm(list=c("mod_bc_glm","mod_bc_p1","mod_bc_ran","mod_on_glm","mod1_atl","mod1_atl_glm","mod1_bc","mod1_bc_glm","mod1_on",
     "mod1_on_glm","mod1_prai","mod1_prai_glm","mod2_atl","mod2_atl_glm","mod2_atl_sub_glm","mod2_bc","mod2_bc_glm",
     "mod2_on_dens_glm", "mod2_on_glm","mod2_on_ran","mod2_prai","mod2_prai_glm","mod3", "mod3_atl", 
     "mod3_atl_glm","mod3_bc","mod3_bc_glm","mod3_on","mod3_on_glm","mod3_prai","mod3_prai_glm","modelposterior",
     "models","waic_mod","waic_mod_wave", "waic_mod_wave_1" ))

ls(pattern="fig")
rm(data5_delta)

cat(paste(ls(pattern="fig"), collapse = ", "))

paste(ls(pattern="fig")) %>% 
  str_c('"', ., '"') %>% 
  str_c(collapse = ",") %>% 
  cat()

rm(list=c("fig_ab","fig_mb","fig_nb","fig_nl","fig_ns","fig_num","fig_on","fig_pe","fig_sk","fig1","fig1_A","fig1_AB","fig1_B","fig2","fig2_A","fig2_A1","fig2_ABC","fig2_alpha","fig2_atl_A","fig2_atl_B","fig2_atl_C","fig2_B","fig2_B1","fig2_bc_A","fig2_bc_B","fig2_bc_C","fig2_C","fig2_C1","fig2_D","fig2_delta","fig2_omicron","fig2_on_A","fig2_on_B","fig2_on_C","fig2_prai_A","fig2_prai_B","fig2_prai_C","fig22","fig24","fig3","fig3_ind","fig3_ind_pv","fig3_nei","fig3_nei_pv","fig3_nei_v","fig4","fig5"))
ls(pattern="month")
rm(list=c("month_age_1","month_age_2","month_list","month_list_cb","month_mat_1","month_mat_2","month_race_1","month_race_2",
     "month_sex_1","month_sex_2","month_soc_1","month_soc_2","month_urban_1","month_urban_2"))

ls()[sapply(ls(), function(i) class(get(i))) == "data.frame"]

library(rstanarm)
start_time <- Sys.time()
fit1_stan <- stan_glmer(interp_roche_n ~ (1 | regpro) + Race + age_group + Sex,
            family = binomial,
            data = df_A_abc,
            #                    prior(normal(0, 0+), class = “whatever”),
            #                    prior(decov(regularization = 0.50), class = “cov”),
            # prior = c(prior(normal(0, 5), class = Intercept),
            #           prior(normal(0, 1), class = b),
            #           prior(exponential(0.5), class = sd)),
            # control = list(adapt_delta = 0.99, max_treedepth  = 10),
            cores = 4,
            seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit1_stan)

start_time <- Sys.time()
fit2_stan <- stan_glmer(interp_roche_n ~ (1 | regpro) + Race + age_group + Sex +
                          QuintMat + QuintSoc,
                        family = binomial,
                        data = df_A_cbs,
                        #                    prior(normal(0, 0+), class = “whatever”),
                        #                    prior(decov(regularization = 0.50), class = “cov”),
                        # prior = c(prior(normal(0, 5), class = Intercept),
                        #           prior(normal(0, 1), class = b),
                        #           prior(exponential(0.5), class = sd)),
                        # control = list(adapt_delta = 0.99, max_treedepth  = 10),
                        cores = 4,
                        seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit2_stan)

poststrat_D <- poststrat_cbs
epred_mat <- posterior_epred(fit2_stan, newdata = poststrat_D, draws = 1000)

mrp_estimates_vector <- epred_mat %*% poststrat_D$n / sum(poststrat_D$n)
mrp_estimate <- c(mean = mean(mrp_estimates_vector), sd = sd(mrp_estimates_vector))
cat('MRP estimate mean, sd: ', round(mrp_estimate, 3))

install.packages("survey")
library(survey)

# Create a data frame with your survey data
survey_data <- read.csv("survey_data.csv")

# Create a survey design object
svy_design <- svydesign(
  ids = ~1,  # Single-stage design, assuming no clustering
  strata = ~ Race + age_group + Sex,  # Stratification variable (if any)
  data = poststrat_abc[poststrat_abc$regpro=='ATL',]
#  weights = ~weight_variable  # Sample weights
)

poststrat_abc[poststrat_abc$regpro=='ATL',] %>%
  group_by(Sex) %>%
  dplyr::summarize(n = sum(n))

population_totals <- c(
  age_18_39 = 575020,
  age_40_54 = 496475,
  age_55_plus = 802315,
  Race_white = 166055,
  Race_nonwhite = 1707755,
  Sex_female = 969955,
  Sex_male = 903855
)

svy_design_raked <- rake(
  d = svy_design,
  sample.margins = list(~age_group, ~Race, ~Sex),  # Variables to match
  population.margins = population_totals,  # Population totals
  control = list(min.pps = TRUE)  # Specify control options if needed
)

result <- svytotal(~interp_roche_n, svy_design_raked)

library(survey)
data <- df_A_abc[df_A_abc$regpro=='ATL',]
#data.svy.unweighted <- svydesign(ids=~1, data=data)

# Create your composite stratification variable
data$composite_strata <- paste(data$age_group, data$Race, data$Sex, sep = "_")

# Create a survey design object with the composite stratification variable
data.svy.unweighted <- svydesign(
  ids = ~1,  # Cluster variable
  strata = ~composite_strata,  # Composite stratification variable
  #  weights = ~sampling_weight,  # Sampling weight variable
  data = data  # Your survey data
)

poststrat_abc$composite_strata <- paste(poststrat_abc$age_group, poststrat_abc$Race, poststrat_abc$Sex, sep = "_")
View(poststrat_abc[poststrat_abc$regpro=='ATL',])

poststrat.dist <- data.frame(composite_strata = poststrat_abc[poststrat_abc$regpro=='ATL',]$composite_strata,
                          Freq = poststrat_abc[poststrat_abc$regpro=='ATL',]$n)

data.svy.rake <- rake(design = data.svy.unweighted,
                      sample.margins = list(~composite_strata),
                      population.margins = list(poststrat.dist))

as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,1]
as.data.frame(svymean(~interp_roche_n, data.svy.rake))[,2]

#### F
data1$Urban <- with(data1,ifelse(substr(fsa,start = 2,stop = 2) != "0",1,0))
df_F_cbs$Urban <- with(df_F_cbs,ifelse(substr(fsa,start = 2,stop = 2) != "0",1,0))
df_A_cbs$Urban <- with(df_A_cbs,ifelse(substr(fsa,start = 2,stop = 2) != "0",1,0))

df_F_cbs <- data1[data1$sampledate >= '2020-12-01' & data1$sampledate < '2021-04-30',] #55156
df_F_cbs$age_group = cut(df_F_cbs$age, 
                         breaks = c(18,40,55,Inf),
                         labels = c('18-39','40-54','55+'),
                         right = FALSE)
fit_F_cbs2<-fit_F_cbs

start_time <- Sys.time()
fit_F_cbs <- stan_glmer(interp_roche_n ~ (1 | regpro) + Urban + age_group + Sex +
                          QuintMat + QuintSoc,
                        family = binomial,
                        data = df_F_cbs,
                        cores = 4,
                        seed = 1010)
end_time <- Sys.time()
end_time - start_time
print(fit_F_cbs)

### AbC B
df_B_abc <- df_abc_cb[df_abc_cb$p3_dbs_received_date >= '2021-07-14' & df_abc_cb$p3_dbs_received_date <= '2021-08-31',]
df_B_abc$Province = province_fun(df_B_abc$p3_fsa)
df_B_abc <- df_B_abc %>% 
  filter(Province != ("YT") & Province !=("NU/NT") & Province != ("QC")) #3445

df_B_abc$age = df_B_abc$p3_age
df_B_abc$age_group = cut(df_B_abc$age, 
                         breaks = c(18,40,55,Inf),
                         labels = c('18-39','40-54','55+'),
                         right = FALSE)

df_B_abc$Sex = 2-df_B_abc$p3_qe2
df_B_abc <- df_B_abc[df_B_abc$Sex != -1,] #3424

df_B_abc$Urban <- with(df_B_abc,ifelse(substr(p3_fsa,start = 2,stop = 2) != "0",1,0))

df_B_abc$Race = with(df_B_abc, ifelse(p3_vizmin==2 & p3_ethnicity_1 !=1, 1, 0))

#df_A_abc$Month <- floor_date(df_A_abc$p2_received_date, unit = "month")
#df_A_abc$month <- plyr::mapvalues(as.character(df_A_abc$Month), as.character(sort(unique(df_A_abc$Month))), order(sort(unique(df_A_abc$Month))))

#df_A_abc$month <- as.integer(df_A_abc$month)


dim(df_B_abc[df_B_abc$p1_vizmin==2 & df_B_abc$p1_ethnicity_1 !=1,]) #4748 white

dim(df_B_abc[df_B_abc$p1_vizmin==2 & df_B_abc$p1_ethnicity_1 ==1,]) #212 nonwhite?

dim(df_B_abc[df_B_abc$p1_vizmin==1 & df_B_abc$p1_ethnicity_1 !=1,]) #539 nonwhite

dim(df_B_abc[df_B_abc$p1_vizmin==1 & df_B_abc$p1_ethnicity_1 ==1,]) #121 nonwhite

df_B_abc$region = factor(case_when(as.character(substr(df_B_abc$p3_fsa,1,1)) %in% c("A", "B", "C", "E")~"Atlantic", 
                                   as.character(substr(df_B_abc$p3_fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"Ontario",
                                   as.character(substr(df_B_abc$p3_fsa,1,1)) %in% c("R", "S", "T")~"Prairies",
                                   as.character(substr(df_B_abc$p3_fsa,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("Atlantic","BC",  "Ontario", "Prairies"))

df_B_abc$regpro = factor(case_when(as.character(substr(df_B_abc$p3_fsa,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                                   as.character(substr(df_B_abc$p3_fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                                   as.character(substr(df_B_abc$p3_fsa,1,1)) %in% c("T")~"AB",
                                   as.character(substr(df_B_abc$p3_fsa,1,1)) %in% c("R")~"MB",
                                   as.character(substr(df_B_abc$p3_fsa,1,1)) %in% c("S")~"SK",
                                   as.character(substr(df_B_abc$p3_fsa,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK"))
df_B_abc$interp_roche_n <- with(df_B_abc,ifelse(p3_np_igg_pred== "Positive",1,0))

### AbC C
df_C_abc <- df_abc_cb[df_abc_cb$p4_dbs_received_date >= '2022-01-24' & df_abc_cb$p4_dbs_received_date <= '2022-04-12',]
df_C_abc$Province = province_fun(df_C_abc$p4a_fsa)
df_C_abc <- df_C_abc %>% 
  filter(Province != ("YT") & Province !=("NU/NT") & Province != ("QC")) #4178

df_C_abc$age = df_C_abc$p4a_age
df_C_abc$age_group = cut(df_C_abc$age, 
                         breaks = c(18,40,55,Inf),
                         labels = c('18-39','40-54','55+'),
                         right = FALSE)

df_C_abc$Sex = 2-df_C_abc$p4a_qe2
df_C_abc <- df_C_abc[df_C_abc$Sex != -1,] #4151

df_C_abc$Urban <- with(df_C_abc,ifelse(substr(p4a_fsa,start = 2,stop = 2) != "0",1,0))

df_C_abc$Race = with(df_C_abc, ifelse(p4a_vizmin==2 & p4a_ethnicity_1 !=1, 1, 0))

#df_A_abc$Month <- floor_date(df_A_abc$p2_received_date, unit = "month")
#df_A_abc$month <- plyr::mapvalues(as.character(df_A_abc$Month), as.character(sort(unique(df_A_abc$Month))), order(sort(unique(df_A_abc$Month))))

#df_A_abc$month <- as.integer(df_A_abc$month)


dim(df_C_abc[df_C_abc$p1_vizmin==2 & df_C_abc$p1_ethnicity_1 !=1,]) #4748 white

dim(df_C_abc[df_C_abc$p1_vizmin==2 & df_C_abc$p1_ethnicity_1 ==1,]) #212 nonwhite?

dim(df_C_abc[df_C_abc$p1_vizmin==1 & df_C_abc$p1_ethnicity_1 !=1,]) #539 nonwhite

dim(df_C_abc[df_C_abc$p1_vizmin==1 & df_C_abc$p1_ethnicity_1 ==1,]) #121 nonwhite

df_C_abc$region = factor(case_when(as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("A", "B", "C", "E")~"Atlantic", 
                                   as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"Ontario",
                                   as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("R", "S", "T")~"Prairies",
                                   as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("Atlantic","BC",  "Ontario", "Prairies"))

df_C_abc$regpro = factor(case_when(as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                                   as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                                   as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("T")~"AB",
                                   as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("R")~"MB",
                                   as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("S")~"SK",
                                   as.character(substr(df_C_abc$p4a_fsa,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK"))
df_C_abc$interp_roche_n <- with(df_C_abc,ifelse(p4_np_igg_pred== "Positive",1,0))


### CBS B ###
data1$regpro = factor(case_when(as.character(substr(data1$fsa,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                                   as.character(substr(data1$fsa,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                                   as.character(substr(data1$fsa,1,1)) %in% c("T")~"AB",
                                   as.character(substr(data1$fsa,1,1)) %in% c("R")~"MB",
                                   as.character(substr(data1$fsa,1,1)) %in% c("S")~"SK",
                                   as.character(substr(data1$fsa,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK"))
df_B_cbs <- data1[data1$sampledate >= '2021-07-14' & data1$sampledate <= '2021-08-31',] #55156
df_B_cbs$age_group = cut(df_B_cbs$age, 
                         breaks = c(18,40,55,Inf),
                         labels = c('18-39','40-54','55+'),
                         right = FALSE)

start_time <- Sys.time()
fit_B_cbs <- stan_glmer(interp_roche_n ~ (1 | regpro) + Race + age_group + Sex +
                          QuintMat + QuintSoc,
                        family = binomial,
                        data = df_B_cbs,
                        cores = 4,
                        seed = 1010, adapt_delta = 0.99)
end_time <- Sys.time()
end_time - start_time
print(fit_B_cbs)

### CBS C ###
df_C_cbs <- data1[data1$sampledate >= '2022-01-24' & data1$sampledate <= '2022-04-12',] #

df_C_cbs <- merge(df_C_cbs, df_C_vac, by='regpro', all.x=TRUE)

df_C_cbs$age_group = cut(df_C_cbs$age, 
                         breaks = c(18,40,55,Inf),
                         labels = c('18-39','40-54','55+'),
                         right = FALSE)

start_time <- Sys.time()
fit2_C_cbs <- stan_glmer(interp_roche_n ~ (1 | regpro) + Race + age_group + Sex +
                          QuintMat + QuintSoc +vac,
                        family = binomial,
                        data = df_C_cbs,
                        cores = 4,
                        seed = 1010, adapt_delta = 0.99)
end_time <- Sys.time()
end_time - start_time
print(fit2_C_cbs)


df_C_vac <- data.frame(
  regpro = c("BC","AB","MB","SK","ON","ATL"),
  vac = c(84.9,78.3,81.5,79.9,83,89.275)
)


## AbC B
start_time <- Sys.time()
fit_B_abc <- stan_glmer(interp_roche_n ~ (1 | regpro) + Race + age_group + Sex,
                        family = binomial,
                        data = df_B_abc,
                        cores = 4,
                        seed = 1010, adapt_delta = 0.99)
end_time <- Sys.time()
end_time - start_time
print(fit_B_abc)

## AbC C
df_C_abc <- merge(df_C_abc, df_C_vac, by='regpro', all.x=TRUE)

start_time <- Sys.time()
fit2_C_abc <- stan_glmer(interp_roche_n ~ (1 | regpro) + Race + age_group + Sex + vac,
                        family = binomial,
                        data = df_C_abc,
                        cores = 4,
                        seed = 1010, adapt_delta = 0.99)
end_time <- Sys.time()
end_time - start_time
print(fit2_C_abc)



### APL C Alberta
df_G_abc <- df_C_abc %>% filter(Province == ("AB")) 

df_G_cbs <- df_C_cbs %>% filter(Province == ("AB")) 

df_G_apl <- RFD4682_e[RFD4682_e$COLLECTION_DATE >= '2022-01-24 00:00:00.0000' & RFD4682_e$COLLECTION_DATE <= '2022-04-12 00:00:00.0000',]
df_G_apl$Province = province_fun(df_G_apl$PAT_FSA)
#df_G_apl <- df_G_apl %>% 
#  filter(Province != ("YT") & Province !=("NU/NT") & Province != ("QC")) #4178

df_G_apl$age = df_G_apl$AGE_AT_COLLECTION
df_G_apl$age_group = cut(df_G_apl$age, 
                         breaks = c(18,40,55,Inf),
                         labels = c('18-39','40-54','55+'),
                         right = FALSE)

df_G_apl <- df_G_apl[!is.na(df_G_apl$GENDER),] #8783
df_G_apl$Sex = with(df_G_apl,ifelse(GENDER == "Male",1,0))

df_G_apl <- df_G_apl[!is.na(df_G_apl$PAT_FSA),]
df_G_apl$Urban <- with(df_G_apl,ifelse(substr(PAT_FSA,start = 2,stop = 2) != "0",1,0))

df_G_apl$region = factor(case_when(as.character(substr(df_G_apl$PAT_FSA,1,1)) %in% c("A", "B", "C", "E")~"Atlantic", 
                                   as.character(substr(df_G_apl$PAT_FSA,1,1)) %in% c("K", "L", "M", "N", "P")~"Ontario",
                                   as.character(substr(df_G_apl$PAT_FSA,1,1)) %in% c("R", "S", "T")~"Prairies",
                                   as.character(substr(df_G_apl$PAT_FSA,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("Atlantic","BC",  "Ontario", "Prairies"))

df_G_apl$regpro = factor(case_when(as.character(substr(df_G_apl$PAT_FSA,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                                   as.character(substr(df_G_apl$PAT_FSA,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                                   as.character(substr(df_G_apl$PAT_FSA,1,1)) %in% c("T")~"AB",
                                   as.character(substr(df_G_apl$PAT_FSA,1,1)) %in% c("R")~"MB",
                                   as.character(substr(df_G_apl$PAT_FSA,1,1)) %in% c("S")~"SK",
                                   as.character(substr(df_G_apl$PAT_FSA,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK"))

df_G_apl <- df_G_apl[!is.na(df_G_apl$'N-IgG_INTERP'),]
df_G_apl$interp_roche_n <- with(df_G_apl,ifelse(df_G_apl$'N-IgG_INTERP' == "Positive",1,0))

### APL G
start_time <- Sys.time()
fit_G_apl <- stan_glm(interp_roche_n ~ Urban + age_group + Sex,
                        family = binomial,
                        data = df_G_apl,
                        cores = 4,
                        seed = 1010, adapt_delta = 0.8)
end_time <- Sys.time()
end_time - start_time
print(fit_G_apl)
waic(fit_G_apl)

### AbC G
start_time <- Sys.time()
fit_G_abc <- stan_glm(interp_roche_n ~ Urban + age_group + Sex,
                        family = binomial,
                        data = df_G_abc,
                        cores = 4,
                        seed = 1010, adapt_delta = 0.8)
end_time <- Sys.time()
end_time - start_time
print(fit_G_abc)
waic(fit_G_abc)

### CBS G
start_time <- Sys.time()
fit_G_cbs <- stan_glm(interp_roche_n ~ Urban + age_group + Sex,
                        family = binomial,
                        data = df_G_cbs,
                        cores = 4,
                        seed = 1010, adapt_delta = 0.8)
end_time <- Sys.time()
end_time - start_time
print(fit_G_cbs)

### Explore the Vaccination effect on the seropositivity estimation 
### APL G
df_G_apl$covidvv = with(df_G_apl, ifelse(VACCINATION_STATUS %in% c('No ULI', 'Not Immunized'), 0, 1))

start_time <- Sys.time()
fit_G_apl_vac <- stan_glm(interp_roche_n ~ Urban + age_group + Sex + covidvv,
                      family = binomial,
                      data = df_G_apl,
                      cores = 4,
                      seed = 1010, adapt_delta = 0.8)
end_time <- Sys.time()
end_time - start_time
print(fit_G_apl_vac)
#waic(fit_G_apl)

### AbC G
df_G_abc$covidvv = with(df_G_abc, ifelse(is.na(p4a_qd5), 0, 1))

start_time <- Sys.time()
fit_G_abc_vac <- stan_glm(interp_roche_n ~ Urban + age_group + Sex + covidvv,
                      family = binomial,
                      data = df_G_abc,
                      cores = 4,
                      seed = 1010, adapt_delta = 0.8)
end_time <- Sys.time()
end_time - start_time
print(fit_G_abc_vac)

### CBS G
start_time <- Sys.time()
fit_G_cbs_vac <- stan_glm(interp_roche_n ~ Urban + age_group + Sex + covidvv,
                      family = binomial,
                      data = df_G_cbs,
                      cores = 4,
                      seed = 1010, adapt_delta = 0.8)
end_time <- Sys.time()
end_time - start_time
print(fit_G_cbs_vac)



## heatmap

## Heatmap XXXX: Generate counts by age-race-material deprivation quintile & plot
library(viridis)
dt_all_arqm<-poststrat_cbs[,Count_by_arqm := list(sum(n)),by = c("age_group","regpro","Sex")]
col_labs<-c("urban","rural")
names(col_labs)<-c(1,0)
ggplot(poststrat_cbs,aes(x = regpro,y = age_group,fill = n))+
  geom_tile()+
  facet_grid(cols = vars(regpro),labeller = labeller(regpro = col_labs))+
  scale_fill_gradientn(colors = turbo(100),
                       n.breaks = 10,
                       labels = comma)+
  labs(fill = "n")

## Create dataset for setting H
drops <- c("Month","composite_strata","composite_strata_1","composite_strata_2")
df_H_abc <- df_A_abc[ , !(names(df_A_abc) %in% drops)]

keep <- c("SER_ADM_COV","Province","age","age_group","Sex","Urban","Race","region","regpro","interp_roche_n")
df_H_abc <- df_A_abc[ , !(names(df_A_abc) %in% keep)]

df_H_abc <- df_A_abc[df_A_abc$p2_received_date >= "2021-03-01" & df_A_abc$p2_received_date <= "2021-04-30" & df_A_abc$p2_age >= 50, !(names(df_A_abc) %in% drops)]
df_H_abc <- rbind(df_H_abc, df_B_abc[df_B_abc$p3_age >= 50,])


keep <- c("sampledate","Province","age","age_group","Sex","Urban","Race","QuintMat","QuintSoc","region","regpro","interp_roche_n")
df_H_cbs <- df_A_cbs[df_A_cbs$sampledate >= "2021-03-01" & df_A_cbs$sampledate <= "2021-04-30" & df_A_cbs$age >= 50 , (names(df_A_cbs) %in% keep)]
df_H_cbs <- rbind(df_H_cbs, df_B_cbs[df_B_cbs$age >= 50, (names(df_B_cbs) %in% keep)])
dim(df_H_cbs)

df_H_clsa <- df_D_clsa[(df_D_clsa$date >= "2021-03-01" & df_D_clsa$date <= "2021-04-30")|
                         (df_D_clsa$date >= "2021-07-01" & df_D_clsa$date <= "2021-08-31"), ]



## AbC H 
start_time <- Sys.time()
fit_H_abc <- stan_glmer(interp_roche_n ~ (1 | regpro) + Urban + age_group + Sex,
                        family = binomial,
                        data = df_H_abc,
                        cores = 4,
                        seed = 1010, adapt_delta = 0.99)
end_time <- Sys.time()
end_time - start_time
print(fit_H_abc)

## CLSA H 
start_time <- Sys.time()
fit_H_clsa <- stan_glmer(interp_roche_n ~ (1 | regpro) + Urban + age_group + Sex,
                        family = binomial,
                        data = df_H_clsa,
                        cores = 4,
                        seed = 1010, adapt_delta = 0.95)
end_time <- Sys.time()
end_time - start_time
print(fit_H_clsa)

## CBS H 
start_time <- Sys.time()
fit_H_cbs <- stan_glmer(interp_roche_n ~ (1 | regpro) + Urban + age_group + Sex + 
                          QuintMat + QuintSoc,
                         family = binomial,
                         data = df_H_cbs,
                         cores = 4,
                         seed = 1010, adapt_delta = 0.95)
end_time <- Sys.time()
end_time - start_time
print(fit_H_cbs)

###Matt
df_all_clsa <- df_clsa

df_all_clsa$Province = province_fun(df_all_clsa$FSA_COVID)
df_all_clsa <- df_all_clsa %>% 
  filter(Province != ("YT") & Province !=("NU/NT") & Province != ("QC")) #11946

df_all_clsa$age = df_all_clsa$SER_AGE_COV
df_all_clsa$age_group = cut(df_all_clsa$age, 
                          breaks = c(18,40,55,Inf),
                          labels = c('18-39','40-54','55+'),
                          right = FALSE)

df_all_clsa <- df_all_clsa[df_all_clsa$SER_SEX_COV %in% c('M','F'),] #11946
df_all_clsa$Sex = with(df_all_clsa, ifelse(SER_SEX_COV=='M', 1, 0))


df_all_clsa$Urban <- with(df_all_clsa,ifelse(substr(FSA_COVID,start = 2,stop = 2) != "0",1,0))

#df_all_clsa <- df_all_clsa[df_all_clsa$SER_ETHN_WH_COV >= 0,] # 11387
df_all_clsa$Race = with(df_all_clsa, ifelse(SER_ETHN_WH_COV==1, 1, NA))

df_all_clsa$Race = case_when(df_all_clsa$SER_ETHN_WH_COV==1 ~ 1,
                           df_all_clsa$SER_ETHN_SA_COV==1 ~ 0,
                           df_all_clsa$SER_ETHN_ZH_COV==1 ~ 0,
                           df_all_clsa$SER_ETHN_BL_COV==1 ~ 0,
                           df_all_clsa$SER_ETHN_FP_COV==1 ~ 0,
                           df_all_clsa$SER_ETHN_LA_COV==1 ~ 0,
                           df_all_clsa$SER_ETHN_AR_COV==1 ~ 0,
                           df_all_clsa$SER_ETHN_SE_COV==1 ~ 0,
                           df_all_clsa$SER_ETHN_WA_COV==1 ~ 0,
                           df_all_clsa$SER_ETHN_KO_COV==1 ~ 0,
                           df_all_clsa$SER_ETHN_JA_COV==1 ~ 0,
                           df_all_clsa$SER_ETHN_OTSP_COV %in% c("guianese of east indian descent","mixed black and white ancestry","indian","india","south america - biracial",
                                                              "west indian of east indian descent","300 hundreds year ago my ancestors came from great britain, prior to that we all originated from africa.",
                                                              "west  indian") ~ 0,
                           TRUE~NA)

length(unique(df_all_clsa[is.na(df_all_clsa$Race),]$SER_ETHN_OTSP_COV))

c("guianese of east indian descent","mixed black and white ancestry","indian","india","south america - biracial",
  "west indian of east indian descent","300 hundreds year ago my ancestors came from great britain, prior to that we all originated from africa.",
  "west  indian")

df_all_clsa$Month <- floor_date(df_all_clsa$date, unit = "month")
df_all_clsa$month <- plyr::mapvalues(as.character(df_all_clsa$Month), as.character(sort(unique(df_all_clsa$Month))), order(sort(unique(df_all_clsa$Month))))

df_all_clsa$month <- as.integer(df_all_clsa$month)
#df_all_clsa$Urban <- as.integer(df_all_clsa$urban)

df_all_clsa$region = factor(case_when(as.character(substr(df_all_clsa$FSA_COVID,1,1)) %in% c("A", "B", "C", "E")~"Atlantic", 
                                    as.character(substr(df_all_clsa$FSA_COVID,1,1)) %in% c("K", "L", "M", "N", "P")~"Ontario",
                                    as.character(substr(df_all_clsa$FSA_COVID,1,1)) %in% c("R", "S", "T")~"Prairies",
                                    as.character(substr(df_all_clsa$FSA_COVID,1,1)) %in% c("V")~"BC",
                                    TRUE~NA), levels = c("Atlantic","BC",  "Ontario", "Prairies"))

df_all_clsa$regpro = factor(case_when(as.character(substr(df_all_clsa$FSA_COVID,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                                    as.character(substr(df_all_clsa$FSA_COVID,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                                    as.character(substr(df_all_clsa$FSA_COVID,1,1)) %in% c("T")~"AB",
                                    as.character(substr(df_all_clsa$FSA_COVID,1,1)) %in% c("R")~"MB",
                                    as.character(substr(df_all_clsa$FSA_COVID,1,1)) %in% c("S")~"SK",
                                    as.character(substr(df_all_clsa$FSA_COVID,1,1)) %in% c("V")~"BC",
                                    TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK"))

df_all_clsa <- df_all_clsa[df_all_clsa$SER_NUCLEOCAPSID_COV >= 0,] # 10417
df_all_clsa$interp_roche_n <- df_all_clsa$SER_NUCLEOCAPSID_COV

#Load antibody participants cohort
df_canpath_qx <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/1_data/private/CANPATH/DAO-543759_ResearcherDataset_Qx_96014par_1125var.csv')
df_canpath_ad <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/1_data/private/CANPATH/DAO-543759_ResearcherDataset_Serology_Admin_25727par.csv')
df_canpath_result <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/1_data/private/CANPATH/DAO-543759_ResearcherDataset_Serology_Results_74503par.csv')


df_canpath_qx_mb <- read.csv(file = '/home/yuanyu/projects/covid-donor-v-survey-antibody/1_data/private/CANPATH/DAO-543759_ResearcherDataset_Qx_1114par_1127varMTP.csv')


df_canpath_1 <- df_canpath

df_canpath <- df_canpath_1
df_canpath <- merge(df_canpath, df_canpath_result, by='ResearcherID', all.x = TRUE)
#Join FSA
df_canpath <- merge(df_canpath, df_canpath_ad, by='ResearcherID', all.x = TRUE)
df_canpath <- merge(df_canpath, df_canpath_result, by='ResearcherID', all.y = TRUE)
col_list <- c("ResearcherID",'C_ADM_STUDY_ID', 'C_SDC_AGE', 'C_SDC_SEX', 'C_SDC_GENDER', 'C_ADM_FSA',
              "C1_ADM_COLLECT_DATE","C1_SAMPLE_ANTIGEN_TESTED","C1_SAMPLE_RESULT","C1_SAMPLE_ANTIGEN_CUTOFF",
              "C1_SAMPLE_RESULTS_DESCRIPTION", "C1_CITF_ASSAY_ID", 'C1_SAMPLE_SUGGESTED_STATUS')

df_canpath <- df_canpath[col_list]

df_canpath$Province = province_fun(df_canpath$C_ADM_FSA)
df_canpath <- df_canpath %>% 
  filter(Province != ("YT") & Province !=("NU/NT") & Province != ("QC")) #54196

df_canpath_mb <- merge(df_canpath_qx_mb, df_canpath_result, by='ResearcherID', all.x = TRUE)


# Anti-N Result
df_canpath <- df_canpath[df_canpath$C1_SAMPLE_ANTIGEN_TESTED==1,] #19070
df_canpath <- df_canpath[!is.na(df_canpath$C1_ADM_COLLECT_DATE),]
df_canpath$date <- as.Date(parse_date_time(df_canpath$C1_ADM_COLLECT_DATE, c("ymd","mdy")))

table(RFD4682_e[RFD4682_e$COLLECTION_DATE >= '2022-04-04 00:00:00.0000' & RFD4682_e$COLLECTION_DATE <= '2022-08-31 00:00:00.0000',])

summary(df_canpath$date)
df_canpath$Sex = abs(df_canpath$C_SDC_SEX - 1)

df_canpath$Urban <- with(df_canpath,ifelse(substr(C_ADM_FSA,start = 2,stop = 2) != "0",1,0))
df_canpath$age_group <- age_groups_fun(df_canpath$C_SDC_AGE)

head(df_canpath[is.na(df_canpath_qx$C_ADM_STUDY_ID),])

dim(df_canpath_result[df_canpath_result$ResearcherID %in% df_canpath_qx$C_ADM_STUDY_ID,])


#### APL ####
RFD4682_e <- RFD4682_e %>% mutate(year_month = format(RFD4682_e$COLLECTION_DATE, "%Y-%m"))
RFD4682_e <- RFD4682_e %>% 
  mutate(interp_roche_n = ifelse(RFD4682_e$'N-IgG_INTERP' == "Positive",1,0)) %>%
  filter(!is.na(interp_roche_n)) %>%
  mutate(Province = province_fun(PAT_FSA)) %>%
  mutate(age_group = age_groups_fun(AGE_AT_COLLECTION)) %>%
  mutate(Sex = ifelse(GENDER == "Male",1,0)) %>%
  mutate(Urban = ifelse(substr(PAT_FSA,start = 2,stop = 2) != "0",1,0)) %>%
  mutate(vac = ifelse(VACCINATION_STATUS %in% c('No ULI', 'Not Immunized'), 0, 1)) %>%
  mutate(regpro = factor(case_when(as.character(substr(PAT_FSA,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                             as.character(substr(PAT_FSA,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                             as.character(substr(PAT_FSA,1,1)) %in% c("T")~"AB",
                             as.character(substr(PAT_FSA,1,1)) %in% c("R")~"MB",
                             as.character(substr(PAT_FSA,1,1)) %in% c("S")~"SK",
                             as.character(substr(PAT_FSA,1,1)) %in% c("V")~"BC",
                             TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK"))) %>%
  filter(!is.na(Sex)) %>%
  filter(!is.na(Urban)) %>%
  filter(!is.na(vac))
  
           

df_E_apl$interp_roche_n <- with(df_E_apl,ifelse(df_E_apl$'N-IgG_INTERP' == "Positive",1,0))


df_E_apl <- RFD4682_e[RFD4682_e$COLLECTION_DATE >= '2022-04-04 00:00:00.0000' & RFD4682_e$COLLECTION_DATE <= '2022-08-31 00:00:00.0000',]
df_E_apl$Province = province_fun(df_E_apl$PAT_FSA)
#df_E_apl <- df_E_apl %>% 
#  filter(Province != ("YT") & Province !=("NU/NT") & Province != ("QC")) #4178

#df_E_apl$age = df_E_apl$AGE_AT_COLLECTION
# df_E_apl$age_group = cut(df_E_apl$age, 
#                          breaks = c(18,40,55,Inf),
#                          labels = c('18-39','40-54','55+'),
#                          right = FALSE)
df_E_apl$age_group <- age_groups_fun(df_E_apl$AGE_AT_COLLECTION)

df_E_apl <- df_E_apl[!is.na(df_E_apl$GENDER),] #8783
df_E_apl$Sex = with(df_E_apl,ifelse(GENDER == "Male",1,0))

df_E_apl <- df_E_apl[!is.na(df_E_apl$PAT_FSA),]
df_E_apl$Urban <- with(df_E_apl,ifelse(substr(PAT_FSA,start = 2,stop = 2) != "0",1,0))

df_E_apl$region = factor(case_when(as.character(substr(df_E_apl$PAT_FSA,1,1)) %in% c("A", "B", "C", "E")~"Atlantic", 
                                   as.character(substr(df_E_apl$PAT_FSA,1,1)) %in% c("K", "L", "M", "N", "P")~"Ontario",
                                   as.character(substr(df_E_apl$PAT_FSA,1,1)) %in% c("R", "S", "T")~"Prairies",
                                   as.character(substr(df_E_apl$PAT_FSA,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("Atlantic","BC",  "Ontario", "Prairies"))

df_E_apl$regpro = factor(case_when(as.character(substr(df_E_apl$PAT_FSA,1,1)) %in% c("A", "B", "C", "E")~"ATL", 
                                   as.character(substr(df_E_apl$PAT_FSA,1,1)) %in% c("K", "L", "M", "N", "P")~"ON",
                                   as.character(substr(df_E_apl$PAT_FSA,1,1)) %in% c("T")~"AB",
                                   as.character(substr(df_E_apl$PAT_FSA,1,1)) %in% c("R")~"MB",
                                   as.character(substr(df_E_apl$PAT_FSA,1,1)) %in% c("S")~"SK",
                                   as.character(substr(df_E_apl$PAT_FSA,1,1)) %in% c("V")~"BC",
                                   TRUE~NA), levels = c("ATL","BC","ON", "AB", "MB", "SK"))

df_E_apl <- df_E_apl[!is.na(df_E_apl$'N-IgG_INTERP'),]
df_E_apl$interp_roche_n <- with(df_E_apl,ifelse(df_E_apl$'N-IgG_INTERP' == "Positive",1,0))
df_E_apl$covidvv = with(df_E_apl, ifelse(VACCINATION_STATUS %in% c('No ULI', 'Not Immunized'), 0, 1))

df_E_apl$week <- week(df_E_apl$COLLECTION_DATE)
#df_E_apl$vac <- with(df_E_apl, covidvv == 1,1,0)
ts_E_apl <-
  df_E_apl %>%
  group_by(regpro,week) %>%
  dplyr::summarize(case_counts = sum(covidvv), rates = mean(covidvv)) %>%
  drop_na()

### APL E
start_time <- Sys.time()
fit_E_apl <- stan_glm(interp_roche_n ~ age_group + Sex + Urban + year_month,
                      family = binomial,
                      data = df_E_apl,
                      cores = 4,
                      seed = 1010, adapt_delta = 0.8)
end_time <- Sys.time()
end_time - start_time
print(fit_E_apl)
waic(fit_E_apl)
table(df_canpath$C1_CITF_ASSAY_ID)


