# Tab S2
library(tableone)

#Clean variables
cbs_dfs<-read.csv("1_data/private/cbs_df_final.csv") %>% 
  select(age_groups,sex,urban,race,quintmat,quintsoc) %>% 
  mutate(cohort = "CBS blood donor")

abc_dfs<-read.csv("1_data/private/abc_df_final.csv") %>% 
  select(age_groups,sex,urban,race) %>% 
  mutate(cohort = "Ab-c open cohort",
         quintmat = "Missing",
         quintsoc = "Missing") #Added only for visualization

#Replace NA values with "Missing"
cbs_dfs[,c("urban","quintmat","quintsoc")]<-lapply(cbs_dfs[,c("urban","quintmat","quintsoc")],
                                                   function(x){
                                                     x<-ifelse(is.na(x) | x == "pnts","Missing",x)
                                                     return(x)
                                                   })

abc_dfs$race<-ifelse(is.na(abc_dfs$race) | abc_dfs$race == "pnts",
                     "Missing",abc_dfs$race)

all_dfs<-rbind(cbs_dfs,abc_dfs)

#Change categorical vars into factors and assign levels for ordering
all_dfs<-all_dfs %>% 
  mutate(sex = factor(sex,levels = c("Female","Male","Missing")),
         race = factor(race,levels = c("Racialized minority","White","Missing")),
         urban = factor(urban,levels = c("Rural","Urban","Missing")),
         cohort = factor(cohort,levels = c("CBS blood donor","Ab-c open cohort")))
         
st<-CreateTableOne(data = all_dfs,vars = c("age_groups","quintmat",
                                           "quintsoc","race","sex","urban"),
                   strata = "cohort",test = F,smd = F)

##### Modify Table S3 in Cohort_rep for the survey comparison 

#Clean variables
cbs_dfs<- df_all_cbs %>% 
  select(age_group,Sex,Race) %>% 
  mutate(cohort = "CBS blood donor")

abc_dfs<-rbind(df_A_abc, df_B_abc, df_C_abc) %>% 
  select(age_group,Sex,Race) %>% 
  mutate(cohort = "Ab-c open cohort") #Added only for visualization

clsa_dfs<-df_n_clsa %>% 
  select(age_group,Sex,Race) %>% 
  mutate(cohort = "CLSA closed cohort")

canpaths_dfs<-df_n_canpaths %>% 
  select(age_group,Sex,Race) %>% 
  mutate(cohort = "CanPath closed cohort")

apl_dfs<-df_n_apl %>% 
  select(age_group,Sex) %>% 
  mutate(Race = NA,
         cohort = "APL outpatient lab")



#Replace NA values with "Missing"
cbs_dfs[,c("urban","quintmat","quintsoc")]<-lapply(cbs_dfs[,c("urban","quintmat","quintsoc")],
                                                   function(x){
                                                     x<-ifelse(is.na(x) | x == "pnts","Missing",x)
                                                     return(x)
                                                   })

abc_dfs$race<-ifelse(is.na(abc_dfs$race) | abc_dfs$race == "pnts",
                     "Missing",abc_dfs$race)


all_dfs<-rbind(cbs_dfs,apl_dfs,clsa_dfs,canpaths_dfs,abc_dfs)

all_dfs<-all_dfs %>% 
  mutate(Sex = factor(case_when(Sex == 1 ~ 'Male',
                         Sex == 0 ~ 'Female',
                         is.na(Sex) ~ 'Missing'),
                         levels = c("Female","Male","Missing")),
         Race = factor(case_when(Race == 1 ~ 'White',
                          Race == 0 ~ 'Racialized minority',
                          is.na(Race) ~ 'Missing'),
                          levels = c("Racialized minority","White","Missing")),
         cohort = factor(cohort,levels = c("CBS blood donor","Ab-c open cohort","CLSA closed cohort",
                                           "CanPath closed cohort", "APL outpatient lab")))


#Change categorical vars into factors and assign levels for ordering

library(tableone)
st<-as.data.frame(print(CreateTableOne(data = all_dfs,vars = c("age_group","Race","Sex"),
                   strata = "cohort",test = F,smd = F), showAllLevels = T,format="fp"))

print(st)

write.csv(st, file = "/home/yuanyu/projects/covid-donor-v-survey-antibody/4_output/all_dfs.csv")
st <- read.csv("/home/yuanyu/projects/covid-donor-v-survey-antibody/4_output/all_dfs.csv")

st_ccahs <- st[,5]

st[,"CCAHS I closed cohort"] <- st_ccahs
rownames(st)<-NULL
st[1,1] <- "n"

colnames(st)[colnames(st) == "Ab-c open cohort"] <- "ABC open cohort"

st <- st[, c(1,3,6,5,2,7,4)]

kbl(st[, c(1,3,6,5,2,7,4)], booktabs = T) %>%
  #kable_paper(bootstrap_options = "striped", full_width = F) %>%
  #kable_styling() %>%
  #column_spec(1:2, extra_css = "word-wrap: break-word; white-space: normal;") %>%
  kable_classic_2(full_width = F)  %>%
  row_spec(0, bold = T) %>%
  pack_rows("Age Group (%)", 2, 6) %>%
  pack_rows("Race (%)", 7, 9) %>%
  pack_rows("Sex (%)", 10, 12)

df[2, 2] <- cell_spec(df[2, 2], "latex", bold = T)

