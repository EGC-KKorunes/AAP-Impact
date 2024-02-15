##Script for AAP Impact Data Analysis and Plotting
##Author: Ethan C. Cissell
##Date: 2.15.2024

#Load in Required Libraries
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(gdata)
library(readxl)
library(car)
library(viridisLite)
library(viridis)
library(Cairo)
library(forcats)
library(maps)
library(mapproj)
library(sf)
library(rmapshaper)
library(asbio)
library(vegan)
library(cowplot)
library(grid)
library(gridExtra)
library(igraph)
library(HiveR)
library(ggraph)

##############################################################
##############################################################
#First, we will analyze the awards data (Fig. 1 in manuscript)
##############################################################
##############################################################
#Read-in Excel files for raw NIH awards data
#Will read in each sheet as a seperate df, and then combine
df_2010 = read_excel("C:/Users/ethan.cissell/EGC Dropbox/Master Grants/AAP/Compiled Data/AAP_Publication/Perspective_Code_DF_and_Figures/Dataframes/AAP_R_Money_df_partcipatingICs.xlsx",
                   sheet=1)
df_2011 = read_excel("C:/Users/ethan.cissell/EGC Dropbox/Master Grants/AAP/Compiled Data/AAP_Publication/Perspective_Code_DF_and_Figures/Dataframes/AAP_R_Money_df_partcipatingICs.xlsx",
                   sheet=2)
df_2012 = read_excel("C:/Users/ethan.cissell/EGC Dropbox/Master Grants/AAP/Compiled Data/AAP_Publication/Perspective_Code_DF_and_Figures/Dataframes/AAP_R_Money_df_partcipatingICs.xlsx",
                   sheet=3)
df_2013 = read_excel("C:/Users/ethan.cissell/EGC Dropbox/Master Grants/AAP/Compiled Data/AAP_Publication/Perspective_Code_DF_and_Figures/Dataframes/AAP_R_Money_df_partcipatingICs.xlsx",
                   sheet=4)
df_2014 = read_excel("C:/Users/ethan.cissell/EGC Dropbox/Master Grants/AAP/Compiled Data/AAP_Publication/Perspective_Code_DF_and_Figures/Dataframes/AAP_R_Money_df_partcipatingICs.xlsx",
                   sheet=5)
df_2015 = read_excel("C:/Users/ethan.cissell/EGC Dropbox/Master Grants/AAP/Compiled Data/AAP_Publication/Perspective_Code_DF_and_Figures/Dataframes/AAP_R_Money_df_partcipatingICs.xlsx",
                   sheet=6)
df_2016 = read_excel("C:/Users/ethan.cissell/EGC Dropbox/Master Grants/AAP/Compiled Data/AAP_Publication/Perspective_Code_DF_and_Figures/Dataframes/AAP_R_Money_df_partcipatingICs.xlsx",
                   sheet=7)
df_2017 = read_excel("C:/Users/ethan.cissell/EGC Dropbox/Master Grants/AAP/Compiled Data/AAP_Publication/Perspective_Code_DF_and_Figures/Dataframes/AAP_R_Money_df_partcipatingICs.xlsx",
                   sheet=8)
df_2018 = read_excel("C:/Users/ethan.cissell/EGC Dropbox/Master Grants/AAP/Compiled Data/AAP_Publication/Perspective_Code_DF_and_Figures/Dataframes/AAP_R_Money_df_partcipatingICs.xlsx",
                   sheet=9)
df_2019 = read_excel("C:/Users/ethan.cissell/EGC Dropbox/Master Grants/AAP/Compiled Data/AAP_Publication/Perspective_Code_DF_and_Figures/Dataframes/AAP_R_Money_df_partcipatingICs.xlsx",
                   sheet=10)
df_2020 = read_excel("C:/Users/ethan.cissell/EGC Dropbox/Master Grants/AAP/Compiled Data/AAP_Publication/Perspective_Code_DF_and_Figures/Dataframes/AAP_R_Money_df_partcipatingICs.xlsx",
                   sheet=11)
df_2021 = read_excel("C:/Users/ethan.cissell/EGC Dropbox/Master Grants/AAP/Compiled Data/AAP_Publication/Perspective_Code_DF_and_Figures/Dataframes/AAP_R_Money_df_partcipatingICs.xlsx",
                   sheet=12)
df_2022 = read_excel("C:/Users/ethan.cissell/EGC Dropbox/Master Grants/AAP/Compiled Data/AAP_Publication/Perspective_Code_DF_and_Figures/Dataframes/AAP_R_Money_df_partcipatingICs.xlsx",
                   sheet=13)

#Now merge into a single df
df_merge = rbind(df_2010,
                 df_2011,
                 df_2012,
                 df_2013,
                 df_2014,
                 df_2015,
                 df_2016,
                 df_2017,
                 df_2018,
                 df_2019,
                 df_2020,
                 df_2021,
                 df_2022)

#Now subset to only the 11 AAP-participating ICs
df_participating = df_merge %>%
  subset(IC_Participating == "TRUE" | AAP_Participant == "Y")

#Create a summary df for awards and funding by year, demographic groups, and AAP participant status
df = df_participating %>%
  group_by(Award_Year,AAP_Participant,Disadvantaged, Woman_Owned) %>%
  summarise(
    Total_Award_Sub = n(),
    Total_Funding_Sub = sum(Award_Amount)
  )

#First, calculate the percent of awards given to WOSBs and SDBs across 2020-2022 for AAP vs. non-AAP (within group totals)
#First for WOSBs
aap_data_money_plot43_df = df %>%
  select(c(Award_Year,Total_Award_Sub, AAP_Participant, Disadvantaged,Woman_Owned)) %>%
  group_by(Award_Year,AAP_Participant) %>%
  mutate(total_award_group  = sum(Total_Award_Sub))%>%
  ungroup() %>%
  subset(Woman_Owned == "Y") %>%
  group_by(Award_Year,AAP_Participant) %>%
  mutate(
    sub_award_group = sum(Total_Award_Sub)) %>%
  distinct(sub_award_group, .keep_all = TRUE) %>%
  subset(Award_Year == "2020" | Award_Year == "2021" | Award_Year == "2022") %>%
  group_by(AAP_Participant) %>%
  summarise(percent_award = 100*(sum(sub_award_group)/sum(total_award_group))) %>%
  mutate(group = "WOSBs")

#Then for SDBs
aap_data_money_plot44_df = df %>%
  select(c(Award_Year,Total_Award_Sub, AAP_Participant, Disadvantaged,Woman_Owned)) %>%
  group_by(Award_Year,AAP_Participant) %>%
  mutate(total_award_group  = sum(Total_Award_Sub))%>%
  ungroup() %>%
  subset(Disadvantaged == "Y") %>%
  group_by(Award_Year,AAP_Participant) %>%
  mutate(
    sub_award_group = sum(Total_Award_Sub)) %>%
  distinct(sub_award_group, .keep_all = TRUE) %>%
  subset(Award_Year == "2020" | Award_Year == "2021" | Award_Year == "2022") %>%
  group_by(AAP_Participant) %>%
  summarise(percent_award = 100*(sum(sub_award_group)/sum(total_award_group))) %>%
  mutate(group = "SDBs")

#Merge WOSB and SDB dfs
aap_data_money_plot45_df = rbind(aap_data_money_plot43_df,aap_data_money_plot44_df)

#Change factor levels for better plotting
aap_data_money_plot45_df$group=factor(aap_data_money_plot45_df$group, levels = c("WOSBs", "SDBs"))

#Now plot
CairoTIFF("C:/Users/ethan.cissell/EGC Dropbox/Master Grants/AAP/Compiled Data/AAP_Publication/Perspective_Code_DF_and_Figures/Figures/Figure_1_updated.tiff",
          dpi=600,
          units="in",
          width=6.5,
          height=4.5)
plot48=ggplot(aap_data_money_plot45_df,
              aes(x = group,
                  y = percent_award,
                  fill = AAP_Participant)) +
  geom_col(position = "dodge",
           stat = "identity",
           color = "black",
           linewidth = 1.3,
           size = 4) + 
  scale_fill_manual(values=c("#BC9E5c","#01365B"),
                    name = ("Group"),
                    labels = c("Non-AAP", "AAP")) +
  ylab("Percentage of Awards (%)") +
  xlab("Demographic Group") +
  theme_classic() +
  guides(color = "none")+
  theme(axis.line = element_line(colour = "black",
                                 linewidth = 1),
        axis.text = element_text(colour = "black",
                                 size = 12),
        axis.title = element_text(colour = "black",
                                  size = 13))
plot48
dev.off()

#Extract percent change between groups
aap_data_money_plot46_df = aap_data_money_plot45_df%>%
  group_by(group) %>%
  summarise(fold = 100*((percent_award-lag(percent_award))/(lag(percent_award))))

#Extract total award counts 
aap_data_money_plot50_df = df %>%
  select(c(Award_Year,Total_Award_Sub, AAP_Participant, Disadvantaged,Woman_Owned)) %>%
  group_by(Award_Year,AAP_Participant) %>%
  mutate(total_award_group  = sum(Total_Award_Sub))%>%
  ungroup() %>%
  subset(Woman_Owned == "Y") %>%
  group_by(Award_Year,AAP_Participant) %>%
  mutate(
    sub_award_group = sum(Total_Award_Sub)) %>%
  distinct(sub_award_group, .keep_all = TRUE) %>%
  subset(Award_Year == "2020" | Award_Year == "2021" | Award_Year == "2022") %>%
  group_by(AAP_Participant) %>%
  summarise(awards1 = (sum(sub_award_group)),
            awards2 = (sum(total_award_group))) %>%
  mutate(group = "WOSBs")

aap_data_money_plot51_df = df %>%
  select(c(Award_Year,Total_Award_Sub, AAP_Participant, Disadvantaged,Woman_Owned)) %>%
  group_by(Award_Year,AAP_Participant) %>%
  mutate(total_award_group  = sum(Total_Award_Sub))%>%
  ungroup() %>%
  subset(Disadvantaged == "Y") %>%
  group_by(Award_Year,AAP_Participant) %>%
  mutate(
    sub_award_group = sum(Total_Award_Sub)) %>%
  distinct(sub_award_group, .keep_all = TRUE) %>%
  subset(Award_Year == "2020" | Award_Year == "2021" | Award_Year == "2022") %>%
  group_by(AAP_Participant) %>%
  summarise(awards1 = (sum(sub_award_group)),
            awards2 = (sum(total_award_group))) %>%
  mutate(group = "SDBs")

#Pull total average percent awards across 2010 - 2022 directed to SDBs + WOSBs
#This is from the full dataset including awards across all ICs
df2 = df_merge %>%
  subset(AAP_Participant == "N") %>% #9388
  group_by(Award_Year,AAP_Participant,Disadvantaged, Woman_Owned) %>%
  summarise(
    Total_Award_Sub = n(),
    Total_Funding_Sub = sum(Award_Amount)
  )

df3 = df2 %>%
  select(c(Award_Year,Total_Award_Sub, AAP_Participant, Disadvantaged,Woman_Owned)) %>%
  group_by(Award_Year) %>%
  mutate(total_award_group  = sum(Total_Award_Sub))%>%
  ungroup() %>%
  subset(Woman_Owned == "Y") %>%
  group_by(Award_Year) %>%
  mutate(
    sub_award_group = sum(Total_Award_Sub)) %>%
  distinct(sub_award_group, .keep_all = TRUE) %>%
  ungroup() %>%
  summarise(awards1 = (sum(sub_award_group)),
            awards2 = (sum(total_award_group))) %>%
  mutate(group = "WOSBs") #12%

df4 = df2 %>%
  select(c(Award_Year,Total_Award_Sub, AAP_Participant, Disadvantaged,Woman_Owned)) %>%
  group_by(Award_Year) %>%
  mutate(total_award_group  = sum(Total_Award_Sub))%>%
  ungroup() %>%
  subset(Disadvantaged == "Y") %>%
  group_by(Award_Year) %>%
  mutate(
    sub_award_group = sum(Total_Award_Sub)) %>%
  distinct(sub_award_group, .keep_all = TRUE) %>%
  ungroup() %>%
  summarise(awards1 = (sum(sub_award_group)),
            awards2 = (sum(total_award_group))) %>%
  mutate(group = "SDBs") #6%

###############################################################
##############################################################
#Now we will analyze the survey data (Figure 3 in manuscript)
##############################################################
##############################################################

#Read in dataframe of survey data
survey_raw = read.csv("C:/Users/ethan.cissell/EGC Dropbox/Master Grants/AAP/Compiled Data/AAP_Publication/Perspective_Code_DF_and_Figures/Dataframes/Surveys_2021_2022.csv")

#First, pull total knowledge gain data (calculated as the difference in % responses with "A great deal" between post and pre AAP participation surveys for each question, grouped by cohort [n = 6 cohorts])
#Do different subsets for Human Subjects and Vertebrate animals because that question was not applicable across all respondents (for calculating denominator for accurate percents).
#First, excluding
survey_full_diffdf = survey_raw %>%
  pivot_longer(cols = c(7:22),
               names_to = "Question",
               values_to = "Response") %>%
  filter(Question != "Human_Subjects") %>%
  filter(Question != "Vertebrate_Animals") %>%
  filter(Response != "") %>%
  mutate(Response = ifelse (Response == "", "Blank", Response)) %>%
  group_by(Post.Pre,Year,Cycle,Question, Response) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count)*100) %>%
  ungroup() %>%
  complete(Post.Pre,Year,Cycle,Question,Response,fill = list(percentage = 0)) %>%
  subset(Response == "A great deal") %>%
  ungroup() %>%
  group_by(Year,Cycle,Question) %>%
  summarise(difference = diff(range(percentage))) %>%
  ungroup()%>%
  mutate(Question = as.factor(Question)) %>%
  arrange(difference)%>%
  as.data.frame() %>%
  mutate(Question = fct_rev(fct_reorder(Question,difference)))

#Next, subset to only including
survey_full_diffd_hsv = survey_raw %>%
  pivot_longer(cols = c(7:22),
               names_to = "Question",
               values_to = "Response") %>%
  filter(Question %in% c("Human_Subjects","Vertebrate_Animals")) %>%
  filter(Response != "") %>%
  filter(Response != "Not applicable") %>%
  mutate(Response = ifelse (Response == "", "Blank", Response)) %>%
  group_by(Post.Pre,Year,Cycle,Question, Response) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count)*100) %>%
  ungroup() %>%
  complete(Post.Pre,Year,Cycle,Question,Response,fill = list(percentage=0)) %>%
  subset(Response == "A great deal") %>%
  ungroup() %>%
  group_by(Year,Cycle,Question) %>%
  summarise(difference = diff(range(percentage))) %>%
  ungroup()%>%
  mutate(Question = as.factor(Question)) %>%
  arrange(difference)%>%
  as.data.frame() %>%
  mutate(Question = fct_rev(fct_reorder(Question,difference)))

survey_full_diffdf = rbind(survey_full_diffdf,survey_full_diffd_hsv)
survey_full_diffdf = survey_full_diffdf%>%
  mutate (Group = "Full")

#Now plot (Figure 3 in manuscript)
CairoPNG("C:/Users/ethan.cissell/EGC Dropbox/Master Grants/AAP/Compiled Data/AAP_Publication/Perspective_Code_DF_and_Figures/Figures/Figure3_updated.png",
         dpi=600,
         units="in",
         width=8.5,
         height=9)
ggplot(survey_full_diffdf, aes(x = difference, y = Question)) +
  geom_boxplot(position = position_dodge(width = 0.8),
               alpha = 0.9,
               outlier.shape=NA,
               size=0.9,
               fill="#555555") +
  #  geom_jitter(aes(color=Question),height = 0.2, size=4,alpha=0.7) +
  theme_classic() +
  theme(axis.line = element_line(colour="black",
                                 linewidth = 1),
        axis.text = element_text(colour="black",
                                 size=12),
        axis.title = element_text(colour = "black",
                                  size=13))+
  labs(x = expression(paste("Knowledge Gain (", Delta, " % of participants)"))) +
  ylab("Knowledge Area") +
  scale_y_discrete(labels = c("Approach",
                              "Significance",
                              "Contacting a PO",
                              "Investigators",
                              "Innovation",
                              "Where to find information",
                              "Overall Impact",
                              "Budget",
                              "Submission (ASSIST)",
                              "PHS Assignment Form",
                              "Scientific Environment",
                              "Scientific Merit Review",
                              "Advisory Council Review",
                              "How CSR works",
                              "Vertebrate Animals",
                              "Human Subjects"))
dev.off()

###Now analyze just the data from resubmissions to quantify knowledge gain against zero for those who already had experience with trying to learn the grant process
#Read in binomial resubmission dataframe (y/n) for each respondent
resub_raw = read.csv("C:/Users/ethan.cissell/EGC Dropbox/Master Grants/AAP/Compiled Data/AAP_Publication/Perspective_Code_DF_and_Figures/Dataframes/resub_df.csv")

#Merge to subset to resub only
survey_subset = merge(survey_raw,resub_raw,by="Company")

#Calculate delta knowledge gain for each question, grouped by cohort for N
#Calculate mean diff +-SD for resub subset by cohort
#Need to analyze separately for Vertebrate and Human Subjects because n() will differ
#First, non hsv
survey_long_diffdf = survey_subset %>%
  pivot_longer(cols = c(7:22),
               names_to = "Question",
               values_to = "Response") %>%
  filter(Question !="Human_Subjects") %>%
  filter(Question !="Vertebrate_Animals") %>%
  filter(Response != "") %>%
  mutate(Response = ifelse (Response == "", "Blank", Response)) %>%
  group_by(Post.Pre,Year,Cycle,Question, Response) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count)*100) %>%
  ungroup() %>%
  complete(Post.Pre,Year,Cycle,Question,Response,fill = list(percentage=0)) %>%
  subset(Response=="A great deal") %>%
  ungroup() %>%
  group_by(Year,Cycle,Question) %>%
  summarise(difference=diff(range(percentage))) %>%
  ungroup()%>%
  mutate(Question = as.factor(Question)) %>%
  arrange(difference)%>%
  as.data.frame() %>%
  mutate(Question = fct_rev(fct_reorder(Question,difference)))

survey_long_diffdf_hsv = survey_subset %>%
  pivot_longer(cols = c(7:22),
               names_to = "Question",
               values_to = "Response") %>%
  filter(Question %in% c("Human_Subjects","Vertebrate_Animals")) %>%
  filter(Response != "") %>%
  filter(Response != "Not applicable") %>%
  mutate(Response = ifelse (Response == "", "Blank", Response)) %>%
  group_by(Post.Pre,Year,Cycle,Question, Response) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count)*100) %>%
  ungroup() %>%
  complete(Post.Pre,Year,Cycle,Question,Response,fill = list(percentage=0)) %>%
  subset(Response == "A great deal") %>%
  ungroup() %>%
  group_by(Year,Cycle,Question) %>%
  summarise(difference = diff(range(percentage))) %>%
  ungroup()%>%
  mutate(Question = as.factor(Question)) %>%
  arrange(difference)%>%
  as.data.frame() %>%
  mutate(Question = fct_rev(fct_reorder(Question,difference)))

survey_long_diffdf = rbind(survey_long_diffdf,survey_long_diffdf_hsv)

#Do T-Tests to see if differences are greater than Zero
#Split the data by group
groups1 = split(survey_long_diffdf$difference, survey_long_diffdf$Question)
#Perform the t-test for each group
t.test_results1 = lapply(groups1, function(x) t.test(x, mu = 0))

#Check Assumption of normality
shapiro_results1=tapply(survey_long_diffdf$difference, survey_long_diffdf$Question,shapiro.test)
#All P>>0.05, accept null of normal distribution for each group

#Print the t-test results
print(t.test_results1)

#Do the same for WOSBs and SDBs
survey_wum_diffdf = survey_raw %>%
  subset(Women == "Yes" | Underrepresented == "Yes")%>%
  pivot_longer(cols = c(7:22),
               names_to = "Question",
               values_to = "Response") %>%
  filter(Question != "Human_Subjects") %>%
  filter(Question != "Vertebrate_Animals") %>%
  filter(Response != "") %>%
  mutate(Response = ifelse (Response == "", "Blank", Response)) %>%
  group_by(Post.Pre,Year,Cycle,Question, Response) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count)*100) %>%
  ungroup() %>%
  complete(Post.Pre,Year,Cycle,Question,Response,fill = list(percentage=0)) %>%
  subset(Response == "A great deal") %>%
  ungroup() %>%
  group_by(Year,Cycle,Question) %>%
  summarise(difference = diff(range(percentage))) %>%
  ungroup()%>%
  mutate(Question = as.factor(Question)) %>%
  arrange(difference)%>%
  as.data.frame() %>%
  mutate(Question = fct_rev(fct_reorder(Question,difference)))

survey_wum_diffd_hsv = survey_raw %>%
  subset(Women == "Yes" | Underrepresented == "Yes")%>%
  pivot_longer(cols = c(7:22),
               names_to = "Question",
               values_to = "Response") %>%
  filter(Question %in% c("Human_Subjects","Vertebrate_Animals")) %>%
  filter(Response != "") %>%
  filter(Response != "Not applicable") %>%
  mutate(Response = ifelse (Response == "", "Blank", Response)) %>%
  group_by(Post.Pre,Year,Cycle,Question, Response) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count)*100) %>%
  ungroup() %>%
  complete(Post.Pre,Year,Cycle,Question,Response,fill = list(percentage=0)) %>%
  subset(Response == "A great deal") %>%
  ungroup() %>%
  group_by(Year,Cycle,Question) %>%
  summarise(difference = diff(range(percentage))) %>%
  ungroup()%>%
  mutate(Question = as.factor(Question)) %>%
  arrange(difference)%>%
  as.data.frame() %>%
  mutate(Question = fct_rev(fct_reorder(Question,difference)))

survey_wum_diffdf = rbind(survey_wum_diffdf,survey_wum_diffd_hsv)

#Pull mean + SD for each subset for Supplementary Tables
resub_means = survey_long_diffdf %>%
  group_by(Question) %>%
  summarise (mean = mean(difference),
             sd = sd(difference))

#Overall mean and SD
mean(survey_long_diffdf$difference) #55.13
sd(survey_long_diffdf$difference)#20.70

#Pull mean + SD for WUM subset
wum_means = survey_wum_diffdf %>%
  group_by(Question) %>%
  summarise (mean = mean(difference),
             sd = sd(difference))
#Overall mean and SD
mean(survey_wum_diffdf$difference) #49.12
sd(survey_wum_diffdf$difference)#25.68

#Pull mean + SD for Full
full_means = survey_full_diffdf %>%
  group_by(Question) %>%
  summarise (mean = mean(difference),
             sd = sd(difference))
#Overall mean and SD
mean(survey_full_diffdf$difference) #57.52
sd(survey_full_diffdf$difference)#13.52

########################################################################
########################################################################
#Now we will analyze the follow-on funding data (Figure 4 in manuscript)
########################################################################
########################################################################
#Read in the dataset
new_AAP_awards = read_excel("C:/Users/ethan.cissell/EGC Dropbox/Master Grants/AAP/Compiled Data/AAP_Publication/Perspective_Code_DF_and_Figures/Dataframes/Updated_Awards.xlsx",
                            sheet=16)

#Analyze subset to only those receiving awards 2021 and before
new_AAP_awards_2021 = new_AAP_awards %>%
  mutate(Award_Year = substr(Award_Date,1,regexpr("-",Award_Date) - 1)) %>%
  subset(Award_Year == "2019" | Award_Year == "2020" | Award_Year == "2021")

#Phase I Total
sum(new_AAP_awards_2021$Award_Amount)
#15910147
follow_on_cleaned5 = follow_on_cleaned1 %>%
  filter(Company %in% (new_AAP_awards_2021$Company_Name)) %>%
  drop_na(Amount)%>%
  group_by(Category)%>%
  summarise(
    count = n(),
    Sum = sum(Amount))
sum(follow_on_cleaned5$Sum)
#97952672

#Pull X follow on for <2021
97952672/15910147 #6.16

#Pull pathways
follow_on_cleaned6 = follow_on_cleaned1 %>%
  filter(Company %in% (new_AAP_awards_2021$Company_Name)) %>%
  drop_na(Amount)%>%
  group_by(Company,Category)%>%
  summarise(
    count = n())
#2

#Pull number of unique companies
follow_on_cleaned7 = follow_on_cleaned1 %>%
  filter(Company %in% (new_AAP_awards_2021$Company_Name)) %>%
  distinct(Company)
#28
