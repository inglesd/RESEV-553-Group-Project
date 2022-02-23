#Install required packages ####
library(tidyverse)
<<<<<<< HEAD

# library(psych)
# library(dplyr)
# library(ggplot2)
# library(scales)
# library(plyr)
=======
library(psych)
library(dplyr)
library(ggplot2)
library(scales)
>>>>>>> 99eb2373efdefd2ab8b333f2d167e011a3b7d5ca

#Import the dataset ####
library(haven)
prgusap1_puf <- read_sas("prgusap1_puf.sas7bdat", NULL)
View(prgusap1_puf)
df <- prgusap1_puf

#Rename the variables
#(Race5=RACETHN_5CAT, Region=REGION_US, EDU8=EDCAT8, Income=J_Q09USX)
df_small <- df %>%
  select(RACETHN_5CAT, REGION_US, EDCAT8, J_Q09USX)

df_small <- df_small %>%
 rename(Race=RACETHN_5CAT, Region=REGION_US, EDUC=EDCAT8, Income=J_Q09USX)


#Change the name of race variable 
df_Race <- df_small %>%
  mutate(Race = ifelse(Race==1, "Hispanic",
                       ifelse(Race==2, "White",
                              ifelse(Race==3, "Black",
                                     ifelse(Race==4, "Asian/Pacific Islander",
                                            ifelse(Race==6, "Other",
                                                   ifelse(Race==NA, "Not Stated")))))))
#Change the name of region variable 
df_Region_Race <- df_Race %>%
  mutate(Region = ifelse(Region== 1, "Northeast",
                         ifelse(Region== 2, "Midwest",
                                ifelse(Region== 3, "South",
                                       ifelse(Region== 4, "West", "Other Region")))))

#Change the name of education variable 
df_Ed_Reg_Race <- df_Region_Race %>%
  mutate(EDUC = ifelse(EDUC==1 | EDUC==2, "Primary",
                       ifelse(EDUC==3 | EDUC==4, "Secondary",
                              ifelse(EDUC==6, "Bachelors",
                                     ifelse(EDUC==7, "Masters",
                                            ifelse(EDUC==5 | EDUC==8, "Professional"))))))
                                                   #ifelse(EDUC==NA, "Not Stated")))))))
#Change the name of Income variable
df_ALL <- df_Ed_Reg_Race %>%
  mutate(Income = ifelse(Income== 1 | Income==2, "1-19,999",
                         ifelse(Income==3 | Income== 4, "20,000-39,999",
                                ifelse(Income== 5 | Income==6, "40,000-59,999",
                                       ifelse(Income==7, "60,000-74,999",
                                              ifelse(Income==8, "75,000-99,999",
                                                     ifelse(Income==9, "100,000-149,999",
                                                            ifelse(Income== 10, "150,000 +", "Other" ))))))))
                                                            #ifelse(Income== 11 | Income==NA, 
                                                                   #"Not Stated", "Other Income")))))))) 

#Normal Descriptive Stats
desc_stat_continuous <- df_ALL %>% group_by(Region,Income,Race,EDUC) %>%
  summarize(N=n(),
            mean=mean(Income),
            SD=sd(Income))
write.csv(desc_state, "desc_stat_continuous.csv")

#Descriptive stat for a plot
desc_stat <- df_ALL %>% group_by(Region,Race, Income, EDUC) %>%
  summarise(N=n()) %>%
  ungroup() %>%
  group_by(Region) %>%
  mutate (perc = N/sum(N))

#plot income by region
desc_stat$Income <- factor(desc_stat$Income,levels = c("1-19,999","20,000-39,999","40,000-59,999","60,000-74,999",
                                                  "75,000-99,999","100,000-149,999", "Not Stated"))

ggplot(desc_stat, aes(x = Region, y=perc, fill=Income)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_y_continuous(label = scales::: percent_format(accuracy =1))

#Plot of Race by Region
ggplot(desc_stat, aes(x = Region, y=perc, fill=Race)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_y_continuous(label = scales::: percent_format(accuracy =1))

#Plot of Education by Region
ggplot(desc_stat, aes(x = Region, y=perc, fill=EDUC)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_y_continuous(label = scales::: percent_format(accuracy =1))


#Cleaning the dataset
# First identifying rows with NAs
rownames(das)[apply(das, 2, anyNA)]
#removing all observations with NAs
das_clean <- das %>% na.omit()

write.csv(df_Region_Race, "df_clean")

