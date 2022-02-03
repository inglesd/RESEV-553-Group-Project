#Install required packages ####
library(tidyverse)
library(psych)
library(dplyr)
library(ggplot2)
library(scales)

#Import the dataset

#Prepare the dataset for analysis ####

df <- subset(prgusap1_puf, select = c(RACETHN_5CAT, REGION_US, EDCAT8, J_Q09USX))

#Rename the variables
df$Race5 <- (df$RACETHN_5CAT)
df$Region <- (df$REGION_US)
df$Edu8 <-(df$EDCAT8)
df$Income <-(df$J_Q09USX)

das <-subset(df, select = c(Race5, Region, Edu8, Income))

#identifying rows with NAs
rownames(das)[apply(das, 2, anyNA)]
#removing all observations with NAs
das_clean <- das %>% na.omit()


#Describe the variables ####
str(das_clean)
summary(das_clean) 
table(das_clean)
describe(das_clean) 

#Change variable type and describe each variable
#Race variable
das_clean$Race5 <- as.factor(das_clean$Race5)
str(das_clean$Race5)
summary(das_clean$Race5)
table(das_clean$Race5)
describe(das_clean$Race5)

#Region variable
das_clean$Region <- as.factor(das_clean$Region)
str(das_clean$Region)
summary(das_clean$Region)
table(das_clean$Region)

#Highest education variable
das_clean$Edu8 <- as.factor(das_clean$Edu8)
str(das_clean$Edu8)
summary(das_clean$Edu8)
table(das_clean$Edu8)

#Income variable
das_clean$Income <- as.factor(das_clean$Income)
str(das_clean$Income)
summary(das_clean$Income)
table(das_clean$Income)

#Categorical Data Visualization ####
data_perc <- t(prop.table(table(das_clean$Race5))) * 100
barplot(data_perc, ylab = "Percent")

ggplot(das_clean, aes(Race5)) +
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels = percent)



#Race
das_clean %>%
  ggplot(aes(Race5, fill = Race5))+
  geom_bar(fill = "#97B3C6", color = "black")+
  geom_bar(position = "dodge",
           alpha = 0.7)+
  theme_bw()+
  labs(title = NULL,
       x = "Race",
       y = "Count")+
  scale_fill_discrete(name = "Race", label=c("1 - Hispanic", "2 - White", "3 - Black", 
                                             "4 - Asian/Pacific islander", "6 - Other", 
                                             "NA - Not stated")) +
  theme(axis.text.x = element_text(size = 10))

#Region
region_per <- t(prop.table(table(das_clean$Region))) * 100
barplot(region_per, y = "Percent")

ggplot(das_clean$region, aes(x))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels = percent)


das_clean %>%
  ggplot(aes(Region, fill = Region))+
  geom_bar(fill = "#97B3C6", color = "black")+
  geom_bar(position = "dodge",
           alpha = 0.7)+
  theme_bw()+
  labs(title = NULL,
       x = "Region",
       y = "Percent")+
  scale_fill_discrete(name = "Region", label=c("1 - Northeast", "2 - Midwest", "3 - South", 
                                             "4 - West")) +
  theme(axis.text.x = element_text(size = 10))

#Highest education
das_clean%>%
  ggplot(aes(Edu8, fill = Edu8))+
  geom_bar(fill = "#97B3C6", color = "black")+
  geom_bar(position = "dodge",
           alpha = 0.7)+
  theme_bw()+
  labs(title = NULL,
       x = "Education level",
       y = "Count")+
  scale_fill_discrete(name = "Education level", label=c("1 - Primary or less", "2 - Lower secondary", "3 - Upper secondary", 
                                             "4 - Post secondary, non-tertiary", "5 - Tertiary, professional degree", 
                                             "6 - Tertiary, bachelor degree", "7 - Tertiary, master degree", "8 - Tertiary, research degree",
                                             "NA - Not stated")) +
  theme(axis.text.x = element_text(size = 10))
#Income
das_clean %>%
  ggplot(aes(Income, fill = Income))+
  geom_bar(fill = "#97B3C6", color = "black")+
  geom_bar(position = "dodge",
           alpha = 0.7)+
  theme_bw()+
  labs(title = NULL,
       x = "Income",
       y = "Count")+
  scale_fill_discrete(name = "Household income in last 12 months", label=c("1 - Between $1 and $9,999", "2 - Between $10,000 and $19,999", "3 - Between $20,000 and $29,999", 
                                                        "4 - Between $30,000 and $39,999", "5 - Between $40,000 and $49,999", 
                                                        "6 - Between $50,000 and $59,999", "7 - Between $60,000 and $74,999", "8 - Between $75,000 and $99,999",
                                                        "9 - Between $100,000 and $149,999", "10 - $150,000 or more",
                                                        "11 - No household income","NA - Not stated")) +
  theme(axis.text.x = element_text(size = 10))


#Comparing the variables ####

table(das_clean) #Race x Geographical region
(df_tbl <- as.data.frame(table(das_clean)))

# Region split by race
ggplot(data = df_tbl) +
  geom_bar(mapping = aes(x = Race5, y = Freq, fill =  Region),
           position = "dodge", #it is next to each other 
           stat = "identity") +
  xlab("Race") +
  ylab("Region") +
  scale_x_discrete(label=c("Hispanic", "White", "Black", "Asian/Pacific islander", "Other")) +
  scale_fill_manual(name = "Region", 
                    label=c("Northeast", "Midwest", "South", "West"),
                    values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
  theme(axis.text.x = element_text(size = 10))

# Education level split by region
ggplot(data = df_tbl) +
  geom_bar(mapping = aes(x = Region, y = Freq, fill =  Edu8), 
           position = "dodge", 
           stat = "identity") +
  xlab("Region") +
  ylab("Education level") +
  scale_x_discrete(label=c("Northeast", "Midwest", "South", "West")) +
  scale_fill_manual(name = "Education level", 
                    label=c("Primary or less", "Lower secondary", "Upper secondary", 
                                             "Post secondary, non-tertiary", "Tertiary, professional degree", 
                                             "Tertiary, bachelor degree", "Tertiary, master degree", 
                                            "Tertiary, research degree"),
                    values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  theme(axis.text.x = element_text(size = 10))

#Household income split by region
ggplot(data = df_tbl) +
  geom_bar(mapping = aes(x = Region, y = Freq, fill =  Income), 
           position = "dodge", 
           stat = "identity") +
  xlab("Region") +
  ylab("Income") +
  scale_x_discrete(label=c("Northeast", "Midwest", "South", "West")) +
  scale_fill_manual(name = "Household income", 
                    label=c("Between $1 and $9,999", "Between $10,000 and $19,999", "Between $20,000 and $29,999", 
                            "Between $30,000 and $39,999", "Between $40,000 and $49,999", 
                            "Between $50,000 and $59,999", "Between $60,000 and $74,999", "Between $75,000 and $99,999",
                            "Between $100,000 and $149,999", "$150,000 or more",
                            "No household income"),
                    values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#9999CC", "#66CC99")) +
  theme(axis.text.x = element_text(size = 10))

