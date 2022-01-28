#Install required packages ####
library(tidyverse)
library(psych)
library(dplyr)
library(ggplot2)

#Import the dataset

#Prepare the dataset for analysis ####

df <- subset(prgusap1_puf, select = c(RACETHN_5CAT, REGION_US, EDCAT8, J_Q09USX))

#Rename the variables
df$Race5 <- (df$RACETHN_5CAT)
df$Region <- (df$REGION_US)
df$Edu8 <-(df$EDCAT8)
df$Income <-(df$J_Q09USX)

das <-subset(df, select = c(Race5, Region, Edu8, Income))

#Describe the variables
str(das)
summary(das) 
table(das)
describe(das) 

#Change variable type and describe one by one
das$Race5 <- as.factor(das$Race5)
str(das$Race5)
summary(das$Race5)
table(das$Race5)

das$Region <- as.factor(das$Region)
str(das$Region)
summary(das$Region)
table(das$Region)

das$Edu8 <- as.factor(das$Edu8)
str(das$Edu8)
summary(das$Edu8)
table(das$Edu8)

das$Income <- as.factor(das$Income)
str(das$Income)
summary(das$Income)
table(das$Income)

#Categorical Data Visualization
das %>%
  ggplot(aes(x = Race5))+
  geom_bar(fill = "#97B3C6", color = "black")+
  theme_bw()+
  labs(x = "Race",
       y = "Count",
       title = NULL) + 
  scale_fill_manual(name = "RACE", 
                    label=c("Hispanic", "White", "Black", "West", "Other"))

ggplot(data = das) +
  geom_bar(mapping = aes(x = Race5, y = Count, fill = Race5), 
           position = "dodge", 
           stat = "identity", 
           color = "black") +
  xlab("Race") +
  ylab("Count") +
  scale_x_discrete(label=c("Hispanice", "White", "Black","West", "Other"))

das %>%
  ggplot(aes(x = Region))+
  geom_bar(fill = "#97B3C6", color = "black")+
  theme_bw()+
  labs(x = NULL,
       y = NULL,
       title = NULL)

das %>%
  ggplot(aes(x = Edu8))+
  geom_bar(fill = "#97B3C6", color = "black")+
  theme_bw()+
  labs(x = NULL,
       y = NULL,
       title = NULL)

das %>%
  ggplot(aes(x = Income))+
  geom_bar(fill = "#97B3C6", color = "black")+
  theme_bw()+
  labs(x = NULL,
       y = NULL,
       title = NULL)

table(das) #Background x Geographical region
(df_tbl <- as.data.frame(table(das)))

# Background split by geographical region
ggplot(data = df_tbl) +
  geom_bar(mapping = aes(x = Region, y = Freq, fill =  Race5), 
           position = "dodge", #it is next to each other 
           stat = "identity", 
           color = "black") +
  xlab("Regions of US") +
  ylab("Freq") +
  scale_x_discrete(label=c("Northeast", "Midwest", "South", "Other")) +
  scale_fill_manual(name = "RACE", 
                    label=c("Hispanic", "White", "Black", "West", "Other"),
                    values=c("chocolate4", "palevioletred1", "papayawhip", "Chocolate")) +
  theme(axis.text.x = element_text(size = 10))

#scale_x_discrete changes x axis 
#scale_fill_manual you can change the colors and find them online 
# Birth region split by gender
ggplot(data = df_tbl) +
  geom_bar(mapping = aes(x = Race5, y = Freq, fill = Race5), 
           position = "dodge", 
           stat = "identity", 
           color = "black") +
  xlab("Birth region in US") +
  ylab("Freq") +
  scale_x_discrete(label=c("North America and Western Europe", "Latin America and the Caribbean", "Other")) +
  scale_fill_discrete(name = "Gender", label=c("Male", "Female")) +
  theme(axis.text.x = element_text(size = 10))
