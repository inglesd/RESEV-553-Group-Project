#Install required packages ####
library(tidyverse)
library(psych)
library(dplyr)
library(ggplot2)

#Import the dataset


#Prepare the dataset for analysis ####

df <- subset(prgusap1_puf, select = c(RACETHN_5CAT, REGION_US, EDCAT8, J_Q09USX))

str(df)
summary(df) #summary of the two variables 

table(df)
describe() #skew kurtosis

table(df) #Background x Geographical region
(df_tbl <- as.data.frame(table(df)))

# Background split by geographical region
ggplot(data = df_tbl) +
  geom_bar(mapping = aes(x = REGION_US, y = Freq, fill =  RACETHN_4CAT), 
           position = "dodge", #it is next to each other 
           stat = "identity", 
           color = "black") +
  xlab("REGION_US") +
  ylab("Freq") +
  scale_x_discrete(label=c("Northeast", "Midwest", "South", "Other")) +
  scale_fill_manual(name = "RACE", 
                    label=c("Hispanic", "White", "Black", "West"),
                    values=c("chocolate4", "palevioletred1", "papayawhip", "Chocolate")) +
  theme(axis.text.x = element_text(size = 10))

#scale_x_discrete changes x axis 
#scale_fill_manual you can change the colors and find them online 
# Birth region split by gender
ggplot(data = df_tbl) +
  geom_bar(mapping = aes(x = GENDER_R, y = Freq, fill = BIRTHRGNUS_C), 
           position = "dodge", 
           stat = "identity", 
           color = "black") +
  xlab("Birth region in US") +
  ylab("Freq") +
  scale_x_discrete(label=c("North America and Western Europe", "Latin America and the Caribbean", "Other")) +
  scale_fill_discrete(name = "Gender", label=c("Male", "Female")) +
  theme(axis.text.x = element_text(size = 10))
