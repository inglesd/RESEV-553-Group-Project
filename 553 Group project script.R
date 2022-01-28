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

#Find the mode
MODE = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

Race5_mode <- MODE(as$Race5)
Region_mode <- MODE(as$Region)
Edu8_mode <- MODE(as$Edu8)
Income_mode <- MODE(as$Income)

#Categorical Data Visualization
das %>%
  ggplot(aes(x = Race5))+
  geom_bar(fill = "#97B3C6", color = "black")+
  theme_bw()+
  labs(x = NULL,
       y = NULL,
       title = NULL)

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

das$Race5 <- as.numeric(das$Race5)

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
