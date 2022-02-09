#Install required packages ####
library(tidyverse)
library(psych)
library(dplyr)
library(ggplot2)
library(scales)
library(AggregateR)
library(plyr)

#Import the dataset ####
library(haven)
prgusap1_puf <- read_sas("prgusap1_puf.sas7bdat", NULL)

#Prepare the dataset for analysis ####

df <- subset(prgusap1_puf, select = c(RACETHN_5CAT, REGION_US, EDCAT8, J_Q09USX))

#Rename the variables
df$Race5 <- (df$RACETHN_5CAT)
df$Region <- (df$REGION_US)
df$Edu8 <-(df$EDCAT8)
df$Income <-(df$J_Q09USX)

#Dataset for the analysis ####
das <- subset(df, select = c(Race5, Region, Edu8, Income))

#Cleaning the dataset
# First identifying rows with NAs
rownames(das)[apply(das, 2, anyNA)]
#removing all observations with NAs
das_clean <- das %>% na.omit()


#Describing each variables ####
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

#Recoding Income variable ####
das_clean$Income <- as.factor(das_clean$Income)

das_clean$Income = revalue(das_clean$Income, c("1" = "1", "2" = "1", "3" = "2", "4" = "2", 
                                               "5" = "3", "6" = "3", "7" = "4", "8" = "5",
                                               "9" = "6", "10" = "7", "11" = "8"))

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
  scale_fill_discrete(name = "Household income in last 12 months", label=c("1 - Between $1 and $19,999", "2 - Between $20,000 and 39,999", "3 - Between $40,000 and $59,999", 
                                                        "4 - Between $60,000 and $74,999", "5 - Between $75,000 and $99,999", 
                                                        "6 - Between $100,000 and $149,999", "7 - $150,000 or more", "8 - No household income")) +
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



ggplot(data = df_tbl) +
  geom_bar(mapping = aes(x = Region, y = Freq, fill =  Income), 
           position = "dodge", 
           stat = "identity") +
  xlab("Region") +
  ylab("Income") +
  scale_x_discrete(label=c("Northeast", "Midwest", "South", "West")) +
  scale_y_continuous(labels = scales::percent(100, scale = 1)) +
  scale_fill_manual(name = "Household income", 
                    label=c("Between $1 and $9,999", "Between $10,000 and $19,999", "Between $20,000 and $29,999", 
                            "Between $30,000 and $39,999", "Between $40,000 and $49,999", 
                            "Between $50,000 and $59,999", "Between $60,000 and $74,999", "Between $75,000 and $99,999",
                            "Between $100,000 and $149,999", "$150,000 or more",
                            "No household income"),
                    values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#9999CC", "#66CC99")) +
  theme(axis.text.x = element_text(size = 10))

#trying to make the table percentages ####

# data_perc <- t(prop.table(table(das$Income))) * 100
# 
# barplot(data_perc, ylab = "Percent") 
# 
# library("scales")
# 
# ggplot(data = das) +
#   geom_bar(aes(y = (..count..)/sum(..count..), x = Income))
#   position = "dodge", 
#           stat = "identity") +
  # xlab("Region") +
  # ylab("Income") +
  # # scale_x_discrete(label=c("Northeast", "Midwest", "South", "West")) +
  # scale_y_continuous(labels = percent)+
  # scale_fill_manual(name = "Household income", 
  #                   label=c("Between $1 and $9,999", "Between $10,000 and $19,999", "Between $20,000 and $29,999", 
  #                           "Between $30,000 and $39,999", "Between $40,000 and $49,999", 
  #                           "Between $50,000 and $59,999", "Between $60,000 and $74,999", "Between $75,000 and $99,999",
  #                           "Between $100,000 and $149,999", "$150,000 or more",
  #                           "No household income"),
  #                   values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#9999CC", "#66CC99")) +
  # theme(axis.text.x = element_text(size = 10))

ggplot(das, aes(x = Region)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = proport)

# das_perc <- das %>%
#   group_by(Income) %>%
#   mutate(perc = 100 / sum (100)) %>%
#   as.data.frame()
# das_perc

# das_perc <- transform(data, 
#                       perc = ave (100,
#                                   Income,
#                                   FUN = prop.table))


das_Income <-subset(das, select = c(Income))

das_Region <- subset(das, select = c(Region))

ggplot(data = das) +
  geom_bar(mapping = aes(x = Region, y = Income, fill = Income),
           position = "dodge", 
           stat = "identity") +
  # xlab("Region") +
  # ylab("Income") +
  # scale_x_discrete(label=c("Northeast", "Midwest", "South", "West")) +
  # scale_y_continuous(labels = scales::percent(100, scale = 1)) +
  # scale_fill_manual(name = "Household income", 
  #                   label=c("Between $1 and $9,999", "Between $10,000 and $19,999", "Between $20,000 and $29,999", 
  #                           "Between $30,000 and $39,999", "Between $40,000 and $49,999", 
  #                           "Between $50,000 and $59,999", "Between $60,000 and $74,999", "Between $75,000 and $99,999",
  #                           "Between $100,000 and $149,999", "$150,000 or more",
  #                           "No household income"),
  #                   values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#9999CC", "#66CC99")) +
  theme(axis.text.x = element_text(size = 10))

# library
library(ggplot2)
library(viridis)
library(hrbrthemes)

# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

# Small multiple
ggplot(das, aes(fill=Income, y=Income, x=Region)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  #ggtitle("Studying 4 species..") +
  theme_ipsum() +
  xlab("")

ggplot(das_perc, aes(fill = perc, y=Income, x=Region)) + 
  geom_bar(position="fill", stat="identity")

perc_income <- table(das$Income)
prop.table(perc_income)


perc_incomebyregion <- table(das$Income, das$Region)
prop.table(perc_incomebyregion)

# reorder_size <- function(x) {
#   factor(x, levels = names(sort(table(x), decreasing = TRUE)))
# }

ggplot(data = perc_incomebyregion, aes(x = Region)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Region") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


das%>%group_by(Region)%>%mutate(Percentage=paste0(round(Income/sum(Income)
                                                        *100,2),"%"))
  