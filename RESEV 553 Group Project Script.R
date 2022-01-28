#test number 3
#more changes n stuff 
#Changes to stuffffffff
#step 1: install packages ####
#install tidyverse and psyche packages

#install.packages("psych") 
library(psych)

#install.packages("tidyverse") 
library(tidyverse)

#install.packages("haven")
library(haven)


#step 2: Read data file ####

prgusap1_puf <- read_sas("prgusap1_puf.sas7bdat", 
                         NULL)

View(prgusap1_puf)

df <- subset(prgusap1_puf, select = c(RACETHN_4CAT, REGION_US, GENDER_R, D_Q18B,
                                      J_Q04A, BIRTHRGNUS_C, EDCAT8, J_Q09USX))

#data overview 

str(df)

summary(df)

table(df$RACETHN_4CAT)
table(df$REGION_US)
table(df$GENDER_R)
table(df$D_Q18B)
table(df$J_Q04A)
table(df$BIRTHRGNUS_C)
table(df$EDCAT8)

describe(df) 

#Range
#applicable variables? 

#max(df$salary)-min(df$salary)

#find the mode 

#first option, view the table (manual)

table(df$nsatisfy)
sort(table(df$nsatisfy), decreasing=TRUE)
View(table(df$nsatisfy))


#second option, write a function

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

df_mode <- MODE(df$nsatisfy)

#third option, use a function already in a package- this only tells us that it
#is numeric 

mode(df$nsatisfy)

#assign variables as categorical 

df$ward <- as.factor(df$ward)
str(df)
summary(df)

#calculate the sd for a continuous variable "nsatisfy"

df_nsatisfy_sd <- sd(df$nsatisfy)

#to view sd "nsatisfy" in the console

df_nsatisfy_sd

#to calculate the variance, co-variance matrix:

var(df)

#to look at the variance for one variable in the matrix:

var(df$nsatisfy)

#squaring the sd of "nsatisfy"

df_nsatisfy_sd^2

#Step 3a: Data Visualization (Continuous)####
#first, view a simple histogram of continuous data:

hist(df$nsatisfy)

#customize graph attributes

hist(df$nsatisfy,
     main = NULL, 
     xlab = "Nursing Care Satisfation Score",
     ylab = "Frequency")

#separating the data into subsets for visualization

df_ward1 <- df[df$ward=="1",]
df_ward2 <- df[df$ward=="2",]

#analyzing differences in the three data frames
#1) full summary

summary(df$ward)

#2) ward 1 summary

summary(df_ward1$nsatisfy)

max(df_ward1$nsatisfy)-min(df_ward1$nsatisfy)

#3 ward 2 summary

summary(df_ward2$nsatisfy)

max(df_ward2$nsatisfy)-min(df_ward2$nsatisfy)

#calculate the sd for a continuous variable "nsatisfy" in ward 1

df_nsatisfy_ward1_sd <- sd(df_ward1$nsatisfy)

#to view sd "nsatisfy" in the console

df_nsatisfy_ward1_sd

#calculate the sd for a continuous variable "nsatisfy" in ward 2

df_nsatisfy_ward2_sd <- sd(df_ward2$nsatisfy)

#to view sd "nsatisfy" in the console

df_nsatisfy_ward2_sd

#mode for ward 1 

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

df_mode_ward1 <- MODE(df_ward1$nsatisfy)

df_mode_ward2 <-MODE(df_ward2$nsatisfy)

#creating new graphs separated by categorical data:

hist(df_ward1$nsatisfy,
     main = "Histogram of Nursing Care Satisfaction for Ward 1",
     xlab = "Nursing Care Satisfaction Score",
     ylab = "Frequency")

hist(df_ward2$nsatisfy,
     main = "Histogram of Nursing Care Satisfaction for Ward 2",
     xlab = "Nursing Care Satisfcation Score",
     ylab = "Frequency")

#graphing in ggplot

library(ggplot2)

#basic histogram in ggplot

ggplot(data =df) +
  geom_histogram(mapping = aes(x=nsatisfy))

#customize in ggplot

ggplot(data = df) +
  geom_histogram(mapping = aes(x = nsatisfy, fill = ward), 
                 color = "black", 
                 binwidth = 1, 
                 alpha = 0.6) +
  scale_fill_manual(values=c("orangered1", "cyan3")) +
  ggtitle("Histogram for Nursing Care Satisfaction") +
  xlab ("Nursing Care Satisfaction Score") +
  ylab ("Frequency") +
  theme_bw()

#overlaying different types of plots and subsets of data

ggplot(data = df) +
  geom_histogram(mapping = aes(x = nsatisfy, y = ..density..),
                 fill = "darkgrey",
                 color = "black",
                 binwidth = 1) +
  geom_density(mapping = aes(x= nsatisfy, color = ward),
               alpha = 0.4, 
               cex = 2) +
  ggtitle ("Histogram for Nursing Care Satisfaction") +
  xlab ("Nursing Care Satsifaction Score") +
  ylab ("Frequency")

#creating grids of the same graph with individual groups of data

ggplot (data = df) +
  geom_histogram(mapping =aes(x= nsatisfy, y=..density.., fill = ward),
                 color = "black",
                 bins = 8) +
  geom_density(mapping = aes (x = nsatisfy)) +
  facet_wrap(~ward, ncol=1) +
  ggtitle (NULL)+
  xlab ("Nursing Care Satsifaction Score") +
  ylab ("Frequency")


#Step 3b: Data Visualization (categorical) ####

#simple bar chart of continuous data

barplot(table(df$ward), 
        main = NULL,
        xlab = "Ward",
        ylab = "Count")

#improved bar chart in ggplot

ggplot(data = df) +
  geom_bar(mapping = aes (x = ward,
                          fill = ward),
           width = 0.5,
           color = "black") +
  ggtitle (NULL) +
  xlab ("Ward") +
  ylab ("Count") +
  theme(legend.position = "none")

# Step 3c: Data Visualization (Saving)

#fonts seen in the "Plot" window preview do not automatically re-size when changing the size of the image

#two main options

#1 Save graph in code to a png, jpeg, etc

png(filename = "Output/Lab1_ward_barchart.png")
ggplot(data = df) +
  geom_bar(mapping = aes (x = ward,
                          fill = ward),
           width = 0.5,
           color = "black") +
  ggtitle ("Distribution of Ward") +
  xlab ("Ward") +
  ylab ("Count") +
  theme(legend.position = "none")
dev.off()

#2) 'Export' file in the 'Plots' window
#
#     i) Click on 'Export' while in the 'Plots' tab.
#     ii) Change the location where the image will be saved.
#     iii) Change size of the image (preview available)

# (Option 3: Copy-and-paste from the preview window. Not as safe
# because if you make changes in script that aren't saved you 
# might forget how the graph was produced or which data were used
# to create the graph)



            