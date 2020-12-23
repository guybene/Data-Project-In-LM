library(ggplot2)
library(dplyr)
#Load the data
df <- read.csv("C:\\Users\\ADMIN\\Desktop\\Survey_Data.csv")

#Cleaning the data. Starting with dropping NA lines, 2 lines to be dropped
df <- na.omit(df)

#Some data was inserted at a 1/10 format, for example 102.3 becomes 10.23.
#We assume that data between 7.5 and 12.5 is a format problem
wrong_format_data <-df$School_Grades[df$School_Grades>7.5 & df$School_Grades<12.5]
df$School_Grades[df$School_Grades>7.5 & df$School_Grades<12.5]<-wrong_format_data*10

#Testing that all the data has valid entry
df<-df[(80<df$School_Grades & df$School_Grades<130) & df$Uni_Grades<101,]
x <- df$School_Grades
mean_x <- mean(x)
var_x <- var(x)

y <- df$Uni_Grades
mean_y <- mean(y)
var_y <- var(y)


#We assume that both variables distribute normally so we want obtain a CI with alpha = 0.05
alpha <- 0.05
n <- length(df$School_Grades)

parameter_school <- (qt(1-alpha/2,n-1))*((var(x)/n))^0.5
parameter_uni <- (qt(1-alpha/2,n-1))*((var(y)/n))^0.5

CI_school <- c(mean(x) - parameter_school, mean(x) + parameter_school)
CI_uni <- c(mean(y) - parameter_uni, mean(y) + parameter_uni)


#Histograms and graphs for the data
ggplot(df,aes(x=School_Grades)) +
  geom_histogram(aes(y = ..density..),binwidth = 2,fill='blue',col='red') +
  stat_function(fun = dnorm, args = list(mean=mean(x), sd = sd(x)),size=2) +
  ggtitle("School Grades Histogram with estimated normal distribution") + 
  xlab("School Grades") + geom_vline(xintercept = mean(x),size = 2, col = 'orange')

ggplot(df,aes(x=Uni_Grades)) + 
  geom_histogram(aes(y = ..density..),binwidth = 4,fill='violet',col='green') +
  stat_function(fun = dnorm, args = list(mean=mean(y), sd = sd(y)),size=2) +
  ggtitle("University Grades Histogram with estimaed normal distribution") + 
  xlab("University Grades") + geom_vline(xintercept = mean(y),size = 2, col = 'brown')

#We want to check for that the tracks are not a relevant parameter
df_without_mistaken_track <- df[df$Study_Track != "",]
ggplot(df_without_mistaken_track,aes(x=Study_Track,y=Uni_Grades)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  ggtitle("University Grades Box Plot") + xlab("Study Track") + ylab("University Grades")

#Cleaning the saved variables
rm(list = c("parameter_school","parameter_uni","wrong_format_data","n","alpha"))

#Calculate linear regression line
coeff <- coefficients(lm(y~x))
linear_r <- coeff[2]* (sd(x)/sd(y))

#Plotting regression line on a scatter plot
ggplot(df,aes(x=School_Grades,y=Uni_Grades)) + geom_point() +
  geom_smooth(method = lm, se=FALSE) +
  ggtitle("Linear regression of university grades as a function of school grades") +
  xlab("School Grades") + ylab("University Grades")

#Plotting regression line on a scatter plot, without outliers
qunt <- quantile(x,c(0.05,0.95))
df_outlier <- df[df$School_Grades>=qunt[1] & df$School_Grades<= qunt[2] &
                   df$Uni_Grades>59,]
coeff_outlier <- coefficients(lm(df_outlier$Uni_Grades~df_outlier$School_Grades)) 

#Regression in sliced data to see local trends, slicing at the mean
sliced_df <-  mutate(df,Group = case_when(
  School_Grades < mean(x) ~ 'Grades Lower than the mean', 
  School_Grades >= mean(x) ~ 'Grades Over the mean'))
ggplot(sliced_df, aes(x = x,y = y)) + geom_point(size = 0.8, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) + facet_grid(. ~ Group) +
  ggtitle("Linear regression of university grades as a function of school grades
          with the data split at the mean") +
  xlab("School Grades") + ylab("University Grades")

df_over_mean <- df[sliced_df$Group == 'Grades Over the mean', ]
df_under_mean <- df[sliced_df$Group != 'Grades Over the mean', ]

coeff_over_mean <- coefficients(lm(df_over_mean$Uni_Grades~df_over_mean$School_Grades))
high_x <- sliced_df[sliced_df$Group == 'Grades Over the mean','School_Grades']
high_y <- sliced_df[sliced_df$Group == 'Grades Over the mean','Uni_Grades']
linear_r_high <- coeff_over_mean[2]* (sd(high_x)/sd(high_y))


coeff_under_mean<- coefficients(lm(df_under_mean$Uni_Grades~df_under_mean$School_Grades))
low_x <- sliced_df[sliced_df$Group != 'Grades Over the mean','School_Grades']
low_y <- sliced_df[sliced_df$Group != 'Grades Over the mean','Uni_Grades']
linear_r_low <- coeff_under_mean[2]* (sd(low_x)/sd(low_y))
