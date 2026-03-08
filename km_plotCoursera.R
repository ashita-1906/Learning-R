# Code changes for Ashita test branch
g <- read.csv(file = "C:/Users/aksha/Downloads/6AiBbg-BEem6Gg6vVM6M8A_e872b4600f8111e9b2f4133a1edfbb40_simulated-HF-mort-data-for-GMPH-_1K_-final-_2_.csv", header=TRUE, sep=',')

getwd()

install.packages("survival")
install.packages("ggplot")

library(survival) # this is the cornerstone command for survival analysis in R
library(ggplot2) # newer package that does nice plots

g
gender <- as.factor(g[,"gender"])
fu_time <- g[,"fu_time"] # continuous variable (numeric) 
death <- g[,"death"] # binary variable (numeric) 

km_fit <- survfit(Surv(fu_time,death)~1)

plot(km_fit)
summary(km_fit, times = c(1:7,30,60,90*(1:10))) 
#times is 1:7 means first 7 days data, then 30thday,60th day,90th day and then every 90 days thereafter

km_gender_fit <- survfit(Surv(fu_time,death)~gender)

plot(km_gender_fit)
survdiff(Surv(fu_time, death) ~ gender, rho=0) 

#plot for patients age over 65 and below 65

age_65plus<- ifelse(g[,"age"]>=65,1,0)

table(age_65plus,exclude=NULL)

age_65plus


survdiff(Surv(fu_time, death) ~ age_65plus, rho=0)
#p value is very less which means there is a statistical difference between 2 grps age<65 and age>65