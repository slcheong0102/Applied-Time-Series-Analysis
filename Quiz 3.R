# ----- Setup -----
rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lmtest)

data <- read.table("C:/Users/soulu/OneDrive - University of Toronto/University of Toronto/UTM 2021-2023/8_2024 Winter/STA457H5S/Quizzes and Tests/Quiz 3/home-sales.txt", header=TRUE)

# ----- Question 1: Make a time series plot of existing home sales -----
plot.ts(data$units,main="Q1: Time Series Plot")


# ----- Question 2: Make a lagged time series plot of existing home sales -----
n = length(data$units)
unitsf <- data$units[-1]
unitsb <- data$units[-n]
Q2 = lm(unitsf~unitsb)
plot(unitsb, unitsf, main = "Q2: Lagged Time Series Plot", 
     xlab = "Y_t-1", ylab = "Y_t", pch = 16, col = "blue")
abline(Q2, col="red")
cor(unitsb,unitsf)


# ----- Question 3: Make a correlogram -----
acf(data$units,lag=200,main="Q3: Correlogram")


# ----- Question 4: Find least square estimate of B0 -----
Q2$coeff
Q4 <- Q2$coeff[1]
round(Q4,4)


# ----- Question 5: Find least square estimate of B1 -----
Q5 <- Q2$coeff[2]
round(Q5,4)

# ----- Question 6: Forecast the sales for August 2001 -----
Q6 <- Q4 + Q5*data$units[n]
Q6


# ----- Question 7: De-trending the time series -----
Q7 <- lm(unitsf~unitsb+data$order[-n])
plot.ts(Q2$residuals)
plot.ts(Q7$residuals)
m1 <- rep(c(1,0,0,0,0,0,0,0,0,0,0,0),33)
m2 <- rep(c(0,1,0,0,0,0,0,0,0,0,0,0),33)
m3 <- rep(c(0,0,1,0,0,0,0,0,0,0,0,0),33)
m4 <- rep(c(0,0,0,1,0,0,0,0,0,0,0,0),33)
m5 <- rep(c(0,0,0,0,1,0,0,0,0,0,0,0),33)
m6 <- rep(c(0,0,0,0,0,1,0,0,0,0,0,0),33)
m7 <- rep(c(0,0,0,0,0,0,1,0,0,0,0,0),33)
m8 <- rep(c(0,0,0,0,0,0,0,1,0,0,0,0),33)
m9 <- rep(c(0,0,0,0,0,0,0,0,1,0,0,0),33)
m10 <- rep(c(0,0,0,0,0,0,0,0,0,1,0,0),33)
m11 <- rep(c(1,0,0,0,0,0,0,0,0,0,1,0),33)
m12 <- rep(c(1,0,0,0,0,0,0,0,0,0,0,1),33)




