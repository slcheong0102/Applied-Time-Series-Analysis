# ----- Setup -----
rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lmtest)

url="https://mcs.utm.utoronto.ca/~nosedal/data/Fresh2.txt"

detergent=read.table(file=url, header=TRUE);

# ----- Question 1: Make a scatter plot of the Y vs X3 -----
Q1 = lm(detergent$Y~detergent$X3)
plot(detergent$X3, detergent$Y, main = "Scatterplot between Y vs X3", 
     xlab = "X3", ylab = "Y", pch = 16, col = "blue")
abline(Q1, col="red")
cor(detergent$Y,detergent$X3)

# ----- Question 2: Make a scatter plot of the Y vs X4 -----
Q2 = lm(detergent$Y~detergent$X4)
plot(detergent$X4, detergent$Y, main = "Scatterplot between Y vs X4", 
     xlab = "X4", ylab = "Y", pch = 16, col = "blue")
abline(Q2, col="red")
cor(detergent$Y,detergent$X4)


# ----- Question 3: Make a residual plot of the Residuals vs X3 -----
ols1=lm(detergent$Y~detergent$X4+detergent$X3)
res1 = ols1$res
plot(detergent$X3, res1, main = "Scatterplot between Residuals vs X3", 
     xlab = "X3", ylab = "res1", pch = 16, col = "black")
abline(h=mean(res1),lty=2,col="red")
cor(res1,detergent$X3)


# ----- Question 4: Make a residual plot of the Residuals vs X4 -----
plot(detergent$X4, res1, main = "Scatterplot between Residuals vs X4", 
     xlab = "X4", ylab = "res1", pch = 16, col = "black")
abline(h=mean(res1),lty=2,col="red")
cor(res1,detergent$X4)


# ----- Question 5: Make a residual plot of the Residuals vs Y_hat -----
B0 = ols1$coeff[1]
B1 = ols1$coeff[2]
B2 = ols1$coeff[3]
Y_hat = B0 + B1*detergent$X4 + B2*detergent$X3
plot(Y_hat, res1, main = "Scatterplot between Residuals vs Y_hat", 
     xlab = "Y_hat", ylab = "res1", pch = 16, col = "blue")
abline(h=mean(res1),lty=2,col="red")
cor(Y_hat,res1)


# ----- Question 6: Compute the correlation between e_t and e_t-1 -----
ols2 = lm(detergent$Y~detergent$X4+detergent$X3+(detergent$X3)^2)
res2 = ols2$res
n = length(res2)
res2_f = res2[-1]
res2_b = res2[-n]
plot(res2_f, res2_b, main = "Scatterplot between res2_f vs res2_b", 
     xlab = "res2_f", ylab = "res2_b", pch = 16, col = "red")
cor(res2_f,res2_b)
round(cor(res2_f,res2_b),4)


# ----- Question 7: Durbin-Watson Test -----
dwtest(ols2)


# ----- Question 8: Is there positive autocorrelation -----
# No, we fail to reject Null



