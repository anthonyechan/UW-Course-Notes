#This code takes downloaded stock data from freestockcharts.com and measures how well
# it compares to the Normal distribution. Comparisons include numerical summaries
# (IQR, skewness, kurtosis) and graphical summaries (histogram vs Normal PDF, 
# empirical CDF vs Normal CDF, QQPlot)

#Run the following once
library(readr)
install.packages("moments")
library(moments)
library(MASS)

#Enter directory of file to be imported
#Use this for MAC:
rawfile <- "~/OneDrive/Trading/KWEB Weekly.txt"
#Use this for Windows:
#rawfile <- "C:/Users/penta/OneDrive/Trading/KWEB Weekly.txt"

#Imports file into R
#If error, run the following line on its own before the rest of the code
KWEB_Weekly <- read_csv(rawfile, col_types = cols(High = col_skip(), Low = col_skip(), Open = col_skip()))

#Displays graph
#View(KWEB_Weekly)

#Generate normal distribution
snorm <- rnorm(length(KWEB_Weekly$Close), mean(KWEB_Weekly$Close), sd(KWEB_Weekly$Close))

#Numerical summaries

#IQR
IQR_i <- IQR(KWEB_Weekly$Close)
IQR_norm <- 1.349*sd(snorm)

#Skewness
skew_i <- skewness(KWEB_Weekly$Close)
skew_norm <- 0

#Kurtosis
kurt_i <- kurtosis(KWEB_Weekly$Close)
kurt_norm <- 3

#Graphical summaries

#Hist vs pdf
truehist(KWEB_Weekly$Close, main="Relative Frequency Histogram of KWEB (Weekly)")
curve(dnorm(x, mean(KWEB_Weekly$Close), sd(KWEB_Weekly$Close)),col="red",add=TRUE,lwd=2)

#ECDF vs CDF
plot(ecdf(KWEB_Weekly$Close),verticals=T,do.points=F,xlab="y",ylab="ecdf",main="")
title(main="Empirical and Gaussian C.D.F.'s")
curve(pnorm(x, mean(KWEB_Weekly$Close), sd(KWEB_Weekly$Close)),col="red",add=TRUE,lwd=2)

#QQPlot
qqnorm(KWEB_Weekly$Close)
qqline(KWEB_Weekly$Close)