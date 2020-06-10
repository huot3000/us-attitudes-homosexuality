#######################
#
# MY459
# Problem Set 2
#
# Philippe Huot - 201320195
#
#######################

## Cleaning

#Clears environment
rm(list=ls())
      
#Clears plots
dev.off()
      
#Clears console
cat("\014")

######################
# Set-up and Tutorial
######################

### Preliminaries

#Setting the wd

setwd("C:/Users/Phil/Documents/École/LSE - LT14/MY459")

#Getting the data

library(foreign)
data <- read.dta ("GSS7212_R3_Extract.dta")

#Having a look at the data

summary(data)
str(data)
head(data)

#Removing obs with missing vars

data2 <- data[ rowSums ( is.na (data)) == 0,]

#Create a birth year var

data2$birthyear <- data2$year - data2$age

#Recoding the homosex data 

data2$wrong <- as.numeric (data2$homosex == "always wrong" | data2$homosex == "almst always wrg")

#Checking out if it worked

print(table(data2$homosex,data2$wrong))

#Creating a data set with babyboomers only

data3 <- data2[data2$birthyear >= 1946 & data2$birthyear <= 1955 ,]

############################
#Writing a Kernel regression function
############################

weighted.mean <- function (x, w) sum (x * w)/ sum (w)
rmse <- function (fitted,observed) sqrt ( mean ((observed-fitted)^2))
kreg <- function (x,y,bw,kernel = "uniform"){
	output <- list (x=x,y=y,bw=bw,kernel=kernel)
	class (output) <- 'kreg'

#Specify kernel function

if (kernel == "uniform") kernelf <-
function (xdiff) dunif (xdiff,min=-bw,max=bw)

#Define function to calculate a single fitted value
fittedf <- function (xfitted) weighted.mean (y, kernelf (xfitted - x))

#Apply fitted value function to each observation and calculate RMSE

output$fitted <- sapply (x,fittedf)
output$rmse <- rmse (output$fitted,output$y)
return (output)

}

#Checking my function

kreg5.fit <- kreg(data3$year, data3$wrong, bw=5)
print(str(kreg5.fit))

#Testing another bandwidth

kreg100.fit <- kreg(data3$year, data3$wrong, bw=100)
print ( sd (kreg100.fit$fitted))

kreg5.fit <- kreg(data3$year, data3$wrong, bw=5)
print(sd(kreg5.fit$fitted))

#Examining an example

par(mfrow=c(1,2)) # Creates grid of plots with 1 row and 2 columns

kreg2.fit <- kreg(data3$year, data3$wrong, bw=2)
plot(kreg2.fit$x, kreg2.fit$fitted, ylim=c(0,1), main="bw = 2")

kreg10.fit <- kreg(data3$year, data3$wrong, bw=10)
plot(kreg10.fit$x, kreg10.fit$fitted, ylim=c(0,1), main="bw = 10")

#Defining a S3 Class

class(output) <- 'kreg'

plot.kreg <- function (kreg,...){
plotorder <- order (kreg$x)
plot (kreg$x[plotorder],kreg$fitted[plotorder],...)
}

plot.kreg (kreg5.fit)


plot.kreg(kreg5.fit, type="l",
xlab='Survey Year',
ylab='Fraction Saying Homosexuality is Wrong'
) 

print.kreg <- function (kreg.fit){
print ('Kernel Regression',quote=FALSE)
print ( paste ('N:', length (kreg.fit$x)),quote=FALSE)
print ( paste ('Bandwidth:',kreg.fit$bw),quote=FALSE)
print ( paste ('RMSE:',kreg.fit$rmse),quote=FALSE)
}

print.kreg

#Comparing bandwidths

print(kreg2.fit)

print(kreg5.fit)

print(kreg10.fit)

print(kreg100.fit)

#Visually compare the kregs with different bandwidths

par (mfrow= c (2,2))
plot (kreg2.fit,ylim= c (0.5,0.8),type="l",xlab="Survey Year",ylab="")
plot (kreg5.fit,ylim= c (0.5,0.8),type="l",xlab="Survey Year",ylab="")
plot (kreg10.fit,ylim= c (0.5,0.8),type="l",xlab="Survey Year",ylab="")
plot (kreg100.fit,ylim= c (0.5,0.8),type="l",xlab="Survey Year",ylab="")

###################
# Question 1
###################

#Creating a fitted method and a residual method for the kreg class

#Fitted method

fitted <- function(xfitted) mean(y[abs(xfitted - x) <= bw])
fitted.kreg <- function(kreg) weighted.mean(kernelf(xfitted - x))

#Residual method

resid.kreg <- function(kreg) mean(kreg - fitted.kreg)

###################
# Question 2
###################

#Creating a lines method

?lines 

#Mmhhmm... I played around with lines and I think I am not able to make the
#desired plot. I'll use the plot command and par(new=T).

plot.new()
plot (kreg2.fit,ylim= c (0.5,0.8),type="l",xlab="Survey Year",ylab="", col="blue")
par(new=T)
plot (kreg5.fit, axes=FALSE, ylim= c (0.5,0.8),type="l",xlab="",ylab="", col="red")
par(new=T)
plot (kreg10.fit, axes=FALSE, ylim= c (0.5,0.8),type="l",xlab="",ylab="", col="brown")
par(new=T)
plot (kreg100.fit, axes=FALSE, ylim= c (0.5,0.8),type="l",xlab="",ylab="", col="orange")
par(new=T)

legend("topright", c("bw=2","bw=5","bw=10","bw=100"), lty=c(1,1,1,1), lwd=c(1,1,1,1),col=c("blue","red","brown","orange"))

#I know the legend is over one of the lines... Not ideal. 
#How can I make the plot size bigger so the legend doesn't overlap? Googling a bit didn't yield an answer.


###################
# Question 3
###################

#Add a normal kernel option to my kernel regression function

#Getting to know dnorm a bit more

?dnorm
plot(dunif(seq(-4,4,0.1),-1,1))
plot(dnorm(seq(-4,4,0.1),-1,1))

#Adding the option

weighted.mean <- function(x, w) sum(x*w)/sum(w)
kreg.normal <- function(x,y,bw,kernel = "normal"){
output <- list(x=x,y=y,bw=bw,kernel=kernel)
class(output) <- 'kreg'

#Specify a normal kernel function
if (kernel == "normal") kernelf <-
function(xdiff)dnorm(xdiff, mean = 0, sd = bw/(2*qnorm(0.75)), log = FALSE)

#Define function to calculate a single fitted value
fittedf <- function(xfitted)weighted.mean(y, kernelf(xfitted - x))

#Apply fitted value function to each observation
output$fitted <- sapply(x, fittedf)

return(output)
}

#Comparing plots of both kernels with a bw of 5

plot.kreg(kreg5.fit, ylim= c (0.5,0.8),type="l",xlab="Survey Year",ylab="", main="Uniform Kernel")

kreg5.normal.fit <- kreg.normal(data3$year, data3$wrong, bw=5)
print(str(kreg5.normal.fit))

plot.kreg(kreg5.normal.fit, ylim= c (0.5,0.8),type="l",xlab="Survey Year",ylab="", main="Normal Kernel")

#Two overlapped plots

plot.kreg(kreg5.fit, ylim= c (0.5,0.8),type="l",xlab="Survey Year",ylab="", col="red")
par(new=T)
plot.kreg(kreg5.normal.fit, axes=FALSE, ylim= c (0.5,0.8),type="l",xlab="Survey Year",ylab="", col="blue")
legend("topright",legend=c("Uniform", "Normal"),col=c("red", "blue"),lty=(1))
par(new=F)


###################
# Question 4
###################

#Modifying the kreg function so that it still works if some of the elements of y are missing

#Create a new data frame to mess with

data4 <- data3

#Decimate the outcome variable with missing data

data4$wrong[seq(1,nrow(data4),10)] <- NA 

#Make a kernel that works if some of the elements of y are missing

weighted.mean <- function (x, w) sum (x * w)/ sum (w)
rmse <- function (fitted,observed) sqrt ( mean ((observed-fitted)^2))
kreg <- function (x,y,bw,kernel = "uniform"){
output <- list (x=x,y=y,bw=bw,kernel=kernel)
class (output) <- 'kreg'

#Specify kernel function

if (kernel == "uniform") kernelf <-
function (xdiff) dunif (xdiff,min=-bw,max=bw)

#Define function to calculate a single fitted value

fittedf <- function (xfitted) weighted.mean (y, kernelf (xfitted - x), na.rm=TRUE)

}

#Checking my function

kreg5.fit.missing <- kreg(data4$year, data4$wrong, bw=5)
print(str(kreg5.fit.missing))

#Not sure if it worked .... Have a hard time interpreting what comes out of print.

###################
# Question 5
###################

#If I calculated a uniform kernel regression with a bw of less than one year,
#the estimates would correspond to actual values collected for each year (same as using a one year bw).


###################
# Question 6
###################

#The RMSEs get smaller as we decrease the bandwidth as we the fitted values become closer to the actual data.
#Using RMSEs to choose a bandwidth is not ideal. Although minimizing the RMSEs allows to minimize variance and bias 
#in sample (lowest RMSE would be attained by "connecting the dots") - this has no value if we want
#an idea of how our model predicts out of sample, or if we want a summary of the relationship in the data. 

###################
# Question 7
###################

#Compare the 1956 to 1964 cohort with the 1946 to 1955 cohort.

plot.new()
with(data3[(data3$birthyear < 1965) & (data3$birthyear > 1955),], plot.kreg(kreg5.normal.fit, ylim= c (0.5,0.8),type="l",xlab="Survey Year",ylab="")
with(data3[(data3$birthyear < 1956) & (data3$birthyear > 1945),], plot.kreg(kreg5.normal.fit, ylim= c (0.5,0.8),type="l",xlab="Survey Year",ylab="")

#The plots are not appearing ... A stackoverflow post suggested this method. 
#I must be getting something wrong. Error in code?


###################
# Question 8
###################

#Based on "looks" only - and everything else being equal - the normal kernel seems to be a better description
# of the trends as it smooths the trend a bit more than the uniform kernel does.
#Regarding the bandwidth, I would say that the bw of size 5 and 10 are the best at describing the trend.
#bw=2 yields fitted value that are a bit too much irregular, and bw=100 is simply a mean 
#of all observations. I would say that bw=5 is slightly better, as we keep a little more information
#at the beginning of the observed time period. The bw=5 is also a bit more sensible to fluctuations and allows
#us to see that a below-average trend at the beginning of the period - something bw=10 doesn't capture. 


