## This file cleans data for R package. 

## clear all
rm(list=ls(all=TRUE))
rm(list=ls())

##-----------------------------------------
## driver 
##-----------------------------------------
DATA1 = read.table("./driver_original.csv", header=TRUE)
names(DATA1)

library(zoo)
yq       = as.yearqtr(paste0(DATA1[,1], "-", DATA1[,2]), format="%Y-%q")
bac      = DATA1[,7] / 100 ## the blood alcohol concentration
age      = DATA1[,4]
gender      = DATA1[,5]           ## 1 = male/0 = female
winter = 1 * (DATA1[,2] == 4)  ## the dummy for winter 

df1 = data.frame(yq, bac, age, gender, winter)
summary(df1)

## save 
write.csv(df1, file = "./driver.csv", row.names=FALSE)


##-----------------------------------------
## gdp  
##-----------------------------------------
DATA2 = read.csv("realGDP.txt", header=F) #<== Full sample: 1947:2 - 2009:2

y = 4 * DATA2$V1
nn  = length(y)
##plot(DATA, type = 'l')

## two lags are included as regressors
n.lag = 2               
gdp = y[(n.lag+1):nn]  #dependent variable

lag1 = y[(n.lag+1-1):(nn-1)]  ## x1 
lag2 = y[(n.lag+1-2):(nn-2)]  ## x2 

## the time index
vec.y = seq(1947, 2009, 1) %x% matrix(1, 4, 1)
vec.q = matrix(c(1, 2, 3, 4), length(vec.y), 1)
yq    = as.yearqtr(paste0(vec.y, "-", vec.q), format="%Y-%q")
yq    = yq[2:(nn+1)]  #<== Full sample: 1947:2 - 2009:2

## check 
if( min(yq) != "1947 Q2")  stop("Wrong Begining")
if( max(yq) != "2009 Q2")  stop("Wrong End")

yq    = yq[(n.lag+1):nn]

## data frame 
df2 = data.frame(yq, gdp, lag1, lag2)

## save 
write.csv(df2, file = "./gdp.csv", row.names=FALSE)


