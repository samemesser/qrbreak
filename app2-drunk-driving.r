## ------------------------------------------------
## Title  : drunk_driving.r
## Authors: Oka and Qu 
## First version: 12/27/2010
## Updated: Jan 28, 2013
## ------------------------------------------------

## Note: 
## This code applies the procedure developed in
## "Estimating Structural Changes in Regression Quantiles" to 
## analyze the drunk driving example in the paper.
## Please be advised that this code uses an R-package, quantreg,
## developed by Roger Koenker. If you have not installed this package,  
## please type the following command before executing our procedure: 
## install.packages('quantreg')

## clear all existing objects
rm(list=ls(all=TRUE))

##----------------------------
## Set up the model
##-----------------------------
## library and source files
library(quantreg)
library(QR.break)

## read data
DATA = read.table("driver2.csv", header=T)
YQ  = DATA[,1:2]
age = DATA[,4]
sex = DATA[,5]                     ## 1 = male/0 = female
time.YQ = YQ[,1] + 0.1 * YQ[,2]
d.Q4 = 1 * (YQ[,2] == 4)           ## the dummy for winter 

#the explanatory variables
x = cbind(age, sex, d.Q4)

#the dependant variable 
y   = DATA[,7] / 100      

# the qunatiles of interest
vec.tau = seq(0.70, 0.85, 0.025)

## DQ test
q.L = min(vec.tau) 
q.R = max(vec.tau) 

## Global varible for the DQ test. Set d.Sym=TRUE if the trimming 
## for vec.tau is symmetric: i.e, q.L = (1 - q.R); Otherwise, set d.Sym=FALSE

d.Sym = FALSE

## If d.Sym = FALSE, then the critical values needs to be obtained via simulations.
## Then, you need to set d.Sim = TRUE.
## The simulation takes a while depending on the computer you use.  
d.Sim = TRUE

## the maximum number of breaks allowed
m.max = 3

## the signifance level for sequenatial testing
## 1, 2 or 3 for 10%, 5% or 1%, respectively
v.a = 2 

## the significance level for the confidence intervals of
## estimated break dates.
## 1 or 2 for 90% and 95%, respectively.
v.b = 2

## the cross-sectional sample size
N.size = 108          

## the trimming proportion when estimating the break dates
trim.e = 0.05 

## the index for time 
mat.time = matrix(time.YQ, N.size, (length(y)/N.size) )
vec.time = mat.time[1,]


#---------------------------------------------
## You should not have to modify the following
#---------------------------------------------
## global variable 
eps = .Machine$double.eps ^ (2/3) 
options(warn=-1)

##===== The Main Analysis ======
rq.break(y, x, vec.tau, N.size, trim.e, q.L, q.R, vec.time, m.max, v.a, v.b)


