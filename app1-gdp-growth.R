## ------------------------------------------------
## Title  : GDP-growth.r
## Authors: Oka and Qu 
## Date   : 12/27/2010
## Updated: 01/28/2013
## ------------------------------------------------

## Note: 
## This code applies the procedure developed in
## "Estimating Structural Changes in Regression Quantiles" to analyze the GDP 
## growth example considered in the paper.
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
DATA = as.matrix(read.csv("realGDP.txt", header=F)) #<== Full sample: 1947:2 - 2009:2

DATA = 4 * DATA
nn   = length(DATA)
##plot(DATA, type = 'l')

## two lags are included as regressors
n.lag = 2               
y = DATA[(n.lag+1):nn]  #dependent variable

x = matrix(0, (nn - n.lag), n.lag) #indepdent variable
for(i in 1:n.lag){
  x[,i] = DATA[(n.lag+1-i):(nn-i)]
}

## quantiles
vec.tau = seq(0.20, 0.80, by = 0.150)
##vec.tau = seq(0.20, 0.80, by = 0.075)  ## a finer grid to check robustness

## DQ test
q.L = min(vec.tau) 
q.R = max(vec.tau) 

## the maximum number of breaks allowed
m.max = 3

## the signifance level for sequenatial testing
## 1, 2 or 3 for 10%, 5% or 1%, respectively
v.a = 2 

## the significance level for the confidence intervals of
## estimated break dates.
## 1 or 2 for 90% and 95%, respectively.
v.b = 2

## the sample size of cross-section
N.size = 1

## the trimming proportion for estimating the break dates (used to exclude 
## the boundaries of the sample)
trim.e = 0.15

## Global varible for the DQ test. Set d.Sym=TRUE if the trimming 
## for vec.tau is symmetric: i.e, q.L = (1 - q.R); Otherwise, set d.Sym=FALSE
d.Sym = TRUE    

## If d.Sym = FALSE, then please set d.Sim = TRUE for the first-time use.
## The simulation takes a while depdeing on the computer you use.  
d.Sim = FALSE


## the time index
vec.time = seq(1947, 2009, 1) %x% matrix(1, 4, 1)
vec.time = vec.time + 0.1 * matrix(c(1, 2, 3, 4), length(vec.time), 1)
vec.time = vec.time[2:(nn+1)]
if( min(vec.time) != 1947.2 ){
  stop("Wrong Begining")
}
if( max(vec.time) != 2009.2){
  stop("Wrong End")
}
vec.time = vec.time[(n.lag+1):nn]




#---------------------------------------------
## You should not have to modify the following
#---------------------------------------------
## global variable 
eps = .Machine$double.eps ^ (2/3) 
options(warn=-1)
## ====== Main Analysis ======
rq.break(y, x, vec.tau, N.size, trim.e, q.L, q.R, vec.time, m.max, v.a, v.b)
