# Simulate Critical Values for SQ test for several specifications

m.max<-5 # number of breaks under the null hypothesis are [0,1,...,m.max]
p.max<-20 # maximum number of parameters allowed to change

#this program will create a text file of critical values at 1,5, and 10 percent levels.

#Load functions
source("CV.R")

#Change to critical value folder
setwd("./crit_vals")

for (parm.chg in 1:1) {
  cat("Function Call: CV(", parm.chg, ", ", m.max, ", ",  0.5, ") \n", sep = "")
  cvs <-CV(parm.chg, m.max, 0.5)  
}


setwd("..")
