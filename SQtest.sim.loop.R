# Simulate Critical Values for SQ test for several specifications

m.max<-5 # number of breaks under the alternaive hypothesis are [1,2,...,n.max]
tau.set<-seq(0.1, 0.9, by = 0.05)
p.max<-20 # maximum number of parameters allowed to change

#this program will create a text file of critical values at 1,5, and 10 percent levels.

source("CV.R")

for (qtl in 1:length(tau.set)) {
  for (parm.chg in 1:p.max) {
    cvs <-CV(parm.chg, m.max, tau.set[qtl])  
  }
}


