m.max<-5 # number of breaks under the alternaive hypothesis are [1,2,...,n.max]
vec.tau<-0.4 # if vec.tau is a single quantile, the code simulates the SQ test; otherwise, the DQ test
p.size<-10 #number of parameters allowed to change

#this program will create a text file of critical values at 1,5, and 10 percent levels.
source("CV.R")
cvs <-CV(p.size, m.max, vec.tau)

