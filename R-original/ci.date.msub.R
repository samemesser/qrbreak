##' Subrutine for ci.date.m
##'
##' This is a sub-rutine for ci.date.m.
##' Related routines include "Resample.split" and "moment".
##'
##' @title Sub-rutine for ci.date.m
##'
##' @param y.L observations (y) on the right-side of the break date
##' @param x.L observations (x) on the right-side of the break date
##' @param y.R observations (y) on the left-side of the break date
##' @param x.R observations (x) on the left-side of the break date
##' @param v.date break date
##' @param mat.size EXPLAIN
##' @param vec.tau Quantiles of interest 
##' @param n.size The size of cross sections (N) [N=1 by default: time series]
##' @param v.b =1 for the 90% confidence interval or =2 for the 95% confidence interval  
##'
##' @return A vector whose 1st column includes a break date, and 2nd and 3rd columns include confidence intervals 
##'
##' @author Tatsushi Oka and Zhongjun Qu
##'
##' @export
ci.date.m.sub = function(y.L, x.L, y.R, x.R, v.date, mat.size, vec.tau, n.size=1, v.b)
{
    ## size
    n.tau = length(vec.tau)
  
    ## Adjustment to use the shrinking-break asymptotic framework
    mat.size = mat.size * sqrt(n.size)

    ## vec.size
    mat.L = matrix(0, n.tau, 2)
    mat.R = matrix(0, n.tau, 2)

    ## taus
    for(i in 1:n.tau){
        v.tau    = vec.tau[i]
        vec.size = mat.size[,i]
        ## moment
        mL  = moment(y.L, x.L, v.tau)
        mR  = moment(y.R, x.R, v.tau)
        ## pi
        mat.L[i,1] = t(vec.size) %*% mL$H %*% vec.size
        mat.R[i,1] = t(vec.size) %*% mR$H %*% vec.size
        ## sigma
        for(s in 1:n.tau){
            ## size for s
            vec.size.S = mat.size[,s]
            ## mix: Note, J does not depdent on tau
            temp = min(v.tau, vec.tau[s]) - v.tau * vec.tau[s]
            ## - - difference m1$J and m2$J 
            mat.L[i,2] = mat.L[i,2] + temp * t(vec.size) %*% mL$J %*% vec.size.S
            mat.R[i,2] = mat.R[i,2] + temp * t(vec.size) %*% mR$J %*% vec.size.S
        }
    }

    ## summation
    pi.L = sum(mat.L[,1])
    pi.R = sum(mat.R[,1])
    s2.L = sum(mat.L[,2])
    s2.R = sum(mat.R[,2])

    ## confidence interval
    vec.Q = c(7.7, 11.0)  ## 90% and 95% confidence levels
    vec.ci = matrix(0, 1, 3)
    vec.ci[1] = v.date
    vec.ci[2] = v.date - round( (vec.Q[v.b] * s2.L / (pi.L ^ 2) )) - 1
    vec.ci[3] = v.date + round( (vec.Q[v.b] * s2.R / (pi.R ^ 2) )) + 1

    return(vec.ci)
}


