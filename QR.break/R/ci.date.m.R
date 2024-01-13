##' Confidence intervals for break dates
##'
##' This function constructs confidence intervals for estimated break dates
##' based on multiple regression quantiles.
##' 
##' @title Confidence intervals for estimated break dates
##' 
##' @param y A vector of dependent variable (NT x 1) 
##' @param x A matrix of regressors (NT x p)
##' @param vec.tau A set of probablities for conditioanl quantiles of interest 
##' @param vec.date Estimated break dates 
##' @param n.size The size of cross sections (N)
##' @param v.b v.b =1 for the 90% confidence interval or =2 for the 95% confidence interval [v.b=2 by default]
##' @return A matrix in which the 1st column includes break dates; 2nd and 3rd columns include confidence intervals
##' 
##' @author Tatsushi Oka and Zhongjun Qu
##' 
##' @export
ci.date.m = function(y, x, vec.tau, vec.date, n.size=1, v.b=2)
{
    ## size
    n.break = length(vec.date)
    n.tau   = length(vec.tau)
    p.size  = ncol(x) + 1    ## + intercept
  
    ## storage
    mat.ci     = matrix(0, n.break, 3) 
    mat.ci[,1] = matrix(vec.date, n.break, 1)

    ## break size
    mat.size = matrix(0, (n.break*p.size), n.tau)
    for(k in 1:n.tau){
        v.tau = vec.tau[k]
        vec.b = rq.est.full(y, x, v.tau, vec.date, n.size)$coef[]
        mat.size[,k] = vec.b[(p.size+1):((n.break+1)*p.size)]
    }
  
    ## confidence interval  
    ## 1st regime
    sub01  = sample.split(y, x, vec.date[1], n.size)
    y.L    = sub01$y1
    x.L    = sub01$x1
    y.rem  = sub01$y2
    x.rem  = sub01$x2

    ## IF the number of breaks is more than two. 
    if(n.break >= 2){
        for(i in 1:(n.break-1)){

            ## split sample
            date02 = vec.date[(i+1)] - vec.date[i] 
            sub02  = sample.split(y.rem, x.rem, date02, n.size)
            y.R    = sub02$y1
            x.R    = sub02$x1
            y.rem  = sub02$y2
            x.rem  = sub02$x2

            ## conficence interval for 1st - (m-1)th break
            v.date    = vec.date[i] # <== Put a real break date
            beg.row   = p.size * (i -1) + 1
            end.row   = p.size * i
            temp.size = as.matrix(mat.size[beg.row:end.row,], ncol = n.tau)
            mat.ci[i,] = ci.date.m.sub(y.L, x.L, y.R, x.R, v.date, temp.size, vec.tau, n.size, v.b)

            ## replace
            y.L = y.R
            x.L = x.R
        }
    }

    ## For the last break
    v.date    = vec.date[n.break] # <== Put a real break date
    beg.row   = p.size * (n.break -1) + 1
    end.row   = p.size *  n.break
    temp.size = as.matrix(mat.size[beg.row:end.row,], ncol = n.tau)
    mat.ci[n.break,] = ci.date.m.sub(y.L, x.L, y.rem, x.rem, v.date, temp.size, vec.tau, n.size, v.b)
  
    ## return
    return(mat.ci)
}
