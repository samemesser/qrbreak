##' Estimation of a linear quantile regression with structual breaks 
##'
##' This procedure estimates a linear quantile regression,
##' given a set of sepcified break dates.
##' 
##' @title Estimation of a linear quantile regression with specific breask dates. 
##' @param y A vector of dependent ($NT \times 1$) 
##' @param x A matrix of regressors (NT x p)
##' @param v.tau the (single) quantile of interest
##' @param vec.date A vector of break dates, sepcified by users. 
##' @param n.size The size of cross sections (N)
##' 
##' @return object from quantile regression estimates, rq(), with structural breaks.
##' 
##' @references
##' Koenker, R., and Bassett Jr, G. (1978).
##' "Regression quantiles". Econometrica, 46(1), 33-50.
##' 
##' Oka, T., and Qu, Z. (2011).
##' "Estimating structural changes in regression quantiles."
##' Journal of Econometrics, 162(2), 248-267.
##'
##' @author Tatsushi Oka and Zhongjun Qu
##' @import quantreg
##' @export

rq.est.full = function(y, x, v.tau, vec.date, n.size=1)
{
    ## add an intercept
    x.1 = cbind(1, x)

    ## size and # breaks
    nt.size = length(y)
    t.size  = nt.size / n.size
    p.size  = ncol(x.1)
    n.break = length(vec.date)

    ## note: matrix looks like stairs to obtain break sizes
    b.size = matrix(0, nt.size, (n.break*p.size)) 
    for(i in 1:n.break){
        r.beg = n.size * vec.date[i] + 1
        c.beg = p.size * (i -1) + 1
        c.end = p.size *  i
        b.size[r.beg:nt.size,c.beg:c.end] = x.1[r.beg:nt.size,]
    }
    big.x = cbind(x, b.size)
    
    ## estimation
    fit = rq(y ~ big.x, tau = v.tau)   ## w/o intercepts b/c rq() adds it

    ## return
    return(fit)
}

