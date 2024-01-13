##' Estimation of a quantile regression model, given break dates 
##'
##' This procedure estimates the coefficients for each regime, given the break dates.
##' 
##' @title Estimation of quantile regression, given break dates 
##' @param y A vecor of dependent variables (NT x 1) 
##' @param x A matrix of regressors (NT x p)
##' @param v.tau A quantile of interest 
##' @param vec.date Estimated break dates 
##' @param n.size The size of cross sections (N)
##' 
##' @return Estimation retusult 
##' 
##' @author Tatsushi Oka and Zhongjun Qu
##' @export

rq.est.regime = function(y, x, v.tau, vec.date, n.size=1)
{

    ## the number of breaks 
    n.break  = length(vec.date)

    ## sequationally split sample 
    rem.y  = y
    rem.x  = x
    pre.date = c(0, vec.date)
    for(j in 1:n.break){
        cat('  < Regime:', j,'>\n')
        v.date = vec.date[j] - pre.date[j]
        ## split sample given a break date
        temp   = sample.split(rem.y, rem.x, v.date, n.size)
        rem.y  = temp$y2
        rem.x  = temp$x2
        ## estimation
        fit.regime  = rq(temp$y1 ~ temp$x1, v.tau)
        sum.regime  = summary.rq(fit.regime, se = "nid", covariance = TRUE)

        ## print results
        print(sum.regime$coef)
        cat('\n') ## space
    }

    ## the last regime
    cat('  < Regime:', (n.break + 1), '>\n')
    fit.regime  = rq(rem.y~rem.x, v.tau)
    sum.regime  = summary.rq(fit.regime, se = "nid", covariance = TRUE)
    print(sum.regime$coef)
    cat('\n') ## space
}

