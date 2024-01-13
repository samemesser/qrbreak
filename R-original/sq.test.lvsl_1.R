##' Break test for a single regression quantile
##'
##' This function tests the null hypothesis of L breaks
##' againsts the alternative hypothesis of L+1 breaks
##' in a single conditional quantile.
##'
##' @title Break test for a single regression quantile
##' @param y A vector of dependent variables (NT x 1)
##' @param x A matrix of regressors (NT x p)
##' @param v.tau A quantile of interest
##' @param vec.date A vector of break dates estimated under the null hypothesis
##' @param n.size The size of cross sections ($N$)
##' @return TBa
##'
##' @author Tatsushi Oka and Zhongjun Qu
##' @references
##' Oka, T., and Qu, Z. (2011).
##' "Estimating structural changes in regression quantiles."
##' Journal of Econometrics, 162(2), 248-267.
##'
##' Qu, Z. (2008).
##' "Testing for structural change in regression quantiles."
##' Journal of Econometrics, 146(1), 170-184.
##' @export

sq.test.lvsl_1 = function(y, x, v.tau, n.size = 1, vec.date) #order n.size, vec.date adjusted
{
    ## the number of breaks
    n.break  = length(vec.date)

    ## sequential test
    vec.test = matrix(0, (n.break+1), 1)
    rem.y    = y
    rem.x    = x
    pre.date = c(0, vec.date)
    for(j in 1:n.break){

        v.date = vec.date[j] - pre.date[j]

        ## split sample given a break date
        temp   = sample.split(rem.y, rem.x, v.date, n.size)
        rem.y  = temp$y2
        rem.x  = temp$x2

        ## test statistics
       # vec.test[j] = SQtest(temp$y1, temp$x1, v.tau, n.size) switched 11/2023
        vec.test[j] = sq.test.0vs1(temp$y1, temp$x1, v.tau, n.size)
    }

    ## the last regime
    #vec.test[(n.break+1)] = SQtest(rem.y, rem.x, v.tau, n.size) switched 11/2023
    vec.test[(n.break+1)] = sq.test.0vs1(rem.y, rem.x, v.tau, n.size)

    ## return: maximum over regimes
    return( max(vec.test) )
}



