##' To test the existence of a break in a single conditional quantile.
##'
##' This function tests a single break in a linear quantile regression
##'
##' @title Break test for a single regression quantile
##' @param y A vector of dependent variables (NT x 1)
##' @param x A matrix of regressors (NT x p)
##' @param v.tau A probablity at which a conditional quantile is estimated.
##' @param n.size The size of cross sections (N)
##' @return The value of the SQ test
##'
##' @author Tatsushi Oka and Zhongjun Qu
##' @references
##' Koenker, R., and Bassett Jr, G. (1978).
##' "Regression quantiles". Econometrica, 46(1), 33-50.
##'
##' Qu, Z. (2008).
##' "Testing for structural change in regression quantiles."
##' Journal of Econometrics, 146(1), 170-184.
##'
##' @import quantreg
##' @export

sq.test.0vs1 = function(y, x, v.tau, n.size = 1)
{
    ## time dimension
    t.size = length(y) / n.size

    ## regressors
    bigX    = cbind(1, x)
    sqXX    = chol( t(bigX) %*% bigX)
    invsqXX = solve( t(sqXX) )

    ## quantile regression using all sample
    res = rq(y ~ x, tau = v.tau)$res

    ## Test
    temp = (res <= 0.0) - v.tau
    H1n  = invsqXX %*% t(bigX) %*% temp
    difH = matrix(0, 1, t.size)

    ## loop
    for (j in 2: t.size){

        end1    = n.size * j
        HH      = t(bigX)[,1:end1] %*% temp[1:end1]
        difH[j] = max( abs(invsqXX %*% HH - (j / t.size) * H1n) )

    }

    v.max = max(difH) / sqrt( v.tau * (1 - v.tau) )

    ## Result
    return( v.max )
}

