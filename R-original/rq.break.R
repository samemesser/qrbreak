#WE may have to exclude this function and istead provide some examples related to this file.


##' Confidence intervals for break dates
##'
##' This function constructs confidence intervals for the break dates
##' based on multiple quantiles.
##' Related subrutines are rq.est.full, sample.split, moment.
##' @title Confidence intervals for break dates
##' @param y A dependent (NT x 1)
##' @param x A matrix of regressors (NT x p)
##' @param vec.tau Quantiles of interest
##' @param vec.date Estimated break dates
##' @param n.size The size of cross sections
##' @param v.b SOMETING
##' @return A matrix in which the 1st column includes break dates; 2nd and 3rd columns include confidence intervals
##' @author Tatsushi Oka and Zhongjun Qu
##' @export


## ---------------------------------------------------------------------------------
## rq.break
## This procedure combines the subroutines and returns the results
## - - input - -
## y        [NTx1]: dependent variable
## x        [NTxp]: the matrix of regressors
## vec.tau        : the quantiles of interest
## n.size   [1x1] : the size of cross sections
## trim.e   [1x1] : the trimming proportion when estimating the break dates
## q.L, q.R [1x1] : the quantile range [q.L, q.R] is used for DQ test
## vec.time [Tx1] : the time label for time-series domain
## m.max    [1x1] : the maximum number of breaks allowed
## ---------------------------------------------------------------------------------

rq.break = function(y, x, vec.tau, n.size=1, trim.e, q.L, q.R, vec.time, m.max, v.a, v.b, d.Sim=TRUE, d.Sym=FALSE) 
{

    ## size
    N.tau     = length(vec.tau)
    T.size    = length(y) / n.size    # size of time series
    trim.size = round(T.size * trim.e)
    p.size = ncol(x) + 1  # plus intercept

    ##===========================================================
    ## Analysis Starts from Here
    ##===========================================================

    ## starting time
    time = proc.time()

    ## simulation to generate critical values
    if (d.Sym == FALSE & d.Sim == TRUE){
        critical.DQtest_specific(x, m.max, vec.tau)
    }

    ## obtain values of objective function
    out.long   = gen.long(y, x, vec.tau, n.size, trim.size)
    mat.long.s = out.long$mat.long  ## for individual quantiles
    vec.long.m = out.long$vec.long  ## for mulitple   quantiles

    ## Single Quantile
    cat('================================================================== \n')
    cat('=====Analysis based on a single conditional quantile function ===== \n')

    ## storage
    vec.nb      = matrix(0, (N.tau*3), 1)
    mat.loc.opt = matrix(0, (N.tau*3), m.max)

    for(i in 1:N.tau){

        ## tau
        v.tau  = vec.tau[i]

        cat('================================================================== \n')
        cat('Quantile: ', v.tau, '\n')

        ## break dates given the maximum number of breaks
        vec.long.s = mat.long.s[,i]
        mat.date   = bdate00.m(y, x, n.size, m.max, trim.size, vec.long.s)
        ## print(mat.date)

        ## determine the number of breaks
        out.s = seq.sq(y, x, v.tau, n.size, m.max, trim.size, mat.date)

        ## estimation and inference
        vec.level = c(10, 5, 1)

        ## print out:
        cat('------------- At the significance level: ', vec.level[v.a],'% -------------\n' )
        cat(' SQ test:              ', out.s$test,   '\n')
        cat(' Critical values:      ', out.s$cv[v.a,],    '\n')
        cat(' The number of breaks detected: ', out.s$nbreak[v.a], '\n')

        ## estimation
        n.break      = out.s$nbreak[v.a]
        ind01        = i + N.tau * (v.a - 1) ## store
        vec.nb[ind01]= n.break             ## store
        if (n.break >= 1){
            vec.date = out.s$date[v.a,1:n.break]
            fit      = rq.est.full(y, x, v.tau, vec.date, n.size)
            result   = summary.rq(fit, se = "nid", covariance = TRUE)
            mat.ci   = ci.date.m(y, x, v.tau, vec.date, n.size, v.b)
            cat(' - - Break Dates and Confidence Intervals: ', 100-vec.level[v.b], '% - -     \n')
            print( mat.ci)
            if (min(mat.ci[,2]) < 1 | T.size < max(mat.ci[,3])){
                cat('  confidence interval is out of the range\n')
            } else {
                print( matrix( vec.time[mat.ci], ncol = 3) )
            }

            cat(' - - Estimation Results - - \n')

            ## Estimates for each regime
            cat(' (a)Coefficients estimates for each regime - - \n')
            rq.est.regime(y, x, v.tau, vec.date, n.size)

            ## Estimates for the break sizes
            cat(' (b) Break sizes - - \n')

            for (j in 1:n.break){
                beg01 = 1 + p.size * j
                end01 =     p.size * (j+1)
                print(result$coef[beg01:end01,])
                cat('  \n') # space
            }

            ## store
            mat.loc.opt[ind01,1:n.break] = vec.date
        }
    }

    ## figure for single quantile
    ## fig.s(y, x, vec.tau, vec.nb, mat.date, vec.time, n.size)


    ## Multiple quantiles
    cat('================================================================== \n')
    cat('===== Analysis based on multiple conditional quantiles  ===== \n')
    cat('================================================================== \n')

    ## experiment
    mat.date = bdate00.m(y, x, n.size, m.max, trim.size, vec.long.m)
    ## print(mat.date)
    ## (i) determine the number of breaks
    out.m = seq.dq(y, x, vec.tau, q.L, q.R, n.size, m.max, trim.size, mat.date,d.Sym)

    vec.level = c(10, 5, 1) # significance level


    ## the number of breaks
    n.break = out.m$nbreak[v.a]

    ## print
    cat('------------- At the significance level: ', vec.level[v.a], '% -------------\n' )
    cat(' DQ test:              ', out.m$test,  '\n')
    cat(' Critical values:      ', out.m$cv[v.a,],    '\n')
    cat(' The number of breaks detected: ', n.break,         '\n')

    if (n.break >= 1){
        vec.date = out.m$date[v.a,1:n.break]
        ## confidence interval
        mat.ci = ci.date.m(y, x, vec.tau, vec.date, n.size, v.b)
        cat(' - - Break Dates and Confidence Intervals: ', 100-vec.level[v.b], '% - -     \n')
        if (min(mat.ci[,2]) < 1 | T.size < max(mat.ci[,3])){
            cat('  confidence interval is out of the range\n')
        } else {
            print(mat.ci)
        }
        print( matrix( vec.time[mat.ci], ncol = 3) )
        cat('- - - - Estimation Results - - - -\n')
        for(k in 1:N.tau){
            v.tau  = vec.tau[k]
            cat('- Quantile: ', v.tau ,'-\n')

            ## Estimates for each regime by splitting sample
            cat(' (a) Coefficients estimates for each regime - - \n')
            rq.est.regime(y, x, v.tau, vec.date, n.size)


            ## Estimates of the break sizes
            cat(' (b) Break sizes - - \n')

            fit    = rq.est.full(y, x, v.tau, vec.date, n.size)
            result = summary.rq(fit, se = 'nid', covariance = TRUE)

            for (j in 1:n.break){
                beg01 = 1 + p.size * j
                end01 =     p.size * (j+1)
                print(result$coef[beg01:end01,])
                cat('  \n') # space
            }

        }

    }

    cat('================================================================== \n')
    ## end
    cat('Estimation Time (min) \n')
    print( (proc.time() - time) / 60)
    cat('================================================================== \n')
}



