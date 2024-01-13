##' This procedure determines the number of breaks using DQ(l|l+1) test
##'
##' This procedure determines the number of breaks by applying the DQ test
##' sequentially.
##' 
##' @title The prcedure based on the DQ test to determine the number of breaks
##'
##' @param y A vector of dependent variables (NT x 1) 
##' @param x A matrix of regressors (NT x p)
##' @param vec.tau Quantiles of interest
##' @param q.L the quantile range (the left end) for the DQ test 
##' @param q.R the quantile range (the right end) for the DQ test 
##' @param n.size The size of cross sections (N)
##' @param m.max the maximum number of breaks allowed
##' @param trim.size a trimming value 
##' @param mat.date a matrix of break dates 
##' @return test DQ test
##' @return cv critical values for DQ test (10%, 5% and 1% signficance levels)
##' @return date estimated break dates
##' @return nbreak the number of break dates
##' 
##' @author Tatsushi Oka and Zhongjun Qu
##' @export
##' 
seq.dq = function(y, x, vec.tau, q.L, q.R, n.size=1, m.max, trim.size, mat.date,d.Sym)
{
    ## the number of regressors
    p.size = ncol(x) + 1  # plus intercept

    ## stroage
    vec.nb   = matrix(0, 3, 1)
    vec.test = matrix(0, 1, m.max)
    mat.cv   = matrix(0, 3, m.max)

    ## 0 vs 1
    vec.test[1] = dq.test.0vs1(y, x, q.L, q.R, n.size)
    mat.cv[,1]  = get.cv.dq(0, p.size, q.L, q.R,d.Sym) # critical value
    for (a in 1:3){
        if (mat.cv[a,1] < vec.test[1]){
            vec.nb[a] = 1
        }
    }

    ## k vs k+1
    if (m.max >= 2){
        for (k in 1:(m.max-1) ) { # k is the number of breaks
            if (max(vec.nb) == k){  
                
                ## 1st-kth break dates are stored in kth column 
                vec.loc = mat.date[1:k,k]  

                ## test: k vs k+1
                vec.test[(k+1)] = dq.test.lvsl_1(y, x, q.L, q.R, n.size, vec.loc)
                mat.cv[,(k+1)]  = get.cv.dq(k, p.size, q.L, q.R,d.Sym) # critical value

                ## for each significance level
                for (a in 1:3){
                    if (vec.nb[a] == k  &  mat.cv[a,(k+1)] < vec.test[(k+1)]){
                        vec.nb[a] = vec.nb[a] + 1
                    }
                }
            }
        }
    }

    ## store optimal break dates
    mat.date.opt = matrix(0, 3, m.max)
    for(a in 1:3){
        nb = vec.nb[a]
        if (nb >=1){
            mat.date.opt[a,1:nb] = mat.date[1:nb,nb]
        }
    }

    ## return
    list(test = vec.test, cv = mat.cv, date = mat.date.opt, nbreak = vec.nb)
}
