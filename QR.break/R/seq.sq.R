##' To determine the number of breaks using the SQ(l|l+1) test
##'
##' This procedure determines the number of breaks by applying the SQ test
##' sequentially.
##' 
##' @title The procedure based on the SQ test to determine the number of breaks 
##'
##' @param y A vector of dependent (NT x 1) 
##' @param x A matrix of regressors (NT x p)
##' @param v.tau A quantile of interest 
##' @param n.size The size of cross sections (N)
##' @param m.max The maximum number of breaks allowed
##' @param trim.size The trimming size
##' @param mat.data a matrix of break dates 
##' @return test   [1xm.max]: DQ test 
##' @return cv     [3xm.max]: critical values for DQ test; 1st, 2nd & 3rd row = 10, 5, 1% size
##' @return date   [3xm.max]: break dates;
##' 1st, 2nd & 3rd row = 10, 5, 1% size 
## @return nbreak [3x1]    : the number of breaks; 1st, 2nd & 3rd row = 10, 5, 1% size 
##' @author Tatsushi Oka and Zhongjun Qu
##' @export
##' 

seq.sq = function(y, x, v.tau, n.size=1, m.max, trim.size, mat.date)
{
    ## the number of regressors
    p.size = ncol(x) + 1  # plus intercept

    ## stroage
    vec.nb       = matrix(0, 3, 1)
    vec.test     = matrix(0, 1, m.max)
    mat.cv       = matrix(0, 3, m.max)

    ## 0 vs 1
    vec.test[1] = sq.test.0vs1(y, x, v.tau, n.size)
    mat.cv[,1]  = get.cv.sq(0, p.size) # critical value
    for (a in 1:3){
        if (mat.cv[a,1] < vec.test[1]){
            vec.nb[a] = 1
        }
    }

    ## k vs k+1
    if (m.max >= 2){
        for(k in 1:(m.max-1)){
            if (max(vec.nb) == k){

                ## 1st-kth break dates are stored in kth column 
                vec.loc = mat.date[1:k,k]  

                ## test: k vs k+1
                vec.test[(k+1)] = sq.test.lvsl_1(y, x, v.tau, n.size, vec.loc)
                mat.cv[,(k+1)]  = get.cv.sq(k, p.size) # critical value
                
                ## for each significance level
                for (a in 1:3){
                    if (vec.nb[a] == k & mat.cv[a,(k+1)] < vec.test[(k+1)]){
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
        if(nb >= 1){ 
            mat.date.opt[a,1:nb] = mat.date[1:nb,nb]
        }
    }

    ## return
    list(test = vec.test, cv = mat.cv, date = mat.date.opt, nbreak = vec.nb)
}

