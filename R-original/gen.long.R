##' Subroutine for dynamic programing algorithm 
##'
##' This function obtains the objective function values given permissible regimes.
##' based on multiple quantiles. 
##' Related subrutines are rq.est.full, sample.split, moment. 
##'
##' @title gen long
##' 
##' @param y A dependent (NT x 1) 
##' @param x A matrix of regressors (NT x p)
##' @param vec.tau Quantiles of interest 
##' @param n.size The size of cross sections; default set to 1 #update 11/06/2013 
##' @param trim.size Triming size
##'  
##' @return Break date estimates
##' 
##' @author Tatsushi Oka and Zhongjun Qu
##' @export
##' 
gen.long = function(y, x, vec.tau, n.size=1, trim.size)
{
    ## size
    t.size = length(y) / n.size
    n.tau  = length(vec.tau)

    ## the following matrix and vector contain the objective function
    ## corresponding to all the possible segments. The value corresponds to the
    ## segment starting at J and lasting for span corresponds to the index
    ## T(j-1)-(j-1)(j-2)/2+span. The matrix and vector are for single quantile and
    ## the combined quantiles, respectively. 
    mat.long = matrix(0, (t.size*(t.size+1)/2), n.tau)  ## individual quantiles  
    vec.long = matrix(0, (t.size*(t.size+1)/2), 1)      ## multiple   quantiles

    for(i in 1:(t.size-trim.size+1)){
        mat.out = gen.mat.rho(y, x, vec.tau, n.size, trim.size, i, t.size)
        
        ## rows
        row01 = (i-1) * t.size + i - (i - 1) * i / 2
        row02 =  i    * t.size     - (i - 1) * i / 2

        ## store
        mat.long[row01:row02,] = mat.out[i:t.size,]
        vec.long[row01:row02]  = rowSums(mat.out[i:t.size,])

    }

    ## return
    list(mat.long = mat.long, vec.long = vec.long)
}
