##' Subroutine: sample splitting 
##'
##' This procedure splits the sample into two subsamples.  
##' 
##' @title Sample splitting subroutine
##' 
##' @param y A vector of dependent variables (NT x 1) 
##' @param x A matrix of regressors (NT x p)
##' @param v.date a break date 
##' @param n.size The size of cross sections
##' 
##' @return y1 a vector of dependent variables in the first sample 
##' @return x1 a matrix of regressors in the first sample 
##' @return y2 a vector of dependent variables in the second sample 
##' @return x2 a matrix of regressors in the second sample 
##' 
##' @author Tatsushi Oka and Zhongjun Qu
##' @export
##' 
sample.split = function(y, x, v.date, n.size=1)
{
    ## sample size and the location to split
    nt.01   = v.date * n.size ## the end of a regime
    nt.size = length(y)

    ## split 
    y1 = as.matrix( y[1:nt.01]  )
    x1 = as.matrix( x[1:nt.01,] )  
    y2 = as.matrix( y[(nt.01+1):nt.size]  )
    x2 = as.matrix( x[(nt.01+1):nt.size,] ) 

    ## return
    list(y1 = y1, x1 = x1, y2 = y2, x2 = x2)
}


