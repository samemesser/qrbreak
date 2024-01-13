##' Critical values for the SQ test 
##'
##' Critical values for the SQ test, SQ(l|l+1).
##'
##' @title Critical values for the SQ test
##' 
##' @param n.break the number of break under the null hypothesis 
##' @param p.size  the number of regressors
##' 
##' @return a 3x1 vector of critical values at at 10%, 5%, 1% significance level
##' 
##' @author Tatsushi Oka and Zhongjun Qu
##' @export
##' 
get.cv.sq = function(n.break, p.size){

    mat.cv = as.matrix(read.table("table.cv.SQtest010910.txt", header=T), ncol = 5)
    
    row.beg = 4 * (p.size-1) + 1 + 1 # the first line includes the number of test 
    row.end = 4 * (p.size-1) + 4

    vec.cv  = mat.cv[row.beg:row.end, (n.break+1)]

    ## return
    return(vec.cv)
}

