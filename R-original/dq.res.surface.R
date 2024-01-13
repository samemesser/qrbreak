##' Critical values using a response surface analysis
##'
##' This procedure returns critical values using a response surface analysis.
##' Please be advised that this procedure only covers
##' the case where the trimming is symmetric, i.e., [tau,1-tau].
##' 
##' @title Critical values using a response surface analysis
##' 
##' @param p The number of parameters in the model
##' @param l The number of changes under H0, i.e. l+1 under H1 
##' @param q.L The left end of a range of quantiles 
##' @param q.R The right end of a range of quantiles
##' 
##' @return Critical value
##' 
##' @author Tatsushi Oka and Zhongjun Qu
##' @export
##' 
dq.res.surface.r = function(p, l, q.L, q.R)
{
    ## We need symmetry
    if (d.Sym != TRUE){
        stop('This program is only for DQ test with a symmetric trimming')
    }

    ## storage
    vec.cv = matrix(0, 3, 1)
    
    ## linear part 1
    vec.cv[1] = 0.9481 + 0.0062 * p + 0.0166 * (l + 1) -0.1386 * (1 / p)
    vec.cv[2] = 0.9944 + 0.0058 * p + 0.0157 * (l + 1) -0.1284 * (1 / p)
    vec.cv[3] = 1.0929 + 0.0050 * p + 0.0134 * (l + 1) -0.1134 * (1 / p)

    
    ## linear part 2
    vec.cv[1] = vec.cv[1] -0.0004 * (l+1) * p + 0.0018 * (l+1) * q.L
    vec.cv[2] = vec.cv[2] -0.0004 * (l+1) * p + 0.0017 * (l+1) * q.L
    vec.cv[3] = vec.cv[3] -0.0002 * (l+1) * p + 0.0010 * (l+1) * q.L

    ## exponantial part
    vec.cv[1] = vec.cv[1]*exp(-0.0801 * (1/(l+1)) -0.0004 * (1 /(q.L*(l+1))) -0.0254 * q.L)
    vec.cv[2] = vec.cv[2]*exp(-0.0716 * (1/(l+1)) -0.0005 * (1 /(q.L*(l+1))) -0.0203 * q.L)
    vec.cv[3] = vec.cv[3]*exp(-0.0565 * (1/(l+1)) -0.0000 * (1 /(q.L*(l+1))) -0.0062 * q.L)

    ## return
    return(vec.cv)
}


