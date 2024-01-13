##' Sample moments 
##'
##' This procedure obtains the sample moments to make statistical inference
##' for quantile regression.
##' 
##' @title Moments for statistical inference
##' @param y A vector of dependent (NT x 1) 
##' @param x A matrix of regressors (NT x p)
##' @param v.tau the (single) quantile of interest
##' 
##' @return H Hessian 
##' @return J Jacobian  
##' @return mean.f density 
##' 
##' @references
##' Koenker, R., and Bassett Jr, G. (1978).
##' "Regression quantiles". Econometrica, 46(1), 33-50.
##' 
##' Oka, T., and Qu, Z. (2011).
##' "Estimating structural changes in regression quantiles."
##' Journal of Econometrics, 162(2), 248-267.
##' 
##'
##' @author Tatsushi Oka and Zhongjun Qu
##' @import quantreg
##' @export
moment = function(y, x, v.tau)
{
  ## size
  nt.size = length(y)

  ## regressor
  x.1 = cbind(1, x)

  ## bandwidth 
  h = bandwidth.rq(v.tau, nt.size, hs = FALSE)
  if (v.tau + h > 1) 
    stop("v.tau + h > 1:  error in summary.rq")
  if (v.tau - h < 0) 
    stop("v.tau - h < 0:  error in summary.rq")

  ## quotient
  bhi = rq.fit.fnb(x.1, y, tau = v.tau + h)$coef
  blo = rq.fit.fnb(x.1, y, tau = v.tau - h)$coef
  dyhat = x.1 %*% (bhi - blo)
  if (any(dyhat <= 0)) {
    warning(paste(sum(dyhat <= 0), "non-positive fis"))
  }
  f = pmax(0, (2 * h)/(dyhat - eps))
  H = (1 / nt.size) * crossprod( (f * x.1), x.1) 
  J = (1 / nt.size) * crossprod(x.1)
  mean.f = mean(f)

  ## return
  list(H = H, J = J, mean.f = mean.f)
}

