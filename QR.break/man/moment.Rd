% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moment.R
\name{moment}
\alias{moment}
\title{Moments for statistical inference}
\usage{
moment(y, x, v.tau)
}
\arguments{
\item{y}{A vector of dependent (NT x 1)}

\item{x}{A matrix of regressors (NT x p)}

\item{v.tau}{the (single) quantile of interest}
}
\value{
H Hessian

J Jacobian

mean.f density
}
\description{
Sample moments
}
\details{
This procedure obtains the sample moments to make statistical inference
for quantile regression.
}
\references{
Koenker, R., and Bassett Jr, G. (1978).
"Regression quantiles". Econometrica, 46(1), 33-50.

Oka, T., and Qu, Z. (2011).
"Estimating structural changes in regression quantiles."
Journal of Econometrics, 162(2), 248-267.
}
\author{
Tatsushi Oka and Zhongjun Qu
}
