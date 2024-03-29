% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rq.est.regimie.R
\name{rq.est.regime}
\alias{rq.est.regime}
\title{Estimation of quantile regression, given break dates}
\usage{
rq.est.regime(y, x, v.tau, vec.date, n.size = 1)
}
\arguments{
\item{y}{A vecor of dependent variables (NT x 1)}

\item{x}{A matrix of regressors (NT x p)}

\item{v.tau}{A quantile of interest}

\item{vec.date}{Estimated break dates}

\item{n.size}{The size of cross sections (N)}
}
\value{
Estimation retusult
}
\description{
Estimation of a quantile regression model, given break dates
}
\details{
This procedure estimates the coefficients for each regime, given the break dates.
}
\author{
Tatsushi Oka and Zhongjun Qu
}
