% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rq.break.R
\name{rq.break}
\alias{rq.break}
\title{Confidence intervals for break dates}
\usage{
rq.break(
  y,
  x,
  vec.tau,
  n.size = 1,
  trim.e,
  q.L,
  q.R,
  vec.time,
  m.max,
  v.a,
  v.b,
  d.Sim = TRUE,
  d.Sym = FALSE
)
}
\arguments{
\item{y}{A dependent (NT x 1)}

\item{x}{A matrix of regressors (NT x p)}

\item{vec.tau}{Quantiles of interest}

\item{n.size}{The size of cross sections}

\item{v.b}{SOMETING}

\item{vec.date}{Estimated break dates}
}
\value{
A matrix in which the 1st column includes break dates; 2nd and 3rd columns include confidence intervals
}
\description{
Confidence intervals for break dates
}
\details{
This function constructs confidence intervals for the break dates
based on multiple quantiles.
Related subrutines are rq.est.full, sample.split, moment.
}
\author{
Tatsushi Oka and Zhongjun Qu
}
