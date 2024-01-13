##' The Data Set for the Real GDP Growth analysis
##'
##' The data set contains the quaterly real GDP growth in the US.
##' The sample period is from 1947 Q4 to to 2009 Q2.
##'
##'
##' \itemize{
##'   \item yq:     Year and quater ("Year Qauter" format: e.g. "1947 Q2")
##'   \item gdp:    Real GDP growth rate (annualized by multiplying 4)
##'   \item lag1:   One quater lagged "gdp"
##'   \item lag2:   Two quaters lagged "gdp"
##' }
##'
##' @docType data
##' @keywords datasets
##' @name gdp
##' @usage data(gdp)
##' @format A data object with four variables
##' @references
##' Oka, T., and Qu, Z. (2011).
##' "Estimating structural changes in regression quantiles."
##' \emph{Journal of Econometrics}, 162(2), 248-267.
##'
##'
##' @examples
##' data(gdp)
##' names(gdp)
##' summary(gdp)
##'
"gdp"
