##' The Data Set for Young Drunk Drivers 
##'
##' The data set is a repated corss-sectional data set,
##' which contains information on young drivers (less than
##' 21 years old) involved in motor vehicle accidents for the state of
##' California over quaters of 1983–2007. It is obtained from National
##' Highway Traffic Safety Administration (NHTSA), which reports the
##' the blood alcohol concentration (BAC) level of the driver,
##' his/her age, gender and whether the crash is fatal.
##' Motor vehicle crash is the leading cause of death among youth ages 15–20,
##' a high proportion of which involves drunk driving, and 
##' the BAC level is an important measure of alcohol impairment.
##' Oka and Qu (2011) analyze the data to document 
##' whether and how young driver’s drinking behavior has changed over time.
##'
##' \itemize{
##'   \item yq:  Year and quater ("Year Qauter" format: e.g. "1947 Q2")
##'   \item bac: The blood alcohol concentration
##'   \item age:    Drivers' age 
##'   \item gender: A gender dummy, which takes 1 for male and 0 for female 
##'   \item winter: A dummy for the fourth quater, taking 1 for Q4 and 0 otherwise
##' }
##'
##' @docType data
##' @keywords datasets
##' @name driver
##' @usage data(driver)
##' @format A data object with five variables
##'
##' @references
##' Oka, T., and Qu, Z. (2011).
##' "Estimating structural changes in regression quantiles."
##' \emph{Journal of Econometrics}, 162(2), 248-267.
##' 
##' @examples
##' data(driver)
##' names(driver)
##' summary(driver)
"driver"
