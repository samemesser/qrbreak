##====================================================
## Development for R package
## ---------------------------------------------------
##====================================================
## - When you start, you need this file only at the working directry. 
## - Close other R studio, if any
## - Update all packages 


## clear all
rm(list=ls(all=TRUE))
rm(list=ls())


##set working directory to this file's location
library(rstudioapi)
## Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )


## install and load packages for building this R package
pkg <- c("devtools", "roxygen2", "usethis", "knitr", "Rcpp",
         "spelling", "usethis", "callr", "np")
installnewpkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

installnewpkg(pkg)


##=======================================================
## Package Development
##=======================================================
#-------------------------------------
# Step 0: Setup New Package 
#-------------------------------------
ver      = "1.0.1"      ## pakcage version 
pkg_name = "QR.break"   ## package name 


#-------------------------------------
# Step 1: Setup (Create a new package)
#-------------------------------------
create_package(file.path(".", pkg_name))
## - Note: this step opens a new Rproject window and keep it open. 
##         Do NOT close the window.


#-------------------------------------
# Step 2: Move the R functions and data files into correct location
#-------------------------------------
## (1) R-files 

dr.R     = paste0("./R-original/") #these are the original R functions
list.ALL = list.files(dr.R)
list.NoN = list.files(dr.R, pattern = ".R~")
list.R   = setdiff(list.ALL, list.NoN)


#dr.package.R = paste0("./",pkg_name,"/") #copy them to package folder
dr.package.R = paste0("./",pkg_name, "/R/")
file.copy(from = paste0(dr.R, list.R),   # Copy files
          to = paste0(dr.package.R, list.R))

## (2) Data files 
## - copy and paste "data" folder into 
driver = read.csv("./data/driver.csv")
gdp    = read.csv("./data/gdp.csv")


#-------------------------------------
# Step 3: Package setup 
#-------------------------------------
## (0) change working directory to the package directory
setwd(file.path("./", pkg_name))

# (1) License and description detail  
#use_description(fields = list(Title = "Structural Breaks in Quantile Rgression",
#                              Version = ver,
                              ##Language = "es",
#                              Description = "Estimation and inference methods for the
#quantile regression with multiple structural breaks: this program does not ",                              
#                              'Authors@R' = 'c( 
#                                 person(given  = "Tatsushi", 
#                                        family = "Oka", 
#                                        email  = "oka.econ@gmail.com", 
#                                        role   = c("aut", "cre")),
#                                 person(given  = "Zhongjun", 
#                                        family = "Qu", 
#                                        email  = "qu@bu.edu", 
#                                        role   = c("aut", "cre"))',
#                              Maintainer = "Tatsushi Oka <oka.econ@gmail.com>"))


use_description(fields = list(
  Title = "Structural Breaks in Quantile Regression",
  Version = ver,  # Replace with your actual version number
  Description = "Estimation and inference methods for quantile regression with multiple structural breaks. [Complete this with a full description of your package functionality.]",
  'Authors@R' = c(
    person("Tatsushi", "Oka", email = "oka.econ@gmail.com", role = c("aut", "cre")),
    person("Zhongjun", "Qu", email = "qu@bu.edu", role = "aut")
  )
))

#License = use_gpl3_license("Tatsushi Oka")



#-------------------------------------
# Step 3: Load Related Package  
#-------------------------------------
use_package("quantreg")
use_package("SparseM",  type = "Imports")

#-------------------------------------
# Step 4: Load R files and Data    
#-------------------------------------
## (1) load_all()
devtools::load_all()

## data 
use_data(gdp, internal = FALSE, overwrite = TRUE, compress = "gzip")
use_data(driver,    internal = FALSE, overwrite = TRUE, compress = "gzip")

devtools::document()

## roxygenoize 
roxygen2::roxygenize('.', roclets=c('rd', 'collate', 'namespace'))  

# Set up various packages ---------------------------------------------
use_roxygen_md()

use_package_doc()

## check
check()

# Set up other files -------------------------------------------------
use_readme_md()
## Writing 'README.md'

use_news_md()
## Writing 'NEWS.md'

## R CMD build to include author (newly added 1st May 20)
build() # build the source file



#untar(paste("../", pkg_name, "_", ver, ".tar.gz", sep=""), exdir = "./pkg") # untar the zip file in pkg folder
#build_manual(pkg=paste("./pkg")) # build manual from the unzip tar folder


