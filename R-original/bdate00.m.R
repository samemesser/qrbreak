##' Break dates estimation from 1 to m breaks in quantile regression
##'
##' This procedure estimates the model allowing for multiple breaks.
##' When m = 1, the procedure, "partition", is used.
##' When m > 1, this function applies the dynamic programming 
##' algorithm to look for the optimal breaks.
##' 
##' @title Estimation of Multiple break dates
##' 
##' @param y A vector of dependent variables (NT x 1) 
##' @param x A matrix of regressors (NT x p)
##' @param n.size The size of cross sections (N)
##' @param m The maximum number of breaks 
##' @param trim.size The trimming parameter (to exclude the boundary of the sample) 
##' @param vec.long SOMETING
##' 
##' @return the estimated break dates given 1-m breaks
##' 
##' @author Tatsushi Oka and Zhongjun Qu
##' @references
##' Oka, T., and Qu, Z. (2011).
##' "Estimating structural changes in regression quantiles."
##' Journal of Econometrics, 162(2), 248-267.
##' 
##' @export

bdate00.m = function(y, x, n.size=1, m, trim.size, vec.long)
{
    ## size
    t.size = length(y) / n.size

    mat.date = matrix(0, m, m) 
    ## an upper-triangular matrix containing the estimated break dates 
    ## from one to m

    mat.opt.loc = matrix(0, t.size, m)
    ## a row index corresponding to the ending dates, column index corresponds 
    ## to the number of breaks permitted before the ending date the cell 
    ## contains the break date

    mat.opt.rho = matrix(0, t.size ,m)      
    ## same as above, the cell contains the minimal value of the objective 
    ## fucntion corresponding to that break date

    dvec = matrix(0, t.size,1)
    ## the index is the date after which we inserting the break point. The 
    ## cell contains the corresponding value of the objective function

    vec.global = matrix(0, m, 1)
    ## Global minimum when i breaks are permitted

    ## m = 1
    if (m == 1){
        res01 = partition(vec.long, 1, trim.size, t.size-trim.size, t.size, t.size) 
        mat.date[1,1] = res01$loc.min 
        vec.global[1] = res01$rho.min 
    } else {
        ## when m > 1, a dynamic programming algorithm is used.
        ## The first step is to obtain the optimal one-break partitions for all
        ## possible ending dates from 2h to T-mh+1.
        ## The optimal dates are stored in a vector optdat.
        ## The associated MML are stored in a vector optssr.

        ## First loop. Looking for the optimal one-break partitions for break 
        ## dates between h and T-h. j1 is the last date of the segment.
        for (j1 in (2*trim.size):t.size) { 
            res02 = partition(vec.long, 1, trim.size, (j1-trim.size), j1, t.size)           
            mat.opt.rho[j1,1] = res02$rho.min         # no typo
            mat.opt.loc[j1,1] = res02$loc.min
            
            vec.global[1] = mat.opt.rho[t.size,1]   # no typo
            mat.date[1,1] = mat.opt.loc[t.size,1]

            
            ## Next, the algorithm looks for optimal 2,3,... breaks partitions
            ## The index used is ib.
            for (ib in 2:m){
                if (ib == m){                 
                    ## if we have reached the maximum number of breaks allowed,
                    ## then do the following
                    jlast = t.size
                    
                    beg01 = ib * trim.size
                    end01 = t.size - trim.size
                    for (jb in beg01:end01){
                        dvec[jb] = mat.opt.rho[jb,ib-1] +
                            vec.long[(jb+1)*t.size-jb*(jb+1)/2]
                    }
                    min.loc = (beg01 - 1) + which.min(dvec[beg01:end01])
                    mat.opt.rho[jlast,ib] = dvec[min.loc]
                    mat.opt.loc[jlast,ib] = min.loc
                } else {
                    ## if we have not reached the highest number of breaks considered, 
                    ## we need to loop over the possible last dates of the segments,
                    ## between (ib+1)*h and T.
                    for(jlast in ((ib+1)*trim.size):t.size){
                        beg01 = ib * trim.size
                        end01 = jlast - trim.size
                        for(jb in beg01:end01){             
                            dvec[jb] = mat.opt.rho[jb,ib-1] +
                                vec.long[jb*t.size-jb*(jb-1)/2+jlast-jb]
                        }
                        min.loc = (beg01 - 1) + which.min(dvec[beg01:end01])
                        mat.opt.rho[jlast,ib] = dvec[min.loc]
                        mat.opt.loc[jlast,ib] = min.loc
                    }
                }
                
                mat.date[ib,ib] = mat.opt.loc[t.size,ib]
                for(i in 1:(ib-1)){
                    xx = ib - i
                    mat.date[xx,ib] = mat.opt.loc[mat.date[xx+1,ib],xx]
                }
                vec.global[ib] = mat.opt.rho[t.size,ib]
            }
        }         # closing the if for the case m > 1

    }

    ## return
    return(mat.date)
}


