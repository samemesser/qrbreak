##' Optimal break partition 
##'
##' procedure to obtain an optimal one break partition
##' a segment, [start,end].
##' 
##' @title Optimal Break Partition
##' 
##' @param vec.long a long vector contains the values of the objective function 
##' @param start the beginning of the segment considered
##' @param b1 the first possible break date
##' @param b2 the last  possible break date
##' @param last the end of segment considered 
##' @param t.size The size of time series 
##' 
##' @return loc.min the optimal break point
##' @return rho.min the value of objective function at the optimal break point
##' 
##' @author Tatsushi Oka and Zhongjun Qu
##' 
##' @export

partition = function(vec.long, start, b1, b2, last, t.size)
{
    ## initial 
    ini = (start - 1) * t.size - (start - 2) * (start - 1) / 2 + 1

    ## storage
    temp.rho = matrix(0, t.size, 1)

    ## pick up vec of rho
    for(j in b1:b2){
        
        loc01 = j - start + ini
        loc02 = j * t.size - (j - 1) * j / 2 + last - j

        ## 1st term: rho of [start,j]
        ## 2nd term: rho of [(j+1),last-j]
        temp.rho[j] = vec.long[loc01] + vec.long[loc02]
    }

    ## minimization
    loc.min = (b1 - 1) + which.min(temp.rho[b1:b2])
    rho.min = temp.rho[loc.min]

    ## return
    list(loc.min = loc.min, rho.min = rho.min)
}
