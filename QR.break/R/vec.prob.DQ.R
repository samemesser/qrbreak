##' This is a subroutine to generate critical values for the DQ test.
##'
##' This is a subroutine to generate critical values for the DQ test.
##' 
##' @title Subroutine to generete critical values of the DQ test
##' @param n.grid the number of grid points
##' @param n.dim the number of repetition 
##' @param n.sim the number of simulation 
##' @return Prob 
##' @author Tatsushi Oka and Zhongjun Qu
##' @export
##' 
vec.prob.DQ = function(n.grid, n.dim, n.sim, vec.tau)
{
    ## size
    n.tau = length(vec.tau)
    
    ## simulation
    vec.lamda = seq(0, 1, length = n.grid)
    vec.p     = matrix(0, n.sim, 1)
    for(s in 1:n.sim){

        ## Over dimenstion
        vec.max = matrix(0, n.dim, 1)
        for(d in 1:n.dim){

            ## e ~ U[0,1]
            vec.e = runif(n.grid, 0, 1)

            vec.limit = matrix(0, n.tau, 1)
            for(i in 1:n.tau){

                ## value tau
                v.tau = vec.tau[i]

                ## brownian motion
                vec.bm = cumsum( (vec.e <= v.tau) )

                ## brownian bridge
                vec.BB = (vec.bm - vec.lamda * vec.bm[n.grid]) / sqrt(n.grid)

                ## max over lamda
                vec.limit[i] = max(abs(vec.BB))
            }

            ## max over tau
            vec.max[d] = max(vec.limit)
        }      

        ## maximization over dimension
        vec.p[s] = max(vec.max)
    }

    ## return 
    return(vec.p)
}

