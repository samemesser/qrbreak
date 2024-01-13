##' critical.DQtes_specific
##'
##' This procedure obtains critical values via simulation.
##' This procedure saves a text file in your working directory.
##' The name of the file is "table.cv.DQ_t1_t2_p.txt",
##' where
##'   t1 = min(vec.tau), the minimum value of the quantiles; 
##'   t2 = max(vec.tau), the maximum value of the quantiles;
##'   p  = the number of regressors (including an intercept).
##'
##' @title Sub-rutine for ci.date.m
##' 
##' @param x the matrix of regressors (not including an intercept)
##' @param m the maximum number of breaks 
##' @param vec.tau Quantiles of interest
##' 
##' @return Output saved in your working directory.
##' 
##' @author Tatsushi Oka and Zhongjun Qu
##' @export
##' 
critical.DQtest_specific = function(x, m.max, vec.tau)
{
    ## start time
    time = proc.time()

    ## size
    p.size = ncol(x) + 1 ## include intercept
    
    ## setting
    n.grid   = 500
    n.sim    = 50000 ## In Qu(2009), 50,000

    ## significance level
    vec.a = c(0.90, 0.95, 0.99)
    n.a   = length(vec.a)

    ## probablity critical value
    vec.nn = seq(1, m.max, by = 1) 
    mat.prob = matrix(0, 3, m.max)
    mat.prob[1,] = 0.90 ^ (1 / vec.nn)
    mat.prob[2,] = 0.95 ^ (1 / vec.nn)
    mat.prob[3,] = 0.99 ^ (1 / vec.nn)
    mat.index = round(mat.prob * n.sim)

    ## discretization of a range of quantiles
    beg.tau   = min(vec.tau)
    end.tau   = max(vec.tau)
    cont.tau = seq(beg.tau, end.tau, by = 1 / n.grid)
    n.tau    = length(cont.tau)

    ## table
    table.cv     = matrix(0, 2, m.max)
    table.cv[1,] = beg.tau
    table.cv[2,] = end.tau

    ## seed
    set.seed(7, kind = NULL)

    ## simulation
    vec.p  = vec.prob.DQ(n.grid, p.size, n.sim, cont.tau)
    vec.p  = sort(vec.p)
    mat.cv   = matrix(0, 3, m.max)
    vec.name = matrix(p.size, 1, m.max)

    for(k in 1:3){
        mat.cv[k,] = vec.p[mat.index[k,]]
    }

    table.cv = rbind(table.cv, vec.name, mat.cv)

    ## save3
    file.name = paste('table.cv.DQ', beg.tau, '_', end.tau , '_', p.size, '.txt', sep='')
    write.table(table.cv,  file = file.name)

    ## end time
    cat('Time used for simulating the critical values',   proc.time() - time, '\n')
}

