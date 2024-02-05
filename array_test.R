setwd("C:/Users/samel/Dropbox/qrbreak/crit_vals")

cv_tables <- list.files()

my_array <- array(dim = c(19, 17, 5, 3))
tau.set<-seq(0.1, 0.9, by = 0.05)

for (i in 1:length(cv_tables)) {
  current_table <- read.table(cv_tables[i])
  p.chg <- current_table[3, 1] - 1
  qtl <- current_table[2, 1]
  qtl_dim <- which(abs(qtl - tau.set) < 0.01)
  for (j in 1:(dim(my_array)[3]) ) {
    cat("Current:", p.chg, ",", qtl, ",", j, "\n")
    my_array[p.chg, qtl_dim, j, ] <- current_table[4:6, j]
  }  
}

save(my_array, file = "my_array.RData")

array_chunk <- my_array[1, 2, , ]


sink("my_cvs.txt")
for (parm in 1:(dim(my_array)[1])) {
  for (qt in 1:(dim(my_array)[2])) {
    for (n.br in 1:(dim(my_array)[3])) {
      cat(my_array[parm, qt, n.br, 1], my_array[parm, qt, n.br, 2], my_array[parm, qt, n.br, 3], "\n", sep = ", ")
    }
  }
}
sink()

cat(array_chunk[1,1], array_chunk[1,2], array_chunk[1,3], "\n", sep = ", ")
array_chunk
