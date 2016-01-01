#'EstFreq
#'
#'Simulate number low freq or zero frequencies for a state-transition table dependend ont the number of time points
#'
#'
#'@param x a matrix containing the assumed probabilities for the expected transition tables
#'@param t number of time intervalls
#'@param k Number of simulations (at least 20.000 is recommended)
#'@param min.cell a single integer defines what counts as a low frequency (5 by convention)
#'
#'
#'@details First for entrys represent the first column, fith to eights entry the second column
#'
#'
#'
#'
#'
#'
#'@examples
#'my.trans.table<-matrix(c(0.57, 0.13,0.05,0.05,0.05, 0.05,0.05,0.05),4,2)
#'
#'my.cellproblems<-EstFreq(my.trans.table, 100, 5, k=20000)
#'
#'my.cellproblems
#'
#'
#'@export


EstFreq<-function(x, t, min.cell=5, k=20000){

  mycat<-as.vector(x)

  if(sum(mycat)!=1) warning("sum of probabilities must be 1!")
  if(!all(dim(x)==c(4,2))) warning("Only 4*2 transition-tables are supported!")
  if(!is.matrix(x)) warning("Transition-table must be entered as matrix!")

  zero.n<-numeric(k)
  below.n<-numeric(k)
  freq<-1:8

  for (i in 1:k){
    a<-sample(freq, t, T, mycat)
    b<-table(c(1:8,a))-1
    zero.n[i]<-sum(b==0)
    below.n[i]<-sum(b<min.cell)
  }

  output<-list(zero.n, below.n, min.cell, k)
  class(output)<-"EstFreq"
  return(output)
}

