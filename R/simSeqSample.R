#'simSeqSample
#'
#'Simulates N single sequences with four possible states. 
#'
#'
#'
#'@param trans a 4x4 matrix containing transition probabilities
#'@param initial a four element vector containing initial states probabilities
#'@param length single value specifying the length of the simulated sequence (columns) 
#'@param N number of sequences which should be simulated (rows)
#'
#'
#'
#'@examples
#' 
#' test1<-matrix(c(0.5 , 0.2 , 0.2 , 0.1,
#'                 0.8 , 0.05, 0.05, 0.1,
#'                 0.5 , 0.1 , 0.2 , 0.2,
#'                 0.1 , 0.1 , 0.1 , 0.7) , 4 , 4 , byrow = TRUE)
#'
#' initial<-c(.25 , .25 , .25 , .25)
#'
#' simSeq(test1, initial, 30)
#'
#'@export


simSeqSample<-function(trans, initial, length, N){
  comb<-simSeq(trans, initial, length)
  #split
  seq1<-c()
  seq1[comb==1|comb==3]<-0
  seq1[comb==2|comb==4]<-1
  seq2<-c()
  seq2[comb==1|comb==2]<-0
  seq2[comb==3|comb==4]<-1
  out<-c(seq1, seq2)
  for(i in 2:N){
    comb<-simSeq(trans, initial, length)
    seq1[comb==1|comb==3]<-0
    seq1[comb==2|comb==4]<-1
    seq2[comb==1|comb==2]<-0
    seq2[comb==3|comb==4]<-1
    out<-rbind(out,c(seq1, seq2))
  }
  return(out)
}
