#'simSeq
#'
#'Simulates a single sequence with four possible states. 
#'
#'
#'
#'@param trans a 4x4 matrix containing transition probabilities
#'@param initial a four element vector containing initial states probabilities
#'@param length single value specifying the length of the simulated sequence 
#'
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


simSeq<-function(trans, initial, length){
  
  init<-sample(c(1,2,3,4),1, prob=initial)
  
  for(i in 2:length){
    if(init[i-1]==1){
      init[i]<-sample(c(1,2,3,4),1, prob=trans[1,])
    }else if(init[i-1]==2){
      init[i]<-sample(c(1,2,3,4),1, prob=trans[2,])
    }else if(init[i-1]==3){
      init[i]<-sample(c(1,2,3,4),1, prob=trans[3,])
    }else{
      init[i]<-sample(c(1,2,3,4),1, prob=trans[4,])
    }
  }
  return(init)
}