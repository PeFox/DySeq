#'ML_Trans
#'
#'Transforms transition tables into multi-level data. 
#'Each transition between states is handled as a single 
#'observation, which is nested within dyads.
#'
#'
#'
#'@param data a data.frame, which contains dyadic sequences in a bride format
#'@param first column-index that defines the first sequence of each dyad
#'@param second column-index that defines the second sequence of each dyad
#' 
#'
#'@examples
#'
#'# Example: Sequences from couples cope into multi-level data
#'
#' data(CouplesCope)
#' ML_data<-ML_Trans(CouplesCope, 2:49, 50:97)
#'@export



ML_Trans<-function(data, first, second){
  
  my.s<-StateExpand(data, first, second)
  
  myTrans1<-StateTrans(my.s)
  myTrans2<-StateTrans(my.s, first=FALSE)
  
  mylist<-list()
  
  for(k in 1:length(myTrans1)){
    
    which.Dep<-c(0)
    Dep<-c(0)
    V1<-c(0)
    V2<-c(0)
    
    ms<-data.frame(which.Dep, Dep, V1, V2)
    
    names(ms)<-c("which.Dep", "Dep", "Var1 at t-1", "Var2 at t-1")
    
    ms
    
    x<-as.numeric(myTrans1[[k]])
    
    if(x[1]!=0){
      for(i in 1:x[1]){
        ms<-rbind(ms, c(1,1,1,1))
      }
    }
    
    
    if(x[2]!=0){
      for(i in 1:x[2]){
        ms<-rbind(ms, c(1,1,1,0))
      }
    }
    
    
    if(x[3]!=0){
      for(i in 1:x[3]){
        ms<-rbind(ms, c(1,1,0,1))
      }
    }
    
    if(x[4]!=0){
      for(i in 1:x[4]){
        ms<-rbind(ms, c(1,1,0,0))
      }
    }
    
    if(x[5]!=0){
      for(i in 1:x[5]){
        ms<-rbind(ms, c(1,0,1,1))
      }
    }
    
    
    if(x[6]!=0){
      for(i in 1:x[6]){
        ms<-rbind(ms, c(1,0,1,0))
      }
    }
    
    
    if(x[7]!=0){
      for(i in 1:x[7]){
        ms<-rbind(ms, c(1,0,0,1))
      }
    }
    
    if(x[8]!=0){
      for(i in 1:x[8]){
        ms<-rbind(ms, c(1,0,0,0))
      }
    }
    
    ms<-ms[-1,]
    
    # Zweite Variable
    x<-as.numeric(myTrans2[[k]])
    
    if(x[1]!=0){
      for(i in 1:x[1]){
        ms<-rbind(ms, c(2,1,1,1))
      }
    }
    
    
    if(x[2]!=0){
      for(i in 1:x[2]){
        ms<-rbind(ms, c(2,1,1,0))
      }
    }
    
    
    if(x[3]!=0){
      for(i in 1:x[3]){
        ms<-rbind(ms, c(2,1,0,1))
      }
    }
    
    if(x[4]!=0){
      for(i in 1:x[4]){
        ms<-rbind(ms, c(2,1,0,0))
      }
    }
    
    if(x[5]!=0){
      for(i in 1:x[5]){
        ms<-rbind(ms, c(2,0,1,1))
      }
    }
    
    
    if(x[6]!=0){
      for(i in 1:x[6]){
        ms<-rbind(ms, c(2,0,1,0))
      }
    }
    
    
    if(x[7]!=0){
      for(i in 1:x[7]){
        ms<-rbind(ms, c(2,0,0,1))
      }
    }
    
    if(x[8]!=0){
      for(i in 1:x[8]){
        ms<-rbind(ms, c(2,0,0,0))
      }
    }
    
    rownames(ms)<-as.character(1:length(ms[,1]))
    
    mylist[[k]]<-ms
  }
  
  
  which.Dep<-c(0)
  Dep<-c(0)
  V1<-c(0)
  V2<-c(0)
  
  ms<-data.frame(which.Dep, Dep, V1, V2)
  
  names(ms)<-c("which.Dep", "Dep", "Var1 at t-1", "Var2 at t-1")
  
  for(i in 1:length(mylist)){
    ms<-rbind(ms, mylist[[i]])
  }
  ms<-ms[-1,]
  
  
  ID<-c()
  for(i in 1:length(mylist)){
    x<-rep(i, length(mylist[[i]][,1]))
    ID<-c(ID,x)
  }
  out<-cbind(ms,ID)
  rownames(out)<-as.character(1:length(out[,1]))
  out
}
