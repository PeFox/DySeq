#'LogSeq
#'
#'Implementation of Bakeman & Gottman (1997) for sequence analysis
#'Kenny, Kashy & Cook (2006) provide further examples.
#'
#'
#'@param x a state.trans object or a list of 4*2 state-transition tables
#'@param delta constant added to every cell, required if zero frequencies occur!
#'@param subgroups an optional vector containing groupmembership if estimates should be compared between groups
#'
#'@references
#'  Bakeman, R., & Gottman, J. M. (1997). Observing interaction: An introduction to sequential analysis. Cambridge university press.
#'  Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. Guilford Press.
#'
#'
#'
#'
#'
#'@examples
#'data(CouplesCope)
#'my.states<-StateExpand(CouplesCope, 2:49, 50:97)
#'my.trans<-StateTrans(my.states, F)
#'my.logseq<-LogSeq(my.trans)
#'my.logseq
#'
#'plot(my.logseq) # interaction can be plotted
#'
#'single.LogSeq(my.logseq, 41) # for single case analysis
#'
#'@export


LogSeq<-function(x, delta=0.5, subgroups=NA, single.case=FALSE){

  if(class(x)[2]!="state.trans") warning("x should be a state.trans object. See Help(StateTrans)!")

  require("gmodels")
  require("MASS")

  lambdas<-matrix(NA, length(x),4) # Empty table for lambdas

  p.values<-matrix(NA, length(x),4)

  for(i in 1:length(x)){

    x.long<-c(x[[i]][1:4,1],x[[i]][1:4,2])

    casearray<-array(x.long, c(2,2,2), list(c("seq2_1", "seq2_0"),c("seq1_1","seq2_0"), c("dep1","dep0")))

    caselog<-loglm(~1+2+3+1*2*3, data=(casearray+delta), fit=F)

    if(single.case==TRUE){
      caselog.b1<-loglm(~1+2+3+2*3,1*2, data=(casearray+delta), fit=F)
      caselog.b2<-loglm(~1+2+3+1*3+2*3, data=(casearray+delta), fit=F)
      caselog.b3<-loglm(~1+2+3+2*3+1*3+1*2, data=(casearray+delta), fit=F)

      p1<-anova(caselog.b1,caselog.b3)[2,5]
      p2<-anova(caselog.b2,caselog.b3)[2,5]
      p3<-unclass(summary(caselog.b3))$tests[2,3]

      p.values[i,1]<-NA
      p.values[i,3]<-unlist(p1)
      p.values[i,2]<-unlist(p2)
      p.values[i,4]<-unlist(p3)
    }

    b0<-coef(caselog)$"3"[1]-coef(caselog)$"3"[2]   #mean
    b1<-coef(caselog)$"1.3"[1,1]-coef(caselog)$"1.3"[1,2]   #partner
    b2<-coef(caselog)$"2.3"[1,1]-coef(caselog)$"2.3"[1,2]   #actor
    b3<-coef(caselog)$"1.2.3"[1,1,1]-coef(caselog)$"1.2.3"[1,1,2]   #interaction

    lambdas[i,1]<-unlist(b0)
    lambdas[i,3]<-unlist(b1)
    lambdas[i,2]<-unlist(b2)
    lambdas[i,4]<-unlist(b3)
  }

  output<-list()

  output[[1]]<-lambdas
  output[[2]]<-x
  output[[3]]<-subgroups
  output[[4]]<-p.values


  class(output)[1]<-"LogSeq"

  return(output)

}


