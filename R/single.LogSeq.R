#'single.LogSeq
#'
#'prints estimates vor single case out of an LogSeq object
#'
#'
#'@param x a LogSeq object, that should be printed
#'
#'@examples
#'data(CouplesCope)
#'my.states<-StateExpand(CouplesCope, 2:49, 50:97)
#'my.trans<-StateTrans(my.states, F)
#'my.logseq<-LogSeq(my.trans)
#'my.logseq
#'
#'single.LogSeq(my.logseq, 41)
#'
#'@export

single.LogSeq<-function(x, case){

  beta<-c(my.logseq[[1]][case,1], my.logseq[[1]][case,3],my.logseq[[1]][case,2], my.logseq[[1]][case,4])

  exp_beta<-exp(beta)

  p.value<-c(c(my.logseq[[4]][case,1], my.logseq[[4]][case,3],my.logseq[[4]][case,2], my.logseq[[4]][case,4]))

  output_seq<-cbind(beta,exp_beta, p.value)
  rownames(output_seq)<-c("intercept","actor","partner","interac")
  output_seq<-as.data.frame(output_seq)
  output<-round(output_seq,3)

  Sys.sleep(0.01)
  print(output)
  cat("\nNote: p.value for intercept is not implemented yet;\np.value of 0 means it is lower than .001!\n\n")

  invisible(output)
}

