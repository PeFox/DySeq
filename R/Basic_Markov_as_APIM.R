#'Basic_Markov_as_APIM
#'
#'Fits a basic Markov-model on dyadic sequences. 
#'The transition matrix is converted into equivalent APIM-beta-coefficients.
#'Bootstrapping is used for approximating p-values. (H1: Effect is different from zero)
#'
#'
#'@examples
#'\dontrun{
#' 
#'trans1<-APIMtoTrans(B0_1=0, AE_1=1, PE_1=0, Int_1=0,
#'                    B0_2=0, AE_2=0, PE_2=0, Int_2=0)
#'
#'x<-simSeqSample(trans=trans1, initial=rep(.25,4), length=100, N=100)
#'
#'# small boot-size sample only for demonstration purposes! 
#'MasAPIM(x, 1:100, 101:200, boot=10)
#'}
#'@export
#'
Basic_Markov_as_APIM<-function(x, first, second, boot=1000, SimOut=FALSE, CPU=1, sim="ordinary", parallel = "no"){
  out<-c()
  require(boot)  
  require(TraMineR)
  
  MyBetas<-function(data, indices){
    a<-StateExpand(data[indices,], first, second) 
    b<-suppressMessages(seqdef(a[,first], 
                               start = 1,
                               labels = c("0-0", "1-0", "0-1", "1-1")))
    z<-suppressMessages(seqtrate(b))
    return(TransToAPIM(z))
  }
  
  
  results<-boot(data=x, 
                statistic=MyBetas, 
                R=boot,
                ncpus=CPU,
                sim=sim)
  
  
  # Approximating the p-values for coefficient + saving estimate
  
  # DC
  # Intercept
  out[1]<-results$t0[1]
  DC_b0_H0_Dist<-results$t[,1]-mean(results$t[,1]) #H0 Distribution
  out[9]<-mean(DC_b0_H0_Dist>abs(results$t0[1])|DC_b0_H0_Dist<(-abs(results$t0[1])))
  
  # Actor
  out[2]<-results$t0[2]
  DC_Act_H0_Dist<-results$t[,2]-mean(results$t[,2]) # H0 Distribution
  out[10]<-mean(DC_Act_H0_Dist>abs(results$t0[2])|DC_Act_H0_Dist<(-abs(results$t0[2])))
  
  # Partner
  out[3]<-results$t0[3]
  DC_Par_H0_Dist<-results$t[,3]-mean(results$t[,3]) # H0 Distribution
  out[11]<-mean(DC_Par_H0_Dist>abs(results$t0[3])|DC_Par_H0_Dist<(-abs(results$t0[3])))
  
  # Interaction
  out[4]<-results$t0[4]
  DC_Int_H0_Dist<-results$t[,4]-mean(results$t[,4]) # H0 Distribution
  out[12]<-mean(DC_Int_H0_Dist>abs(results$t0[4])|DC_Int_H0_Dist<(-abs(results$t0[4])))
  
  
  # SC
  # Intercept
  # Intercept
  out[5]<-results$t0[5]
  SC_b0_H0_Dist<-results$t[,5]-mean(results$t[,5]) #H0 Distribution
  out[13]<-mean(SC_b0_H0_Dist>abs(results$t0[5])|SC_b0_H0_Dist<(-abs(results$t0[5])))
  
  # Actor
  out[6]<-results$t0[6]
  SC_Act_H0_Dist<-results$t[,6]-mean(results$t[,6]) # H0 Distribution
  out[14]<-mean(SC_Act_H0_Dist>abs(results$t0[6])|SC_Act_H0_Dist<(-abs(results$t0[6])))
  
  # Partner
  out[7]<-results$t0[7]
  SC_Par_H0_Dist<-results$t[,7]-mean(results$t[,7]) # H0 Distribution
  out[15]<-mean(SC_Par_H0_Dist>abs(results$t0[7])|SC_Par_H0_Dist<(-abs(results$t0[7])))
  
  # Interaction
  out[8]<-results$t0[8]
  SC_Int_H0_Dist<-results$t[,8]-mean(results$t[,8]) # H0 Distribution
  out[16]<-mean(SC_Int_H0_Dist>abs(results$t0[8])|SC_Int_H0_Dist<(-abs(results$t0[8])))
  
  
  if(SimOut){
    
    names(out)<-c("DC_b0",
                  "DC_Actor",
                  "DC_Partner",
                  "DC_Inter",
                  "SC_b0",
                  "SC_Actor",
                  "SC_Partner",
                  "SC_Inter",
                  "P_DC_b0",
                  "P_DC_Actor",
                  "P_DC_Partner",
                  "P_DC_Inter",
                  "P_SC_b0",
                  "P_SC_Actor",
                  "P_SC_Partner",
                  "P_SC_Inter")
    
    As_APIM<-out
    
  }else{
    
    out2<-data.frame(rep(NA, 8),rep(NA, 8))
    rownames(out2)<-c("First Intercept",
                      "First Actor",
                      "First Partner",
                      "First Interaction",
                      "Second Intercept",
                      "Second Actor",
                      "Second Partner",
                      "Second Interaction")
    colnames(out2)<-c("Estimate","P_Value")
    out2[1:4,1]<-out[1:4]
    out2[5:8,1]<-out[5:8]
    out2[1:4,2]<-out[9:12]
    out2[5:8,2]<-out[13:16]
    As_APIM<-out2
  }
  
  a<-StateExpand(x, first, second) 
  b<-suppressMessages(seqdef(a[,first], 
                             start = 1,
                             labels = c("0-0", "1-0", "0-1", "1-1")))
  Transition_Matrix<-suppressMessages(seqtrate(b))
  
  rownames(Transition_Matrix)<-c("[0:0 ->]", "[1:0 ->]", "[0:1 ->]", "[1:1 ->]")
  colnames(Transition_Matrix)<-c("[-> 0:0]", "[-> 1:0]", "[-> 0:1]", "[-> 1:1]")
  
  output<-list(Transition_Matrix, As_APIM)
  names(output)<-c("Transition Matrix", "Transitions converted as APIM")
  return(output)
}
