#'TransToAPIM
#'
#'Transforms a transition matrix into equivalent APIM beta-coefficients. 
#'(only implemented for binary dyadic sequences!)
#'
#'@param M a transition matrix which should be converted to APIM like beta-coefficients 
#'
#'@return B0_1 Intercept if the first sequence is the dependent variable 
#'@return AE_1 Actor-Effect if the first sequence is the dependent variable
#'@return PE_1 Partner-Effect if the first sequence is the dependent variable
#'@return Int_1 Actor*Partner-Intercation-Effect if the first sequence is the dependent variable
#'@return B0_2 Intercept if the second sequence is the dependent variable
#'@return AE_2 Actor-Effect if the second sequence is the dependent variable
#'@return PE_2 Partner-Effect if the second sequence is the dependent variable
#'@return Int_2 Actor*Partner-Intercation-Effect if the second sequence is the dependent variable
#'
#'@examples
#'
#'
#'test<-matrix(c(0.41 , 0.28 , 0.19 , 0.12,
#'               0.18 , 0.18 , 0.32 , 0.32,
#'               0.18 , 0.22 , 0.27 , 0.33,
#'               0.05 , 0.09 , 0.30 , 0.55) , 4 , 4 , byrow = TRUE)
#'
#'x<-TransToAPIM(test)
#'# inspecting the beta-coefficients
#'x
#'
#'#backtesting (last row will show minor errors caused by rounding)
#'round(APIMtoTrans(x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8]),2)
#'
#'@export

TransToAPIM<-function(M){
  
  # transforming to actor & partner for the men
  DC_none<-sum(M[1,3:4])    # P(DC|none)
  DC_SC<-sum(M[2,3:4])      # P(DC|SC) 
  DC_DC<-sum(M[3,3:4])      # P(DC|DC)
  DC_SC_DC<-sum(M[4,3:4])   # P(DC|SC+DC)
  
  # Transforming probabilities into logits
  DC_none_L<-log(DC_none/(1-DC_none))
  DC_SC_L<-log(DC_SC/(1-DC_SC))
  DC_DC_L<-log(DC_DC/(1-DC_DC))
  DC_SC_DC_L<-log(DC_SC_DC/(1-DC_SC_DC))
  
  # Transforming logits into betas
  DCb0<-sum(DC_none_L, DC_SC_L, DC_DC_L, DC_SC_DC_L)/4
  DCPart<-(DC_SC_L+DC_SC_DC_L)/2-DCb0
  DCAct<-(DC_DC_L+DC_SC_DC_L)/2-DCb0
  DCint<-DC_SC_DC_L-(DCb0+DCAct+DCPart)
  
  # For DC:
  B0_2<-DCb0   # Mean logit
  PE_2<-DCPart # Partner effect
  AE_2<-DCAct  # Actor effect
  Int_2<-DCint # Interaction effect
  
  # transforming to actor & partner for the womens
  
  SC_none<-sum(M[1,c(2,4)])  # P(SC|none)
  SC_SC<-sum(M[2,c(2,4)])    # P(SC|SC)
  SC_DC<-sum(M[3,c(2,4)])    # P(SC|DC)
  SC_SC_DC<-sum(M[4,c(2,4)]) # P(SC|SC+DC)
  
  SC_none_L<-log(SC_none/(1-SC_none))
  SC_SC_L<-log(SC_SC/(1-SC_SC))
  SC_DC_L<-log(SC_DC/(1-SC_DC))
  SC_SC_DC_L<-log(SC_SC_DC/(1-SC_SC_DC))
  
  SCb0<-sum(SC_none_L, SC_SC_L, SC_DC_L, SC_SC_DC_L)/4
  SCAct<-(SC_SC_L+SC_SC_DC_L)/2-SCb0   # Note: SC at t-1 is now actor!
  SCPart<-(SC_DC_L+SC_SC_DC_L)/2-SCb0  # DC at t-1 is not partner!
  SCint<-SC_SC_DC_L-(SCb0+SCAct+SCPart)
  

  # For DC:
  B0_1<-SCb0   # Mean logit
  PE_1<-SCPart # Partner effect
  AE_1<-SCAct  # Actor effect
  Int_1<-SCint # Interaction effect
  
  results<-c(B0_1, AE_1, PE_1, Int_1,
             B0_2, AE_2, PE_2, Int_2)
  
  names(results)<-c("Seq1 Intercept","Seq1 Actor","Seq1 Partner","Seq1 Interaction",
                    "Seq2 Intercept","Seq2 Actor","Seq2 Partner","Seq2 Interaction")
  
  return(results)
}