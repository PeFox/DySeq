#'APIMtoTrans
#'
#'Transforms APIM beta-coefficients into an equivalent transition matrix. 
#'(only implemented for binary dyadic sequences!)
#'
#'
#'@param B0_1 Intercept if the first sequence is the dependent variable 
#'@param AE_1 Actor-Effect if the first sequence is the dependent variable
#'@param PE_1 Partner-Effect if the first sequence is the dependent variable
#'@param Int_1 Actor*Partner-Intercation-Effect if the first sequence is the dependent variable
#'@param B0_2 Intercept if the second sequence is the dependent variable
#'@param AE_2 Actor-Effect if the second sequence is the dependent variable
#'@param PE_2 Partner-Effect if the second sequence is the dependent variable
#'@param Int_2 Actor*Partner-Intercation-Effect if the second sequence is the dependent variable
#'
#'@return myTrans a transition matrix 
#'
#'@examples
#'
#'trans1<-APIMtoTrans(B0_1=0.1, AE_1=0.2, PE_1=0.3, Int_1=0.4,
#'                    B0_2=0.5, AE_2=0.6, PE_2=0.7, Int_2=0.8)
#'
#'#inspecting the equivalent matrix
#'trans1
#'
#'#backtesting by transforming the matrix back into beta-coefficients
#'round(TransToAPIM(trans1),6)
#'
#'@export



APIMtoTrans<-function(B0_1, AE_1, PE_1, Int_1,
                      B0_2, AE_2, PE_2, Int_2){
  
  # Coefficients of the first partner
  # B0_1 Intercept
  # AE_1 Actor Effect
  # PE_1 Partner Effect
  # Int_1 Interaction
  
  # Coefficients of the second partner
  # B0_2 Intercept 
  # AE_2 Actor Effect
  # PE_2 Partner Effect 
  # Int_2 Interaction
  
  # Preparing empty matrix object
  myTrans<-matrix(NA, 4,4)
  
  # State 1: -1 on both variables at t-1
  # State 2: -1 on both the first partners variable at t-1
  # State 3: -1 on both the second partners variable at t-1
  # State 4: +1 on both variables at t-1
  
  # First row:
  odds1<-exp(B0_2)*exp(AE_2)^(-1)*exp(PE_2)^(-1)*exp(Int_2)^(1)
  prob1<-odds1/(odds1+1)
  prob1 # probability to go from state 1 to either state 3 or 4
  1-prob1 # probability to go from state 1 to either 1 or 2
  
  odds2<-exp(B0_1)*exp(AE_1)^(-1)*exp(PE_1)^(-1)*exp(Int_1)^(1)
  prob2<-odds2/(odds2+1)
  prob2 # probability to go from state 1 to either state 2 or 4
  1-prob2 # probability to go from state 1 to either 3 or 4
  
  myTrans[1,1]<-(1-prob1)*(1-prob2)
  myTrans[1,2]<-(1-prob1)*prob2
  myTrans[1,3]<-prob1*(1-prob2)
  myTrans[1,4]<-prob1*prob2
  
  # Second row:
  odds1<-exp(B0_2)*exp(AE_2)^(-1)*exp(PE_2)^(1)*exp(Int_2)^(-1)
  prob1<-odds1/(odds1+1)
  
  odds2<-exp(B0_1)*exp(AE_1)^(1)*exp(PE_1)^(-1)*exp(Int_1)^(-1)
  prob2<-odds2/(odds2+1)
  
  myTrans[2,1]<-(1-prob1)*(1-prob2)
  myTrans[2,2]<-(1-prob1)*prob2
  myTrans[2,3]<-prob1*(1-prob2)
  myTrans[2,4]<-prob1*prob2
  
  # Third row:
  odds1<-exp(B0_2)*exp(AE_2)^(1)*exp(PE_2)^(-1)*exp(Int_2)^(-1)
  prob1<-odds1/(odds1+1)
  
  odds2<-exp(B0_1)*exp(AE_1)^(-1)*exp(PE_1)^(1)*exp(Int_1)^(-1)
  prob2<-odds2/(odds2+1)
  
  myTrans[3,1]<-(1-prob1)*(1-prob2)
  myTrans[3,2]<-(1-prob1)*prob2
  myTrans[3,3]<-prob1*(1-prob2)
  myTrans[3,4]<-prob1*prob2
  
  # Fourth row:
  odds1<-exp(B0_2)*exp(AE_2)^(1)*exp(PE_2)^(1)*exp(Int_2)^(1)
  prob1<-odds1/(odds1+1)
  
  odds2<-exp(B0_1)*exp(AE_1)^(1)*exp(PE_1)^(1)*exp(Int_1)^(1)
  prob2<-odds2/(odds2+1)
  
  myTrans[4,1]<-(1-prob1)*(1-prob2)
  myTrans[4,2]<-(1-prob1)*prob2
  myTrans[4,3]<-prob1*(1-prob2)
  myTrans[4,4]<-prob1*prob2
  
  return(myTrans)
}
