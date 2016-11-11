#'MLAP_Trans
#'
#'Transforms data, which has been transformed by the function ML_Trans
#'from sequence data into multi-level transitions, into 
#'lagged partner and lagged actor effects. This transformation 
#'is often required before fitting a multi-level actor-partner-interaction
#'model (APIM). 
#'
#'@param x the output of the ML_Trans function
#' 
#'
#'@examples
#'
#'# Example: Applying a APIM on the example data
#'
#'# Transforms Sequences into Multi-Level data
#'ML_data<-ML_Trans(CouplesCope, 2:49, 50:97)
#'
#'# Transforms transitions into lagged Actor and Partner effects
#'MLAP_data<-MLAP_Trans(ML_data)
#'
#'# Data preparation
#'
#'# In example data first seq referred to females
#'# and second to males
#'
#'names(MLAP_data)[1]<-"sex"
#'MLAP_data$sex<-as.factor(MLAP_data$sex)
#'levels(MLAP_data$sex)<-c("female", "male")
#'
#'# Effectcoding
#'MLAP_data$Partner[MLAP_data$Partner==0]<-(-1)
#'MLAP_data$Actor[MLAP_data$Actor==0]<-(-1)
#'
#'# Fits a multi-level APIM using lme4
#'# Here a random intercept-only model
#'\dontrun{
#'## make sure lme4 is installed!
#'## and loaded!
#'#install.packages("lme4")
#'# library(lme4)
#'
#'set.seed(1234)
#'glmer(DV~1+sex+Actor+Partner+Actor*Partner+
#'        sex*Actor+sex*Partner+sex*Actor*Partner+
#'        (1|ID),
#'      data=MLAP_data,
#'      family=binomial)
#'}
#'
#'
#'@export







MLAP_Trans<-function(x) {
  
  part1<-x[x$which.Dep==1,]
  
  names(part1)<-c("which.seq",
                  "DV", 
                  "Actor",
                  "Partner",
                  "ID")
  
  part2<-x[x$which.Dep==2,c(1,2,4,3,5)]
  
  names(part2)<-c("which.seq",
                  "DV", 
                  "Actor",
                  "Partner",
                  "ID")
  
  out<-rbind(part1, part2)
}