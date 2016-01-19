#'NonCumHaz
#'
#'Computes the non-cumulated hazard from cumulated hazard
#'
#'
#'@param x a vector containing cumulated hazard or a survfit-object from the 'survival'-package
#'@param t optional: vector containting time reference for x (is required for plot)
#'@param plot logical value indicating if non-cumulated plot should be generated
#'
#'@examples
#'
#'# Example 1: Short artificial data
#'# example cumulated hazard with time referenz
#'cumhaz<-c(0.2 ,0.21 ,0.31 ,0.44 ,1.1  ,1.1  ,1.12 ,1.2)
#'time<-c(4     ,5    ,6    ,7    ,10   ,14   ,15   ,16)
#'
#'NonCumHaz(cumhaz, time, plot=T)
#'
#'
#'
#'# Example 2: Every hazard entry represents one point of time
#'
#'# if every hazard entry repesents one point of time
#' NonCumHaz(cumhaz, 1:8, plot=T)
#'
#'
#'
#'# Example 3: real data and real researchquestion
#'# install.packages("survival")
#'library(survival)
#'
#'# How long till the last stress signal
#'my.last<-LastOccur(CouplesCope[,50:97],1)
#'
#'# If last stress signal was in time intervall 48,
#'# stress did not end till the observation duration
#'event<-rep(1,length(my.last))
#'event[my.last==48]<-0

# Coxregression
#'my.surv<-Surv(my.last,event) # creates a object for survival time analysis
#'my.fit<-survfit(coxph(my.surv~1)) # fits coxregression without covariates on the data

#'plot(my.fit) # survival curve
#'plot(my.fit, fun="cumhaz") # cumulated survival curve

# Different uses for NonCumHaz
#'NonCumHaz(my.fit, plot=T)
#'NonCumHaz(my.fit$cumhaz, my.fit$time, plot=T) # if over packages than 'survival'are used


#'@export


NonCumHaz<-function(x, t=NA, plot=FALSE){
    if(!(any(class(x)=="survfit")||is.numeric(x))) {warning("x needs to be a numeric vector or a survfit-object!")}

    if(is.na(t) && plot && (!any(class(x)=="survfit")))  {warning("plot can not be produced, because time referrence is missing!")}

    if(is.numeric(x)){
      output<-numeric(length(x))
      for (i in 1:(length(x)-1)) output[i] <- x[1+i]-x[i]

      if(plot) {o1<-plot(output[1:(length(output)-1)]~t[1:(length(output)-1)], xlab="Time", ylab="Hazard", type ="l")}
    }

    if(any(class(x)=="survfit")){
      my.cumhaz<-x$cumhaz
      my.cumhaz<-c(0,my.cumhaz)
      my.hazard<-c()
      for (i in 1:(length(my.cumhaz)-1)) my.hazard[i] <- my.cumhaz[1+i]-my.cumhaz[i]

      if(plot) {o1<-plot(my.hazard[1:(length(my.hazard)-1)]~x$time[1:(length(my.hazard)-1)], xlab="Time", ylab="Hazard", type ="l")}
      output<-my.hazard
    }

  return(output)
}



