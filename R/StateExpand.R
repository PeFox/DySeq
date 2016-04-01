#'StateExpand
#'
#'Transforms dyadic binary data into state-expand-sequences
#'(combines two corresponding sequences into one for every row of a dataframe)
#'
#'@param x Dataframe or matix containing the sequences that should be combined
#'@param pos1 a vector that indicates all columns of the first sequence
#'@param pos2 a vector that indicates all columns of the second sequence
#'@param replace.na an integer that is used for replacement or if FALSE, no replacement will take place
#'
#'@return returns a matrix with the combined sequences. 
#'
#'@details Takes a dataframe or matrix with dyadic binary data in wide data format, that is:
#' - one observation unit (for example one couple) is represented by one row
#' - every observation unit has two sequences with the same length
#' - entrys must be corresponding to each other (for example they represent same time intervalls)
#' - every sequence contains only zeros and/or ones (for example behavior is shown or not)
#'
#' ...transforms it into state-expand-sequences:
#' - one sequence per observation unit that contains the same information as before
#' - every entry represents the combination of the corresponding previous entrys
#' - 0 represents two former zeros,
#' - 1 represents a one in the first sequence and a zeros in the second
#' - 2 represents a zero in the first sequence and a one in the second
#' - 3 represents a one in both former sequeces
#'
#' .... use:
#'- Most packages are only suited for univariat sequence analysis.
#'- This function transforms dyadic dequences into univariate sequences.
#'- state-expand-sequences are needed for some of the other functions of this package.
#'  
#'
#'@references
#'  Bakeman, R., & Gottman, J. M. (1997). Observing interaction: An introduction to sequential analysis. Cambridge university press.
#'  Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. Guilford Press.
#'
#'@examples
#'  # Example 1
#'  data(CouplesCope) # Load sample data
#'  CouplesCope[1:5,] # inspect first five cases
#'
#'  my.expand<-StateExpand(CouplesCope, 2:49, 50:97)
#'  my.expand[1:5,] # inspect first five cases of the combined sequences
#'
#'
#'
#'  # Example 2: with NA replacement
#'  data(CouplesCope)
#'  
#'  # copy part of the example data
#'  # excluding code and EDCm for simplification
#'  na.CouplesCope<-CouplesCope[,2:97] 
#'  
#'    
#'  # fill it with 10% NA's as an example:
#'  na.CouplesCope[matrix(sample(c(TRUE, rep(FALSE,9)),64*96, TRUE), 64, 96)]<-NA 
#'  na.CouplesCope[1:5,] # inspect the first 5 cases
#'  
#'  # demonstrate na.replace: combine states and fill NA's with zeros!
#'  my.expand<-StateExpand(na.CouplesCope, 1:48, 49:96, replace.na=0) 
#'  my.expand[1:5,] # inspect the first 5 cases
#'
#'
#'
#'\dontrun{
#'  # Example 3: Use StateExpand for further analyis 
#'  #            or plotting using the Package TraMineR
#'               
#'  # install.packages("TraMineR") # install "TraMineR" for graphical analysis
#'  library(TraMineR) #load TraMineR
#'
#'  my.expand<-StateExpand(CouplesCope, 2:49, 50:97) # create combined sequences
#'
#'  # create labels for plot
#'  couple.labels <-c("no reaction", "stress only", "coping only", "both reactions")  
#'  
#'  # create a sequence object (the way TraMineR represents sequences)
#'  couple.seq <- seqdef(my.expand, labels = couple.labels) 
#'  seqdplot(couple.seq)
#'
#'  detach(TraMineR) # unloading TraMineR
#'}
#'@export


StateExpand<-function(x,pos1,pos2, replace.na=FALSE){
  l1<-length(pos1)
  l2<-length(pos2)
  if(l1!=l2) warning("Both sequences must have the same length!")
  x1<-as.matrix(x[,pos1])
  x2<-as.matrix(x[,pos2])
  output<-matrix(data=NA, nrow=length(x[,1]), ncol=l1)
  output[x1==0 & x2==0]<-0
  output[x1==1 & x2==0]<-1
  output[x1==0 & x2==1]<-2
  output[x1==1 & x2==1]<-3
  if(is.numeric(replace.na)) output[is.na(output)]<-replace.na
  class(output)[2]<-"state.expand"
  return(output)
}




