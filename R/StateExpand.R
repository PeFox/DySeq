#'StateExpand
#'
#'Transforms dyadic binary data into state-expand-sequences
#'(combines two corresponding sequences into one for every row of a dataframe)
#'
#'@param x Dataframe or matix containing the sequences that should be combined
#'@param seq1 a vector that indicates all columns of the first sequence
#'@param seq2 a vector that indicates all columns of the second sequence
#'@param replace.na an integer that is used for replacement or if FALSE, no replacement will take place
#'
#'@return returns a matrix with the combined sequences. Columns keep their names.
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
#'  By this tranformation other
#'- state-expand-sequences is needed for some of the other functions
#'  of this package. If it is, it will be mentioned in the functions documentation.
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
#'  na.CouplesCope<-CouplesCope # copy the example data
#'  na.CouplesCope[matrix(sample(c(T, rep(F,9)),64*97, T), 64, 97)]<-NA # fill it with 10% NA's
#'  na.CouplesCope[1:5,] # inspect the first 5 cases
#'
#'  my.expand<-StateExpand(na.CouplesCope, 2:49, 50:97, 0) # combine states and fill NA's with zeros!
#'  my.expand[1:5,] # inspect the first 5 cases
#'
#'
#'
#'  # Example 3: Use StateExpand for further analyis or plotting using the Package TraMineR
#'
#'  # install.packages("TraMineR") # install "TraMineR" for graphical analysis
#'  library(TraMineR) #load TraMineR
#'
#'  my.expand<-StateExpand(CouplesCope, 2:49, 50:97) # create combined sequences
#'
#'  couple.labels <-c("no reaction", "stress only", "coping only", "both reactions")  # create labels for plot
#'  couple.seq <- seqdef(my.expand, labels = couple.labels) # create a sequence object (the way TraMineR represents sequences)
#'  seqdplot(couple.seq)
#'
#'  detach(TraMineR) # unloading TraMineR
#'
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




