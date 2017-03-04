#' Dyadic sequences of 64 heterosexual couples
#'
#' Will be used to exemplify a typical data structure
#' and to illustrate the several function of this package.
#' 
#' In the original sample study, which was promoted as a study on close relationship and stress,
#' 198 heterosexual couples living in Switzerland participated. 
#' 
#' The couples had to have been in the current romantic relationship for at least a year 
#' and to use German language as their main communication language. During the study, either 
#' the woman, the man, or both partners were stressed using the Trier Social Stress
#' Test (TSST; Kirschbaum, Pirke,  Hellhammer, 1993). For exemplification purposes, only those 
#' 64 couples are included where only the female partner was stressed.  Directly after the stress 
#' induction, both partners joint again and the couple was left alone for eight minutes. During 
#' this period (a 'fake' waiting condition) the two partners were filmed for 8 minutes divided 
#' into 48 intervals of ten seconds length. It was coded if the female partners showed stress
#' communication (SC) within an interval (sequence 1; Colums 50:97) and if the male partner showd
#' dyadic coping reactions (DC; sequence 2; columns 2:49). For rurther insides about dyadic coping 
#' and/or stress communication, see Bodenmann (2015).
#'
#' 
#' 
#' Coding:
#' \itemize{
#'  \item code: ID variable
#'  \item IKCB01-IKCB48:   Was stress communication (SC) shown in the time intervalls 1-48?
#'  \item DCCB01-DCCB48:   Was dyadic coping (DC) shown in the time intervalls 1-48? 
#'  \item EDCm:            Men's self-assessed dyadic coping ability\cr
#'  }
#' @format A data frame with 64 rows and 98 variables:
#' 
#' @source data: research grants 100013-115948/1 and 100014-115948 from the Swiss National Science Foundation.
#'
#' @references
#' \itemize{
#'  \item Kirschbaum, C., Pirke, K. M., & Hellhammer, D. H. (1993) <DOI: 10.1159/000119004> 
#'  \item Bodenmann, G. (2015) <DOI: 10.1037/11031-002>
#'  }
'CouplesCope'




#' Dyadic sequences of 45 subjects engaging a social dilemma
#'
#' The data set stems from the bachelor thesis of Halstenberg (2016) and
#' contains sequences of 45 subjects that engaged in a 32-rounds-long 
#' four-coin dilemma. That is, each player starts with four coins that 
#' are worth one point for oneself and two points for the opponent. Both
#' players have to submit zero to four of them to the other player. The 
#' decision is made secretly and simultaneously.
#' 
#' The computer was set to ignore the humans behavior at all. Instead the 
#' 32 rounds were divided into eight blocks. Within each block, the computer
#' gave one-times one, two-times two, and one-times three coins in randomized
#' order. The only exception was the very first turn, in which the algorithm
#' always gave two coins followed by one, two and three coins in randomized 
#' order.Thus, on average, the algorithm gave two coins. Hence, it was 
#' always possible for the human player to give more or fewer coins than the
#' algorithm did before.
#'
#' For the humnan, it was coded whether the human player gave more (1) coins 
#' in his turn than the algorithm did in the last turn or not (0). The same 
#' was coded for the computer. Thus, coding started in the second turn 
#' resulting in 31 entries for each of both. 
#'  
#' The data frame contains 45 rows (subjects) and 62 columns. Columns 1 to
#' 31 correspond to the human behavior, columns 32:61 to the algorithm.   
#' 
#' @source Halstenberg, E. (2016). The effect of social value orientation on cooperation in a four-coin dilemma: a quasi-replication study using the svo slider measure. (Unpublished bachelor thesis). University Bielefeld, Germany.
#'
'GiveSome'






