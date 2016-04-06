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
#' Test (TSST; Kirschbaum, Pirke, Hellhammer, 1993). For exemplification purposes, only those 
#' 64 couples are included where only the female partner was stressed.  Directly after the stress 
#' induction, both partners joint again and the couple was left alone for eight minutes. During 
#' this period (a 'fake' waiting condition) the two partners were filmed for 8 minutes divided 
#' into 48 intervals of ten seconds length. It was coded if the female partners showed stress
#' communication (SC) within an interval (sequence 1; Colums 50:97) and if the male partner showd
#' dyadic coping reactions (DC; sequence 2; columns 2:49).
#'
#' This is just an excerpt of the original study!
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
#'
#' for TSST:
#' Kirschbaum, C., Pirke, K. M., & Hellhammer, D. H. (1993). The 'Trier Social Stress Test' - a tool for investigating psychobiological stress responses in a laboratory setting. Neuropsychobiology, 28(1-2), 76-81.
#'
#' On the topic of dyadic coping:
#' Bodenmann, G. (1995). A systemic-transactional conceptualization of stress and coping in couples. Swiss Journal of Psychology.
#'
'CouplesCope'

