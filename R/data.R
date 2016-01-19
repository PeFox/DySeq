#' Dyadic sequences of 64 heterosexual couples
#'
#' First variable is an ID variable.
#' The following show if dyadic coping was shown 
#' by the male partner (columns 2:49)
#' Columns 50:97 show if stress was communicated by
#' the female parter. 
#' Every column represents a time interval of 10 sec. 
#' Prior to the first interval Stress was/induced via TSST 
#' (TSST; Kirschbaum et al 1993).
#' 
#' This is just an excerpt of the original study in which
#' one or both partners were stressed via the TSST.
#' This data contains only the cases in whichs the female
#' partner was stressed.
#'
#' @format A data frame with 64 rows and 97 variables:
#' \describe{
#' \item{code}{ID variable}
#' \item{IKCB...}{Was stress communication shown in the following time intervall? (...)}
#' \item{DCCB...}{Was dyadic coping shown in the following time intervall? (...)}
#' }
#'
#' @source research grants 100013-115948/1 and 100014-115948 from the Swiss National Science Foundation.
#'
#' @references
#'
#' for TSST:
#' Kirschbaum, C., Pirke, K. M., & Hellhammer, D. H. (1993). The 'Trier Social Stress Test' - a tool for investigating psychobiological stress responses in a laboratory setting. Neuropsychobiology, 28(1-2), 76-81.
#'
#' On the topic of dyadic coping:
#' Bodenmann, G. (1995). A systemic-transactional conceptualization of stress and coping in couples. Swiss Journal of Psychology.
#'
"CouplesCope"

