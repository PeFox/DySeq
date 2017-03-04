\docType{package}
\name{DySeq}
\alias{DySeq}
\alias{DySeq-package}
\title{DySeq Overview}
\description{A collection of functions for dyadic sequence anaylses.}
\details{
  DySeq provides an implementation of Bakeman & Gottman's (1997) approach of aggregated logit models. And some additional functions for data preparation. A commented R-Script with examples can be found at https://github.com/PeFox/DySeq_script .
  \itemize{
    \item Combining two sequences into one, see \code{\link{StateExpand}}.
    \item Producing state-transition tables from two combined dyadic dichotomous sequences, see \code{\link{StateTrans}}.
    \item For analyzing multiple state-transition tables using Bakeman & Gottman's (1997) approach, see \code{\link{LogSeq}}.
    \item For analyzing dyadic sequences using generalized multi-level models, see \code{\link{MLAP_Trans}}.
    \item For analyzing dyadic sequences using a basic Markov model, see \code{\link{Basic_Markov_as_APIM}}.
    \item For estimating the expected number of cases with low- or zero-frequencies for a behavioral coding study, see \code{\link{EstFreq}} or \code{\link{EstTime}}.  
    \item For 'decumulating' a cumulated hazard function, see \code{\link{NonCumHaz}}.
    \item For finding the last occurence of a certain state or event within a sequence (needed to transform sequence data into time-to-event data), see \code{\link{LastOccur}}.
  }
}
\author{Peter Fuchs}
\keyword{package}
