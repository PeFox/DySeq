% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LogSeq.R
\name{LogSeq}
\alias{LogSeq}
\title{LogSeq}
\usage{
LogSeq(x, delta = 0.5, subgroups = NA, single.case = FALSE)
}
\arguments{
\item{x}{a state.trans object or a list of 4*2 state-transition tables}

\item{delta}{constant added to every cell, required if zero frequencies occur!}

\item{subgroups}{an optional vector containing groupmembership if estimates should be compared between groups}

\item{single.case}{should p-values be computed for single case analysis}
}
\description{
Implementation of Bakeman & Gottman (1997) for sequence analysis.
Kenny, Kashy & Cook (2006) provide further examples.
}
\details{
\itemize{
\item Runs logit models over a multiple number of state-transition tables, see: \code{\link{StateTrans}} 
\item Aggregates coefficients of all logit models and tests them against zero. 
\item If subgroups are defined, coefficients are tested to be different between groups.
\item Print-function displays mean logit-coefficients and p-values. 
}
}
\examples{
\dontrun{
data(CouplesCope)
my.states<-StateExpand(CouplesCope, 2:49, 50:97)
my.trans<-StateTrans(my.states, FALSE)
my.logseq<-LogSeq(my.trans, single.case=TRUE)
my.logseq

plot(my.logseq) # interaction can be plotted

single.LogSeq(my.logseq, 41) # for single case analysis
}
}
\references{
\itemize{
 \item Bakeman, R., & Gottman, J. M. (1997) <DOI: 10.1017/cbo9780511527685 >
 \item Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006) <DOI: 10.1177/1098214007300894>
}
}
