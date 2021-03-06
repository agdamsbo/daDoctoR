% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rep_olr.R
\name{rep_olr}
\alias{rep_olr}
\title{A repeated ordinal logistic regression function}
\usage{
rep_olr(meas, vars, ci = FALSE, data, ctp = 0.1)
}
\arguments{
\item{meas}{Effect meassure. Input as c() of columnnames, use dput().}

\item{vars}{variables in model. Input as c() of columnnames, use dput().}

\item{ci}{flag to get results as OR with 95 percent confidence interval.}

\item{data}{data.frame to pull variables from.}

\item{ctp}{cut point for drop/include. Standard 0.1.}
}
\description{
For bivariate analyses. The confint() function is rather slow, causing the whole function to hang when including many predictors and calculating the ORs with CI.
}
\keyword{olr}
