% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_search.R
\name{BIC_smooth_spline}
\alias{BIC_smooth_spline}
\title{Calculate the Bayesian Information Criterion for a smooth.spline}
\usage{
BIC_smooth_spline(fittedSmoothSpline)
}
\arguments{
\item{fittedSmoothSpline}{A fitted \code{\link[stats]{smooth.spline}}}
}
\value{
The BIC value.
}
\description{
Calculate the Bayesian Information Criterion (\emph{BIC}) for a fitted \code{\link[stats]{smooth.spline}}. The smaller the BIC, the better the spline fit.
}
