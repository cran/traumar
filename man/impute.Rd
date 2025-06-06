% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute.r
\name{impute}
\alias{impute}
\title{Impute Numeric Column Values}
\usage{
impute(
  x,
  focus = c("skew", "missing"),
  method = c("winsorize", "iqr", "mean", "median"),
  percentile = NULL
)
}
\arguments{
\item{x}{A numeric vector to be cleaned.}

\item{focus}{A character string indicating the focus. Options are:
\itemize{
\item \code{"skew"}: Handle extreme values using percentile or
IQR methods (default).
\item \code{"missing"}: Impute missing values.
}}

\item{method}{A character string specifying the method:
\itemize{
\item For \code{focus = "skew"}:
\itemize{
\item \code{"winsorize"}: Replace values outside specified percentiles (default).
\item \code{"iqr"}: Use IQR to limit extreme values.
}
\item For \code{focus = "missing"}:
\itemize{
\item \code{"mean"}: Replace missing values with the mean.
\item \code{"median"}: Replace missing values with the median.
}
}}

\item{percentile}{A numeric value (percentile > 0) for winsorization.
If not provided, defaults to 0.01 and 0.99.}
}
\value{
A numeric vector with cleaned or imputed values.
}
\description{
Cleans numeric columns by handling extreme values or imputing
missing values. The function supports two main focuses: handling skewed
distributions or imputing missing data.
}
\examples{
x <- c(1, 2, 3, 100, 200, NA)
# Winsorize to 1\% and 99\%
impute(x, focus = "skew", method = "winsorize")

# Replace missing values with the mean
impute(x, focus = "missing", method = "mean")

}
