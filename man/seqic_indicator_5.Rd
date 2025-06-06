% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seqic_5.R
\name{seqic_indicator_5}
\alias{seqic_indicator_5}
\title{SEQIC Indicator 5 - Alcohol and Drug Screening}
\usage{
seqic_indicator_5(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  unique_incident_id,
  blood_alcohol_content,
  drug_screen,
  groups = NULL,
  calculate_ci = NULL,
  ...
)
}
\arguments{
\item{data}{A data frame containing trauma incident records.}

\item{level}{Column indicating the trauma center designation level (e.g., I,
II, III, IV).}

\item{included_levels}{Character vector indicating what facility levels to
include in the analysis.  Defaults to \code{c("I", "II", "III", "IV")}.}

\item{unique_incident_id}{Unique identifier for each record.}

\item{blood_alcohol_content}{Unquoted column name for blood alcohol
concentration. Numeric. A non-missing value indicates a test was performed.
Values greater than zero are considered positive results.}

\item{drug_screen}{Unquoted column name for the drug screen result. Character
or factor. May contain keywords (e.g., "opioid", "cocaine", "none"). The
keywords used in this function correspond to the National Trauma Data Bank
(NTDB) field values for the corresponding data element.}

\item{groups}{Additional columns passed as a vector of strings to
\code{dplyr::summarize()} via the \code{.by} argument for grouped summaries. Defaults
to \code{NULL}.}

\item{calculate_ci}{If \code{NULL}, 95\% confidence intervals will not be
calculated for the performance estimates.  Otherwise, options of "wilson"
or "clopper-pearson" can be supplied to utilize the corresponding methods
to calculate the confidence intervals for the proportions. Defaults to
\code{NULL}.}

\item{...}{
  Arguments passed on to \code{\link[nemsqar:nemsqa_binomial_confint]{nemsqar::nemsqa_binomial_confint}}
  \describe{
    \item{\code{conf.level}}{Numeric value between 0 and 1 indicating the confidence
level. Defaults to 0.95 (95\% confidence interval).}
    \item{\code{correct}}{Logical, indicating whether to apply continuity correction for
Wilson intervals. Defaults to \code{TRUE}.}
  }}
}
\value{
A tibble summarizing SEQIC Indicator 5a–5d results. Includes
numerator, denominator, and calculated proportion for each measure.
Optionally includes 95\% confidence intervals.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Computes SEQIC Indicator 5a–5d for trauma system quality monitoring. These
indicators measure alcohol and drug screening rates among trauma patients at
trauma level I–IV facilities.
}
\details{
This function:
\itemize{
\item Filters to trauma records at trauma levels I–IV.
\item Deduplicates by \code{unique_incident_id} to ensure one record per
incident.
\item Calculates four sub-measures:
\itemize{
\item {Indicator 5a:} Proportion of patients with a blood
alcohol test performed.
\item {Indicator 5b:} Among those tested, the proportion with
BAC > 0.
\item {Indicator 5c:} Proportion of patients with any recorded
drug screen result.
\item {Indicator 5d:} Among those with a drug result, the
proportion that included a known positive drug (e.g., opioids,
cocaine, THC).
}
\item Matches drug-related terms using regular expressions for a broad set
of known substances. Matching is case-insensitive.
}
}
\note{
Users must ensure input columns are correctly named and contain standardized
values where applicable. Drug screen values should ideally use consistent
naming or be mapped to recognizable substance terms prior to function use.
}
\examples{
# Packages
library(dplyr)
library(traumar)

# Create synthetic test data for Indicators 5a–5d
test_data <- tibble::tibble(
  id = as.character(1:10),
  trauma_level = rep(c("I", "II", "III", "IV", "V"), each = 2),
  bac = c(0.08, NA, 0, 0.02, NA, 0.15, NA, NA, 0, 0),
  drug = c(
    "opioid", "none", "cocaine", "none", NA,
    "benzodiazepine", "alcohol", "thc", "none", NA
  )
)

# Run the indicator function
traumar::seqic_indicator_5(
  data = test_data,
  level = trauma_level,
  unique_incident_id = id,
  blood_alcohol_content = bac,
  drug_screen = drug
) |>
  tidyr::pivot_longer(cols = -1, names_to = "Indicator", values_to =
  "Values")

}
\author{
Nicolas Foss, Ed.D., MS
}
