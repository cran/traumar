#' @title SEQIC Indicator 11 – Overtriage for Minor Trauma Patients
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculates SEQIC Indicator 11, which estimates the proportion of minor trauma
#' patients who were transferred into a trauma center and remained in the
#' Emergency Department for less than 24 hours. This indicator is designed to
#' identify potential overtriage events within the trauma system. Minor trauma
#' patients are identified using the Injury Severity Score (ISS < 9). Patients
#' must not have been transferred out and must have been received at a trauma
#' center level included in `included_levels`.
#'
#' @inheritParams seqic_indicator_1
#' @inheritParams seqic_indicator_4
#' @inheritParams seqic_indicator_6
#' @inheritParams seqic_indicator_10
#' @inheritDotParams nemsqar::nemsqa_binomial_confint conf.level correct
#'
#' @returns A tibble summarizing the numerator, denominator, and proportion of
#'   overtriaged patients (Indicator 11), with optional 95% confidence
#'   intervals.
#'
#' @details
#'
#' This function:
#' \itemize{
#'   \item Filters the dataset to include only patients treated at trauma
#'   centers designated Level I through IV.
#'   \item Excludes patients transferred out and retains only those received by
#'   the trauma center.
#'   \item Deduplicates incident-level records using `unique_incident_id`.
#'   \item Classifies patients as low-risk based on the  Injury Severity Score
#'   (ISS < 9).
#'   \item Flags low-risk patients who were discharged from the ED in under 24
#'   hours.
#'   \item Stratifies results by one or more user-defined grouping variables.
#'   \item Returns a summarized tibble with the number of eligible low-risk
#'   short-stay discharges (numerator), all received patients meeting inclusion
#'   criteria (denominator), and the resulting proportion.
#'   \item Optionally includes 95% confidence intervals if `calculate_ci` is
#'   specified.
#' }
#'
#' Users must ensure appropriate column names are passed and data is
#' pre-processed to include the necessary fields without missing critical
#' identifiers or timestamps.
#'
#' @examples
#' # Packages
#' library(dplyr)
#' library(traumar)
#'
#' # Simulated data for SEQIC Indicator 11
#' test_data <- tibble::tibble(
#'   id = as.character(1:10),
#'   trauma_level = c("I", "II", "III", "IV", "II", "I", "IV", "III", "II",
#'   "I"),
#'   transferred_out = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE,
#'   FALSE, FALSE),
#'   received = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
#'   iss = c(4, 8, 10, 6, 5, 7, 6, 15, 3, 2),
#'   ed_LOS = c(6, 20, 30, 18, 8, 5, 22, 40, 2, 4),
#'   region = rep(c("East", "West"), each = 5)
#' )
#'
#' # Run the function
#' traumar::seqic_indicator_11(
#'   data = test_data,
#'   level = trauma_level,
#'   included_levels = c("I", "II", "III", "IV"),
#'   transfer_out_indicator = transferred_out,
#'   receiving_indicator = received,
#'   unique_incident_id = id,
#'   iss = iss,
#'   ed_LOS = ed_LOS,
#'   groups = "region",
#'   calculate_ci = "clopper-pearson"
#' )
#'
#' @references
#'
#' Roden-Foreman JW, Rapier NR, Yelverton L, Foreman ML. Asking a Better
#' Question: Development and Evaluation of the Need For Trauma Intervention
#' (NFTI) Metric as a Novel Indicator of Major Trauma. J Trauma Nurs. 2017
#' May/Jun;24(3):150-157. doi: 10.1097/JTN.0000000000000283. PMID: 28486318.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export

seqic_indicator_11 <- function(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  transfer_out_indicator,
  receiving_indicator,
  unique_incident_id,
  iss,
  ed_LOS,
  groups = NULL,
  calculate_ci = NULL,
  ...
) {
  ###___________________________________________________________________________
  ### Data validation
  ###___________________________________________________________________________

  # Ensure input is a data frame or tibble
  if (!is.data.frame(data) && !tibble::is_tibble(data)) {
    cli::cli_abort(c(
      "{.var data} must be a data frame or tibble.",
      "i" = "You provided an object of class {.cls {class(data)}}."
    ))
  }

  # make the `level` column accessible for validation
  level_check <- tryCatch(
    {
      data |> dplyr::pull({{ level }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var level}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_11())
      )
    }
  )
  if (!is.character(level_check) && !is.factor(level_check)) {
    cli::cli_abort(c(
      "{.var level} must be character or factor.",
      "i" = "Provided class: {.cls {class(level_check)}}."
    ))
  }

  # make the `unique_incident_id` column accessible for validation
  unique_incident_id_check <- tryCatch(
    {
      data |> dplyr::pull({{ unique_incident_id }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var unique_incident_id}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_11())
      )
    }
  )

  # Validate `unique_incident_id` to ensure it's either character or factor.
  if (
    !is.character(unique_incident_id_check) &&
      !is.factor(unique_incident_id_check) &&
      !is.numeric(unique_incident_id_check)
  ) {
    cli::cli_abort(
      c(
        "{.var unique_incident_id} must be of class {.cls character}, {.cls numeric}, or {.cls factor}.",
        "i" = "{.var unique_incident_id} was an object of class {.cls {class(unique_incident_id_check)}}."
      )
    )
  }

  # Validate that `iss` is numeric.
  iss_check <- tryCatch(
    {
      data |> dplyr::pull({{ iss }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var iss}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_11())
      )
    }
  )
  if (!is.numeric(iss_check)) {
    cli::cli_abort(c(
      "{.var iss} must be numeric when provided.",
      "i" = "Provided class: {.cls {class(iss_check)}}."
    ))
  }

  # Validate that `transfer_out_indicator` is character, factor, or logical.
  transfer_out_indicator_check <- tryCatch(
    {
      data |> dplyr::pull({{ transfer_out_indicator }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var transfer_out_indicator}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_11())
      )
    }
  )
  if (
    !is.character(transfer_out_indicator_check) &&
      !is.factor(transfer_out_indicator_check) &&
      !is.logical(transfer_out_indicator_check)
  ) {
    cli::cli_abort(
      c(
        "{.var transfer_out_indicator} must be of class {.cls character}, {.cls factor}, or {.cls logical}.",
        "i" = "{.var transfer_out_indicator} was an object of class {.cls {class(transfer_out_indicator_check)}}."
      )
    )
  }

  # Validate that `receiving_indicator` is character, factor, or logical.
  receiving_indicator_check <- tryCatch(
    {
      data |> dplyr::pull({{ receiving_indicator }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var receiving_indicator}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_11())
      )
    }
  )
  if (
    !is.character(receiving_indicator_check) &&
      !is.factor(receiving_indicator_check) &&
      !is.logical(receiving_indicator_check)
  ) {
    cli::cli_abort(
      c(
        "{.var receiving_indicator} must be of class {.cls character}, {.cls factor}, or {.cls logical}.",
        "i" = "{.var receiving_indicator} was an object of class {.cls {class(receiving_indicator_check)}}."
      )
    )
  }

  # Validate `ed_LOS`
  ed_los_check <- # Validate `ed_LOS`
    ed_los_check <- tryCatch(
      {
        data |> dplyr::pull({{ ed_LOS }})
      },
      error = function(e) {
        cli::cli_abort(
          "It was not possible to validate {.var ed_LOS}, please check this column in the function call.",
          call = rlang::expr(seqic_indicator_11())
        )
      }
    )
  if (!is.numeric(ed_los_check)) {
    cli::cli_abort(
      c(
        "{.var ed_LOS} must be of class {.cls numeric}.",
        "i" = "{.var ed_LOS} was an object of class {.cls {class(ed_los_check)}}."
      )
    )
  }

  # Check if all elements in groups are strings (i.e., character vectors)
  if (!is.null(groups)) {
    if (!is.character(groups)) {
      cli::cli_abort(c(
        "All elements in {.var groups} must be strings.",
        "i" = "You passed an object of class {.cls {class(groups)}} to {.var groups}."
      ))
    }
  }

  # Check if all groups exist in the `data`
  if (!all(groups %in% names(data))) {
    invalid_vars <- groups[!groups %in% names(data)]
    cli::cli_abort(
      "Invalid grouping variable(s): {paste(invalid_vars, collapse = ', ')}"
    )
  }

  # Validate confidence interval method
  if (!is.null(calculate_ci)) {
    attempt <- try(
      match.arg(calculate_ci, choices = c("wilson", "clopper-pearson")),
      silent = TRUE
    )
    if (inherits(attempt, "try-error")) {
      cli::cli_abort(c(
        "If {.var calculate_ci} is not NULL, it must be {.val wilson} or {.val clopper-pearson}.",
        "i" = "Provided value: {.val {calculate_ci}}"
      ))
    }
    calculate_ci <- attempt
  }

  # Validate the `included_levels` argument
  if (
    !is.character(included_levels) &&
      !is.numeric(included_levels) &&
      !is.factor(included_levels)
  ) {
    cli::cli_abort(
      c(
        "{.var included_levels} must be of class {.cls character}, {.cls factor}, or {.cls numeric}.",
        "i" = "{.var included_levels} was an object of class {.cls {class(included_levels)}}."
      )
    )
  }

  ###___________________________________________________________________________
  ### Data preparation
  ###___________________________________________________________________________

  # Dynamically classify patients using either ISS or NFTI logic
  data_prep <- data |>
    dplyr::filter(
      {{ level }} %in% included_levels,
      {{ transfer_out_indicator }} %in% c("No", FALSE),
      {{ receiving_indicator }} %in% c("Yes", TRUE)
    ) |>
    dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>
    dplyr::mutate(
      # Patients who clearly did not require activation (ISS < 9)
      minor_trauma = {{ iss }} < 9,
      los_less_24_hrs = {{ ed_LOS }} < 1440
    )

  ###___________________________________________________________________________
  ### Calculations
  ###___________________________________________________________________________

  # state 11
  seqic_11 <- data_prep |>
    dplyr::summarize(
      numerator_11 = sum(
        minor_trauma &
          los_less_24_hrs,
        na.rm = TRUE
      ),
      denominator_11 = dplyr::n(),
      seqic_11 = dplyr::if_else(
        denominator_11 > 0,
        numerator_11 / denominator_11,
        NA_real_
      ),
      .by = {{ groups }}
    )

  # Optionally calculate confidence intervals for the proportions:
  # - If `calculate_ci` is provided, apply a binomial confidence interval method (Wilson or Clopper-Pearson).
  # - `nemsqa_binomial_confint()` calculates the confidence intervals for the proportion.
  # - Select only the relevant columns and rename the CI columns for clarity.
  if (!is.null(calculate_ci)) {
    seqic_11 <- seqic_11 |>
      dplyr::bind_cols(
        nemsqar::nemsqa_binomial_confint(
          data = seqic_11,
          x = numerator_11, # Number of successes (numerator).
          n = denominator_11, # Number of trials (denominator).
          method = calculate_ci, # Confidence interval calculation method (e.g., "wilson").
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(lower_ci_11 = lower_ci, upper_ci_11 = upper_ci) # Rename CI columns.
      )
  }

  # Assign a label to indicate whether the data represents population or sample-level results:
  # - If no grouping is applied, label the data as "Population/Sample".
  if (is.null(groups)) {
    seqic_11 <- seqic_11 |>
      tibble::add_column(data = "population/sample", .before = "numerator_11") # Add the label column.
  } else if (!is.null(groups)) {
    seqic_11 <- seqic_11 |>
      dplyr::arrange(!!!rlang::syms(groups))
  }

  return(seqic_11)
}
