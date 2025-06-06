#' @title Create Nonlinear Probability of Survival Bins
#'
#' @description
#'
#' This function generates nonlinear bins for probability of survival data based
#' on specified thresholds and divisors as specified in Napoli et al. (2017),
#' Schroeder et al. (2019), and Kassar et al. (2016). This function calculates
#' bin statistics, including mean, standard deviation, total alive, total dead,
#' count, and percentage for each bin.
#'
#' @param data A `data.frame` or `tibble` containing the probability of survival
#'   data for a set of patients.
#' @param Ps_col The name of the column containing the survival probabilities
#'   (Ps). Should be numeric on a scale from 0 to 1.
#' @param outcome_col The name of the column containing the outcome data. It
#'   should be binary, with values indicating patient survival. A value of `1`
#'   should represent "alive" (survived), while `0` should represent "dead" (did
#'   not survive).  `TRUE/FALSE` are accepted as well. Ensure the column
#'   contains only these possible values.
#' @param group_vars Optional grouping variables for bin statistics
#'   calculations. These should be specified as quoted column names.
#' @param divisor1 A parameter to control the width of the probability of
#'   survival range bins. Affects the creation of step sizes for the beginning
#'   of each bin range. Defaults to `5`.
#' @param divisor2 A parameter to control the width of the probability of
#'   survival range bins. Affects the creation of step sizes for the beginning
#'   of each bin range. Defaults to `5`.
#' @param threshold_1 A parameter to decide where data indices will begin to
#'   create step sizes. Defaults to `0.9`.
#' @param threshold_2 A parameter to decide where data indices will end to
#'   create step sizes. Defaults to `0.99`.
#'
#' @details
#' Like other statistical computing functions, `nonlinear_bins()` is happiest
#' without missing data.  It is best to pass complete probability of survival
#' and outcome data to the function for optimal performance. With smaller
#' datasets, this is especially helpful.  However, `nonlinear_bins()` will throw
#' a warning about missing values, if any exist in `Ps_col` and/or
#' `outcome_col`.
#'
#' `nonlinear_bins()` assumes `Ps_col` contains probabilities derived from
#' real-world inputs for the Trauma Injury Severity Score (TRISS) model.
#' Synthetic or low-variability data (especially with small sample sizes) may
#' not reflect the distribution of TRISS-derived survival probabilities. This
#' can result in unstable estimates or function failure due to insufficient
#' dispersion. With small sample sizes, it may be important to use smaller
#' values with the divisor arguments and adjust the thresholds (based on the
#' distribution of the `Ps_col` values) to create bins that better accommodate
#' the data.
#'
#' By default, `nonlinear_bins()` derives bin cut points from the full dataset’s
#' distribution. This ensures comparability across groups when `group_vars` is
#' used. To tailor binning to a specific group (e.g., a single hospital), filter
#' the dataset to that subgroup before calling `nonlinear_bins()`. The function
#' will then compute bins and related statistics using only that subset’s
#' `Ps_col` distribution. When `group_vars` is used, and ff a group lacks
#' observations within one or more bins, `rm_bin_summary()` will compute
#' statistics only for the bins that contain data. Bins with no observations are
#' excluded from the summary for that group.
#'
#' @returns A list with two elements:
#'   - `intervals`: A vector defining bin boundaries for probability of
#'        survival.
#'   - `bin_stats`: A `tibble` containing:
#'     - `bin_number`: Bin index.
#'     - `bin_start`, `bin_end`: Bin range.
#'     - `mean`, `sd`: Mean and standard deviation of `Ps_col` within the bin.
#'     - `Pred_Survivors_b`, `Pred_Deaths_b`: Predicted counts of survivors and
#'        decedents, respectively.
#'     - `AntiS_b`, `AntiM_b`: Anticipated proportion survived, and deceased,
#'        respectively.
#'     - `alive`, `dead`: Count of observed survivors and non-survivors.
#'     - `count`: Total records in the bin.
#'     - `percent`: Percentage of total records within each bin.
#'
#' @note
#'
#' This function will produce the most reliable and interpretable results when
#' using a dataset that has one row per patient, with each column being a
#' feature.
#'
#' The `mean` and `AntiS_b` are approximately equivalent in this context.  They
#' are kept in the output for clarity.
#'
#' @export
#'
#' @references
#'
#' Kassar, O.M., Eklund, E.A., Barnhardt, W.F., Napoli, N.J., Barnes, L.E.,
#' Young, J.S. (2016). Trauma survival margin analysis: A dissection of trauma
#' center performance through initial lactate. The American Surgeon, 82(7),
#' 649-653. <doi:10.1177/000313481608200733>
#'
#' Napoli, N. J., Barnhardt, W., Kotoriy, M. E., Young, J. S., & Barnes, L. E.
#' (2017). Relative mortality analysis: A new tool to evaluate clinical
#' performance in trauma centers. IISE Transactions on Healthcare Systems
#' Engineering, 7(3), 181–191. <doi:10.1080/24725579.2017.1325948>
#'
#' Schroeder, P. H., Napoli, N. J., Barnhardt, W. F., Barnes, L. E., & Young, J.
#' S. (2018). Relative mortality analysis of the “golden hour”: A comprehensive
#' acuity stratification approach to address disagreement in current literature.
#' Prehospital Emergency Care, 23(2), 254–262.
#' <doi:10.1080/10903127.2018.1489021>
#'
#' @seealso [probability_of_survival()], [rmm()], and [rm_bin_summary()]
#'
#' @examples
#' # Generate example data
#' set.seed(123)
#'
#' # Parameters
#' # Total number of patients
#' n_patients <- 5000
#'
#' # Arbitrary group labels
#' groups <- sample(x = LETTERS[1:2], size = n_patients, replace = TRUE)
#'
#' # Trauma types
#' trauma_type_values <- sample(
#'   x = c("Blunt", "Penetrating"),
#'   size = n_patients,
#'   replace = TRUE
#' )
#'
#' # RTS values
#' rts_values <- sample(
#'   x = seq(from = 0, to = 7.8408, by = 0.005),
#'   size = n_patients,
#'   replace = TRUE
#' )
#'
#' # patient ages
#' ages <- sample(
#'   x = seq(from = 0, to = 100, by = 1),
#'   size = n_patients,
#'   replace = TRUE
#' )
#'
#' # ISS scores
#' iss_scores <- sample(
#'   x = seq(from = 0, to = 75, by = 1),
#'   size = n_patients,
#'   replace = TRUE
#' )
#'
#' # Generate survival probabilities (Ps)
#' Ps <- traumar::probability_of_survival(
#'   trauma_type = trauma_type_values,
#'   age = ages,
#'   rts = rts_values,
#'   iss = iss_scores
#' )
#'
#' # Simulate survival outcomes based on Ps
#' survival_outcomes <- rbinom(n_patients, size = 1, prob = Ps)
#'
#' # Create data frame
#' data <- data.frame(Ps = Ps, survival = survival_outcomes, groups = groups) |>
#'   dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))
#'
#' # Apply the nonlinear_bins function
#' results <- nonlinear_bins(
#'   data = data,
#'   Ps_col = Ps,
#'   outcome_col = survival,
#'   divisor1 = 4,
#'   divisor2 = 4,
#'   threshold_1 = 0.9,
#'   threshold_2 = 0.99
#' )
#'
#' # View results
#' results$intervals
#' results$bin_stats
#'
#' # Example with grouping by a categorical variable
#'
#' # Run the function using a single grouping variable
#' results_grouped <- nonlinear_bins(
#'   data,
#'   Ps_col = Ps,
#'   outcome_col = survival,
#'   group_vars = "groups"
#' )
#'
#' # View grouped results
#' results_grouped$bin_stats
#'
#' @author Nicolas Foss, Ed.D, MS, original implementation in MATLAB by Nicholas
#'   J. Napoli, Ph.D., MS
#'
nonlinear_bins <- function(
  data,
  Ps_col,
  outcome_col,
  group_vars = NULL,
  divisor1 = 5,
  divisor2 = 5,
  threshold_1 = 0.9,
  threshold_2 = 0.99
) {
  # Validation checks using `cli` for robust error messaging:
  # Ensures the input data is a data frame or tibble.
  if (!is.data.frame(data) && !tibble::is_tibble(data)) {
    cli::cli_abort("The input data must be a data frame or tibble.")
  }

  # Ensure Ps_col and outcome_col arguments are provided with tailored error messages
  if (missing(Ps_col) && missing(outcome_col)) {
    cli::cli_abort(
      "Both {.var Ps_col} and {.var outcome_col} arguments must be provided."
    )
  } else if (missing(Ps_col)) {
    cli::cli_abort("The {.var Ps_col} argument must be provided.")
  } else if (missing(outcome_col)) {
    cli::cli_abort("The {.var outcome_col} argument must be provided.")
  }

  # Check if Ps column is numeric

  # dplyr::pull the Ps data
  Ps_check <- data |> dplyr::pull({{ Ps_col }})

  # check the Ps_check remains continuous
  if (!is.numeric(Ps_check)) {
    cli::cli_abort("The {.var Ps_col} must contain numeric values.")
  }

  # Check if Ps column is continuous (values between 0 and 1)
  if (any(Ps_check < 0 | Ps_check > 1, na.rm = TRUE)) {
    cli::cli_abort(
      "The probability of survival (Ps) values must be between 0 and 1."
    )
  }

  if (any(is.na(Ps_check))) {
    cli::cli_warn(
      "Missing values detected in {.var Ps_col}; please apply an appropriate treatment to the missings and rerun {.fn nonlinear_bins}."
    )
  }

  # Pull and check the outcome column
  binary_data <- data |> dplyr::pull({{ outcome_col }})

  # Ensure the column is either logical or numeric
  if (!is.logical(binary_data) && !is.numeric(binary_data)) {
    cli::cli_abort(
      "The {.var outcome_col} must be of type logical (TRUE/FALSE) or numeric (1/0)."
    )
  }

  # Get unique non-missing values
  non_missing <- stats::na.omit(binary_data)

  # Validate type and values
  if (is.logical(binary_data)) {
    # Logical vector: ensure only TRUE/FALSE (no coercion needed)
    invalid_vals <- setdiff(unique(non_missing), c(TRUE, FALSE))
    if (length(invalid_vals) > 0) {
      cli::cli_abort(
        "The {.var outcome_col} contains invalid logical values: {.val {invalid_vals}}."
      )
    }
  } else if (is.numeric(binary_data)) {
    # Numeric vector: ensure strictly 0 or 1
    invalid_vals <- setdiff(unique(non_missing), c(0, 1))
    if (length(invalid_vals) > 0) {
      cli::cli_abort(
        "The {.var outcome_col} contains numeric values other than 0 and 1: {.val {invalid_vals}}."
      )
    }
  } else {
    # Not logical or numeric
    cli::cli_abort(
      "The {.var outcome_col} must be either logical (TRUE/FALSE) or numeric (1/0)."
    )
  }

  # Warn if missing
  if (any(is.na(binary_data))) {
    cli::cli_warn(
      "Missing values detected in {.var outcome_col}; please apply an appropriate treatment to the missings and rerun {.fn nonlinear_bins}."
    )
  }

  # Check if all elements in group_vars are strings (i.e., character vectors)
  if (!all(sapply(group_vars, is.character))) {
    cli::cli_abort(c(
      "All elements in {.var group_vars} must be strings.",
      "i" = "You passed a {.cls {class(group_vars)}} variable to {.var group_vars}."
    ))
  }

  # Check if all group_vars exist in the data
  if (!all(group_vars %in% names(data))) {
    invalid_vars <- group_vars[!group_vars %in% names(data)]
    cli::cli_abort(
      "The following group variable(s) are not valid columns in the data: {paste(invalid_vars, collapse = ', ')}"
    )
  }

  # Treat the column-names-as-strings as symbols
  if (!is.null(group_vars)) {
    group_vars <- rlang::syms(group_vars)
  }

  # Select and sort the column
  survival_data <- data |> dplyr::pull({{ Ps_col }}) |> sort()
  total <- length(survival_data)

  # length of non-missing `Ps_col` must be >= 2
  if (na.omit(total) < 2) {
    cli::cli_abort(c(
      "At least two non-missing values are required in {.var Ps_col} to compute survival probability intervals.",
      "v" = "Ensure {.var Ps_col} contains at least two valid, non-missing numeric entries."
    ))
  }

  # Step 1: Find indices for level thresholds
  loc_9A <- which(survival_data > threshold_1) # Everything above 0.9 or other threshold
  loc_9B <- which(survival_data > threshold_2) # Everything above 0.99 or other threshold
  loc_9C <- which(survival_data > threshold_1 & survival_data <= threshold_2) # Between 0.9 and 0.99 or other thresholds

  # Step 2: Define step sizes based on the data
  step1 <- round(suppressWarnings(min(loc_9A, na.rm = TRUE)) / divisor1)
  step2 <- round(length(loc_9C) / divisor2)

  # Step 3: Define intervals
  # Check that loc_9A and loc_9B are finite before using them in seq()
  if (
    suppressWarnings(
      is.finite(min(loc_9A)) && is.finite(min(loc_9B))
    )
  ) {
    # Defensive approach to control for potential error when `Ps_col`
    # is not sufficiently dispersed and/or has a low sample size.
    attempt <- try(
      unique(
        c(
          seq(
            from = 1,
            to = suppressWarnings(min(loc_9A, na.rm = TRUE)),
            by = step1
          ),
          seq(
            from = suppressWarnings(min(loc_9A, na.rm = TRUE)),
            to = suppressWarnings(min(loc_9B, na.rm = TRUE)),
            by = step2
          ),
          suppressWarnings(max(loc_9B, na.rm = TRUE))
        )
      ),
      silent = TRUE
    )

    if (inherits(attempt, "try-error")) {
      cli::cli_abort(c(
        "Unable to calculate valid step sizes for defining survival probability intervals due to non-finite values.",
        "i" = "This typically occurs when {.var Ps_col} has low variability or the sample size is too small to support the requested binning.",
        "v" = "Try reducing {.var divisor1} and {.var divisor2} to create fewer, broader bins.",
        "v" = "Examine the distribution of {.var Ps_col} and consider adjusting {.var threshold_1} and {.var threshold_2} to better match the data.",
        "v" = "Increasing the sample size may also improve dispersion and allow for more stable interval boundaries."
      ))
    } else {
      # If `Ps_col` meets distributional assumptions, proceed
      len <- attempt
    }
  } else {
    # If either of the parts of the interval calculation are not finite throw an
    # error
    cli::cli_abort(
      c(
        "Unable to calculate valid step sizes for defining survival probability intervals due to non-finite values.",
        "i" = "This typically occurs when {.var Ps_col} has low variability or the sample size is too small to support the requested binning.",
        "v" = "Try reducing {.var divisor1} and {.var divisor2} to create fewer, broader bins.",
        "v" = "Examine the distribution of {.var Ps_col} and consider adjusting {.var threshold_1} and {.var threshold_2} to better match the data.",
        "v" = "Increasing the sample size may also improve dispersion and allow for more stable interval boundaries."
      )
    )
  }

  # Generate intervals based on these positions
  intervals <- unique(survival_data[len])

  # Step 4: Bin statistics

  # Apply binning to each group separately
  data <- data |>
    dplyr::mutate(
      bin_number = .bincode(
        {{ Ps_col }},
        breaks = intervals,
        include.lowest = TRUE
      ),
      bin_start = intervals[bin_number], # Start of the bin
      bin_end = intervals[bin_number + 1] # End of the bin
    )

  # Optionally group data by dynamic group_vars
  # Or run the bin statistics on the whole dataset
  if (!is.null(group_vars)) {
    grouped_stats <- data |>
      dplyr::group_by(!!!group_vars, bin_number, bin_start, bin_end) |>
      dplyr::summarize(
        mean = mean({{ Ps_col }}, na.rm = TRUE),
        sd = stats::sd({{ Ps_col }}, na.rm = TRUE),
        Pred_Survivors_b = sum({{ Ps_col }}, na.rm = TRUE),
        Pred_Deaths_b = sum(1 - {{ Ps_col }}, na.rm = TRUE),
        AntiS_b = dplyr::if_else(
          dplyr::n() > 0,
          Pred_Survivors_b / dplyr::n(),
          NA_real_
        ),
        AntiM_b = dplyr::if_else(
          dplyr::n() > 0,
          Pred_Deaths_b / dplyr::n(),
          NA_real_
        ),
        alive = sum({{ outcome_col }} == 1, na.rm = TRUE),
        dead = sum({{ outcome_col }} == 0, na.rm = TRUE),
        count = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::ungroup() |>
      dplyr::group_by(!!!group_vars) |>
      dplyr::mutate(
        percent = count / sum(count, na.rm = TRUE)
      ) |>
      dplyr::ungroup()
  } else {
    grouped_stats <- data |>
      dplyr::group_by(bin_number, bin_start, bin_end) |>
      dplyr::summarize(
        mean = mean({{ Ps_col }}, na.rm = TRUE),
        sd = stats::sd({{ Ps_col }}, na.rm = TRUE),
        Pred_Survivors_b = sum({{ Ps_col }}, na.rm = TRUE),
        Pred_Deaths_b = sum(1 - {{ Ps_col }}, na.rm = TRUE),
        AntiS_b = dplyr::if_else(
          dplyr::n() > 0,
          Pred_Survivors_b / dplyr::n(),
          NA_real_
        ),
        AntiM_b = dplyr::if_else(
          dplyr::n() > 0,
          Pred_Deaths_b / dplyr::n(),
          NA_real_
        ),
        alive = sum({{ outcome_col }} == 1, na.rm = TRUE),
        dead = sum({{ outcome_col }} == 0, na.rm = TRUE),
        count = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        percent = count / sum(count, na.rm = TRUE)
      )
  }

  # Return a list with intervals and the bin statistics
  return(list(intervals = intervals, bin_stats = grouped_stats))
}
