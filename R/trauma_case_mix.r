#' View the Current Patient Population Case Mix Compared to the Major Trauma
#' Study Case Mix
#'
#' This function compares the current patient population's case mix (based on
#' probability of survival, Ps) to the MTOS case mix by binning patients into
#' specific Ps ranges. It returns the fraction of patients in each range and
#' compares it to the MTOS distribution. For more information on the methods used
#' in these calculations, please see Flora (1978) and Boyd et al. (1987).
#'
#' @param df A data frame containing patient data.
#' @param Ps_col The name of the column containing the probability of survival
#'   (Ps) values.
#' @param outcome_col The name of the column containing the binary outcome data
#'   (valid values are 1 or TRUE for alive, 0 or FALSE for dead).
#'
#' @details The function checks whether the `outcome_col` contains exactly two
#' unique values representing a binary outcome. It also ensures that `Ps_col`
#' contains numeric values within the range 0 to 100. If any values exceed 1,
#' they are converted to decimal format. The patients are then grouped into
#' predefined Ps ranges, and the function compares the fraction of patients in
#' each range with the MTOS case mix distribution.
#'
#' @return A data frame containing the Ps ranges, the fraction of patients in
#'   each range in the current population, and the MTOS distribution for each
#'   range.
#'
#' @examples
#' # Generate example data with high negative skewness
#' set.seed(123)
#'
#' # Parameters
#' n_patients <- 10000  # Total number of patients
#'
#' # Generate survival probabilities (Ps) using a logistic distribution
#' set.seed(123)  # For reproducibility
#' Ps <- plogis(rnorm(n_patients, mean = 2, sd = 1.5))  # Skewed towards higher values
#'
#' # Simulate survival outcomes based on Ps
#' survival_outcomes <- rbinom(n_patients, size = 1, prob = Ps)
#'
#' # Create data frame
#' data <- data.frame(Ps = Ps, survival = survival_outcomes) |>
#' dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))
#'
#' # Compare the current case mix with the MTOS case mix
#' trauma_case_mix(data, Ps_col = Ps, outcome_col = death)
#'
#' @export
#'
#' @author Nicolas Foss, Ed.D., MS
#'
trauma_case_mix <- function(df, Ps_col, outcome_col) {

  # Evaluate column names passed in
  Ps_col <- rlang::enquo(Ps_col)
  outcome_col <- rlang::enquo(outcome_col)

  # Check if the dataframe is valid
  if (!is.data.frame(df)) {
    cli::cli_abort("The first argument must be a dataframe.")
  }

  # Check if the outcome_col is binary
  binary_data <- df |>
    dplyr::select(!!outcome_col) |>
    dplyr::pull()

  binary_col <- length(unique(stats::na.omit(binary_data)))

  if(binary_col > 2 | binary_col < 2) {

    cli::cli_abort("The {.var outcome_col} must be binary, such as 1/0, TRUE/FALSE, 'Yes'/'No', etc.  Check the column and ensure a binary structure.")

  }

  # Check if Ps column is numeric
  Ps_data <- df |> dplyr::pull(!!Ps_col)
  if (!is.numeric(Ps_data)) {
    cli::cli_abort("The probability of survival (Ps) column must be numeric.")
  }

  # Check if Ps column is continuous (values between 0 and 1 or 0 and 100)
  if (any(Ps_data < 0 | Ps_data > 100, na.rm = T)) {
    cli::cli_abort("The input probability of survival (Ps) values must be between 0 and 100.")
  }

  # Notify the user if any conversions were made
  if (any(Ps_data > 1, na.rm = T)) {
    cli::cli_alert_info("Some Probability of survival (Ps) values will be divided by 100 to convert to decimal format.")
  }

  # Convert ##.## format to decimal if needed (rowwise operation but vectorized)
  Ps_data <- dplyr::if_else(Ps_data > 1, Ps_data / 100, Ps_data)

  # Check if Ps column is continuous (values between 0 and 1 or 0 and 100)
  if (any(Ps_data < 0 | Ps_data > 1, na.rm = T)) {
    cli::cli_abort("The rescaled probability of survival (Ps) values must be between 0 and 1.")
  }

  Ps_range_order <- c("0.96 - 1.00", "0.91 - 0.95", "0.76 - 0.90", "0.51 - 0.75", "0.26 - 0.50", "0.00 - 0.25")

  # Define the MTOS Ps distribution
  MTOS_distribution <- tibble::tibble(Ps_range = factor(c("0.96 - 1.00",
                                                          "0.91 - 0.95",
                                                          "0.76 - 0.90",
                                                          "0.51 - 0.75",
                                                          "0.26 - 0.50",
                                                          "0.00 - 0.25"
  ), levels = Ps_range_order
  ),
  MTOS_distribution = c(0.842, 0.053, 0.052, 0, 0.043, 0.01)
  )

  # Bin patients into Ps ranges and calculate current fractions
  fractions_set <- df |>
    dplyr::filter(!is.na(!!Ps_col) & !is.na(!!outcome_col)) |>
    dplyr::mutate(
      !!Ps_col := Ps_data,
      Ps_range = dplyr::case_when(
        !!Ps_col >= 0.96 ~ "0.96 - 1.00",
        !!Ps_col >= 0.91 ~ "0.91 - 0.95",
        !!Ps_col >= 0.76 ~ "0.76 - 0.90",
        !!Ps_col >= 0.51 ~ "0.51 - 0.75",
        !!Ps_col >= 0.26 ~ "0.26 - 0.50",
        TRUE ~ "0.00 - 0.25"
      )
    ) |>
    dplyr::summarize(current_fraction = dplyr::n() / nrow(df),
                     .by = Ps_range
    ) |>
    dplyr::left_join(MTOS_distribution, by = "Ps_range") |>
    dplyr::arrange(Ps_range)

  return(fractions_set)

}
