#' Compute z-scores
#'
#' Computes standardized (z-scored) values, handling NAs gracefully.
#'
#' @param x Numeric vector to standardize.
#'
#' @return Numeric vector of z-scores. NA values in input remain NA in output.
#'   Returns a vector of NAs if standard deviation is zero.
#'
#' @examples
#' zscore(c(1, 2, 3, 4, 5))
#' zscore(c(1, NA, 3))
#'
#' @export
zscore <- function(x) {
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector.", call. = FALSE)
  }
  mu <- mean(x, na.rm = TRUE)
  sigma <- stats::sd(x, na.rm = TRUE)
  if (is.na(sigma) || sigma == 0) {
    warning("Standard deviation is zero or NA; returning vector of NAs.", call. = FALSE)
    return(rep(NA_real_, length(x)))
  }
  (x - mu) / sigma
}

#' Assign quadrant labels based on z-scored PSI and CS
#'
#' Maps communities to the four quadrants of the ThemeScope theoretical space
#' based on their z-scored Prototypical Salience Index (PSI) and Concreteness
#' Score (CS).
#'
#' @param psi_z Numeric vector of z-scored PSI values.
#' @param cs_z Numeric vector of z-scored CS values.
#'
#' @return Factor with levels:
#'   \itemize{
#'     \item \code{"Stable Core"}: high PSI, high CS (++)
#'     \item \code{"Ideological Core"}: high PSI, low CS (+-)
#'     \item \code{"Emerging Practices"}: low PSI, high CS (-+)
#'     \item \code{"Latent Representations"}: low PSI, low CS (--)
#'   }
#'
#' @examples
#' assign_quadrant(c(1, 1, -1, -1), c(1, -1, 1, -1))
#'
#' @export
assign_quadrant <- function(psi_z, cs_z) {
  if (!is.numeric(psi_z) || !is.numeric(cs_z)) {
    stop("'psi_z' and 'cs_z' must be numeric vectors.", call. = FALSE)
  }
  if (length(psi_z) != length(cs_z)) {
    stop("'psi_z' and 'cs_z' must have the same length.", call. = FALSE)
  }

  quadrant_levels <- c(
    "Stable Core",
    "Ideological Core",
    "Emerging Practices",
    "Latent Representations"
  )

  result <- dplyr::case_when(
    psi_z >= 0 & cs_z >= 0  ~ "Stable Core",
    psi_z >= 0 & cs_z < 0   ~ "Ideological Core",
    psi_z < 0  & cs_z >= 0  ~ "Emerging Practices",
    psi_z < 0  & cs_z < 0   ~ "Latent Representations",
    TRUE                      ~ NA_character_
  )

  factor(result, levels = quadrant_levels)
}

#' Match terms to Brysbaert concreteness lexicon
#'
#' Looks up concreteness ratings for a set of terms from the Brysbaert et al.
#' concreteness norms. Matching is case-insensitive.
#'
#' @param terms Character vector of terms to look up.
#' @param lexicon Data frame with columns \code{"word"} (character) and
#'   \code{"conc.m"} (numeric, mean concreteness rating on a 1--5 scale).
#'
#' @return Named numeric vector of concreteness values aligned to \code{terms}.
#'   Terms not found in the lexicon receive \code{NA}.
#'
#' @examples
#' lex <- data.frame(word = c("dog", "cat", "freedom"),
#'                   conc.m = c(4.8, 4.7, 1.5),
#'                   stringsAsFactors = FALSE)
#' match_concreteness(c("dog", "freedom", "unknown"), lex)
#'
#' @export
match_concreteness <- function(terms, lexicon) {
  if (!is.character(terms)) {
    stop("'terms' must be a character vector.", call. = FALSE)
  }
  if (!is.data.frame(lexicon)) {
    stop("'lexicon' must be a data frame.", call. = FALSE)
  }
  required_cols <- c("word", "conc.m")
  missing_cols <- setdiff(required_cols, names(lexicon))
  if (length(missing_cols) > 0) {
    stop(
      "Lexicon is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Build a lookup table with lowercase keys for case-insensitive matching
  lookup <- stats::setNames(lexicon$conc.m, tolower(lexicon$word))
  conc_values <- lookup[tolower(terms)]
  names(conc_values) <- terms
  conc_values
}

#' Validate a tokens data frame
#'
#' Checks that the input data frame has all required columns for ThemeScope
#' analysis. Emits informative errors via \code{cli} if columns are missing.
#'
#' @param tokens_df Data frame to validate. Must contain at minimum:
#'   \itemize{
#'     \item \code{doc_id}: document identifier
#'     \item \code{sentence_id}: sentence identifier
#'     \item \code{token} or \code{lemma}: the word form
#'     \item \code{upos}: Universal POS tag
#'   }
#'
#' @return Invisibly returns \code{TRUE} if validation passes.
#'
#' @examples
#' df <- data.frame(
#'   doc_id = 1, sentence_id = 1,
#'   token = "dog", upos = "NOUN",
#'   stringsAsFactors = FALSE
#' )
#' validate_tokens_df(df)
#'
#' @export
validate_tokens_df <- function(tokens_df) {
  if (!is.data.frame(tokens_df)) {
    cli::cli_abort(
      c(
        "x" = "{.arg tokens_df} must be a data frame.",
        "i" = "Got an object of class {.cls {class(tokens_df)}}."
      )
    )
  }

  required_always <- c("doc_id", "sentence_id", "upos")
  missing_always <- setdiff(required_always, names(tokens_df))
  if (length(missing_always) > 0) {
    cli::cli_abort(
      c(
        "x" = "{.arg tokens_df} is missing required column{?s}: {.field {missing_always}}.",
        "i" = "Required columns: {.field doc_id}, {.field sentence_id}, {.field upos}, and at least one of {.field token} or {.field lemma}."
      )
    )
  }

  if (!("token" %in% names(tokens_df)) && !("lemma" %in% names(tokens_df))) {
    cli::cli_abort(
      c(
        "x" = "{.arg tokens_df} must contain at least one of {.field token} or {.field lemma}.",
        "i" = "Found columns: {.field {names(tokens_df)}}."
      )
    )
  }

  if (nrow(tokens_df) == 0) {
    cli::cli_abort("{.arg tokens_df} must not be empty.")
  }

  invisible(TRUE)
}

#' Emit a progress message (if verbose)
#'
#' A thin wrapper around \code{cli::cli_alert_info} that respects a verbosity
#' flag. Used internally throughout the ThemeScope pipeline.
#'
#' @param msg Character string. The message to display. Supports \code{cli}
#'   inline markup (e.g., \code{\{.val x\}}).
#' @param verbose Logical. If \code{FALSE}, the message is suppressed.
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @examples
#' themescope_progress("Building co-occurrence matrix...", verbose = TRUE)
#' themescope_progress("This is suppressed.", verbose = FALSE)
#'
#' @export
themescope_progress <- function(msg, verbose) {
  if (isTRUE(verbose)) {
    cli::cli_alert_info(msg)
  }
  invisible(NULL)
}
