#' Build a vocabulary from a tokens data frame
#'
#' Identifies the top \code{vocab_size} most frequent terms in a POS-filtered
#' token data frame. Uses the \code{lemma} column when available, falling back
#' to \code{token}.
#'
#' @param tokens_df Data frame with columns \code{doc_id}, \code{sentence_id},
#'   \code{upos}, and at least one of \code{lemma} or \code{token}. See
#'   \code{\link{validate_tokens_df}} for details.
#' @param vocab_size Integer. Maximum number of terms to keep (default 1500).
#' @param pos_filter Character vector of Universal POS tags to retain (default
#'   \code{c("NOUN", "ADJ", "PROPN")}).
#'
#' @return A character vector of at most \code{vocab_size} terms, ordered by
#'   decreasing frequency.
#'
#' @examples
#' df <- data.frame(
#'   doc_id     = c(1, 1, 1, 1),
#'   sentence_id = c(1, 1, 1, 2),
#'   lemma      = c("dog", "cat", "dog", "bird"),
#'   upos       = c("NOUN", "NOUN", "NOUN", "NOUN"),
#'   stringsAsFactors = FALSE
#' )
#' build_vocab(df, vocab_size = 10)
#'
#' @export
build_vocab <- function(tokens_df,
                        vocab_size = 1500,
                        pos_filter = c("NOUN", "ADJ", "PROPN")) {
  validate_tokens_df(tokens_df)

  # Prefer lemma over token
  word_col <- if ("lemma" %in% names(tokens_df)) "lemma" else "token"

  filtered <- tokens_df[
    !is.na(tokens_df$upos) & tokens_df$upos %in% pos_filter,
  ]

  if (nrow(filtered) == 0) {
    cli::cli_abort(
      c(
        "x" = "No tokens remain after POS filtering.",
        "i" = "POS filter applied: {.val {pos_filter}}.",
        "i" = "Unique UPOS tags found: {.val {unique(tokens_df$upos)}}."
      )
    )
  }

  words <- filtered[[word_col]]
  words <- words[!is.na(words) & nchar(trimws(words)) > 0]

  if (length(words) == 0) {
    cli::cli_abort("No non-empty terms found in column {.field {word_col}} after filtering.")
  }

  freq_table <- sort(table(words), decreasing = TRUE)
  n_keep <- min(vocab_size, length(freq_table))
  as.character(names(freq_table)[seq_len(n_keep)])
}


#' Build a sentence-level co-occurrence matrix
#'
#' Counts how many sentences each ordered pair of vocabulary terms co-occurs in.
#' Only unique pairs per sentence are counted. The resulting matrix is symmetric.
#'
#' @param tokens_df Data frame as described in \code{\link{build_vocab}}.
#' @param vocab Character vector of vocabulary terms. If \code{NULL}, it is
#'   built automatically using \code{\link{build_vocab}} with \code{vocab_size}
#'   and \code{pos_filter}.
#' @param vocab_size Integer. Passed to \code{\link{build_vocab}} when
#'   \code{vocab} is \code{NULL} (default 1500).
#' @param pos_filter Character vector of POS tags. Passed to
#'   \code{\link{build_vocab}} when \code{vocab} is \code{NULL}.
#' @param window Character. Co-occurrence window type. Currently only
#'   \code{"sentence"} is supported.
#'
#' @return A named list with:
#'   \describe{
#'     \item{\code{cooc_matrix}}{Symmetric sparse \code{\link[Matrix]{Matrix}}
#'       of class \code{dgCMatrix}. Rows and columns are named by vocabulary
#'       terms; cell \code{[i, j]} gives the number of sentences in which terms
#'       \code{i} and \code{j} co-occur.}
#'     \item{\code{freq}}{Named integer vector of raw term frequencies (total
#'       token count across all sentences) for each vocabulary term.}
#'     \item{\code{vocab}}{Character vector of vocabulary terms (same order as
#'       matrix rows/columns).}
#'   }
#'
#' @examples
#' df <- data.frame(
#'   doc_id      = c(1, 1, 1, 1, 1),
#'   sentence_id = c(1, 1, 1, 2, 2),
#'   lemma       = c("dog", "cat", "dog", "bird", "cat"),
#'   upos        = c("NOUN", "NOUN", "NOUN", "NOUN", "NOUN"),
#'   stringsAsFactors = FALSE
#' )
#' result <- build_cooccurrence_matrix(df, vocab_size = 10)
#' result$cooc_matrix
#'
#' @export
build_cooccurrence_matrix <- function(tokens_df,
                                      vocab      = NULL,
                                      vocab_size  = 1500,
                                      pos_filter  = c("NOUN", "ADJ", "PROPN"),
                                      window      = "sentence") {
  validate_tokens_df(tokens_df)

  if (!identical(window, "sentence")) {
    cli::cli_abort(
      c("x" = "Only {.val sentence} window is currently supported.",
        "i" = "Got: {.val {window}}.")
    )
  }

  # Build vocab if not supplied
  if (is.null(vocab)) {
    vocab <- build_vocab(tokens_df, vocab_size = vocab_size,
                         pos_filter = pos_filter)
  }

  n <- length(vocab)
  if (n < 2) {
    cli::cli_abort("Vocabulary must contain at least 2 terms.")
  }

  # Term-to-index map
  term_idx <- stats::setNames(seq_len(n), vocab)

  word_col <- if ("lemma" %in% names(tokens_df)) "lemma" else "token"

  # Filter to POS and vocab
  filtered <- tokens_df[
    !is.na(tokens_df$upos) & tokens_df$upos %in% pos_filter,
  ]
  filtered <- filtered[!is.na(filtered[[word_col]]), ]
  filtered <- filtered[filtered[[word_col]] %in% vocab, ]

  if (nrow(filtered) == 0) {
    cli::cli_abort("No tokens in vocab remain after POS filtering.")
  }

  # Compute raw term frequencies (over the FULL filtered set)
  freq_raw <- tabulate(term_idx[filtered[[word_col]]], nbins = n)
  names(freq_raw) <- vocab

  # Build a sentence key
  filtered$sent_key <- paste(filtered$doc_id, filtered$sentence_id, sep = "__")

  # Group by sentence
  sentences <- split(filtered[[word_col]], filtered$sent_key)

  # Accumulate co-occurrence counts using sparse triplet format
  row_idx_list <- vector("list", length(sentences))
  col_idx_list <- vector("list", length(sentences))

  for (s_i in seq_along(sentences)) {
    terms_in_sent <- unique(sentences[[s_i]])  # unique terms per sentence
    terms_in_sent <- terms_in_sent[terms_in_sent %in% vocab]
    m <- length(terms_in_sent)
    if (m < 2) next

    idx <- term_idx[terms_in_sent]
    # All ordered pairs (i < j) to avoid double counting
    pairs <- which(lower.tri(matrix(0, m, m)), arr.ind = TRUE)
    if (nrow(pairs) == 0) next

    row_idx_list[[s_i]] <- idx[pairs[, 1]]
    col_idx_list[[s_i]] <- idx[pairs[, 2]]
  }

  all_rows <- unlist(row_idx_list)
  all_cols <- unlist(col_idx_list)

  if (length(all_rows) == 0) {
    # Return empty matrix
    cooc <- Matrix::sparseMatrix(i = integer(0), j = integer(0),
                                  x = numeric(0),
                                  dims = c(n, n),
                                  dimnames = list(vocab, vocab))
    return(list(cooc_matrix = cooc, freq = freq_raw, vocab = vocab))
  }

  # Sum duplicates
  cooc_upper <- Matrix::sparseMatrix(
    i    = all_rows,
    j    = all_cols,
    x    = rep(1L, length(all_rows)),
    dims = c(n, n),
    dimnames = list(vocab, vocab)
  )

  # Symmetrize: add transpose (diagonal stays zero since we used i < j)
  cooc_matrix <- cooc_upper + Matrix::t(cooc_upper)

  list(cooc_matrix = cooc_matrix, freq = freq_raw, vocab = vocab)
}


#' Compute Association Strength from a co-occurrence matrix
#'
#' Normalizes raw co-occurrence counts using the Association Strength measure:
#' \deqn{sA(i,j) = \frac{c(i,j)^2}{f(i) \cdot f(j)}}
#' where \eqn{c(i,j)} is the number of co-occurring sentences and \eqn{f(i)}
#' is the raw term frequency of term \eqn{i}.
#'
#' @param cooc_matrix Symmetric sparse \code{Matrix} of co-occurrence counts as
#'   returned by \code{\link{build_cooccurrence_matrix}}.
#' @param freq Named numeric (or integer) vector of raw term frequencies. Names
#'   must match the row/column names of \code{cooc_matrix}.
#'
#' @return A sparse \code{dgCMatrix} of the same dimensions as
#'   \code{cooc_matrix} containing AS values. Values are always in \eqn{[0,1]}.
#'
#' @examples
#' # Minimal example
#' m <- Matrix::sparseMatrix(
#'   i = c(1, 2), j = c(2, 1), x = c(3, 3),
#'   dims = c(3, 3),
#'   dimnames = list(c("a","b","c"), c("a","b","c"))
#' )
#' f <- c(a = 5L, b = 4L, c = 2L)
#' compute_association_strength(m, f)
#'
#' @export
compute_association_strength <- function(cooc_matrix, freq) {
  if (!inherits(cooc_matrix, "Matrix") && !is.matrix(cooc_matrix)) {
    cli::cli_abort(
      "{.arg cooc_matrix} must be a {.cls Matrix} or base matrix object."
    )
  }
  if (!is.numeric(freq)) {
    cli::cli_abort("{.arg freq} must be a numeric vector.")
  }

  n <- nrow(cooc_matrix)
  if (length(freq) != n) {
    cli::cli_abort(
      c("x" = "Length of {.arg freq} ({length(freq)}) must equal the number of rows in {.arg cooc_matrix} ({n}).")
    )
  }

  # Replace zeros in freq with NA to avoid division by zero
  freq_safe <- as.numeric(freq)
  freq_safe[freq_safe == 0] <- NA_real_

  # Convert to dgCMatrix for efficient element-wise ops
  cm <- methods::as(cooc_matrix, "dgCMatrix")

  # Compute AS only for non-zero entries in the sparse matrix
  # For sparse matrix with triplet access: cm@i (0-based row), cm@j (0-based col)
  # Use the slot representation for speed
  cm_csc <- methods::as(cm, "CsparseMatrix")

  # Extract non-zero positions
  nz <- Matrix::which(cm_csc != 0, arr.ind = TRUE)
  if (nrow(nz) == 0) {
    return(cm_csc)
  }

  rows <- nz[, 1]
  cols <- nz[, 2]
  vals <- cm_csc[nz]

  fi <- freq_safe[rows]
  fj <- freq_safe[cols]

  denom <- fi * fj
  as_vals <- ifelse(!is.na(denom) & denom > 0, vals^2 / denom, 0)

  as_matrix <- Matrix::sparseMatrix(
    i    = rows,
    j    = cols,
    x    = as_vals,
    dims = dim(cm_csc),
    dimnames = dimnames(cm_csc)
  )

  as_matrix
}
