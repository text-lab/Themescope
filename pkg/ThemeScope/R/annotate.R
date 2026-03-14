#' Annotate a text corpus with udpipe
#'
#' Tokenises, lemmatises and POS-tags a data frame of raw texts using a
#' \pkg{udpipe} language model. Returns a tokens data frame in the format
#' expected by \code{\link{themescope}}.
#'
#' @param texts_df Data frame containing at minimum a text column and a
#'   document-ID column.
#' @param model A \pkg{udpipe} model object as returned by
#'   \code{udpipe::udpipe_load_model()}, or a character string with the path
#'   to a \code{.udpipe} model file.
#' @param text_col Character. Name of the column containing the raw text
#'   (default \code{"text"}).
#' @param doc_id_col Character. Name of the column containing document
#'   identifiers (default \code{"doc_id"}).
#' @param batch_size Integer. Number of documents processed per batch.
#'   Smaller values reduce peak memory at the cost of speed (default
#'   \code{500}).
#' @param verbose Logical. Print progress messages (default \code{TRUE}).
#'
#' @return A data frame with columns \code{doc_id}, \code{sentence_id},
#'   \code{lemma}, and \code{upos}, ready for use in \code{\link{themescope}}.
#'
#' @examples
#' \dontrun{
#' library(udpipe)
#' model <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")
#' texts <- data.frame(
#'   doc_id = c("d1", "d2"),
#'   text   = c("Climate change is a global crisis.", "Governments must act now.")
#' )
#' tokens <- annotate_corpus(texts, model)
#' result <- themescope(tokens)
#' }
#'
#' @export
annotate_corpus <- function(texts_df,
                             model,
                             text_col   = "text",
                             doc_id_col = "doc_id",
                             batch_size = 500,
                             verbose    = TRUE) {

  if (!requireNamespace("udpipe", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg udpipe} is required. Install with: {.code install.packages('udpipe')}"
    )
  }

  # ---- Validate inputs -------------------------------------------------------
  if (!is.data.frame(texts_df)) {
    cli::cli_abort("{.arg texts_df} must be a data frame.")
  }
  if (!text_col %in% names(texts_df)) {
    cli::cli_abort("Column {.val {text_col}} not found in {.arg texts_df}.")
  }
  if (!doc_id_col %in% names(texts_df)) {
    cli::cli_abort("Column {.val {doc_id_col}} not found in {.arg texts_df}.")
  }

  # ---- Load model if path given ----------------------------------------------
  if (is.character(model)) {
    if (!file.exists(model)) {
      cli::cli_abort("Model file not found: {.path {model}}")
    }
    themescope_progress("Loading udpipe model...", verbose)
    model <- udpipe::udpipe_load_model(model)
  }

  texts  <- as.character(texts_df[[text_col]])
  docids <- as.character(texts_df[[doc_id_col]])
  n      <- length(texts)

  themescope_progress(
    paste0("Annotating ", n, " documents (batch size = ", batch_size, ")..."),
    verbose
  )

  # ---- Batch annotation ------------------------------------------------------
  n_batches <- ceiling(n / batch_size)
  chunks    <- vector("list", n_batches)

  for (i in seq_len(n_batches)) {
    idx_start <- (i - 1L) * batch_size + 1L
    idx_end   <- min(i * batch_size, n)

    if (verbose && n_batches > 1) {
      themescope_progress(
        paste0("  Batch ", i, "/", n_batches,
               " (docs ", idx_start, "-", idx_end, ")"),
        verbose
      )
    }

    ann <- udpipe::udpipe_annotate(
      object  = model,
      x       = texts[idx_start:idx_end],
      doc_id  = docids[idx_start:idx_end]
    )
    ann_df <- as.data.frame(ann, detailed = FALSE)
    chunks[[i]] <- ann_df
  }

  # ---- Combine and select columns -------------------------------------------
  result <- do.call(rbind, chunks)

  # udpipe returns: doc_id, paragraph_id, sentence_id, sentence,
  #                 token_id, token, lemma, upos, xpos, feats, ...
  # sentence_id from udpipe is already unique per sentence within a doc.
  # Make it globally unique by combining doc_id + sentence_id.
  result$sentence_id <- paste0(result$doc_id, "_s", result$sentence_id)

  # Replace missing lemmas with token
  no_lemma <- is.na(result$lemma) | result$lemma == ""
  result$lemma[no_lemma] <- result$token[no_lemma]

  # Lower-case lemmas (consistent with Brysbaert lexicon)
  result$lemma <- tolower(result$lemma)

  out <- result[, c("doc_id", "sentence_id", "lemma", "upos"),
                drop = FALSE]
  rownames(out) <- NULL

  themescope_progress(
    paste0("Annotation complete: ", nrow(out), " tokens across ",
           length(unique(out$sentence_id)), " sentences."),
    verbose
  )

  out
}


#' Download and load a udpipe language model
#'
#' Convenience wrapper around \code{udpipe::udpipe_download_model()} and
#' \code{udpipe::udpipe_load_model()}. If the model file already exists in
#' \code{model_dir} it is loaded directly without re-downloading.
#'
#' @param language Character. Language name accepted by
#'   \code{udpipe::udpipe_download_model()} (e.g. \code{"english"},
#'   \code{"italian"}, \code{"french"}).
#' @param model_dir Character. Directory where the model file is stored or
#'   should be downloaded to (default: current working directory).
#' @param verbose Logical. Print progress messages (default \code{TRUE}).
#'
#' @return A \pkg{udpipe} model object.
#'
#' @examples
#' \dontrun{
#' model <- load_udpipe_model("english", model_dir = "~/udpipe_models")
#' }
#'
#' @export
load_udpipe_model <- function(language   = "english",
                               model_dir  = ".",
                               verbose    = TRUE) {

  if (!requireNamespace("udpipe", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg udpipe} is required. Install with: {.code install.packages('udpipe')}"
    )
  }

  # Look for an existing .udpipe file matching the language
  existing <- list.files(
    model_dir,
    pattern = paste0("^", language, ".*\\.udpipe$"),
    full.names = TRUE
  )

  if (length(existing) > 0L) {
    themescope_progress(
      paste0("Loading existing model: ", basename(existing[1L])),
      verbose
    )
    return(udpipe::udpipe_load_model(existing[1L]))
  }

  # Download
  themescope_progress(
    paste0("Downloading udpipe model for '", language, "' into ", model_dir, "..."),
    verbose
  )
  dl <- udpipe::udpipe_download_model(
    language    = language,
    model_dir   = model_dir,
    overwrite   = FALSE
  )
  udpipe::udpipe_load_model(dl$file_model)
}
