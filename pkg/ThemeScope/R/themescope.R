#' Run the full ThemeScope analysis pipeline
#'
#' @description
#' Executes the complete ThemeScope workflow in a single call:
#' \enumerate{
#'   \item Build vocabulary from POS-filtered tokens.
#'   \item Compute sentence-level co-occurrence matrix.
#'   \item Normalise with Association Strength.
#'   \item Construct a thresholded co-occurrence network.
#'   \item Detect communities via the walktrap algorithm.
#'   \item Compute PSI (Prototypical Salience Index) per community.
#'   \item Compute CS (Concreteness Score) per community (requires lexicon).
#'   \item Collect network statistics.
#' }
#'
#' @param tokens_df Data frame with POS-tagged, lemmatised tokens. Must contain
#'   columns \code{doc_id}, \code{sentence_id}, \code{upos}, and at least one
#'   of \code{lemma} or \code{token}. Typically produced by \pkg{udpipe}.
#' @param concreteness_lexicon Data frame with columns \code{"word"} and
#'   \code{"conc.m"} (Brysbaert et al. norms, 1--5 scale). Defaults to the
#'   bundled \code{\link{brysbaert}} lexicon (39,954 English words). Pass
#'   \code{NULL} to skip CS computation (returns \code{NA}).
#' @param vocab_size Integer. Maximum vocabulary size (default \code{1500}).
#' @param pos_filter Character vector of Universal POS tags to include
#'   (default \code{c("NOUN", "ADJ", "PROPN")}).
#' @param threshold_percentile Numeric in \eqn{(0, 1)}. Percentile of non-zero
#'   AS values used to threshold the network (default \code{0.98}).
#' @param walktrap_steps Integer. Steps for walktrap community detection
#'   (default \code{4}).
#' @param min_community_size Integer. Communities smaller than this are removed
#'   (default \code{10}).
#' @param verbose Logical. Print progress messages (default \code{TRUE}).
#'
#' @return An S3 object of class \code{"themescope"} with slots:
#'   \describe{
#'     \item{\code{graph}}{The thresholded \code{igraph} network.}
#'     \item{\code{communities}}{Named list of character vectors (community members).}
#'     \item{\code{membership}}{Named integer vector of community IDs.}
#'     \item{\code{psi}}{Named numeric vector of PSI values.}
#'     \item{\code{cs}}{Named numeric vector of CS values (\code{NA} if no lexicon).}
#'     \item{\code{network_stats}}{List from \code{\link{compute_network_stats}}.}
#'     \item{\code{vocab}}{Character vector of vocabulary terms used.}
#'     \item{\code{params}}{List of all pipeline parameters.}
#'     \item{\code{call}}{The matched call.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(udpipe)
#' ud_model <- udpipe_download_model("english")
#' ud        <- udpipe_load_model(ud_model$file_model)
#' ann       <- udpipe_annotate(ud, x = my_texts, doc_id = seq_along(my_texts))
#' toks      <- as.data.frame(ann)
#'
#' # brysbaert lexicon is used automatically (bundled in the package)
#' result <- themescope(toks, vocab_size = 1000)
#' print(result)
#' plot(result, type = "map")
#' }
#'
#' @export
themescope <- function(tokens_df,
                        concreteness_lexicon  = NULL,
                        vocab_size             = 1500,
                        pos_filter             = c("NOUN", "ADJ", "PROPN"),
                        threshold_percentile   = 0.98,
                        walktrap_steps         = 4,
                        min_community_size     = 10,
                        verbose                = TRUE) {

  call <- match.call()

  # ---- 0. Default lexicon ----
  if (is.null(concreteness_lexicon)) {
    concreteness_lexicon <- brysbaert
  }

  # ---- 1. Validate input ----
  validate_tokens_df(tokens_df)

  themescope_progress("Step 1/7: Building vocabulary...", verbose)
  vocab <- build_vocab(tokens_df,
                        vocab_size = vocab_size,
                        pos_filter  = pos_filter)
  themescope_progress(
    paste0("  Vocabulary size: ", length(vocab), " terms."),
    verbose
  )

  # ---- 2. Co-occurrence matrix ----
  themescope_progress("Step 2/7: Building co-occurrence matrix...", verbose)
  cooc_result <- build_cooccurrence_matrix(
    tokens_df   = tokens_df,
    vocab       = vocab,
    pos_filter   = pos_filter,
    window       = "sentence"
  )

  # ---- 3. Association Strength ----
  themescope_progress("Step 3/7: Computing Association Strength...", verbose)
  as_matrix <- compute_association_strength(
    cooc_matrix = cooc_result$cooc_matrix,
    freq        = cooc_result$freq
  )

  # ---- 4. Network construction ----
  themescope_progress("Step 4/7: Building network...", verbose)
  graph <- build_network(
    as_matrix             = as_matrix,
    threshold_percentile  = threshold_percentile
  )

  # ---- 5. Community detection ----
  themescope_progress("Step 5/7: Detecting communities...", verbose)
  comm_result <- detect_communities(
    graph    = graph,
    steps    = walktrap_steps,
    min_size = min_community_size
  )

  communities <- comm_result$communities
  membership  <- comm_result$membership

  if (length(communities) == 0) {
    cli::cli_abort(
      c(
        "x" = "No communities with >= {min_community_size} members were found.",
        "i" = "Try lowering {.arg min_community_size} or {.arg threshold_percentile}."
      )
    )
  }

  # ---- 6. PSI ----
  themescope_progress("Step 6/7: Computing PSI...", verbose)
  psi <- compute_PSI(
    graph       = graph,
    communities = communities,
    freq        = cooc_result$freq
  )

  # ---- 7. CS ----
  if (!is.null(concreteness_lexicon)) {
    themescope_progress("Step 7/7: Computing CS...", verbose)
    cs <- compute_CS(
      graph                = graph,
      communities          = communities,
      concreteness_lexicon = concreteness_lexicon
    )
  } else {
    themescope_progress(
      "Step 7/7: No concreteness lexicon provided -- CS set to NA.", verbose
    )
    cs <- stats::setNames(
      rep(NA_real_, length(communities)),
      names(communities)
    )
  }

  # ---- Network stats ----
  network_stats <- compute_network_stats(graph, communities)

  params <- list(
    vocab_size            = vocab_size,
    pos_filter            = pos_filter,
    threshold_percentile  = threshold_percentile,
    walktrap_steps        = walktrap_steps,
    min_community_size    = min_community_size
  )

  structure(
    list(
      graph         = graph,
      communities   = communities,
      membership    = membership,
      psi           = psi,
      cs            = cs,
      network_stats = network_stats,
      vocab         = vocab,
      params        = params,
      call          = call
    ),
    class = "themescope"
  )
}


#' Print a themescope object
#'
#' Displays a concise summary of the ThemeScope analysis.
#'
#' @param x A \code{themescope} object.
#' @param ... Ignored.
#'
#' @return Invisibly returns \code{x}.
#'
#' @export
print.themescope <- function(x, ...) {
  n_comm <- length(x$communities)
  n_nodes <- igraph::vcount(x$graph)
  n_edges <- igraph::ecount(x$graph)

  cli::cli_h1("ThemeScope Analysis")
  cli::cli_bullets(c(
    "*" = "Network: {n_nodes} nodes, {n_edges} edges",
    "*" = "Communities: {n_comm}",
    "*" = "Vocabulary size: {length(x$vocab)} terms"
  ))

  if (n_comm > 0) {
    comm_sizes <- sapply(x$communities, length)
    psi_z  <- zscore(x$psi)
    cs_z   <- zscore(x$cs)
    quads  <- assign_quadrant(psi_z, cs_z)

    cli::cli_h2("Communities")
    for (k in seq_len(n_comm)) {
      cname <- names(x$communities)[k]
      sz    <- comm_sizes[k]
      psi_v <- round(x$psi[cname], 4)
      cs_v  <- if (!is.na(x$cs[cname])) round(x$cs[cname], 3) else "NA"
      qv    <- if (!is.na(quads[k])) as.character(quads[k]) else "N/A"
      cli::cli_alert_info(
        "{cname} (n={sz}): PSI={psi_v}, CS={cs_v} [{qv}]"
      )
    }
  }

  invisible(x)
}


#' Summarise a themescope object
#'
#' Provides a detailed summary including a table of community statistics.
#'
#' @param object A \code{themescope} object.
#' @param ... Ignored.
#'
#' @return Invisibly returns a data frame of community statistics.
#'
#' @export
summary.themescope <- function(object, ...) {
  x <- object
  df <- as.data.frame.themescope(x)

  cli::cli_h1("ThemeScope Analysis -- Summary")
  cli::cli_h2("Parameters")
  cli::cli_bullets(c(
    "*" = "Vocabulary size: {x$params$vocab_size}",
    "*" = "POS filter: {paste(x$params$pos_filter, collapse = ', ')}",
    "*" = "AS threshold percentile: {x$params$threshold_percentile}",
    "*" = "Walktrap steps: {x$params$walktrap_steps}",
    "*" = "Min community size: {x$params$min_community_size}"
  ))

  cli::cli_h2("Network")
  gs <- x$network_stats$global_stats
  cli::cli_bullets(c(
    "*" = "Nodes: {gs$n_nodes}",
    "*" = "Edges: {gs$n_edges}",
    "*" = "Mean degree: {round(gs$mean_degree, 2)}",
    "*" = "Modularity: {round(gs$modularity, 4)}",
    "*" = "Communities: {gs$n_communities}"
  ))

  cli::cli_h2("Community Table")
  print(df)

  invisible(df)
}


#' Plot a themescope object
#'
#' Dispatches to the appropriate visualisation function based on \code{type}.
#'
#' @param x A \code{themescope} object.
#' @param type Character. One of \code{"map"} (ThemeScope map, default) or
#'   \code{"network"} (co-occurrence network plot).
#' @param ... Additional arguments passed to \code{\link{themescope_map}} or
#'   \code{\link{plot_network}}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object (for \code{type = "map"}) or
#'   the result of \code{\link{plot_network}}.
#'
#' @export
plot.themescope <- function(x, type = c("map", "network"), ...) {
  type <- match.arg(type)

  if (type == "map") {
    comm_sizes <- sapply(x$communities, length)
    themescope_map(
      psi              = x$psi,
      cs               = x$cs,
      community_sizes  = comm_sizes,
      ...
    )
  } else {
    plot_network(
      graph      = x$graph,
      membership = x$membership,
      ...
    )
  }
}


#' Convert a themescope object to a data frame
#'
#' Returns a tidy data frame summarising all community-level results, including
#' raw and z-scored metrics and quadrant assignments.
#'
#' @param x A \code{themescope} object.
#' @param ... Ignored.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{\code{community_id}}{Community identifier (e.g., \code{"C1"}).}
#'     \item{\code{label}}{Community label (same as \code{community_id} unless
#'       labels were set externally).}
#'     \item{\code{size}}{Number of terms in the community.}
#'     \item{\code{psi}}{Raw PSI value.}
#'     \item{\code{cs}}{Raw CS value (\code{NA} if not computed).}
#'     \item{\code{psi_z}}{Z-scored PSI.}
#'     \item{\code{cs_z}}{Z-scored CS.}
#'     \item{\code{quadrant}}{Quadrant assignment (factor).}
#'   }
#'
#' @export
as.data.frame.themescope <- function(x, ...) {
  comm_names <- names(x$communities)
  sizes      <- sapply(x$communities, length)

  psi_z <- zscore(x$psi)
  cs_z  <- zscore(x$cs)
  quad  <- assign_quadrant(psi_z, cs_z)

  data.frame(
    community_id = comm_names,
    label        = comm_names,
    size         = as.integer(sizes),
    psi          = as.numeric(x$psi[comm_names]),
    cs           = as.numeric(x$cs[comm_names]),
    psi_z        = as.numeric(psi_z),
    cs_z         = as.numeric(cs_z),
    quadrant     = quad,
    stringsAsFactors = FALSE
  )
}
