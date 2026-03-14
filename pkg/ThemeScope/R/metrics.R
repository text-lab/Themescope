#' Compute Prototypical Salience Index (PSI) for each community
#'
#' The PSI measures the degree to which a community represents an anchored,
#' prototypical element of the social representation. It is computed as:
#' \deqn{\Psi(G_i) = \frac{\sum_{t \in G_i} f_t \cdot d_t}{\max_i D(G_i)}}
#' where \eqn{f_t = \text{freq}[t] / \sum_v \text{freq}[v]} is the normalized
#' term frequency, \eqn{d_t} is the degree of term \eqn{t} in the community
#' subgraph, and \eqn{D(G_i) = \sum_{t \in G_i} f_t \cdot d_t} before
#' normalization.
#'
#' @param graph An undirected weighted \code{igraph} object (the full network).
#' @param communities Named list of character vectors, as returned by
#'   \code{\link{detect_communities}}.
#' @param freq Named numeric vector of raw term frequencies for all vocabulary
#'   terms (not only those in the network). Names must correspond to vertex
#'   names in \code{graph}.
#'
#' @return A named numeric vector of PSI values, one per community (names match
#'   those of \code{communities}). The maximum PSI value is \code{1} before
#'   z-scoring.
#'
#' @examples
#' \dontrun{
#' psi <- compute_PSI(graph, comm$communities, freq)
#' }
#'
#' @export
compute_PSI <- function(graph, communities, freq) {
  if (!igraph::is_igraph(graph)) {
    cli::cli_abort("{.arg graph} must be an {.cls igraph} object.")
  }
  if (!is.list(communities) || length(communities) == 0) {
    cli::cli_abort("{.arg communities} must be a non-empty named list.")
  }
  if (!is.numeric(freq)) {
    cli::cli_abort("{.arg freq} must be a numeric vector.")
  }

  # Normalized frequency over the full vocabulary (all terms in freq)
  total_freq <- sum(freq, na.rm = TRUE)
  if (total_freq == 0) {
    cli::cli_abort("Sum of {.arg freq} is zero; cannot normalize frequencies.")
  }
  freq_norm <- freq / total_freq

  # Compute raw D(Gi) for each community
  D_values <- vapply(communities, function(members) {
    sub_g <- igraph::induced_subgraph(graph, vids = members)
    deg   <- igraph::degree(sub_g)  # degree within community
    # ft for each term in this community
    ft <- freq_norm[names(deg)]
    ft[is.na(ft)] <- 0
    sum(ft * deg, na.rm = TRUE)
  }, numeric(1))

  max_D <- max(D_values, na.rm = TRUE)
  if (is.na(max_D) || max_D == 0) {
    warning("Maximum D(G) is zero or NA; PSI values will be 0.", call. = FALSE)
    psi <- stats::setNames(rep(0, length(communities)), names(communities))
    return(psi)
  }

  psi <- D_values / max_D
  psi
}


#' Compute Concreteness Score (CS) for each community
#'
#' The CS measures the degree to which a community is grounded in concrete,
#' perceptually accessible content (objectification in SRT). It is a
#' weighted average of the mean concreteness ratings of edge endpoints:
#' \deqn{C_w(G_i) = \frac{\sum_{(t,t') \in E_i} w_{t,t'} \cdot \frac{c(t)+c(t')}{2}}{\sum_{(t,t') \in E_i} w_{t,t'}}}
#' where \eqn{w_{t,t'}} is the AS edge weight and \eqn{c(t)} is the
#' Brysbaert concreteness rating (1--5 scale). Only edges where both endpoints
#' have a concreteness score contribute.
#'
#' @param graph An undirected weighted \code{igraph} object. Edges must have a
#'   \code{weight} attribute.
#' @param communities Named list of character vectors (community members).
#' @param concreteness_lexicon Data frame with columns \code{"word"} and
#'   \code{"conc.m"} (mean concreteness, 1--5 scale).
#'
#' @return A named numeric vector of CS values (one per community). Communities
#'   where no edge has both endpoints in the lexicon return \code{NA}.
#'
#' @examples
#' \dontrun{
#' cs <- compute_CS(graph, comm$communities, brysbaert_lexicon)
#' }
#'
#' @export
compute_CS <- function(graph, communities, concreteness_lexicon) {
  if (!igraph::is_igraph(graph)) {
    cli::cli_abort("{.arg graph} must be an {.cls igraph} object.")
  }
  if (!is.list(communities) || length(communities) == 0) {
    cli::cli_abort("{.arg communities} must be a non-empty named list.")
  }
  if (!is.data.frame(concreteness_lexicon)) {
    cli::cli_abort("{.arg concreteness_lexicon} must be a data frame.")
  }
  required_cols <- c("word", "conc.m")
  missing_cols <- setdiff(required_cols, names(concreteness_lexicon))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Concreteness lexicon missing column{?s}: {.field {missing_cols}}."
    )
  }

  # Build fast lookup
  conc_lookup <- stats::setNames(
    concreteness_lexicon$conc.m,
    tolower(concreteness_lexicon$word)
  )

  cs_values <- vapply(communities, function(members) {
    sub_g <- igraph::induced_subgraph(graph, vids = members)

    if (igraph::ecount(sub_g) == 0) return(NA_real_)

    el    <- igraph::as_edgelist(sub_g, names = TRUE)
    wts   <- igraph::E(sub_g)$weight
    if (is.null(wts)) wts <- rep(1, nrow(el))

    ci <- conc_lookup[tolower(el[, 1])]
    cj <- conc_lookup[tolower(el[, 2])]

    # Only edges where both endpoints have a score
    valid <- !is.na(ci) & !is.na(cj)
    if (!any(valid)) return(NA_real_)

    mean_conc  <- (ci[valid] + cj[valid]) / 2
    w_valid    <- wts[valid]
    sum(w_valid * mean_conc) / sum(w_valid)
  }, numeric(1))

  cs_values
}


#' Compute network-level and community-level statistics
#'
#' Summarises both the overall graph and each individual community.
#'
#' @param graph An \code{igraph} object.
#' @param communities Named list of character vectors (community members).
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{\code{community_stats}}{Data frame with one row per community and
#'       columns \code{community}, \code{size}, \code{density},
#'       \code{mean_degree}, \code{n_edges}.}
#'     \item{\code{global_stats}}{Named list with \code{n_nodes},
#'       \code{n_edges}, \code{mean_degree}, \code{modularity},
#'       \code{n_communities}.}
#'   }
#'
#' @examples
#' \dontrun{
#' stats <- compute_network_stats(graph, comm$communities)
#' stats$global_stats
#' }
#'
#' @export
compute_network_stats <- function(graph, communities) {
  if (!igraph::is_igraph(graph)) {
    cli::cli_abort("{.arg graph} must be an {.cls igraph} object.")
  }
  if (!is.list(communities)) {
    cli::cli_abort("{.arg communities} must be a list.")
  }

  n_comms <- length(communities)

  # Per-community stats
  community_stats <- do.call(rbind, lapply(seq_len(n_comms), function(k) {
    members <- communities[[k]]
    sub_g   <- igraph::induced_subgraph(graph, vids = members)
    sz      <- igraph::vcount(sub_g)
    ne      <- igraph::ecount(sub_g)
    dens    <- igraph::edge_density(sub_g, loops = FALSE)
    md      <- if (sz > 0) mean(igraph::degree(sub_g)) else 0

    data.frame(
      community   = names(communities)[k],
      size        = sz,
      density     = dens,
      mean_degree = md,
      n_edges     = ne,
      stringsAsFactors = FALSE
    )
  }))

  # Global stats
  # Modularity requires a membership vector aligned to graph vertices
  all_vertices <- igraph::V(graph)$name
  membership_vec <- rep(NA_integer_, length(all_vertices))
  names(membership_vec) <- all_vertices
  for (k in seq_len(n_comms)) {
    membership_vec[communities[[k]]] <- k
  }

  # Only compute modularity for vertices with a community assignment
  has_comm <- !is.na(membership_vec)
  if (sum(has_comm) > 1) {
    # Create subgraph with only assigned vertices for modularity
    subg_all <- igraph::induced_subgraph(graph,
                                          vids = all_vertices[has_comm])
    mem_sub <- membership_vec[has_comm]
    modularity_val <- tryCatch(
      igraph::modularity(subg_all, membership = mem_sub,
                          weights = igraph::E(subg_all)$weight),
      error = function(e) NA_real_
    )
  } else {
    modularity_val <- NA_real_
  }

  global_stats <- list(
    n_nodes       = igraph::vcount(graph),
    n_edges       = igraph::ecount(graph),
    mean_degree   = mean(igraph::degree(graph)),
    modularity    = modularity_val,
    n_communities = n_comms
  )

  list(community_stats = community_stats, global_stats = global_stats)
}


#' Compute edge-density coherence for each community
#'
#' Coherence is the ratio of actual edges to the maximum possible edges within
#' a community (i.e., graph edge density):
#' \deqn{\text{coherence}(G_i) = \frac{|E_i|}{|G_i|(|G_i|-1)/2}}
#'
#' @param graph An \code{igraph} object.
#' @param communities Named list of character vectors (community members).
#'
#' @return Named numeric vector of coherence values in \eqn{[0, 1]}, one per
#'   community. Communities with fewer than 2 members return \code{NA}.
#'
#' @examples
#' \dontrun{
#' coh <- compute_coherence(graph, comm$communities)
#' }
#'
#' @export
compute_coherence <- function(graph, communities) {
  if (!igraph::is_igraph(graph)) {
    cli::cli_abort("{.arg graph} must be an {.cls igraph} object.")
  }
  if (!is.list(communities)) {
    cli::cli_abort("{.arg communities} must be a list.")
  }

  coherence <- vapply(communities, function(members) {
    n <- length(members)
    if (n < 2) return(NA_real_)
    sub_g    <- igraph::induced_subgraph(graph, vids = members)
    igraph::edge_density(sub_g, loops = FALSE)
  }, numeric(1))

  coherence
}
