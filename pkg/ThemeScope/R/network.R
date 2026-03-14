#' Build a semantic co-occurrence network
#'
#' Constructs an \code{igraph} weighted undirected graph from an Association
#' Strength matrix. Only edges whose AS value exceeds a threshold derived from
#' the \code{threshold_percentile} of all non-zero AS values are retained.
#'
#' @param as_matrix Symmetric sparse \code{Matrix} of Association Strength
#'   values, as returned by \code{\link{compute_association_strength}}.
#' @param threshold_percentile Numeric in \eqn{(0, 1)}. The quantile of
#'   non-zero AS values used as the minimum edge weight (default \code{0.98}).
#'
#' @return An undirected weighted \code{igraph} object with:
#'   \describe{
#'     \item{\code{E(graph)$weight}}{Association Strength value for each edge.}
#'     \item{\code{V(graph)$name}}{Term label.}
#'   }
#'
#' @examples
#' \dontrun{
#' net <- build_network(as_mat, threshold_percentile = 0.98)
#' igraph::vcount(net)
#' }
#'
#' @export
build_network <- function(as_matrix, threshold_percentile = 0.98) {
  if (!inherits(as_matrix, "Matrix") && !is.matrix(as_matrix)) {
    cli::cli_abort("{.arg as_matrix} must be a {.cls Matrix} or base matrix.")
  }
  if (!is.numeric(threshold_percentile) ||
      length(threshold_percentile) != 1 ||
      threshold_percentile <= 0 || threshold_percentile >= 1) {
    cli::cli_abort(
      "{.arg threshold_percentile} must be a single numeric in (0, 1)."
    )
  }

  # Work in upper triangle only to avoid double counting
  upper <- Matrix::triu(as_matrix, k = 1)
  nz_vals <- upper@x[upper@x > 0]

  if (length(nz_vals) == 0) {
    cli::cli_abort("No non-zero values found in {.arg as_matrix}.")
  }

  threshold <- stats::quantile(nz_vals, probs = threshold_percentile,
                                na.rm = TRUE)

  cli::cli_alert_info(
    "AS threshold ({threshold_percentile * 100}th percentile): {round(threshold, 6)}"
  )

  # Extract edges above threshold
  upper_thresh <- upper
  upper_thresh@x[upper_thresh@x <= threshold] <- 0
  upper_thresh <- Matrix::drop0(upper_thresh)

  edge_positions <- Matrix::which(upper_thresh > 0, arr.ind = TRUE)

  if (nrow(edge_positions) == 0) {
    cli::cli_abort(
      c(
        "x" = "No edges remain after applying the AS threshold.",
        "i" = "Try lowering {.arg threshold_percentile} (currently {threshold_percentile})."
      )
    )
  }

  term_names <- rownames(as_matrix)
  if (is.null(term_names)) {
    term_names <- as.character(seq_len(nrow(as_matrix)))
  }

  edge_weights <- upper_thresh[edge_positions]

  # Build igraph
  edge_list <- cbind(
    term_names[edge_positions[, 1]],
    term_names[edge_positions[, 2]]
  )

  graph <- igraph::graph_from_edgelist(edge_list, directed = FALSE)
  igraph::E(graph)$weight <- as.numeric(edge_weights)

  # Add isolated vocab nodes that fall below threshold (optional: include all)
  # Here we only include nodes that have at least one edge — standard practice
  # Vertex names are already set from the edge list

  cli::cli_alert_success(
    "Network built: {igraph::vcount(graph)} nodes, {igraph::ecount(graph)} edges."
  )

  graph
}


#' Detect communities in a semantic network
#'
#' Runs the walktrap community detection algorithm on a weighted undirected
#' graph and filters out small communities.
#'
#' @param graph An undirected weighted \code{igraph} object (e.g., as returned
#'   by \code{\link{build_network}}).
#' @param steps Integer. Number of steps for the walktrap random walk
#'   (default \code{4}).
#' @param min_size Integer. Minimum number of members for a community to be
#'   retained. Smaller communities are assigned \code{NA} membership
#'   (default \code{10}).
#'
#' @return A named list with:
#'   \describe{
#'     \item{\code{membership}}{Named integer vector mapping each vertex to its
#'       community ID (\code{NA} for vertices in small communities).}
#'     \item{\code{communities}}{Named list of character vectors. Each element
#'       is a retained community, containing the names of its member vertices.}
#'     \item{\code{walktrap_result}}{The raw \code{communities} object returned
#'       by \code{\link[igraph]{cluster_walktrap}}.}
#'   }
#'
#' @examples
#' \dontrun{
#' comm <- detect_communities(graph, steps = 4, min_size = 10)
#' length(comm$communities)
#' }
#'
#' @export
detect_communities <- function(graph, steps = 4, min_size = 10) {
  if (!igraph::is_igraph(graph)) {
    cli::cli_abort("{.arg graph} must be an {.cls igraph} object.")
  }
  if (!is.numeric(steps) || steps < 1) {
    cli::cli_abort("{.arg steps} must be a positive integer.")
  }
  if (!is.numeric(min_size) || min_size < 1) {
    cli::cli_abort("{.arg min_size} must be a positive integer.")
  }

  steps <- as.integer(steps)
  min_size <- as.integer(min_size)

  wt <- igraph::cluster_walktrap(graph, steps = steps,
                                   weights = igraph::E(graph)$weight)

  raw_membership <- igraph::membership(wt)
  vertex_names   <- igraph::V(graph)$name

  # Frequency table of community sizes
  comm_sizes <- table(raw_membership)
  valid_comms <- as.integer(names(comm_sizes[comm_sizes >= min_size]))

  # Re-label valid communities as 1, 2, 3, ...
  relabel_map <- stats::setNames(seq_along(valid_comms), valid_comms)

  new_membership <- raw_membership
  new_membership[] <- NA_integer_

  for (old_id in valid_comms) {
    members_idx <- which(raw_membership == old_id)
    new_membership[members_idx] <- relabel_map[as.character(old_id)]
  }

  names(new_membership) <- vertex_names

  # Build named list of communities
  n_comms <- length(valid_comms)
  communities <- vector("list", n_comms)
  for (k in seq_len(n_comms)) {
    members <- vertex_names[which(new_membership == k)]
    communities[[k]] <- members
  }
  names(communities) <- paste0("C", seq_len(n_comms))

  n_removed <- sum(comm_sizes < min_size)
  cli::cli_alert_success(
    "Community detection: {n_comms} communit{?y/ies} retained ",
    "(min size = {min_size}); {n_removed} small communit{?y/ies} removed."
  )

  list(
    membership       = new_membership,
    communities      = communities,
    walktrap_result  = wt
  )
}


#' Extract community subgraphs
#'
#' For each retained community, induces a subgraph containing only that
#' community's vertices and their interconnecting edges.
#'
#' @param graph An \code{igraph} object (the full network).
#' @param membership Named integer vector of community assignments as returned
#'   by \code{\link{detect_communities}}. \code{NA} values are ignored.
#'
#' @return A named list of \code{igraph} objects, one per community. Names
#'   follow the \code{"C1"}, \code{"C2"}, \ldots convention.
#'
#' @examples
#' \dontrun{
#' subgraphs <- get_community_subgraphs(graph, comm$membership)
#' }
#'
#' @export
get_community_subgraphs <- function(graph, membership) {
  if (!igraph::is_igraph(graph)) {
    cli::cli_abort("{.arg graph} must be an {.cls igraph} object.")
  }
  if (!is.integer(membership) && !is.numeric(membership)) {
    cli::cli_abort("{.arg membership} must be a numeric/integer vector.")
  }

  comm_ids <- sort(unique(membership[!is.na(membership)]))

  subgraphs <- lapply(comm_ids, function(cid) {
    members <- names(membership)[!is.na(membership) & membership == cid]
    igraph::induced_subgraph(graph, vids = members)
  })

  names(subgraphs) <- paste0("C", comm_ids)
  subgraphs
}
