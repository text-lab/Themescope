#' Create a ThemeScope representational map
#'
#' Produces a two-dimensional scatter plot locating each community in the
#' theoretical space defined by z-scored PSI (x-axis, anchoring) and z-scored
#' CS (y-axis, objectification). The four quadrants correspond to Social
#' Representation Theory constructs.
#'
#' @param psi Named numeric vector of PSI values (one per community).
#' @param cs Named numeric vector of CS values (one per community). May contain
#'   \code{NA} for communities without concreteness data.
#' @param community_labels Named character vector of display labels for
#'   communities. If \code{NULL}, community names from \code{psi} are used.
#' @param community_sizes Named numeric vector of community sizes used to scale
#'   point area. If \code{NULL}, all points are the same size.
#' @param title Character. Plot title (default \code{"ThemeScope Map"}).
#' @param highlight Character vector of community names to highlight with a
#'   distinct colour.
#' @param palette Character vector of colours. If \code{NULL}, a default
#'   palette is used.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @examples
#' \dontrun{
#' themescope_map(psi = ts$psi, cs = ts$cs,
#'               community_sizes = sapply(ts$communities, length))
#' }
#'
#' @export
themescope_map <- function(psi,
                            cs,
                            community_labels  = NULL,
                            community_sizes   = NULL,
                            title             = "ThemeScope Map",
                            highlight         = NULL,
                            palette           = NULL,
                            ...) {
  if (!is.numeric(psi)) cli::cli_abort("{.arg psi} must be numeric.")
  if (!is.numeric(cs))  cli::cli_abort("{.arg cs} must be numeric.")

  comm_names <- names(psi)
  if (is.null(comm_names)) comm_names <- paste0("C", seq_along(psi))

  # z-score
  psi_z <- zscore(psi)
  cs_z  <- zscore(cs)

  # Labels
  labels <- if (!is.null(community_labels)) {
    community_labels[comm_names]
  } else {
    comm_names
  }
  labels[is.na(labels)] <- comm_names[is.na(labels)]

  # Sizes
  sizes <- if (!is.null(community_sizes)) {
    as.numeric(community_sizes[comm_names])
  } else {
    rep(4, length(comm_names))
  }
  sizes[is.na(sizes)] <- 4

  # Quadrant assignment
  quadrant <- assign_quadrant(psi_z, cs_z)

  # Highlight flag
  is_highlight <- if (!is.null(highlight)) {
    comm_names %in% highlight
  } else {
    rep(FALSE, length(comm_names))
  }

  plot_df <- data.frame(
    community   = comm_names,
    label       = as.character(labels),
    psi_z       = psi_z,
    cs_z        = cs_z,
    size        = sizes,
    quadrant    = quadrant,
    highlight   = is_highlight,
    stringsAsFactors = FALSE
  )

  # Default palette
  quad_levels <- c("Stable Core", "Ideological Core",
                   "Emerging Practices", "Latent Representations")
  if (is.null(palette)) {
    palette <- c(
      "Stable Core"              = "#2166AC",
      "Ideological Core"         = "#D6604D",
      "Emerging Practices"       = "#4DAC26",
      "Latent Representations"   = "#B2ABD2"
    )
  } else {
    if (is.null(names(palette))) {
      palette <- stats::setNames(palette, quad_levels[seq_along(palette)])
    }
  }

  # Compute axis range for quadrant annotation placement
  x_range <- range(c(psi_z, 0), na.rm = TRUE)
  y_range <- range(c(cs_z, 0), na.rm = TRUE)
  x_margin <- diff(x_range) * 0.05
  y_margin <- diff(y_range) * 0.05

  # Quadrant label positions (outside the data cloud slightly)
  qx <- c(
    x_range[2] + x_margin * 0.5,
    x_range[2] + x_margin * 0.5,
    x_range[1] - x_margin * 0.5,
    x_range[1] - x_margin * 0.5
  )
  qy <- c(
    y_range[2] + y_margin * 0.5,
    y_range[1] - y_margin * 0.5,
    y_range[2] + y_margin * 0.5,
    y_range[1] - y_margin * 0.5
  )
  quad_df <- data.frame(
    x     = qx,
    y     = qy,
    label = c("Stable Core", "Ideological Core",
              "Emerging Practices", "Latent Representations"),
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = psi_z, y = cs_z, colour = quadrant)
  ) +
    # Reference lines
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                         colour = "grey50", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                         colour = "grey50", linewidth = 0.5) +
    # Points
    ggplot2::geom_point(
      ggplot2::aes(size = size),
      alpha = 0.85, stroke = 0.5
    ) +
    # Labels with repulsion
    ggrepel::geom_text_repel(
      ggplot2::aes(label = label),
      size       = 3.2,
      max.overlaps = 30,
      segment.colour = "grey60",
      segment.size   = 0.3,
      show.legend    = FALSE
    ) +
    # Quadrant annotations
    ggplot2::annotate(
      "text",
      x      = quad_df$x,
      y      = quad_df$y,
      label  = quad_df$label,
      size   = 3,
      colour = "grey35",
      fontface = "italic",
      hjust  = c(0.5, 0.5, 0.5, 0.5)
    ) +
    ggplot2::scale_colour_manual(
      values  = palette,
      name    = "Quadrant",
      drop    = FALSE,
      na.value = "grey70"
    ) +
    ggplot2::scale_size_continuous(
      name   = "Community size",
      range  = c(3, 12),
      guide  = ggplot2::guide_legend(override.aes = list(colour = "grey40"))
    ) +
    ggplot2::labs(
      title    = title,
      x        = expression(italic("PSI")["z"] ~ "(Prototypical Salience)"),
      y        = expression(italic("CS")["z"] ~ "(Concreteness Score)")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "right"
    )

  # Optionally add highlight ring
  if (any(is_highlight)) {
    highlight_df <- plot_df[plot_df$highlight, , drop = FALSE]
    p <- p + ggplot2::geom_point(
      data       = highlight_df,
      ggplot2::aes(size = size),
      shape  = 21,
      colour = "black",
      stroke = 1.5,
      fill   = NA,
      show.legend = FALSE
    )
  }

  p
}


#' Plot a community-coloured semantic network
#'
#' Visualises the co-occurrence network with nodes coloured by community
#' membership. Attempts to use \pkg{ggraph} for a ggplot2-based output; falls
#' back to base-R \code{igraph::plot.igraph} if \pkg{ggraph} is not installed.
#'
#' @param graph An \code{igraph} object.
#' @param membership Named integer vector of community assignments (as returned
#'   by \code{\link{detect_communities}}).
#' @param community_labels Named character vector mapping community IDs to
#'   display labels. Optional.
#' @param layout Character. Graph layout algorithm passed to igraph layout
#'   functions or \pkg{ggraph}. One of
#'   \code{"fr"} (Fruchterman-Reingold), \code{"kk"} (Kamada-Kawai),
#'   \code{"dh"} (Davidson-Harel), \code{"lgl"} (LGL). Default \code{"fr"}.
#' @param top_n_labels Integer. Number of highest-degree nodes per community to
#'   label (default \code{10}).
#' @param ... Additional arguments passed to the plotting function.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object (if \pkg{ggraph} is
#'   available) or invisibly \code{NULL} after a base-R plot (if not).
#'
#' @examples
#' \dontrun{
#' plot_network(graph, comm$membership, top_n_labels = 5)
#' }
#'
#' @export
plot_network <- function(graph,
                          membership,
                          community_labels = NULL,
                          layout           = "fr",
                          top_n_labels     = 10,
                          ...) {
  if (!igraph::is_igraph(graph)) {
    cli::cli_abort("{.arg graph} must be an {.cls igraph} object.")
  }

  vertex_names <- igraph::V(graph)$name
  comm_ids     <- membership[vertex_names]

  # Identify top-degree nodes per community for labelling
  deg <- igraph::degree(graph)
  label_nodes <- character(0)
  for (cid in unique(comm_ids[!is.na(comm_ids)])) {
    cid_nodes <- names(comm_ids)[!is.na(comm_ids) & comm_ids == cid]
    cid_deg   <- deg[cid_nodes]
    top_nodes <- names(sort(cid_deg, decreasing = TRUE))[
      seq_len(min(top_n_labels, length(cid_nodes)))
    ]
    label_nodes <- c(label_nodes, top_nodes)
  }

  # Labels: only top nodes
  node_labels <- ifelse(vertex_names %in% label_nodes, vertex_names, "")

  # Community colours
  n_comms  <- max(comm_ids, na.rm = TRUE)
  pal      <- grDevices::hcl.colors(n_comms, palette = "Set2")
  node_col <- ifelse(
    is.na(comm_ids),
    "grey80",
    pal[comm_ids]
  )

  if (requireNamespace("ggraph", quietly = TRUE)) {
    # ggraph path
    igraph::V(graph)$community  <- as.character(comm_ids)
    igraph::V(graph)$node_label <- node_labels
    igraph::V(graph)$node_degree <- deg[vertex_names]

    p <- ggraph::ggraph(graph, layout = layout) +
      ggraph::geom_edge_link(
        ggplot2::aes(alpha = weight),
        colour    = "grey60",
        linewidth = 0.3,
        show.legend = FALSE
      ) +
      ggraph::geom_node_point(
        ggplot2::aes(
          colour = community,
          size   = node_degree
        ),
        alpha = 0.85
      ) +
      ggraph::geom_node_text(
        ggplot2::aes(label = node_label),
        size         = 2.8,
        repel        = TRUE,
        max.overlaps = 20,
        show.legend  = FALSE
      ) +
      ggplot2::scale_size_continuous(range = c(2, 8), guide = "none") +
      ggplot2::theme_void() +
      ggplot2::labs(
        title  = "Semantic Co-occurrence Network",
        colour = "Community"
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
      )
    return(p)
  }

  # Fallback: base igraph plot
  layout_fn <- switch(
    layout,
    fr  = igraph::layout_with_fr,
    kk  = igraph::layout_with_kk,
    dh  = igraph::layout_with_dh,
    lgl = igraph::layout_with_lgl,
    igraph::layout_with_fr
  )

  lyt <- layout_fn(graph)

  igraph::plot.igraph(
    graph,
    layout      = lyt,
    vertex.color    = node_col,
    vertex.label    = node_labels,
    vertex.label.cex = 0.7,
    vertex.size  = 4 + log1p(deg[vertex_names]) * 2,
    edge.width   = 0.5,
    edge.color   = "grey70",
    vertex.frame.color = NA,
    main         = "Semantic Co-occurrence Network"
  )

  invisible(NULL)
}
