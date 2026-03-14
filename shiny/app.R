# ==============================================================================
# ThemeScope Shiny Application
# Analyzes social representations in text using co-occurrence networks
# ==============================================================================

# ---- Load ThemeScope package --------------------------------------------------
pkg_path <- normalizePath(
  file.path(dirname(normalizePath(".")), "pkg", "ThemeScope"),
  mustWork = FALSE
)
if (dir.exists(pkg_path)) {
  devtools::load_all(pkg_path, quiet = TRUE)
} else {
  library(ThemeScope)
}

# ---- Required packages -------------------------------------------------------
library(shiny)
library(bslib)
library(DT)
library(shinycssloaders)
library(ggplot2)
library(plotly)
library(visNetwork)

# Optional packages
has_rhandsontable <- requireNamespace("rhandsontable", quietly = TRUE)

# ---- Demo dataset (Reddit climate change 2025, 10k docs) ---------------------
.demo_path <- file.path(
  dirname(normalizePath(".", mustWork = FALSE)),
  "tokens_sample_10000docs.csv"
)

demo_data <- if (file.exists(.demo_path)) {
  message("Loading demo dataset from: ", .demo_path)
  df <- data.table::fread(
    .demo_path,
    select        = c("doc_id", "sentence_id", "lemma", "upos"),
    data.table    = FALSE
  )
  df
} else {
  # Fallback minimo se il CSV non è disponibile
  message("Demo CSV not found at: ", .demo_path, " — using minimal fallback.")
  set.seed(42)
  words <- c("climate","change","energy","carbon","government","policy",
             "global","renewable","science","data","report","crisis",
             "action","emission","temperature","sea","ice","flood")
  upos  <- c(rep("NOUN",12), rep("ADJ",4), rep("NOUN",2))
  rows  <- vector("list", 40 * 6)
  idx   <- 1L
  for (d in 1:40) for (s in 1:6) {
    toks <- sample(words, 6, replace = TRUE)
    rows[[idx]] <- data.frame(
      doc_id      = paste0("doc", d),
      sentence_id = paste0("doc", d, "_s", s),
      lemma       = toks,
      upos        = upos[match(toks, words)],
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }
  do.call(rbind, rows)
}

.demo_n_docs  <- length(unique(demo_data$doc_id))
.demo_n_toks  <- nrow(demo_data)

# ---- Helper: extract top terms per community ---------------------------------
get_top_terms <- function(result, n = 10) {
  g          <- result$graph
  membership <- result$membership
  deg        <- igraph::degree(g)

  comm_names <- names(result$communities)

  rows <- lapply(comm_names, function(cid) {
    members  <- result$communities[[cid]]
    sub_deg  <- sort(deg[members], decreasing = TRUE)
    top_n    <- head(sub_deg, n)
    terms    <- names(top_n)
    # Pad if fewer than n
    if (length(terms) < n) {
      terms  <- c(terms, rep(NA_character_, n - length(terms)))
      top_n  <- c(top_n, rep(NA_real_,      n - length(top_n)))
    }
    data.frame(
      community = cid,
      rank      = seq_len(n),
      term      = terms,
      degree    = as.integer(top_n),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

# ---- Helper: ThemeScope map as plotly ----------------------------------------
build_map_plotly <- function(result, custom_labels = NULL, pal = NULL) {

  comm_sizes <- sapply(result$communities, length)
  psi  <- result$psi
  cs   <- result$cs

  # z-score
  zscore_safe <- function(x) {
    mu <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
    if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
    (x - mu) / s
  }
  psi_z <- zscore_safe(psi)
  cs_all_na <- all(is.na(cs))
  cs_z  <- if (cs_all_na) rep(0, length(cs)) else zscore_safe(cs)

  comm_names <- names(psi)
  labels <- if (!is.null(custom_labels)) {
    ifelse(comm_names %in% names(custom_labels), custom_labels[comm_names], comm_names)
  } else comm_names

  quad_colors <- c(
    "Stable Core"              = "#2166AC",
    "Ideological Core"         = "#D6604D",
    "Emerging Practices"       = "#4DAC26",
    "Latent Representations"   = "#B2ABD2",
    "N/A"                      = "#aaaaaa"
  )

  quadrant <- mapply(function(p, c) {
    if (is.na(p) || is.na(c)) return("N/A")
    if (p >= 0 && c >= 0) "Stable Core"
    else if (p >= 0 && c < 0) "Ideological Core"
    else if (p < 0 && c >= 0) "Emerging Practices"
    else "Latent Representations"
  }, psi_z, cs_z)

  node_color <- if (!is.null(pal)) {
    pal[comm_names]
  } else {
    quad_colors[quadrant]
  }
  node_size  <- 10 + (comm_sizes / max(comm_sizes)) * 25

  hover_text <- paste0(
    "<b>", labels, "</b><br>",
    "Size: ", comm_sizes, " terms<br>",
    "PSI (z): ", round(psi_z, 3), "<br>",
    if (cs_all_na) "" else paste0("CS (z): ", round(cs_z, 3), "<br>"),
    "Quadrant: ", quadrant
  )

  xrange <- range(c(psi_z, 0), na.rm = TRUE)
  yrange <- range(c(cs_z,  0), na.rm = TRUE)
  xpad   <- diff(xrange) * 0.25
  ypad   <- diff(yrange) * 0.25

  quad_annot <- list(
    list(x = xrange[2] + xpad * 0.3, y = yrange[2] + ypad * 0.3,
         text = "<i>Stable Core</i>",            showarrow = FALSE, font = list(color = quad_colors["Stable Core"],            size = 11)),
    list(x = xrange[2] + xpad * 0.3, y = yrange[1] - ypad * 0.3,
         text = "<i>Ideological Core</i>",        showarrow = FALSE, font = list(color = quad_colors["Ideological Core"],        size = 11)),
    list(x = xrange[1] - xpad * 0.3, y = yrange[2] + ypad * 0.3,
         text = "<i>Emerging Practices</i>",      showarrow = FALSE, font = list(color = quad_colors["Emerging Practices"],      size = 11)),
    list(x = xrange[1] - xpad * 0.3, y = yrange[1] - ypad * 0.3,
         text = "<i>Latent Representations</i>",  showarrow = FALSE, font = list(color = quad_colors["Latent Representations"],  size = 11))
  )

  plot_ly(
    x            = psi_z,
    y            = cs_z,
    type         = "scatter",
    mode         = "markers+text",
    text         = labels,
    textposition = "top center",
    hovertext    = hover_text,
    hoverinfo    = "text",
    marker       = list(
      color  = node_color,
      size   = node_size,
      line   = list(color = "white", width = 1.5),
      opacity = 0.88
    ),
    textfont = list(size = 10)
  ) |>
    layout(
      title = list(text = "<b>ThemeScope Representational Map</b>", font = list(size = 15)),
      xaxis = list(
        title      = "<i>PSI<sub>z</sub></i>  (Prototypical Salience — anchoring)",
        zeroline   = TRUE, zerolinecolor = "#cccccc", zerolinewidth = 1.5,
        showgrid   = FALSE
      ),
      yaxis = list(
        title    = if (cs_all_na) "CS not available (no lexicon)" else "<i>CS<sub>z</sub></i>  (Concreteness Score — objectification)",
        zeroline   = TRUE, zerolinecolor = "#cccccc", zerolinewidth = 1.5,
        showgrid   = FALSE
      ),
      shapes = list(
        list(type = "line", x0 = 0, x1 = 0,
             y0 = yrange[1] - ypad, y1 = yrange[2] + ypad,
             line = list(color = "#bbbbbb", width = 1, dash = "dash")),
        list(type = "line", y0 = 0, y1 = 0,
             x0 = xrange[1] - xpad, x1 = xrange[2] + xpad,
             line = list(color = "#bbbbbb", width = 1, dash = "dash"))
      ),
      annotations = quad_annot,
      showlegend  = FALSE,
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      margin = list(t = 60, b = 60, l = 70, r = 40)
    ) |>
    config(displayModeBar = TRUE, toImageButtonOptions = list(
      format = "png", filename = "themescope_map", width = 1200, height = 800
    ))
}

# ---- Helper: network as plotly -----------------------------------------------
# ---- Helper: network as visNetwork ------------------------------------------
build_network_visnetwork <- function(result, custom_labels = NULL, pal = NULL,
                                      hide_unclassified = FALSE) {
  g    <- result$graph
  memb <- result$membership
  deg  <- igraph::degree(g)

  vnames   <- igraph::V(g)$name
  comm_ids <- memb[vnames]

  # Optionally drop unclassified nodes and their edges
  if (hide_unclassified) {
    keep    <- !is.na(comm_ids)
    vnames  <- vnames[keep]
    comm_ids <- comm_ids[keep]
    deg     <- deg[keep]
    g       <- igraph::induced_subgraph(g, vids = vnames)
  }

  # Community labels — membership values are integers; keys are "C1", "C2", ...
  comm_label_map <- if (!is.null(custom_labels)) {
    function(cid) {
      key <- paste0("C", cid)
      ifelse(key %in% names(custom_labels), custom_labels[key], key)
    }
  } else {
    function(cid) paste0("C", cid)
  }

  # Colour palette
  unique_comms <- sort(unique(na.omit(comm_ids)))
  if (is.null(pal)) {
    n_c  <- length(unique_comms)
    cols <- grDevices::hcl.colors(n_c, palette = "Set2")
    pal  <- setNames(cols, paste0("C", unique_comms))
  }

  # ---- Nodes data frame ----
  node_size  <- 10 + log1p(deg) * 5
  node_color <- ifelse(is.na(comm_ids), "#cccccc", pal[paste0("C", comm_ids)])
  comm_lbl   <- sapply(comm_ids, comm_label_map)

  nodes <- data.frame(
    id        = vnames,
    label     = vnames,
    title     = paste0(
      "<b>", vnames, "</b><br>",
      "Community: ", comm_lbl, "<br>",
      "Degree: ", deg
    ),
    group     = comm_lbl,
    value     = as.numeric(node_size),
    color     = node_color,
    font.size = 12,
    stringsAsFactors = FALSE
  )

  # ---- Edges data frame ----
  el      <- igraph::as_edgelist(g)
  w_vals  <- igraph::E(g)$weight
  w_norm  <- (w_vals - min(w_vals)) / (max(w_vals) - min(w_vals) + 1e-9)

  edges <- data.frame(
    from  = el[, 1],
    to    = el[, 2],
    value = w_norm * 3 + 0.3,                          # width scaled to [0.3, 3.3]
    color = sprintf("rgba(150,150,150,%.2f)",
                    0.15 + w_norm * 0.50),              # opacity by weight
    title = paste0("AS weight: ", round(w_vals, 4)),
    stringsAsFactors = FALSE
  )

  # ---- visNetwork ----
  visNetwork::visNetwork(nodes, edges,
    main  = list(text  = "Semantic Co-occurrence Network",
                 style = "font-family:sans-serif; font-size:16px; font-weight:bold;"),
    width = "100%", height = "100%"
  ) |>
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      selectedBy       = list(variable = "group", main = "Filter by community")
    ) |>
    visNetwork::visPhysics(
      solver            = "forceAtlas2Based",
      forceAtlas2Based  = list(
        gravitationalConstant = -300,
        springLength          = 200,
        springConstant        = 0.04,
        damping               = 0.9
      ),
      stabilization = list(enabled = TRUE, iterations = 300, fit = TRUE)
    ) |>
    visNetwork::visEdges(smooth = list(enabled = FALSE)) |>
    visNetwork::visNodes(
      shape   = "dot",
      scaling = list(min = 8, max = 40),
      shadow  = list(enabled = TRUE, size = 4)
    ) |>
    visNetwork::visInteraction(
      navigationButtons = TRUE,
      tooltipDelay      = 100
    )
}

# ---- Helper: top terms as plotly ---------------------------------------------
build_topterms_plotly <- function(df, custom_labels = NULL) {
  df <- df[!is.na(df$term), ]

  df$community_label <- if (!is.null(custom_labels)) {
    ifelse(df$community %in% names(custom_labels), custom_labels[df$community], df$community)
  } else df$community

  comms   <- unique(df$community_label)
  n_comms <- length(comms)
  ncols   <- min(2L, n_comms)
  nrows   <- ceiling(n_comms / ncols)

  pal <- setNames(
    grDevices::hcl.colors(n_comms, palette = "Set2"),
    comms
  )

  plots <- lapply(comms, function(clab) {
    sub <- df[df$community_label == clab, ]
    sub <- sub[order(sub$degree), ]
    plot_ly(sub,
      x         = ~degree, y = ~reorder(term, degree),
      type      = "bar", orientation = "h",
      marker    = list(color = pal[clab], opacity = 0.85),
      hovertemplate = "<b>%{y}</b><br>Degree: %{x}<extra></extra>",
      showlegend = FALSE
    ) |> layout(
      annotations = list(list(
        text = paste0("<b>", clab, "</b>"),
        x = 0.5, y = 1.05, xref = "paper", yref = "paper",
        showarrow = FALSE, font = list(size = 11)
      )),
      xaxis = list(title = "Degree", showgrid = TRUE, gridcolor = "#eeeeee"),
      yaxis = list(title = "", automargin = TRUE)
    )
  })

  subplot(plots, nrows = nrows, shareX = FALSE, shareY = FALSE,
          titleX = TRUE, titleY = TRUE, margin = 0.06) |>
    layout(
      title = list(text = "<b>Top Terms per Community (Degree Centrality)</b>",
                   font = list(size = 14)),
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      margin = list(t = 60)
    ) |>
    config(displayModeBar = FALSE)
}

# ---- Helper: parse uploaded CSV/TSV -----------------------------------------
parse_uploaded_tokens <- function(filepath, filename) {
  ext <- tolower(tools::file_ext(filename))
  sep <- if (ext == "tsv") "\t" else ","
  df  <- utils::read.table(
    filepath,
    header    = TRUE,
    sep       = sep,
    quote     = '"',
    fill      = TRUE,
    comment.char = "",
    stringsAsFactors = FALSE
  )
  df
}

# ==============================================================================
# UI
# ==============================================================================

ui <- page_sidebar(
  title = tags$span(
    style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
    # Left: app name
    tags$span("ThemeScope", style = "font-weight:600; letter-spacing:0.03em;"),
    # Right: authors + GitHub
    tags$span(
      style = "font-size:0.78rem; font-weight:400; opacity:0.85; display:flex; align-items:center; gap:1.2rem;",
      tags$span(
        style = "opacity:0.7;",
        "M. Spano \u00b7 M. Misuraca \u00b7 L. D\u2019Aniello"
      ),
      tags$a(
        href   = "https://github.com/mariaspano/Themescope",
        target = "_blank",
        style  = "color:inherit; text-decoration:none; display:flex; align-items:center; gap:0.35rem;",
        tags$svg(
          xmlns = "http://www.w3.org/2000/svg", width = "16", height = "16",
          viewBox = "0 0 24 24", fill = "currentColor",
          tags$path(d = "M12 0C5.37 0 0 5.37 0 12c0 5.3 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61-.546-1.385-1.335-1.755-1.335-1.755-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 21.795 24 17.295 24 12c0-6.63-5.37-12-12-12z")
        ),
        "GitHub"
      )
    )
  ),
  theme = bs_theme(
    bootswatch = "flatly",
    base_font  = font_google("Inter", wght = "300;400;500;600"),
    font_scale = 0.9
  ),
  tags$head(tags$style(HTML("
    /* Navbar gradient */
    .navbar, .bslib-page-sidebar > .navbar {
      background: linear-gradient(135deg, #3d4451 0%, #5a6270 60%, #7a8390 100%) !important;
      border-bottom: none !important;
      box-shadow: 0 2px 8px rgba(0,0,0,0.18);
    }
    .navbar .navbar-brand, .navbar .nav-link, .navbar-text {
      color: #f0f2f5 !important;
    }
    /* Slightly smaller base font */
    body, .shiny-input-container, .card-body, .sidebar, label {
      font-size: 0.875rem;
    }
  "))),

  # ---- Sidebar ---------------------------------------------------------------
  sidebar = sidebar(
    width = 310,

    # ----- Data input tabs
    card(
      card_header("Data Input"),
      navset_tab(
        id = "input_tab",

        nav_panel(
          "Upload CSV/TSV",
          fileInput(
            "file_upload",
            label       = NULL,
            accept      = c(".csv", ".tsv", "text/csv", "text/tab-separated-values"),
            placeholder = "Browse or drag & drop",
            buttonLabel = "Browse"
          ),
          helpText(
            "Required columns: ",
            tags$code("doc_id"), ", ",
            tags$code("sentence_id"), ", ",
            tags$code("lemma"), ", ",
            tags$code("upos")
          )
        ),

        nav_panel(
          "Demo Data",
          p(
            icon("reddit"), " Reddit — Climate Change 2025",
            class = "fw-semibold mb-1 mt-1"
          ),
          tags$ul(
            class = "small text-muted ps-3 mb-2",
            tags$li(paste0(format(.demo_n_docs, big.mark=","), " documenti")),
            tags$li(paste0(format(.demo_n_toks, big.mark=","), " token annotati")),
            tags$li("24 subreddit, gen-dic 2025"),
            tags$li("Campione stratificato per subreddit")
          ),
          actionButton(
            "load_demo",
            tagList(icon("database"), " Carica dataset demo"),
            class = "btn-outline-primary w-100"
          ),
          uiOutput("demo_loaded_badge")
        )
      )
    ),

    # ----- Parameters
    card(
      card_header("Analysis Parameters"),

      numericInput(
        "vocab_size",
        "Vocabulary size",
        value = 1500, min = 50, max = 5000, step = 50
      ),

      checkboxGroupInput(
        "pos_filter",
        "POS filter",
        choices  = c("NOUN", "ADJ", "PROPN", "VERB"),
        selected = c("NOUN", "ADJ", "PROPN"),
        inline   = TRUE
      ),

      sliderInput(
        "threshold_percentile",
        "AS threshold percentile",
        min = 0.90, max = 0.99, value = 0.98, step = 0.01
      ),

      selectInput(
        "community_algorithm",
        "Community detection",
        choices  = c("Walktrap" = "walktrap", "Louvain" = "louvain"),
        selected = "walktrap"
      ),

      conditionalPanel(
        condition = "input.community_algorithm === 'walktrap'",
        numericInput(
          "walktrap_steps",
          "Walktrap steps",
          value = 4, min = 2, max = 10, step = 1
        )
      ),

      numericInput(
        "min_community_size",
        "Min community size",
        value = 10, min = 2, max = 50, step = 1
      ),

      hr(),

      tags$p(
        tags$strong("Concreteness lexicon: "),
        tags$span(
          class = "badge bg-success",
          "Brysbaert et al. (2014) — bundled"
        ),
        class = "mb-1 mt-2"
      ),
      helpText(
        icon("circle-info"),
        " 39,954-word English norms (1-5 scale) included automatically.",
        class = "small text-muted mb-2"
      ),
      fileInput(
        "lexicon_upload",
        "Override lexicon (optional)",
        accept      = c(".csv", "text/csv"),
        placeholder = "Use bundled Brysbaert",
        buttonLabel = "Browse"
      ),
      helpText(
        "CSV with columns ",
        tags$code("word"), " and ", tags$code("conc.m"),
        " to replace the bundled lexicon.",
        class = "small text-muted"
      )
    ),

    # ----- Run button
    actionButton(
      "run_analysis",
      label = tagList(icon("play"), " Run Analysis"),
      class = "btn-primary w-100 mt-2"
    ),

    uiOutput("analysis_status_badge")
  ),

  # ---- Main panel ------------------------------------------------------------
  navset_card_tab(
    id       = "main_tabs",
    selected = "info",

    # --- Tab 0: Info ----------------------------------------------------------
    nav_panel(
      value = "info",
      title = tagList(icon("circle-info"), " Info"),
      div(
        class = "container-fluid py-4 px-4",
        style = "max-width: 1100px; margin: 0 auto;",

        # ---- Header ----
        div(
          class = "text-center mb-4",
          tags$h2(class = "fw-bold mb-1", "ThemeScope"),
          tags$p(
            class = "text-muted fs-5 mb-0",
            "Social Representation Analysis in Digital Discourse"
          )
        ),
        tags$hr(class = "mb-4"),

        # ---- Row 1: What is + Pipeline ----
        div(
          class = "row g-4 mb-4",

          # What is ThemeScope
          div(
            class = "col-md-6",
            div(
              class = "card h-100 border-0 shadow-sm",
              div(
                class = "card-body",
                tags$h5(class = "card-title fw-bold text-primary mb-3",
                  icon("microscope"), " What is ThemeScope"
                ),
                tags$p(class = "card-text",
                  "ThemeScope is a computational framework for detecting and visualising ",
                  tags$strong("social representations"), " in large-scale digital text corpora. ",
                  "It combines sentence-level word co-occurrence networks with ",
                  tags$em("Social Representation Theory"), " (SRT), operationalising two core mechanisms:"
                ),
                tags$ul(
                  tags$li(tags$strong("Anchoring"), " — how new ideas are integrated into familiar categories (measured by PSI)"),
                  tags$li(tags$strong("Objectification"), " — how abstract concepts become concrete (measured by CS)")
                )
              )
            )
          ),

          # Pipeline
          div(
            class = "col-md-6",
            div(
              class = "card h-100 border-0 shadow-sm",
              div(
                class = "card-body",
                tags$h5(class = "card-title fw-bold text-primary mb-3",
                  icon("diagram-project"), " Analytical Pipeline"
                ),
                tags$ol(
                  class = "mb-0",
                  tags$li(tags$strong("Co-occurrence network: "), "sentence-level pairs among top-N terms (NOUN, ADJ, PROPN), normalised with Association Strength"),
                  tags$li(tags$strong("Community detection: "), "walktrap algorithm (igraph), minimum community size configurable"),
                  tags$li(tags$strong("PSI — Prototypical Salience Index: "), "anchoring proxy — weighted sum of term frequency × intra-community degree"),
                  tags$li(tags$strong("CS — Concreteness Score: "), "objectification proxy — edge-weighted mean concreteness (Brysbaert et al. norms, 1–5)"),
                  tags$li(tags$strong("2D map: "), "z-scored PSI × CS space with four theoretically grounded quadrants")
                )
              )
            )
          )
        ),

        # ---- Row 2: Four quadrants ----
        div(
          class = "mb-4",
          tags$h5(class = "fw-bold text-primary mb-3",
            icon("chart-scatter"), " The Four Quadrants"
          ),
          div(
            class = "row g-3",
            div(
              class = "col-sm-6 col-lg-3",
              div(
                class = "card border-0 h-100 text-center py-3",
                style = "background:#dbeafe;",
                div(class = "card-body py-2",
                  tags$h6(class = "fw-bold mb-1", "Stable Core"),
                  tags$p(class = "small text-muted mb-1", "High PSI · High CS"),
                  tags$p(class = "small mb-0", "Concrete, well-anchored themes — the dominant narrative frame")
                )
              )
            ),
            div(
              class = "col-sm-6 col-lg-3",
              div(
                class = "card border-0 h-100 text-center py-3",
                style = "background:#fee2e2;",
                div(class = "card-body py-2",
                  tags$h6(class = "fw-bold mb-1", "Ideological Core"),
                  tags$p(class = "small text-muted mb-1", "High PSI · Low CS"),
                  tags$p(class = "small mb-0", "Abstract but structurally central — overarching interpretive frames")
                )
              )
            ),
            div(
              class = "col-sm-6 col-lg-3",
              div(
                class = "card border-0 h-100 text-center py-3",
                style = "background:#dcfce7;",
                div(class = "card-body py-2",
                  tags$h6(class = "fw-bold mb-1", "Emerging Practices"),
                  tags$p(class = "small text-muted mb-1", "Low PSI · High CS"),
                  tags$p(class = "small mb-0", "Concrete but peripheral — experiential and operational themes")
                )
              )
            ),
            div(
              class = "col-sm-6 col-lg-3",
              div(
                class = "card border-0 h-100 text-center py-3",
                style = "background:#f3e8ff;",
                div(class = "card-body py-2",
                  tags$h6(class = "fw-bold mb-1", "Latent Representations"),
                  tags$p(class = "small text-muted mb-1", "Low PSI · Low CS"),
                  tags$p(class = "small mb-0", "Abstract and weakly integrated — emerging or fragmented themes")
                )
              )
            )
          )
        ),

        # ---- Row 3: How to use + Input format ----
        div(
          class = "row g-4 mb-4",

          # How to use
          div(
            class = "col-md-7",
            div(
              class = "card border-0 shadow-sm h-100",
              div(
                class = "card-body",
                tags$h5(class = "card-title fw-bold text-success mb-3",
                  icon("play-circle"), " How to Use"
                ),
                tags$div(
                  class = "small",
                  # steps
                  tags$table(
                    class = "table table-borderless mb-0",
                    tags$tbody(
                      tags$tr(
                        tags$td(class="align-top pe-2 pt-1",
                          tags$span(class="badge bg-primary rounded-pill", "1")
                        ),
                        tags$td(class="pb-3",
                          tags$strong("Load data"), tags$br(),
                          "Use ", tags$strong("Upload CSV/TSV"), " to load a pre-annotated file",
                          " (columns: ", tags$code("doc_id"), ", ", tags$code("sentence_id"), ", ",
                          tags$code("lemma"), ", ", tags$code("upos"), "). ",
                          "Or click ", tags$strong("Demo Data"), " to load the built-in ",
                          "Reddit Climate Change 2025 corpus (10,000 documents)."
                        )
                      ),
                      tags$tr(
                        tags$td(class="align-top pe-2 pt-1",
                          tags$span(class="badge bg-primary rounded-pill", "2")
                        ),
                        tags$td(class="pb-3",
                          tags$strong("Set parameters"), tags$br(),
                          tags$strong("Vocabulary size"), " — top-N most frequent terms (default 1500); ",
                          tags$strong("AS threshold"), " — network edge percentile cutoff (default 0.98 = top 2%); ",
                          tags$strong("Min community size"), " — exclude communities smaller than this (default 10)."
                        )
                      ),
                      tags$tr(
                        tags$td(class="align-top pe-2 pt-1",
                          tags$span(class="badge bg-primary rounded-pill", "3")
                        ),
                        tags$td(class="pb-3",
                          tags$strong("Run Analysis"), tags$br(),
                          "Click the ", tags$strong("Run Analysis"), " button. Analysis typically completes in a few seconds."
                        )
                      ),
                      tags$tr(
                        tags$td(class="align-top pe-2 pt-1",
                          tags$span(class="badge bg-primary rounded-pill", "4")
                        ),
                        tags$td(class="pb-3",
                          tags$strong("Explore results"), tags$br(),
                          tags$strong("Map"), " — interactive PSI × CS scatter (hover for details); ",
                          tags$strong("Network"), " — drag, zoom, and click nodes; ",
                          tags$strong("Communities"), " — double-click the ", tags$em("Label"), " column to rename; ",
                          tags$strong("Top Terms"), " — highest-degree terms per community."
                        )
                      ),
                      tags$tr(
                        tags$td(class="align-top pe-2 pt-1",
                          tags$span(class="badge bg-primary rounded-pill", "5")
                        ),
                        tags$td(class="pb-0",
                          tags$strong("Export"), tags$br(),
                          "Use the ", tags$strong("\U1F4F7 camera icon"), " in each chart toolbar to download PNG. ",
                          "The ", tags$strong("HTML"), " button exports a self-contained interactive file."
                        )
                      )
                    )
                  )
                )
              )
            )
          ),

          # Input format
          div(
            class = "col-md-5",
            div(
              class = "card border-0 shadow-sm h-100",
              div(
                class = "card-body",
                tags$h5(class = "card-title fw-bold text-success mb-3",
                  icon("file-csv"), " Input File Format"
                ),
                tags$p(class = "small", "CSV or TSV with at least these four columns:"),
                tags$table(
                  class = "table table-sm table-bordered small mb-3",
                  tags$thead(class = "table-light",
                    tags$tr(
                      tags$th("Column"), tags$th("Type"), tags$th("Example")
                    )
                  ),
                  tags$tbody(
                    tags$tr(tags$td(tags$code("doc_id")),      tags$td("character"), tags$td("doc001")),
                    tags$tr(tags$td(tags$code("sentence_id")), tags$td("character"), tags$td("doc001_s1")),
                    tags$tr(tags$td(tags$code("lemma")),       tags$td("character"), tags$td("climate")),
                    tags$tr(tags$td(tags$code("upos")),        tags$td("character"), tags$td("NOUN"))
                  )
                ),
                tags$p(class = "small mb-3",
                  icon("circle-info"), " Additional columns (metadata) are allowed and ignored."
                ),
                tags$hr(class = "my-3"),
                tags$h6(class = "fw-bold", "Preparing data from raw text"),
                tags$p(class = "small mb-1",
                  "Annotate with ", tags$strong("udpipe"), " in R:"
                ),
                tags$pre(
                  class = "bg-light rounded p-2 small mb-0",
                  style = "font-size:0.78rem; white-space:pre-wrap;",
                  "library(udpipe)\nud <- udpipe_load_model('english')\nann <- udpipe_annotate(ud, x=texts,\n        doc_id=doc_ids)\ntokens_df <- as.data.frame(ann)[,\n  c('doc_id','sentence_id',\n    'lemma','upos')]"
                )
              )
            )
          )
        ),

        # ---- Footer ----
        tags$hr(),
        div(
          class = "text-center small text-muted py-2",
          icon("book-open"), " ",
          tags$strong("Methodology: "),
          "See the ThemeScope manuscript for the full methodological description.",
          tags$br(),
          icon("database"), " ",
          tags$strong("Concreteness lexicon: "),
          "Brysbaert, M., Warriner, A.B., & Kuperman, V. (2014). ",
          tags$em("Behavior Research Methods"), ", 46(3), 904\u2013911.",
          tags$a(href="https://doi.org/10.3758/s13428-013-0403-5", " doi:10.3758/s13428-013-0403-5",
            target="_blank", class="text-muted")
        )
      )
    ),

    # --- Tab 1: Summary -------------------------------------------------------
    nav_panel(
      title = tagList(icon("chart-bar"), " Summary"),
      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header("Network Statistics"),
          card_body(
            uiOutput("summary_placeholder_net"),
            withSpinner(
              tableOutput("network_stats_table"),
              type  = 6,
              color = "#2c3e50"
            )
          )
        ),

        card(
          card_header("Analysis Parameters"),
          card_body(
            uiOutput("summary_placeholder_params"),
            tableOutput("params_table")
          )
        ),

        card(
          col_widths = 12,
          card_header("Console Output"),
          card_body(
            verbatimTextOutput("summary_text")
          )
        )
      )
    ),

    # --- Tab 2: Map -----------------------------------------------------------
    nav_panel(
      title = tagList(icon("map"), " Map"),
      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center",
          tagList("ThemeScope Representational Map",
            tags$small(class="text-muted ms-2", "(usa la \U1F4F7 toolbar per PNG)")),
          downloadButton("dl_map_png", "HTML", class = "btn-sm btn-outline-secondary")
        ),
        card_body(
          uiOutput("map_placeholder"),
          withSpinner(
            plotlyOutput("map_plot", height = "580px"),
            type  = 6,
            color = "#2c3e50"
          )
        )
      )
    ),

    # --- Tab 3: Communities ---------------------------------------------------
    nav_panel(
      title = tagList(icon("layer-group"), " Communities"),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Community Statistics",
          uiOutput("label_update_ui")
        ),
        card_body(
          uiOutput("communities_placeholder"),
          withSpinner(
            DTOutput("communities_table"),
            type  = 6,
            color = "#2c3e50"
          )
        ),
        card_footer(
          helpText(
            icon("pencil"),
            " Double-click the ",
            tags$strong("Label"),
            " column to edit community names. Click ",
            tags$strong("Update Map Labels"),
            " to re-render the map.",
            class = "small text-muted mb-0"
          )
        )
      )
    ),

    # --- Tab 5: Top Terms -----------------------------------------------------
    nav_panel(
      title = tagList(icon("list-ol"), " Top Terms"),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Top Terms by Community",
          numericInput(
            "top_n_terms",
            label = "Top N",
            value = 10, min = 3, max = 30, step = 1,
            width = "100px"
          )
        ),
        card_body(
          uiOutput("topterms_panels")
        )
      )
    ),

    # --- Tab 6: Network -------------------------------------------------------
    nav_panel(
      title = tagList(icon("circle-nodes"), " Network"),
      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center",
          tagList("Co-occurrence Network",
            tags$small(class="text-muted ms-2", "(usa la \U1F4F7 toolbar per PNG)")),
          div(
            class = "d-flex align-items-center gap-3",
            div(
              class = "form-check form-switch mb-0",
              tags$input(
                class = "form-check-input", type = "checkbox",
                id = "net_hide_unclassified", role = "switch"
              ),
              tags$label(
                class = "form-check-label small",
                `for` = "net_hide_unclassified",
                "Only classified"
              )
            ),
            downloadButton("dl_net_png", "HTML", class = "btn-sm btn-outline-secondary")
          )
        ),
        card_body(
          uiOutput("network_placeholder"),
          withSpinner(
            visNetworkOutput("network_plot", height = "620px"),
            type  = 6,
            color = "#2c3e50"
          )
        )
      )
    )
  )
)

# ==============================================================================
# Server
# ==============================================================================

server <- function(input, output, session) {

  # ---- Reactive state --------------------------------------------------------
  tokens_data   <- reactiveVal(NULL)   # current tokens data frame
  result_obj    <- reactiveVal(NULL)   # themescope result
  custom_labels <- reactiveVal(NULL)   # user-edited community labels
  data_source   <- reactiveVal(NULL)   # "upload" | "demo"

  # ---- Load demo data --------------------------------------------------------
  observeEvent(input$load_demo, {
    tokens_data(demo_data)
    data_source("demo")
    result_obj(NULL)
    custom_labels(NULL)
    # Parametri ottimali per il dataset Reddit climate change 10k docs
    updateNumericInput(session, "vocab_size",           value = 1500)
    updateNumericInput(session, "min_community_size",   value = 10)
    updateSliderInput( session, "threshold_percentile", value = 0.98)
    showNotification(
      tagList(
        icon("check-circle"), " ",
        strong("Demo caricato: "),
        paste0(format(.demo_n_docs, big.mark=","), " documenti, ",
               format(.demo_n_toks, big.mark=","), " token.")
      ),
      type     = "message",
      duration = 5
    )
  })

  output$demo_loaded_badge <- renderUI({
    if (!is.null(data_source()) && data_source() == "demo") {
      tags$span(
        class = "badge bg-success mt-2",
        icon("check"), " Demo data loaded"
      )
    }
  })

  # ---- Handle file upload ----------------------------------------------------
  observeEvent(input$file_upload, {
    req(input$file_upload)
    tryCatch({
      df <- parse_uploaded_tokens(
        input$file_upload$datapath,
        input$file_upload$name
      )
      validate_tokens_df(df)
      tokens_data(df)
      data_source("upload")
      result_obj(NULL)
      custom_labels(NULL)
      showNotification(
        paste0("File loaded: ", nrow(df), " rows, ",
               length(unique(df$doc_id)), " documents."),
        type     = "message",
        duration = 4
      )
    }, error = function(e) {
      showNotification(
        paste("Error reading file:", conditionMessage(e)),
        type     = "error",
        duration = 8
      )
    })
  })

  # ---- Load concreteness lexicon ---------------------------------------------
  # Returns the bundled brysbaert lexicon by default; user upload overrides it.
  lexicon_data <- reactive({
    if (is.null(input$lexicon_upload)) {
      return(ThemeScope::brysbaert)   # bundled default
    }
    tryCatch({
      df <- utils::read.csv(
        input$lexicon_upload$datapath,
        stringsAsFactors = FALSE
      )
      if (!all(c("word", "conc.m") %in% names(df))) {
        showNotification(
          "Lexicon file must contain columns 'word' and 'conc.m'. Using bundled Brysbaert instead.",
          type     = "warning",
          duration = 6
        )
        return(ThemeScope::brysbaert)
      }
      showNotification(
        paste0("Custom lexicon loaded: ", nrow(df), " words."),
        type     = "message",
        duration = 4
      )
      df
    }, error = function(e) {
      showNotification(
        paste("Error reading lexicon:", conditionMessage(e), "— using bundled Brysbaert."),
        type     = "error",
        duration = 8
      )
      ThemeScope::brysbaert
    })
  })

  # ---- Input validation -------------------------------------------------------
  observeEvent(input$run_analysis, {
    if (is.null(tokens_data())) {
      showNotification(
        "No data loaded. Please upload a CSV/TSV file or load the demo data.",
        type     = "warning",
        duration = 5
      )
    }
    if (length(input$pos_filter) == 0) {
      showNotification(
        "Please select at least one POS tag.",
        type     = "warning",
        duration = 5
      )
    }
  })

  # ---- Run analysis ----------------------------------------------------------
  observeEvent(input$run_analysis, {
    req(tokens_data())
    req(length(input$pos_filter) > 0)

    tokens <- tokens_data()
    lex    <- lexicon_data()

    withProgress(
      message = "Running ThemeScope analysis...",
      value   = 0,
      {
        setProgress(0.05, detail = "Validating input...")
        Sys.sleep(0.1)  # allow UI to update

        tryCatch({
          setProgress(0.10, detail = "Building vocabulary...")

          res <- themescope(
            tokens_df             = tokens,
            concreteness_lexicon  = lex,
            vocab_size            = input$vocab_size,
            pos_filter            = input$pos_filter,
            threshold_percentile  = input$threshold_percentile,
            community_algorithm   = input$community_algorithm,
            walktrap_steps        = input$walktrap_steps,
            min_community_size    = input$min_community_size,
            verbose               = FALSE
          )

          setProgress(0.95, detail = "Finalising...")
          result_obj(res)
          custom_labels(NULL)

          n_comm  <- length(res$communities)
          n_nodes <- igraph::vcount(res$graph)
          n_edges <- igraph::ecount(res$graph)
          showNotification(
            paste0(
              "Analysis complete: ", n_comm, " communities detected (",
              n_nodes, " nodes, ", n_edges, " edges)."
            ),
            type     = "message",
            duration = 5
          )

          # Switch to map tab
          nav_select("main_tabs", "Map")

          setProgress(1.0, detail = "Done.")
        }, error = function(e) {
          msg <- gsub("\033\\[[0-9;]*m", "", conditionMessage(e))
          showNotification(
            paste("Analysis failed:", msg),
            type     = "error",
            duration = 10
          )
        })
      }
    )
  })

  # ---- Analysis status badge -------------------------------------------------
  output$analysis_status_badge <- renderUI({
    res <- result_obj()
    if (is.null(res)) {
      tags$span(class = "badge bg-secondary mt-2 d-block text-center",
                "No analysis run yet")
    } else {
      n_comm <- length(res$communities)
      tags$span(
        class = "badge bg-success mt-2 d-block text-center",
        icon("check"),
        sprintf(" %d communities found", n_comm)
      )
    }
  })

  # ---- Placeholder helper ----------------------------------------------------
  placeholder_card <- function(msg) {
    div(
      class = "d-flex flex-column align-items-center justify-content-center py-5 text-muted",
      icon("circle-info", class = "fa-2x mb-2"),
      p(msg, class = "mb-0")
    )
  }

  # ---- Shared community colour palette ---------------------------------------
  # Keys are "C1", "C2", ... (names of result$communities / result$psi)
  community_palette <- reactive({
    req(result_obj())
    comm_keys <- names(result_obj()$psi)          # "C1", "C2", ...
    comm_keys <- comm_keys[order(as.integer(sub("^C", "", comm_keys)))]
    n_c  <- length(comm_keys)
    cols <- grDevices::hcl.colors(n_c, palette = "Set2")
    setNames(cols, comm_keys)
  })

  # ---- Map plot (plotly) -----------------------------------------------------
  map_reactive <- reactive({
    req(result_obj())
    build_map_plotly(result_obj(), custom_labels(), community_palette())
  })

  output$map_placeholder <- renderUI({
    if (is.null(result_obj())) {
      placeholder_card("Run the analysis to view the ThemeScope map.")
    }
  })

  output$map_plot <- renderPlotly({
    req(result_obj())
    map_reactive()
  })

  # ---- Network plot (visNetwork) ---------------------------------------------
  network_reactive <- reactive({
    req(result_obj())
    hide_unc <- isTRUE(input$net_hide_unclassified)
    build_network_visnetwork(result_obj(), custom_labels(), community_palette(),
                             hide_unclassified = hide_unc)
  })

  output$network_placeholder <- renderUI({
    if (is.null(result_obj())) {
      placeholder_card("Run the analysis to view the co-occurrence network.")
    }
  })

  output$network_plot <- visNetwork::renderVisNetwork({
    req(result_obj())
    network_reactive()
  })

  # ---- Communities table -----------------------------------------------------
  communities_df <- reactive({
    req(result_obj())
    df <- as.data.frame(result_obj())
    # Apply custom labels if available
    cl <- custom_labels()
    if (!is.null(cl)) {
      idx <- match(df$community_id, names(cl))
      df$label[!is.na(idx)] <- cl[idx[!is.na(idx)]]
    }
    df
  })

  output$communities_placeholder <- renderUI({
    if (is.null(result_obj())) {
      placeholder_card("Run the analysis to view community statistics.")
    }
  })

  output$communities_table <- renderDT({
    req(result_obj())
    df <- communities_df()
    df$psi     <- round(df$psi,    4)
    df$cs      <- round(df$cs,     3)
    df$psi_z   <- round(df$psi_z,  3)
    df$cs_z    <- round(df$cs_z,   3)

    datatable(
      df,
      rownames  = FALSE,
      selection = "none",
      editable  = list(target = "cell", disable = list(columns = c(0, 2:7))),
      colnames  = c(
        "Community ID", "Label", "Size",
        "PSI", "CS", "PSI (z)", "CS (z)", "Quadrant"
      ),
      options = list(
        dom        = "tp",
        pageLength = 20,
        scrollX    = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = c(2, 3, 4, 5, 6)),
          list(width = "160px", targets = 1)
        )
      ),
      class = "stripe hover compact"
    ) |>
      formatStyle(
        "quadrant",
        backgroundColor = styleEqual(
          c("Stable Core", "Ideological Core",
            "Emerging Practices", "Latent Representations"),
          c("#d6eaf8", "#fadbd8", "#d5f5e3", "#e8daef")
        )
      ) |>
      formatStyle(
        "psi",
        background = styleColorBar(range(df$psi, na.rm = TRUE), "#aed6f1"),
        backgroundSize    = "100% 80%",
        backgroundRepeat  = "no-repeat",
        backgroundPosition = "center"
      )
  })

  # Capture cell edits (label column = index 1 in 0-based)
  observeEvent(input$communities_table_cell_edit, {
    info <- input$communities_table_cell_edit
    if (info$col == 1) {   # Label column (0-indexed)
      df     <- communities_df()
      cid    <- df$community_id[info$row]
      cl     <- custom_labels()
      if (is.null(cl)) {
        cl <- setNames(df$label, df$community_id)
      }
      cl[cid] <- info$value
      custom_labels(cl)
    }
  })

  output$label_update_ui <- renderUI({
    req(result_obj())
    actionButton(
      "update_labels",
      label = tagList(icon("rotate"), " Update Map Labels"),
      class = "btn-sm btn-outline-primary"
    )
  })

  # Update map labels button — custom_labels already stored via cell_edit,
  # so this just triggers a notification
  observeEvent(input$update_labels, {
    req(result_obj())
    cl <- custom_labels()
    if (is.null(cl)) {
      showNotification(
        "No label edits found. Double-click a label cell to edit.",
        type = "warning", duration = 4
      )
    } else {
      showNotification(
        "Map labels updated. Switch to the Map tab to view.",
        type = "message", duration = 3
      )
    }
  })

  # ---- Summary tab -----------------------------------------------------------
  output$summary_placeholder_net <- renderUI({
    if (is.null(result_obj())) {
      placeholder_card("Run the analysis to view statistics.")
    }
  })

  output$summary_placeholder_params <- renderUI({ NULL })

  output$network_stats_table <- renderTable({
    req(result_obj())
    gs <- result_obj()$network_stats$global_stats
    data.frame(
      Statistic = c("Nodes", "Edges", "Mean degree",
                    "Modularity", "Communities"),
      Value     = c(
        as.character(gs$n_nodes),
        as.character(gs$n_edges),
        sprintf("%.3f", gs$mean_degree),
        sprintf("%.4f", gs$modularity),
        as.character(gs$n_communities)
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s")

  output$params_table <- renderTable({
    req(result_obj())
    p <- result_obj()$params
    data.frame(
      Parameter = c(
        "Vocabulary size",
        "POS filter",
        "AS threshold percentile",
        "Community detection",
        "Walktrap steps",
        "Min community size",
        "Concreteness lexicon"
      ),
      Value = c(
        as.character(p$vocab_size),
        paste(p$pos_filter, collapse = ", "),
        as.character(p$threshold_percentile),
        toupper(p$community_algorithm),
        if (p$community_algorithm == "walktrap") as.character(p$walktrap_steps) else "—",
        as.character(p$min_community_size),
        if (!is.null(input$lexicon_upload)) input$lexicon_upload$name else "Brysbaert et al. (2014) — bundled"
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s")

  output$summary_text <- renderPrint({
    req(result_obj())
    summary(result_obj())
  })

  # ---- Top Terms tab ---------------------------------------------------------
  top_terms_df <- reactive({
    req(result_obj())
    n <- input$top_n_terms
    get_top_terms(result_obj(), n = n)
  })

  output$topterms_panels <- renderUI({
    if (is.null(result_obj())) {
      return(placeholder_card("Run the analysis to view top terms per community."))
    }
    df  <- top_terms_df()
    cl  <- custom_labels()
    comms <- sort(unique(df$community))

    get_label <- function(cid) {
      lbl <- if (!is.null(cl) && as.character(cid) %in% names(cl))
        cl[as.character(cid)] else as.character(cid)
      paste0("Community ", lbl)
    }

    panels <- lapply(comms, function(cid) {
      pid <- paste0("comm_", gsub("[^A-Za-z0-9]", "_", cid))
      nav_panel(
        title = get_label(cid),
        div(
          style = "display: flex; gap: 16px; align-items: flex-start;",
          div(
            style = "flex: 1; min-width: 0;",
            withSpinner(
              plotlyOutput(paste0("topterms_plot_", pid), height = "420px"),
              type = 6, color = "#2c3e50"
            )
          ),
          div(
            style = "flex: 1; min-width: 0;",
            withSpinner(
              DTOutput(paste0("topterms_tbl_", pid)),
              type = 6, color = "#2c3e50"
            )
          )
        )
      )
    })

    do.call(navset_pill, c(panels, list(id = "topterms_comm_tabs")))
  })

  observe({
    req(result_obj())
    df  <- top_terms_df()
    cl  <- custom_labels()
    pal <- community_palette()
    comms <- sort(unique(df$community))

    lapply(comms, function(cid) {
      local({
        lcid <- cid
        pid  <- paste0("comm_", gsub("[^A-Za-z0-9]", "_", lcid))

        output[[paste0("topterms_plot_", pid)]] <- renderPlotly({
          sub <- df[df$community == lcid & !is.na(df$term), ]
          sub <- sub[order(sub$degree), ]
          plot_ly(sub,
            x    = ~degree, y = ~reorder(term, degree),
            type = "bar", orientation = "h",
            marker = list(color = pal[as.character(lcid)], opacity = 0.85),
            hovertemplate = "<b>%{y}</b><br>Degree: %{x}<extra></extra>",
            showlegend = FALSE
          ) |>
            layout(
              xaxis = list(title = "Degree", showgrid = TRUE, gridcolor = "#eeeeee"),
              yaxis = list(title = "", automargin = TRUE),
              plot_bgcolor  = "white",
              paper_bgcolor = "white",
              margin = list(l = 10, r = 10, t = 20, b = 40)
            ) |>
            config(displayModeBar = FALSE)
        })

        output[[paste0("topterms_tbl_", pid)]] <- renderDT({
          sub <- df[df$community == lcid & !is.na(df$term), ]
          sub <- sub[, c("rank", "term", "degree")]
          datatable(
            sub,
            rownames = FALSE,
            colnames = c("Rank", "Term", "Degree"),
            options  = list(
              dom        = "tp",
              pageLength = 15,
              scrollX    = TRUE
            ),
            class = "stripe hover compact"
          ) |>
            formatStyle(
              "degree",
              background = styleColorBar(range(sub$degree, na.rm = TRUE), "#a9cce3"),
              backgroundSize     = "100% 80%",
              backgroundRepeat   = "no-repeat",
              backgroundPosition = "center"
            )
        })
      })
    })
  })

  # ---- Download handlers -----------------------------------------------------
  # Nota: PNG/PDF sono disponibili direttamente dalla toolbar plotly (camera icon).
  # I download button esportano l'HTML interattivo self-contained.

  save_plotly_html <- function(p, file) {
    htmlwidgets::saveWidget(p, file, selfcontained = TRUE, libdir = NULL)
  }

  output$dl_map_png <- downloadHandler(
    filename = function() paste0("themescope_map_", Sys.Date(), ".html"),
    content  = function(file) { req(result_obj()); save_plotly_html(map_reactive(), file) }
  )

  output$dl_map_pdf <- downloadHandler(
    filename = function() paste0("themescope_map_", Sys.Date(), ".html"),
    content  = function(file) { req(result_obj()); save_plotly_html(map_reactive(), file) }
  )

  build_network_export <- function() {
    net <- build_network_visnetwork(result_obj(), custom_labels(), community_palette())
    net$x$width  <- "1400px"
    net$x$height <- "900px"
    net
  }

  output$dl_net_png <- downloadHandler(
    filename = function() paste0("themescope_network_", Sys.Date(), ".html"),
    content  = function(file) {
      req(result_obj())
      visNetwork::visSave(build_network_export(), file = file, selfcontained = TRUE)
    }
  )

  output$dl_net_pdf <- downloadHandler(
    filename = function() paste0("themescope_network_", Sys.Date(), ".html"),
    content  = function(file) {
      req(result_obj())
      visNetwork::visSave(build_network_export(), file = file, selfcontained = TRUE)
    }
  )
}

# ==============================================================================
# Launch
# ==============================================================================

shinyApp(ui = ui, server = server)
