## Tests for R/metrics.R

# ---- Helpers ----------------------------------------------------------------

# Build a reproducible small network for testing metrics
make_test_network <- function() {
  # 20-term network, 2 clear communities of 10
  set.seed(42)
  comm1 <- paste0("term_", 1:10)
  comm2 <- paste0("term_", 11:20)
  all_terms <- c(comm1, comm2)

  # Dense within-community edges, sparse between
  edges <- c()
  weights <- c()

  # Within comm1: all pairs
  for (i in 1:(length(comm1) - 1)) {
    for (j in (i + 1):length(comm1)) {
      edges   <- c(edges, comm1[i], comm1[j])
      weights <- c(weights, stats::runif(1, 0.1, 0.9))
    }
  }
  # Within comm2: all pairs
  for (i in 1:(length(comm2) - 1)) {
    for (j in (i + 1):length(comm2)) {
      edges   <- c(edges, comm2[i], comm2[j])
      weights <- c(weights, stats::runif(1, 0.1, 0.9))
    }
  }
  # A few cross-community edges (weak)
  edges   <- c(edges, "term_1", "term_11", "term_2", "term_12")
  weights <- c(weights, 0.01, 0.01)

  g <- igraph::make_graph(edges, directed = FALSE)
  igraph::E(g)$weight <- weights

  communities <- list(C1 = comm1, C2 = comm2)

  # Build membership
  membership <- c(
    stats::setNames(rep(1L, length(comm1)), comm1),
    stats::setNames(rep(2L, length(comm2)), comm2)
  )

  # Raw frequencies (simulate token counts)
  freq <- stats::setNames(
    as.integer(round(stats::runif(length(all_terms), 5, 50))),
    all_terms
  )

  list(graph = g, communities = communities, membership = membership,
       freq = freq)
}


# ---- compute_PSI ------------------------------------------------------------

test_that("compute_PSI returns a named numeric vector", {
  obj  <- make_test_network()
  psi  <- compute_PSI(obj$graph, obj$communities, obj$freq)
  expect_type(psi, "double")
  expect_named(psi)
})

test_that("PSI values are in [0, 1]", {
  obj <- make_test_network()
  psi <- compute_PSI(obj$graph, obj$communities, obj$freq)
  expect_true(all(psi >= 0 - .Machine$double.eps))
  expect_true(all(psi <= 1 + .Machine$double.eps))
})

test_that("maximum PSI equals 1", {
  obj <- make_test_network()
  psi <- compute_PSI(obj$graph, obj$communities, obj$freq)
  expect_equal(max(psi), 1, tolerance = 1e-10)
})

test_that("PSI names match community names", {
  obj <- make_test_network()
  psi <- compute_PSI(obj$graph, obj$communities, obj$freq)
  expect_equal(sort(names(psi)), sort(names(obj$communities)))
})

test_that("compute_PSI errors on non-igraph graph", {
  obj <- make_test_network()
  expect_error(compute_PSI("not_a_graph", obj$communities, obj$freq))
})

test_that("PSI is zero when all frequencies are equal and degrees are equal", {
  # Two communities with uniform frequency — PSI is proportional to mean
  # degree, so all equal communities should still have max = 1
  obj   <- make_test_network()
  freq2 <- stats::setNames(rep(10L, length(obj$freq)), names(obj$freq))
  psi   <- compute_PSI(obj$graph, obj$communities, freq2)
  expect_equal(max(psi), 1, tolerance = 1e-10)
})


# ---- compute_CS -------------------------------------------------------------

make_lexicon <- function(terms, low = 1.5, high = 4.5) {
  # Assign alternating low/high concreteness for predictability
  vals <- rep_len(c(low, high), length(terms))
  data.frame(word = terms, conc.m = vals, stringsAsFactors = FALSE)
}

test_that("compute_CS returns a named numeric vector", {
  obj <- make_test_network()
  all_terms <- unlist(obj$communities)
  lex <- make_lexicon(all_terms)
  cs  <- compute_CS(obj$graph, obj$communities, lex)
  expect_type(cs, "double")
  expect_named(cs)
})

test_that("CS values are in [1, 5] range when all edges covered", {
  obj <- make_test_network()
  all_terms <- unlist(obj$communities)
  # Give all terms scores between 1 and 5
  lex <- data.frame(
    word   = all_terms,
    conc.m = stats::runif(length(all_terms), 1, 5),
    stringsAsFactors = FALSE
  )
  cs <- compute_CS(obj$graph, obj$communities, lex)
  valid_cs <- cs[!is.na(cs)]
  expect_true(all(valid_cs >= 1 - .Machine$double.eps * 10))
  expect_true(all(valid_cs <= 5 + .Machine$double.eps * 10))
})

test_that("CS is NA when no terms are in lexicon", {
  obj <- make_test_network()
  lex <- data.frame(word = "notaword", conc.m = 3.0,
                     stringsAsFactors = FALSE)
  cs  <- compute_CS(obj$graph, obj$communities, lex)
  expect_true(all(is.na(cs)))
})

test_that("CS names match community names", {
  obj <- make_test_network()
  all_terms <- unlist(obj$communities)
  lex <- make_lexicon(all_terms)
  cs  <- compute_CS(obj$graph, obj$communities, lex)
  expect_equal(sort(names(cs)), sort(names(obj$communities)))
})

test_that("CS is correct for a known trivial case", {
  # Single-community, single edge, both endpoints in lexicon with known scores
  g <- igraph::make_graph(c("a", "b"), directed = FALSE)
  igraph::E(g)$weight <- 1.0
  comms <- list(C1 = c("a", "b"))
  lex   <- data.frame(word = c("a", "b"), conc.m = c(2.0, 4.0),
                       stringsAsFactors = FALSE)
  cs <- compute_CS(g, comms, lex)
  # Expected: (1.0 * (2.0 + 4.0) / 2) / 1.0 = 3.0
  expect_equal(as.numeric(cs["C1"]), 3.0, tolerance = 1e-10)
})

test_that("CS errors on missing lexicon columns", {
  obj <- make_test_network()
  bad_lex <- data.frame(word = "dog", concreteness = 3.5,
                         stringsAsFactors = FALSE)
  expect_error(compute_CS(obj$graph, obj$communities, bad_lex))
})


# ---- compute_network_stats --------------------------------------------------

test_that("compute_network_stats returns expected structure", {
  obj   <- make_test_network()
  stats <- compute_network_stats(obj$graph, obj$communities)
  expect_named(stats, c("community_stats", "global_stats"))
  expect_s3_class(stats$community_stats, "data.frame")
  expect_type(stats$global_stats, "list")
})

test_that("community_stats has correct number of rows", {
  obj   <- make_test_network()
  stats <- compute_network_stats(obj$graph, obj$communities)
  expect_equal(nrow(stats$community_stats), length(obj$communities))
})

test_that("global_stats contains required fields", {
  obj   <- make_test_network()
  stats <- compute_network_stats(obj$graph, obj$communities)
  gs    <- stats$global_stats
  expect_true(all(c("n_nodes", "n_edges", "mean_degree",
                    "modularity", "n_communities") %in% names(gs)))
})

test_that("global_stats n_nodes matches graph", {
  obj   <- make_test_network()
  stats <- compute_network_stats(obj$graph, obj$communities)
  expect_equal(stats$global_stats$n_nodes, igraph::vcount(obj$graph))
})


# ---- compute_coherence ------------------------------------------------------

test_that("compute_coherence returns values in [0, 1]", {
  obj <- make_test_network()
  coh <- compute_coherence(obj$graph, obj$communities)
  valid <- coh[!is.na(coh)]
  expect_true(all(valid >= 0 - .Machine$double.eps))
  expect_true(all(valid <= 1 + .Machine$double.eps))
})

test_that("complete community has coherence 1", {
  # Clique of 4: complete graph
  g <- igraph::make_full_graph(4, directed = FALSE)
  igraph::V(g)$name <- c("a", "b", "c", "d")
  comms <- list(C1 = c("a", "b", "c", "d"))
  coh   <- compute_coherence(g, comms)
  expect_equal(as.numeric(coh["C1"]), 1.0, tolerance = 1e-10)
})
