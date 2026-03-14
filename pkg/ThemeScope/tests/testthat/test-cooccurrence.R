## Tests for R/cooccurrence.R

# ---- Helpers ----------------------------------------------------------------

# A small reproducible tokens data frame
make_tokens <- function() {
  data.frame(
    doc_id      = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
    sentence_id = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 1, 1, 1, 2, 2),
    lemma       = c("dog", "cat",  "bird",
                    "dog", "fish", "cat",
                    "bird", "dog", "fish",
                    "cat",  "dog", "bird",
                    "fish", "cat"),
    upos        = rep("NOUN", 14),
    stringsAsFactors = FALSE
  )
}


# ---- build_vocab ------------------------------------------------------------

test_that("build_vocab returns a character vector", {
  toks <- make_tokens()
  vocab <- build_vocab(toks, vocab_size = 10)
  expect_type(vocab, "character")
})

test_that("build_vocab respects vocab_size", {
  toks <- make_tokens()
  vocab <- build_vocab(toks, vocab_size = 3)
  expect_lte(length(vocab), 3)
})

test_that("build_vocab returns the most frequent terms first", {
  toks <- make_tokens()
  vocab <- build_vocab(toks, vocab_size = 10)
  # dog appears 4 times, cat 4 times, fish 3 times, bird 3 times
  expect_true(all(c("dog", "cat") %in% vocab[1:2]))
})

test_that("build_vocab filters by POS", {
  toks <- make_tokens()
  # Add some non-NOUN tokens
  extra <- data.frame(
    doc_id = 1, sentence_id = 1,
    lemma = "quickly", upos = "ADV",
    stringsAsFactors = FALSE
  )
  toks2 <- rbind(toks, extra)
  vocab <- build_vocab(toks2, vocab_size = 10,
                        pos_filter = c("NOUN"))
  expect_false("quickly" %in% vocab)
})

test_that("build_vocab uses lemma over token", {
  toks <- data.frame(
    doc_id = 1, sentence_id = 1,
    token  = "dogs",
    lemma  = "dog",
    upos   = "NOUN",
    stringsAsFactors = FALSE
  )
  vocab <- build_vocab(toks, vocab_size = 5)
  expect_true("dog" %in% vocab)
  expect_false("dogs" %in% vocab)
})

test_that("build_vocab errors on missing required columns", {
  bad <- data.frame(x = 1:3, stringsAsFactors = FALSE)
  expect_error(build_vocab(bad))
})


# ---- build_cooccurrence_matrix ----------------------------------------------

test_that("build_cooccurrence_matrix returns the expected list structure", {
  toks   <- make_tokens()
  result <- build_cooccurrence_matrix(toks, vocab_size = 10)
  expect_named(result, c("cooc_matrix", "freq", "vocab"))
})

test_that("co-occurrence matrix is square", {
  toks   <- make_tokens()
  result <- build_cooccurrence_matrix(toks, vocab_size = 10)
  m <- result$cooc_matrix
  expect_equal(nrow(m), ncol(m))
})

test_that("co-occurrence matrix is symmetric", {
  toks   <- make_tokens()
  result <- build_cooccurrence_matrix(toks, vocab_size = 10)
  m <- result$cooc_matrix
  # Check that M == t(M) element-wise
  diff_mat <- m - Matrix::t(m)
  expect_equal(Matrix::nnzero(diff_mat), 0L)
})

test_that("diagonal of co-occurrence matrix is zero", {
  toks   <- make_tokens()
  result <- build_cooccurrence_matrix(toks, vocab_size = 10)
  m <- result$cooc_matrix
  expect_true(all(Matrix::diag(m) == 0))
})

test_that("co-occurrence counts are non-negative integers", {
  toks   <- make_tokens()
  result <- build_cooccurrence_matrix(toks, vocab_size = 10)
  vals   <- result$cooc_matrix@x
  expect_true(all(vals >= 0))
  expect_true(all(vals == floor(vals)))
})

test_that("freq vector has names matching vocab", {
  toks   <- make_tokens()
  result <- build_cooccurrence_matrix(toks, vocab_size = 10)
  expect_equal(sort(names(result$freq)), sort(result$vocab))
})

test_that("known co-occurrence is counted correctly", {
  # In sentence 1 (doc 1): dog, cat, bird all co-occur
  # In sentence 2 (doc 1): dog, fish, cat all co-occur
  # In sentence 3 (doc 1): bird, dog, fish all co-occur
  # In sentence 1 (doc 2): cat, dog, bird all co-occur
  # In sentence 2 (doc 2): fish, cat all co-occur
  # dog-cat co-occur in sentences: d1s1, d1s2, d2s1 => 3 times
  toks   <- make_tokens()
  result <- build_cooccurrence_matrix(toks, vocab_size = 10)
  m <- result$cooc_matrix
  expect_equal(as.numeric(m["dog", "cat"]), 3)
  expect_equal(as.numeric(m["cat", "dog"]), 3)
})


# ---- compute_association_strength -------------------------------------------

test_that("association strength returns a Matrix", {
  toks   <- make_tokens()
  r      <- build_cooccurrence_matrix(toks, vocab_size = 10)
  as_mat <- compute_association_strength(r$cooc_matrix, r$freq)
  expect_true(inherits(as_mat, "Matrix"))
})

test_that("association strength values are in [0, 1]", {
  toks   <- make_tokens()
  r      <- build_cooccurrence_matrix(toks, vocab_size = 10)
  as_mat <- compute_association_strength(r$cooc_matrix, r$freq)
  vals   <- as_mat@x
  expect_true(all(vals >= 0))
  # AS can theoretically exceed 1 only if co-occ > freq, which is impossible
  # by construction; verify upper bound
  expect_true(all(vals <= 1 + .Machine$double.eps * 100))
})

test_that("association strength matrix is symmetric", {
  toks   <- make_tokens()
  r      <- build_cooccurrence_matrix(toks, vocab_size = 10)
  as_mat <- compute_association_strength(r$cooc_matrix, r$freq)
  diff_mat <- as_mat - Matrix::t(as_mat)
  max_diff <- max(abs(diff_mat@x))
  expect_lt(max_diff, 1e-10)
})

test_that("diagonal of AS matrix is non-trivially computed but can be 0", {
  # The diagonal of cooc_matrix is always 0, so AS diagonal is also 0
  toks   <- make_tokens()
  r      <- build_cooccurrence_matrix(toks, vocab_size = 10)
  as_mat <- compute_association_strength(r$cooc_matrix, r$freq)
  expect_true(all(Matrix::diag(as_mat) == 0))
})

test_that("AS formula: sA(i,j) = c(i,j)^2 / (f(i)*f(j))", {
  # Manual check for dog-cat pair
  toks   <- make_tokens()
  r      <- build_cooccurrence_matrix(toks, vocab_size = 10)
  as_mat <- compute_association_strength(r$cooc_matrix, r$freq)

  c_ij <- as.numeric(r$cooc_matrix["dog", "cat"])
  f_i  <- unname(r$freq["dog"])
  f_j  <- unname(r$freq["cat"])
  expected <- c_ij^2 / (f_i * f_j)

  obtained <- as.numeric(as_mat["dog", "cat"])
  expect_equal(obtained, unname(expected), tolerance = 1e-10)
})
