# ==============================================================================
# ThemeScope — Preparazione subsample da dfTag_climate_complete.csv
# ==============================================================================
# Output: tokens_sample_<N>docs.csv  pronto per ThemeScope / Shiny
# ==============================================================================

library(data.table)

# ---- Parametri ---------------------------------------------------------------
INPUT_FILE  <- "dfTag_climate_complete.csv"
N_DOCS      <- 10000     # numero di documenti da campionare
SEED        <- 42
# Stratificazione: NULL = campione casuale, "subreddit" = proporzionale per subreddit
STRAT_BY    <- "subreddit"
OUTPUT_FILE <- paste0("tokens_sample_", N_DOCS, "docs.csv")

cat("=== ThemeScope Sample Preparation ===\n")
cat("Input :", INPUT_FILE, "\n")
cat("N docs:", N_DOCS, "\n")
cat("Seed  :", SEED, "\n\n")

# ---- Step 1: leggi solo i doc_id + metadati ----------------------------------
cat("[1/4] Lettura doc_id e metadati...\n")
t0 <- proc.time()

meta <- fread(
  INPUT_FILE,
  select = c("doc_id", "date", "subreddit", "score"),
  colClasses = list(character = c("doc_id", "subreddit"),
                    character = "date",
                    integer   = "score")
)

# Collassa a un record per documento (primo token = metadato del doc)
meta_docs <- unique(meta, by = "doc_id")
cat("   Doc totali:", nrow(meta_docs), "\n")
cat("   Subreddit :", uniqueN(meta_docs$subreddit), "\n")
cat("   Range date:", paste(range(meta_docs$date), collapse = " / "), "\n")
cat(sprintf("   Tempo: %.1f s\n\n", (proc.time() - t0)["elapsed"]))

# ---- Step 2: campionamento ---------------------------------------------------
cat("[2/4] Campionamento", N_DOCS, "documenti...\n")
set.seed(SEED)

if (!is.null(STRAT_BY) && STRAT_BY %in% names(meta_docs)) {
  # Campionamento stratificato proporzionale
  tab <- table(meta_docs[[STRAT_BY]])
  props <- round(prop.table(tab) * N_DOCS)
  # Aggiusta per arrivare esattamente a N_DOCS
  diff_n <- N_DOCS - sum(props)
  if (diff_n != 0) {
    idx_adj <- order(props, decreasing = (diff_n > 0))[seq_len(abs(diff_n))]
    props[idx_adj] <- props[idx_adj] + sign(diff_n)
  }

  sampled_ids <- character(0)
  for (sub in names(props)) {
    pool <- meta_docs[subreddit == sub, doc_id]
    n_sub <- min(props[sub], length(pool))
    if (n_sub > 0) sampled_ids <- c(sampled_ids, sample(pool, n_sub))
  }
  cat("   Distribuzione subreddit nel campione:\n")
  tab_sample <- table(meta_docs[doc_id %in% sampled_ids, subreddit])
  print(sort(tab_sample, decreasing = TRUE))
} else {
  # Campionamento casuale semplice
  sampled_ids <- sample(meta_docs$doc_id, min(N_DOCS, nrow(meta_docs)))
}

cat(sprintf("\n   Documenti selezionati: %d\n\n", length(sampled_ids)))

# ---- Step 3: leggi e filtra i token per i doc selezionati --------------------
cat("[3/4] Lettura token per i documenti selezionati (può richiedere qualche minuto)...\n")
t1 <- proc.time()

tokens_full <- fread(
  INPUT_FILE,
  select = c("doc_id", "sentence_id", "lemma", "upos", "date", "subreddit", "score")
)

tokens_sample <- tokens_full[doc_id %in% sampled_ids]
rm(tokens_full)
gc()

cat(sprintf("   Token totali nel campione: %s\n", format(nrow(tokens_sample), big.mark = ",")))
cat(sprintf("   Frasi uniche            : %s\n", format(uniqueN(paste0(tokens_sample$doc_id, "_", tokens_sample$sentence_id)), big.mark = ",")))
cat(sprintf("   Tempo: %.1f s\n\n", (proc.time() - t1)["elapsed"]))

# ---- Step 4: prepara formato ThemeScope e salva ------------------------------
cat("[4/4] Preparazione formato ThemeScope e salvataggio...\n")

# sentence_id globalmente unico
tokens_sample[, sentence_id := paste0(doc_id, "_s", sentence_id)]

# Rimuovi token senza lemma
tokens_sample <- tokens_sample[!is.na(lemma) & lemma != ""]

# Lemma in minuscolo (coerente col lessico Brysbaert)
tokens_sample[, lemma := tolower(lemma)]

# Statistiche POS
cat("   Distribuzione POS (top 10):\n")
print(sort(table(tokens_sample$upos), decreasing = TRUE)[1:10])
n_useful <- nrow(tokens_sample[upos %in% c("NOUN", "ADJ", "PROPN")])
cat(sprintf("\n   Token NOUN+ADJ+PROPN: %s (%.1f%%)\n",
            format(n_useful, big.mark = ","),
            100 * n_useful / nrow(tokens_sample)))

# Salva — include metadati per usi futuri
fwrite(tokens_sample, OUTPUT_FILE)

file_mb <- round(file.size(OUTPUT_FILE) / 1024^2, 1)
cat(sprintf("\n✓ Salvato: %s (%.1f MB)\n", OUTPUT_FILE, file_mb))
cat(sprintf("  Righe  : %s\n",   format(nrow(tokens_sample), big.mark = ",")))
cat(sprintf("  Doc    : %s\n",   format(uniqueN(tokens_sample$doc_id), big.mark = ",")))
cat(sprintf("  Frasi  : %s\n\n", format(uniqueN(tokens_sample$sentence_id), big.mark = ",")))

cat("=== Pronto per ThemeScope ===\n")
cat("Carica", OUTPUT_FILE, "nella Shiny (colonne: doc_id, sentence_id, lemma, upos)\n")
