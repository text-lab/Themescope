# ThemeScope

**Social Representation Analysis via Semantic Network Mapping**

ThemeScope is a computational framework for detecting and visualising social representations in large-scale digital text corpora. It combines sentence-level word co-occurrence networks with Social Representation Theory (SRT), operationalising *anchoring* (PSI) and *objectification* (CS) as quantitative measures.

## Repository structure

```
ThemeScope/
├── pkg/ThemeScope/          # R package (install with devtools)
│   ├── R/                   # Source functions
│   ├── data/                # Bundled Brysbaert concreteness lexicon
│   ├── man/                 # Documentation (auto-generated)
│   └── tests/               # Unit tests
├── shiny/
│   └── app.R                # Interactive Shiny application
├── prepare_sample.R         # Script to subsample a large annotated corpus
└── brysbaert_concreteness.csv  # Brysbaert et al. (2014) norms — source CSV
```

## Installation

```r
# Install the package from this repository
# install.packages("devtools")
devtools::install_github("<your-username>/ThemeScope", subdir = "pkg/ThemeScope")
```

Or locally:

```r
devtools::install("pkg/ThemeScope")
```

## Quick start

```r
library(ThemeScope)

# tokens_df must have columns: doc_id, sentence_id, lemma, upos
# (output of udpipe annotation — see ?annotate_corpus)

result <- themescope(tokens_df, vocab_size = 1500)

plot(result, type = "map")      # ThemeScope 2D map
plot(result, type = "network")  # Co-occurrence network
summary(result)
as.data.frame(result)
```

## Shiny application

```r
shiny::runApp("shiny/app.R")
```

The app provides a point-and-click interface for the full pipeline:
upload an annotated CSV → configure parameters → run analysis → explore interactive maps and networks.

## Preparing input data

Input requires a POS-tagged, lemmatised corpus. Use `udpipe`:

```r
library(udpipe)
model <- udpipe_download_model("english")
ud    <- udpipe_load_model(model$file_model)
ann   <- udpipe_annotate(ud, x = my_texts, doc_id = seq_along(my_texts))
tokens_df <- as.data.frame(ann)[, c("doc_id", "sentence_id", "lemma", "upos")]
```

For large corpora, use `prepare_sample.R` to extract a stratified subsample.

## Methodology

### Core measures

| Measure | Formula | SRT construct |
|---------|---------|--------------|
| **PSI** | Ψ(Gᵢ) = Σ(fₜ · dₜ) / max D(G) | Anchoring |
| **CS**  | Cw(Gᵢ) = Σ wₜₜ′ · (c(t)+c(t′))/2 / Σ wₜₜ′ | Objectification |

- *fₜ* = normalised term frequency; *dₜ* = intra-community degree; *D* = community density
- *wₜₜ′* = Association Strength edge weight; *c(t)* = concreteness rating (Brysbaert 1–5)

### Four quadrants

|  | **High CS** | **Low CS** |
|---|---|---|
| **High PSI** | Stable Core | Ideological Core |
| **Low PSI** | Emerging Practices | Latent Representations |

## Dependencies

**R package:** `igraph`, `ggplot2`, `ggrepel`, `Matrix`, `cli`, `dplyr`, `grDevices`
**Shiny app:** `shiny`, `bslib`, `DT`, `plotly`, `visNetwork`, `shinycssloaders`
**Optional:** `udpipe` (for corpus annotation)

## Concreteness lexicon

The Brysbaert et al. (2014) concreteness norms (39,954 English words) are bundled in the package as `ThemeScope::brysbaert`.

> Brysbaert, M., Warriner, A.B., & Kuperman, V. (2014). Concreteness ratings for 40 thousand generally known English word lemmas. *Behavior Research Methods*, 46(3), 904–911. https://doi.org/10.3758/s13428-013-0403-5

## License

MIT
