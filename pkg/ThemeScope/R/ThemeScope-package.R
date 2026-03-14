#' @keywords internal
"_PACKAGE"

# Suppress R CMD check NOTEs about ggplot2 aes() column names
utils::globalVariables(c(
  "psi_z", "cs_z", "size", "label", "quadrant",
  "community", "node_degree", "node_label", "weight",
  "brysbaert"
))
