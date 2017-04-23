
bind_bm25 <- function(tbl, term_col, document_col, n_col, k = 1.2, b = 1) {
  bind_bm25_(tbl,
               col_name(substitute(term_col)),
               col_name(substitute(document_col)),
               col_name(substitute(n_col)),
               k = k,
               b = b)
}




bind_bm25_ <- function(tbl, term_col, document_col, n_col, k = 1.2, b = 1) {
  terms <- tbl[[term_col]]
  documents <- tbl[[document_col]]
  n <- tbl[[n_col]]


  doc_totals <- tapply(n, documents, sum)
  avg_dl <- mean(doc_totals)


  idf <- log(length(doc_totals) / table(terms))

  tbl$tf_bm25 <- ((k+1)*n)/(n+(k*((1-b)+b*(as.numeric(doc_totals[documents])/avg_dl))))
  tbl$idf <- as.numeric(idf[terms])
  tbl$bm25 <- tbl$tf_bm25 * tbl$idf

  tbl
}
