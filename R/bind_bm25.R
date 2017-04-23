#' Bind BM25 Score
#'
#' @param tbl A tidy text dataset with one-row-per-term-per-document
#' @param term_col Column containing terms
#' @param document_col Column containing document IDs
#' @param n_col Column containing document-term counts
#' @param k tuning parameter to modify the impact the TF. higher k causes TF to take longer to reach saturation.
#' @param b tuning parameter to control the effect of document length normalization. \code{b = 0} disables normalization and \code{b = 1} enables full normalization.
#'
#' @details
#' BM25 (also known as Okapi BM25) is probabilistic term weighting model. One of its uses is search engines, where it is used as a ranking function.
#'
#' \code{bind_bm25} is given bare names, while \code{bind_bm25_}
#' is given strings.
#' The dataset must have one row per document-term combination.
#' @examples
#'
#' library(dplyr)
#' library(janeaustenr)
#'
#' book_words <- austen_books() %>%
#'   unnest_tokens(word, text) %>%
#'   anti_join(stop_words) %>%
#'   count(book, word, sort = TRUE) %>%
#'   ungroup()
#'
#' # calculate the BM25 score for words in each document
#' book_words %>%
#'   bind_bm25(word, book, n, k = 1.2, b = 1) %>%
#'   arrange(desc(bm25))

#' @export
bind_bm25 <- function(tbl, term_col, document_col, n_col, k = 1.2, b = 1) {
  bind_bm25_(tbl,
               col_name(substitute(term_col)),
               col_name(substitute(document_col)),
               col_name(substitute(n_col)),
               k = k,
               b = b)
}


#' @rdname bind_bm25
#' @export
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
