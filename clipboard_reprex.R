
#' ---
#' output:
#'   md_document
#' ---
#'<!-- language-all: lang-r --><br/>
#+ reprex-setup, include = FALSE
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", error = TRUE)
knitr::opts_knit$set(upload.fun = knitr::imgur_upload)


knitr::opts_chunk$set(tidy = TRUE, tidy.opts = list(indent = 2))
#+ reprex-body
y <- boxplot(1:100);y$stats


