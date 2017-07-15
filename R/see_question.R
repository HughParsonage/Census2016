#' View census form for particular question
#' 
#' @rdname see_question
#' @param ... Passed to method: either a question number or a data frame.
#' @param qn Question number.
#' @param .data A \code{data.frame} from this package.
#' @return Primarily called for its side effect: plots an image of the question (and available answers, if appropriate) in the plot window.
#' If a \code{data.frame} from this package is called, it is returned (invisibly if \code{knitr.in.progress} is \code{TRUE}).
#' 
#' @examples
#' \dontrun{
#' see_question(5)
#' see_question(Census2016_languages)
#' }
#' 
#' @importFrom graphics plot.new
#' @importFrom graphics plot.window
#' @importFrom graphics rasterImage
#' @export 
#' 
see_question <- function(...) {
  UseMethod("see_question")
}

#' @rdname see_question
#' @export
see_question.data.frame <- function(.data, ...) {
  data_name <- deparse(substitute(.data))
  if (data_name %in% c("Census2016_ancestories",
                       "Census2016_countries_of_birth", 
                       "Census2016_languages",
                       "Census2016_n_women_by_children_ever_born", 
                       "Census2016_religions",
                       "Census2016_wide_by_SA2_year")) {
    topic <- sub("Census2016_", "", data_name, fixed = TRUE)
  } else {
    # Try to work out whether it is equivalent to a data frame
    noms <- names(.data)
    if (ncol(.data) == 4 &&
        identical(noms[1:2], c("sa2_code", "year")) &&
        noms[4] %in% c("persons", "n_women")) {
      topic <- noms[3]
    } else {
      
      stop("If passing a data.frame to see_question, it must be one of the data sets in this package. That is, it must be one of:",
           "\n\n\t",
           paste0(c("Census2016_ancestories", "Census2016_countries_of_birth", 
                    "Census2016_languages", "Census2016_n_women_by_children_ever_born", 
                    "Census2016_religions", "Census2016_wide_by_SA2_year"),
                  collapse = "\n\t"))
    }
  }
  qn <- 
    switch(topic,
           "ancestories" = "18",
           "ancestory" = "18",
           "countries_of_birth" = "12",
           "country_of_birth" = "12",
           "languages" = "16",
           "language" = "16",
           "n_women_by_children_ever_born" = "32",
           "n_children_ever_born" = "32",
           "religions" = "19",
           "religion" = "19" )
  see_question.numeric(qn)
  if (isTRUE("knitr.in.progress")) {
    return(invisible(.data))
  } else {
    return(.data[])
  }
}

#' @rdname see_question
#' @export
see_question.numeric <- function(qn, ...) {
  
  if (!(qn %in% 1:61)) {
    stop("qn (question number) must be a whole number from 1 to 61.")
  }
  
  image <- 
    switch(as.character(qn),
           "3" = q3,
           "4" = q4,
           "5" = q6,
           "7" = q7,
           "12" = q12,
           "14" = q14_15,
           "15" = q14_15,
           "16" = q16,
           "18" = q18,
           "19" = q19,
           "32" = q32,
           stop("This question is not available through see_question(). ",
                "Must be one of question 3, 4, 5, 7, 12, 14, 15, 16, 18, 19, 32."))
  aspect_ratio <- attr(image, "info")[["dim"]]
  plot.new()
  plot.window(c(0, aspect_ratio[1]), 
              c(0, aspect_ratio[2]))
  rasterImage(image, 0, 0, aspect_ratio[1], aspect_ratio[2])
}
