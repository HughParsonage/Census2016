#' @title Two dimensional tables
#' @description Tables with two keys (the SA2 and a label for which the count applies)
#' @format A \code{data.table} of 4 columns. The keys are \code{sa2_code}
#' 
#' @section \code{Census2016_ancestories}:
#' N.B. This table is a multi-response table, and therefore the total response count will not equal the total persons count.
#' Sinhalese is equivalent to Sri Lankan.
#' \describe{
#' \item{\code{sa2_code}}{The SA2 code. See details.}
#' \item{\code{year}}{The census year.}
#' \item{\code{ancestory}}{Selected ancestries are based on the 30 most common ancestry responses reported in the 2011 Census.} 
#' \item{\code{persons}}{Count of persons.}
#' }
#' 
#' @section \code{Census2016_religions}:
#' \describe{
#' \item{\code{sa2_code}}{The SA2 code. See details.}
#' \item{\code{year}}{The census year.}
#' \item{\code{religion}}{Religion. "What is the person's religion?".}
#' \item{\code{persons}}{Number of adherents.}
#' }
#' 
#' @section \code{Census2016_languages}:
#' \describe{
#' \item{\code{sa2_code}}{The SA2 code. See details.}
#' \item{\code{year}}{The census year.}
#' \item{\code{language}}{Does the person speak a language other than English at home?}
#' \item{\code{persons}}{Number of persons who speak the specified language. 
#' Note that the complement of "English" is not the number of people who do not speak English.}
#' }
#' 
#' @section \code{Census2016_n_women_by_children_ever_born}:
#' \describe{
#' \item{\code{sa2_code}}{The SA2 code. See details.}
#' \item{\code{year}}{The census year.}
#' \item{\code{n_children_ever_born}}{Number of children ever born.} 
#' \item{\code{n_women}}{Count of females over 15 by the number of children they have ever given birth to.}
#' }
#' 
#'
#' @name TwoDimTbl
#' 
#' @details From the Community Series time series data packs. 
#' N.B. \code{countries_of_birth} only includes the top 35 responses countries of birth across Australia.
#' \code{sa2_code} is not necessarily the same as \code{SA2_MAINYY} for 2006, 2011, and 2016.
#' Instead, the code reflects the 2016 boundaries and the values given allow comparisons to be made over time on these boundaries.
NULL

#' @rdname TwoDimTbl
'Census2016_ancestories'

#' @rdname TwoDimTbl
'Census2016_countries_of_birth'

#' @rdname TwoDimTbl
'Census2016_languages'

#' @rdname TwoDimTbl
'Census2016_religions'

#' @rdname TwoDimTbl
'Census2016_n_women_by_children_ever_born'
