#' Courtesy Nick Evershed
#' 
#' 

library(RSQLite)
library(DBI)
library(data.table)
library(magrittr)
library(dplyr)
library(dtplyr)
library(hutils)
library(devtools)

con <- dbConnect(RSQLite::SQLite(), "~/../Downloads/scraperwiki.sqlite/scraperwiki.sqlite")

swdata <- dbGetQuery(con, "SELECT * FROM swdata")

# First get the tables that are easy:

Census2016_wide_by_SA2_year <- 
  swdata %>%
  as.data.frame %>%
  select(-ancestries,
         -languages,
         -countries_of_birth,
         -religions) %>% 
  as.data.table %>%
  set_cols_first(c("sa2_name", "sa2_code", "year", "persons")) %>%
  # Nick you brute
  setnames(grep("seperate", names(.), value = TRUE),
           gsub("seperate", "separate", grep("seperate", names(.), value = TRUE))) %>%
  # Original data includes Australia etc
  .[nchar(sa2_code) == 9] %>%
  .[, n_dwellings := coalesce(as.integer(dwelling_owned_outright / dwelling_owned_outright_percent),
                              as.integer(separate_house / separate_house_percent),
                              as.integer(dwelling_owned_mortgage / dwelling_owned_mortgage_percent),
                              as.integer(dwelling_rented / dwelling_rented_percent),
                              as.integer(housing_other_or_not_stated / housing_other_or_not_stated_percent),
                              NA_integer_)] %>%
  .[, isMissing := persons == 0] %>%
  select(-starts_with("percent")) %>%
  
  # Redundant:
  select(-starts_with("total")) %>%
  
  set_cols_first(c("sa2_name", "sa2_code",
                   "year",
                   "persons",
                   
                   # Q3
                   "female",
                   "male",
                   
                   # Q4
                   "median_age",
                   
                   # Q6
                   "married_persons",
                   "married_females", 
                   "married_males",
                   "defacto_persons",
                   "defacto_females",
                   "defacto_males",
                   "notmarried_persons",
                   "notmarried_females", 
                   "notmarried_males",
                   
                   # Q7
                   "indig_persons",
                   "indig_males",
                   "indig_females", 
                   "non_indig_persons",
                   "non_indig_females", 
                   "non_indig_males",
                   "not_stated_indig_persons",
                   "not_stated_indig_males", 
                   "not_stated_indig_females",
                   
                   # Q11
                   "born_in_australia",
                   )) %>%
  .[]

replace_zeros_withNA <- function(DT) {
  DT[, isMissing := persons == 0]
  
  for (jj in names(DT)) {
    if (!(jj %in% c("year", "sa2_name", "sa2_code", "isMissing"))) {
      DT[, (jj) := ifelse(isMissing, NA, DT[[jj]])]
    }
  }
}

replace_zeros_withNA(Census2016_wide_by_SA2_year)

use_data(Census2016_wide_by_SA2_year, overwrite = TRUE)

# Census 2016 SA3
swdata %>%
  as.data.frame %>%
  select(-ancestries,
         -languages,
         -countries_of_birth,
         -religions) %>% 
  as.data.table %>%
  .[nchar(sa2_code) != 9]


sa2_year_by_i <- 
  Census2016_wide_by_SA2_year %>% 
  select(sa2_code, year) %>%
  .[, row := 1:.N]

# Then extract the nested JSONs:
swdata.List <- 
  swdata %>%
  as.data.frame %>%
  filter(nchar(sa2_code) == 9) %>%
  select(ancestries,
         languages,
         countries_of_birth, 
         religions)


fromJSON <- function(x1, ...) {
  tryCatch(rjson::fromJSON(x1, ...),
           error = function(e) {
             tryCatch(jsonlite::fromJSON(x1, ...), 
                      error = function(e) {
                        # Quote in JWs causes issues
                        input <- gsub("Jehovah.s Witnesses", "Jehovahs Witnesses", x1, perl = TRUE)  
                        input <- gsub(": u'", ": '", x = input, fixed = TRUE)
                        input <- gsub(': u"', ": '", x = input, fixed = TRUE)
                        
                        input <- gsub("'", '\"', x = input, fixed = TRUE)
                        rjson::fromJSON(input)
                      })
           })
}

extract_ancestries <- function(el) {
  data.table(persons = el$persons,
             persons_percent = el$persons_percent,
             ancestory = el$label)
}

extract_Var <- function(Var, el) {
  data.table(persons = el$persons,
             persons_percent = el$persons_percent,
             Var = el$label)
}

get_all_ancestories <- function(i) {
  fromJSON(swdata.List$ancestries[i]) %>%
    lapply(extract_ancestries) %>%
    rbindlist %>%
    .[, row := i]
}

get_all_Var <- function(var, i) {
  fromJSON(swdata.List[[var]][i]) %>%
    lapply(extract_Var, Var = var) %>%
    rbindlist %>%
    .[, row := i]
}

Census2016_ancestories <- 
  seq_along(swdata.List$ancestries) %>%
  lapply(get_all_ancestories) %>%
  rbindlist %>%
  sa2_year_by_i[., on = "row"] %>%
  .[, c("persons_percent", "row") := NULL] %>%
  set_cols_last("persons") %>%
  setorder(sa2_code, year, -persons) %>%
  setkey(sa2_code, year) %>%
  .[]

Census2016_languages <-
  seq_along(swdata.List$languages) %>%
  lapply(get_all_Var, var = "languages") %>%
  rbindlist %>%
  setnames("Var", "Language") %>%
  sa2_year_by_i[., on = "row"] %>%
  .[, c("persons_percent", "row") := NULL] %>%
  set_cols_last("persons") %>%
  setorder(sa2_code, year, -persons) %>%
  setkey(sa2_code, year) %>%
  .[]

Census2016_countries_of_birth <-
  seq_along(swdata.List$countries_of_birth) %>%
  lapply(get_all_Var, var = "countries_of_birth") %>%
  rbindlist %>%
  setnames("Var", "countries_of_birth") %>%
  sa2_year_by_i[., on = "row"] %>%
  .[, c("persons_percent", "row") := NULL] %>%
  set_cols_last("persons") %>%
  setorder(sa2_code, year, -persons) %>%
  setkey(sa2_code, year) %>%
  .[]

Census2016_religions <-
  seq_along(swdata.List$religions) %>%
  lapply(get_all_Var, var = "religions") %>%
  rbindlist %>%
  setnames("Var", "religions") %>%
  sa2_year_by_i[., on = "row"] %>%
  .[, c("persons_percent", "row") := NULL] %>%
  
  set_cols_last("persons") %>%
  setorder(sa2_code, year, -persons) %>%
  setkey(sa2_code, year) %>%
  .[]



# replace_zeros_withNA(Census2016_countries_of_birth)
# replace_zeros_withNA(Census2016_languages)
# replace_zeros_withNA(Census2016_religions)
# replace_zeros_withNA(Census2016_ancestories)

use_data(Census2016_countries_of_birth, overwrite = TRUE)
use_data(Census2016_languages, overwrite = TRUE)
use_data(Census2016_religions, overwrite = TRUE)
use_data(Census2016_ancestories, overwrite = TRUE)

tools::resaveRdaFiles("./data/")

Census2016_wide_by_SA2_year %T>%
  fwrite("data-raw/csv/Census2016_wide_by_SA2_year.csv") %>%
  fwrite("data-raw/tsv/Census2016_wide_by_SA2_year.tsv", sep = "\t")

write2ctsv <- function(qobj) {
  get(qobj) %T>%
    fwrite(paste0("data-raw/csv/", qobj, ".csv")) %T>%
    fwrite(paste0("data-raw/tsv/", qobj, ".tsv"), sep = "\t")
}

ls(pattern = "Census2016") %>%
  lapply(write2ctsv)


