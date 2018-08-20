
library(htmltools) ## HTML()
library(htmlwidgets)
library(dplyr)
library(dtplyr)
library(data.table)
library(magrittr)
library(scales)
library(grattanCharts)  # devtools::install_github('hughparsonage/grattanCharts')
library(grattan)
library(rvest)
library(xml2)
library(httr)
library(leaflet)
library(ASGS)
library(hutils)

provide.dir <- function(path, ...) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}

get_quickstats_tbl <- function(sa2cd) {
  quick_stats <- read_html(x = paste0("http://www.censusdata.abs.gov.au/census_services/getproduct/census/2016/quickstat/", sa2cd, "?opendocument"))
  
  tbl_seq <- quick_stats %>% html_nodes(css = "table") %>% seq_along
  
  for (tbl.no in tbl_seq) {
    tryCatch({
      provide.dir(paste0("./data-raw/quickstats/", tbl.no))
      
      quick_stats %>%
        html_nodes(css = "table") %>%
        extract2(tbl.no) %>%
        html_table %>%
        as.data.table %>%
        .[, SA2_CD := sa2cd] %>%
        .[, Tbl_No := tbl.no] %>%
        .[] %>%
        fwrite(paste0("./data-raw/quickstats/", tbl.no, "/", sa2cd, ".csv"))
    }, 
    error = function(e) {
      cat(sa2cd, tbl.no)
      cat("\n")
      data.table:::null.data.table()
    })
  }
}

if (FALSE) {
  for (sa2cd in (as.character(SA2_2016@data$SA2_MAIN16))) {
    get_quickstats_tbl(sa2cd)
  }
}
  
tryDcast <- function(DT) {
  drop_cols(DT, c("Other Territories", "Northern Territory", "Tasmania", 
                  "Australian Capital Territory", "South Australia", "Western Australia", 
                  "Victoria", "Queensland", "New South Wales", "Australia"))
  
  if ("%" %in% names(DT)) {
    while ("%" %in% names(DT)) {
      DT[, "%" := NULL]
    }
    which_pc <- which(names(DT) == "%")
    title <- names(DT)[1]
    setnames(DT, 1:2, c("X1", "X2"))
    DT 
  }
  
  tryCatch({
    dcast.data.table(DT,
                     SA2_CD + Tbl_No ~ X1,
                     value.var = "X2")
  }, error = function(e) data.table:::null.data.table())
}



All_SA2s <- data.table(SA2_CD = as.integer(as.character(SA2_2016@data$SA2_MAIN16)),
                       key = "SA2_CD")

clean_column <- function(v) {
  if (is.character(v)) {
    if (all(grepl("%$", v, perl = TRUE))) {
      v <- as.numeric(sub("%$", "", v, perl = TRUE)) / 100
    } else {
      if (all(grepl("^[$]", v, perl = TRUE))) {
        v <- gsub("^[$]", "", v, perl = TRUE)
      }
      
      if (any(grepl("[0-9],[0-9]", v, perl = TRUE))) {
        v <- as.numeric(gsub(",", "", v, perl = TRUE))
      }
    }
  }
  
  v
}

clean_Table <- function(DT) {
  noms <- names(DT)
  setnames(DT, noms, gsub(" ", "_", noms, fixed = TRUE))
  if ("Tbl_no" %in% noms){
    DT[, Tbl_no := NULL]
  }
  DT
}

Table1 <- 
  dir("./data-raw/quickstats/1", full.names = TRUE) %>%
  lapply(fread) %>%
  lapply(tryDcast) %>%
  rbindlist(use.names = TRUE, fill = TRUE) %>%
  .[, lapply(.SD, clean_column)] %>%
  .[All_SA2s, on = "SA2_CD"]

Table2 <- 
  dir("./data-raw/quickstats/2", full.names = TRUE) %>%
  lapply(fread, na.strings = "") %>%
  lapply(tryDcast) %>%
  rbindlist(use.names = TRUE, fill = TRUE) %>%
  .[, lapply(.SD, clean_column)] %>%
  .[All_SA2s, on = "SA2_CD"]

Table3 <- 
  dir("./data-raw/quickstats/3", full.names = TRUE) %>%
  lapply(fread, na.strings = "") %>%
  lapply(tryDcast) %>%
  rbindlist(use.names = TRUE, fill = TRUE) %>%
  .[, lapply(.SD, clean_column)] %>%
  .[All_SA2s, on = "SA2_CD"]

Table4 <- 
  dir("./data-raw/quickstats/4", full.names = TRUE) %>%
  lapply(fread, na.strings = "") %>%
  lapply(tryDcast) %>%
  rbindlist(use.names = TRUE, fill = TRUE) %>%
  .[, lapply(.SD, clean_column)] %>%
  .[All_SA2s, on = "SA2_CD"]

merge_on_SA2 <- function(X, Y) X[Y]

Merge <- function(...) {
  dots <- list(...)
  Reduce(merge_on_SA2, ...)
}

Retrieve_Table <- function(i) {
  tryCatch({
  dir(paste0("./data-raw/quickstats/", i), full.names = TRUE) %>%
    lapply(fread, na.strings = "") %>%
    lapply(tryDcast) %>%
    rbindlist(use.names = TRUE, fill = TRUE) %>%
    .[, lapply(.SD, clean_column)] %>%
    clean_Table %>%
    setkey(SA2_CD) %>%
    .[All_SA2s]
  }, 
  error = function(e) data.table:::null.data.table())
}

All_Tables <- list(41)

All_Tables <- lapply(1:41, Retrieve_Table)

saveRDS(All_Tables, "./data-raw/quickstats/All_Tables.rds", compress = FALSE)

for (j in 1:41) {
  assign(paste0("Table", j), All_Tables[[j]])
}

Age_ranges_by_SA2 <- 
  Table5 %>%
  select(-Tbl_No, -Median_age) %>%
  melt.data.table(id.vars = "SA2_CD", variable.name = "Age_range", value.name = "N") %>%
  .[, Age_range := gsub("_years", "", Age_range)] %>%
  .[, Age_range := factor(Age_range, levels = unique(.$Age_range), ordered = TRUE)]

Median_age_by_SA2 <- 
  Table5[, .(SA2_CD, Median_age)]

Marital_status_by_SA2 <-
  Table6 %>%
  select(-Tbl_No) %>%
  melt.data.table(id.vars = "SA2_CD",
                  variable.name = "Marital_status", 
                  value.name = "N") %>%
  .[, Marital_status := gsub("_", " ", Marital_status, fixed = TRUE)] %>%
  .[]

# 7
Marriages_by_SA2 <-
  Table7 %>%
  select(-Tbl_No) %>%
  melt.data.table(id.vars = "SA2_CD",
                  variable.name = "Marriage_type",
                  value.name = "N") %>%
  .[, Marriage_type := gsub("_", " ", Marriage_type, fixed = TRUE)] %>%
  .[]

Table8
