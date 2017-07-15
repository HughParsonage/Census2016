
# Purpose is to obtain:
# sa2_code  year  children

library(glue)
library(readxl)
# http://www.censusdata.abs.gov.au/CensusOutput/copsub2016.NSF/All%20docs%20by%20catNo/2016~Community%20Profile~{sa2}/$File/TSP_{sa2}.zip?OpenElement"
library(httr)
library(hutils)


GET_children_by_sa2_year <- function(sa2) {
  orig_wd <- getwd()
  the_wd <- tempdir()
  on.exit(setwd(orig_wd))
  setwd(the_wd)
  
  while (length(list.files()) > 0) {
    new_dir <- file.path(the_wd, sample(c(LETTERS, letters), size = 1), sa2)
    provide.dir(new_dir)
    setwd(new_dir)
  }
  
  url <- glue("http://www.censusdata.abs.gov.au/CensusOutput/copsub2016.NSF/All%20docs%20by%20catNo/2016~Community%20Profile~{sa2}/$File/TSP_{sa2}.zip?OpenElement")
  
  tempf.zip <- tempfile(fileext = ".zip", tmpdir = the_wd)
  if (http_error(as.character(url))) {
    return(data.table:::null.data.table())
  } else {
    cat(tempf.zip)
    GET(url, write_disk(tempf.zip, overwrite = TRUE))
    unzip(tempf.zip)
    
    file.xls <- list.files(pattern = "^TSP.*XLS", ignore.case = TRUE)
    stopifnot(length(file.xls) == 1)
    
    children_2006 <- read_excel(file.xls,
                                col_names = FALSE,
                                sheet = "T 07a",
                                range = "B30:I30")
    children_2011 <- read_excel(file.xls,
                                col_names = FALSE,
                                sheet = "T 07a",
                                range = "B51:I51")
    children_2016 <- read_excel(file.xls,
                                col_names = FALSE,
                                sheet = "T07b",
                                range = "B30:I30")
    
    setDT(children_2006)
    setDT(children_2011)
    setDT(children_2016)
    setnames(children_2006, c(0:6, "N/A"))
    setnames(children_2011, c(0:6, "N/A"))
    setnames(children_2016, c(0:6, "N/A"))
    rbindlist(list("2006" = children_2006,
                   "2011" = children_2011,
                   "2016" = children_2016),
              idcol = "year") %>%
      .[, sa2_code := sa2]
  }
}

children_wide_by_sa2_year <- 
  lapply(unique(Census2016_wide_by_SA2_year$sa2_code),
       GET_children_by_sa2_year) %>%
  rbindlist

force_int <- function(x) suppressWarnings(as.integer(x))

Census2016_n_women_by_children_ever_born <-
  children_wide_by_sa2_year %>% 
  melt.data.table(id.vars = c("sa2_code", "year"),
                  variable.name = "n_children_ever_born",
                  value.name = "n_women",
                  variable.factor = FALSE) %>%
  .[, n_children_ever_born := force_int(n_children_ever_born)] %>%
  .[]

devtools::use_data(Census2016_n_women_by_children_ever_born)
