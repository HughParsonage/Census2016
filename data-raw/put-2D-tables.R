

Census2016_wide_by_SA2 %T>%
  fwrite("data-raw/csv/Census2016_wide_by_SA2.csv") %>%
  fwrite("data-raw/tsv/Census2016_wide_by_SA2.tsv", sep = "\t")

write2ctsv <- function(qobj) {
 get(qobj) %T>%
    fwrite(paste0("data-raw/csv/", qobj, ".csv")) %T>%
    fwrite(paste0("data-raw/csv/", qobj, ".csv"), sep = "\t")
}

ls(pattern = "Census2016") %>%
  lapply(write2ctsv)
