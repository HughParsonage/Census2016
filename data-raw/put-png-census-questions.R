library(devtools)
library(magrittr)
library(graphics)
library(magick)
library(png)

# http://www.abs.gov.au/ausstats/abs@.nsf/Lookup/by%20Subject/2900.0~2016~Main%20Features~Understanding%20the%20data~6

pngs2convert <- 
  dir(path = "data-raw/png-census-questions/",
      pattern = "[^1]\\.png$",
      full.names = TRUE)

file.copy(pngs2convert, sub(".png", "1.gif", pngs2convert, fixed = TRUE))


gif2PNG <- function(filename) {
  if (!file.exists(sub("\\.gif$", ".PNG", filename))) {
    image_read(filename) %>%
      image_write(sub("\\.gif$", ".PNG", filename), format = "png")
  }
}

dir("./data-raw/png-census-questions/", 
    pattern = "1.gif$", 
    full.names = TRUE) %>%
  lapply(png2PNG)

q3  <- readPNG("./data-raw/png-census-questions/Q3-Sex.PNG", info = TRUE)
q4  <- readPNG("./data-raw/png-census-questions/Q4-DOB-Age.PNG", info = TRUE)
q5  <- readPNG("./data-raw/png-census-questions/Q5-Relation-to-head-of-household.PNG", info = TRUE)
q6  <- readPNG("./data-raw/png-census-questions/Q6-Marital-status.PNG", info = TRUE)
q7  <- readPNG("./data-raw/png-census-questions/Q7-Indigenous.PNG", info = TRUE)
q12 <- readPNG("./data-raw/png-census-questions/Q12-country-of-birth.PNG", info = TRUE)
q16 <- readPNG("./data-raw/png-census-questions/Q16-languages.PNG", info = TRUE)
# q14_15 <- readPNG("./data-raw/png-census-questions/Q14_15-ancestories.PNG", info = TRUE)
q18 <- readPNG("./data-raw/png-census-questions/Q18-ancestory.PNG", info = TRUE)
q19 <- readPNG("./data-raw/png-census-questions/Q19-religion.PNG", info = TRUE)
q32 <- readPNG("./data-raw/png-census-questions/Q32-n-babies.PNG", info = TRUE)

for (png_file in dir(path = "data-raw/png-census-questions/", pattern = "\\.(png|PNG)$", full.names = TRUE)) {
  if (!exists(paste0("q", q_no))) {
    q.png <- dir(path = "data-raw/png-census-questions/",
                 pattern = paste0(q_no, ".*\\.PNG$"),
                 full.names = TRUE)
    if (length(q.png)) {
      q_no <- sub("^Q([0-9]+)-.*$", "\\1", basename(png_file), perl = TRUE)
      assign(paste0("q", q_no), readPNG(dir(path = "data-raw/png-census-questions/",
                                            pattern = paste0(q_no, ".*\\.PNG$"),
                                            full.names = TRUE), 
                                        info = TRUE))
    }
  }
}

use_data(q3, q4, q5, q6, q7, q12, q16, q18, q19, q32,
         internal = TRUE, overwrite = TRUE)
# For space saving
tools::resaveRdaFiles("R")
