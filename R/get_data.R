get_data <- function(file){

#' @title get_data
#' @description get_data() converts the pdf Williams College Catalog to a partially clean data frame
#' @param file The file, a Williams College catalog, to be converted from pdf and cleaned
#' @return The output of get_data() is a data frame
#' @usage get_data(file), for a 1999-2000 catalog, type: get_data("00")
#' @export
#' @import tidyr dplyr

#Use an executable script to convert .pdf files to .txt files

pdftotext <- system.file("bin/pdftotext", package = "wcgrads", mustWork = TRUE)
pdf_file <- system.file(paste0("extdata/", file, ".pdf"), package = "wcgrads", mustWork = TRUE)
pdf <- file.path(pdf_file)

system(paste("\"", pdftotext, "\" \"", pdf, "\""," -raw", sep=""), wait = TRUE)

#Read .txt files as a data frame and find the list of graduating seniors

text_file <- system.file(paste0("extdata/", file, ".txt"), package = "wcgrads", mustWork = TRUE)
x <- read.delim(text_file, quote = "",
                stringsAsFactors = FALSE)
colnames(x) <- "strings"
y <- as.numeric(which(x$strings == "Bachelor of Arts, Summa Cum Laude")) + 1
z <- as.numeric(which(x$strings == "CONFERRING OF HONORARY DEGREES")) - 1
w <- as.data.frame(x[y:z,])

#Identify names and honors designations, isolate names, break names into first and last

v <- w %>%
  rename(strings = `x[y:z, ]`) %>%
  separate(strings, c("name", "honors"), sep = ",")
v <- gsub(".*(Art|Astrophysics|Biology|Chemistry|Classics|Econom|
          English|Environmental Studies|Geosciences|French|History|
          Literature|Mathematics|Music|Neuroscience|Philosophy|Political Science|
          Psychology|Russian|Science|Sociology|Spanish|Studies|<|Bachelor of Arts|
          Degrees Conferred|Phi Beta Kappa|Sigma|--|3|
          and honors in|honors in English).*", NA, v$name)
v <- tbl_df(v)
v <- v %>%
  separate(value, c("first", "second"), sep = " ", extra = "merge") %>%
  separate(second, c("second", "third"), sep = " ", fill = "left")
u <- v %>%
  mutate(firstinit = substr(gsub("[^[:alnum:] ]", "", first), 1, 1)) %>%
  mutate(lastinit = substr(third, 1, 1))

return(u)

}
