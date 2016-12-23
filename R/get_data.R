get_data <- function(file){

library(dplyr)
library(tidyr)

# Use pdftotext extension? to convert pdf to txt
# Still need to cut down and clean, *'s make it look like it will be easy

pdftotext <- "/Users/Ben/Documents/wcgrads/wcgrads/xpdfbin-mac-3.04/bin64/pdftotext"
pdf <- file.path(paste0(file, ".pdf"))
system(paste("\"", pdftotext, "\" \"", pdf, "\""," -raw", sep=""), wait = TRUE)

# Now to get the text into a dataframe
x <- read.delim(paste0(file, ".txt"), quote = "",
                stringsAsFactors = FALSE)
colnames(x) <- "strings"
y <- as.numeric(which(x$strings == "Bachelor of Arts, Summa Cum Laude")) + 1
z <- as.numeric(which(x$strings == "CONFERRING OF HONORARY DEGREES")) - 1
w <- as.data.frame(x[y:z,])

v <- w %>%
  rename(strings = `x[y:z, ]`) %>%
  separate(strings, c("name", "honors"), sep = ",")

v <- gsub(".*(Astrophysics|Biology|Chemistry|Classics|Econom|English|Environmental Studies|Geosciences|French|History|Literature|Mathematics|Music|Neuroscience|Political Science|Psychology|Russian|Science|Spanish|Studies|<|Bachelor of Arts|Degrees Conferred|Phi Beta Kappa|Sigma|--|3).*", NA, v$name)

v <- tbl_df(v)

v <- v %>%
  separate(value, c("first", "second"), sep = " ", extra = "merge") %>%
  separate(second, c("second", "third"), sep = " ", fill = "left")

u <- v %>%
  mutate(firstinit = substr(gsub("[^[:alnum:] ]", "", first), 1, 1)) %>%
  mutate(lastinit = substr(third, 1, 1))

print(u)
}
