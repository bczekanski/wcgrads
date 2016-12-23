library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggExtra)
library(zoo)


# Use pdftotext extension? to convert pdf to txt
# Still need to cut down and clean, *'s make it look like it will be easy

pdftotext <- "/Users/Ben/Downloads/xpdfbin-mac-3.04/bin64/pdftotext"
pdf <- file.path('/Users/Ben/Documents/wcgrads/wcgrads/Data/bulletin2003-04.pdf')
system(paste("\"", pdftotext, "\" \"", pdf, "\""," -raw", sep=""), wait = TRUE)

# Now to get the text into a dataframe
x <- read.delim('/Users/Ben/Documents/wcgrads/wcgrads/Data/bulletin2003-04.txt', quote = "", stringsAsFactors = FALSE)
colnames(x) <- "strings"
y <- as.numeric(which(x$strings == "Bachelor of Arts, Summa Cum Laude")) + 1
z <- as.numeric(which(x$strings == "CONFERRING OF HONORARY DEGREES")) - 1
w <- as.data.frame(x[y:z,])

v <- w %>%
  rename(strings = `x[y:z, ]`) %>%
  separate(strings, c("name", "honors"), sep = ",") %>%
  separate(honors, c("honors", "dept"), sep = " in")
  
  
  separate(name, c("first", "second"), sep = " ", extra = "merge") %>%
  separate(second, c("second", "third"), sep = " ", fill = "left") %>%
  
  #if dept is empty (not NA), then fill it in with the next line

    v$dept <- ifelse(v$dept == "    ", v$dept, lead(v$name))    
    
    fillTheBlanks(v$dept, missing = " ")

fillTheBlanks <- function(x, missing=""){
  rle <- rle(as.character(x))
  empty <- which(rle$value==missing)
  rle$values[empty] <- rle$value[empty-1] 
  inverse.rle(rle)
    }
  

u <- v %>%
  mutate(firstinit = substr(gsub("[^[:alnum:] ]", "", first), 1, 1)) %>%
  mutate(lastinit = substr(third, 1, 1))
  
a <- u %>%
  ggplot(aes(firstinit, lastinit)) +
    stat_bin2d()

b <- ggMarginal(a, u, u$lastinit, u$firstinit, type = "histogram", margins = "both", stat = "count")
 b
  