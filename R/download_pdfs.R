download_pdfs <- function(){
  #' @title download_pdfs
  #' @description download_pdfs() adds pdf Williams College Catalogs from the Williams College website to data-raw
  #' @usage download_pdfs()
  #' @export

#Download each file from the Williams website

download.file('http://web.williams.edu/admin/registrar//catalog/depts0001/catalog.pdf', system.file('extdata/01.pdf', package = "wcgrads"))
download.file('http://web.williams.edu/admin/registrar//catalog/depts0102/catalog.pdf', system.file('extdata/02.pdf', package = "wcgrads"))
download.file('http://web.williams.edu/admin/registrar//catalog/depts0203/catalog.pdf', system.file('extdata/03.pdf', package = "wcgrads"))
download.file('http://web.williams.edu/admin/registrar//catalog/depts0304/catalog.pdf', system.file('extdata/04.pdf', package = "wcgrads"))
download.file('http://web.williams.edu/admin/registrar//catalog/depts0405/catalog0405.pdf', system.file('extdata/05.pdf', package = "wcgrads"))
download.file('http://web.williams.edu/admin/registrar//catalog/catalog0506.pdf', system.file('extdata/06.pdf', package = "wcgrads"))
download.file('http://web.williams.edu/admin/registrar//catalog/catalog0607.pdf', system.file('extdata/07.pdf', package = "wcgrads"))
download.file('http://web.williams.edu/admin/registrar//catalog/catalog0708.pdf', system.file('extdata/08.pdf', package = "wcgrads"))
download.file('http://web.williams.edu/admin/registrar//catalog/catalog0809.pdf', system.file('extdata/09.pdf', package = "wcgrads"))
download.file('http://web.williams.edu/admin/registrar//catalog/catalog0910.pdf', system.file('extdata/10.pdf', package = "wcgrads"))
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin2010-11.pdf', system.file('extdata/11.pdf', package = "wcgrads"))
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin2011_12.pdf', system.file('extdata/12.pdf', package = "wcgrads"))
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin12_13.pdf', system.file('extdata/13.pdf', package = "wcgrads"))
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin2013_14.pdf', system.file('extdata/14.pdf', package = "wcgrads"))
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin2014_15.pdf', system.file('extdata/15.pdf', package = "wcgrads"))
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin2015_16.pdf', system.file('extdata/16.pdf', package = "wcgrads"))
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin2016_17.pdf', system.file('extdata/17.pdf', package = "wcgrads"))

}
