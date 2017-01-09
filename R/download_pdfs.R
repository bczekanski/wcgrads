download_pdfs <- function(){
  #' @title download_pdfs
  #' @description download_pdfs() adds pdf Williams College Catalogs from the Williams College website to data-raw
  #' @usage download_pdfs()
  #' @import utils
  #' @export

#Download each file from the Williams website

download.file('http://web.williams.edu/admin/registrar//catalog/depts0001/catalog.pdf', 'data-raw/01.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/depts0102/catalog.pdf', 'data-raw/02.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/depts0203/catalog.pdf', 'data-raw/03.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/depts0304/catalog.pdf', 'data-raw/04.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/depts0405/catalog0405.pdf', 'data-raw/05.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/catalog0506.pdf', 'data-raw/06.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/catalog0607.pdf', 'data-raw/07.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/catalog0708.pdf', 'data-raw/08.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/catalog0809.pdf', 'data-raw/09.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/catalog0910.pdf', 'data-raw/10.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin2010-11.pdf', 'data-raw/11.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin2011_12.pdf', 'data-raw/12.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin12_13.pdf', 'data-raw/13.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin2013_14.pdf', 'data-raw/14.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin2014_15.pdf', 'data-raw/15.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin2015_16.pdf', 'data-raw/16.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin2016_17.pdf', 'data-raw/17.pdf')

}
