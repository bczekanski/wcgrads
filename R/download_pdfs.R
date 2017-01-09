download_pdfs <- function(){
  #' @title download_pdfs
  #' @description download_pdfs() adds pdf Williams College Catalogs from the Williams College website to data-raw
  #' @usage download_pdfs()
  #' @export

#Download each file from the Williams website

download.file('http://web.williams.edu/admin/registrar//catalog/depts0001/catalog.pdf', 'inst/extdata/01.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/depts0102/catalog.pdf', 'inst/extdata/02.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/depts0203/catalog.pdf', 'inst/extdata/03.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/depts0304/catalog.pdf', 'inst/extdata/04.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/depts0405/catalog0405.pdf', 'inst/extdata/05.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/catalog0506.pdf', 'inst/extdata/06.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/catalog0607.pdf', 'inst/extdata/07.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/catalog0708.pdf', 'inst/extdata/08.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/catalog0809.pdf', 'inst/extdata/09.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/catalog0910.pdf', 'inst/extdata.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin2010-11.pdf', 'inst/extdata/11.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin2011_12.pdf', 'inst/extdata/12.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin12_13.pdf', 'inst/extdata/13.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin2013_14.pdf', 'inst/extdata/14.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin2014_15.pdf', 'inst/extdata/15.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin2015_16.pdf', 'inst/extdata/16.pdf')
download.file('http://web.williams.edu/admin/registrar//catalog/bulletin2016_17.pdf', 'inst/extdata/17.pdf')

}
