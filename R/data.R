#' All Years of Williams College Graduates
#'
#' A dataset containing information from Williams College Course Catalogs from 2000-2016. Each row is a single graduate.
#'
#' @format A data frame with 8865 rows and 12 variables:
#' \describe{
#'   \item{first}{Graduate's first name}
#'   \item{middle}{Graduate's middle name(s), if any, otherwise NA}
#'   \item{last}{Graduate's last or family name(s)}
#'   \item{suffix}{Graduate's suffix such as Jr., III or V, if any, otherwise NA}
#'   \item{grad.year}{Year of graduation}
#'   \item{gender}{Graduate's gender, as predicted by the "gender" R package, using their first and middle names and method "ssa."
#'   If there is no prediction, NA}
#'   \item{pbk}{TRUE if graduate was a member of Phi Beta Kappa, otherwise FALSE}
#'   \item{latin.honors}{Graduate's latin honors, if any, otherwise NA}
#'   \item{first.honors.dept}{Graduate's first department of departmental honors, if any, otherwise NA}
#'   \item{first.honors.level}{Graduate's first level of departmental honors, if any, otherwise NA}
#'   \item{second.honors.dept}{Graduate's second department of departmental honors, if any, otherwise NA}
#'   \item{second.honors.level}{Graduate's second level of departmental honors, if any, otherwise NA}
#' }
#' @source http://web.williams.edu/admin/registrar//catalog/archive.html
"allyrs"
