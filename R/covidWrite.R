#' covidWrite
#'
#' @param x
#' @param localpath
#'
#' @return
#' @export
#'
#' @examples
covidWrite <- function(x, localpath=getwd()) {
  asof <-  max(x$date)
  write.csv(x, file = file.path(localpath, paste('covid by county by day as of ', asof, '.csv', sep = '')), row.names = FALSE)
}
