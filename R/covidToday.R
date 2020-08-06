#' covidToday
#'
#' @param x
#' @param mydate
#' @param writecsv
#' @param localpath
#'
#' @return
#' @export
#'
#' @examples
covidToday <- function(x, mydate=NULL, writecsv=FALSE, localpath=getwd()) {

  if (missing(x)) {
    x <- covidDownload(writecsv = writecsv, localpath = localpath)
  }
 if (missing(mydate)) {
   asof <- max(x$date)
 }  else {
   asof <- mydate
   if (!(asof %in% x$date)) {
     warning('specified date not found in the dataset (in that format at least) so using most recent date in dataset')
     asof <- max(x$date)
     }
 }

xnow <- x[x$date == asof, ]
xnow <- xnow[order(xnow$percap), ]

# tail(xnow, 40)
# tail(xnow[!is.na(xnow$oneper), ],40)

return(xnow)
}
