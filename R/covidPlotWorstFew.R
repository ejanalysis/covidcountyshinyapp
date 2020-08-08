#' covidPlotWorstFew
#'
#' @param xnow from covidToday(covidDownload())
#' @param n how many places, optional
#' @param asofhere date to use, optional
#' @param ... passed to barplot
#'
#' @export
#'
covidPlotWorstFew <- function(xnow, n = 7, asofhere, ...) {

  if (missing(xnow)) {
    xnow <- covidToday(covidDownload())
  }
  if (missing(asofhere)) {
    asofhere <- max(xnow$date)
  }
  xnow.withdata <- xnow[!is.na(xnow$percap), ]
  yl <- 'cases per 100 people'
  myvar <- 100 * xnow.withdata$percap

  # data.frame(
  #   cases.per.100 = rev(round(tail(myvar, n),1)),
  #   place = rev(tail(xnow.withdata$fullnameST, n))
  # )

  barplot(height = tail(myvar, n), las = 2, mai = c(5,7,4,1),
          ylab = yl, main = paste('COVID cases data', asofhere),
          names.arg = tail(xnow.withdata$fullnameST, n), ...)
}
