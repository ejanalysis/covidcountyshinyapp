#' covidPlotBarNow
#'
#' @param x
#' @param asofhere
#' @param horiz
#' @param n
#' @param noworever
#' @param dayscontagious
#' @param inapp
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
covidPlotBarNow <- function(x, asofhere, horiz = FALSE, n=10, noworever='ever', dayscontagious=14, inapp=TRUE, ...) {

  if (missing(x)) {
    x <- covidDownload()
  }
  if (missing(asofhere)) {
    asofhere <- max(x$date)
  }

  if (!missing(horiz) & horiz == TRUE) {
    xl <- 'cases per 100 people'
    yl <- ''
  } else {
    xl <- ''
    yl <- 'cases per 100 people'
  }

  if (noworever == 'ever') {
    xnow <- covidToday(x, mydate = asofhere)
    xnow.withdata <- xnow[!is.na(xnow$percap), ]
    myvar <- 100 * xnow.withdata$percap
    worstfewplaces <- tail(xnow.withdata$fullnameST[order(myvar, decreasing = FALSE)], n)
    worstfewvalues <- tail(myvar[order(myvar, decreasing = FALSE)], n)
  } else {
    if (noworever == 'now') {
      countylist <- unique(x$fullname)
      myvar <- 100 * latestcontagiouspercap(x, countylist = countylist, dayscontagious = dayscontagious)
      worstfewplaces <- tail(countylist[order(myvar, decreasing = FALSE)], n)
      worstfewplaces <- x$fullnameST[match(worstfewplaces, x$fullname)]
      worstfewvalues <- tail(myvar[order(myvar, decreasing = FALSE)], n)
    } else {stop('noworever must be now or ever')}
  }
  #n <- 10  # top few only will fit well
  if (inapp) {par('mai' = c(1.02, 2, 0.82, 0.42)) } #works ok for browser but not interactive window...
  barplot(height = worstfewvalues,
          names.arg = worstfewplaces,
          las = 2, xlab = xl, ylab = yl,
          sub = paste('Top few selected area(s), through', asofhere),
          horiz = horiz, ...)
  if (inapp) {par('mai' = c(1.02, 0.82, 0.82, 0.42)) } #works ok for browser but not interactive window...
}
