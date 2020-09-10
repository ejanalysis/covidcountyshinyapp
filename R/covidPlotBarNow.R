#' covidPlotBarNow
#'
#' @param x from covidDownload
#' @param asofhere date to look at, optional
#' @param horiz controls whether barplot is vertical or horizontal, optional
#' @param n how many places, the worst n, optional
#' @param noworever worst cumulatively for all past days (ever) or worst now (cum cases for the past dayscontagious only)
#' @param dayscontagious how many recent days of new cases to add together, as a way to approximate how many people are currently still contagious
#' @param inapp optional, related to formatting in browser vs RStudio interactive window
#' @param ... passed to barplot
#'
#' @export
#'
covidPlotBarNow <- function(x, asofhere, horiz = FALSE, n=15, noworever='ever', dayscontagious=14, inapp=TRUE, ...) {

  if (missing(x)) {
    x <- covidDownload()
  }
  if (missing(asofhere)) {
    asofhere <- max(x$date)
  }

  if (!missing(horiz) & horiz == TRUE) {
    xl <- 'cases per 100k people'
    yl <- ''
  } else {
    xl <- ''
    yl <- 'cases per 100k people'
  }

  if (noworever == 'ever') {
    xnow <- covidToday(x, mydate = asofhere)
    xnow.withdata <- xnow[!is.na(xnow$percap), ]
    myvar <- 100000 * xnow.withdata$percap
    worstfewplaces <- tail(xnow.withdata$fullnameST[order(myvar, decreasing = FALSE)], n)
    worstfewvalues <- tail(myvar[order(myvar, decreasing = FALSE)], n)
  } else {
    if (noworever == 'now') {
      countylist <- unique(x$fullname)
      myvar <- 100000 * latestcontagiouspercap(x, countylist = countylist, dayscontagious = dayscontagious)
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
