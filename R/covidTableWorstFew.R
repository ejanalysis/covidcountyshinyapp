#' covidTableWorstFew
#'
#' @param xnow  from covidToday(covidDownload())
#' @param n how many places, optional
#' @param asofhere what date, optional
#'
#' @export
#'
covidTableWorstFew <- function(xnow, n = 7, asofhere) {

  if (missing(xnow)) {
    xnow <- covidToday(covidDownload())
  }
  if (missing(asofhere)) {
    asofhere <- max(xnow$date)
  }
  xnow.withdata <- xnow[!is.na(xnow$percap), ]
  # yl <- 'cases per 100 people'
  myvar <- 100 * xnow.withdata$percap

  data.frame(
    cases.per.100 = rev(round(tail(myvar, n),1)),
    place = rev(tail(xnow.withdata$fullnameST, n)) # ,
    # date = asofhere
  )

  # barplot(height = tail(myvar, n),
  #         ylab = yl, main = paste('COVID cases data', asofhere),
  #         names.arg = tail(xnow.withdata$fullnameST, n), horiz = FALSE, cex.names = cex.names, ...)
}
