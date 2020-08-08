#' covidTableSelected
#'
#' @param xnow from covidToday(covidDownload())
#' @param asofhere date to look at
#'
#' @return
#' @export
#'
covidTableSelected <- function(xnow, asofhere) {

  if (missing(asofhere)) {
    asofhere <- max(xnow$date)
  }
  xnow.withdata <- xnow[!is.na(xnow$percap), ]
  myvar <- 100 * xnow.withdata$percap

  these <- data.frame(
    cases.per.100 = rev(round(myvar, 1)),
    cases = rev(xnow.withdata$cases),
    deaths = rev(xnow.withdata$deaths),
    place = rev(xnow.withdata$fullnameST)
  )
  return(these)
}
