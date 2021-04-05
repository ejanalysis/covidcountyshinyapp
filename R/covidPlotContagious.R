#' covidPlotContagious
#'
#' @param x from covidDownload
#' @param countylist vector of county names like "Montgomery County, Maryland" as found in unique fullname entries in covidDownload()
#' @param ndays show only the last ndays days of data
#' @param dayscontagious how many recent days of new cases to add together, as a way to approximate how many people are currently still contagious
#' @param perx per how many, such as per 100 or per 100000
#' @param add adding to existing plot or no
#' @param show whether to draw plot or just return info
#' @param ylim as in plot()
#' @param showlinesforpriordate draw short line fit slopes to show midpoint of linefit to each of several recent sets of days
#' @param ... passed to plot()
#'
#' @export
#'
covidPlotContagious  <- function(x, countylist = NULL, ndays, dayscontagious=14, perx=100000, add = FALSE, show = TRUE, ylim=NULL, showlinesforpriordate=TRUE, ...) {

  if (missing(x)) {
    x <- covidDownload()
  }
  if (is.null(countylist)) {countylist <- unique(x$fullname)}
  if (length(countylist) == 0) return() # nothing to plot
  # myplace <- countylist[1]
  # here <- x[x$fullname == myplace, ]

  if (length(countylist) == 1) {
    myplace <- countylist
  } else {
    myplace <- 'Selected Counties'
  }
  here <- sum_counties(x, countylist = countylist, datecolname = 'date', countycolname = 'fullname')

  if (missing(ndays)) {ndays <- NROW(here)}
  if (ndays > NROW(here)) {ndays <- NROW(here)}
  here <- here[order(here$date, decreasing = FALSE), ]
  here <- here[(NROW(here) - ndays + 1):NROW(here), ] # show only the last n days of data
  asofhere <- max(here$date)

  # ** could replace code below with the function stillcontagious_percap_bycounty()

  dailychange <- here$new # since it is now provided by covidDownload()
  dailychangepercap <- dailychange / here$pop

  here$percapnow <- 0
  for (daynum in 1:length(dailychangepercap)) {
    here$percapnow[daynum] <- sum(
      dailychangepercap[max(1, (daynum - dayscontagious + 1)):daynum]
    )
  }
  oneperasof <- round(1 / here$percapnow, 0)[here$date == asofhere]

  if (show) {

    maintitle <- paste('Contagious ', asofhere, ' if contagious ', dayscontagious, ' days after positive',
                       # round(100*here$percapnow[here$date == asofhere], 3),
                       # 'per 100 people (1 in', oneperasof, ') in ', myplace,
                       sep = '')
    ylab = paste('Contagious/', format(perx,scientific=FALSE, big.mark=','), 'people, if lasts', dayscontagious, 'days')

    if (is.null(ylim)) {
      myylim <- perx * c(min(here$percapnow, na.rm = T), max(here$percapnow, na.rm = T))
      # or maybe make lower bound zero??
    } else {
      myylim <- ylim
    }

    if (showlinesforpriordate) {
      # date when prevalence first was almost same as currently
      # ******* THIS IS NOT THE RIGHT NUMBER IF YOU CHANGE LAST N DAYS TO SMALLER VALUE, AS CALCULATED NOW
      whenwasliketoday <- min(here$date[perx*here$percapnow > tail(perx*here$percapnow, 1)])
      if (is.na(whenwasliketoday)) {whenwasliketoday <- 'at no prior time.'}
      dateinfo <- paste('Now rate is what it was ', whenwasliketoday)
    } else {
      dateinfo <- ''
    }
    par(lty = 3)
    if (add) {
      # adding lines to an existing plot
      lines(here$date, perx*here$percapnow, ...)
    } else {
      # new, first plot
      plot(here$date, perx * here$percapnow, main = maintitle,
           sub = dateinfo,
           ylim = myylim,  #asp = 0.5,
           xlab = 'Date', ylab = ylab, ...)
    }
    if (showlinesforpriordate) {
      abline(h = tail(perx*here$percapnow, 1) ) # level of most recent day seen as horizontal line
      abline(v = whenwasliketoday) # vertical line on date when prevalence first was almost same as currently
    }
  }
  return(here[ , c('date', 'percapnow', 'fullname')])
}

