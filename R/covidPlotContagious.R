#' covidPlotContagious
#'
#' @param x
#' @param countylist
#' @param ndays
#' @param dayscontagious
#' @param add
#' @param show
#' @param ylim
#' @param showlinesforpriordate
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
covidPlotContagious  <- function(x, countylist = NULL, ndays, dayscontagious=14, add = FALSE, show = TRUE, ylim=NULL, showlinesforpriordate=TRUE, ...) {

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

  # I want to plot cumulative new cases added in past dayscontagious days as rolling sum
  # so start with delta in cases
  # To see NEW cases added each day, and running avg of that:
  # dailychange <- c(0, diff(here$cases))
  # dailychange[1] <- dailychange[2] # since I dont know the true value but it is not zero
  # dailychangepercap <- dailychange / here$pop
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

    maintitle <- paste('Still contagious as of ', asofhere, ' assuming contagious for just the ', dayscontagious, ' days after reported positive test',
                       # round(100*here$percapnow[here$date == asofhere], 3),
                       # 'per 100 people (1 in', oneperasof, ') in ', myplace,
                       sep = '')
    ylab = paste('Still contagious per 100 people, assuming only stay contagious', dayscontagious, 'days')

    if (is.null(ylim)) {
      myylim <- 100 * c(min(here$percapnow, na.rm = T), max(here$percapnow, na.rm = T))
      # or maybe make lower bound zero??
    } else {
      myylim <- ylim
    }

    if (showlinesforpriordate) {
      # date when prevalence first was almost same as currently
      # ******* THIS IS NOT THE RIGHT NUMBER IF YOU CHANGE LAST N DAYS TO SMALLER VALUE, AS CALCULATED NOW
      whenwasliketoday <- min(here$date[100*here$percapnow > tail(100*here$percapnow, 1)])
      dateinfo <- paste('Now is back down to same prevalence as it was around', whenwasliketoday)
    } else {
      dateinfo <- ''
    }
    par(lty = 3)
    if (add) {
      # adding lines to an existing plot
      lines(here$date, 100*here$percapnow, ...)
    } else {
      # new, first plot
      plot(here$date, 100 * here$percapnow, main = maintitle,
           sub = dateinfo,
           ylim = myylim,  #asp = 0.5,
           xlab = 'Date', ylab = ylab, ...)
    }
    if (showlinesforpriordate) {
      abline(h = tail(100*here$percapnow, 1) ) # level of most recent day seen as horizontal line
      abline(v = whenwasliketoday) # vertical line on date when prevalence first was almost same as currently
    }
  }
  return(here[ , c('date', 'percapnow', 'fullname')])
}

