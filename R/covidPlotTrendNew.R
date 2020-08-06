#' covidPlotTrendNew
#'
#' @param x
#' @param countylist
#' @param averagingtime
#' @param smoothspan
#' @param ndays
#' @param lastn
#' @param fitlwd
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
covidPlotTrendNew <- function(x, countylist = NULL,
                              averagingtime=3, smoothspan, ndays, lastn=14, fitlwd=3, ...) {

  if (missing(x)) {
    x <- covidDownload()
  }
  if (missing(smoothspan)) {smoothspan <- 0.15}
  if (is.null(countylist)) {countylist <- unique(x$fullname)}
  if (length(countylist) == 0) return() # nothing to plot
  if (length(countylist) == 1) {
    myplace <- countylist
  } else {
    myplace <- 'Selected Counties'
  }
  here <- sum_counties(x, countylist = countylist, datecolname = 'date', countycolname = 'fullname')

  if (missing(ndays)) {ndays <- NROW(here)}
  if (ndays > NROW(here)) {ndays <- NROW(here)}
  # already sorted  by covidDownload now
  # here <- here[order(here$date, decreasing = FALSE), ]
  here <- here[(NROW(here) - ndays + 1):NROW(here), ] # show only the last n days of data
  asofhere <- max(here$date)

# already have new cases per day in download now
    # To see NEW cases added each day, and running avg of that:
  # dailychange <- c(0, diff(here$cases))
  # dailychange <- -1 * rev(c(diff(rev(here$cases)), 0))
  # dailychange[1] <- dailychange[2] # since I dont know the true value but it is not zero
  # newcasestoday <- tail(dailychange, 1)
  dailychange <- here$new
  newcasestoday <- tail(dailychange, 1)
  xvals <- 1:length(dailychange)

  maintitle <- paste("New cases each day (", averagingtime, '-day running average) ',  myplace, ' as of ', asofhere, ' for past ', ndays, ' days avail.', sep = '')
  subtitle <- paste('Most recent day saw ', newcasestoday, ' new cases among ', round(here$pop[1] / 1e6, 3), ' million people in area, or ', round(1e6 * newcasestoday / here$pop[1], 1), ' new cases per million people/day', sep = '')

  color.loess <- 'blue'
  color.running <- 'gray'
  color.recentavg <- 'black'
  color.recentfit <- 'green'
  color.daily <- 'red'


  # PLOT raw data, running avg, and locally smoothed polynomial lowess curve
  plot(dailychange, type = 'p', col = color.daily,
       main = maintitle, sub = subtitle,
       xlab = 'Day #', ylab = 'New cases each day',
       ylim = c(0, max(dailychange) * 1.05), ...)
  lines(c(round(caTools::runmean(dailychange, averagingtime, align = 'right'))), type = 'o', col = color.running)
  lines(lowess(xvals, dailychange, f = smoothspan), col = color.loess)

  # PLOT AVG OF LAST FEW DAYS
  xvals_lastfew <- tail(xvals, lastn)
  yvals_lastfew <- tail(dailychange, lastn)
  recentmean <- mean(yvals_lastfew)
  lines(xvals_lastfew, rep(recentmean, length(xvals_lastfew)), type = 'l', col = color.recentavg)

  # PLOT LINE FIT TO LAST FEW DAYS # lastn days
  # If plot lastn lines, each fit to a group of lastn days starting at lastn to 2*lastn days before now
  for (i in 1:lastn) {
    xvals_lastfew <- head(tail(xvals, lastn + i - 1), lastn)
    yvals_lastfew <- head(tail(dailychange, lastn + i - 1), lastn)

    interceptslope <- coef(line(xvals_lastfew, yvals_lastfew))
    intercept <- interceptslope[[1]]
    slope <- interceptslope[[2]]
    yvalsfit <- slope * xvals_lastfew + intercept
    if (i == 1) {
      # only draw full fit line for the very last few days
      lines(xvals_lastfew, yvalsfit, col = color.recentfit, lwd = fitlwd, lty=1)
    } else {
      # draw tiny line segment middle of fit line. to show mean and direction of slope for all the other periods of lastn days each
      i1 <- round(lastn / 2); i2 <- i1 + 1
      ysegment <- slope * xvals_lastfew[i1:i2] + intercept
      lines(xvals_lastfew[i1:i2], ysegment, col = color.recentfit)
    }
  }

  legend('top', #xvals[2],  (max(dailychange) * 0.98),
         legend = c(
           'new cases daily',
           paste(averagingtime, '-day running average of new cases', sep = ''),
           'lowess smoothing, local-wtd polynomial regression',
           paste('last', lastn, 'days fit to daily values'),
           paste('last', lastn, 'days avg')
         ),
         col = c(
           color.daily,
           color.running,
           color.loess,
           color.recentfit,
           color.recentavg
         ),
         lty = c(-1,1,1,1,1), pch = c(1, 1, -1, -1, -1), lwd = c(-1,1,1, fitlwd, 1)
  )
}