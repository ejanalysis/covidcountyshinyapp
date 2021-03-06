#' covidPlotTrendNewRunFit
#'
#' @param x from covidDownload
#' @param countylist vector of county names like "Montgomery County, Maryland" as found in unique fullname entries in covidDownload()
#' @param averagingtime how many days to include in the running average
#' @param ndays show only the last ndays days of data
#' @param lastn how many days to use in fitting line to a few recent days
#' @param fitlwd width of fitted line
#' @param fitlty type of fitted line
#' @param autoregress not used
#' @param ... passed to plot()
#'
#' @export
#'
covidPlotTrendNewRunFit <- function(x, countylist = NULL, averagingtime=3, ndays, lastn=14, fitlwd=3, fitlty=1, autoregress=FALSE, ...) {
  if (missing(x)) {x <- covidDownload()}
  if (is.null(countylist)) {countylist <- unique(x$fullname)}
  if (length(countylist) == 0) return() # nothing to plot
  if (length(countylist) == 1) {
    myplace <- countylist
  } else {
    myplace <- 'Selected Counties'
  }
  # CALC NUMBERS FOR AGGREGATE OF ALL SELECTED COUNTIES
  here <- sum_counties(x, countylist = countylist, datecolname = 'date', countycolname = 'fullname')

  if (missing(ndays)) {ndays <- NROW(here)}
  if (ndays > NROW(here)) {ndays <- NROW(here)}
  # already sorted by covidDownload  # here <- here[order(here$date, decreasing = FALSE), ]
  here <- here[(NROW(here) - ndays + 1):NROW(here), ] # show only the last n days of data
  dailychange <- here$new
  xvals <- 1:length(dailychange)   # xvals <- here$date  # this would make it a bit harder to do the line fit

  # PLOT #####
  runav <- c(round(caTools::runmean(dailychange, averagingtime, align = 'right')))
  asofhere <- max(here$date)
  maintitle <- paste("New cases/day (", averagingtime, '-day avg) ',  myplace, ' as of ', asofhere, ' for past ', ndays, ' days avail.', sep = '')
  color.running <- 'gray'
  color.recentfit <- 'green' # color.loess <- 'blue'  # color.recentavg <- 'black' #  # color.daily <- 'red'
  plot(runav,  # x = xvals, y = runav,
       type = 'o', col = color.running,
       main = maintitle, ylab = 'Running avg new cases/day', xlab = 'Day #',
       ylim = c(0, max(runav * 1.05)), ...)

  # PLOT LINE FIT TO LAST FEW DAYS # lastn days ###l####
  if (autoregress) {
    warning('autoregression not coded yet')
  } else {
    i <- 1
    xvals_lastfew <- head(tail(xvals, lastn + i - 1), lastn)
    yvals_lastfew <- head(tail(runav, lastn + i - 1), lastn)
    interceptslope <- coef(line(xvals_lastfew, yvals_lastfew))  # MAYBE USE lm INSTEAD? and get p-value ****
    intercept <- interceptslope[[1]]
    slope <- interceptslope[[2]]
    yvalsfit <- slope * xvals_lastfew + intercept
    #  draw full fit line for the very last few days
    lines(xvals_lastfew, yvalsfit, col = color.recentfit, lwd = fitlwd, lty = fitlty)
  }
  legend('top', # xvals[2], round(max(runav) * 4/5),
         legend = c(
           paste(averagingtime, '-day avg new cases', sep = ''),
           paste('last ', lastn, ' day fit to ', averagingtime,'-day avg', sep = '')
         ),
         col = c(
           color.running,
           color.recentfit
         ),
         lty = c(1, fitlty), pch = c(-1, -1), lwd = c(1, fitlwd)
  )
}
