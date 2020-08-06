#' covidPlotCum_v_New
#'
#' @param x
#' @param countylist
#' @param averagingtime
#' @param ndays
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
covidPlotCum_v_New <- function(x, countylist = NULL, averagingtime=3, ndays, ...) {
  # countylist = c("District of Columbia, District of Columbia")
  if (missing(x)) {
    x <- covidDownload()
  }
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
  here <- here[order(here$date, decreasing = FALSE), ]
  here <- here[(NROW(here) - ndays + 1):NROW(here), ] # show only the last n days of data
  asofhere <- max(here$date)

  maintitle <- paste("New cases/mill. people each day (", averagingtime, '-day running average) ', myplace, ' as of ', asofhere, ' for past ', ndays, ' days avail.', sep = '')
  # To see NEW cases added each day, and running avg of that:

  # Do I really want the x axis to be a running avg??? prob not
  # heresmooth <- caTools::runmean(here$cases, averagingtime, align = 'right')
  heresmooth <- here$cases

  # dailychange <- c(0, diff(here$cases))
  dailychange <- 1e6 * c(0, diff(here$percap))
  dailychange[1] <- dailychange[2] # since I dont know the true value but it is not zero
  dailychangesmooth <- caTools::runmean(dailychange, averagingtime, align = 'right')

  plot(heresmooth, dailychangesmooth, main = maintitle,
       xlab = 'Cumulative Cases', ylab = 'New cases per million people, each day',
       ylim = c(0, max(dailychangesmooth) * 1.05),
       type = 'b')
  abline(h = 100); abline(h = 10)

  # plot(dailychange, type = 'p', sub = subtitle, main = maintitle, xlab = 'Day #', ylab = 'New cases/day')
  # lines(c(round(caTools::runmean(dailychange, averagingtime, align = 'right'))), type = 'o', col = 'gray')
  # linefit(1:length(dailychange), dailychange, show.lowess = T, show.lm = F, show.line = F)

}
