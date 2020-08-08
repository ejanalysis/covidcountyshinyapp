#' covidPlotTrendNewEachPlace
#'
#' @param x from covidDownload
#' @param countylist vector of county names like "Montgomery County, Maryland" as found in unique(covidDownload()[ , "fullname"])
#' @param averagingtime how many days to include in the running average
#' @param smoothspan parameter controlling how localized or smoothed the loess curve should be
#' @param ndays show only the last ndays days of data
#' @param lastn how many days to use in fitting line to a few recent days
#' @param percap should it show per capita or total counts
#' @param ... passed to plot()
#'
#' @export
#'
covidPlotTrendNewEachPlace <- function(x, countylist = c('Montgomery County, Maryland', "District of Columbia, District of Columbia"), averagingtime=3, smoothspan, ndays, lastn=14, percap=TRUE, ...) {

  if (missing(x)) {
    x <- covidDownload()
  }
  if (missing(smoothspan)) {smoothspan <- 0.15}
  nplaces <- length(countylist)
  if (nplaces == 0) return() # nothing to plot
  if (nplaces == 1) {
    myplace <- countylist
  } else {
    myplace <- 'Selected Counties'
  }

  hereall <- x[x$fullname %in% countylist, ]
  dayshere <- length(unique(hereall$date)) # how many days have data for at least one county in hereall
  if (missing(ndays)) {ndays <- dayshere}
  if (ndays > dayshere) {ndays <- dayshere}
  asofhere <- max(hereall$date)
  mycolors <- rainbow(nplaces, alpha = 1)

  # will find max y value for graph - largest new cases per day for any of these places
  miny <- 0
  maxy <- rep(0, nplaces)

  xvals <- list()
  dailychange <- list()

  for (i in 1:nplaces) { # i = 1
    oneplace <- countylist[i]
    here <- hereall[hereall$fullname == oneplace, ]
    here <- here[order(here$date, decreasing = FALSE), ]
    dayshere <- length(unique(here$date)) # this can be different for each county
    if (ndays > dayshere) {ndays <- dayshere}
    here <- here[(dayshere - ndays + 1):dayshere, ] # show only the last n days of data
    # no longer assumes all places have data each day in the full range of days!!!
    # but assumes no missing days in past ndays  *********** otherwise need recode to get diff for specific dates and then align all counties by date
    # To see NEW cases added each day, and running avg of that:
    # dailychange <- c(0, diff(here$cases)) # or the same as...

    if (percap) {
      dailychange[[i]] <- -1 * rev(c(diff(rev(1e6 * here$percap)), 0))
      ylab <- 'New cases per million people, per day'
      maintitle <- paste("New cases/mill. people each day (", averagingtime, '-day running average) ', myplace, ' as of ', asofhere, ' for past ', ndays, ' days avail.', sep = '')

    } else {
      dailychange[[i]] <- -1 * rev(c(diff(rev(here$cases)), 0))
      ylab <- 'New cases per day'
      maintitle <- paste("New cases each day (", averagingtime, '-day running average) ',  myplace, ' as of ', asofhere, ' for past ', ndays, ' days avail.', sep = '')
    }
    dailychange[[i]][1] <- dailychange[[i]][2] # since I dont know the true value but it is not zero
    xvals[[i]] <- here$date #1:length(dailychange) # this can vary by place

    # will find max y value for graph - largest new cases per day for any of these places
    maxy[i] <- max(caTools::runmean(dailychange[[i]], averagingtime, align = 'right'))
  }
  for (i in 1:nplaces) {
    if (i == 1) {
      plot(xvals[[i]], caTools::runmean(dailychange[[i]], averagingtime, align = 'right'), type = 'b', col = mycolors[i],
           ylim = c(miny, max(maxy)), main = maintitle, xlab = 'Day #', ylab = ylab, pch = i, ...)

    } else {
      lines(xvals[[i]], caTools::runmean(dailychange[[i]], averagingtime, align = 'right'), type = 'b', col = mycolors[i], pch = i, ...)

    }
  }
  legend(x = xvals[[1]][1], y = max(maxy) * 0.95,
         legend = countylist,
         col = mycolors, lty = 1, pch = 1:nplaces  #fill = c('red', 'gray', 'blue'),
  )

  # linefit(1:length(dailychange), dailychange, show.lowess = T, show.lm = F, show.line = F)
}
