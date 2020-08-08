#' covidPlotContagiousEach
#'
#' @param x from covidDownload
#' @param countylist vector of county names like "Montgomery County, Maryland" as found in unique(covidDownload()[ , "fullname"])
#' @param ndays show only the last ndays days of data
#' @param dayscontagious how many recent days of new cases to add together, as a way to approximate how many people are currently still contagious
#' @param digits controls rounding in legend
#' @param ... passed to covidPlotContagious()
#'
#' @export
#'
covidPlotContagiousEach <- function(x, countylist = c('Montgomery County, Maryland', "District of Columbia, District of Columbia"), ndays, dayscontagious=14, digits=3, ...) {

  # get estimates for aggregate of all these counties
  mydata <- covidPlotContagious(x, countylist = countylist, show = FALSE, ndays = ndays, dayscontagious = dayscontagious)
  # that is max of aggr not max of any 1:
  # myylim <- 100 * c(min(mydata$percapnow, na.rm = T), 1.1 * max(mydata$percapnow, na.rm = T))
  # myylim <- 100 * c(0, max(mydata$percapnow, na.rm = T))
  mycolors <- rainbow(length(countylist), alpha = 1)

  x <- x[x$fullname %in% countylist, ]

  # do I really want to ? just to draw them in this  find worst places as of cumulative percap, not as of today which can be a different ranking
  latestdata <- x[x$date == max(x$date), c('fullname', 'percap')]
  latestdata <- latestdata[order(latestdata$percap, decreasing = TRUE), ]
  sortedcountylist <- latestdata$fullname
  # find worst places as of today

  eachdata <- list()
  latestcontagiouspercap <- vector()
  for (i in 1:length(countylist)) {
    if (i == 1) {
      eachdata[[i]] <- covidPlotContagious(x, add = FALSE, countylist = sortedcountylist[i], show = TRUE, col = mycolors[i], type = 'b', pch = i - 1, ndays = ndays, dayscontagious = dayscontagious, showlinesforpriordate = FALSE, ...)
    } else {
      eachdata[[i]] <- covidPlotContagious(x, add = TRUE,  countylist = sortedcountylist[i], show = TRUE, col = mycolors[i], type = 'b', pch = i - 1, ndays = ndays, dayscontagious = dayscontagious, showlinesforpriordate = FALSE, ...)
    }
    latestcontagiouspercap[i] <- (eachdata[[i]])[ eachdata[[i]]$date == max(eachdata[[i]]$date), 'percapnow']
  }
  nn <- length(countylist)

  # Put the graphic's legend counties sorted by per capita rate on most recent day
  newsort <- order(latestcontagiouspercap, decreasing = TRUE)
  newsortedcountylist <- sortedcountylist[newsort]
  mycolors <- mycolors[newsort]
  oldpch <- (1:nn) - 1
  newsortedpch <- (oldpch)[newsort]
  latestcontagiouspercap <-  latestcontagiouspercap[newsort] # sort

  legend('topleft', legend = paste(round(100 * latestcontagiouspercap, digits = digits), '/100:', newsortedcountylist),
         col = mycolors, lty = 1, pch =  newsortedpch)
}
