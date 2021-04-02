#' covidPlotContagiousEach
#'
#' @param x from covidDownload
#' @param countylist vector of county names like "Montgomery County, Maryland" as found in unique fullname entries in covidDownload()
#' @param ndays show only the last ndays days of data
#' @param dayscontagious how many recent days of new cases to add together, as a way to approximate how many people are currently still contagious
#' @param perx per how many people such as per 100 or per 100000
#' @param digits controls number of significant digits in legend
#' @param crop_length_legend how many characters to use of the place names in legend
#' @param ... passed to covidPlotContagious()
#'
#' @export
#'
covidPlotContagiousEach <- function(x, countylist = c('Montgomery County, Maryland', "District of Columbia, District of Columbia"), ndays, dayscontagious=14, perx=100000, digits=3, crop_length_legend=10, ...) {

  # get estimates for aggregate of all these counties
  mydata <- covidPlotContagious(x, countylist = countylist, show = FALSE, ndays = ndays, dayscontagious = dayscontagious, perx=perx)
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
      eachdata[[i]] <- covidPlotContagious(x, add = FALSE, countylist = sortedcountylist[i], show = TRUE, col = mycolors[i], type = 'b', pch = i - 1, ndays = ndays, dayscontagious = dayscontagious, perx=perx, showlinesforpriordate = FALSE, ...)
    } else {
      eachdata[[i]] <- covidPlotContagious(x, add = TRUE,  countylist = sortedcountylist[i], show = TRUE, col = mycolors[i], type = 'b', pch = i - 1, ndays = ndays, dayscontagious = dayscontagious, perx=perx, showlinesforpriordate = FALSE, ...)
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

  zeroes <- round(log10(perx))
  perxlegend <- perx
  if (zeroes >= 6) {
    perxlegend <- paste(perxlegend/1e6, 'mill', sep = '')
  } else {
    if (zeroes >= 3) perxlegend <- paste(perxlegend/1000, 'k', sep = '')
  }
  newsortedcountylist.cropped <- substr(newsortedcountylist, 1, crop_length_legend)
  legend('topleft', legend = paste(signif(perx * latestcontagiouspercap, digits = digits), '/', perxlegend,':',
                                   newsortedcountylist.cropped),
         col = mycolors, lty = 1, pch =  newsortedpch)
}

