#' covidPlotTrend
#'
#' Draws plot of cumulative total cases (per 100 population) vs date
#' for specified US counties
#'
#' @param x from covidDownload()
#' @param countylist
#' @param ndays Filter to only the last n days of data
#' @param ... other parameters to pass to plot()
#'
#' @export
#'
#' @examples
covidPlotTrend <- function(x, countylist = NULL, ndays, ...) {

  if (missing(x)) {
    x <- covidDownload()
  }
  if (is.null(countylist)) {countylist <- unique(x$fullname)}

  # Filter to only one county &
  # sort by date
  # if (length(countylist) == 0) return() # nothing to plot
  # myplace <- countylist[1]
  # if (length(countylist) == 1) {
  #   myplace2 = ''
  # } else {
  #   myplace2 <- countylist[2]
  # }
  # here <- x[x$fullname == myplace, ]
  #

  if (length(countylist) == 1) {
    myplace <- countylist
  } else {
    myplace <- 'Selected Counties'
  }
  here <- sum_counties(x, countylist = countylist, datecolname = 'date', countycolname = 'fullname')

  here <- here[order(here$date, decreasing = FALSE), ]

  # Filter to only the last n days of data
  if (missing(ndays)) {ndays <- NROW(here)}
  if (ndays > NROW(here)) {ndays <- NROW(here)}
  here <- here[(NROW(here) - ndays + 1):NROW(here), ]

  # what is the most recent date with data here?
  asofhere <- max(here$date)
  oneperasof <- here$oneper[here$date == asofhere] # the number for just the latest one day

  # Define plot title and range of y values
  # if (myplace2 == '') {
    # *** ONE LOCATION -
    # if (length(myplace) == 0) {return() } # no data to plot

    maintitle <- paste(round(100*here$percap[here$date == asofhere], 1), 'per 100 people (1 in', oneperasof, ') in', myplace, 'was ever confirmed infected through', asofhere)

    myylim <- 100 * c(min(here$percap, na.rm = T), max(here$percap, na.rm = T))
  # } else {
    # TWO LOCATIONS TO PLOT - define here2, plot title and range of y values
    # here2 <- x[x$fullname == myplace2, ]
    # show only the last n days of data for second location
    # but that will fail to graph correctly if place 2 has data for different dates than place 1 *****
    # here <- here[(NROW(here) - ndays + 1):NROW(here), ]
    # maintitle <- paste(myplace, 'and', myplace2, 'as of', asofhere, '(now 1 in', oneperasof, 'people in former)')
    # myylim <- 100 * c(min(here$percap, here2$percap, na.rm = T), max(here$percap, here2$percap, na.rm = T))
  # }

  # *** PLOT GRAPH
  # LEGEND OR lty get reversed if inserting new county before single one shown!!
  par(lty = 3)
  plot(here$date, 100*here$percap, main = maintitle,
       ylim = myylim,  #asp = 0.5,
       xlab = 'Date', ylab = 'Reported cases per 100 people', ...)


  # To see NEW cases added each day, and running avg of that:
  # x <- covidDownload()
  # here <- x[x$fullname == 'Montgomery County, Maryland', ]
  # here <- here[order(here$date), ]
  # plot(diff(here$cases), type = 'p', main = 'New cases added each day, and running avg', xlab = 'Day #')
  # lines(c(round(runmean(diff(here$cases), 3, align = 'right'))))

  # if (myplace2 != '') {
    # PLOT second county, and add legend
    # and later COULD ADD ABILITY TO PLOT 3+ COUNTIES AT ONCE HERE
    # par(lty = 4)
    # lines(here2$date, 100*here2$percap)
    # par(lty = 1)
    # legend('left', legend = c(myplace, myplace2), lty = c(1, 4), pch =  c('o', '.'))
  # }
}
