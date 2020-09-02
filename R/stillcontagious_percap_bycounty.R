#' Calculate cumulative recent new cases per capita for each place
#'
#' @param date dates (not necessarily sorted)
#' @param new new cases per day for given place and date
#' @param countyname place names (defining subsets within each of which the running cum recent totals are gotten)
#' @param pop population count for given place
#' @param dayscontagious how many recent days of new cases to add up
#'
#' @export
#'
#' @examples na
stillcontagious_percap_bycounty <- function(date, new, countyname, pop, dayscontagious=14) {

  # for data.frame x from covidDownload that has date, fullname, pop, new
  if (length(unique(NROW(date), NROW(new), NROW(countyname), NROW(pop))) > 1) {stop('all inputs have to be the same length')}

  # Slow to reassemble it but it should work
  x <- data.frame(oldrow = 1:NROW(date), date = date, fullname = countyname, new = new, pop = pop, stringsAsFactors = FALSE)
  countylist <- unique(countyname)
  x <- x[order(x$fullname, x$date, decreasing = FALSE), ] # oldest to newest just for now
  x$contagiouspercap <- 0

  for (i in 1:length(countylist)) {
    these <- x$fullname == countylist[i]
    here <- x[these, ]
    # sort order stillcontagious() assumes =  Assumes data are sorted in date order oldest to newest
    contagiouspercap <- stillcontagious(new = here$new, dayscontagious = dayscontagious) / here$pop
    # latest[i] <- contagiouspercap[here$date == max(here$date)]
    x[these, 'contagiouspercap'] <- contagiouspercap
  }

  x$contagiouspercap <- x$contagiouspercap[order(x$oldrow, decreasing = FALSE)] # back to original sort order
  return(x$contagiouspercap)
}
