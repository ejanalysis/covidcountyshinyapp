#' latestcontagiouspercap
#'
#' @param x
#' @param countylist
#' @param dayscontagious
#'
#' @return
#' @export
#'
#' @examples
latestcontagiouspercap <- function(x, countylist=NULL, dayscontagious=14) {

  # Given new cases per day by county (and each county's total population size),
  # and given assumed number of days from being a new case until no longer contagious
  # (assuming only contagious from when report new case to dayscontagious days later)
  # calculate recent cases per cap (ie. possibly still contagious per cap)
  # and then return that for each county on the most recent day available
  # so that you can sort counties based on that

  latest <- vector()
  if (missing(countylist)) {countylist <- unique(x$fullname)}

  for (i in 1:length(countylist)) {
    here <- x[x$fullname == countylist[i], ]
    # get vector over all days for this one place, of recent new case count (ie still contagious) per capita
    contagiouspercap <- stillcontagious(new = here$new, dayscontagious = dayscontagious) / here$pop
    latest[i] <- contagiouspercap[here$date == max(here$date)]
  }
  names(latest) <- countylist
  return(latest)
}
