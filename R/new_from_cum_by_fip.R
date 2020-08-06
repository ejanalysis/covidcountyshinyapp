#' For each subset (place), use cumulative case data to calculate new cases per day
#'
#' Problem if more than one place has NA as FIPS code. covidDownload handled that before calling this.
#' Assumes dates are consecutive (no breaks) but need not be sorted by date beforehand.
#' If there is a gap, the new cases is the change from the prior date, whether that was
#' one day prior or longer. In other words, it is new cases since the last available date
#' for that location.
#'
#' @param cum Cumulative cases as of date
#' @param fip place, such as FIPS code from Census
#' @param date Dates that can be sorted, ideally with no gaps in dates for a given place
#'
#' @export
#'
new_from_cum_by_fip <- function(cum, fip, date) {
  originalorder <- 1:length(cum)
  # if (length())
  print(Sys.time())
  newdaily <- rep(0, length(cum))
  # could do this more efficiently, but this loop works  # TAKES 7 SECONDS !!!
  fip[is.na(fip)] <- 0 # problem if more than one place has NA fip
  dateorder <- order(date)
  cum <- cum[dateorder]
  fip <- fip[dateorder]
  for (thisfip in unique(fip)) {
    these <- fip == thisfip
    thesecases <- cum[these]
    newdaily[these] <- c(thesecases[1], diff(thesecases))
  }

  print('done'); print(Sys.time())

  return(newdaily[order(originalorder)])
}
