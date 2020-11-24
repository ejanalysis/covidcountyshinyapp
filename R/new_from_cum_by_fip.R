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

  #########
  if (1 == 0) {
    # FOR TESTING:
    # This reads in the basic dataset up until the step where we want to calc new cases/day from cumulative total each day.
    x <- read.csv(fileurl, stringsAsFactors = FALSE) # 5 seconds
    x$date <- lubridate::ymd(x$date)
    x$fips <- lead.zeroes_covid(x$fips, 5) # analyze.stuff::lead.zeroes(x$fips, 5)
    x <- x[x$county != 'Unknown', ]
    x$fips[is.na(x$fips)] <- x$county[is.na(x$fips)] # makes it easier to use fips later
    x$ST <- statenameabbrev$ST[match(x$state, statenameabbrev$state)]
    x$ST[x$state == 'Virgin Islands'] <- 'VI'
    x$fullname <- countyinfo$fullname[match(x$fips, countyinfo$FIPS.COUNTY)] # returns NA if fips not found since was not available
    x$fullname[is.na(x$fullname)] <- paste(x$county[is.na(x$fullname)], ', ', x$state[is.na(x$fullname)], sep = '')
    x$fullnameST <- paste(x$county, ', ', x$ST, sep = '')
    cpop <- countypopdata_2013_to_2017 # this is now in package data. was via # getcountypop()
    x$pop <- cpop$pop[match(x$fips, cpop$FIPS.COUNTY)]
    x$percap <- x$cases / x$pop
    x$oneper <- round(1 / x$percap,0)
    x <- x[order(x$fullname, x$date), ]  # though it sorts again in new_from_cum_by_fip()
    # NOW IT NEEDS TO GET x$new CALCULATED
  }


}
