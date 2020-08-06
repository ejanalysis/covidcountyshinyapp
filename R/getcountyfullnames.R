#' getcountyfullnames
#'
#' @param fips
#'
#' @return
#' @export
#'
getcountyfullnames <- function(fips) {
  # data(countyinfo, package = 'covidcountyshinyapp')
  if (missing(fips)) {
    fips <- countyinfo$FIPS.COUNTY
  }
  fullname <- countyinfo$fullname[match(fips, countyinfo$FIPS.COUNTY)]

  # statename <- countyinfo$statename[match(fips, countyinfo$FIPS.COUNTY)]
  #
  # some names are missing in the lookup in that countyinfo dataset - need to fix later
  #   fullname[is.na(fullname)] <- paste(  x$county[is.na(fullname)], ', ', statename[is.na(fullname)], sep = '')

  return(fullname)
}
