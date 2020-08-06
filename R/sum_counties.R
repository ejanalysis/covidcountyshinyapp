#' sum_counties
#'
#' @param df
#' @param countylist
#' @param countycolname
#' @param datecolname
#'
#' @return
#' @export
#'
#' @examples
sum_counties <- function(df,
                         countylist = NULL,
                         countycolname='fullname', datecolname = 'date') {
  # c("District of Columbia, District of Columbia", 'Montgomery County, Maryland', 'Prince George\'s County, Maryland', 'Fairfax County, Virginia', 'Fairfax City, Virginia', 'Arlington County, Virginia')
  if (is.null(countylist)) {countylist <- unique(df[ , countycolname])}
  df <- df[ df[ , countycolname] %in% countylist, ]

  if (length(countylist) == 1) {
    return(df[ , c('date', 'cases', 'deaths', 'new', 'pop', 'percap', 'oneper', countycolname)])
  }
  together <- aggregate(x = df$cases, by = list(date = df[ , datecolname]), FUN = function(z) {sum(z, na.rm = TRUE)})
  names(together)[2] <- 'cases'
  sumdeaths <- aggregate(x = df$deaths, by = list(date = df[ , datecolname]), FUN = function(z) {sum(z, na.rm = TRUE)})
  together$deaths <- sumdeaths[ , 2]
  sumnew <- aggregate(x = df$new, by = list(date = df[ , datecolname]), FUN = function(z) {sum(z, na.rm = TRUE)})
  together$new <- sumnew[ , 2]

  pops <- aggregate(df$pop, by = list(fullname = df[ , countycolname]), FUN = function(z) z[1])
  poptot <- sum(pops$x)
  together$pop <- poptot

  together$percap <- together$cases / together$pop
  together$oneper <- round(1 / together$percap, 0)

  together[ , countycolname] <- 'area' # NOTE THAT THIS RETURNS THE ACTUAL COUNTYNAME IF ONLY ONE IS PASSED TO THIS FUNCTION
  return(together)
}
