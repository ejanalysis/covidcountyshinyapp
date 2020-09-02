#' sum_counties
#'
#' Create appropriately aggregated statistics on cases etc. from all counties in the data.frame
#'
#' @param df data.frame from covidDownload()
#' @param countylist vector of county names like "Montgomery County, Maryland" as found in unique(covidDownload()[ , "fullname"])
#' @param countycolname optional name of data.frame column with county names
#' @param datecolname optional name of data.frame column with date
#'
#' @export
#'
sum_counties <- function(df,
                         countylist = NULL,
                         countycolname='fullname', datecolname = 'date') {
  # c("District of Columbia, District of Columbia", 'Montgomery County, Maryland', 'Prince George\'s County, Maryland', 'Fairfax County, Virginia', 'Fairfax City, Virginia', 'Arlington County, Virginia')
  if (is.null(countylist)) {countylist <- unique(df[ , countycolname])}
  df <- df[ df[ , countycolname] %in% countylist, ]

  if (length(countylist) == 1) {
    return(df[ , c('date', 'cases', 'deaths', 'new', 'pop', 'percap', 'oneper', countycolname,
                   'newrecentlyper100k')])
  }
  # note cases is cumulative ever -- not same as currently still contagious or new today
  together <- aggregate(x = df$cases, by = list(date = df[ , datecolname]), FUN = function(z) {sum(z, na.rm = TRUE)})
  names(together)[2] <- 'cases'
  sumdeaths <- aggregate(x = df$deaths, by = list(date = df[ , datecolname]), FUN = function(z) {sum(z, na.rm = TRUE)})
  together$deaths <- sumdeaths[ , 2]
  sumnew <- aggregate(x = df$new, by = list(date = df[ , datecolname]), FUN = function(z) {sum(z, na.rm = TRUE)})
  together$new <- sumnew[ , 2]

  pops <- aggregate(df$pop, by = list(fullname = df[ , countycolname]), FUN = function(z) z[1])
  poptot <- sum(pops$x)
  together$pop <- poptot

  # note percap is cumulative per cap not new per cap or current per cap
  together$percap <- together$cases / together$pop
  together$oneper <- round(1 / together$percap, 0)

  together[ , countycolname] <- 'area' # NOTE THAT THIS RETURNS THE ACTUAL COUNTYNAME IF ONLY ONE IS PASSED TO THIS FUNCTION


  # together$newrecentlyper100k = 0 # temporarily until fixed?
  #
  # recently new per cap for overall area = pop weighted avg of that for each place ! tried:
  # together$newrecentlyper100k <- aggregate(x = df[ , c('newrecentlyper100k','pop')], by = list(date = df[ , datecolname]), FUN = function(z) {
  #   sum(z[1] * z[2], na.rm = TRUE) / sum(z[2], na.rm = TRUE)
  # })
  # THIS WAS WRONG: (it was the sum of per capita rates across places, but should be the popwtd mean of those percap rates):
  # sumnewrecentlyper100k <- aggregate(x = df$newrecentlyper100k, by = list(date = df[ , datecolname]), FUN = function(z) {sum(z, na.rm = TRUE)})
  # together$newrecentlyper100k <- sumnewrecentlyper100k[ , 2]

  together$newrecentlyper100k <- 100000 * stillcontagious_percap_bycounty(together$date, together$new, together[ , countycolname], together$pop, dayscontagious = 14)

  return(together)
}
