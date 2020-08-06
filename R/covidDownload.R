#' covidDownload
#'
#' Read daily update of US county-resolution COVID data from NYT dataset on github
#'
#' @param fileurl optional but will not really work if csv is not formatted as the one here
#' @param writecsv optional logical
#' @param localpath optional
#' @param testing whether to print extra info to console
#'
#' @export
#'
#' @examples
#'   x <- covidDownload()
#'   covidPlotBarNow(x)
covidDownload <- function(fileurl='https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv', writecsv=FALSE, localpath=getwd(), testing=FALSE) {
  if (testing) { print(Sys.time())}
  x <- read.csv(fileurl, stringsAsFactors = FALSE)
  if (testing) {print('finished reading dataset from website at'); print(Sys.time()); cat('\n')} # 5 seconds
  # > head(x)
  #         date    county      state  fips cases deaths
  # 1 2020-01-21 Snohomish Washington 53061     1      0
  # 2 2020-01-22 Snohomish Washington 53061     1      0

  x$date <- lubridate::ymd(x$date)
  x$fips <- lead.zeroes_covid(x$fips, 5) # analyze.stuff::lead.zeroes(x$fips, 5)
  # NOTE THAT fips in the dataset here can be NA
  # AND FOR MOST OF THOSE THE COUNTY NAME IS NOT PROVIDED, SO MAYBE IT IS A STATE TOTAL BUT I WILL DROP THOSE ROWS
  # SOMETIMES IT IS NO fips but an area like New York City, New York
  # and I will leave those here for now
  # unique(x[x$county !='Unknown' & is.na(x$fips) , 'county'])
  # [1] "New York City" "Kansas City"   "Joplin"
  x <- x[x$county != 'Unknown', ]
  #### new:
  x$fips[is.na(x$fips)] <- x$county[is.na(x$fips)] # makes it easier to use fips later

  # GET OFFICIAL COUNTY NAMES BY FIPS (e.g. Los Angeles County not just Los Angeles)

  # x$ST <- ejanalysis::get.state.info(query = x$state, fields = 'ST')$ST
  # now use data(statenameabbrev) in this pkg
  x$ST <- statenameabbrev$ST[match(x$state, statenameabbrev$state)]
  x$ST[x$state == 'Virgin Islands'] <- 'VI'
  #  could also rename  # District of Columbia, District of Columbia ?
  if (testing) {print('finished STATE NAMES at'); print(Sys.time()); cat('\n')} # 1 second

  #  could use getcountyfullnames() here but it lacks the way to handle is.na below
  # data(countyinfo, package = 'covidcountyshinyapp')
  x$fullname <- countyinfo$fullname[match(x$fips, countyinfo$FIPS.COUNTY)] # returns NA if fips not found since was not available
  x$fullname[is.na(x$fullname)] <- paste(x$county[is.na(x$fullname)], ', ', x$state[is.na(x$fullname)], sep = '')
  x$fullnameST <- paste(x$county, ', ', x$ST, sep = '')

  cpop <- countypopdata_2013_to_2017 # this is now in package data. was via # getcountypop()
  # get pop counts for these counties
  x$pop <- cpop$pop[match(x$fips, cpop$FIPS.COUNTY)]
  x$percap <- x$cases / x$pop
  x$oneper <- round(1 / x$percap,0)

  x <- x[order(x$fullname, x$date), ]  # though it sorts again in new_from_cum_by_fip()
  # c('date', 'fips', 'fullname', 'cases', 'percap', 'oneper')
  if (testing) {print('finished all except new cases per day per place at '); print(Sys.time()); cat('\n')} # 1 second

  # ## could do this more efficiently, but this loop works  # TAKES 11 SECONDS !!!
  x$new <- 0
  for (thisplace in unique(x$fullname)) {
    these <- x$fullname == thisplace
    thesecases <- x$cases[these]
    x$new[these] <- c(thesecases[1], diff(thesecases))
  }

  # x$new <- new_from_cum_by_fip(x$cases, fip = x$fips, date = x$date)

  if (testing) print('done with all but save csv at '); print(Sys.time()); cat('\n')
  if (writecsv) {
    covidWrite(x, localpath = localpath)
  }
  return(x)
}



