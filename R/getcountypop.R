getcountypop <- function() {
  # OBSOLETE - NOW VIA data(countypopdata_2013_to_2017)  # GET COUNTY POPULATION TOTALS - but very inefficient way - get a smaller dataset
  # warning('uses bg19 blockgroup data from ejscreen package - planning to replace in this function with just county pop data')
  # it was created this way:
  # library(ejscreen)
  # cpop <- aggregate(bg19$pop, by = list(bg19$FIPS.COUNTY), FUN = function(z) sum(z, na.rm = TRUE))
  # names(cpop) <- c('FIPS.COUNTY', 'pop')
  # return(cpop)
}
