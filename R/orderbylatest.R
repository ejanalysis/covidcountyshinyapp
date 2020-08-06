#' orderbylatest
#'
#' @param myvar
#' @param placevar
#' @param mydate
#'
#' @return
#' @export
#'
#' @examples
orderbylatest <- function(myvar, placevar, mydate) {
  # returns number that is new sort order for
  # dataset with multiple places and multiple dates,
  # based on rank of place-specific myvar on latest date

  placelist <- unique(placevar)
  latestdate <- max(mydate)

  # get just latest for all places

  myvarlatest <- rep(0, length(placelist))

  for (i in 1:length(placelist)) {
    myvarlatest[i] <- myvarlatest[(placevar == placelist[i]) & (mydate == latestdate)]
    #   myvarhere <- myvar[placevar == placelist[i]]
    #   datehere <- mydate[placevar == placelist[i]]
    #   myvarlatest[i] <- myvarhere[datehere == max(datehere)] # problem - lengths differ
  }


  # find rank of each place
  ranks <- rank(myvarlatest)
  ranks <- max(ranks) - ranks + 1 # so that 1 = lowest value

  # apply that to whole dataframe
  allranks <- ranks[match(placevar, placelist)]
  return(allranks)
}
