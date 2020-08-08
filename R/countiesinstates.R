#' countiesinstates
#' Returns all the counties found in the specified states
#' @param states Full capitalized state names like Maryland
#' @param allcounties from getcountyfullnames()
#'
#' @export
#'
countiesinstates <- function(states, allcounties) {
  # allcounties <- getcountyfullnames()
  # allstates <- sort(c(state.name, 'District of Columbia'))
  countylist <- allcounties[gsub('(^.*, )(*.)', '\\2', x = allcounties) %in% states]
  return(countylist)
}
