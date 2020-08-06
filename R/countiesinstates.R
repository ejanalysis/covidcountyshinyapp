#' countiesinstates
#'
#' @param states
#' @param allcounties
#'
#' @return
#' @export
#'
#' @examples
countiesinstates <- function(states, allcounties) {
  # allcounties <- getcountyfullnames()
  # allstates <- sort(c(state.name, 'District of Columbia'))
  countylist <- allcounties[gsub('(^.*, )(*.)', '\\2', x = allcounties) %in% states]
  return(countylist)
}
