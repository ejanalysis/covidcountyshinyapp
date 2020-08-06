#' stillcontagious
#'
#' @param new
#' @param dayscontagious
#'
#' @return
#' @export
#'
#' @examples
stillcontagious <- function(new, dayscontagious) {

  still <- 0
  for (daynum in 1:length(new)) {
    startday <- max(1, daynum - dayscontagious + 1)
    endday <- daynum
    # each day, still contagious is sum of last n days of new cases,
    # but for first n days use prorated sum of days through that day
    still[daynum] <- sum(new[startday:endday], na.rm = TRUE) * (dayscontagious / (endday - startday + 1))
  }

  return(still)

  # same as this:
  #  still <- dayscontagious * caTools::runmean(new, k = dayscontagious, alg = 'fast', align = 'right', endrule = 'mean')
  #
  # testdata <- round(runif(30, min = 0, max = 20) +3*(1:30), 0)
  # cbind(testdata, stillcontagious(testdata, 3), 3 * caTools::runmean(new, k = 3, alg = 'fast', align = 'right', endrule = 'mean'))
}

