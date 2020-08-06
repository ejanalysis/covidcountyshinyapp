#' covidtrackingDownload
#' Get daily data from \code{\url{'https://covidtracking.com/api/v1/states/daily.csv'}}
#' @return
#' @export
#'
#' @examples
covidtrackingDownload <- function() {

  # state not county resolution

x <- read.csv('https://covidtracking.com/api/v1/states/daily.csv', stringsAsFactors = FALSE)

# 'https://covidtracking.com/data/download/api/v1/states/daily.csv'
# daily new viral tests positive daily  'positiveIncrease'
# daily new total tests positive     'totalTestResultsIncrease'

x$newtestpctpos <- x$positiveIncrease / x$totalTestResultsIncrease
x$date <- lubridate::ymd(x$date)
deprecatedfields <- c('checkTimeEt', 'commercialScore', 'dateChecked', 'dateModified', 'grade', 'hash', 'hospitalized', 'negativeIncrease', 'negativeRegularScore', 'negativeScore', 'posNeg', 'positiveScore', 'score', 'total')
x <- x[ , !(names(x) %in% deprecatedfields)]

# of most interest:
# my newtestpctpos
# and date, state, fips,
# # positive, negative,
# inIcuCurrently           "144"                                      "149"
# inIcuCumulative          NA                                         NA
# onVentilatorCurrently    NA                                         NA
# onVentilatorCumulative   NA                                         NA
# recovered                "5029"                                     "5013"
# death, hospitalized,

# and
# c('positiveCasesViral', 'positiveIncrease','negativeIncrease',"totalTestResultsIncrease")

return(x)
}
