#' lead.zeroes_covid
#'
#' @param fips Census FIPS such as for state-county
#' @param length.desired how many character after leading zeroes added
#'
#' @return
#' @export
#'
lead.zeroes_covid <- function(fips, length.desired) {
  navalues <- which(is.na(fips))
  fips <- as.character(fips)
  # might trim whitespace?
  if ( (length(length.desired) > 1) & (length(fips) != length(length.desired))) {print("warning: #s of inputs don't match")}
  if ( any(length.desired == 0 | length.desired >= 100) ) {stop("error: string lengths must be >0 & <100")}
  if ( any(nchar(fips) > length.desired, na.rm = TRUE) ) {stop("error: some are longer than desired length")}

  fips <- paste( paste( rep( rep("0", length(length.desired)), length.desired), collapse = ""), fips, sep = "")
  # does that work vectorized?

  # or maybe this, but can't say length.desired[i] unless it has same length as fip & can't handle recycling also:
  # fips <- for (i in 1:length(fip)) { paste( paste( rep("0", length.desired[i]), collapse=""), fips[i], sep="") }

  fips <- substr(fips, nchar(fips) - length.desired + 1, nchar(fips))
  fips[navalues] <- NA
  return(fips)
}
