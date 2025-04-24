#' Check if a year is a leap year
#' 
#' @param year Year provided
#' 
#' @return Boolean TRUE or FALSE
#' 
#' @export
#' 
#' @examples
#' is_leap(2000)
#' 
is_leap <- function(year) {
  if (year <= 0 || year %% 1 != 0) {
    return(stop()) # Throw error if year is 0 or less
  } else if (year %% 4 == 0 && year %% 100 != 0 || year %% 400 == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
