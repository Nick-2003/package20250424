#' Temperature conversion from one scale to another
#' 
#' @param temp Temperature value
#' @param start_scale Starting temperature scale
#' @param end_scale Ending temperature scale
#' 
#' @return Temperature value in the end scale
#' 
#' @export
#' 
#' @examples
#' temp_conv(41, "F", "C")
#' 
temp_conv <- function(temp, start_scale, end_scale) {
  if (!(start_scale %in% c("F", "C", "K")) ||
        !(end_scale %in% c("F", "C", "K"))) {
    stop("Invalid start or end scale.")
  } else if (!is.numeric(temp)) {
    stop("Invalid temperature value.")
  } else if ((temp < 0 && start_scale == "K") || (temp < -273.15 && start_scale == "C") || (temp < -459.67 && start_scale == "F")) {
    stop("Invalid temperature.")
  }
  # stopifnot((start_scale %in% c("F", "C", "K")), !(end_scale %in% c("F", "C", "K")), 
  #           is.numeric(temp),
  #           (temp >= 0 && start_scale == "K") || (temp >= -273.15 && start_scale == "C") || (temp >= -459.67 && start_scale == "F"))


  if (start_scale == end_scale) {
    return(temp)
  }

  if (start_scale == "F") {
    if (end_scale == "C") {
      result <- (temp - 32) * (5 / 9)
    } else if (end_scale == "K") {
      result <- ((temp - 32) * (5 / 9)) + 273.15
    }
  } else if (start_scale == "C") {
    if (end_scale == "F") {
      result <- (temp * (9 / 5)) + 32
    } else if (end_scale == "K") {
      result <- temp + 273.15
    }
  } else if (start_scale == "K") {
    if (end_scale == "F") {
      result <- ((temp - 273.15) * (9 / 5)) + 32
    } else if (end_scale == "C") {
      result <- temp - 273.15
    }
  }

  return(result)
}