




#' function to scan the landsld-df and check if there is a column called date
#'
#' @importFrom sf read_sf

check_date = function(landsld) {
  if (is.null(landsld)) {
    stop("You need to provide a lanslide polygon")
  }

  # there is something. Is it ab object of type sf
  if (class(landsld)[[1]] == "sf") {
    # check if there is date column
    if ("date" %in% names(landsld)) {
      # check if this is really of type date
      cl = class(landsld[["date"]])
      if (!cl == "Date") {
        stop("The column called date is not of class Date")
      }
    } else{
      stop("There is no column called date in the landsld object")
    }

  } else{
    # check if the files exist
    if (!file.exists(landsld)) {
      stop("There is no file at the landsld-location")
    }

    # if the file exists read it in as sf
    landsld = read_sf(landsld)
    cl = class(landsld[["date"]])

    # check if there is a column called date
    if ("date" %in% names(landsld)) {
      # check if this is really of type date
      cl = class(landsld[["date"]])
      if (!cl == "Date") {
        stop("The column called date is not of class Date")
      }

    } else{
      stop("There is no column called date in the landsld object")
    }

  }

}
