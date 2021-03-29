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

  return(landsld)

}


#' check if we are working with points

checkpoint = function(type, point_buffer) {

  if (type == "POINT") {
    # POINT WITHS BUFFER
    if (!is.null(point_buffer)) {
      point = FALSE
      # POINTS NO BUFFER
    } else{
      point = TRUE

      # if we pass polygons right away
    }
  } else
    point = FALSE

  return(point)

}

#' parse the dates from all the files
#' @export
get_dates = function(paths){
 dates = gsub(pattern = ".*_(20[12]\\d\\d{4})_.*", "\\1", paths)
 dates = as.Date(dates, "%Y%m%d")
 return(dates)
}

#' parse the tracks from all the soilmoisture files
#' @export
get_tracks = function(paths){
 tracks = gsub(pattern = ".*\\d{3}_([A|D]).*", "\\1", paths)
 return(tracks)
}

#' parse the time
#' @export
get_time = function(paths){

 dates = gsub(pattern = ".*_(20[12]\\d\\d{4})_.*", "\\1", paths)
 time = gsub(pattern = ".*_(\\d{6}).*", "\\1", paths)

 # we cant crate a vector like .Posixct(10) as there is no POSIX mode
 # see ?mode
 # but we can create posixct objects from a character
#' @export
 date_time = .POSIXct(character(length(time)))

 for (i in seq_along(1:length(date_time))) {
   date = as.POSIXct(paste0(dates[[i]], time[[i]]), format = "%Y%m%d%H%M%S")
   date_time[[i]]  = date
 }

 return(date_time)
}

#' parse the swath
#' @export
get_swath = function(paths){
 swath = gsub(pattern = ".*_(\\d{3})_.*", "\\1", paths)
 return(swath)
}
