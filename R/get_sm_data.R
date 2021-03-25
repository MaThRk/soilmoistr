#' Function to extract the soilmoisture data for a set of landslides with a given date
#'
#' @description This funtions takes a path to the folder of all the soilmoisture-tiffs and a geometry.
#' The geometry must have a column called \code{date}. This is necessasry in order to check if there is
#' soilmoisture data for the date of the landslide (plus, minus \code{days_before_window, days_after_window}).
#' It then extracts the soilmoisture values for the point, the buffered point, ot the polygon.
#'
#' @param quiet Print an updating message on the status of the extraction
#'
#'
#' @importFrom sf read_sf st_drop_geometry st_buffer st_geometry_type
#' @importFrom lubridate second minute hour day month year
#' @importFrom raster raster
#' @importFrom stars read_stars st_extract
#' @importFrom dplyr mutate
#' @importFrom exactextractr exact_extract
#' @importFrom magrittr '%>%'
#'
#'
#' @export

get_sm_data = function(landsld = NULL,
                       path_sm = "\\\\projectdata.eurac.edu/projects/Proslide/soilmoisture/32632",
                       days_before_window = 5,
                       days_after_window = 0,
                       point_buffer = NULL,
                       aggre_fun = NULL,
                       quiet = TRUE) {

  # check if the landsld data is available and has a date column ------------
  check_date(landsld)

  # check that the path to the tiffs has no slash at the end
  last_char = substr(path_sm, nchar(path_sm), nchar(path_sm))
  if(last_char == "/"){
    path_sm = substr(path_sm, 1, nchar(path_sm) - 1)
  }

  # check if polygon or point
  type = st_geometry_type(landsld, by_geometry = F) %>% as.character()
  if (type == "POINT") {
    point = TRUE
  } else{
    point = FALSE
  }

  # get all the paths  ------------------------------------------------------
  paths_sm_tiffs = list.files(path_sm, full.names = TRUE)


  # if there are no files ---------------------------------------------------
  if(length(paths_sm_tiffs) == 0){
    stop("There are no files at the path you provided...")
  }

  # get the dates, tracks, times, swaths
  dates = get_dates(paths_sm_tiffs)
  tracks = get_tracks(paths_sm_tiffs)
  swaths = get_swath(paths_sm_tiffs)
  # this returns a Poixct object --> lubridate handles it!
  times = get_time(paths_sm_tiffs)

  # subset the landslides to only the days ----------------------------------
  landsld = landsld[!is.na(landsld$date),]

  ################
  # Now for each slide check if there is a soilmoisture image
  ################

  # create a column for the numner of matches and the actual soil moisture values
  landsld[["n_matches"]] = NA
  landsld[["sm_values"]] = vector("list", length(nrow(landsld)))

  # go through each spatial row
  for (i in seq_along(1:nrow(landsld))) {

    # print superinformative message
    if (!quiet) {
      n = nrow(landsld)
      str = paste0(i, "/", n)
      dashes = paste0(replicate(20, "-"), collapse = "")
      if (i == 1) {
        cat("Processing Slide No:\n")
      }
      cat(paste0("\r------------", str, dashes))
    }


    # get the date of the slide
    date_slide = landsld[i,]$date

    # range of days around landsld
    date_range_slides = seq(date_slide - days_before_window,
                            date_slide + days_after_window,
                            by = "day")

    # images that are within that range
    matches = times[dates %in% date_range_slides]

    # append the number of matches for that slide
    landsld[["n_matches"]][[i]] = length(matches)

    # get the actual spatial object
    spatial.obj = landsld[i,]

    # if there is a match check the raster values that we have at that location
    if (length(matches) > 0) {

      # cat("\nMATCH")


      # POINTS OR BUFFERED POINTS
      if (point) {

        res = point_extraction(spatial.obj = spatial.obj
                               , paths_sm_tiffs = paths_sm_tiffs
                               , matches = matches
                              , tracks = tracks
                              , swaths = swaths
                              , date_time = times
                              , point_buffer = point_buffer
                              , aggre_fun = aggre_fun)

        landsld[["sm_values"]][[i]] = res

      } else{

        # WORKING WITH POLYGONS
        res = poly_extraction(spatial.obj, matches, dates, aggre_fun)
        landsld[["sm_values"]][[i]] = res

      }

    } else{

      # No Match of dates --> the values for that slide is 0
      landsld[["sm_values"]][[i]] = NA
    }
  }

  return(landsld)

}
