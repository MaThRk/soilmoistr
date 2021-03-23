#' Function to extract the soilmoisture data for a set of landslides with a given date
#'
#' @description This funtions takes a path to the folder of all the soilmoisture-tiffs and a geometry.
#' The geometry must have a column called \code{date}. This is necessasry in order to check if there is
#' soilmoisture data for the date of the landslide (plus, minus \code{days_before_window, days_after_window}).
#' It then extracts the soilmoisture values for the point, the buffered point, ot the polygon.
#'
#'
#' @importFrom sf read_sf st_drop_geometry st_buffer st_geometry_type
#' @importFrom lubridate hour minute month second year
#' @importFrom raster raster
#' @importFrom stars read_stars st_extract
#' @importFrom dplyr mutate
#' @importFrom exactextractr exact_extract
#' @importFrom magrittr '%>%'
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom future availableCores
#' @importFrom future.apply future_lapply
#' @importFrom future plan
#' @importFrom future multisession
#'
get_sm_data_parallel = function(landsld = NULL,
                                path_sm = "\\\\projectdata.eurac.edu/projects/Proslide/soilmoisture/32632",
                                days_before_window = 5,
                                days_after_window = 0,
                                point_buffer = NULL,
                                aggre_fun = NULL,
                                parallel = TRUE,
                                ncores = NULL) {

  # check if the landsld data is available and has a date column ------------
  check_date(landsld)

  # check that the path to the tiffs has no slash at the end
  last_char = substr(path_sm, nchar(path_sm), nchar(path_sm))
  if (last_char == "/") {
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

  # if parallel
  if (parallel) {
    # the number of cores
    if (is.null(ncores)) {
      nc = availableCores() - 4
    } else{
      nc = ncores
    }
  } else{
    stop("This only works in parallel")
  }

  # setup the workers
  registerDoParallel(nc)


  res = foreach(
    i = 1:nrow(landsld),
    .packages = c("soilmoistr",
                  "magrittr")
  ) %dopar% {

    # get the date of the landslide
    date_slide = landsld[i,][["date"]]

    # range of days around landsld
    date_range_slides = seq(date_slide - days_before_window,
                            date_slide + days_after_window,
                            by = "day")

    # images that are within that range
    matches = times[dates %in% date_range_slides]

    spatial.obj = landsld[i,]

    res = point_extraction(
      spatial.obj = spatial.obj,
      paths_sm_tiffs = paths_sm_tiffs,
      matches = matches,
      tracks = tracks,
      swaths = swaths,
      date_time = times,
      point_buffer = point_buffer,
      aggre_fun = aggre_fun)

    res = res[,1]

  }

  # return the result
  return(res)

}
