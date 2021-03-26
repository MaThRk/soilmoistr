
#' function to extract the rainfall data for points or buffered points
#'
#'
#' @param landsld One single polygon from a \code{sf}-object with many polygons (like on slope unit)
#' @param matches A vector of class \code{Date} with the dates where the day of the landslide (plus minus the temporal buffer) intersect
#' @param aggre_fun A vector of one or multiple functions in order to aggregate the raster values that fall within the polygon.
#' For more see \code{\link{exact_extract}}
#'
#' @export

poly_extraction = function(spatial.obj,
                           paths_sm_tiffs,
                           matches,
                           tracks,
                           swaths,
                           point_buffer,
                           times,
                           aggre_fun) {

  # create the list of values for each matches date that we will put in the sm_values list
  values_match = vector("list", length = length(matches))

  # for potentially multiple matches in that time frame
  for (j in seq_along(1:length(matches))) {

    # the day of the soilmoisture image
    match = matches[[j]]

    # which soilmoisture image?
    idx = which(times == match)

    # Get more information about this soilmoisture images
    track = tracks[[idx]]
    swath = swaths[[idx]]
    year = year(match)
    month = month(match)
    if (nchar(month) == 1) {
      month = paste0(0, month)
    }
    day = day(match)
    hour = hour(match)
    minute = minute(match)
    second = second(match)
    time = paste0(hour, "_", minute, "_", second)

    # load the soilmoisture image as raster
    matched_raster = raster(paths_sm_tiffs[[i]])

    # if we were originally using points, we must here create the buffer around that point
    if (!is.null(point_buffer)) {
      spatial.obj = st_buffer(spatial.obj, point_buffer)
    }

    # extract the cell-values --> no aggregation
    if (is.null(aggre_fun)) {
      res = exact_extract(matched_raster,
                                      spatial.obj,
                                      force_df = TRUE) %>%
        .[[1]] %>%
        mutate(
          date = as.Date(paste0(year, month, day), "%Y%m%d"),
          track = track,
          swath = swath,
          time = time
        )

      values_match[[j]] = res

    } else{
      # if we aggregate
      res = exact_extract(matched_raster,
                                      spatial.obj,
                                      fun = aggre_fun,
                                      force_df = TRUE) %>%
        mutate(
          date = as.Date(paste0(year, month, day), "%Y%m%d"),
          track = track,
          swath = swath,
          time = time
        )

      # put it in the list
      values_match[[j]] = res
    }


  }

  # stack all the extracted dfs to one
  values_extraction = do.call("rbind", values_match)

  # return the df
  return(values_extraction)

}
