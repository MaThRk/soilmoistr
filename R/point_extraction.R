#' function to extract the rainfall data for points or buffered points
#'
#'
#'
#' @export


point_extraction = function(spatial.obj,
                            paths_sm_tiffs,
                            matches,
                            tracks,
                            swaths,
                            date_time) {

  # create the list of values for each matches date that we will put in the sm_values list
  values_match = vector("list", length = length(matches))

  # for potentially multiple matches for one slide
  for (j in seq_along(1:length(matches))) {

    # the day Posixct of the soilmoisture image
    match = matches[[j]]

    # which soilmoisture image?
    idx = which(date_time == match)

    # which acquisition track and swath
    track = tracks[[idx]]
    swath = swaths[[idx]]
    year = year(match)
    month = month(match)
    if(nchar(month) == 1){
      month = paste0(0, month)
    }
    day = day(match)
    hour = hour(match)
    minute = minute(match)
    second = second(match)
    time = paste0(hour, "_", minute, "_", second)


      # load that tiff as stars object
      matched_stars = read_stars(paths_sm_tiffs[[idx]])

      # extract the raster value for that point
      point_extraction = st_extract(matched_stars, spatial.obj) %>%
        st_drop_geometry() %>%
        mutate(date = as.Date(paste0(year, month, day), "%Y%m%d"),
               track = track,
               swath = swath,
               time = time)

      names(point_extraction)[[1]] = c("sm_values")

      # put the dataframe in the values match list
      values_match[[j]] = point_extraction

  }

  # stack all the extracted dfs to one
  values_extraction = do.call("rbind", values_match)

  # return the df
  return(values_extraction)

}
