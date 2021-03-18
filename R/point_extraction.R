#' function to extract the rainfall data for points or buffered points
#'
#'
#'
#'
#' @export


point_extraction = function(landsld,
                            matches,
                            dates,
                            point_buffer,
                            aggre_fun) {

  # create the list of values for each matches date that we will put in the sm_values list
  values_match = vector("list", length = length(matches))

  # for potentially multiple matches in that time frame
  for (j in seq_along(1:length(matches))) {

    # the day of the soilmoisture image
    match = matches[[j]]

    # which soilmoisture image?
    idx = which(dates == match)

    # if working with point data, lets load it as stars
    if (is.null(point_buffer)) {

      # load that tiff as stars object
      matched_stars = read_stars(paths_sm_tiffs[[idx]])

      # extract the raster value for that point
      point_extraction = st_extract(matched_stars, landsld) %>%
        st_drop_geometry() %>%
        mutate(date_sm_acquisition = match)

      names(point_extraction)[[1]] = c("sm_values")

      # put the dataframe in the values match list
      values_match[[j]] = point_extraction

    } else{
      # We create the buffer
      # we use a polygon --> exact_extract
      buf = st_buffer(landsld[i, ], point_buffer)

      # load the tif as raster
      matched_raster = raster(paths_sm_tiffs[[i]])

      # extract the cell-values --> no aggregation
      if (is.null(aggre_fun)) {
        poly_extraction = exact_extract(matched_raster,
                                        buf,
                                        force_df = TRUE) %>%
          .[[1]] %>%
          mutate(date_sm_acquisition = match)

        values_match[[j]] = poly_extraction

      } else{
        # if we aggregate
        poly_extraction = exact_extract(matched_raster,
                                        buf,
                                        fun = aggre_fun,
                                        force_df = TRUE) %>%
          mutate(date_sm_acquisition = match)

        # put it in the list
        values_match[[j]] = poly_extraction
      }
    }

  }

  # stack all the extracted dfs to one
  values_extraction = do.call("rbind", values_match)

  # put the df with the extracted values in the list at the first place
  landsld[["sm_values"]][[1]] = values_extraction

  return(landsld)

}
