#' function to extract the rainfall data for points or buffered points
#'
#'
#' @param landsld One single polygon from a \code{sf}-object with many polygons (like on slope unit)
#' @param matches A vector of class \code{Date} with the dates where the day of the landslide (plus minus the temporal buffer) intersect
#' @param aggre_fun A vector of one or multiple functions in order to aggregate the raster values that fall within the polygon.
#' For more see \code{\link{exact_extract}}
#'
#' @export

poly_extraction = function(landsld,
                            matches,
                            dates,
                            aggre_fun) {
  # create the list of values for each matches date that we will put in the sm_values list
  values_match = vector("list", length = length(matches))

  # for potentially multiple matches in that time frame
  for (j in seq_along(1:length(matches))) {

    # the day of the soilmoisture image
    match = matches[[j]]

    # which soilmoisture image?
    idx = which(dates == match)

    # load the tif as raster
    matched_raster = raster(paths_sm_tiffs[[i]])

    # extract the cell-values --> no aggregation
    if (is.null(aggre_fun)) {

      poly_extraction = exact_extract(matched_raster,
                                      landsld,
                                      force_df = TRUE) %>%
      .[[1]] %>%
        mutate(date_sm_acquisition = match)

      values_match[[j]] = poly_extraction

    } else{

      # if we aggregate
      poly_extraction = exact_extract(matched_raster,
                                      landsld,
                                      fun = aggre_fun,
                                      force_df = TRUE) %>%
        mutate(date_sm_acquisition = match)

      # put it in the list
      values_match[[j]] = poly_extraction
    }

    # stack all the extracted dfs to one
    values_extraction = do.call("rbind", values_match)

    # put the df with the extracted values in the list
    landsld[["sm_values"]][[i]] = values_extraction



    return(landsld)

  }

}
