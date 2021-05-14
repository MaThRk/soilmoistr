#' Cumulate the soilmoisture-pixel value over time
#'
#' @importFrom  doFuture registerDoFuture
#' @param sm_image_paths A character vector containing the paths to soilmoisture images
#'
#' @export
#'

cumulate_pixel_value = function(sm_image_paths = NULL) {
  # check input
  if (is.null(sm_image_paths)) {
    stop("You need to proivde character vector with paths to soilmoisture images")
  }

  # check paths
  t = all(grepl(".*soilmoisture.*\\.tif$$", sm_image_paths))
  if (!t) {
    warning("I guess there might be problem with your paths")
  }

  # # calculate the cumulated pixel-values
  # registerDoFuture()
  # plan(multisession)
  # cumsum = foreach(i = seq_along(sm_image_paths),
  #                  .combine = "+") %dopar%{
  #
  #                    star = read_stars(sm_image_paths[[i]])
  #                    vals = star[[1]]
  #                    vals
  #                  }
  #
  #
  # # read in the first image
  # dummy = read_stars(sm_image_paths[[1]])
  # dummy[[1]] = cumsum


  ######################################
  # Do it in sequential as the parallel fails...
  ######################################

  sum = read_stars(sm_image_paths[[1]])[[1]]
  for (i in seq_along(sm_image_paths)) {
    if (i != 1) {
      cat("\r", i, "/", length(sm_image_paths))
      star = read_stars(sm_image_paths[[i]])
      star_vals = star[[1]]
      sum = sum + star_vals
    }
  }

  # read in the first image
  dummy = read_stars(sm_image_paths[[1]])
  dummy[[1]] = sum


  # return the dummy stars object with the computed values
  return(dummy)


  # return the dummy stars object with the computed values
  return(dummy)
}
