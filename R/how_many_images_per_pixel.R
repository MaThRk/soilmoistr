#' Mean Pixel value
#'
#' @description Get the mean pixel value for a list of soilmoisture-image paths
#'
#' @importFrom  doFuture registerDoFuture
#'
#' @param sm_image_paths A character vector containing the paths to soilmoisture images
#'
#'
#' @export

how_many_images_per_pixel = function(
  sm_image_paths = NULL
){

  # sum up the times it was not 0
  registerDoFuture()
  plan(multisession)
  sum = foreach(i = seq_along(sm_image_paths),
                .combine = "+") %dopar% {

                 star = read_stars(sm_image_paths)
                 star_vals = star[[1]]
                 star_vals[star_vals == 0] = 0 # unnecessary
                 star_vals[star_vals > 0] = 1
                 star_vals
                }

  # read in the first image
  dummy = read_stars(sm_image_paths[[1]])
  dummy[[1]] = sum

  # return the dummy stars object with the computed values
  return(dummy)

}
