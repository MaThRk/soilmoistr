#' Cumulate the soilmoisture-pixel value over time
#'
#' @importFrom  doFuture registerDoFuture
#' @param sm_image_paths A character vector containing the paths to soilmoisture images
#'
#' @export
#'

cumulate_pixel_value = function(
  sm_image_paths = NULL
){

  # check input
  if(is.null(sm_image_paths)){
    stop("You need to proivde character vector with paths to soilmoisture images")
  }

  # check paths
  t = all(grepl(".*soilmoisture.*\\.tif$$", sm_image_paths))
  if(!t){
    warning("I guess there might be problem with your paths")
  }

  # calculate the cumulated pixel-values
  registerDoFuture()
  plan(multisession)
  cumsum = foreach(i = seq_along(sm_image_paths),
                   .combine = "+") %dopar%{

                     star = read_stars(sm_image_paths[[i]])
                     vals = star[[1]]
                     vals
                   }


  # read in the first image
  dummy = read_stars(sm_image_paths[[1]])
  dummy[[1]] = cumsum

  # return the dummy stars object with the computed values
  return(dummy)
}
