#' Rerun the sequence of calls used to produce a recolorize object
#'
#' Evaluates the series of calls in the 'call' element of a recolorize object,
#' either on the original image (default) or on another image. It will almost
#' always be easier (and better practice) to define a new function that calls a
#' series of recolorize function in order than to use this function!
#'
#' @param recolorize_obj An object of S3 class 'recolorize'.
#' @param img The image on which to call the recolorize functions. If left as
#'   "original" (the default), functions are called on the original image stored
#'   in the recolorize object. Otherwise can be an object taken by the `img`
#'   argument of recolorize functions (a path to an image or an image array).
#'
#' @return A `recolorize` object.
#'
#' @details This function utilizes `eval` statements to evaluate the calls
#' that were stored in the `call` element of the specified recolorize object.
#' This makes it potentially more unpredictable than simply defining your own
#' function, which is shown in the example below.
#'
#' @examples
#'
#' # list images
#' corbetti <- system.file("extdata/corbetti.png", package = "recolorize")
#' chongi <- system.file("extdata/chongi.png", package = "recolorize")
#'
#' # fit a recolorize object by running two functions in a row:
#' rc <- recolorize(corbetti, bins = 2, plotting = FALSE)
#' rc <- recluster(rc, cutoff = 45)
#'
#' # check out the call structure (a list of commands that were run):
#' rc$call
#'
#' # we can rerun the analysis on the same image (bit pointless):
#' rerun <- rerun_recolorize(rc)
#'
#' # or, we can rerun it on a new image:
#' rerun_chongi <- rerun_recolorize(rc, img = chongi)
#'
#' \dontrun{
#' # alternatively, define a function:
#' recolorize_custom <- function(img, bins = 2, cutoff = 45) {
#'   fit <- recolorize(img, bins = bins, plotting = FALSE)
#'   fit <- recluster(fit, cutoff = cutoff)
#'   return(fit)
#' }
#'
#' # and run it on the new image:
#' chongi_fit <- recolorize_custom(chongi)
#' }
#'
#' @export
rerun_recolorize <- function(recolorize_obj,
                             img = "original") {

  original_call <- recolorize_obj$call

  # if we're using the original image, recreate it real quick
  if(img == "original") {
    img <- raster_to_array(recolorize_obj$original_img)
  }

  # if there's only one call, just run that (list structure is different)
  if (!is.list(recolorize_obj$call)) {
    recolorize_obj$call$img <- img
    recolorize_object <- eval(recolorize_obj$call)
  } else {
    # otherwise, initialize the recolorize object
    recolorize_obj$call[[1]]$img <- img
    recolorize_object <- eval(recolorize_obj$call[[1]])
    recolorize_obj
    # and then continuously modify it
    for (i in 2:length(recolorize_obj$call)) {
      recolorize_obj$call[[i]]$recolorize_obj <- recolorize_object
      recolorize_object <- eval(recolorize_obj$call[[i]])
    }
  }
  recolorize_object$call <- original_call
  return(recolorize_object)
}
