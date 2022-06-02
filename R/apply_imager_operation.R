#' Apply imager operations to layers of an image
#'
#' Internal wrapper function for applying any of several
#' `imager` morphological operations for cleaning pixsets.
#'
#' @param pixset An object of class `pixset`. Usually a layer from
#'   [splitByColor()] that has been converted to a `pixset`
#'   object.
#' @param imager_function The name of an imager morphological operation that can
#'   be performed on a pixset, passed as a string. See details.
#' @param ... Further arguments passed to the imager function being used.
#'
#' @details
#' Current imager operations are:
#' \itemize{
#'  \item [imager::grow()]: Grow a pixset
#'  \item [imager::shrink()]: Shrink a pixset
#'  \item [imager::fill()]: Remove holes in an pixset. Accomplished by
#'    growing and then shrinking a pixset.
#'  \item [imager::clean()]: Remove small isolated elements (speckle).
#'    Accomplished by shrinking and then growing a pixset.
#' }
#'
#' @return The resulting pixset after applying the specified morphological
#'   operation.
#'
apply_imager_operation <- function(pixset, imager_function, ...) {
  switch(imager_function,
         fill = imager::fill(pixset, ...),
         clean = imager::clean(pixset, ...),
         grow = imager::grow(pixset, ...),
         shrink = imager::shrink(pixset, ...))
}
