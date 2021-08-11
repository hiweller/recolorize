#' Merge layers in a recolorized image
#'
#'  Merges specified layers in a recolorized image. This is a good option if you
#'  want to manually specify which layers to merge (and what color to make the
#'  resulting merged layer); it's also called on by other `recolorize` functions
#'  like \code{\link{recluster}} to merge layers that have been identified
#'  as highly similar in color using a given distance metric.
#'
#' @param recolorize_obj An object of class "recolorize", such as from
#'   \code{\link{recolorize}}, \code{\link{recluster}}, or
#'   \code{\link{imposeColors}}.
#' @param merge_list A list of numeric vectors specifying which layers
#'   to merge. Layers not included in this list are unchanged. See examples.
#' @param color_to Color(s) for the merged layers. See examples.
#' @param plotting Logical. Plot the results of the layer merging next
#'   to the original color fit for comparison?
#'
#'
#' @return
#' A `recolorize` class object with merged layers. The order of the returned
#' layers depends on `merge_list`: the first layers will be any not included
#' in the list, followed by the new merged layers. If you start with layers
#' 1-8 and merge layers 4 & 5 and 7 & 8, the returned 5 layers will be, in
#' order and in terms of the original layers: 1, 2, 3, 6, 4 & 5 (merged), 7 & 8
#' (merged). This is probably easiest to see in the examples.
#'
#' @details
#'  Colors can be supplied as numeric RGB triplets (e.g. `c(1, 1, 1)` for
#'  white), a valid R color name (`"white"`), or a hex code (`"#FFFFFF`).
#'  Alternatively, `color_to = "weighted average"` will set the merged layer to
#'  the average color of the layers being merged, weighted by their relative
#'  size. Must be either a single value or a vector the same length as
#'  `merge_list`. If a single color is supplied, then all merged layers
#'  will be set to that color (so this really is only useful if you're
#'  already merging those layers into a single layer).
#'
#' @examples
#' # image path:
#' img <- system.file("extdata/corbetti.png", package = "recolorize")
#'
#' # initial fit, 8 bins:
#' init_fit <- recolorize(img)
#' # redundant green, red, and blue clusters
#'
#' # to make it easier to see, we can plot the numbered palette:
#' plot(init_fit)
#'
#' # based on visual inspection, we should merge:
#' mlist <- list(c(3, 5),
#'               c(4, 7),
#'               c(6, 8))
#'
#' # we can merge with that list, leaving layers 1 & 2 intact:
#' vis_merge <- mergeLayers(init_fit,
#'                          merge_list = mlist)
#'
#' # we can include layers 1 & 2 as their own list elements,
#' # leaving them intact (result is identical to above):
#' mlist2 <- list(1, 2,
#'                c(3, 5),
#'                c(4, 7),
#'                c(6, 8))
#' redundant_merge <- mergeLayers(init_fit,
#'                                merge_list = mlist2)
#'
#' # we can also swap layer order this way without actually merging layers:
#' swap_list <- list(2, 5, 3, 4, 1)
#' swap_layers <- mergeLayers(redundant_merge,
#'                            merge_list = swap_list)
#'
#' # merging everything but the first layer into a single layer,
#' # and making that merged layer orange (result looks
#' # a bit like a milkweed bug):
#' milkweed_impostor <- mergeLayers(init_fit,
#'                                  merge_list = list(c(2:8)),
#'                                  color_to = "orange")
#'
#' # we can also shuffle all the layer colors while
#' # leaving their geometry intact:
#' centers <- vis_merge$centers
#' centers <- centers[sample(1:nrow(centers), nrow(centers)), ]
#' shuffle_layers <- mergeLayers(vis_merge,
#'                               merge_list = as.list(1:5),
#'                               color_to = centers)
#' # (this is not really the intended purpose of this function)
#'
#' @export
mergeLayers <- function(recolorize_obj,
                        merge_list = NULL,
                        color_to = "weighted average",
                        plotting = TRUE) {

  # check parameters
  pm <- clean_merge_params(recolorize_obj,
                              merge_list,
                              color_to)
  merge_list <- pm$merge_list
  color_to <- pm$color_to

  # split the layers
  layers <- splitByColor(recolorize_obj,
                          plot_method = "none")

  # get centers and sizes
  centers <- recolorize_obj$centers
  sizes <- recolorize_obj$sizes

  # get the list of untouched layers
  orig_layers <- which(!(1:length(layers) %in% unlist(merge_list)))
  new_centers <- centers[orig_layers, ]
  new_sizes <- sizes[orig_layers]
  px_assign <- matrix(0,
                      nrow = nrow(recolorize_obj$pixel_assignments),
                      ncol = ncol(recolorize_obj$pixel_assignments))

  # if any layers are going untouched...
  if (length(orig_layers) > 0 & sum(new_sizes) > 0) {
    for (i in 1:length(orig_layers)) {
      # copy their pixel assignments:
      if (new_sizes[i] > 0) {
        px_assign[which(recolorize_obj$pixel_assignments ==
                          orig_layers[i])] <- i
      } else {
        next
      }
    }
  }

  # for every element of merge list...
  for (i in 1:length(merge_list)) {

    # get the vector of colors to combine
    merge_vector <- merge_list[[i]]

    # extract the first layer
    layer <- layers[[merge_vector[1]]]

    # get the new color:
    col_to <- color_to[i]
    if (col_to == "weighted average") {
      if (length(merge_vector) > 1) {
        col_to <- apply(centers[merge_vector, ], 2,
                        function(j) stats::weighted.mean(j,
                                     sizes[merge_vector]))
      } else {
        col_to <- centers[merge_vector, ]
      }

    } else {
      col_to <- color_to[i]
      col_to <- as.vector(grDevices::col2rgb(col_to) / 255)
    }

    # add the other layers
    # this is silly but doesn't seem that slow
    if (length(merge_vector) > 1) {
      for (j in 2:length(merge_vector)) {
        layer <- layer + layers[[merge_vector[j]]]
      }
    }

    # add new center & size values
    new_centers <- rbind(new_centers, col_to)
    new_sizes <- c(new_sizes, sum(sizes[merge_vector]))

    # change pixel assignments
    idx <- which(recolorize_obj$pixel_assignments %in% merge_vector)
    px_assign[idx] <- length(orig_layers) + i

    # convert to pixset
    layer <- imager::as.pixset(imager::as.cimg(layer))

  }

  # thbbt
  rownames(new_centers) <- NULL

  # remove any stray empty things
  if (any(new_sizes == 0)) {
    new_centers <- new_centers[-which(new_sizes == 0), ]
    new_sizes <- new_sizes[-which(new_sizes == 0)]
  }

  # reconstruct the recolorize obj
  merged_obj <- recolorize_obj
  merged_obj$centers <- new_centers
  merged_obj$sizes <- new_sizes
  merged_obj$pixel_assignments <- px_assign

  if (plotting) {

    # be polite
    user_par <- graphics::par(no.readonly = TRUE)

    # set layout
    graphics::layout(matrix(1:4, nrow = 1),
           widths = c(0.3, 0.2, 0.3, 0.2))

    # plot original color map & palette
    recolorize::plotImageArray(recoloredImage(recolorize_obj),
                               main = "Recolored original")
    recolorize::plotColorPalette(recolorize_obj$centers,
                                 horiz = FALSE)

    # plot new color map & palette
    recolorize::plotImageArray(recoloredImage(merged_obj),
                               main = "Merged image")
    recolorize::plotColorPalette(merged_obj$centers,
                                 horiz = FALSE)

    # reset parameters
    graphics::par(user_par)

  }

  # append the call
  merged_obj$call <- append(recolorize_obj$call, match.call())

  return(merged_obj)

}

#' Clean up parameters passed to mergeLayers
#'
#' Internal function for tidiness.
#'
#' @param recolorize_obj Object of `recolorize` class.
#' @param merge_list List of layers to merge.
#' @param color_to Argument for coloring new layers.
clean_merge_params <- function(recolorize_obj,
                         merge_list,
                         color_to) {

  # check if recolorize_obj is a correct class
  if (class(recolorize_obj) != "recolorize") {
    stop("Must provide an object of class 'recolorize', as output
         by recolorize(), recluster(), or imposeColors()")
  }

  # if it's RGB triplets, change to hex codes...
  if (is.numeric(color_to)) {
    dim(color_to) <- c(length(color_to) / 3, 3)
    color_to <- apply(color_to, 1,
                      function(i) grDevices::rgb(i[1], i[2], i[3]))
  }

  # make sure that color_to, fill, and clean parameters
  # are all of length == 1 | length(merge_list)
  if (!length(color_to) %in% c(1, length(merge_list))) {
    stop("'color_to' must be either length 1 or the
         same length as 'merge_list'")
  }

  # if merge_list is NULL, make a list of all layers
  if (is.null(merge_list)) {
    merge_list <- list(1:nrow(recolorize_obj$centers))
  }

  # repeat params if needed
  if (length(color_to) == 1) {
    color_to <- rep(color_to, length(merge_list))
  }

  return(list(merge_list = merge_list,
              color_to = color_to))

}
