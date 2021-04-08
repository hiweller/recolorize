#' Convert a recolorize object to a vector
#'
#' Converts a `recolorize` color map to a set of polygons, which
#' can be plotted at any scale without losing quality (as opposed to
#' the pixel-based bitmap format). Requires the `raster`, `rgeos`, and
#' `sp` packages to be installed. Useful for creating nice visualizations;
#' slow on large images. It's recommended to fit a `recolorize` object
#' by reducing the original image first, rather than the `resize` argument
#' here, which reduces the color map itself (to mixed results).
#'
#' @param recolorize_obj An object of class `recolorize`, as generated
#'   by [recolorize], [recolorize2], [imposeColors], or [wernerColor].
#' @param smoothness Passed to [smoothr::smooth] using the `"ksmooth"`
#'   method for smoothing the jagged lines that result from converting
#'   pixel coordinates to polygon vertices. Higher values = more smoothing.
#' @param base_color The color to use to fill in the gaps that can result from
#'   smoothing. If `base_color = "default"`, defaults to the darkest color
#'   in the palette. Otherwise, should be the numeric index of one of the colors
#'   in `recolorize_obj$centers` to use.
#' @param size_filter The size (as a proportion of the shortest dimension of the
#'   image) of the color patch elements to absorb before vectorizing. Small
#'   details (e.g. stray pixels) tend to look very strange after vectorizing,
#'   so removing these beforehand can improve results.
#' @param plotting Logical. Plot results while computing?
#' @param resize Proportion by which to resize the color map before turning
#'   into a polygon, e.g. `resize = 0.5` will reduce color map size by 50%.
#'   Speeds up the function, but you will almost always get better results
#'   by resizing the initial image when fitting the `recolorize` object.
#' @param ... Plotting parameters, passed on to [graphics::plot].
#'
#' @return
#' A `vector_recolorize` object, which is a list with the following
#' elements:
#' \enumerate{
#'   \item `base_layer`: The base polygon, essentially the image silhouette.
#'   \item `layers`: A list of [sp::SpatialPolygonsDataFrame] polygons, one per
#'   color patch.
#'   \item `layer_colors`: The colors (as hex codes) for each polygon.
#'   \item `base_color`: The color (as hex code) for the base polygon.
#'   \item `asp`: The original image aspect ratio, important for plotting.
#' }
#'
#' @details
#' Although vector objects will typically be smaller than `recolorize` objects,
#' because they only need to specify the XY coordinates of the perimeters of
#' each polygon, they can still be fairly large (and take a long time to
#' calculate). Users can try a few things to speed this up: using lower
#' smoothness values; setting `plotting = FALSE`; resizing the image
#' (preferably when fitting the initial `recolorize` object); and
#' reducing the complexity of the color patches using [absorbLayer] or
#' [editLayer] (e.g. by absorbing all components < 10 pixels in size). Still,
#' expect this function to take several minutes on even moderately sized
#' images--it takes about 7-10 seconds for the ~200x100 pixel images in the
#' examples! Once the function finishes running, however, plotting is
#' quite fast, and the objects themselves are smaller than the  `recolorize`
#' objects.
#'
#' @examples
#' \dontrun{
#' img <- system.file("extdata/corbetti.png", package = "recolorize")
#' rc <- recolorize2(img, cutoff = 45)
#'
#' # takes ~10 seconds
#' as_vector <- recolorizeVector(rc, smoothness = 5,
#'                               size_filter = 0.05)
#'
#' # to save as an SVG with a transparent background and
#' # no margins (e.g. for an illustration figure):
#' grDevices::svg("recolorize_vector.svg",
#' height = 4, width = 2, bg = "transparent")
#' par(mar = rep(0, 4))
#' plot(as_vector)
#' dev.off()
#' }
#' @export
recolorizeVector <- function(recolorize_obj,
                             size_filter = 0.1,
                              smoothness = 1,
                              base_color = "default",
                              plotting = FALSE,
                              resize = 1,
                              ...) {

  size_filter <- min(round(dim(recolorize_obj$original_img) * size_filter))

  rc <- recolorize_obj

  if (size_filter > 0) {
    for (i in 1:nrow(recolorize_obj$centers)) {
      recolorize_obj <- absorbLayer(recolorize_obj, i,
                                    function(s) s <= size_filter,
                                    plotting = FALSE)
    }
  }


  # resize pixel assignments
  im <- imager::as.cimg(recolorize_obj$pixel_assignments)
  im <- round(imager::imresize(im, scale = resize,
                               interpolation = 3))
  recolorize_obj$pixel_assignments <- cimg_to_array(im)

  # get layers
  layers <- splitByColor(recolorize_obj,
                         plot_method = "none")

  # get base color - default to darkest color
  if (base_color == "default") {
    lab_cols <- col2col(recolorize_obj$centers, to = "Lab")
    base_color <- which.min(lab_cols[ , 1])
  }

  # convert to hex
  base_color <- ifelse(base_color <= nrow(recolorize_obj$centers),
                       yes = grDevices::rgb(recolorize_obj$centers)[base_color],
                       no = stop("`base_color` must be the numeric index of the layer to use
                   as the base color"))

  # create base layer
  base_layer <- recolorize_obj$pixel_assignments
  base_layer[which(base_layer > 0)] <- 1

  # get polygon
  b <- raster::rasterToPolygons(raster::raster(base_layer),
                                fun = function(x) x > 0,
                                dissolve = TRUE)

  # smooth it
  b <- smoothr::smooth(b, "ksmooth", smoothness = smoothness)

  # initialize polygon list
  p <- list(base_layer = b,
            layers = vector("list", length(nrow(recolorize_obj$centers))),
            layer_colors = grDevices::rgb(recolorize_obj$centers),
            base_color = base_color,
            asp = dim(base_layer)[1] / dim(base_layer)[2])
  class(p) <- "recolorizeVector"

  # vectorize each layer
  for (i in 1:length(layers)) {

    # get polygons
    pol <- raster::rasterToPolygons(raster::raster(layers[[i]]),
                                    fun = function(x) x > 0, dissolve = TRUE)

    # smooth them
    pol <- smoothr::smooth(pol, "ksmooth", smoothness = smoothness)

    # add to list
    p$layers[[i]] <- pol
  }

  # if plotting, put base layer first and then add the rest
  if (plotting) {

    # initialize plot
    sp::plot(b, col = base_color, border = p$base_color,
         asp = dim(layers[[i]])[1] / dim(layers[[i]])[2],
         xlim = c(0, 1), ylim = c(0, 1),
         ...)

    for (i in 1:length(p$layers)) {
      # rgb colors
      col <- p$layer_colors[i]

      # add to plot
      sp::plot(p$layers[[i]],
           col = col,
           border = col,
           add = TRUE,
           ...)
    }
  }

  return(p)
}

#' Plot a `recolorizeVector` object
#'
#' Plots an object generated by [recolorizeVector].
#'
#' @param x Object returned by [recolorizeVector].
#' @param ... Further arguments passed to [graphics::plot].
#'
#' @rdname plot.recolorizeVector
#' @export
plot.recolorizeVector <- function(x, ...) {

  requireNamespace("sp", quietly = TRUE)

  # initialize plot
  sp::plot(x$base_layer, col = x$base_color,
       border = x$base_color,
       asp = x$asp,
       ...)

  for (i in 1:length(x$layers)) {
    # rgb colors
    col <- x$layer_colors[i]

    # add to plot
    sp::plot(x$layers[[i]],
         col = col,
         border = col,
         add = TRUE,
         ...)
  }
}
