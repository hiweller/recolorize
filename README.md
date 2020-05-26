# recolorize v 0.0.0.9000 [![Build Status](https://travis-ci.org/hiweller/recolorize.svg?branch=master)](https://travis-ci.org/hiweller/recolorize)
> "As we computer programmers put it, if it doesn't have to work, I can make it run as fast as you want."  
> —*Maciej Cegłowski*

A tentatively working R package for simplifying and remapping colors.

![](inst/extdata/msc/output.png)
> Original image credit: Nathan P. Lord / Able Chow

## What is this?

Functions for recoloring images based on various color binning schemes. Eventually for use going between patternize, pavo, colordistance, etc. The idea is to simplify the colors of an image according to a metric that is useful for the user.

## Quick start

To generate the image above:
```{r}
devtools::install_github("hiweller/recolorize")

corbetti <- system.file("extdata/corbetti.png", package = "recolorize")

recolorize::recolorize(corbetti, method = "kmeans", n = 10)
```
Vignettes and better documentation coming soon.

## How does it work?

You put in a binning scheme, an image, and (optional) a background masking condition. You get out a recolored image and a color palette with (optional) the proportion of pixels assigned to each color. The above image was generated using 10 k-means color clusters in RGB space.

## Contact

Email: [hannahiweller@gmail.com](hannahiweller@gmail.com)
