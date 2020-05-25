# recolorize v 0.0

> "As we computer programmers put it, if it doesn't have to work, I can make it run as fast as you want."  
> —*Maciej Cegłowski*

THIS PACKAGE IS STILL A BABY. THIS IS A HALF-THING. 

![](inst/extdata/msc/output.png)
> Original image credit: Nathan P. Lord / Able Chow

## What is this?

Functions for recoloring images based on various color binning schemes. Eventually for use going between patternize, pavo, colordistance, etc. The idea is to simplify the colors of an image according to a metric that is useful for the user.

## How does it work?

You put in a binning scheme, an image, and (optional) a background masking condition. You get out a recolored image and a color palette with (optional) the proportion of pixels assigned to each color. The above image was generated using 10 k-means color clusters in RGB space.

## Contact

Email: [hannahiweller@gmail.com](hannahiweller@gmail.com)
