## Test environments

* local macOS 15.3.1 install, R version 4.4.3 (2025-02-28)
* GitHub Actions (macos-latest release, windows-latest release, ubuntu-latest devel, release, and oldrel-1)
* win-builder (release and oldrelease)


## R CMD check results

* local check: 0 errors | 0 warnings | 1 note
* GitHub Actions: 0 errors | 0 warnings | 0 notes
* win-builder: 0 errors | 0 warnings | 0 notes

- checking installed package size ... NOTE
    installed size is 15.3Mb
    sub-directories of 1Mb or more:
      doc       2.9Mb
      extdata   1.1Mb
      help     11.2Mb

This is an image-processing package so the vignettes include several images, hence the larger directory size. 

## Downstream dependencies

There are currently no downstream dependencies for this package.

# Examples

Four examples are not run with R CMD check (\donttest): recolorizeVector, absorbLayer, recolorize_to_patternize, and recolorize_to_png. The first three are not run because they take longer than 10 seconds to evaluate (they involve relatively complex image processing and/or conversion). recolorize_to_png is not run because it writes (and deletes) a file to the current working directory.
