
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SAFARI

<!-- badges: start -->
<!-- badges: end -->

The R package **SAFARI** (**S**hape **A**nalysis **f**or
**A**I-Reconstructed **I**mages) provides functions for reading,
preprocessing, segmenting ROI, and extracting shape-based features in
AI-reconstructed images.

## Description

**SAFARI** is an R package that consists of a collection of functions
for image processing and shape analysis. The available functions allow
users to segment regions of interest and extract quantitative shape
descriptors in AI-reconstructed images, especially those produced from
medical imaging modalities.

## Development

The latest version of the package is under development at
[GitHub](https://github.com/estfernandez/SAFARI). One may install it by

``` r
## install.packages("devtools")
devtools::install_github("estfernandez/SAFARI")
```

## Dependencies

This package requires the use of `EBImage` from the
[Bioconductor](https://bioconductor.org/packages/release/bioc/html/EBImage.html)
project. One must install this package by

``` r
## install.packages("BiocManager")
BiocManager::install("EBImage")
```

## Get Started

Examples are provided for the main functions. One may get started from
those examples and the function documentation. We also provide a
command-line tool for applications in the terminal.

``` r
library(SAFARI)
?read.image           # read and preprocess reconstructed image
?binary.segmentation  # segmentation procedure
?compute.features     # feature extraction for an individual ROI
?rc.plot              # visualize binary or segmented images
```

## License

[GNU General Public License](https://www.gnu.org/licenses/) (≥ 3)
