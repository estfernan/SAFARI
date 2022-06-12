# ******************************************************************************
#
# R package SAFARI by Esteban Fernández and Qiwei Li
# Copyright (C) 2021
#
# This file is part of the R package SAFARI.
#
# The R package SAFARI is free software: You can redistribute it and/or
# modify it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or any later
# version (at your option). See the GNU General Public License at
# <https://www.gnu.org/licenses/> for details.
#
# The R package SAFARI is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# ******************************************************************************

#' Read a Binary Image
#'
#' This function reads a reconstructed binary image, possibly, produced from an
#' X-Ray, CT scan, MRI, etc. and processed by standard image processing
#' algorithms. The image is then preprocessed to facilitate the procedure in
#' the \code{\link{binary.segmentation}} function.
#'
#' The binary image is preprocessed as follows:
#' \itemize{
#'     \item Check if image contains multiple color channels, non-binary, or empty.
#'     \item Invert black and white colors (optional).
#'     \item Ensure image is truly binary by converting all values greater than zero to \code{1}.
#'     \item Expand the border around the image with \code{0}s.
#'     \item Rotating the image, if it is not stored as an \code{RData} file.
#' }
#'
#' @param file a character string that specifies the image to read.
#' @param invert a logical value that indicates if the B/W colors should be inverted.
#'
#' @return An integer matrix that represents the preprocessed image.
#'
#' @seealso
#' \code{\link{binary.segmentation}} for more information on reconstructed binary images.
#'
#' @export
#' @importFrom caTools read.gif
#' @importFrom png readPNG
#' @importFrom tools file_ext
#'
read.image <- function(file, invert = FALSE)
{
    # error message for loading image
    msg <- "image in 'file' could not be read : extension not supported"

    # attempt to load image, base on file extension
    ext <- tolower(file_ext(file))
    img <- switch(ext,
                  gif = read.gif(file)$image,
                  png = readPNG(file),
                  rdata = get(load(file)),
                  stop(msg))

    # number of channels
    n <- dim(img)[3]

    # multiple channels check
    if (!is.na(n))
    {
        if (n == 2)
        {
            img <- img[,,1]
        }
        else
        {
            img <- img[,,2]
        }
    }

    # binary image check - change all non-zero values in image matrix to 1
    if (!is.binary(img))
    {
        img[img != 0] <- 1
    }

    # empty matrix check
    if (is.empty(img))
    {
        stop("image in 'file' appears to be empty")
    }

    # invert black and white
    if (invert)
    {
        img <- !img
    }

    # image preprocessing
    img[img > 0] <- 1              # ensure image is binary
    img <- image.border(img, 0, 5) # expand image border

    # rotate image
    if (ext != "rdata")
    {
        img <- t(img)[, nrow(img):1]
    }

    # modify to integer
    mode(img) <- "integer"

    # end of procedure
    return(img)
}
