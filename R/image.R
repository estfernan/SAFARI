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

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Standard morphological operations ----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#' Extract Contours
#'
#' @references Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and Wolfgang Huber (2010):
#'   EBImage - an R package for image processing with applications to cellular phenotypes.
#'   Bioinformatics, 26(7), pp. 979-981, 10.1093/bioinformatics/btq046
#'
#' @keywords internal morphology
#' @noRd
#' @importFrom EBImage ocontour
#'
image.contour <- function(img)
{
    ocontour(img)
}

#' Erosion
#'
#' @references Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and Wolfgang Huber (2010):
#'   EBImage - an R package for image processing with applications to cellular phenotypes.
#'   Bioinformatics, 26(7), pp. 979-981, 10.1093/bioinformatics/btq046
#'
#' @keywords internal morphology
#' @noRd
#' @importFrom EBImage erode
#'
image.erode <- function(img, k)
{
    erode(img, k)
}

#' Fill Holes
#'
#' @references Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and Wolfgang Huber (2010):
#'   EBImage - an R package for image processing with applications to cellular phenotypes.
#'   Bioinformatics, 26(7), pp. 979-981, 10.1093/bioinformatics/btq046
#'
#' @keywords internal morphology
#' @noRd
#' @importFrom EBImage fillHull
#'
image.fill <- function(img)
{
    fillHull(img)
}

#' Opening
#'
#' @references Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and Wolfgang Huber (2010):
#'   EBImage - an R package for image processing with applications to cellular phenotypes.
#'   Bioinformatics, 26(7), pp. 979-981, 10.1093/bioinformatics/btq046
#'
#' @keywords internal morphology
#' @noRd
#' @importFrom EBImage opening
#'
image.opening <- function(img, k)
{
    opening(img, k)
}

#' Segment Regions
#'
#' @references Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and Wolfgang Huber (2010):
#'   EBImage - an R package for image processing with applications to cellular phenotypes.
#'   Bioinformatics, 26(7), pp. 979-981, 10.1093/bioinformatics/btq046
#'
#' @keywords internal
#' @noRd
#' @importFrom EBImage bwlabel
#'
image.segment <- function(img)
{
    bwlabel(img)
}

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Operations relying on morphology ----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#' Fill, Segment, and Filter Regions
#'
#' @keywords internal
#' @noRd
#'
image.fsf <- function(img, min.area, n.largest, minimum)
{
    filter.segments(
        image.segment(
            image.fill(img)
        ), min.area = min.area, n.largest = n.largest, minimum = minimum
    )
}

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Additional image operations ----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#' Expand Image Border
#'
#' @keywords internal
#' @noRd
#'
image.border <- function(image, val, width)
{
    # vertical border
    v <- matrix(val, nrow = nrow(image), ncol = width)

    # add left-right borders
    image <- cbind(v, image, v)

    # horizontal border
    h <- matrix(val, nrow = width, ncol = ncol(image))

    # add top-bottom border
    image <- rbind(h, image, h)

    # end procedure
    return(image)
}

#' Enlarge Image Pixels
#'
#' @keywords internal
#' @noRd
#'
image.enlarge <- function(img, k)
{
    # obtain dimensions of the img
    rows <- dim(img)[1]; cols <- dim(img)[2]

    # create enlarged img matrix
    enlarged <- matrix(0, nrow = k * rows, ncol = k * cols)

    # populate matrix from original img
    for (i in 1:rows)
    {
        for (j in 1:cols)
        {
            for (ii in (k*(i - 1) + 1):(k*i))
            {
                for (jj in (k*(j - 1) + 1):(k*j))
                {
                    enlarged[ii, jj] <- img[i, j];
                }
            }
        }
    }

    enlarged
}

#' Fix Border Issue
#'
#' @keywords internal
#' @noRd
#'
image.sides <- function(image, old, new)
{
    # fix image with full rows of the given value
    image[, colSums(image == old) >= nrow(image)] <- new

    # fix image with full columns of the given value
    image[rowSums(image == old) >= ncol(image) - 2, ] <- new

    # end procedure
    image
}

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Operations on regions within images ----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#' Filter Small Regions
#'
#' @keywords internal
#' @noRd
#'
filter.segments <- function(seg, min.area = NA, n.largest = NA, minimum = 5)
{
    # net area for regions (sorted and w/o zero index)
    area <- sort(table(seg)[-1], decreasing = TRUE)

    # number of regions
    n <- length(area)

    # no regions
    if (n <= 0)
    {
        return(seg)
    }

    # compute minimum net area
    if (is.na(min.area))
    {
        if (!is.na(n.largest))
        {
            min.area <- area[ifelse(n.largest > n, n, n.largest)]

            if (min.area < minimum)
            {
                min.area <- minimum
            }
        }
        else
        {
            min.area <- max(area) / 4
        }
    }

    # regions to keep (logical)
    bool <- area >= min.area

    # re-shape kept segments
    new <- matrix(match(seg, names(area)[bool], nomatch = 0),
                  nrow = nrow(seg),
                  ncol = ncol(seg))

    # store removed regions (add argument: islands = FALSE)
    # if (islands)
    # {
    #     islands <- (seg != 0L) - (new != 0L)
    #     new <- list(seg = new, islands = islands)
    # }

    # end of procedure
    return(new)
}
