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

#' Reconstructed Binary Pathology Slide
#'
#' This reconstructed binary image was first represented as a three-class image,
#' prepared using a tumor recognition system (ConvPath) developed by the
#' Quantitative Biomedical Research Center. The original whole-slide image
#' comes from a lung cancer patient in the Lung Screening Study (LSS)
#' subcomponent of NLST. Specifically, this is an image from an H&E-stained
#' slide that was obtained as part of a pathology specimen collection.
#'
#' @name rBPS
#' @aliases rBPS
#' @docType data
#' @format \code{rBPS} is a 314-by-224 binary matrix where each entry
#' corresponds to a tissue or region in the H&E image. In our case the ones and
#' zeros indicate an empty region or tumor tissue, respectively.
#'
#' @references ConvPath: A software tool for lung adenocarcinoma digital
#' pathological image analysis aided by a convolutional neural network. (2019)
#' EBioMedicine.
#'
#' @source Original H&E slide available at
#' \url{https://biometry.nci.nih.gov/cdas/nlst/}.
#'
#' @keywords dataset
#' @usage data(rBPS)
#'
NULL
