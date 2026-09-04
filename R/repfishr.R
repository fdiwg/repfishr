#' Fisheries data reporting flows
#'
#' R framework to facilitate fisheries data reporting flows, with first intention to help countries to validate, 
#' format and send data to relevant organizations including regional fisheries management organizations or bodies handling #' data collection reference frameworks, and FAO.
#' 
#' @import R6
#' @import methods
#' @import tibble
#' @rawNamespace import(sf, except = c(st_perimeter, st_minimum_bounding_circle))
#' @rawNamespace import(terra, except = c(extract,intersect,inset,origin,union))
#' @import magrittr
#' @import jsonlite
#' @import yaml
#' @import readr
#' @import mime
#' @import lubridate
#' @import writexl
#' @import openxlsx
#' @import vrule
#' @import fdi4R
#' @import fdisfdata
#' 
#' @name repfishr
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' 
"_PACKAGE"
