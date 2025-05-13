#'reporting_actor
#'
#'@docType class
#'@importFrom R6 R6Class
#'@export
#'@keywords reporting actor
#'@return Object of class \code{\link[R6]{R6Class}} for modelling a reporting actor
#'@format \code{\link[R6]{R6Class}}
#'
#'@author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
reporting_actor <- R6::R6Class("reporting_actor",
  private = list(),
  public = list(
    #'@field id id
    id = NA,
    #'@field name name,
    name = NA,
    #'@field type type
    type = NA,
    
    #'@description Initializes a reporting sender
    #'@param id id
    #'@param name name
    #'@param type type
    initialize = function(id, name = NULL, type = NULL){
      self$id = id
      self$name = name
      self$type = type
    }
  )
)