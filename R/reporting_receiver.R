#'reporting_receiver
#'
#'@docType class
#'@importFrom R6 R6Class
#'@export
#'@keywords reporting receiver
#'@return Object of class \code{\link[R6]{R6Class}} for modelling a reporting receiver
#'@format \code{\link[R6]{R6Class}}
#'
#'@author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
reporting_receiver <- R6::R6Class("reporting_receiver",
  inherit = reporting_actor,
  private = list(),
  public = list(
    
    #'@description Initializes a reporting receiver
    #'@param id id
    #'@param name name
    #'@param type type
    initialize = function(id, name, type){
      super$initialize(id, name, type)
    }
  )
)