#'reporting_sender
#'
#'@docType class
#'@importFrom R6 R6Class
#'@export
#'@keywords reporting sender
#'@return Object of class \code{\link[R6]{R6Class}} for modelling a reporting sender
#'@format \code{\link[R6]{R6Class}}
#'
#'@author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
reporting_sender <- R6::R6Class("reporting_sender",
  inherit = reporting_actor,
  private = list(),
  public = list(
    
    #'@description Initializes a reporting sender
    #'@param id id
    #'@param name name
    #'@param type type
    initialize = function(id, name, type){
      super$initialize(id, name, type)
    }
  )
)