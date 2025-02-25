#'reporting_format
#'
#'@docType class
#'@importFrom R6 R6Class
#'@export
#'@keywords reporting format
#'@return Object of class \code{\link[R6]{R6Class}} for modelling a reporting format
#'@format \code{\link[R6]{R6Class}}
#'
#'@author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
reporting_format <- R6::R6Class("reporting_format",
  private = list(),
  public = list(
    #'@field id id
    id = NA,
    #'@field name name
    name = NA,
    #'@field ref ref format specification link
    ref = NA,
    #'@field spec format specification object of class \link[vrule]{format_spec}
    spec = NA,
    
    #'@description Initializes a reporting task
    #'@param id id
    #'@param name name
    #'@param ref ref
    initialize = function(id, name, ref){
      self$id = id
      self$name = name
      self$ref = ref
      self$spec = vrule::format_spec$new(json = jsonlite::read_json(ref))
    }
  )
)