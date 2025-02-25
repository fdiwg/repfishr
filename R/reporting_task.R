#'reporting_task
#'
#'@docType class
#'@importFrom R6 R6Class
#'@export
#'@keywords reporting task
#'@return Object of class \code{\link[R6]{R6Class}} for modelling a reporting task
#'@format \code{\link[R6]{R6Class}}
#'
#'@author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
reporting_task <- R6::R6Class("reporting_task",
  private = list(),
  public = list(
    #'@field id id
    id = NA,
    #'@field name name
    name = NA,
    #'@field measurement measurement
    measurement = NA,
    #'@field formats formats
    formats = list(),
    
    #'@description Initializes a reporting task
    #'@param file file
    initialize = function(file){
      task = switch(mime::guess_type(file),
        "text/yaml" = yaml::read_yaml(file),
        "application/json" = jsonlite::read_json(file),
        NULL
      )
      if(!is.null(task)){
        self$id = task$id
        self$name = task$name
        self$formats = lapply(task$formats, function(x){
          reporting_format$new(id = x$id, name = x$name, ref = x$ref)
        })
        names(self$formats) = names(task$formats)
      }
    }
  )
)