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
    #'@field receiver receiver
    receiver = NA,
    #'@field id id
    id = NA,
    #'@field name name
    name = NA,
    #'@field measurement measurement
    measurement = NA,
    #'@field formats formats
    formats = list(),
    #'@field report_fun report handler (function)
    report_fun = NULL,
    
    #'@description Initializes a reporting task
    #'@param receiver receiver
    #'@param file file
    initialize = function(receiver, file){
      task = switch(mime::guess_type(file),
        "text/yaml" = yaml::read_yaml(file),
        "application/json" = jsonlite::read_json(file),
        NULL
      )
      if(!is.null(task)){
        self$receiver = receiver
        self$id = task$id
        self$name = task$name
        self$formats = lapply(task$formats, function(x){
          reporting_format$new(id = x$id, name = x$name, ref = x$ref)
        })
        names(self$formats) = names(task$formats)
        if(!is.null(task$report$handler)){
          report_fun = source(system.file("extdata/specs", self$receiver, task$report$handler, package = "repfishr"))$value
          if(!all(names(formals(report_fun)) == c("data","metadata","path"))){
            stop("The report handler should be standardized with the following arguments: [data, metadata, path]")
          }
          self$report_fun = report_fun
        }
      }
    },
    
    #'@description Process data
    #'@param data object of class \link{data.frame}
    #'@param format format id for data validation
    #'@param parallel whether data validation should be run in parallel
    #'@param metadata metadata object
    #'@param path path for the output file
    #'@param ... any other arguments to be passed to \pkg{vrule} validation method
    process = function(data, format, parallel = FALSE, 
                       metadata, path, ...){
      if(!format %in% names(self$formats)){
        
        errMsg = sprintf("Format '%s' is not among available formats for this task: [%s]", 
                         format, paste0(names(self$formats), collapse = ","))
        ERROR(errMsg)
        stop(errMsg)
      }
      
      #validation
      INFO("Data validation")
      validation_output = self$formats[[format]]$spec$validate(data = data, parallel = parallel, ...)
      if(nrow(validation_output)>0 & any(validation_output$type == "ERROR")){
        errMsg = "Errors were detected during validation phase, reporting is aborted"
        ERROR(errMsg)
        return(validation_output)
      }
      
      #reporting (if any)
      if(!is.null(self$report_fun)){
        INFO("Data reporting")
        out = self$report_fun(
          data = data,
          metadata = metadata,
          path = path
        )
      }else{
        WARN(sprintf("No report associated to task '%s'. Data will be reported in its original form", self$id))
        out = readr::write_csv(data, path)
      }
      return(out)      
    }
  )
)