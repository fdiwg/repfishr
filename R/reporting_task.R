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
    #'@field sender sender
    sender = NULL,
    #'@field receiver receiver
    receiver = NA,
    #'@field id id
    id = NA,
    #'@field name name
    name = NA,
    #'@field context context
    context = NA,
    #'@field measurement measurement
    measurement = NA,
    #'@field formats formats
    formats = list(),
    #'@field process_fun process handler (function)
    process_fun = NULL,
    #'@field report_fun report handler (function)
    report_fun = NULL,
    #'@field report_data data to report
    report_data = NULL,
    #'@field report_metadata metadata to report
    report_metadata = NULL,
    
    #'@description Initializes a reporting task
    #'@param receiver receiver
    #'@param file file
    #'@param task task (as list object)
    initialize = function(receiver = NULL, file = NULL, task = NULL){
      if(is.null(task)) if(!is.null(file)){
        task = switch(mime::guess_type(file),
          "application/yaml" = yaml::read_yaml(file),
          "application/json" = jsonlite::read_json(file),
          NULL
        )
      }
      if(!is.null(task)){
        self$receiver = receiver
        self$id = task$id
        self$name = task$name
        self$context = task$context
        self$measurement = task$measurement
        self$formats = lapply(task$formats, function(x){
          reporting_format$new(id = x$id, name = x$name, ref = x$ref)
        })
        names(self$formats) = names(task$formats)
        if(!is.null(receiver)){
          #process handler
          if(!is.null(task$process$handler)){
            process_fun = source(system.file("extdata/specs", self$receiver, "handlers", task$process$handler, package = "repfishr"))$value
            if(!all(names(formals(process_fun)) == c("sender", "data", "metadata"))){
              stop("The process handler should be standardized with the following arguments: [sender, data, metadata]")
            }
            self$process_fun = process_fun
          }
          #report handler
          if(!is.null(task$report$handler)){
            report_fun = source(system.file("extdata/specs", self$receiver, "handlers", task$report$handler, package = "repfishr"))$value
            if(!all(names(formals(report_fun)) == c("sender", "data", "metadata", "path"))){
              stop("The report handler should be standardized with the following arguments: [sender, data, metadata, path]")
            }
            self$report_fun = report_fun
          }
        }
      }
    },
    
    #'@description Set sender
    #'@param sender sender object of class \link{reporting_sender}
    setSender = function(sender){
      self$sender = sender
    },
    
    #'@description Process data before reporting
    #'@param data object of class \link{data.frame}
    #'@param metadata metadata object
    #'@param path path for the output file
    #'@param parallel whether data validation should be run in parallel
    #'@param ... any other arguments to be passed to \pkg{vrule} validation method
    process = function(data, metadata, path, parallel = FALSE, ...){
      
      #pre-processing (if any) before reporting
      if(!is.null(self$process_fun)){
        
        #validation before processing with format 'process' if needed
        if("process" %in% names(self$formats)){
          INFO("Data validation before processing")
          validation_output = self$formats[["process"]]$spec$validate(data = data, parallel = parallel, ...)
          if(nrow(validation_output)>0 & any(validation_output$type == "ERROR")){
            errMsg = "Errors were detected during validation phase, reporting is aborted"
            ERROR(errMsg)
            return(validation_output)
          }else{
            INFO("Data validation before processing sucessful")
          }
        }
        
        INFO("Processing data before reporting")
        report_data = self$process_fun(
          sender = self$sender,
          data = data,
          metadata = metadata
        )
        self$report_data = report_data
        self$report_metadata = attr(report_data, "metadata")
      }else{
        self$report_data = data
        self$report_metadata = metadata
      }
    },
    
    #'@description Reports data
    #'@param data object of class \link{data.frame}
    #'@param metadata metadata object
    #'@param path path for the output file
    #'@param parallel whether data validation should be run in parallel
    #'@param ... any other arguments to be passed to \pkg{vrule} validation method
    report = function(data, metadata, path, parallel = FALSE, ...){
      
      #pre-processing
      self$process(
        data = data,
        metadata = metadata,
        path = path,
        parallel = parallel,
        ...
      )
      
      #reporting (if any)
      if(!is.null(self$report_fun)){
        
        #validation before reporting with format 'report' if needed
        if("report" %in% names(self$formats)){
          INFO("Data validation before reporting")
          validation_output = self$formats[["report"]]$spec$validate(data = self$report_data, parallel = parallel, ...)
          if(nrow(validation_output)>0 & any(validation_output$type == "ERROR")){
            errMsg = "Errors were detected during validation phase, reporting is aborted"
            ERROR(errMsg)
            return(validation_output)
          }else{
            INFO("Data validation before reporting sucessful")
          }
        }
        
        INFO("Data reporting")
        out = self$report_fun(
          sender = self$sender,
          data = self$report_data,
          metadata = self$report_metadata,
          path = path
        )
      }else{
        WARN(sprintf("No report associated to task '%s'. Data will be reported in its original form", self$id))
        readr::write_csv(self$report_data, path)
        out = path
      }
      return(out)      
    }
  )
)