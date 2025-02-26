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
    
    #'@field tasks tasks
    tasks = list(),
    
    #'@description Initializes a reporting receiver
    #'@param id id
    #'@param name name
    #'@param type type
    initialize = function(id, name, type){
      super$initialize(id, name, type)
    },
    
    #'@description Get tasks
    #'@return a list of \link{reporting_task}
    getTasks = function(){
      if(length(self$tasks)==0){
        task_specs = list.files(system.file("extdata/specs", self$id, package = "repfishr"), pattern = ".yml", full.names = T)
        if(length(task_specs)>0){
          self$tasks = lapply(task_specs, function(file){
            reporting_task$new(receiver = self$id, file = file)
          })
        }
      }
      return(self$tasks)
    },
    
    #'@description Get task by ID
    #'@param id id
    #'@return an object of class \link{reporting_task}
    getTaskById = function(id){
      task = NULL
      if(length(self$tasks)>0){
        task = self$tasks[sapply(self$tasks, function(x){x$id == id})][[1]]
      }else{
        task_spec = system.file("extdata/specs", self$id, paste0(id, ".yml"), package = "repfishr")
        if(task_spec == "") stop("It seems the task YML file is not named with its ID!")
        task = reporting_task$new(receiver = self$id, file = task_spec)
      }
      return(task)
    }
  )
)