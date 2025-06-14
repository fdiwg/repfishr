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
    initialize = function(id, name = NULL, type = NULL){
      super$initialize(id, name = name, type = type)
    },
    
    #'@description Get task IDs
    #'@return the list of task IDs
    getTasks = function(){
      task_specs = list.files(system.file("extdata/specs", self$id, "tasks", package = "repfishr"), pattern = ".yml", full.names = T)
      tasks = sapply(task_specs, function(x){
        task = yaml::read_yaml(x)
        task$id
      })
      names(tasks) = NULL
      return(tasks)
    },
    
    #'@description Get tasks
    #'@param raw raw
    #'@return a list of \link{reporting_task}
    getTaskDefinitions = function(raw = FALSE){
      if(length(self$tasks)==0){
        task_specs = list.files(system.file("extdata/specs", self$id, "tasks", package = "repfishr"), pattern = ".yml", full.names = T)
        if(length(task_specs)>0){
          if(raw){
            self$tasks = lapply(task_specs, yaml::read_yaml)
          }else{
            self$tasks = lapply(task_specs, function(file){
              reporting_task$new(receiver = self$id, file = file)
            })
          }
        }
      }
      return(self$tasks)
    },
    
    #'@description Get task definition by ID
    #'@param id id
    #'@return an object of class \link{reporting_task}
    getTaskDefinitionById = function(id){
      task = NULL
      if(length(self$tasks)>0){
        task = self$tasks[sapply(self$tasks, function(x){x$id == id})][[1]]
      }else{
        task_spec = system.file("extdata/specs", self$id, "tasks", paste0(id, ".yml"), package = "repfishr")
        if(task_spec == "") stop("It seems the task YML file is not named with its ID!")
        task = reporting_task$new(receiver = self$id, file = task_spec)
      }
      return(task)
    }
  )
)