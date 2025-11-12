#'reporting_flow
#'
#'@docType class
#'@importFrom R6 R6Class
#'@export
#'@keywords reporting flow
#'@return Object of class \code{\link[R6]{R6Class}} for modelling a reporting flow
#'@format \code{\link[R6]{R6Class}}
#'
#'@author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
reporting_flow <- R6::R6Class("reporting_flow",
  private = list(
    actors = NULL
  ),
  public = list(
    #'@field sender sender object of class \link{reporting_sender}
    sender = NULL,
    
    #'@description Initializes a reporting flow
    #'@param sender sender id
    #'@param sender_type type of sender
    initialize = function(sender, sender_type){
      rep_flow_actors = get_reporting_flow_actors()
      rep_flow_receivers = rep_flow_actors[rep_flow_actors$sender_type == sender_type &
                                           rep_flow_actors$sender_id == sender,]
      if(nrow(rep_flow_receivers)==0){
        errMsg = sprintf("No available reporting flow of for sender '%s' (type = '%s')",
                         sender, sender_type)
        ERROR(errMsg)
        stop(errMsg)
      }
      self$sender = reporting_sender$new(
        id = sender,
        name = rep_flow_receivers[1,]$sender_name,
        type = sender_type
      )
      private$actors = rep_flow_receivers
    },
    
    #'@description Get sender
    #'@return an object of class \link{reporting_sender} 
    getSender = function(){
      return(self$sender)
    },
    
    #'@description Get list of valid receivers for the selected sender
    #'@param raw raw
    #'@return an object of class \link{data.frame} or a \code{list} of \link{reporting_receiver}
    getReceivers = function(raw = FALSE){
      receivers = private$actors[,c("receiver_id", "receiver_name", "receiver_type")]
      if(raw){
        receivers = lapply(1:nrow(private$actors), function(i){
          actor = private$actors[i,]
          rec = reporting_receiver$new(
            id = actor$receiver_id,
            name = actor$receiver_name,
            type = actor$receiver_type
          )
          rec$setSender(self$sender)
          return(rec)
        })
      }
      return(receivers)
    },
    
    #'@description Get list of valid receivers IDs
    getReceiverIds = function(){
      return(private$actors$receiver_id)
    },
    
    #'@description Get a receiver by its id
    #'@param id id
    #'@return an object of class \link{reporting_organization}
    getReceiver = function(id){
      receiver = NULL
      receivers = self$getReceivers(raw = T)
      receivers = receivers[sapply(receivers, function(x){x$id == id})]
      if(length(receivers)>0) receiver = receivers[[1]]
      if(length(receivers)==0){
        WARN(sprintf("No possible receiver '%s' for sender '%s'", id, sender))
      }
      #propagate the sender
      receiver$setSender(self$sender)
      
      return(receiver)
    }
     
  )
)