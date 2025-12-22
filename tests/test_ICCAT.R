# testthat::test_that("repfishr for ICCAT - reporting part",{
#   
#   my_flow = reporting_flow$new(sender = "GRD", sender_type = "country")
#   
#   testthat::expect_is(my_flow$getSender(), "R6")
#   testthat::expect_equal(my_flow$getSender()$name, "Grenada")
#   testthat::expect_equal(my_flow$getReceiverIds(), c("CRFM", "ICCAT", "IWC", "WECAFC", "UN-FAO"))
#   testthat::expect_is(my_flow$getReceiver("ICCAT"), "R6")
#   testthat::expect_equal(my_flow$getReceiver("ICCAT")$name, "International Commission for the Conservation of Atlantic Tunas")
#   testthat::expect_equal(length(my_flow$getReceiver("ICCAT")$getTasks()), 1L)
#   testthat::expect_equal(my_flow$getReceiver("ICCAT")$sender$id, "GRD") #test propagation of sender
#   
#   
#   data = readr::read_csv("./sandbox/iccat_grd_t1nc.csv")
#   data[1L,]$gear_type = "Not trolling, this is a hobbit troll"
#   
#   task_def = reporting_flow$new(sender = "GRD", sender_type = "country")$
#     getReceiver("ICCAT")$
#     getTaskDefinitionById("iccat_task_t1nc")
#   
#   spec = task_def$formats$generic$spec
#   spec$validate_and_display_as_handsontable(data)
#   
#   output = reporting_flow$new(sender = "GRD", sender_type = "country")$
#     getReceiver("ICCAT")$
#     getTaskDefinitionById("iccat_task_t1nc")$
#     process(
#       data = data, format = "generic",
#       metadata = list(
#         fullname = "GRD Data clerk",
#         country = "Grenada",
#         from = min(data$year), to = max(data$year)
#       ),
#       path = ".sandbox/test_GRD.xlsx"
#     )
# })

testthat::test_that("repfishr for ICCAT - processing part",{
  
  if(!dir.exists("sandbox")) dir.create("sandbox")
  
  data = readr::read_csv(system.file("extdata", "sample/fdi_test_dataset.csv", package = "repfishr"))
  metadata = list()
  
  task = reporting_flow$new(sender = "GRD", sender_type = "country")$
    getReceiver("ICCAT")$
    getTaskDefinitionById("iccat_task_t1nc")
  
  task$process(data, metadata, path = NULL, parallel = FALSE)
  testthat::expect_is(task$report_data, "data.frame")
  testthat::expect_is(task$report_metadata, "list")
  
  task$report(data, metadata, path = "sandbox/test_iccat_task_t1nc.xlsx")
  
})