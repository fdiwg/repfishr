require(repfishr, quietly = TRUE)
require(testthat)

testthat::test_that("repfishr for ICCAT",{
  
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