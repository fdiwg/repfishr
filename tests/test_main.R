testthat::test_that("repfishr",{
  
  my_flow = reporting_flow$new(sender = "TTO", sender_type = "country")

  testthat::expect_is(my_flow$getSender(), "R6")
  testthat::expect_equal(my_flow$getSender()$name, "Trinidad and Tobago")
  testthat::expect_equal(my_flow$getReceiverIds(), c("CRFM", "ICCAT", "WECAFC", "UN-FAO"))
  testthat::expect_is(my_flow$getReceiver("ICCAT"), "R6")
  testthat::expect_equal(my_flow$getReceiver("ICCAT")$name, "International Commission for the Conservation of Atlantic Tunas")
  testthat::expect_equal(my_flow$getReceiver("WECAFC")$getTasks())
  
  
  
  
  data = as.data.frame( readr::read_csv("D:/Documents/CLIENTS/FAO/Projets/WECAFC-FIRMS/data/TTO-dcf-shiny/WECAFC_task_I_2_TTO_1998_2012_simplified_format.csv", guess_max = 0) )
  
  
  output = reporting_flow$new(sender = "TTO", sender_type = "country")$
    getReceiver("WECAFC")$
    getTaskById("task_I.2")$
    process(
      data = data, format = "simplified",
      metadata = data.frame(author = "Fisheries Division", date = Sys.Date()),
      path = "D:/Downloads/test_TTO.xlsx"
    )
  
})