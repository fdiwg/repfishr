testthat::test_that("repfishr",{
  
  my_flow = reporting_flow$new(sender = "TTO", sender_type = "country")

  testthat::expect_is(my_flow$getSender(), "R6")
  testthat::expect_equal(my_flow$getSender()$name, "Trinidad and Tobago")
  testthat::expect_equal(my_flow$getReceiverIds(), c("CRFM", "ICCAT", "WECAFC", "UN-FAO"))
  testthat::expect_is(my_flow$getReceiver("ICCAT"), "R6")
  testthat::expect_equal(my_flow$getReceiver("ICCAT")$name, "International Commission for the Conservation of Atlantic Tunas")
  testthat::expect_equal(my_flow$getReceiver("WECAFC")$getTasks())
  
  
})