.onLoad <- function (libname, pkgname) {
  
  assign(".repfishr", new.env(), envir= asNamespace(pkgname))
  
  .repfishr$reporting_flow_actors = readr::read_csv(system.file("extdata", "reporting_flow_actors.csv", package = "repfishr"))
  
}