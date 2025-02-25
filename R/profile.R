.onLoad <- function (libname, pkgname) {
  
  assign(".repfishr", new.env(), envir= asNamespace(pkgname))
  
}