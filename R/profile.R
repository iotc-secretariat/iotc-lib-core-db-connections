.onLoad <- function (libname, pkgname) { # nocov start
  assign(".dbi.options", new.env(), envir= asNamespace(pkgname))
  setDefaultDBIHandler(DB_IOTDB)
}
