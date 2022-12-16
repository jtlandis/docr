
box::use(
  R7[`@`, `method<-`, R7_dispatch]
)

#' @export
box::use(./format[...])

# --- adding usage classes ----

method(`+`, list(usage, usage)) <- function(x, y) {
  usage(!!!x@children, !!!y@children)
}
method(`+`, list(usage, fields)) <- function(x, y) {
  usage(!!!x@children, y)
}
method(`+`, list(fields, usage)) <- function(x, y) {
  usage(x, !!!y@children)
}
method(`+`, list(fields, fields)) <- function(x, y) {
  usage(x, y)
}


method(`+`, list(qualifier, usage)) <- function(x, y) {
  usage(x, !!!y@children)
}
method(`+`, list(usage, qualifier)) <- function(x, y) {
  usage(!!!x@children, y)
}
method(`+`, list(qualifier, qualifier)) <- function(x, y) {
  usage(x, y)
}
method(`+`, list(req, req)) <- function(x, y) {
  req(!!!x@children, !!!y@children)
}
method(`+`, list(opt, opt)) <- function(x, y) {
  opt(!!!x@children, !!!y@children)
}


method(`+`, list(qualifier, fields)) <- function(x, y) {
  usage(x, y)
}
method(`+`, list(fields, qualifier)) <- function(x, y) {
  usage(x, y)
}






method(`|`, list(usage, usage)) <- function(x, y) {
  exclsv(x, y)
}
method(`|`, list(exclsv, usage)) <- function(x, y) {
  exclsv(!!!x@children, y)
}
method(`|`, list(usage, exclsv)) <- function(x, y) {
  exclsv(x, !!!y@children)
}
