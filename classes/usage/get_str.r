
box::use(
  R7[new_generic, R7_dispatch, `@`, class_any, `method<-`],
  ./usage[usage, fields]
)

#' @export
get_str <- new_generic("get_str", "x", function(x) R7_dispatch())

method(get_str, fields) <- function(x) {
  str <- gsub("^-{0,2}", "", x@str)
  str[which.max(nchar(str))]
}

method(get_str, usage) <- function(x) {
  unlist(lapply(x@children, get_str))
}

method(get_str, class_any) <- function(x) NULL