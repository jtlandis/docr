

box::use(
  rlang[set_names],
  R7[`@`, new_generic, R7_dispatch, `method<-`, class_any],
  ../usage[get_str, usage, fields]
)

#' @export
init_list <- function(usage) {
  str_nms <- get_str(usage)
  set_names(vector("list", length = length(str_nms)), str_nms)
}


#' @export
defaults <- new_generic("defaults", "x", function(x) R7_dispatch())

method(defaults, usage) <- function(x) {
  unlist(lapply(x@children, defaults), F)
}
method(defaults, fields) <- function(x) {
  str <- get_str(x)
  set_names(vector("list", length(str)), str)
}

method(defaults, class_any) <- function(x) {
  NULL
}