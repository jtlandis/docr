
box::use(
  R7[`@`, new_generic, `method<-`],
  utils[str],
  ./get_str[...]
)

#' @export
box::use(
  ./usage[...]
)

.on_load <- function(ns) {
  
  box::register_S3_method("format", "usage", format.usage)
  box::register_S3_method("format", "opt", format.opt)
  box::register_S3_method("format", "exclsv", format.exclsv)
  box::register_S3_method("format", "req", format.req)
  box::register_S3_method("format", "cmd", format.cmd)
  box::register_S3_method("format", "arg", format.arg)
  box::register_S3_method("format", "flg", format.flg)
  box::register_S3_method("print",  "usage", print.usage)
  box::register_S3_method("str", "usage", str.usage)
  
}

str.usage <- function(object, ...) {
  
  if (length(object@children)==0) {
    attr(attr(object, "R7_class"), "properties")$children <- NULL
  }
  NextMethod()
  
}

color <- function(fun, use_color = TRUE) {
  if (use_color) {
    fun
  } else {
    identity
  }
}

print.usage <- function(x, indent = "", sep = " ", use_color = TRUE, ...) {
  cat(format(x, indent = indent, use_color = use_color), sep = sep)
}

format.usage <- function(x, indent = "", use_color = TRUE) {
  
  fmt <- vector('list', length = length(x))
  children <- x@children
  for (i in seq_along(children)) {
    fmt[[i]] <- format(children[[i]], indent = indent, use_color = use_color)
  }
  out <- unlist(fmt)
  paste0(indent, out)
  
}

format.opt <- function(x, indent = "", use_color = TRUE) {
  
  fun <- color(function(x) cli::col_green(cli::style_bold(x)), use_color = use_color)
  children <- x@children
  fmt <- vector('list', length = length(children))
  for (i in seq_along(children)) {
    fmt[[i]] <- format(children[[i]], indent = indent)
  }
  out <- c(
    fun("["),
    unlist(fmt),
    fun("]")
  )
  paste0(indent, out)
}

format.exclsv <- function(x, indent = "", use_color = TRUE) {
  fun <- color(function(x) cli::col_br_cyan(cli:: style_bold(x)), use_color = use_color)
  children <- x@children
  if (length(children)==0) return(NULL)
  fmt <- vector('list', length = length(children) * 2L - 1L)
  fmt[] <- fun("|")
  for (i in seq_along(children)) {
    fmt[[i * 2L - 1L]] <- format(children[[i]], indent = indent)
  }
  out <- unlist(fmt)
  paste0(indent, out)
}

format.req <- function(x, indent = "", use_color = TRUE) {
  
  fun <- color(function(x) cli::col_red(cli::style_bold(x)), use_color = use_color)
  children <- x@children
  fmt <- vector('list', length = length(children))
  for (i in seq_along(children)) {
    fmt[[i]] <- format(children[[i]], indent = indent)
  }
  out <- c(
    fun("("),
    unlist(fmt),
    fun(")")
  )
  paste0(indent, out)
}

format.multi <- function(x, indent = "", use_color = TRUE) {
  children <- x@children
  fmt <- vector("list", length = length(children))
  for (i in seq_along(children)) {
    fmt[[i]] <- format(children[[i]], indent = indent, use_color = use_color)
  }
  paste0(indent, fmt, "...")
}

format.cmd <- function(x, indent = "", use_color = TRUE) {
  fun <- color(cli::col_yellow, use_color = use_color)
  paste0(indent, fun(x@str))
}

format.arg <- function(x, indent = "", use_color = TRUE) {
  fun <- color(cli::col_blue, use_color = use_color)
  paste0(indent, fun(sprintf("<%s>", x@str)))
}

format.flg <- function(x, indent = "", use_color = TRUE) {
  str <- get_str(x)
  if (nchar(str)>1) {
    str <- paste0("--", str)
  } else {
    str <- paste0("-", str)
  }
  fun <- color(cli::col_silver, use_color = use_color)
  paste0(indent, fun(str))
}


