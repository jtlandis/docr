

box::use(
  R7[new_generic, R7_dispatch, `method<-`, R7_inherits],
  ../usage[...],
  rlang[abort],
)

docr_abort <- function(usage, parser, footer = NULL) {
  
  message <- "Parsing failure occured"
  arg <- parser@args[1]
  if (!is.na(arg)) {
    message <- sprintf("%s when parsing '%s'", message, arg)
  } else {
    message <- c(
      message,
      "x" = "no more arguments to parse!"
    )
  }
  message <- c(
    message,
    "i" = sprintf("occured in usage pattern: %s",
                  paste0(format(usage), collapse = " ")),
    footer)
  abort(message,
        .usage = usage,
        .arg = arg,
        class = "docr_parse_failure")
}

#' @export
parse_fun <- new_generic("parse_fun", c("usage"), fun = function(usage) R7_dispatch())

method(parse_fun, usage) <- function(usage) {
  
  funs <- lapply(usage@children, parse_fun)
  children <- usage@children
  
  function(parser) {
      
    for (i in seq_along(funs)) {
      new_parser <- tryCatch(
        funs[[i]](parser),
        docr_parse_failure = function(cnd) {
          docr_abort(usage, parser,
                     footer = c(
                       "i" = sprintf(
                         "specifically in %s",
                         paste0(format(cnd$.usage), collapse = " ")
                       )
                     ))
        }
      )
      parser <- new_parser
    }
    parser
  }
  
}

method(parse_fun, flg) <- function(usage) {
  
  pattern <- usage@str
  name <- get_str(usage)
  function(parser) {
    token <- parser@args[1]
    short <- gsub("^-+", "", pattern[1])
    long <- gsub("^-+", "", pattern[2])
    if (!is.na(long) && (grepl(sprintf("^--%s$", long), token))) {
      cur_val <- parser@out[[name]]
      if (is.null(cur_val)) {
        parser@out[[name]] <- 1L
      } else {
        parser@out[[name]] <- cur_val + 1L
      }
      
      parser@args <- parser@args[-1L]
      return(parser)
    }
    if (!is.na(short) && grepl(sprintf("^-[^-]*%s.*", short), token)) {
      cur_val <- parser@out[[name]]
      if (is.null(cur_val)) {
        parser@out[[name]] <- 1L
      } else {
        parser@out[[name]] <- cur_val + 1L
      }
      
      new_token <- sub(short, "", token)
      if (new_token=="-") {
        new_token <- NULL
      }
      parser@args <- c(new_token, parser@args[-1L])
      return(parser)
    }
    docr_abort(usage, parser)
  }
  
}

method(parse_fun, arg) <- function(usage) {
  
  pattern <- usage@str
  function(parser) {
    token <- parser@args[1]
    res <- grepl("^[^-]", token)
    if (res) {
      cur_val <- parser@out[[pattern]]
      if (is.null(cur_val)) {
        parser@out[[pattern]] <- token
      } else {
        parser@out[[pattern]] <- c(cur_val, token)
      }
      parser@args <- parser@args[-1L]
    } else {
      docr_abort(usage, parser)
    }
    parser
  }
}

method(parse_fun, cmd) <- function(usage) {
  pattern <- usage@str
  function(parser) {
    token <- parser@args[1]
    res <- grepl(pattern, token)
    if (res) {
      cur_val <- parser@out[[pattern]]
      if (is.null(cur_val)) {
        parser@out[[pattern]] <- 1L
      } else {
        parser@out[[pattern]] <- cur_val + 1L
      }
      
      parser@args <- parser@args[-1L]
      if (pattern == "--") {
        parser@stop_options <- TRUE
      }
      
    } else {
      docr_abort(usage, parser)
    }
    return(parser)
  }
}


method(parse_fun, exclsv) <- function(usage) {
  
  funs <- lapply(usage@children, parse_fun)
  
  function(parser) {
    
    for (f in funs) {
      parser_out <- tryCatch(f(parser),
                             docr_parse_failure = function(cnd) {
                               NULL
                             })
      if (!is.null(parser_out)) {
        break
      }
    }
    if (is.null(parser_out))
      docr_abort(usage, parser)
    parser_out
  }
  
}

method(parse_fun, req) <- function(usage) {
  
  funs <- lapply(usage@children, parse_fun)
  
  function(parser) {
    parser <- tryCatch({
      for (f in funs) {
        parser <- f(parser)
      }
      parser
    },
    docr_parse_failure = function(cnd) {
      docr_abort(usage, parser,
                 footer = c(
                   "i" = sprintf(
                     "failure raised by parsing '%s' in %s",
                     cnd$.arg,
                     paste0(format(cnd$.usage), collapse = " ")
                   )
                 ))
    }
      
    )
    parser
  }
  
}

method(parse_fun, opt) <- function(usage) {
  
  children <- usage@children
  funs <- lapply(children, parse_fun)
  
  function(parser) {
    if (parser@stop_options) return(parser)
    exhaused <- rep(F, length(funs))
    is_multi <- vapply(children, R7_inherits,
                       class = multi, FUN.VALUE = logical(1L))
    done <- rep(F, length(funs))
    old_parser <- parser
    if (any(.lgl <- parser@args=="--")) {
      
    }
    while(any(!exhaused)) {
      exhaused <- rep(F, length(funs))
      for (i in seq_along(funs)) {
        if (done[i]) {
          exhaused[i] <- T
          next
        }
        new_parser <- tryCatch(
          funs[[i]](parser),
          docr_parse_failure = function(cnd) {
            exhaused[i] <<- T
            parser
          }
        )
        if (!identical(new_parser, parser) &&
            !is_multi[i]) {
          done[i] <- T
        }
        parser <- new_parser
        if (parser@stop_options) {
          break
        }
      }
    }
    parser
  }
}

method(parse_fun, multi) <- function(usage) {
  
  fun <- parse_fun(usage@children[[1]])
  
  function(parser) {
    parser_orig <- parser
    parser <- tryCatch(
      while(TRUE) {
        parser <- fun(parser)
      },
      docr_parse_failure = function(cnd) {
        parser
      }
    )
    if (identical(parser, parser_orig))
      docr_abort(usage, parser)
    parser
  }
}