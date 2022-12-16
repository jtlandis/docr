

box::use(R7[...], rlang[list2])


#'@export
usage <- new_class("usage", properties = list(children = class_list),
                   constructor = function(...) {
                     dots <- list2(...)
                     new_object(NULL, children = dots)
                   })
#' @export
fields <- new_class("fields",
                    usage,
                    properties = list(
                      str = class_character
                    ),
                    constructor = function(str = class_missing, ...) {
                      new_object(usage(...), str = str)
                    })

#' @export
flg <- new_class("flg", fields,
                 constructor = function(str = class_missing, ...) {
                   if (missing(str)) stop("`str` needs an argument")
                   if ((n <- length(str))>2) stop("`str` can be at most 2 arguments")
                   short <- grep("^-?[a-zA-Z]$", str)
                   if (length(short)==0) {
                     short <- NA
                   }
                   long <- grep("^(--)?[a-zA-Z][a-zA-Z0-9]*", str)
                   if (length(long)==0) {
                     long <- NA
                   }
                   new_object(usage(...), str = str[c(short,long)])
                 })

#' @export
arg <- new_class("arg", fields)

#' @export
cmd <- new_class("cmd", fields)

#' @export
qualifier <- new_class("qualifier", usage,
                       constructor = function(...) {
                         dots <- list2(...)
                         for (i in seq_along(dots)){
                           dot <- dots[[i]]
                           if (identical(class(dot), c("usage","R7_object"))) {
                             dots[[i]] <- dot@children
                           }
                         }
                         dots <- unlist(dots, F)
                         new_object(usage(!!!dots))
                       })

#' @export
opt <- new_class("opt", qualifier)

#' @export
req <- new_class("req", qualifier)

#' @export
exclsv <- new_class("exclsv", qualifier,
                    constructor = function(...){
                      dots <- list2(...)
                      .parent <- qualifier()
                      .parent@children <- dots
                      new_object(.parent = .parent)
                    })

#' @export
multi <- new_class("multi", qualifier)


# usage() + flg(str = "--test") + arg(str = "NUM") + flg(str = "OPTS")
# 
# usage() + req() + (flg() + arg())
# req(opt(arg(str = "1"), arg(str = "2")))
# 
# usage(req(flg() + arg() | arg()))
# u <- usage() + req(flg(str = "either-this") + arg(str = "and-that") | arg(str = "or-this"))
