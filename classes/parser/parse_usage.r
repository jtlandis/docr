

box::use(./R7_classes[...],
         ./R7_methods[...],
         R7[...],
         ./format[...],
         ./methods/get_str[...])




#   m <- numeric(n)
#   for (i in i_seq) {
#     child <- children[[i]]
#     fields <- get_str(child)
#     purrr::map2_lgl(parsers_out[[i]]@out[fields], defaults(child))
#   }
#   
# }
# 
# usage1 <- c("program.r", "--either-this", "FILE")
# usage2 <- c("program.r", "FILE")
# 
# u1 <- usage1[-1]





# o <- flg(str = "--test") + arg("FILE")
# o
# 
# token <- u@children[[1]]





# Apply a function across the available arguments
# of the parser
# papply <- function(parser_obj, .fun) {
#   browser()
#   try_fun <- function(p) {
#     tryCatch(
#       .fun(p),
#       docr_parse_failure = function(cnd) {
#         p
#       }
#     )
#   }
#   
#   new_parsers <- lapply(parser_obj@args, parser, out = list())
#   out <- vector("list", length(new_parsers))
#   for (i in seq_along(new_parsers)) {
#     out[[i]] <- try_fun(new_parsers[[i]])
#   }
#   
#   avail_names <- names(parser_obj@out)
#   out_obj <- lapply(
#     avail_names,
#     function(x, obj) {
#       rlang::list2(!!!lapply(obj, function(y) y@out[[x]]))
#     }, out = rlang::list2(parser_obj, !!!out))
#   
#   
#   return(out_obj)
# }




