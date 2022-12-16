
box::use(R7[...],
         rlang[abort],
         ./parse_fun[...],
         ./defaults[...])


#' @export
parser <- new_class("parser",
                    properties = list(out = class_list,
                                      args = class_character,
                                      stop_options = class_logical))


#' @export
usage_parser <- function(usage) {
  
  parser_fun <- parse_fun(usage)
  function(args) {
    parser_obj <- parser(out = init_list(usage), args = args, stop_options = FALSE)
    new_parser <- parser_fun(parser_obj)
    if (length(new_parser@args)>0)
      abort(c("failed to completely parse arguments",
              "i" = "The following could not be parsed",
            "*" = paste0("`", new_parser@args, "`", collapse = ", ")),
                   class = "docr_parse_fatal")
    
    new_parser@out
  }
}