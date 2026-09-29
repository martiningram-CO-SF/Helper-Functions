#' @title script_message
#' @description
#' This function is used to print a message to the console that indicates the status of a script.
#' This is useful for identifying which stage the pipeline is at whilst running. The messages can 
#' be customised using the following inputs. 
#' @param input_line_symbol The symbol to use for the line that separates the
#' message from previous console outputs
#' @param input_line_length The length of the line to be printed
#' @param input_message The message to be printed to the console within the lines.
#' @return message in console log
#' @examples script_message(input_message = "All ALB Landscape Data Gathered", input_line_symbol = "=")

script_message <- function(input_line_symbol = "-", 
                           input_line_length = 65, 
                           input_message){
    
    # message to print in console
    message("\n", 
            rep(input_line_symbol, input_line_length),  
            "\n", 
            input_message, " at: ",
            str_sub(as.character(Sys.time()), end = 19),
            "\n", 
            rep(input_line_symbol, input_line_length), 
            "\n")
}