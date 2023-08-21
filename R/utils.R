
# as_data_frame_fieldgo <-  function(.data, tranpose = FALSE){
#
#     # if(class(.data)=="FieldGoBlabla"){
#     #     cont <- jsonlite::fromJSON(.data)
#     #     out <- replace_null(cont,"") %>%
#     #         as.data.frame(stringsAsFactors=FALSE) %>%
#     #         tibble::rownames_to_column()
#
#     if(nrow(jsonlite::fromJSON(.data)$data)>1){
#
#         out <- jsonlite::fromJSON(.data)$data
#
#     }else {
#         cont <- jsonlite::fromJSON(.data)$data
#         #cont <- jsonlite::fromJSON(.data)
#         if(length(cont)==0){
#             out <- data.frame()
#         }else if(nrow(cont)>=1){
#             out <- cont
#         }
#     }
#     out
# }

#' @title fill missing values in a list
#' @description fill missing values (NAS) and replace them by empty strings
#' @param .list data
#' @export
#'
fill_missing_values <- function(.list) {
    browse()
    lapply(.list, function(x) {
        if(is.na(x)) {
            return("")
        } else {
            return(x)
        }
    })
}

#' Transform a list into a data.frame
#'
#' The function `list_to_dataframe` takes a named list as input and
#' transforms it into a two-column data.frame. The names of the list elements
#' are put into the first column, and the values of the list elements are put
#' into the second column.
#'
#' @param input_list A named list which needs to be converted into a dataframe.
#' This list should be one-dimensional.
#'
#' @return A two-column data.frame, where the first column ("Names") contains the
#' names of the list elements, and the second column ("Values") contains the values
#' of the list elements.
#'
#' @examples
#' # Create a list
#' my_list <- list("name1" = "value1", "name2" = "value2")
#'
#' # Use the function to convert the list to a data frame
#' df <- list_to_dataframe(my_list)
#'
#' # Print the data frame
#' print(df)
#'
#' @importFrom tibble enframe
#' @export
#'
list_to_dataframe <- function(.list) {
    enframe(.list, name = "Names", value = "Values") %>%
    as.data.frame(stringsAsFactors=FALSE)
}



#' Convert Input Data to a Data Frame
#'
#' The function `as_data_frame_fieldgo` takes input data and transforms it into a data frame.
#' The function provides an option to transpose the resulting data frame.
#'
#' @param .data The input data to be converted to a data frame.
#' This could be a list, vector, matrix, or another type of data,
#' depending on the implementation of the function.
#'
#' @param tranpose A logical value indicating whether to transpose the resulting data frame.
#' Defaults to FALSE.
#'
#' @return data.frame
#'
#' @examples
#' # as_data_frame_fieldgo(object)
#'
#' @export
#'
as_data_frame_fgo <-  function(.data){

    #extract only data (not success atribute)
    cont <- jsonlite::fromJSON(.data)$data
    if(nrow(cont)>1){
        out <- cont
    } else {
        out <-  fill_missing_values(cont) %>%
                list_to_dataframe()

        colnames(out) <- c("DbAttribute","Value")

    }
    return(out)
}


