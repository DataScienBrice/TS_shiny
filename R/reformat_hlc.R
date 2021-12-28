test <- getSymbols("GOOG",src="yahoo", auto.assign = FALSE)




#' Reformt HLC
#'
#' @param data data_frame from quantmod::getsymbols
#' @param name_index index for yahoo finance
#' @importFrom dplyr mutate select renmae_all
#' @importfrom stringr str_to_lower str_replace_all
#' @importFrom tibble rowid_to_collumn
#' @return dataframe usable with our function
#' @export
#'
#'
#'
#' @examples
#' 
#' goog <- "GOOG"
#' 
#' test <- getSymbols(goog,src="yahoo", auto.assign = FALSE)
#' reformat_hlc(test, goog)
reformat_hlc <- function(data, name_index){
  
  data %>% as.data.frame() %>%
    dplyr::mutate(date =   rownames(.)) %>%
    dplyr::rename_all(    funs( stringr::str_to_lower(.) %>%
                                stringr::str_replace_all(., paste0(tolower(name_index),"."), '')))  %>%
    tibble::rowid_to_column("row_id") %>% 
    dplyr::select(-row_id)
  
  
}
