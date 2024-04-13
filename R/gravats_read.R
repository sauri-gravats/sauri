#' @name gravats_read
#' 
#' @description Read a dataset of engravings.
#'
#' @param gravats Path to the file of engraved rocks
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return a dataframe
#'
#' @examples
#' 
#' 
#' @export
gravats_read <- function(gravats = "C:/Rprojects/sauri/data/240413/llista_gravats_19-20-23_DEFINITIVA_FINAL_FINAL.xls.xlsx",
                         verbose = TRUE){
  if(DescTools::SplitPath(gravats)$extension == "xlsx"){
    if(verbose){
      print(paste0("Read an XLSX"))
    }
    gravats.df <- openxlsx::read.xlsx(gravats,
                                      rowNames = F,
                                      skipEmptyRows = TRUE)
  }
  return(gravats.df)
}

