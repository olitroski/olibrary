#' @title Olito's function para seleccionar wd
#' @description Selecciona un working directory mediante un archivo, ejecuta el setwd() y guarda un objeto mainDir.
#' @return character con el path
#' @export
#' @examples
#' # selectWD()
#' @importFrom stringr str_split
selectWD <- function(){
    wd <- file.choose()
    wd <- as.character(str_split(wd, "\\\\", simplify = TRUE))
    wd <- wd[1:length(wd)-1]
    wd <- paste(wd, collapse = '/')
    # mainDir <<- wd
    setwd(wd)
    return(wd)
}


