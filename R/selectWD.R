#' @title Olito's function para seleccionar wd
#' @description Selecciona un working directory mediante un archivo, ejecuta el setwd() y guarda un objeto mainDir.
#' @return character con el path
#' @export
#' @examples
#' # selectWD()
#' @importFrom stringr str_split
#' @importFrom glue glue
#' @importFrom clipr write_clip
selectWD <- function(){
    wd <- file.choose()
    wd <- as.character(str_split(wd, "\\\\", simplify = TRUE))
    wd <- wd[1:length(wd)-1]
    wd <- paste(wd, collapse = '/')
    cat(glue("{wd}\n"))
    wd <- glue("setwd('{wd}')")
    write_clip(wd, object_type = "character")
}
