#' @title Olito's function para copiar una tabla al portapapeles
#' @description Pega una data.frame o data.table al portapapeles para pegar facil al Excel
#' @param df data.frame o data.table apegar
#' @return character de aviso
#' @export
#' @examples
#' # data(mtcars)
#' # clipTab(mtcars)
#'
ocliptab <- function(df = NULL){
    write.table(df, "clipboard-128", sep="\t", row.names=FALSE)
    cat("Tabla pegada al portapapeles... \n")
}
