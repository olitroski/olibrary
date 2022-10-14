#' @title Olito's function para copiar una objeto al portapapeles
#' @description Pega una data.frame o vector al portapapeles
#' @param obj_to_copy data.frame o vector a pegar
#' @param tipo tipo de objeto data.frame o vector a pegar
#' @return character de aviso
#' @export
#' @examples
#' # data(mtcars)
#' # clipTab(mtcars, 'tab')
#'
oclip <- function(obj_to_copy = NULL, tipo = "vector"){

    if (tipo == "vector"){
        writeLines(as.character(obj_to_copy), "clipboard-128")
        cat("Vector copiado al portapapeles... \n")

    } else if (tipo == "df") {
        write.table(obj_to_copy, "clipboard-128", sep="\t", row.names=FALSE)
        cat("data.frame copiada al portapapeles... \n")

    } else {
        stop("copia: 'vector' o 'tabla' al portapapeles\n")
    }

}














