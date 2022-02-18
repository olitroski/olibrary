#' @title Olito's function buscar textos similar
#' @description Dado un texto o palabra, busca el más similar en un vector de palabras. Excepto si el input es NA o "", usa la libreria "stringdist" con default en "osa". No la he probado vectorizada... solo digo!!!
#' @param test character, txto a buscar
#' @param txtvector character vector, es el vector en donde se van a buscar los similares
#' @param excluye vector con la excepciones para el texto, devuelve un NA
#' @return character del valor más parecido
#' @export
#' @examples
#' # Crear un vector de testeo con la data de "islands"
#' # txtvector <- names(datasets::islands)
#' # simText("4Fr1caaa", txtvector)
#' # simText("#", txtvector, excluye = "#")
#' @importFrom stringdist stringsim
simText <- function(test = NULL, txtvector = NULL, excluye = c(NA, "", " ")){

    # Excluidos
    if (test %in% excluye){
        return(NA)

    # y el mas parecido
    } else {
        similar <- stringsim(test, txtvector)
        similar <- which(similar == max(similar))
        similar <- txtvector[similar]

        # Si hay más de 1 devuelve el primero
        if (length(similar) > 1){
            return(similar[1])
        } else {
            return(similar)
        }
    }
}

