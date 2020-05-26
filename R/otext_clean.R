#' @title Olito Text Cleaner
#' @description Quita las puntuaciones, espacios, tildes, numeros de un un texto.
#' @param txt texto a limpiar
#' @return char texto limpio
#' @export
#' @examples
#' otext_clean("texto con tildes")
#' @import stringr
#'
otext_clean <- function(txt){
    # letras solas rodeadas de espacio
    text <- str_replace_all(txt, "\\s[bcdefghjklmnopqrstuvwxyz]\\s", " ")   # letras guachas

    # Punctuacion
    text <- str_replace_all(text, "[[:punct:]]", " ") # puntuaciones

    # Acentos y ñ
    text <- str_replace_all(text, "á", "a")
    text <- str_replace_all(text, "é", "e")
    text <- str_replace_all(text, "í", "i")
    text <- str_replace_all(text, "ó", "o")
    text <- str_replace_all(text, "ú", "u")
    text <- str_replace_all(text, "ñ", "n")

    # Numeros
    text <- str_replace_all(text, "[[:digit:]]", " ")

    # Espacios y pa abajo
    text <- str_replace_all(text, '[[:space:]]+'," ")
    text <- str_to_lower(text)

    return(text)
}
# otext_clean(text)
