#' @title Olito Text Cleaner
#' @description Quita las puntuaciones, espacios, tildes, números de un un texto.
#' @param txt texto a limpiar
#' @return char texto limpio
#' @export
#' @examples
#' otext_clean("texto con tildes")
#' @import stringr
#'
otext_clean <- function(txt){
    # letras solas rodeadas de espacio
    text <- tolower(txt)
    text <- str_replace_all(text, "\\s[bcdefghjklmnopqrstuvwxyz]\\s", " ")   # letras guachas

    # Punctuacion
    text <- str_replace_all(text, "[[:punct:]]", " ")

    # Acentos y enie
    text <- str_replace_all(text, "\u00E1", "a")
    text <- str_replace_all(text, "\u00E9", "e")
    text <- str_replace_all(text, "\u00ED", "i")
    text <- str_replace_all(text, "\u00F3", "o")
    text <- str_replace_all(text, "\u00FA", "u")
    text <- str_replace_all(text, "\u00F1", "n")

    # Numeros
    text <- str_replace_all(text, "[[:digit:]]", " ")

    # Espacios y pa abajo
    text <- str_replace_all(text, '[[:space:]]+'," ")
    text <- str_to_lower(text)

    return(text)
}


