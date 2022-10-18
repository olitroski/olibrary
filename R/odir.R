#' @title Olito's function para ver el directorio filtrado
#' @description filtra del dir por extension o texto
#' @param ext Extension con punto o sin punto, mayuscula o minuscula
#' @param find Texto a buscar archvio, da igual minusc o mayusc
#' @return data.table con la columna filtrada
#' @export
#' @examples
#' # odir(ext = 'xlsx')
#' # odir(find = 'oli')
#' # odir(ext = 'xlsx', find = 'oli')
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_locate_all
#'
#'
odir <- function(ext = NULL, find = NULL){
    archivos <- i <- temp <- extension <- archivo <- ext_pos <- name <- NULL

    if (!is(ext, 'NULL')){
        ext <- str_to_lower(ext)
        ext <- sub('\\.', "", ext)
    }

    if (!is(find, 'NULL')){
        find <- str_to_lower(find)
    }

    # Listado
    archivos <- data.frame(archivo = dir())
    archivos$ext_pos <- NA

    # La extension i <- 1
    for (i in 1:nrow(archivos)){
        temp <- archivos[i, 'archivo']
        temp <- str_locate_all(temp, '\\.')
        temp <- data.frame(temp[[1]])
        temp <- temp[nrow(temp), 'end']

        archivos[i, 'ext_pos'] <- temp
        rm(temp)
    }

    setDT(archivos)
    archivos[, extension := substr(archivo, ext_pos + 1, str_length(archivo))]
    archivos[, extension := str_to_lower(extension)]
    archivos[, name := substr(archivo, 1, ext_pos - 1)]
    archivos[, name := str_to_lower(name)]

    # Y las querys
    if (!is(ext, "NULL") & is(find, "NULL")){
        temp <- archivos[extension == ext]
        return(temp[, 'archivo'])
    } else if (is(ext, "NULL") & !is(find, "NULL")) {
        temp <- archivos[grep(find, name)]
        return(temp[, 'archivo'])
    } else if (!is(ext, "NULL") & !is(find, "NULL")) {
        temp <- archivos[extension == ext]
        temp <- temp[grep(find, name)]
        return(temp[, 'archivo'])
    }

}

