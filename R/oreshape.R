#'
#' @title oreshape - Olito's reshape
#'
#' @description Es un wrapper del reshape de data.table, la idea es que sea más sencillo y parecido al de Stata en donde se debe identificar a todo evento una serie de parametros. Evita que de wide a long las variables sean del mismo tipo si son stub, el n de variables stub debe ser el mismo, pero no es necesario que el identificador de tiempo sea un numero. Todos los parametros (excepto la data) van entre comillas
#'
#' @param idvar variable que identifica a cada sujeto o unidad muestral
#' @param stub  base o prefijo de las medidas repetidas, de long a wide es la medicion repetida, puede ser un vector de stubs
#' @param tiempo es la variable que indica la medida repetida, de wide a long es el nombre que tendra luego de la transformacion
#' @param data datos en formato data.table, si no viene asi se aplica este formato
#' @param direction direccion de la transformacion 'long' o 'wide' en minuscula
#'
#' @export
#'
#' @return devuelve un data.table object
#'
#' @examples
#'
#' # # Armar datos -----------------------
#' # # El indometh es de "datasets"
#' # write.table(Indometh, "datos.txt", sep = ",", row.names = FALSE)
#' # datos <- fread("datos.txt")
#' # setnames(datos, c("Subject", "time", "conc"), c("id", "tiempo", "num"))
#' #
#' # # Crear una variable fija
#' # datos[(datos[, id] %in% c(1, 2, 3)), sexo := "Hombre"]
#' # datos[(datos[, id] %in% c(4, 5, 6)), sexo := "Mujer"]
#' #
#' # # Crear una data adicional pero en string
#' # datos[, char := rep(letters[], length.out = 66)]
#' #
#' # # Desbalancear data
#' # datos <- datos[-c(15:18, 52:53, 60),]
#' # datos[, .N, by = id]
#' #
#' # # Simplificar el tiempo
#' # datos[, .N, by = tiempo]
#' # lookup <- data.table(tiempo = unique(datos[, tiempo]), to = 1:11)
#' # datos[lookup, on = "tiempo", tiempo := i.to]
#' # rm(lookup)
#' #
#' # # Agregar algunos NA
#' # datos[c(4, 23, 45), num := NA]
#' # datos[c(2, 15, 25), char := NA]
#' #
#' # setnames(datos, 'tiempo', 'semana')
#' #
#' #
#' # # Pruebas ---------------------------
#' # # Con 2 variables stub
#' # head(datos)
#' # wideDF <- oreshape(idvar = "id", stub = c("num", "char"),
#' #            tiempo = "semana", data = datos, direction = "wide")
#' # longDF <- oreshape(idvar = "id", stub = c("num_", "char_"),
#' #         tiempo = "semana", data = wideDF, direction = "long")
#' #
#' # # Con 1 variable stub
#' # datos[, char := NULL]
#' # wideDF <- oreshape(idvar = "id", stub = "num",
#' #            tiempo = "semana", data = datos, direction = "wide")
#' # longDF <- oreshape(idvar = "id", stub = "num_",
#' #            tiempo = "semana", data = wideDF, direction = "long")
#'
#' @import glue
#' @importFrom data.table setDT
#' @importFrom data.table dcast.data.table
#' @importFrom data.table melt.data.table
#' @importFrom data.table merge.data.table
#' @importFrom data.table :=
#'





oreshape <- function(idvar = NULL, stub = NULL, tiempo = NULL, data = NULL, direction = NULL){
    ..idVar <- ..idVars <- ..idvar <- firstDf <-  id <- key <- NULL

    # Si se... mucho eval(parse)... no ando inteligente estos días... dont judge me...
    setDT(data)
    # ---- Hacia Wide ---------------------------------------------------------
    # idvar <- 'id'
    # stub <- c('num', 'char')
    # tiempo <- 'semana'
    # data <- datos

    if (direction == "wide"){
        # Variables bloque id
        idVars <- c(stub, tiempo)
        idVars <- names(data)[!(names(data) %in% idVars)]

        # Revisar registro por IDvar
        if (nrow(unique(data[, ..idVars])) > nrow(unique(data[, ..idvar]))){
            repetido <- unique(data[, ..idVars])
            repId <- duplicated(repetido[, ..idvar])
            repId <- repetido[repId, ..idvar]
            repId <- as.data.frame(repId)[, 1]
            print(repetido[get(idvar) %in% repId])
            rm(repId, repetido)
            stop("Bloque de variables ID repetidas...")

        # Continuar con el reshape
        } else {
            # Armar con texto
            y <- paste(idVars, collapse = ' + ')
            v <- paste(glue("'{stub}'"), collapse = ", ")
            temp <- glue("dcast.data.table(data, {y} ~ {tiempo}, value.var = c({v}))")
            temp <- eval(parse(text = temp))

            # Nombres de variable cuando el stub es uno
            if (length(stub) == 1){
                indx <- !(names(temp) %in% idVars)
                names(temp)[indx] <- paste0(stub, "_", names(temp)[indx])
                return(temp)
            } else {
                return(temp)
            }
        }


    # ---- Hacia Long ---------------------------------------------------------
    } else if (direction == "long"){
        # idvar <- 'id'
        # stub <- c('num_', 'char_')
        # tiempo <- 'semana'
        # data <- temp

        # Bloque mediciones repetidas e Id
        bloqueRep <- NULL
        for (s in stub){
            bloqueRep <- c(bloqueRep, grep(s, names(data)))
        }
        bloqueRep <- names(data)[bloqueRep]
        bloqueId <- names(data)[!(names(data) %in% bloqueRep)]
        rm(s, bloqueRep)


        # Crear "Data stub"
        nColumn <- NULL     # Para chequeo
        for (s in stub){
            varVec <- c(bloqueId, names(data)[grep(s, names(data))])
            cmd <- glue("{s} <- data[, ..varVec]")
            eval(parse(text = cmd))

            cmd <- glue("temp <- ncol({s})")
            eval(parse(text = cmd))
            nColumn <- c(nColumn, temp)
        }
        rm(s, varVec, cmd, temp)

        # Para cuando solo hay 1 stub en el check proximo
        testnColumn <- sd(nColumn, na.rm = TRUE)
        if (is.na(testnColumn)){
            testnColumn <- 0
        }


        # Buscar combinaciones id duplicadas
        if (nrow(unique(data[, ..idvar])) != nrow(data)){
            repId <- duplicated(data[, ..idvar])
            repId <- data[repId, ..idVar]
            print(repId)
            rm(repId)
            stop("Bloque de variables ID repetidas...")

        # chequear que el N de variables sea el mismo en cada Stub
        } else if (testnColumn != 0){
            stop("Cada Stub debe tener el mismo numero de variables....")

        # Continuar el reshape
        } else {
            # Lupito de reshapes
            for (s in stub){
                # Antecedentes
                repMerge <- names(data)[grep(s, names(data))]
                repMerge <- glue("'{repMerge}'")
                repMerge <- paste(repMerge, collapse = ",")
                varname <- paste0(tiempo, "_", s)
                idMerge <- paste(glue("'{bloqueId}'"), collapse = ', ')

                # Ejecutar
                meltCmd <- glue("melt.data.table(data, id.vars = c({idMerge}), ",
                                "measure.vars = c({repMerge}), ",
                                "variable.factor = FALSE, value.factor = FALSE,
                                 value.name = '{s}', variable.name = '{varname}')")

                meltTemp <- eval(parse(text = meltCmd))
                meltTemp[, key := paste0(id, "_", sub(s, "", get(varname)))]

                # Dejar en la data merge
                cmd <- glue("{s} <- meltTemp")
                eval(parse(text = cmd))
                # print(paste("listo reshape en", s ))
                # rm(repMerge, varname, idMerge, meltCmd, meltTemp, cmd)
            }
        }


        # Ahora juntar la data ------------------------------------------------------
        if (length(stub) == 1){
            temp <- glue("{s}[, key := NULL]")
            temp <- eval(parse(text = temp))
            return(temp)

        } else {
            contador <- NULL
            for (s in stub){
                # Para la primera iteración
                if (is.null(contador) == TRUE){
                    cmd <- glue("firstDf <- {s}")
                    eval(parse(text = cmd))
                    contador <- c(contador, s)

                # El resto le lleva merge con "firstDf"
                } else {
                    # quitar las variables id
                    for (var in bloqueId){
                        eval(parse(text = glue("{s}[, {var} := NULL]")))
                    }

                    # Y el merge
                    cmd <- glue("firstDf <- merge.data.table(firstDf, {s}, by = 'key', all = TRUE)")
                    # print(cmd)
                    eval(parse(text = cmd))
                }
            }
        }

        # y listo
        firstDf[, key := NULL]
        return(firstDf)


    # ---- Direction misspell -------------------------------------------------
    } else {
        stop("Debe seleccionar 'long' o 'wide'")
    }
}

