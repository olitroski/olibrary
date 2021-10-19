#' @title Olito's Correlation
#'
#' @description Calculate correlations by transforming variables into a matrix and then
#' use of hmisc package the rest is data management. Calculates pearson and spearman. Wrapper from Stata.
#'
#' @param target target variables
#' @param indep independent variables, if NULL: target vars will fill this parameter
#' @param grpvar Group variable (only one)
#' @param data data frame
#' @param type Type of correltation "pearson" o "spearman"
#' @return data frame
#' @export
#' @examples
#' # library(dplyr); library(Hmisc)
#' # ocorr("mpg", "hp", data = mtcars, type = "spearman")
#' # ocorr(c("mpg", "cyl", "qsec"), c("drat", "hp", "wt"), data = mtcars, grpvar = "am")
#' # ocorr(c("mpg", "cyl", "qsec"), data = mtcars)
#' @importFrom Hmisc rcorr
#' @importFrom stringr str_split
#' @importFrom dplyr rename

ocorr <- function(target = NULL, indep = NULL, grpvar = NULL, data = NULL, type = "pearson"){
    # For test
    # rm(list=ls())
    # target <- c("mpg", "cyl", "qsec")
    # indep <- NULL
    # indep <- c("drat", "hp", "wt")
    # grpvar <- NULL # "am"
    # data <- mtcars
    # type <- "pearson"

    # Nulos
    N <- par <- par1 <- par2 <- pvalue <- r <- NULL

    # ---- La data en lista para procesar cada elemento por separado ------------------------------
    # Crear la data
    datalist <- list()
    if (is.null(grpvar) == FALSE){
        # Sacar grupos y dejarlos como factor
        grupos <- unique(data[[grpvar]])
        for (grp in grupos){
            grpname <- paste0(grpvar, ": ", grp)
            temp <- data[data[grpvar] == grp, ]
            datalist[[grpname]] <- temp
        }
        rm(grpname, temp)
    } else {
        datalist[["data"]] <- data
    }
    rm(data)


    # ---- Para cada elemento de la lista ---------------------------------------------------------
    options(scipen = 999)
    listnames <- names(datalist)
    resultados <- NULL             # aca van los resultados

    for (dfname in listnames){
        # select de variables
        if (is.null(indep)){
            corrData <- datalist[[dfname]]
            corrData <- corrData[target]
        } else {
            corrData <- datalist[[dfname]]
            corrData <- corrData[c(target, indep)]
        }

        # <<<<< Calcular correlaciones >>>>>
        corrStat <- rcorr(as.matrix(corrData), type = type)

        # Dejar solo el triangulo inferior de la matriz
        corrVal <- corrStat$r
        corrVal[upper.tri(corrVal, diag = TRUE)] <- NA

        corrP <- corrStat$P
        corrP[upper.tri(corrP, diag = TRUE)] <- NA

        corrN <- corrStat$n
        corrN[upper.tri(corrN, diag = TRUE)] <- NA

        rm(corrStat, corrData)


        # <<<< Compilar resultados sin independientes >>>>
        # Listado de variables
        if (is.null(indep)){
            variables <- target
        } else {
            variables <- c(target, indep)
        }

        # Pasar las variables
        for (var in variables){
            # print(var)

            # Correlacion
            temp <- as.data.frame(corrVal)
            tempCol <- temp[[var]]
            tempCol <- data.frame(par = paste(var, variables, sep = " - "), r = tempCol, stringsAsFactors = FALSE)
            tempRow <- temp[row.names(temp) == var,]
            tempRow <- as.numeric(tempRow[1,])
            tempRow <- data.frame(par = paste(variables, var, sep = " - "), r = tempRow, stringsAsFactors = FALSE)
            tempR <- rbind(tempCol, tempRow)
            tempR <- filter(tempR, !is.na(r))
            rm(temp, tempCol, tempRow)

            # P-valor
            temp <- as.data.frame(corrP)
            tempCol <- temp[[var]]
            tempCol <- data.frame(par = paste(var, variables, sep = " - "), pvalue = tempCol, stringsAsFactors = FALSE)
            tempRow <- temp[row.names(temp) == var,]
            tempRow <- as.numeric(tempRow[1,])
            tempRow <- data.frame(par = paste(variables, var, sep = " - "), pvalue = tempRow, stringsAsFactors = FALSE)
            tempP <- rbind(tempCol, tempRow)
            tempP <- filter(tempP, !is.na(pvalue))
            rm(temp, tempCol, tempRow)

            # N
            temp <- as.data.frame(corrN)
            tempCol <- temp[[var]]
            tempCol <- data.frame(par = paste(var, variables, sep = " - "), N = tempCol, stringsAsFactors = FALSE)
            tempRow <- temp[row.names(temp) == var,]
            tempRow <- as.numeric(tempRow[1,])
            tempRow <- data.frame(par = paste(variables, var, sep = " - "), N = tempRow, stringsAsFactors = FALSE)
            tempN <- rbind(tempCol, tempRow)
            tempN <- filter(tempN, !is.na(N))
            rm(temp, tempCol, tempRow)

            # Combinar y agregar a resutlados
            temp <- merge(tempR, tempP, by = "par", all = TRUE)
            temp <- merge(temp, tempN, by = "par", all = TRUE)
            temp$grp <- dfname
            resultados <- rbind(resultados, temp)
        }
    }

    # Quitar los repetidos
    resultados <- dplyr::distinct(resultados, par, grp, .keep_all = TRUE)

    # Corregir lineas sobrantes cuando hay datos en "indep"
    if (is.null(indep) == FALSE){
        temp <- str_split(resultados$par, " - ", simplify = TRUE)
        resultados$par1 <- temp[,1] %in% indep
        resultados$par2 <- temp[,2] %in% target
        resultados <- filter(resultados, par1 == FALSE & par2 == FALSE)
        resultados <- select(resultados, -par1, -par2)

    }

    # Toques finales
    resultados <- mutate(resultados, r = format(round(r, 3), nsmall = 3), pvalue = format(round(pvalue, 3), nsmall = 3))
    if (type == "spearman"){resultados <- rename(resultados, rho = r)}
    resultados <- rename(resultados, pair = par, group = grp)
    return(resultados)
}

