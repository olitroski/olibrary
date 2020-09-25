#' @title Olito's ttest para variables independientes.
#' @description Prueba ttest directo a un dataframe para muestras independientes.
#' @param varlist Vector String de variables dependientes
#' @param grpvar Variable (string) de grupo
#' @param data Data frame con los datos
#' @param version tipo de resultado
#' @param save Guarda un excel en el wd
#' @return Devuelve un data frame con los resultados
#' @export
#' @examples
#' # grpvar <- "am"
#' # depvar <- "hp"
#' # varlist <- c("mpg", "disp", "drat", "hp")
#' # ottest.indep(varlist, grpvar, mtcars)
#' @import dplyr
#' @import openxlsx
#' @importFrom stats t.test
#' @importFrom lazyeval interp
#'
ottest.indep <- function(varlist = NULL, grpvar = NULL, data = NULL, version = 1, save = "file"){

    ## Loopeo entre las variable del varlist
    effect <- variable <- finaldf <- NULL

    for (depvar in varlist){
        # Crear sub base y capturar nombres
        tdata <- select_(data, depvar, grpvar)
        tdata <- tdata[is.na(tdata[1])==FALSE, ]
        deplab <- names(tdata)[1]
        grplab <- names(tdata)[2]

        ## Si hubiera variable constante genera error, ac? se escapa antes
        if (sd(tdata[,1]) == 0 ) { next() }

        ## Si hay insuficientes datos lo mismo
        tabla <- as.data.frame(table(tdata[,2]))
        if (tabla[1,2] <= 1) {next()} else if (tabla[2,2] <= 1) {next()}

        # hacer la t y guardar
        t <- t.test(tdata[,1] ~ tdata[,2], alternative = "less")
        less <- t$p.value
        t <- t.test(tdata[,1] ~ tdata[,2], alternative = "greater")
        great <- t$p.value

        if (less < great){
            p.value <- round(less,3)
        } else {
            p.value <- round(great,3)
        }

        t.value <- round(t$statistic,3)
        grados <- round(t$parameter,1)
        t <- data.frame(t.value, df=grados, p.value)


        # Crear base de stats
        stat <- group_by_(tdata, grplab)
        stat <- summarize_(stat, mean = interp(~mean(stub), stub = as.name(deplab)),
                           sd = interp(~sd(stub), stub = as.name(deplab)),
                           n = interp(~n(), stub = as.name(deplab)))
        stat <- as.data.frame(stat)
        stat <- mutate(stat, variable = deplab, mean = round(mean, 3), sd = round(sd, 3))
        stat <- reshape(stat, timevar = names(stat)[1], idvar = "variable", direction = "wide")
        stat$diff <- as.numeric(stat[5] - stat[2])
        stat <- dplyr::select(stat, -variable)

        vs <- names(table(tdata[2]))
        vs <- paste(vs[1], "vs", vs[2])
        vs <- data.frame(variable = deplab, grplab = vs)
        names(vs)[2] <- grplab
        stat <- cbind(vs, stat)


        # Combinar
        tresult <- cbind(stat, t)
        tresult <- mutate(tresult, sig = ifelse(p.value <= 0.05, "***", ""))
        finaldf <- rbind(finaldf, tresult)
    }


    # Tipos de resultado
    if (version == 1){       # Version base
        if (save == "file"){
            write.table(finaldf, "clipboard-128", sep="\t", row.names=FALSE)
            return(finaldf)
        } else {
            filename <- paste(save, ".xlsx", sep="")
            excel <- createWorkbook()
            addWorksheet(excel, "ttest")
            writeData(excel, "ttest", finaldf)
            freezePane(excel, "ttest", firstRow = TRUE)
            setColWidths(excel, "ttest", cols=c(1:13), widths="auto")
            saveWorkbook(excel, filename, overwrite=TRUE)
            write.table(finaldf, "clipboard-128", sep="\t", row.names=FALSE)
            return(finaldf)
        }


    } else if (version == 2){
        # Tipo resultado de publicacion con mean +/- sd. Recortado por partes
        vs <- finaldf[,c(1,2)]
        R <- 1    # REDONDEO # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

        grp1 <- finaldf[, c(3,4,5)]
        name1 <- names(grp1)[1]
        name1 <- unlist(strsplit(name1, ".", fixed=TRUE))[2]
        name1 <- paste("Group.", name1, ".N.", max(grp1[3]),sep="")
        grp1 <- data.frame(paste(round(grp1[[1]], R), rawToChar(as.raw(177)), round(grp1[[2]], R)))
        names(grp1)[1] <- name1

        grp2 <- finaldf[, c(6,7,8)]
        name2 <- names(grp2)[1]
        name2 <- unlist(strsplit(name2, ".", fixed=TRUE))[2]
        name2 <- paste("Group.", name2, ".N.", max(grp2[3]),sep="")
        grp2 <- data.frame(paste(round(grp2[[1]], R), rawToChar(as.raw(177)), round(grp2[[2]], R)))
        names(grp2)[1] <- name2

        tdf <- finaldf[, c(9, 12, 13)]
        tdf <- rename(tdf, effect = diff) %>% mutate(effect = round(effect, R))
        finaldf2 <- cbind(vs, grp1, grp2, tdf)

        # Guarda si hay que
        if (save == "file"){
            write.table(finaldf2, "clipboard-128", sep="\t", row.names=FALSE)
            return(finaldf2)
        } else {
            filename <- paste(save, ".xlsx", sep="")
            excel <- createWorkbook()
            addWorksheet(excel, "ttest")
            writeData(excel, "ttest", finaldf2)
            freezePane(excel, "ttest", firstRow = TRUE)
            setColWidths(excel, "ttest", cols=c(1:7), widths="auto")
            saveWorkbook(excel, filename, overwrite=TRUE)
            write.table(finaldf2, "clipboard-128", sep="\t", row.names=FALSE)
            return(finaldf2)
        }

    }
}
