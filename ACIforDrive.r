library(seewave)
library(tuneR)
library(soundecology)

source("libPIA.r")

main <- function(threshold = 0.85, delta = 0.5){
    #Init
    p <- "C:/Users/Eduardo Calvillo Uni/Documents/FIME/Topicos Selectos 2/AUDIOS PIA/Final29Sep/"
    list <- unlist(list.files(path = p, pattern = ".*\\.wav", full.names = FALSE))
    og.size <- length(list)
    # list <- list[11:16]
    sink("logs.txt")
    cat("Analysis for files in ", p,"\n")
    sink()
    
    BIs <- c()
    ACIs <-c()
    BIs.norm <- c()
    ACIs.norm <-c()
    #Tratamiento
    for(i in length(list):1){
        sink("logs.txt", append  = TRUE)
        cat("Treating wave #",i,"/",length(list),"\n")
        wave.og <- mono(readWave(paste(p,list[i], sep="")))
        wave.og <- myFFilter(wave.og,from = 1000, to = 8000, bandpass = TRUE )
        
        sp <- spectro(wave.og, plot = FALSE)
        dbMin <- -23
        dbMax <- -10
        transformed.vector <- mapply(validateThreshold, sp$amp, threshold.min = dbMin, threshold.max = dbMax)
        transformed.matrix <- matrix(transformed.vector, ncol = length(sp$freq), nrow = length(sp$time))

        succ <- sumOfSuccesses(transformed.matrix)
        thold <- findThreshold(delta, sp$time[1], sp$time[2])
        isOk <- findBird(succ,thold)

        if(!is.na(isOk[1])){
            # # Normalizar el audio utilizandolo su envolvente absoluta
            # wave.abs <- env(wave.og, envt = "abs", plot = FALSE)
            # wave.abslog.left <- unlist(lapply(wave.abs, logVar, var = 0))
            # wave.abslog <- Wave(left = wave.abslog.left, samp.rate = as.numeric(wave.og@samp.rate))
            #Se normaliza el audio 
            m<- abs(min(wave.og@left))
            wave.norm.left <- unlist(lapply(wave.og@left, logVar, var = m))
            wave.norm <- Wave(left = wave.norm.left, samp.rate = as.numeric(wave.og@samp.rate), bit = 16)
            # Calcular el AEI para filtrar muestras con grandes cambios de amplitud 
            AEI.norm <- AE(wave.norm)
            cat("AEI: ",AEI.norm$aei_left,"\n")
            if(AEI.norm$aei_left >= threshold){
                sink()
                #Visualizar sinusoidales
                par(mfrow=c(2,1))
                plot(wave.og, main = paste(list[i],"Original Wave"))
                plot(wave.norm, main = paste(list[i],"Normalized Wave"))

                write.csv(transformed.matrix, file = paste(list[i],".csv",sep=""))

                #Calcular BI
                BIs <- c(BIs,myBI(wave.og))
                BIs.norm <- c(BIs.norm, myBI(wave.norm))
                #Calcular ACI
                ACIs <- c(ACIs,newACI(wave.og))
                ACIs.norm <- c(ACIs.norm,newACI(wave.norm))
            } else{
                cat(list[i], " didn't pass AEI test.\n")
                sink()
                list <- list[-i]
            }
        } else {
            cat(list[i], " didn't pass amplitude range test.\n")
            sink()
            list <- list[-i]
        }

    }
    sin <- paste("logs",".txt", sep ="")
    sink(sin, append = TRUE)
    cat("==List of files analized\n")
    print(rev(list))
    cat("==Number of files cut\n")
    cat((100*(og.size-length(list)))/og.size,"%\n")

    cat("\n==BIs calculated\n")
    print(BIs)

    cat("\n==Normalized BIs calculated\n")
    print(BIs.norm)

    ##Id-Indices Min-Max
    # IBmax <- max(BIs)
    # IBmin <- min(BIs)

    # IBmax.norm <- max(BIs.norm)
    # IBmin.norm <- min(BIs.norm)

    #Identificamos el grupo al que pertence cada uno de los waves
    # Groups <- idGroup(BIs,IBmax,IBmin)
    # Groups <- t(matrix(Groups,nrow=2))
    # cat("\n==BIs Groups\n")
    # print(Groups)

    # Groups.norm <- idGroup(BIs.norm,IBmax.norm,IBmin.norm)
    # Groups.norm <- t(matrix(Groups.norm,nrow=2))
    # cat("\n==Normalized BIs Groups\n")
    # print(Groups.norm)

    cat("\n==ACIs calculated\n")
    print(ACIs)
    cat("\n==Normalized ACIs calculated\n")
    print(ACIs.norm)


    Final <- cbind(BIs, ACIs)
    colnames(Final) <- c("BI","ACI")
    cat("\n==Final Table\n")
    print(Final)

    Final.norm <- cbind(BIs.norm, ACIs.norm)
    colnames(Final.norm) <- c("BI","ACI")
    cat("\n==Normalized Final Table\n")
    print(Final.norm)

    correlation <- cor(Final[,"BI"], Final[,"ACI"],method = "spearman")
    cat("\n==Correlation between BIs and ACIs\n")
    print(correlation)

    correlation.norm <- cor(Final.norm[,"BI"], Final.norm[,"ACI"],method = "spearman")
    cat("\n==Normalized Correlation between BIs and ACIs\n")
    print(correlation.norm)

    sink()

    # par(mfrow=c(2,1))
    plot(Final[,"BI"],Final[,"ACI"], main = "Correlation")
    plot(Final.norm[,"BI"],Final.norm[,"ACI"], main = "Normalized Correlation")

    print("Finished")
    return(Final)
}

main()