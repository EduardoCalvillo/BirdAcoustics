library(seewave)
library(tuneR)
library(soundecology)

newACI <- function(wave) {
    output <- ACI(wave)
    cat("Finished calculating ACI...\n")
    return(output)
}

myFFilter <- function(wave, from, to, bandpass) {
    f <- wave@samp.rate
    wave <- ffilter(wave, f= f, from = from, to = to, bandpass = bandpass, output = "Wave")
    cat("Finished filtering noise...\n")
    return(wave)
}

myBI <- function(wave) {
    invisible(capture.output(output <- bioacoustic_index(wave)))
    return(output$left_area)
}

idGroup <- function(BIS, IBmax, IBmin){ 
    dif <- IBmax-IBmin
    Silence <- IBmin
    Low <- Silence + (dif/4)
    Medium <- Low + (dif/4)
    High <- Medium + (dif/4)
    cat("\n==BI group ranges\n")
    cat("Silence",Silence, ", Low",Low, ", Medium",Medium, ", High",High, ", Top",IBmax,"\n")
    output <- c()
    for(x in 1:length(BIS)){
        if(BIS[x] >= Silence && BIS[x] <= Low){ id <- 1}
        if(BIS[x] > Low && BIS[x] <= Medium ){ id <- 2}
        if(BIS[x] > Medium && BIS[x] <= High ){ id <- 3}
        if(BIS[x] > High && BIS[x] <= IBmax){ id <- 4}
        output<- c(output,as.integer(id),BIS[x])
    }
    return(output)
}

logVar <- function(x, var) {
    # print(paste(x,var+1))
    return(log(x+var+1))
}
AE <- function(wave) {
    invisible(capture.output(result <- acoustic_evenness(wave)))
    return(result)
}

main <- function(threshold = 0.85){
    #Init
    BIs <- c()
    ACIs <-c()
    BIs.norm <- c()
    ACIs.norm <-c()

    #Leer Directorio
    p <- "C:/Users/Eduardo Calvillo Uni/Documents/FIME/Topicos Selectos 2/AUDIOS PIA/Partes29Sep/"
    list <- unlist(list.files(path = p, pattern = ".*\\.wav", full.names = TRUE))
    og.size <- length(list)
    # list <- list[20:25]


    #Tratamiento
    for(i in length(list):1){
        cat("Treating wave #",i,"/",length(list),"\n")
        wave.og <- mono(readWave(list[i])
        wave.og <- myFFilter(wave.og,from = 1000, to = 8000, bandpass = TRUE )
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
            #Visualizar sinusoidales
            # par(mfrow=c(3,1))
            # plot(wave.og)
            # plot(wave.norm)
            # plot(wave.abslog)

            #Calcular BI
            BIs <- c(BIs,myBI(wave.og))
            BIs.norm <- c(BIs.norm, myBI(wave.norm))
            #Calcular ACI
            ACIs <- c(ACIs,newACI(wave.og))
            ACIs.norm <- c(ACIs.norm,newACI(wave.norm))
        } else{
            cat(list[i], " didn't pass AEI test.\n")
            list <- list[-i]
        }
    }
    sink("logs29SepFFilter.txt")
    cat("==List of files analized\n")
    print(rev(list))
    cat("==Number of files cut\n")
    cat((100*(og.size-length(list)))/og.size,"%\n")

    cat("\n==BIs calculated\n")
    print(BIs)

    cat("\n==Normalized BIs calculated\n")
    print(BIs.norm)

    ##Id-Indices Min-Max
    IBmax <- max(BIs)
    IBmin <- min(BIs)

    IBmax.norm <- max(BIs.norm)
    IBmin.norm <- min(BIs.norm)

    #Identificamos el grupo al que pertence cada uno de los waves
    Groups <- idGroup(BIs,IBmax,IBmin)
    Groups <- t(matrix(Groups,nrow=2))
    cat("\n==BIs Groups\n")
    print(Groups)

    Groups.norm <- idGroup(BIs.norm,IBmax.norm,IBmin.norm)
    Groups.norm <- t(matrix(Groups.norm,nrow=2))
    cat("\n==Normalized BIs Groups\n")
    print(Groups.norm)

    cat("\n==ACIs calculated\n")
    print(ACIs)
    cat("\n==Normalized ACIs calculated\n")
    print(ACIs.norm)


    Final <- cbind(Groups, ACIs)
    colnames(Final) <- c("Group","BI","ACI")
    cat("\n==Final Table\n")
    print(Final)

    Final.norm <- cbind(Groups.norm, ACIs.norm)
    colnames(Final.norm) <- c("Group","BI","ACI")
    cat("\n==Normalized Final Table\n")
    print(Final.norm)

    correlation <- cor(Final[,"Group"], Final[,"ACI"],method = "spearman")
    cat("\n==Correlation between groups and ACIs\n")
    print(correlation)

    correlation.norm <- cor(Final.norm[,"Group"], Final.norm[,"ACI"],method = "spearman")
    cat("\n==Normalized Correlation between groups and ACIs\n")
    print(correlation.norm)

    # plot(Final[,"Group"],Final[,"ACI"])
    # plot(Final.norm[,"Group"],Final.norm[,"ACI"])

    sink()
    print("Finished")
    return(Final)
}