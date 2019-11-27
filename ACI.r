library(seewave)
library(tuneR)
library(soundecology)

NewACI <- function(wave) {
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

myspec <- function(wave) {
    cat("Finished getting spec\n")
    return(spec(wave, f = as.numeric(wave@samp.rate), plot = FALSE)[,"y"])
}

myBI <- function(wave) {
    invisible(capture.output(output <- bioacoustic_index(wave)))
    return(output$left_area)
}

IdGroup <- function(BIS, IBmax, IBmin){ 
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
BIs <- c()
ACIs <-c()
sink("logs.txt")
#Leer Directorio
p <- "C:/Users/Eduardo Calvillo Uni/Documents/FIME/Topicos Selectos 2/AUDIOS PIA/Partes/"
list <- unlist(list.files(path = p, pattern = ".*\\.wav", full.names = TRUE))
# list <- list[20:25]
cat("==List of files analized\n")
print(list)
#Tratamiento
sink()
for(i in 1:length(list)){
    cat("Treating wave #",i,"\n")
    wave <- mono(readWave(list[i]))
    wave <- myFFilter(wave,from = 8000, to = 9200, bandpass = FALSE )
    # Verificar que el filtro de frecuencia se haya aplicado
    # spec(waves[[2]], f = as.numeric(waves[[2]]@samp.rate), plot = TRUE)

    #Calcular BI
    BIs <- c(BIs,myBI(wave))

    # Tratamiento de Amplitudes (Transformación logarítimca)
    # amplitudes <- lapply(monoWaves, env, envt = "abs", plot = FALSE)
    # logAmplitudes <- lapply(amplitudes, log1p)
    # log1p(amplitudes[[1]])

    # Tratamiento de Frecuencias (Transformación logarítimca)
    # specs <- lapply(newlist, myspec)
    # logFrequencies <- lapply(specs, log1p)

    #Calcular ACI
    ACIs <- c(ACIs,NewACI(wave))
}
sink("logs.txt", append = TRUE)

cat("\n==BIs calculated\n")
print(BIs)

##Id-Indices Min-Max
IBmax <- max(BIs)
IBmin <- min(BIs)

#Identificamos el grupo al que pertencere
#Cada uno de los waves
Groups <- IdGroup(BIs,IBmax,IBmin)
Groups <- t(matrix(Groups,nrow=2))
cat("\n==BIs Groups\n")
print(Groups)

cat("\n==ACIs calculated\n")
print(ACIs)

Final <- cbind(Groups, ACIs)
colnames(Final) <- c("Group","BI","ACI")
cat("\n==Final Table\n")
print(Final)

correlation <- cor(Final[,"Group"], Final[,"ACI"],method = "spearman")
cat("\n==Correlation between groups and ACIs\n")
print(correlation)
# plot(Final[,"Group"],Final[,"ACI"])

sink()