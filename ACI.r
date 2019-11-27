library(seewave)
library(tuneR)
library(soundecology)

NewACI <- function(wave) {
    output <- ACI(wave)
    cat("finished\n")
    return(output)
}

myFFilter <- function(wave, from, to, bandpass) {
    f <- wave@samp.rate
    wave <- ffilter(wave, f= f, from = from, to = to, bandpass = bandpass, output = "Wave")
    cat("finished\n")
    return(wave)
}

myspec <- function(wave) {
    cat("finished\n")
    return(spec(wave, f = as.numeric(wave@samp.rate), plot = FALSE)[,"y"])
}

myBI <- function(wave) {
    invisible(capture.output(output <- bioacoustic_index(wave)))
    return(output$left_area)
}

SplitBIs <- function(wave){
    cat("Recibo"+""+ wave)
}

Idmax <- function(BIS){
    Mayor <- 0
    for(x in 1:length(BIS)){
        if(BIS[[x]] > Mayor ){
            Mayor <- BIS[[x]]
        }
    } 
    return(Mayor)
}

Idmin <- function(BIS, IBmax){
    Menor <- IBmax
    for(x in 1:length(BIS)){
        
        if(BIS[[x]] < Menor ){
            Menor <- BIS[[x]]
            print(BIS[[x]])
        }
    } 
    return(Menor)
}

IdGroup <- function(BIS, IBmax, IBmin){ 
    Silence <- IBmin
    Low <- Silence + ((IBmax-IBmin)/4)
    Means <- Low + ((IBmax-IBmin)/4)
    Tall <- IBmax
    output <- c()

   for(x in 1:length(BIS)){
        if(BIS[[x]] <= Silence){ id <- 1}
        if(BIS[[x]] > Silence && BIS[[x]] <= Low ){ id <- 2}
        if(BIS[[x]] > Low && BIS[[x]] <= Means ){ id <- 3}
        if(BIS[[x]] > Means){ id <- 4}
    
       output<- c(output,BIS[[x]],as.integer(id))
   }

    return(output)
    
}

#Leer Directorio
##p <- "C:/Users/Eduardo Calvillo Uni/Documents/FIME/Topicos Selectos 2/AUDIOS PIA/Partes/"
p <- "C:/Users/etame/OneDrive/Escritorio/Escuela/Escuela/Acustica/PIA/Session_5/"
list <- unlist(list.files(path = p, pattern = ".*\\.wav", full.names = TRUE))
shortlist <- list[1:8]

#Tratamiento
waves <- lapply(shortlist, readWave)
waves <- unlist(lapply(waves, mono))
waves <- lapply(waves, myFFilter, from = 8000, to = 9200, bandpass = FALSE )


# Verificar que el filtro de frecuencia se haya aplicado
# spec(waves[[2]], f = as.numeric(waves[[2]]@samp.rate), plot = TRUE)

#Calcular BI
BIs <- lapply(waves, myBI)
BIs

##Id-Indices Min-Max
IBmax <- Idmax(BIs)
IBmin <- Idmin(BIs, IBmax)

#Identificamos el grupo al que pertencere
#Cada uno de los waves
Groups <- IdGroup(BIs,IBmax,IBmin)
Groups

# Tratamiento de Amplitudes (Transformación logarítimca)
##amplitudes <- lapply(monoWaves, env, envt = "abs", plot = FALSE)
logAmplitudes <- lapply(amplitudes, log1p)
##log1p(amplitudes[[1]])

# Tratamiento de Frecuencias (Transformación logarítimca)
# specs <- lapply(newlist, myspec)
logFrequencies <- lapply(specs, log1p)

#Calcular ACI
ACIs <- lapply(waves, NewACI)
ACIs




