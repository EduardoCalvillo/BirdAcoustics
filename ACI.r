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

#Leer Directorio
p <- "C:/Users/Eduardo Calvillo Uni/Documents/FIME/Topicos Selectos 2/AUDIOS PIA/Partes/"
list <- unlist(list.files(path = p, pattern = ".*\\.wav", full.names = TRUE))
shortlist <- list[1:5]

#Tratamiento
waves <- lapply(shortlist, readWave)
waves <- unlist(lapply(waves, mono))
waves <- lapply(waves, myFFilter, from = 8000, to = 9200, bandpass = FALSE )

# Verificar que el filtro de frecuencia se haya aplicado
# spec(waves[[2]], f = as.numeric(waves[[2]]@samp.rate), plot = TRUE)

#Calcular BI
BIs <- lapply(waves, myBI)
BIs

# Tratamiento de Amplitudes (Transformación logarítimca)
# amplitudes <- lapply(monoWaves, env, envt = "abs", plot = FALSE)
logAmplitudes <- lapply(amplitudes, log1p)
# log1p(amplitudes[[1]])

# Tratamiento de Frecuencias (Transformación logarítimca)
# specs <- lapply(newlist, myspec)
logFrequencies <- lapply(specs, log1p)

#Calcular ACI
ACIs <- lapply(waves, NewACI)
ACIs