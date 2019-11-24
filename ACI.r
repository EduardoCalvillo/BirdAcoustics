library(seewave)
library(tuneR)

AcCoIn <- function(waves) {
    l <- length(waves)
    out <- c()
    for(i in 1:l){
        out <- c(out,ACI(wave[[i]]))
    }
    return(out)
}

NewACI <- function(wave) {
    output <- ACI(wave)
    print("finished")
    return(output)
}
p <- "C:/Users/Eduardo Calvillo Uni/Documents/FIME/Topicos Selectos 2/AUDIOS PIA/Partes/"
list <- unlist(list.files(path = p, pattern = ".*\\.wav", full.names = TRUE))
waves <- lapply(list, readWave)
monoWaves <- unlist(lapply(waves, mono))
list
ACIs <- lapply(newlist, NewACI)
amplitudes <- lapply(monoWaves, env, envt = "abs", plot = FALSE)
newlist <- monoWaves[1:10]
ACIs
specs <- lapply(newlist, myspec)
head(specs[[1]]) 

waves
# monoACI <- lapply(monoWaves,ACI)
# stereoACI <- lapply(waves, ACI)

log1p(monoWaves[[1]]@left)

log1p(amplitudes[[1]])

logAmplitudes <- lapply(amplitudes, log1p)
logFrequencies <- lapply(specs, log1p)
head(logFrequencies[[1]])
myspec <- function(wave) {
    return(spec(wave, f = as.numeric(wave@samp.rate), plot = FALSE)[,"y"])
}

monoWaves
max(logFrequencies)
lapply(logFrequencies, max)
wave <- waves[[1]]
bb <- wave@left
head(bb)
zero <- function(vector)
destroy <- function(wave) {
    wave@left <- wave@left*0
    return(wave)
}

new <- destroy(wave)
head(new@left)

