library(seewave)
library(tuneR)
library(soundecology)

AE <- function(wave) {
    invisible(capture.output(result <- acoustic_evenness(wave)))
    return(result)
}

TE <- function(wave) {
    return(th(env(wave, plot=FALSE)))
}

AD <- function(wave) {
    invisible(capture.output(result <- acoustic_diversity(wave)))
    return(result)
}

NDS <- function(wave) {
    return(NDSI(soundscapespec(wave, plot=FALSE)))
}

logVar <- function(x, var) {
    # print(paste(x,var+1))
    return(log(x+var+1))
}

#Leer Directorio
p.bird <- "C:/Users/Eduardo Calvillo Uni/Documents/FIME/Topicos Selectos 2/AUDIOS PIA/Partes/"
list.bird <- unlist(list.files(path = p.bird, pattern = ".*\\.wav", full.names = TRUE))


p.noise <-"C:/Users/Eduardo Calvillo Uni/Documents/FIME/Topicos Selectos 2/AUDIOS PIA/Ruido/"
list.noise<- unlist(list.files(path = p.noise, pattern = ".*\\.wav", full.names = TRUE))


AE.birdTotal <- 0
AE.noiseTotal <- 0
AD.birdTotal <- 0
AD.noiseTotal <- 0
TE.birdTotal <- 0
TE.noiseTotal <- 0
NDSI.birdTotal <- 0
NDSI.noiseTotal <- 0

sink("indexTest.txt")
for(i in 1:min(length(list.bird),length(list.noise))){
    wave.bird <- mono(readWave(list.bird[i]))
    wave.noise <- mono(readWave(list.noise[i]))

    m.bird<- abs(min(wave.bird@left))
    wave.bird.left <- unlist(lapply(wave.bird@left, logVar, var = m.bird))
    wave.bird <- Wave(left = wave.bird.left, samp.rate = as.numeric(wave.bird@samp.rate), bit = 16)
    
    m.noise<- abs(min(wave.noise@left))
    wave.noise.left <- unlist(lapply(wave.noise@left, logVar, var = m.noise))
    wave.noise <- Wave(left = wave.noise.left, samp.rate = as.numeric(wave.noise@samp.rate), bit = 16)


    cat("\n Test #",i,"\n")

    cat(" mean bird: ",mean(wave.bird@left), "\n mean noise: ", mean(wave.noise@left), "\n")

    # AE.bird <- AE(wave.bird)$aei_left
    # AE.noise <- AE(wave.noise)$aei_left
    # cat(" AE bird: ", AE.bird, "\n AE noise: ", AE.noise, "\n")
    # AE.birdTotal <- AE.birdTotal+AE.bird
    # AE.noiseTotal <- AE.noiseTotal+AE.noise

    # NDSI.bird <- NDS(wave.bird)
    # NDSI.noise <- NDS(wave.noise)
    # cat(" NDSI bird: ", NDSI.bird, "\n NDSI noise: ", NDSI.noise, "\n")
    # NDSI.birdTotal <- NDSI.birdTotal+NDSI.bird
    # NDSI.noiseTotal <- NDSI.noiseTotal+NDSI.noise

    # AD.bird <- AD(wave.bird)$adi_left
    # AD.noise <- AD(wave.noise)$adi_left
    # cat(" AD bird: ", AD.bird, "\n AD noise: ", AD.noise, "\n")
    # AD.birdTotal <- AD.birdTotal+AD.bird
    # AD.noiseTotal <- AD.noiseTotal+AD.noise

    # TE.bird <- TE(wave.bird)
    # TE.noise <- TE(wave.noise)
    # cat(" TE bird: ", TE.bird, "\n TE noise: ", TE.noise, "\n")
    # TE.birdTotal <- TE.birdTotal+TE.bird
    # TE.noiseTotal <- TE.noiseTotal+TE.noise



}
cat(" AEI bird: ", AE.birdTotal, "\n AEI noise: ", AE.noiseTotal, "\n")
cat(" NDSI bird: ", NDSI.birdTotal, "\n NDSI noise: ", NDSI.noiseTotal, "\n")
cat(" ADI bird: ", AD.birdTotal, "\n ADI noise: ", AD.noiseTotal, "\n")
cat(" TEI bird: ", TE.birdTotal, "\n TEI noise: ", TE.noiseTotal, "\n")
sink()


