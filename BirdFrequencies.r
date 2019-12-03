library(seewave)
library(tuneR)

prevPotenceOf2 <- function(x) {
    return(2^floor(log2(length(x))))
}

sumFrecuencies <- function(frecuencies, percentages,delta){
    cat("\nTable frecuencies\n")
    nmbrGroups <- ceiling(max(frecuencies)/delta)
    perio <- matrix(nrow = nmbrGroups,ncol = 2)    
    colnames(perio) <- c("Freq (Hz)","%")
    j <- 0
    k <- 1

    while(k <= nmbrGroups){
        perio[k,1] <- j
        perio[k,2] <- 0
        k <- k+1
        j <- j+delta
    }
    for(i in 1:length(frecuencies)){
            group<-ceiling(frecuencies[i]/delta) #Calculating in which group frecuency i belongs
            # print(group)
            perio[group,2] <- perio[group,2]+percentages[i]
    }
    perio[,2] <- unlist(lapply(perio[,2],formatC, digits = 4, format = "f"))
    return(perio)
}

main <- function() {
    #Leer Directorio
    p <- "C:/Users/Eduardo Calvillo Uni/Documents/FIME/Topicos Selectos 2/AUDIOS PIA/Aves_Chipinque/"
    list <- unlist(list.files(path = p, pattern = ".*\\.mp3", full.names = FALSE))
    # list <- list[20:25]
    sink("BirdFreqs.txt")
    for(i in 1:length(list)){
        wave <- mono(readMP3(paste(p,list[i], sep="")))
        wave <- resamp(wave, f = wave@samp.rate, g = wave@samp.rate/2, output = "Wave")
        pgram <- periodogram(wave)
        # plot(pgram@freq, unlist(pgram@spec), main = list[i])
        # stop <- readline()
        a<-sumFrecuencies(pgram@freq, unlist(pgram@spec),500)
        b <- a[order(a[,2], decreasing = TRUE),]
        print(list[i])
        print(b)
    }
    sink()
}
wave <- mono(readMP3(paste(p,list[8], sep="")))
wave <- resamp(wave, f = wave@samp.rate, g = wave@samp.rate/2, output = "Wave")
pgram <- periodogram(wave2, width = length(wave2))
length(pgram@freq)
length(unlist(pgram@spec))