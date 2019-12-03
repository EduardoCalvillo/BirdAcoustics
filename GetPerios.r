#Leer Directorio
    p <- "C:/Users/Eduardo Calvillo Uni/Documents/FIME/Topicos Selectos 2/AUDIOS PIA/AllParts/"
    list <- unlist(list.files(path = p, full.names = TRUE))
    l <- length(list)
    x<- runif(9, 1, l)
    for(i in 1:length(x)){
        wave <- readWave(list[x[i]])
        plot(periodogram(wave))
        vin <- readline()
    }

    l[1]    