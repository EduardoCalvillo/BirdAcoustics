library(seewave)
library(tuneR)

delete <- function(x, threshold.min, threshold.max){
    if(x >= threshold.min && x <= threshold.max){
        return(1)
    } else{
        return(0)
    }
}

sumOfSuccesses <- function(matrix){
    list <- c()
    for(i in 1:length(matrix[,1])){
        x <- sum(matrix[i,])
        list <- c(list,x)
    }
    return(list)
}

findThreshold <- function(delta, t1, t2) {
    return(ceiling(delta/(t2-t1)))
}

findBird <- function(list, threshold) {
    res <- c()
    consec <- 0
    flag <- 0
    for(i in 1:length(list)){
        if(list[i] != 0){
            consec <- consec + 1
            res <- c(res,i)
        } else {
            consec <- 0
            res <- c()
        }
        if(consec == threshold) {
            flag <- 1
            break
        }
    }
    if(flag == 1){
        print("Success")
        return(res)
    } else {
        print("Failure")
        return(NA)
    }
}

wave <- mono(readWave("C:/Users/Eduardo Calvillo Uni/Documents/FIME/Topicos Selectos 2/AUDIOS PIA/Partes06Oct/Test 12 .wav"))
# wave <- mono(readWave("C:/Users/Eduardo Calvillo Uni/Documents/FIME/Topicos Selectos 2/AUDIOS PIA/Ruido29Sep/Test 1 .wav"))

wave.filtered <- ffilter(wave, f = as.numeric(wave@samp.rate), from = 2000, to =8000, bandpass = TRUE, output = "Wave")
sp <- spectro(wave.filtered, plot = FALSE)
dbMin <- -23
dbMax <- -10
transformed.vector <- mapply(delete, sp$amp, threshold.min = dbMin, threshold.max = dbMax)
transformed.matrix <- matrix(transformed.vector, ncol = length(sp$freq), nrow = length(sp$time))

list <- sumOfSuccesses(transformed.matrix)
delta <- 0.5 #medio seegundo continuo de sonido
threshold <- findThreshold(delta, sp$time[1], sp$time[2])
isOk <- findBird(list,threshold)