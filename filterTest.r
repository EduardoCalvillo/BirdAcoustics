library(seewave)
library(tuneR)

wave <- mono(readWave("C:/Users/Eduardo Calvillo Uni/Documents/FIME/Topicos Selectos 2/AUDIOS PIA/Partes06Oct/Test 12 .wav"))
# wave <- mono(readWave("C:/Users/Eduardo Calvillo Uni/Documents/FIME/Topicos Selectos 2/AUDIOS PIA/Ruido29Sep/Test 1 .wav"))

wave.filtered <- ffilter(wave, f = as.numeric(wave@samp.rate), from = 2000, to =8000, bandpass = TRUE, output = "Wave")
# wave.filtered <- afilter(wave, f = as.numeric(wave@samp.rate), threshold = )

# listen(wave.filtered)

sp <- spectro(wave.filtered, plot = TRUE)
# locator()
# head(sp$time)
# head(sp$freq)
# length(sp$freq)
# length(sp$amp)
sp$amp
sp$freq
sp$amp[1,257]
sp


delete <- function(x, threshold.min, threshold.max){
    if(x >= threshold.min && x <= threshold.max){
        return(1)
    } else{
        return(0)
    }
}



freq <- length(sp$freq)
time <- length(sp$time)
# time.nams <- seq(from = 1, to = 60, length.out = time)
# # freq.nams <- seq(from = 1, to = 8, lensp$sgth.out = freq)
# time.nams <- unlist(lapply(time.nams,formatC, digits = 1, format = "f"))
# freq.nams <- unlist(lapply(freq.nams, formatC, digits = 1, format = "f"))
# a <- unlist(lapply(c(runif(freq,0,10),runif(time,0,10)), formatC, digits = 2, format = "f"))
sp$amp
# colnames(m) <- freq.nams
# rownames(m) <- time.nams
# z <- mapply(kill, m)
w <- mapply(delete, sp$amp, threshold.min = -23, threshold.max = -10)
y <- matrix(w, ncol = freq, nrow = time)

results <- c()

for(i in 1:time){
    x <- sum(y[i,])/freq
    results <- c(results,x)
}
results
lista <- c()
consec <- 0
for(i in 1:length(results)){
    if(results[i] > 0.2){
        consec <- consec +1
        lista <- c(lista,i)
    } else {
        consec <- 0
        lista <- c()
    }
    if(consec == 5) {
        print("Success")
        print(lista)
        break
    }
}
results
sum(y[6,])