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

validateThreshold <- function(x, threshold.min, threshold.max){
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