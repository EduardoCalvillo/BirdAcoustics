AcCoIn <- function(waves) {
    l <- length(waves)
    out <- c()
    for(i in 1:l){
        out <- c(out,ACI(wave[[i]]))
    }
    return(out)
}