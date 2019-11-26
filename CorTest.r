paquete <- 5 #Número de muestras
nrow = paquete*4 #Hay 4 divisiones (Sin, Bajo, Medio, Alto)
qty <- 10 #Numero de loops
for(i in 1:qty){
    cat("Test", i,"\n")
    y <- c(rep(0,paquete),rep(1,paquete),rep(2,paquete),rep(3,paquete)) # 0 Sin Presencia, 1 Baja Presencia, 2 Media Presencia, 3 Alta Prescencia
    x <- c(runif(paquete,0,50),runif(paquete,50,100),runif(paquete,100,150),runif(paquete,150,200)) #Numeros aleatorios representando ACIs pero con limites marcados por nivel de prescencia
    # matrix(c(y,x), nrow = nrow)
    z <- c(runif(paquete,0,50),runif(paquete,0,100),runif(paquete,0,150),runif(paquete,0,200)) #Numeros que solo ascienden
    # matrix(c(y,z), nrow = nrow)
    w <- runif(nrow,0,200) #Numeros completamente aleatorios 
    # matrix(c(y,w), nrow= nrow)
    print(cor(x,y, method = "spearman")) #Esto siempre tiene correlación >.9, lo cual es correcto a lo que deseamos, hay una relación ASCENDENTE
    print(cor(z,y, method = "spearman")) #Esto da valores variados entre .5 y .7, lo cual no es ideal pero al menos muestra relacion ascendente.
    print(cor(w,y, method = "spearman")) #Esto da valores muy aleatorios. Muchas veces cercanos a 0, lo cual indica que NO hay correlación
}
