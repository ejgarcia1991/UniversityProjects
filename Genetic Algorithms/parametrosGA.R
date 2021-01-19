# optimizacion de la funcion Rastrigin 
# con el paquete GA (instalarlo) 

library(GA)

# definicion de la funcion objetivo
Ras <- function(x) {
  sum(x^2 - 10 * cos(2 * pi  * x)) + 10 * length(x)
}

fnHim2 <- function (vector){
  # Vector of the parameters
  x <- vector[1]
  y <- vector[2]
  f.x <- (x^2+y-11)^2+(x+y^2-7)^2
  return(f.x)
}


dimension <- 2
GA1 <- ga(type = "real-valued",
         fitness =  function(x) -eggholder(x),
         #fitness =  function(x) -fnHim2(x),
         lower = rep(-512, dimension), upper = rep(512, dimension),
         popSize = 300, maxiter = 50,  
        
         elitism = 50,
         
         # tipos de mutacion = {gareal_raMutation,gareal_nraMutation,gareal_rsMutation,gareal_powMutation}
         mutation=gareal_powMutation,
         
         pmutation = 0.05,
         pcrossover = 0.8,
         # tipos de cruce = {gareal_blxCrossover,gareal_waCrossover,gareal_laplaceCrossover,gareal_spCrossover}
         crossover=gareal_blxCrossover,
         
         # tipos de seleccion {gareal_lrSelection,gareal_nlrSelection,gareal_rwSelection,
         #                     gareal_tourSelection, gareal_lsSelection,gareal_sigmaSelection}
         optim=TRUE,
         selection = gareal_tourSelection,
         seed=15334
              )
summary(GA1)
plot(GA1)

