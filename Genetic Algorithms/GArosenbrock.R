library(GA)

# definicion de la funcion objetivo
Rosenbrock <- function(vector){
  if(sum(vector[1:10])>500 || sum(vector[1:10])<200){
    return(9697276000000)
  }
  vector1<-vector+1
  sum<-0
  for(i in 1:(length(vector)-1)){
  sum<-sum + (100*(vector1[i]^2-vector1[i+1])^2+(vector1[i]-1)^2)
  }
  return(sum)
}



fnHim2 <- function (vector){
  # Vector of the parameters
  x <- vector[1]
  y <- vector[2]
  f.x <- (x^2+y-11)^2+(x+y^2-7)^2
  return(f.x)
}




dimension <- 1000
GA1 <- ga(type = "real-valued",
          fitness =  function(x) -Rosenbrock(x),
          lower = rep(-100, dimension), upper = rep(100, dimension),
          popSize = 2000, maxiter = 200,  
          
          elitism = 500,
          
          # tipos de mutacion = {gareal_raMutation,gareal_nraMutation,gareal_rsMutation,gareal_powMutation}
          mutation=gareal_rsMutation,
          
          pmutation = 0.1,
          pcrossover = 0.8,
          # tipos de cruce = {gareal_blxCrossover,gareal_waCrossover,gareal_laplaceCrossover,gareal_spCrossover}
          crossover=gareal_waCrossover,
          
          # tipos de seleccion {gareal_lrSelection,gareal_nlrSelection,gareal_rwSelection,
          #                     gareal_tourSelection, gareal_lsSelection,gareal_sigmaSelection}
          optim=TRUE,
          selection = gareal_lrSelection,
          seed=15334
)
summary(GA1)
plot(GA1)

plot(attributes(GA1)$summary[100:200,1],type="l",xlab="generation",ylab="best fitness")
GA1
