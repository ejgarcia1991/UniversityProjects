# instalar paquete TSP
library(TSP)

#cargamos el mapa
mapa <-read_TSPLIB("ch150.tsp")
#"ch150.tsp"
#"eil101.tsp"
#""berlin52.tsp""
#mostrar el mapa
plot(mapa,xlab = "coord x", ylab = "coord y")


# metodos disponibles

# "nearest_insertion", "farthest_insertion", "cheapest_insertion"
# "arbitrary_insertion", "nn", "repetitive_nn", "two_opt"

algor = "nearest_insertion"
tour<-solve_TSP(mapa,method=algor)

# mostrar el recorrido y su costo
as.integer(tour)
tour_length(tour)

#grafico
plot(mapa,tour, xlab = "coord x", ylab = "coord y")
