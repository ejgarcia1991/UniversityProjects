#Construir el DAG de la red bayesiana
library(bnlearn)
library(gRain)

C<-"Clima"
H<-"Humor"
Ran<-"Ranking"
A<-"Amigos"
Eq<-"Equipo"
Es<-"Estilo_de_Juego"
Fort<-"Fortaleza"
Res<-"Resultado"

dag <- empty.graph(nodes = c(C, H, Ran, A,Eq,Es,Fort,Res))
dag <- set.arc(dag, from = C, to = H)
dag <- set.arc(dag, from = C, to = A)
dag <- set.arc(dag, from = C, to = Ran)
dag <- set.arc(dag, from = C, to = Fort)
dag <- set.arc(dag, from = H, to = Es)
dag <- set.arc(dag, from = H, to = A)
dag <- set.arc(dag, from = A, to = Eq)
dag <- set.arc(dag, from = Ran, to = Fort)
dag <- set.arc(dag, from = Eq, to = Res)
dag <- set.arc(dag, from = Fort, to = Res)
dag <- set.arc(dag, from = Es, to = Res)
dag

nodes(dag)
arcs(dag)


graphviz.plot(dag)

C.st<-c("Lluvia","Claro")
H.st<-c("Bueno","Malo")
Ran.st<-c("Alto","Bajo")
A.st<-c("Disponibles","No disponibles")
Eq.st<-c("Fuerte","Debil")
Es.st<-c("Serio","Casual")
Fort.st<-c("Fuerte","Debil")
Res.st<-c("Victoria","Derrota")

#Crear las tablas de probabilidad
C.prob <- array(c(0.18, 0.82), dim=2,dimnames = list(Clima = C.st))
H.prob <- array(c(0.65, 0.35,0.85,0.15), dim=c(2,2),dimnames = list(Humor = H.st,Clima=C.st))
Ran.prob <- array(c(0.2, 0.8,0.9,0.1), dim=c(2,2),dimnames = list(Ranking = Ran.st,Clima=C.st))
A.prob <- array(c(0.95, 0.05, 0.25, 0.75, 0.35, 0.65, 0.01,0.99), 
                dim=c(2, 2, 2), dimnames = list(Amigos = A.st, Humor = H.st, Clima = C.st))
Eq.prob <- array(c(0.8, 0.2,0.1,0.9), dim=c(2,2),dimnames = list(Equipo = Eq.st,Amigos=A.st))
Es.prob <- array(c(0.85, 0.15,0.3,0.7), dim=c(2,2),dimnames = list(Estilo_de_Juego = Es.st,Humor=H.st))
Fort.prob <- array(c(0.8, 0.2, 0.05, 0.95, 0.9, 0.1, 0.3,0.7), 
                dim=c(2, 2, 2), dimnames = list(Fortaleza = Fort.st, Ranking = Ran.st, Clima = C.st))
Res.prob <- array(c(0.65, 0.35, 0.3, 0.7, 0.9, 0.1, 0.7,0.3,0.4, 0.6, 0.1, 0.9, 0.7, 0.3, 0.5,0.5), 
                   dim=c(2, 2, 2, 2), dimnames = list(Resultado = Res.st, Estilo_de_Juego = Es.st, Equipo = Eq.st,Fortaleza=Fort.st))

cpt <- list(Clima=C.prob, Humor=H.prob, Ranking=Ran.prob, Amigos=A.prob,Equipo=Eq.prob, Estilo_de_Juego=Es.prob,Fortaleza=Fort.prob,Resultado=Res.prob)



bn <- custom.fit(dag,cpt)
nparams(bn)
arcs(bn)
bn$Resultado


#Comprobar D separación
dsep(dag, x = C, y = Res)
dsep(dag, x = Eq, y = Ran)
path(dag, from = Res, to = Es)
dsep(dag, x = A, y = Ran,z=c(H,C))



#Como el conversor a Grain no sirve, tengo que construir la red de nuevo para Grain
listaGRain <-list(~Clima, ~Humor|Clima,~Ranking|Clima,~Amigos|Humor:Clima,~Estilo_de_Juego|Humor,~Fortaleza|Ranking:Clima,~Equipo|Amigos,~Resultado|Equipo : Estilo_de_Juego : Fortaleza) 
dagGRain<-dagList(listaGRain)

C.st<-c("Lluvia","Claro")
H.st<-c("Bueno","Malo")
Ran.st<-c("Alto","Bajo")
A.st<-c("Disponibles","No disponibles")
Eq.st<-c("Fuerte","Debil")
Es.st<-c("Serio","Casual")
Fort.st<-c("Fuerte","Debil")
Res.st<-c("Victoria","Derrota")

C.CPT <- cptable(~Clima, values=c(18, 82), levels=C.st)
H.CPT <- cptable(~Humor+Clima, values=c(65, 35,85,15), levels=H.st)
Ran.CPT <- cptable(~Ranking+Clima, values=c(20, 80, 90, 10), levels=Ran.st)
A.CPT <- cptable(~Amigos+Humor+Clima, values=c(95, 5, 25, 75,35,65,1,99), levels=A.st)
Eq.CPT <- cptable(~Equipo+Amigos, values=c(80, 20, 10, 90), levels=Eq.st)
Es.CPT <- cptable(~Estilo_de_Juego+Humor, values=c(85, 15, 30, 70), levels=Es.st)
Fort.CPT <- cptable(~Fortaleza+Ranking+Clima, values=c(80, 20, 5, 95, 90, 10, 30, 70), levels=Fort.st)
Res.CPT <- cptable(~Resultado+Equipo+Estilo_de_Juego+Fortaleza, values=c(65, 35, 30, 70, 90, 10, 70, 30, 40, 60, 10, 90,70,30,50,50), levels=Res.st)



potentialList <- compileCPT(list(C.CPT, H.CPT, Ran.CPT, A.CPT, Eq.CPT, Es.CPT,Fort.CPT,Res.CPT ))

redGrain <- grain(potentialList)
GrainNetwork <- compile(redGrain)


#Hacer al menos tres consultas de la probabilidad a posteriori dada
#cierta evidencia, de alguna variable, usando el método exacto y los
#dos métodos aproximados

queryExact1 <- setEvidence(GrainNetwork, nodes = c("Estilo_de_Juego"), states = "Serio")
queryExact1 <- setEvidence(queryExact1, nodes = c("Fortaleza"), states = "Debil")
queryExact1 <- setEvidence(queryExact1, nodes = c("Equipo"), states = "Fuerte")
querygrain(queryExact1, nodes = "Clima")
querygrain(queryExact1, nodes = "Ranking")

queryExact2 <- setEvidence(GrainNetwork, nodes = c("Resultado"), states = "Derrota")
queryExact2 <- setEvidence(queryExact2, nodes = c("Fortaleza"), states = "Fuerte")
querygrain(queryExact2, nodes = "Amigos")
querygrain(queryExact2, nodes = "Humor")


queryApprox1<-cpquery(bn, event = (Humor == "Malo") ,evidence = ((Estilo_de_Juego == "Serio") & (Clima == "Lluvia")),n = 10^6)
queryApprox1

queryApprox2<-cpquery(bn, event = (Ranking == "Bajo") & (Clima=="Claro"),evidence = list(Fortaleza="Fuerte",Amigos="Disponibles"), method = "lw")
queryApprox2


