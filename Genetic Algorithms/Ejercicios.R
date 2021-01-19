# read D bits from integer x:
#5.1
binint=function(x,D)
{ x=rev(intToBits(x)[1:D]) # get D bits
# remove extra 0s from raw type:
as.numeric(unlist(strsplit(as.character(x),""))[(1:D)*2])
}

D=16
maxsin=function(x) -sin(pi*(intbin(x))/(2^D))
G=rbga.bin(size=D,popSize=20,iters=100,evalFunc=maxsin,elitism=1)

summary(G,echo=TRUE)
-G$best

#5.4
D=2
set.seed(123)
c1=sample(100,1)
set.seed(1236)
c2=sample(100,1)

eggholder=function(pop) -(pop[2] + c1)*sin(sqrt(abs((pop[2]+pop[1])/c2+c1)))-pop[1]*sin(sqrt(abs(pop[1]-pop[2]+c1))) #eval function
eggholder2=function(pop) -(pop[2] + sample(100,1))*sin(sqrt(abs((pop[2]+pop[1])/sample(100,1)+sample(100,1))))-pop[1]*sin(sqrt(abs(pop[1]-pop[2]+sample(100,1)))) #eval function

E=rbga(rep(-512,D),rep(512,D),popSize=500,evalFunc=eggholder)
summary(E,echo=TRUE)
E$best[length(E$best)]
