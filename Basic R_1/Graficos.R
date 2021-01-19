x = (0:60)/10
y = sin(x)
plot(y)
plot(x, y) #abre automaticamente
plot(x, y, main= "Funcion Seno")
z  = cos(x)
x11()  #Crea ventana si windows (x11() para linux)
plot(x, z, sub=" z ~ x", main=" Funcion Coseno")
plot(x, y, type = "p");
plot(x, y, type = "l");
plot(x, y, type = "b");
plot(x, y, type = "p", pch=2)
plot(x, y, lty=6, cex=0.5);
plot(x, y,  main="Funs Seno y Coseno",type="l")
lines(x,  z, col="blue",  lty=2) 
text(x=c(0.5,0.5),y=c(0, 1), labels=c("sen(x)", "cos(x)"),col=c("black","blue"))


plot(x, y, main="Funs Seno y Coseno",type="l")
lines(x, z, col="blue",lty=2)
legend(x=3, y=1,legend=c("sen(x)","cos(x)"), lty=c(1,2), col=c("black", "blue"))


plot(x,y,xlab=" texto eje X",ylab="texto eje Y", main=" Plot de X vs Y")    # se vuelve a abrir la ventana
text(1,0.5, "texto en 1,0.5")	# anade texto
mtext(paste("lado",1:4), side= 1:4,line=-1,font=2)   
# anade texto en los margenes pero por dentro (-1)

opar = par(no.readonly=T) # consulta 
n.col  =  2
n.row  =  2
par(mfrow = c(n.row,n.col))
plot(x, y, main="Seno", type="l")
plot(x, z, main="Coseno", lty=2, col="red", type="l")
plot(x, z, main="Coseno", lty=3, col="blue", type="s")
plot(x, z, main="Seno vs Coseno", lty=6, col=5) # cos
lines(x,y)  #seno
par(opar)

par(mar=c(4,4,2,2)) # se gana espacio si no hay titulo
plot(x,y,col=2, xlab=" texto eje X",ylab="texto eje Y", main=" Plot de X vs Y")    

X  =  matrix(rnorm(100), ncol = 2); 
colnames(X)  = c("aAtributo", "bAtributo")
plot(X)

X  = matrix(rnorm(100), ncol=5)
colnames(X)  =  colnames(X) =  paste("a",1:5,sep="")
pairs(X)

# data(iris) con clase 
especie = unclass(iris$Species)
plot(iris[1:2],pch=21,bg = especie+1)
#pairs(~ .,data = iris, main = "Iris Dataset", pch = 21, bg = especie+1)
pairs(iris, main = "Iris Dataset", pch = 21, bg = especie+1)

x  =  rnorm(100,5,3)
hist(x) # genera un histograma
hist(x, main="Histograma", breaks=10)

getwd()                # comprobar el directorio actual
pdf("plot03.pdf")	# cada vez se abre un dispositivo nuevo
hist(x, freq=F)	# histograma
curve(dnorm(x), add=T) # se superpones graficas
dev.off()

dev.list()		# se consulta la lista
dev.off(dev.cur())	# cierra el actual o se espefica número

grados = factor(sample(c("A","B","C","D"),replace=T,20,c(1/7,2/7,3/7,1/7)))
xtabs(~grados)
prop.table(xtabs(~grados))
barplot(xtabs(~grados))
barplot(prop.table(xtabs(~grados)))

dotchart(xtabs(~grados))
pie(xtabs(~grados))
