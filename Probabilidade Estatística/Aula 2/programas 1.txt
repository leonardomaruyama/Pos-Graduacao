#Declarando Objetos
idade <- c(23, 22, 30, 27, 19, 25, 26, 22)
idade

idade <- scan()
23 22 30 27 19 25 26 22
idade

peso <- c(65, 70, 62, 68, 74, 59, 68, 75)
peso

altura <- c(1.75, 1.65, 1.69, 1.7, 1.72, 1.64, 1.6, 1.65)
altura

sexo <- c("M", "F", "M", "M", "F", "F", "M", "M")
sexo

estado <- c("solteiro", "casado", "solteiro", "casado", "solteiro",
"casado", "casado", "solteiro")

dados <- data.frame(idade,peso,altura,sexo,estado)
dados

#########Gráficos#########

###1-Representação gráfica: dados categóricos

###Gráficoem barras###
barplot(c(0.4333333,0.5666667),xlab="Status de clientes.",col=c("darkblue","green"),names.arg=c("Fora das Espec.", "Dentro das Espec."),beside=T,ylim=c(0,1),xlim=c(0,4),legend = c("Fora das Espec.", "Dentro das Espec."))

###Gráfico em setores###
pie(c(13,27), col=c("blue","white"),labels=c("Fora das Especificações", "Dentro das Especificações"), main="Produto")

###2-Representação gráfica: dados quantitativos

###Histograma###
set.seed(2015)
x<-rgamma(300,6.25,0.25)
hist(x, freq=F, right=F, main="Histograma da Idade",
Xlab="anos", ylab="dens.freq.rel", density=6, col="red")

X=c(10.8,5.3,4.0,13.9,6.0,8.3,11.3,2.2,9.3,3.0,5.7,8.8,6.0,3.8,5.8,
13.2,13.0,8.0,7.7,6.4,3.4,7.6,9.3,7.9,1.3,11.7,19.6,10.5,7.4,6.3,
7.0,7.4,7.9,9.5,7.1,8.4,8.0,4.7,2.5,8.6,3.7,10.8,3.6,5.9,5.5,
4.2,1.9,6.5,11.4,6.2)

hist(X, main="Histograma dos tempos de falha")

require(ggplot2)
###Histograma###
qplot(X, geom="histogram", main="Histograma dos tempos de falha",xlab ="Tempo",ylab ="Frequência")

###Polígono de frequência acumulada###

y<-cumsum(rep(1,length(X)))

##Crescente
plot(sort(X),y,type="o",col="blue", lwd=1, xlab=" ", ylab="",main="", pch=19)

#Descrescente
plot(sort(X),sort(y,decreasing = TRUE),type="o",col="blue", lwd=1, xlab=" ", ylab="",main="", pch=19)

###Gráfico temporal###
publica <-c(45.9,45.5,41.0,37.1,38.2,33.7,33.3,34.06,32.15,31.82,27.92,27.18)

privada<-c(54.1,54.5,59.0,62.8,61.8,66.3,66.7,65.94,67.85,68.17,72.08,72.82)

anos<-c(1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005)

plot(anos, publica,type="o",col="blue", lwd=1,ylim=c(0,90),lty=1, xlab="Anos", ylab="Percentual",
main=" ", pch=19, xlim=c(1994,2005))
points(anos, privada, type="o", col="red", lwd=2,pch=19)
legend(2002, 91, lwd=3, cex=1, c("Publica", "Privada"),
col=c("blue","red"),lty=1)


###Box-plot###
boxplot(publica,privada, range = 1.5,col=c("darkblue","green"),
main="Boxplot para Duracao",names=c("Publica", "Privada"),legend = c("bom","ruin"), horizontal=FALSE)

col=c("blue","red"))

X2<-c(11.5,5.1,8.7,7.3,2.2,6.6,13.2,6.4,8.4,9.9,9.1,8.1,10.0,12.6,3.6,
12.3,12.4,6.9,6.3,10.2,3.5,16.6,5.9,6.8,3.8,6.4,5.7,10.5,5.6,6.8,
15.4,8.5,15.8,6.5,6.0,10.1,9.6,9.1,3.8,8.0,6.6,3.9,5.6,7.7,7.1,
6.1,8.7,11.0,6.1,5.7)

###Boxplot###
qplot(c(rep("Falha 1",length(X)),rep("Falha 2",length(X2))),c(X,X2), geom="boxplot", main="Boxplot do tempo de falha",ylab ="Tempo",xlab="Tipo de falha")



#Medidas de Dispersão

#Declarando os dados
x<-c(1.850,1.900,1.350,1.750,1.700,1.500,1.650,1.700)
n<-length(x)

#Média
mean(x) ; sum(x)/n
#Desvio Absoluto
sum(abs(x-mean(x)))/n
#Variância
var(x) ; sum((x-mean(x))^2)/(n-1) ; sum((x-mean(x))^2)/n
#Coeficiente de Variação
sd(x)/mean(x)

desviop<-function(x){
sqrt(sum((x-mean(x))^2)/(n-1)) }
desviop(x)
sd(x)


## Dados
A=c(30,30,30,30,30)
B=c(60,10,10,10,60)
C=c(20,40,20,40,30)
D=c(10,20,30,40,50)
E=c(10,20,90,20,10)

# Estatísticas

length(A) # traz o numero de dados do conjunto A

A # permite rever os elementos indexados em A

sort(B) # ordena os elemento do conjunto B

mean(C) # retorna o valor medio de C

median(C) # retorna a mediana de C

quantile(C) # retorna os quantis 0, 25%, 50%, 75% e 100%

quantile(C, probs = 0.4, na.rm = FALSE,names = FALSE,type = 7)

summary(C) # fornece medidas de quantil

max(C)-min(C) # permite obter a amplitude do conjunto

var(C) # retorna a vari^ancia do conjunto

sd(C) # retorna o desvio padrao

# Para calcular o desvio medio
desvmed=mean(abs(C-mean(C)))
desvmed # fornece o desvio medio

# Coeficiente de Variac~ao Populacional de C
sqrt((sum((C-mean(C))^2))/length(C))/mean(C)

#Q-Q Plot 
h<-rnorm(100,0,1)
qqnorm(h,main="Q-Q Plot h")
abline(0,1)

#Q-Q Plot dos conjuntos A-F em uma unica figura.
par(mfrow=c(3,2))
qqnorm(A,main="Q-Q Plot A")
qnorm(B,main="Q-Q Plot B")
qqnorm(C,main="Q-Q Plot C")
qqnorm(D,main="Q-Q Plot D")
qqnorm(E,main="Q-Q Plot E")
qqnorm(F,main="Q-Q Plot F")



moda<-function(C) { # C e o vetor que deseja calcular a moda
D=sort(C) # Comando para ordenar os dados
D1=numeric()
D1=D[1]
l1<-1
for (i in 2:length(C) ){
if (D[i]!=D[i-1])
{
l1<-l1+1
D1[l1] <- D[i]
} # end if
} #end for
contador<-as.numeric(0)
for (i in 1:length(D1)){
cont=0
for (j in 1:length(C)){
if(D1[i]==C[j]) cont=cont+1
} #end for
contador[i]=cont
}# end for
lm <-0
moda<-numeric()
for (i in 1:length(D1)){
if(contador[i]==max(contador))
{ lm<-lm+1
moda[lm]<-D1[i] } # end if
} # end for
list(moda=moda)
} #end function
#Apos implementado a func~ao moda, basta fazer:
moda (C) # tera a moda do conjunto C
moda (E) # tera a moda do conjunto E


library(ggplot2)

#the doughnut function permits to draw a donut plot
doughnut <-
function (x, labels = names(x), edges = 200, outer.radius = 0.8, 
          inner.radius=0.6, clockwise = FALSE,
          init.angle = if (clockwise) 90 else 0, density = NULL, 
          angle = 45, col = NULL, border = FALSE, lty = NULL, 
          main = NULL, ...)
{
    if (!is.numeric(x) || any(is.na(x) | x < 0))
        stop("'x' values must be positive.")
    if (is.null(labels))
        labels <- as.character(seq_along(x))
    else labels <- as.graphicsAnnot(labels)
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L])
        xlim <- (pin[1L]/pin[2L]) * xlim
    else ylim <- (pin[2L]/pin[1L]) * ylim
    plot.window(xlim, ylim, "", asp = 1)
    if (is.null(col))
        col <- if (is.null(density))
          palette()
        else par("fg")
    col <- rep(col, length.out = nx)
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    angle <- rep(angle, length.out = nx)
    density <- rep(density, length.out = nx)
    twopi <- if (clockwise)
        -2 * pi
    else 2 * pi
    t2xy <- function(t, radius) {
        t2p <- twopi * t + init.angle * pi/180
        list(x = radius * cos(t2p), 
             y = radius * sin(t2p))
    }
    for (i in 1L:nx) {
        n <- max(2, floor(edges * dx[i]))
        P <- t2xy(seq.int(x[i], x[i + 1], length.out = n),
                  outer.radius)
        polygon(c(P$x, 0), c(P$y, 0), density = density[i], 
                angle = angle[i], border = border[i], 
                col = col[i], lty = lty[i])
        Pout <- t2xy(mean(x[i + 0:1]), outer.radius)
        lab <- as.character(labels[i])
        if (!is.na(lab) && nzchar(lab)) {
            lines(c(1, 1.05) * Pout$x, c(1, 1.05) * Pout$y)
            text(1.1 * Pout$x, 1.1 * Pout$y, labels[i], 
                 xpd = TRUE, adj = ifelse(Pout$x < 0, 1, 0), 
                 ...)
        }
        ## Add white disc          
        Pin <- t2xy(seq.int(0, 1, length.out = n*nx),
                  inner.radius)
        polygon(Pin$x, Pin$y, density = density[i], 
                angle = angle[i], border = border[i], 
                col = "white", lty = lty[i])
    }
 
    title(main = main, ...)
    invisible(NULL)
}

doughnut( c(6,7,13) , inner.radius=0.5, col=c("firebrick2","goldenrod1","dodgerblue4"))



