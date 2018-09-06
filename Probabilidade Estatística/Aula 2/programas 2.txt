##Dados 1##

#Suponha que foi realizado um teste a cego objetivando-se avaliar a qualidade de um produto
#que será lançado no mercado, para isto, selecionamos 100 participantes e os dividimos de forma 
#aleatória em dois grupos, primeiro grupo recebeu o produto padrão que esta disponível no mercado, 
#por outro lado o segundo grupo recebeu o novo produto. Podemos afirmar com um nível $95%$ de 
#significância que há diferença entre os produtos.

##Dados 1##
x1<-c(6.9,6.6,7.1,6.7,7.2,4.7,7.0,7.9,2.7,7.1,4.4,7.4,6.5,4.9,7.2,6.0,7.1,8.0,5.1,
5.9,9.4,7.6,5.4,7.8,5.1,8.0,7.1,8.1,7.8,5.3,8.2,7.7,7.6,8.0,5.9,6.5,5.1,6.3,
7.6,7.1,7.8,7.6,4.6,7.8,4.1,7.2,5.8,5.7,7.7,6.3)

##Estatisticas,Descritivas
summary(x1)

##Histograma###
hist(x1)

##Boxplot##
boxplot(x1)

##Dados 2##
x2<-c(8.8,7.0,9.2,9.8,6.9,7.1,8.9,7.0,9.4,9.6,6.1,6.1,7.4,7.6,8.5,9.8,7.9,8.0,7.4,
8.1,6.7,6.3,8.6,7.6,5.7,9.6,7.1,7.1,7.2,6.5,8.5,5.4,8.5,7.0,7.0,6.8,7.3,8.3,
7.5,7.0,7.6,9.6,6.6,8.0,6.4,8.2,7.4,7.6,8.8,7.0)

##Estatisticas,Descritivas
summary(x2)

##Histograma###
hist(x2)

##Boxplot##
boxplot(x2)

boxplot(x1,x2, ylab="Escore", range = 1.5,col=c("deepskyblue4","gray70"),
main="Boxplot da pontuação entre os dois produtos",names=c("Padrão", "Proposto"), horizontal=FALSE)

###Teste de Normalidade para X1##
shapiro.test(x1)

##Teste de Normalidade para X2##
shapiro.test(x2)

##Teste da dados não paramétricos, testa os valores da mediana
wilcox.test(x1,x2)

##############################
##Se os dados são normais#####
##########EXEMPLO#############
##############################

##Valores aleatórios mas com as mesmas observações
set.seed(2018)

##Gera valores de uma normal##
y1<-rnorm(100,5,1)

##Gera valores de uma normal##
y2<-rnorm(100,7,1)

###Teste Normalidade de Y1####
shapiro.test(y1)
###Teste Normalidade de Y2####
shapiro.test(y2)

#Testa igualdade das variâncias#
var.test(y1,y2)

##Realizando o teste t para igualdade de média
t.test(y1,y2,var.equal = TRUE)


#Suponha que uma empresa esteja interessada em melhorar seu nível de satifação com seus clientes.
Alguns procedimentos foram definidos para serem implementados objetivando-se melhorar tal experiência
para isto, um grupo de 40 candidatos foram selacionados e apresentaram uma avaliação de satisfação
antes e após a alteração dos procedimentos da empresa. Aplique um teste de hipotese para confirmar ou 
rejeitar tal hipótese

##Gera valores de uma normal##
w1<-c(5.7,4.4,4.6,5.3,6.8,5.2,6.1,6.6,6.9,6.2,6.4,6.0,3.6,5.3,4.0,4.8,5.4,3.8,4.0,
6.2,3.9,5.9,5.5,3.7,6.6,6.1,7.7,4.6,3.9,5.8,6.7,5.8,6.4,5.6,7.4,7.6,5.8,6.1,
5.5,6.0)

##Gera valores de uma normal##
w2<-c(7.0,5.8,6.6,6.7,8.6,6.2,7.7,7.7,7.5,7.8,8.0,7.0,5.1,5.9,5.7,6.8,5.6,5.3,5.7,
8.0,5.4,8.0,6.6,5.6,7.9,7.0,9.0,5.7,5.2,6.7,7.7,7.4,8.4,6.4,9.1,9.0,7.2,7.5,
7.3,8.0)

###Teste Normalidade de W1####
shapiro.test(w1)
###Teste Normalidade de W2####
shapiro.test(w2)

#Testa igualdade das variâncias#
var.test(w1,w2)

##Realizando o teste t para igualdade de média
t.test(w1,w2,paired = TRUE, var.equal = TRUE)

##Boxplot para visualização##
boxplot(w1,w2, ylab="Escore", range = 1.5,col=c("deepskyblue4","gray70"),
main="Boxplot do Escore",names=c("Antes", "Depois"), horizontal=FALSE)

########Intervalos de Confiança##########


#Funçao para o cálculo do IC
ICmedia<-function(x,alpha,desvio){
n<-length(x)
z<-qnorm(alpha/2, lower.tail = FALSE)
if(missing(desvio)){
desvio<-sd(x)
z<-qt(1-alpha/2, n-1)
}
LI<-mean(x)-z*desvio/sqrt(n)
LS<-mean(x)+z*desvio/sqrt(n)
return(c(LI,LS))
}



#Funçao para o cálculo do IC
ICprop<-function(y,n,alpha){
p<-y/n
z<-qnorm(alpha/2, lower.tail = FALSE)
LI<-max(p-z*sqrt((p*(1-p))/n),0)
LS<-min(p+z*sqrt((p*(1-p))/n),1)
return(c(LI,LS)) }

#Considere a situação em que o interesse é obter o intervalo de confiança
#ao nível de significância de $5\%$ para a proporção
#de alunos da instituição que utilizam produtos da Apple.
#Tendo como resultado da amostragem que 9 dos 25 alunos responderam
#afirmativamente. obtenha o intervalo de confiança para proporção
##Resposta###
ICprop(9,25,0.05)

############################
####Calculando Correlação###
############################
cor(p18_m,p12_m)

############################
####Testando Correlação#####
############################
cor.test(p18_m,p12_m)


###########################
###########################
#####Regressão Linear######
###########################
###########################

ajuste_m2<-lm(p18_m ~  p12_m + esc_m +tempd_m * pd_m - 1)

summary (ajuste_m2) 

par(mfrow = c(2, 2))
plot(ajuste_m2)
shapiro.test(ajuste_m2$residuals)

AIC(ajuste_m2)

X1<-mean(p12_m)
X2<-mean(esc_m)
X3<-mean(pd_m)
X4<-mean(tempd_m)

Y = 0.5678*X1 -12.52*X2 + 0.7841*X3 + 1.686*X4 - 0.005275*X3*X4