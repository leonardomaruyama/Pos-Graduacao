require(multcomp)     ##Comparação multipla##
require(nlme)

#########Estatísticas Básicas###
summary(malpreqt); sd(malpreqt)
summary(malposqt); sd(malpreqt)
summary(mal1qt); sd(malpreqt)

boxplot(malpreqt,malposqt,mal1qt)


qplot(factor(c(rep("Pré",20),rep("Pós",20),rep("Atual",20)), levels=c("Pré","Pós","Atual")),c(malpreqt,malposqt,mal1qt), geom="boxplot", ylab ="MAL",xlab=" ",ylim=c(min(malpreqt),max(malposqt)))

##Declarando dados###
dv<-c(malpreqt,malposqt,mal1qt)
grupo<-factor(c(rep("A",20),rep("B",20),rep("C",20)))
##Criando um data frame###
idade<-c(aidade,aidade,aidade)
tlesao<-c(atlesao,atlesao,atlesao)
sexo<-c(asexo,asexo,asexo)
dominancia<-c(adominancia,adominancia,adominancia)
comprometimento<-c(acomprometimento,acomprometimento,acomprometimento)
ltratado<-c(altratado,altratado,altratado)
acognitiva<-c(cognitiva,cognitiva,cognitiva)
familiar<-c(afamiliar,afamiliar,afamiliar)
mydata <- data.frame(dv, grupo,aidade,tlesao,sexo,dominancia,comprometimento,ltratado,acognitiva,familiar)


###Ajustando o modelo linear###
am2 <- lm(dv ~ grupo, data=mydata)
summary(am2)
shapiro.test(am2$residuals)

plot(TukeyHSD(am2))

shapiro.test(malpreqt);shapiro.test(malposqt);shapiro.test(mal1qt)

##Aplicando o teste de comparação###
summary(glht(am2,linfct=mcp(grupo="Tukey")))
am2 <- aov(dv ~ grupo, data=mydata)
summary(am2)
TukeyHSD(am2)



###Ajustando o modelo linear###
am3 <- lm(log(dv) ~ grupo+idade+tlesao+sexo+factor(dominancia)+factor(comprometimento)+factor(ltratado)+factor(acognitiva)+factor(familiar), data=mydata)
summary(am3)
shapiro.test(am3$residuals)

###Ajustando o modelo linear###
am3 <- lm(dv ~ grupo + tlesao + sexo + factor(dominancia) + factor(familiar), data=mydata)
summary(am3)
shapiro.test(am3$residuals)

step <- stepAIC(am3, direction="both")
step$anova
