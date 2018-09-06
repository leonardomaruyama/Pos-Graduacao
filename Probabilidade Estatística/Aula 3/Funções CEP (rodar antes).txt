### Funções para geração dos gráficos e cálculo dos índices de capabilidade

#####################################################################################################################

##### Quadro 1.1 - Gráfico de Pareto

# x = vetor com as frequências totais de cada categoria. Use o comando names(x) para rotular as categorias.

G.Pareto=function(x){
		m=max(nchar(names(x)))
		if(m<=2) aux=0 else aux=3
		if(aux==0){
			     a=4.5; b=4.5
		}
		if(aux==3){
			     a=9.5; b=4
		}
		x=sort(x,decreasing=TRUE)
		soma.acum=cumsum(x)
		porc.acum=seq(0,100,25)
		quantis=quantile(seq(0,max(soma.acum)),porc.acum/100)
		op=par(mar=c(a,4.5,b,4.5),las=aux)
		gb=barplot(x,main="Gráfico de Pareto",
               ylim=c(0,max(soma.acum)*1.05),col="blue",border="blue")
		abline(h=quantis,col="lightgrey",lty=3)
		rect(gb-0.5,rep(0,length(x)),gb+0.5,x,col="blue",
               border="blue")
		lines(gb+0.5,soma.acum,type="b",col="red",cex=0.7,pch=16)
		axis(4,at=quantis,las=3,labels=paste(porc.acum,"%",sep=""))
		mtext(c("Frequência","Porcentagem Acumulada"),side=c(2,4),
               line=2.5)
		box()
		tabela=round(cbind(x,soma.acum,x/max(soma.acum)*100,
               soma.acum/max(soma.acum)*100),2) 
            colnames(tabela)=c("Frequência","Freq.Acum.","Porcentagem",
               "Porcent.Acum.")
    		return(as.table(tabela))
		par(op)
}

#####################################################################################################################

##### Quadro 1.2 - Diagrama de Causa e Efeito

# x = lista com as causas principais e secundárias (subcausas)
# y = variável string com o nome do efeito a ser analisado

Ishikawa=function(x,y){
	m=length(x)
    	m1=m-round(m/2)
    	m2=m-m1
    	if(m1<m2){
		  v1=m1; v2=m2
		  m1=v2; m2=v1
    	}
    	nmax=max(sapply(x,length))
    	op=par(mar=c(1,1,3,1),no.readonly=TRUE)
	on.exit(par(op))
	plot(0:50,0:50,type="n",xlab="",ylab="",axes=FALSE,
        main="Diagrama de causa-e-efeito")
	u=par("usr")
	le=strwidth(y,units="user")*1.1
	lc=max(unlist(sapply(x,strwidth,units="user")))
	ac=max(strheight(y,units="user"))
	arrows(0,25,u[2]-le-1,25,code=2,length=0.1,angle=25)
	text(u[2]-le,25,y,adj=c(0,0.35),cex=0.95,font=2)
	a=(u[2]-le-1)/(max(m1,m2)+1)
	a2=a*(0:(max(m1,m2)))
	media2=function(k){
		m2=rep(0,length(k)-1)
        	for (i in 1:(length(k)-1)) m2[i]=mean(k[c(i,i+1)])
        	return(m2)
    	}
    	for (i in 1:(length(a2)-1)){
        	segments(media2(a2)[i],45,a2[i+1],25)
	      text(media2(a2)[i],45.5,names(x)[i],pos=3,offset=0.5,
              cex=0.85,font=2,col="red")
        	if (i<=m2){
            	     segments(media2(a2)[i],5,a2[i+1],25) 		      
                       text(media2(a2)[i],4.5,names(x)[[m1+i]],pos=1,
                         offset=0.5,cex=0.85,font=2,col="red")
     	  		     }
    	}
    	for (i in 1:m1){
        	beta=(25-45)/(a2[i+1]-media2(a2)[i])
        	alpha=45-(beta*media2(a2)[i])
        	y2=sort(25+(cumsum((45-25)/(nmax+1))*(1:(nmax))),
               decreasing=TRUE)
       	x2=(y2-alpha)/beta
        	for (j in 1:length(y2)){
           		nomes=x[[i]][j]
            	if (!is.na(nomes))          		 
                       text(x2[j],y2[j],nomes,pos=4,offset=0.2,
                         cex=0.75,font=4,col="blue")
        	}
    	}
    	for (i in 1:m1){
        	beta=(25-5)/(a2[i+1]-media2(a2)[i])
        	alpha=5-(beta*media2(a2)[i])
        	y2=5+(cumsum((45-25)/(nmax+1))*(1:(nmax)))
        	x2=(y2-alpha)/beta
        	if (i<=m2) 
                for (j in 1:length(y2)) {
                		nomes=x[[m1+i]][j]
                		if (!is.na(nomes))     			 
                            text(x2[j],y2[j],nomes,pos=4,offset=0.2,
                              cex=0.75,font=4,col="blue")
                }
    	}
    	invisible()
}

#####################################################################################################################

##### Quadro 2.2 - Funções para construção do gráfico S e X-barra

# m = número de amostras observadas. 
# n = número de observações de cada amostra. 
# y = vetor com as observações, ordenadas por amostra, da variável observada.
# mi = especificação para a média do processo.
# sigma = especificação para o desvio-padrão do processo.
 
xS1=function(m,n,y,mi,sigma){
	if (length(n)==1) n=rep(n,m)
	mat=matrix(NA,m,max(n)); k=1 
	for (i in 1:m) {
		for (j in 1:n[i]) {
			mat[i,j]=y[k]
			k=k+1
		}
	}
	media=apply(mat,1,mean,na.rm=T)
	desviop=apply(mat,1,sd,na.rm=T)
	LIC1=numeric(m)
	LSC1=numeric(m)
	LIC2=numeric(m)
	LSC2=numeric(m)
	for (i in 1:m){ 
		LIC1[i]=mi-3*sigma/sqrt(n[i])
		LSC1[i]=mi+3*sigma/sqrt(n[i])
		c4=(gamma(n[i]/2)/gamma((n[i]-1)/2))*sqrt(2/(n[i]-1))
		LIC2[i]=c4*sigma-3*sigma*sqrt((1-c4^2))
		LSC2[i]=c4*sigma+3*sigma*sqrt((1-c4^2))
		if (LIC2[i] < 0) LIC2[i]=0
	}
	par(mfrow=c(2,1))
	amostra=matrix(rep(seq(1,m),4),m,4,byrow=F)
	xbar=cbind(rep(mi,m),LIC1,LSC1,media)
	rbar=cbind(rep(sigma,m),LIC2,LSC2,desviop)
	matplot(amostra,rbar,type="o",ylab="Desvio-Padrão",
		col=c("black","red","red","blue"),
		ylim=c(min(rbar),max(rbar)),
		xlab="Amostras",main="Gráfico S",pch=20,lty=1,lwd=2)
      matplot(amostra,xbar,type="o",ylab="Média",xlab="Amostras",
		main=expression(paste("Gráfico " * bar(x))), 
      col=c("black","red","red","blue"),
		ylim=c(min(xbar),max(xbar)),pch=20, lty=1, lwd=2)
}

xS2=function(m,n,y){
	if (length(n)==1) n=rep(n,m)
	mat=matrix(NA,m,max(n)); k=1 
	for (i in 1:m) {
		for (j in 1:n[i]) {
			mat[i,j]=y[k]
			k=k+1
		}
	}
	media=apply(mat,1,mean,na.rm=T)
	desviop=apply(mat,1,sd,na.rm=T)
	x2barras=sum(n*media)/sum(n)
	Sbarra=sqrt(sum((n-1)*desviop^2)/(sum(n)-m))
	LIC1=numeric(m)
	LSC1=numeric(m)
	LIC2=numeric(m)
	LSC2=numeric(m)
	c4=0	
	for (i in 1:m){ 
		c4=(gamma(n[i]/2)/gamma((n[i]-1)/2))*sqrt(2/(n[i]-1))
		LIC1[i]=x2barras-3*Sbarra/(c4*sqrt(n[i]))
		LSC1[i]=x2barras+3*Sbarra/(c4*sqrt(n[i]))
		LIC2[i]=Sbarra-3*Sbarra*sqrt((1-c4^2))/c4
		LSC2[i]=Sbarra+3*Sbarra*sqrt((1-c4^2))/c4
		if (LIC2[i] < 0) LIC2[i]=0
	}
	amostra=matrix(rep(seq(1,m),4),m,4,byrow=F)
	xbar=cbind(rep(x2barras,m),LIC1,LSC1,media)
	rbar=cbind(rep(Sbarra,m),LIC2,LSC2,desviop)
	par(mfrow=c(2,1))
	matplot(amostra,rbar,type="o",ylab="Desvio-Padrão",
		col=c("black","red","red","blue"),
		ylim=c(min(rbar),max(rbar)),
		xlab="Amostras",main="Gráfico S",pch=20,lty=1,lwd=2)
      matplot(amostra,xbar,type="o",ylab="Média",xlab="Amostras",
		main=expression(paste("Gráfico " * bar(x))), 
      col=c("black","red","red","blue"),
		ylim=c(min(xbar),max(xbar)),pch=20, lty=1, lwd=2)
}

#####################################################################################################################

##### Quadro 2.4 - Funções para construção do gráfico R e X-barra

# m = número de amostras observadas. 
# n = número de observações de cada amostra.
# y = vetor com as observações, ordenadas por amostra, da variável observada.
# mi = especificação para a média do processo.
# sigma = especificação para o desvio-padrão do processo.

xR1=function(m,n,y,mi,sigma){
	if (n>1) {
		amplit=numeric(m) 	
		mat=matrix(y,m,n,byrow=T)
		media=apply(mat,1,mean)
		for (i in 1:m) {
			amplit[i]=max(mat[i,])-min(mat[i,])
		}
	} else {
		amplit=numeric(m-1)
		media=y
		for (i in 2:m) amplit[i-1]=sqrt((y[i]-y[i-1])^2)
	}
	d2=c(1.1296,1.6918,2.0535,2.3248,2.5404,2.7074,
		2.8501,2.9677,3.0737,3.1696)
	d3=c(.8541,.8909,.8800,.8674,.8508,
		.8326,.8209,.8102,.7978,.7890)
	LIC1=mi-3*sigma/sqrt(n)
	LSC1=mi+3*sigma/sqrt(n)
	if(n>1) {
		LIC2=d2[n-1]*sigma-3*d3[n-1]*sigma
		LSC2=d2[n-1]*sigma+3*d3[n-1]*sigma
		if (LIC2 < 0) LIC2=0
	} else {
		LIC2=d2[n]*sigma-3*d3[n]*sigma
		LSC2=d2[n]*sigma+3*d3[n]*sigma
		if (LIC2 < 0) LIC2=0
	}	
	par(mfrow=c(2,1))
	amostra1=matrix(rep(seq(1,m),4),m,4,byrow=F)
	if (n>1) amostra2=amostra1 else amostra2=amostra1[-1,]
	xbar=cbind(rep(mi,m),rep(LIC1,m),rep(LSC1,m),media)
	if (n>1) {
		rbar=cbind(rep(sigma,m),rep(LIC2,m),rep(LSC2,m),amplit) 
	} else rbar=cbind(rep(sigma,m-1),rep(LIC2,m-1),
		rep(LSC2,m-1),amplit)
	matplot(amostra2,rbar,type="o",ylab="Amplitude",
		col=c("black","red","red","blue"),
		ylim=c(min(rbar),max(rbar)),
		xlab="Amostras",main="Gráfico R",pch=20, lty=1, lwd=2)
	matplot(amostra1,xbar,type="o",ylab="Média",xlab="Amostras",
		main=expression(paste("Gráfico " * bar(x))),col=c("black","red","red",
		"blue"),ylim=c(min(xbar),max(xbar)),pch=20, lty=1, lwd=2)
	out=rbind(round(xbar[1,1:3],2),round(rbar[1,1:3],2))
	colnames(out)=c("Referência","LIC","LSC")
	out
}

xR2=function(m,n,y){
	if (n>1) {
		amplit=numeric(m) 	
		mat=matrix(y,m,n,byrow=T)
		media=apply(mat,1,mean)
		for (i in 1:m) amplit[i]=max(mat[i,])-min(mat[i,])
		x2barras=mean(media)
		Rbarra=mean(amplit) 
		d2=c(1.1296,1.6918,2.0535,2.3248,2.5404,2.7074,2.8501,
			2.9677,3.0737,3.1696)
		d3=c(.8541,.8909,.8800,.8674,.8508,.8326,.8209,
			.8102,.7978,.7890)
		LIC1=x2barras-3*Rbarra/(d2[n-1]*sqrt(n))
		LSC1=x2barras+3*Rbarra/(d2[n-1]*sqrt(n))
		LIC2=Rbarra-3*d3[n-1]*Rbarra/d2[n-1]
		LSC2=Rbarra+3*d3[n-1]*Rbarra/d2[n-1]
		if (LIC2 < 0) LIC2=0
	
	} else {
		amplit=numeric(m-1)
		media=y
		for (i in 2:m) amplit[i-1]=sqrt((y[i]-y[i-1])^2)
		x2barras=mean(media)
		Rbarra=mean(amplit) 
		d2=1.1296
		d3=.8541
		LIC1=x2barras-3*Rbarra/(d2*sqrt(n))
		LSC1=x2barras+3*Rbarra/(d2*sqrt(n))
		LIC2=Rbarra-3*d3*Rbarra/d2
		LSC2=Rbarra+3*d3*Rbarra/d2
		if (LIC2 < 0) LIC2=0
	}
	par(mfrow=c(2,1))
	amostra1=matrix(rep(seq(1,m),4),m,4,byrow=F)
	if (n>1) amostra2=amostra1 else amostra2=amostra1[-1,]
	xbar=cbind(rep(x2barras,m),rep(LIC1,m),rep(LSC1,m),media)
	if (n>1) {
		rbar=cbind(rep(Rbarra,m),rep(LIC2,m),rep(LSC2,m),amplit) 
	} else rbar=cbind(rep(Rbarra,m-1),rep(LIC2,m-1),
		rep(LSC2,m-1),amplit)
	matplot(amostra2,rbar,type="o",ylab="Amplitude",
		col=c("black","red","red","blue"),
		ylim=c(min(rbar),max(rbar)),
		xlab="Amostras",main="Gráfico R",pch=20, lty=1, lwd=2)
	matplot(amostra1,xbar,type="o",ylab="Média",xlab="Amostras",
		main=expression(paste("Gráfico " * bar(x))),col=c("black","red","red",
		"blue"),ylim=c(min(xbar),max(xbar)),pch=20, lty=1, lwd=2)
	out=rbind(round(xbar[1,1:3],2),round(rbar[1,1:3],2))
	colnames(out)=c("Referência","LIC","LSC")
	out
}

#####################################################################################################################

##### Quadro 3.2 - Funções para construção do gráfico Cusum Tabular

# m = número de amostras observadas. 
# n = número de observações de cada amostra. 
# y = vetor com as observações, ordenadas por amostra, da variável observada.
# mi = especificação para a média do processo.
# sigma = especificação para o desvio-padrão do processo.

cusum=function(m,n,y,mi,sigma){
	k=0.5*sigma/sqrt(n)
	h=5*sigma/sqrt(n)
	hs=3.5*sigma/sqrt(n)
	cp=numeric(m)
	cn=numeric(m) 
	if (n>1) {
		dados=matrix(y,m,n,byrow=T)
		media=apply(dados,1,mean)
	} else media=y 
	cp[1]=max(c(0,media[1]-mi-k+h/2))
	cn[1]=max(c(0,mi+k-media[1]+h/2))
	for (i in 2:m) {
		cp[i]=max(c(0,media[i]-mi-k+cp[i-1]))
		cn[i]=max(c(0,mi-k-media[i]+cn[i-1]))
	}
	cn=-cn
	amostra=matrix(rep(seq(1,m),7),m,7,byrow=F)
	cusums=cbind(rep(0,m),rep(hs,m),rep(-hs,m),
		rep(h,m),rep(-h,m),cp,cn)
	par(mfrow=c(1,1))
	matplot(amostra,cusums,xlab="Amostras",ylab="Cusums",
		main="Gráfico de Somas Acumuladas",type="o",pch=20,
		col=c("black","red","red","orange","orange","blue",
		"blue"),ylim=c(min(cusums),max(cusums)) , lty=1, lwd=2)
}

#####################################################################################################################

##### Quadro 3.4 - Funções para construção do gráfico MMEP

# m = número de amostras observadas. 
# n = número de observações de cada amostra, se as amostras forem balanceadas ou
# n = vetor com o número de observações de cada amostra.
# mi = especificação para a média do processo.
# sigma = especificação para o desvio-padrão do processo.
# lambda = constante de suavização entre 0 e 1 (sugestão: use 0.05, 0.1 ou 0.2).
# gamma = constante de significância dos limites de controle (sugestão: use 3). 
# y = vetor com as observações, ordenadas por amostra, da variável observada.

MMEP=function(m,n,mi,sigma,lambda,gamma,y){
	if(length(n)==1){
 		if (n > 1) {
			matdados=matrix(y,m,n,byrow=T)
			media=apply(matdados,1,mean,na.rm=T) 
		} else media=y 
	} else {
		matdados=matrix(NA,m,max(n))
		k=1
		for (i in 1:m) {
			for (j in 1:n[i]) {
				matdados[i,j]=y[k]
				k=k+1
			}
		}
		media=apply(matdados,1,mean,na.rm=T)
	}
	W=numeric(m); 
	W[1]=lambda*media[1]+(1-lambda)*mi
	for (i in 2:m) {
		W[i]=lambda*media[i]+(1-lambda)*W[i-1]
	}	
	LIC=numeric(m)
	LSC=numeric(m)	
	if(length(n)==1) {
		for (i in 1:m) {
			fator =(lambda*sigma^2)/((2-lambda)*n)
			ep=gamma*sqrt(fator*(1-(1-lambda)^(2*i)))
			LIC[i]=mi-ep
			LSC[i]=mi+ep
		}
	} else {
		for (i in 1:m) {
			fator =(lambda*sigma^2)/((2-lambda)*n[i])
			ep=gamma*sqrt(fator*(1-(1-lambda)^(2*i)))
			LIC[i]=mi-ep
			LSC[i]=mi+ep
		}
	}
	amostra=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
	LCMM=cbind(W,LIC,LSC,rep(mi,m))
	matplot(amostra,LCMM,type="o",ylab="Média",xlab="Amostras",
		main="Gráfico MMEP",col=c("blue","red","red","black"),
		 ylim=c(min(LCMM),max(LCMM)),pch=20, lty=1, lwd=2)
	}

#####################################################################################################################

##### Quadro 4.3 - Funções para construção do gráfico p

# m = número de amostras disponíveis.
# n = número de unidades observadas em cada amostra. Pode ser um vetor, caso as amostras tenham tamanhos diferentes.
# y = vetor contendo o total de itens defeituosos em cada amostra, ordenado de acordo com o tempo das amostras.
# p = especificação para a probabilidade aceitável de que um item qualquer seja defeituoso.  

pce=function(m,n,y,p) {
	if (length(n)==1){
		LIC=p-3*sqrt(p*(1-p)/n);if (LIC<0) LIC=0;
		LSC=p+3*sqrt(p*(1-p)/n)	
		time=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
		w=cbind(y/n,rep(p,m),rep(LIC,m),rep(LSC,m))
		par(mfrow=c(1,1))
		matplot(time,w,type="o",pch=20, lty=1,lwd=2,
		col=c("black","blue","red","red"),ylim=c(min(w),max(w)),
		main="Gráfico p",ylab="p",xlab="Amostras")
	} else {
		LIC=numeric(m);LSC=numeric(m);z=numeric(m)
		for (i in 1:m){
			LIC[i]=p-3*sqrt(p*(1-p)/n[i]);if (LIC[i]<0) LIC[i]=0
			LSC[i]=p+3*sqrt(p*(1-p)/n[i])
			z[i]=((y[i]/n[i]-p)/sqrt(p*(1-p)/n[i]))
		}
		time=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
		w1=cbind(y/n,rep(p,m),LIC,LSC)
		w2=cbind(z,rep(0,m),rep(-3,m),rep(3,m))
		par(mfrow=c(1,2))
		matplot(time,w1,type="o",pch=20,ylim=c(min(w1),max(w1)),
			col=c("black","blue","red","red"), lty=1,lwd=2,
			main="Gráfico p",ylab="p",xlab="Amostras"	)
		matplot(time,w2,type="o",pch=20,ylim=c(min(w2),max(w2)),
			col=c("black","blue","red","red"),xlab="Amostras",
			main="Gráfico p Padronizado",ylab="z", lty=1,lwd=2)
	}
}

pse=function(m,n,y) {
	if (length(n)==1){
		p=sum(y)/(n*m);LIC=p-3*sqrt(p*(1-p)/n);if (LIC<0) LIC=0
		LSC=p+3*sqrt(p*(1-p)/n)	
		time=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
		w=cbind(y/n,rep(p,m),rep(LIC,m),rep(LSC,m))
		par(mfrow=c(1,1))
		matplot(time,w,type="o",pch=20,main="Gráfico p",
			col=c("black","blue","red","red"),ylab="p",
			xlab="Amostras", ylim=c(min(w),max(w)),lty=1,lwd=2)
	} else {
		p=sum(y)/sum(n);LIC=numeric(m);LSC=numeric(m);z=numeric(m)
		for (i in 1:m){
			LIC[i]=p-3*sqrt(p*(1-p)/n[i]);if (LIC[i]<0) LIC[i]=0
			LSC[i]=p+3*sqrt(p*(1-p)/n[i])	
			z[i]=((y[i]/n[i]-p)/sqrt(p*(1-p)/n[i]))
		}
		time=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
		w1=cbind(y/n,rep(p,m),LIC,LSC)
		w2=cbind(z,rep(0,m),rep(-3,m),rep(3,m))
		par(mfrow=c(1,2))
		matplot(time,w1,type="o",pch=20,ylim=c(min(w1),max(w1)),
			col=c("black","blue","red","red"),lty=1,lwd=2,
			main="Gráfico p",ylab="p",xlab="Amostras"	)
		matplot(time,w2,type="o",pch=20,ylim=c(min(w2),max(w2)),
			col=c("black","blue","red","red"),xlab="Amostras",
			main="Gráfico p Padronizado",ylab="z",lty=1,lwd=2)
	}
}

#####################################################################################################################

##### Quadro 4.5 - Funções para construção do gráfico np

# m = número de amostras disponíveis.
# n = número de unidades observadas em cada amostra.
# y = vetor contendo o total de itens defeituosos em cada amostra, ordenado de acordo com o tempo das amostras.
# p = especificação para a probabilidade aceitável de que um item qualquer seja defeituoso.  

npce=function(m,n,y,p) {
	LIC=n*p-3*sqrt(n*p*(1-p));if (LIC<0) LIC=0; 	LSC=n*p+3*sqrt(n*p*(1-p))	
	time=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
	w=cbind(y,rep(n*p,m),rep(LIC,m),rep(LSC,m))
	par(mfrow=c(1,1))
	matplot(time,w,pch=20,main="Gráfico np",xlab="Amostras",lwd=2,
		col=c("black","blue","red","red"),type="o",lty=1,
		ylab="np",ylim=c(min(w),max(w)))
}

npse=function(m,n,y) {
	p=sum(y)/(n*m); LIC=n*p-3*sqrt(n*p*(1-p))
	if (LIC<0) LIC=0; LSC=n*p+3*sqrt(n*p*(1-p))	
	time=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
	w=cbind(y,rep(n*p,m),rep(LIC,m),rep(LSC,m))
	par(mfrow=c(1,1))
	matplot(time,w,type="o",ylab="np",
		col=c("black","blue","red","red"),pch=20,lty=1,lwd=2,
		main="Gráfico np",xlab="Amostras",ylim=c(min(w),max(w)))
}

#####################################################################################################################

##### Quadro 5.2 - Funções para construção do gráfico c

# m = número de amostras disponíveis.
# y = vetor contendo o número total de defeitos em cada amostra, ordenado de acordo com o tempo de coleta das amostras.
# c = especificação para a taxa aceitável de defeitos para a amostra observada.  

cce=function(m,y,c) {
	LIC=c-3*sqrt(c); if (LIC<0) LIC=0; LSC=c+3*sqrt(c)
	time=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
	w=cbind(y,rep(c,m),rep(LIC,m),rep(LSC,m))
	par(mfrow=c(1,1))
	matplot(time,w,type="o",pch=20,ylim=c(min(w),max(w)),
		col=c("black","blue","red","red"), xlab="Amostras",
		main="Gráfico c",ylab="c", lty=1,lwd=2)
}

cse=function(m,y) {
	c=mean(y); LIC=c-3*sqrt(c); if (LIC<0) LIC=0; LSC=c+3*sqrt(c)
	time=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
	w=cbind(y,rep(c,m),rep(LIC,m),rep(LSC,m))
	par(mfrow=c(1,1))
	matplot(time,w,type="o",pch=20, ylim=c(min(w),max(w)),
		col=c("black","blue","red","red"), xlab="Amostras",
		main="Gráfico c",ylab="c",lty=1,lwd=2)
}

#####################################################################################################################

##### Quadro 5.5 - Funções para construção do gráfico u

# m = número de amostras disponíveis.
# n = número de unidades disponíveis em cada amostra. Pode ser um vetor, caso as amostras tenham tamanhos diferentes. 
# y = vetor contendo o número total de defeitos em cada amostra, ordenado de acordo com o tempo de coleta das amostras.
# u = especificação para a taxa aceitável de defeitos para a amostra observada.  

uce=function(m,n,y,u) {
	if (length(n)==1) {
		y=matrix(y,m,n,byrow=T); y=apply(y,1,mean)
		LIC=u-3*sqrt(u/n); if (LIC<0) LIC=0; LSC=u+3*sqrt(u/n)
		time=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
		w=cbind(y,rep(u,m),rep(LIC,m),rep(LSC,m))
		par(mfrow=c(1,1))
		matplot(time,w,type="o",pch=20,
			col=c("black","blue","red","red"), xlab="Amostras",
			main="Gráfico u",ylab="u",
			ylim=c(min(w),max(w)),lty=1,lwd=2)
	} else {
		dados=matrix(NA,m,max(n)); k=1
		for (i in 1:m) { 
			for (j in 1:n[i]) {
				dados[i,j]=y[k]; k=k+1
			}
		}
		y=apply(dados,1,mean,na.rm=T)
		LIC=numeric(m); LSC=numeric(m); z=numeric(m)
		for (i in 1:m) {
			LIC[i]=u-3*sqrt(u/n[i]); if (LIC[i]<0) LIC[i]=0
			LSC[i]=u+3*sqrt(u/n[i]); z[i]=(y[i]-u)/sqrt(u/n[i])
		}
		time=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
		w1=cbind(y,rep(u,m),LIC,LSC);
		w2=cbind(z,rep(0,m),rep(-3,m),rep(3,m))
		par(mfrow=c(1,2))
		matplot(time,w1,type="o",pch=20,
			col=c("black","blue","red","red"), main="Gráfico u",			
			ylim=c(min(w1),max(w1)), lty=1,lwd=2,
			ylab="u", xlab="Amostras")
		matplot(time,w2,type="o",pch=20, ylim=c(min(w2),max(w2)),
			col=c("black","blue","red","red"), xlab="Amostras",
			main="Gráfico u Padronizado",ylab="z", lty=1,lwd=2)
	}
}

use=function(m,n,y) {
	if (length(n)==1) {
		u=mean(y); y=matrix(y,m,n,byrow=T); y=apply(y,1,mean)
		LIC=u-3*sqrt(u/n); if (LIC<0) LIC=0; LSC=u+3*sqrt(u/n)
		time=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
		w=cbind(y,rep(u,m),rep(LIC,m),rep(LSC,m))
		par(mfrow=c(1,1))
		matplot(time,w,type="o",pch=20,
			col=c("black","blue","red","red"), xlab="Amostras",
			main="Gráfico u",ylab="u",
			ylim=c(min(w),max(w)),lty=1,lwd=2)
	} else {
		u=mean(y); dados=matrix(NA,m,max(n)); k=1
		for (i in 1:m) { 
			for (j in 1:n[i]) {
				dados[i,j]=y[k]; k=k+1
			}
		}
		y=apply(dados,1,mean,na.rm=T); LIC=numeric(m)
		LSC=numeric(m); z=numeric(m)
		for (i in 1:m) {
			LIC[i]=u-3*sqrt(u/n[i]); if (LIC[i]<0) LIC[i]=0
			LSC[i]=u+3*sqrt(u/n[i]); z[i]=(y[i]-u)/sqrt(u/n[i])
		}
		time=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
		w1=cbind(y,rep(u,m),LIC,LSC);
		w2=cbind(z,rep(0,m),rep(-3,m),rep(3,m))
		par(mfrow=c(1,2))
		matplot(time,w1,type="o",pch=20,
			col=c("black","blue","red","red"), lty=1,lwd=2,
			main="Gráfico u", ylim=c(min(w1),max(w1)),
			ylab="u", xlab="Amostras")
		matplot(time,w2,type="o",pch=20,
			col=c("black","blue","red","red"), ylab="z",
			main="Gráfico u Padronizado", lty=1,lwd=2,
			xlab="Amostras",ylim=c(min(w2),max(w2)))
	}
}

#####################################################################################################################

##### Quadro 6.1 - Capacidade do Processo

# y = vetor de valores da variável de interesse.
# LIE = Limite Inferior do Intervalo de Especificação.
# LSE = Limite Superior do Intervalo de Especificação.
# alpha = nível de confiança dos intervalos a serem construídos para cada índice.

ICAP=function(y,LIE,LSE,alpha) {
	Qinf=0.5-alpha/2; Qsup=0.5+alpha/2; T=(LSE+LIE)/2
	n=length(y); xbar=mean(y); S=sd(y); gl=n-1; Cp=(LSE-LIE)/(6*S)
	Cpinf=Cp*sqrt(qchisq(Qinf,gl)/gl)
	Cpsup=Cp*sqrt(qchisq(Qsup,gl)/gl)
	P=100/Cp; Pinf=100/Cpsup; Psup=100/Cpinf	
	I1=(LSE-xbar)/(3*S); I2=(xbar-LIE)/(3*S); Cpk=min(c(I1,I2))
	c1=1/(9*n*(Cpk^2)); c2=1/(2*gl)
	Cpkinf=Cpk*(1-qnorm(Qsup)*sqrt(c1+c2))
	Cpksup=Cpk*(1+qnorm(Qsup)*sqrt(c1+c2)); V=(xbar-T)/S
	Cpm=Cp/(sqrt(1+(V^2))); desvio=(LSE-LIE)/(6*Cpm)
	out1=matrix(NA,3,3)
	IC1=c(Cpinf,Cp,Cpsup); out1[1,]=round(IC1,2)
	IC2=c(Pinf,P,Psup); out1[2,]=round(IC2,2)
	IC3=c(Cpkinf,Cpk,Cpksup); out1[3,]=round(IC3,2)
	colnames(out1)=c("Limite Inferior",
		"Estimativa Pontual","Limite Superior")
	rownames(out1)=c("Cp","P","Cpk")
	out2=matrix(NA,1,2); out2[1,]=round(c(Cpm,desvio),2)
	colnames(out2)=c("Cpm","Máximo para |MI-T|") 
	rownames(out2)="Valores"; lista=list(out1,out2); lista
}

#####################################################################################################################

##### Quadro 7.2 - Gráficos de Controle de Variabilidade

# n = número de observações por amostra.
# y = matriz dos dados coletados. Cada coluna deve se referir a uma variável.
# MC = matriz de covariâncias das variáveis especificada a priori. 

cvse=function(n,y){
	p=ncol(y); obs=nrow(y);m=obs/n; f=p*n*(log(n)-1); S=cov(y)
      Sinv=solve(S); det1=det(S); amostra=matrix(0,n,p); W1=numeric(m) 
      W2=W1
	for (i in 1:m) {
		amostra=y[(n*i-n+1):(n*i),]; Si=cov(amostra); A=(n-1)*Si
		W1[i]=f+sum(diag(Sinv%*%A))-n*log(det(A)/det1)
		W2[i]=det(Si)
	}
	time1=cbind(seq(1,m),seq(1,m)); time2=cbind(time1,time1)	
	LSC1=qchisq(0.997,p*(p+1)/2); z1=cbind(W1,rep(LSC1,m))
	produto1=1; produto2=1; x=seq(1,m)
	for ( i in 1:p) produto1=(n-i)*produto1
	for ( i in 1:p) produto2=(n-i+2)*produto2
	b1=produto1/((n-1)^p)
	b2=produto1*(produto2-produto1)/((n-1)^(2*p)) 
	LIC2=det1*(b1-3*sqrt(b2))/b1; LSC2=det1*(b1+3*sqrt(b2))/b1
	if (LIC2<=0) LIC2=0
	z2=cbind(W2,rep(det1,m),rep(LIC2,m),rep(LSC2,m))
	par(mfrow=c(1,2))
	matplot(time1, z1, type="o", pch=20, col=c("black","red"),
		main="Método 1", xlab="Amostras", ylab="W",
		ylim=c(min(z1), max(z1)), lwd=2, lty=1)
	matplot(time2, z2, type="o", col=c("black","blue","red","red"),
		main="Método 2", xlab="Amostras", ylab="det(S)",
		ylim=c(min(z2), max(z2)), lty=1, lwd=2, pch=20)
}

cvce=function(n,y,MC){
	p=ncol(y); obs=nrow(y); m=obs/n; f=p*n*(log(n)-1); W1=numeric(m)
	Sinv=solve(MC); det1=det(MC);	amostra=matrix(0,n,p); W2=W1
	for (i in 1:m) {
		amostra=y[(n*i-n+1):(n*i),]; A=(n-1)*cov(amostra)
		W1[i]=f+sum(diag(Sinv%*%A))-n*log(det(A)/det1)
		W2[i]=det(cov(amostra))
	}
	time1=cbind(seq(1,m),seq(1,m)); time2=cbind(time1,time1)	
	LSC1=qchisq(0.997,p*(p+1)/2); z1=cbind(W1,rep(LSC1,m))
	produto1=1; produto2=1; x=seq(1,m)
	for ( i in 1:p) produto1=(n-i)*produto1
	for ( i in 1:p) produto2=(n-i+2)*produto2
	b1=produto1/((n-1)^p)
	b2=produto1*(produto2-produto1)/((n-1)^(2*p)) 
	LIC2=det1*(b1-3*sqrt(b2)); LSC2=det1*(b1+3*sqrt(b2))
	if (LIC2<=0) LIC2=0
	z2=cbind(W2,rep(b1*det1,m),rep(LIC2,m),rep(LSC2,m))
	par(mfrow=c(1,2))
	matplot(time1, z1, type="o", pch=20, col=c("black","red"),
		main="Método 1", xlab="Amostras", ylab="W",
		ylim=c(min(z1), max(z1)), lty=1, lwd=2)
	matplot(time2,z2,type="o",pch=20, main="Método 2",
	 	col=c("black","blue","red","red"), lty=1,lwd=2,
		xlab="Amostras",ylab="det(S)", ylim=c(min(z2), max(z2)))
}

#####################################################################################################################

##### Quadro 7.5 - Códigos para construção do gráfico T²

# n = número de observações por amostra.
# y = matriz com os dados coletados. Cada coluna deve se referir a uma variável.
# VM = vetor contendo as médias especificadas para cada variável.
# MC = matriz de covariâncias das variáveis especificada a priori. 
# etapa = variável dicotômica que deve assumir o valor 1 caso se deseje construir 
# o gráfico T2 para identificar as amostras fora de controle e 2, caso se deseje 
# estabelecer quais são as estimativas do vetor de médias, da matriz de covariâncias e 
# do limite superior de controle, visando o monitoramento futuro do processo.     

T2ce=function(n,y,VM,MC){
	p=ncol(y); obs=nrow(y); m=obs/n;
	amostra=matrix(NA,n,p); t2=numeric(m)
	if(n>1) {
		for (i in 1:m) {
			amostra=y[(n*i-n+1):(n*i),]
			media=apply(amostra,2,mean)
			t2[i]=n*((media-VM)%*%solve(MC)%*%(media-VM))
		}
	} else {
		for (i in 1:m ) t2[i]=(y[i,]-VM)%*%solve(MC)%*%(y[i,]-VM)
	}
	time=cbind(seq(1,m),seq(1,m)); LSC=qchisq(0.997,p)
	W=cbind(t2,rep(LSC,m))
	par(mfrow=c(1,1))
		matplot(time, W, type="o", pch=20, col=c("black","red"),
			main=expression(paste("Gráfico " * T^2)), xlab="Amostras", ylab=expression(paste(T^2)),
			ylim=c(min(W), max(W)), lty=1, lwd=2)
}

T2se=function(n,y,etapa){
	p=ncol(y);obs=nrow(y);m=obs/n
	if(n>1) {
		Sbarra=matrix(0,p,p); media=matrix(NA,m,p)
		amostra=matrix(NA,n,p); t2=numeric(m)	
		for (i in 1:m) {
			amostra=y[(n*i-n+1):(n*i),]
			media[i,]=apply(amostra,2,mean); S=cov(amostra)
			Sbarra=S+Sbarra
		}
		x2barras=apply(media,2,mean); Sinv=solve(Sbarra/m)
		for (i in 1:m) {
			z=media[i,]-x2barras; t2[i]=n*(z%*%Sinv%*%z)
		}
		if(etapa==1) {
			LSC=p*(m-1)*(n-1)*qf(0.997,p,m*n-m-p+1)/(m*n-m-p+1)
		} 
		if(etapa==2) {
			LSC=p*(m+1)*(n-1)*qf(0.997,p,m*n-m-p+1)/(m*n-m-p+1)
		}
		time=cbind(seq(1,m),seq(1,m)); W1=cbind(t2,rep(LSC,m))
		par(mfrow=c(1,1))
		matplot(time, W1, type="o", pch=20, col=c("black","red"),
			main=expression(paste("Gráfico " * T^2)), xlab="Amostras", ylab=expression(paste(T^2)),
			ylim=c(min(W1), max(W1)), lty=1,lwd=2)
		if (etapa==2) {
			lista=list(x2barras,Sbarra/m,LSC)
			names(lista)=c("Vetor de Médias",
			"Matriz de Covariâncias", 
			"Limite Superior de Controle")
			lista
		}
	} else {
		t2m1=numeric(m); t2m2=numeric(m); xbarra=apply(y,2,mean)
		Sinv1=solve(cov(y)); V=matrix(NA,m-1,p)
		for (i in 1:m-1) V[i,]=y[i+1,]-y[i,]
		S=(t(V)%*%V)/(2*(m-1));	Sinv2=solve(S)
		for (i in 1:m) {
			z=y[i,]-xbarra; t2m1[i]=z%*%Sinv1%*%z
			t2m2[i]=z%*%Sinv2%*%z
		}
		time=cbind(seq(1,m),seq(1,m))
		if(etapa==1) {
			LSC=((m-1)^2)*qbeta(0.997,p/2,(m-p-1)/2)/m
		}
		if(etapa==2) {
			LSC=p*(m+1)*(m-1)*qf(0.997,p,m-p)/(m*(m-p))
		} 
		W1=cbind(t2m1,rep(LSC,m)); W2=cbind(t2m2,rep(LSC,m))
		par(mfrow=c(1,2))
		matplot(time, W1, type="o", pch=20, col=c("black","red"),
			main=expression(paste("Gráfico " * T^2 * " - Estimador 1")), xlab="Amostras", ylab=expression(paste(T^2)),
			ylim=c(min(W1), max(W1)), lty=1, lwd=2)
		matplot(time, W2, type="o", pch=20, col=c("black","red"),
			main=expression(paste("Gráfico " * T^2 * " - Estimador 2")), xlab="Amostras", ylab=expression(paste(T^2)),
			ylim=c(min(W2), max(W2)), lty=1, lwd=2)
		if (etapa==2) {
			lista=list(xbarra,S,LSC)
			names(lista)=c("Vetor de Médias",
			"Matriz de Covariâncias", 
			"Limite Superior de Controle")
			lista
		}
	}
}

#####################################################################################################################

##### Quadro 8.1 - Cálculo dos índices de capacidade do processo multivariado

# y = matriz com os dados coletados. Cada coluna deve se referir a uma  variável.
# LIE = vetor de especificação inferior de cada variável.
# LSE = vetor de especificação superior de cada variável.
# v = número ideal de componentes a ser utilizado na análise (use os   códigos do Quadro 7.6 para escolher v).
# alpha = nível de confiança dos intervalos a serem construídos para os índices.

ICAPM=function(y,LIE,LSE,v,alpha) {
	Qinf=0.5-alpha/2; Qsup=0.5+alpha/2; n=nrow(y); p=ncol(y)  
	LIE=as.matrix(LIE,p,1); LSE=as.matrix(LSE,p,1) 
	T=(LSE+LIE)/2; xbar=apply(y,2,mean); S=cov(y); gl=n-1; N=20000 
	Cp=numeric(v); I1=numeric(v); I2=numeric(v); Cpk=numeric(v)
	c1=numeric(v); V=numeric(v); Cpm=numeric(v); cbar=numeric(v)
	S2c=numeric(v); Cpc=numeric(v); x=matrix(NA,n,p);	x1=matrix(NA,N,v); x2=matrix(NA,N,v); n1=numeric(N)
	n2=numeric(N); LIE3=numeric(v); LSE3=numeric(v)   
	u=eigen(S)$vectors; lambda=eigen(S)$values; S2=diag(lambda)
	LIE2=t(u)%*%LIE; LSE2=t(u)%*%LSE; T2=t(u)%*%T; xbar2=t(u)%*%xbar
	PC=as.matrix(y,n,p)%*%u
      for (i in 1:v) { Cp[i]=abs(LSE2[i]-LIE2[i])/(6*sqrt(lambda[i])) }
	log.MCp=(1/v)*sum(log(Cp)); MCp=exp(log.MCp)
	log.MCpinf=(1/v)*sum(log(Cp*sqrt(qchisq(Qinf,gl)/gl)))
	MCpinf=exp(log.MCpinf)
	log.MCpsup=(1/v)*sum(log(Cp*sqrt(qchisq(Qsup,gl)/gl)))
	MCpsup=exp(log.MCpsup)
	for (i in 1:v) { I1[i]=abs(LSE2[i]-xbar2[i])/(3*sqrt(lambda[i]))
 	   		     I2[i]=abs(xbar2[i]-LIE2[i])/(3*sqrt(lambda[i]))
			     Cpk[i]=min(c(I1[i],I2[i])) }
	log.MCpk=(1/v)*sum(log(Cpk)); MCpk=exp(log.MCpk)
	soma1=0; soma2=0
      for (i in 1:v) { c1[i]=1/(9*n*(Cpk[i]^2)); c2=1/(2*gl)	
	          soma1=soma1+log(Cpk[i]*(1-qnorm(Qsup)*sqrt(c1[i]+c2)))
                soma2=soma2+log(Cpk[i]*(1+qnorm(Qsup)*sqrt(c1[i]+c2))) }
      log.MCpkinf=(1/v)*soma1; MCpkinf=exp(log.MCpkinf)
	log.MCpksup=(1/v)*soma2; MCpksup=exp(log.MCpksup)
	for (i in 1:v) { V[i]=(xbar2[i]-T2[i])/sqrt(lambda[i])
			     Cpm[i]=Cp[i]/(sqrt(1+V[i]^2)) }
	log.MCpm=(1/v)*sum(log(Cpm)); MCpm=exp(log.MCpm)
	d=(LSE-LIE)/2 
	for (i in 1:p) { x[,i]=y[,i]/d[i,1] } 
	Sx=cov(x)
	MPpc=(1+sqrt(2))/(6*sqrt(eigen(Sx)$values[1]))
	library(MASS)
	set.seed(4330); amostra1=mvrnorm(N,T2[1:v],S2[1:v,1:v])
	set.seed(2952); amostra2=mvrnorm(N,xbar2[1:v],S2[1:v,1:v])
	for (i in 1:v) { for (j in 1:N) { 
      LIE3[i]=min(LIE2[i],LSE2[i]); LSE3[i]=max(LIE2[i],LSE2[i]) 
	if (amostra1[j,i]>LIE3[i]&amostra1[j,i]<LSE3[i]) x1[j,i]=1 else x1[j,i]=0
	if (amostra2[j,i]>LIE3[i]&amostra2[j,i]<LSE3[i]) x2[j,i]=1 else x2[j,i]=0 } }
	for (j in 1:N) { soma3=sum(x1[j,]) 
                       if (soma3==v) n1[j]=1 else n1[j]=0
			     soma4=sum(x2[j,])
                       if (soma4==v) n2[j]=1 else n2[j]=0 }
	Mp1=sum(n1)/N; Mp2=sum(n2)/N
	out1=matrix(NA,2,3)
	IC1=c(MCpinf,MCp,MCpsup); out1[1,]=round(IC1,2)
	IC2=c(MCpkinf,MCpk,MCpksup); out1[2,]=round(IC2,2)
	colnames(out1)=c("Limite Inferior",
            "Estimativa Pontual","Limite Superior")
	rownames(out1)=c("MCp","MCpk")
	out2=matrix(NA,2,1) 
	out2[1,]=round(c(MCpm),2); out2[2,]=round(c(MPpc),2) 
	colnames(out2)=c("Estimativa Pontual") 
	rownames(out2)=c("MCpm","MPpc")
	out3=matrix(NA,2,1) 
	out3[1,]=round(c(Mp1),5); out3[2,]=round(c(Mp2),5) 
	colnames(out3)=c("Estimativa Pontual") 
	rownames(out3)=c("Mp1","Mp2")
	lista=list(out1,out2,out3); lista
}

#####################################################################################################################