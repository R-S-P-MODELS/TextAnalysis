
require(pdftools)
leitura=function(caminho){
library(tm)
read <- readPDF(control = list(text = "-layout"))
document <- Corpus(URISource(caminho), readerControl = list(reader = read))
doc <- content(document[[1]])
return(doc)
}

Palavras=function(texto,linguagem){
#alvo=strsplit(texto,c(" ","\n"))
alvo=strsplit(texto, "[\n ]+")
for(i in 1:length(alvo)){
	alvo[[i]]=alvo[[i]][alvo[[i]]!=""] #eliminando espacos

}
alvo=unlist(alvo)

alvo=alvo[which(!(alvo %in% stopwords(kind=linguagem)))]
auxiliarVector=c("\n")
alvo=alvo[which(!(alvo %in% auxiliarVector))]

#alvo= setdiff(alvo,stopwords(kind="pt"))

return(alvo)
}

ConversorMatriz=function(z,reject){
cont=1

u=list()
u1=c()
for(i in 1:length(unique(z))){
if(length(which(unique(z)[i]==z) )>reject ){

u[[cont]]=which(unique(z)[i]==z)
u1[[cont]]=unique(z)[i]
cont=cont+1

}
#u=u[[-1]]
}
conjunto=list(u,u1) #u1 são os nomes de u
#return(u)
return(conjunto)
}


MatrizDistanciasPalavras=function(u,method="media"){
nomes=u[[2]]
u=u[[1]]
x=matrix(nrow=length(u),ncol=length(u))
peso1=1
peso2=1
for(k in 1:length(u) ){
	for(i in 1:length(u) ){
		vec=c()
		for(j in 1:length(u[[k]]) ){
			vec[j]=	min(abs(u[[k]][j]-u[[i]]),na.rm=TRUE )

		}
		if(method=="media")		
			x[i,k]=mean(vec,na.rm=TRUE)
		else if(method=="desvio"){
			if(length(vec)==1)
				x[i,k]=0
			else
				x[i,k]=sd(vec,na.rm=TRUE)
		}
		else if(method=="ambos"){
		  if(length(vec)==1)
		    x1=0
		  else
		    x1=sd(vec,na.rm=TRUE)
		  x[i,k]=peso1*x1+peso2*mean(vec,na.rm = TRUE)
		  
		}
		else{
			print("metodo invalido")
			return(0)
		}

	}
}
rownames(x)=nomes
colnames(x)=nomes
return(x)
}

MatrixHeat=function(ly){
require(fields)
image.plot(ly)


}

Matrixggplot=function(ly){
require(reshape2)
require(ggplot2)
require(plotly)
h1=melt(ly)
p1=ggplot(data = h1, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + labs(x="",y=""  ) + theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
        axis.ticks.x=element_blank() ,axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank() )    
p1=p1+scale_fill_gradientn(colours = rainbow(20)) 
ggplotly(p1)

#ggplotly(ggplot(data = h1, aes(x=Var1, y=Var2, fill=value)) + geom_tile() +scale_color_gradientn(colours = rainbow(20)) + labs(x="",y=""  ) + theme(axis.title.x=element_blank(),
 #       axis.text.x=element_blank(),
 #       axis.ticks.x=element_blank() ,axis.title.y=element_blank(),
 #       axis.text.y=element_blank(),
 #       axis.ticks.y=element_blank() )  )  



}

MatrixggplotShiny=function(ly){
require(reshape2)
require(ggplot2)
require(plotly)
h1=melt(ly)
p1=ggplot(data = h1, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + labs(x="",y=""  ) + theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
        axis.ticks.x=element_blank() ,axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank() )    
p1=p1+scale_fill_gradientn(colours = rainbow(20)) 
#return(ggplotly(p1))
return(p1)
#ggplotly(ggplot(data = h1, aes(x=Var1, y=Var2, fill=value)) + geom_tile() +scale_color_gradientn(colours = rainbow(20)) + labs(x="",y=""  ) + theme(axis.title.x=element_blank(),
 #       axis.text.x=element_blank(),
 #       axis.ticks.x=element_blank() ,axis.title.y=element_blank(),
 #       axis.text.y=element_blank(),
 #       axis.ticks.y=element_blank() )  )  



}

Processo=function(caminho,reject,graph="Heatmap",method="media",linguagem="en"){
a=leitura(caminho)
a=Palavras(a,linguagem)
b=ConversorMatriz(a,reject)
b=MatrizDistanciasPalavras(b,method)
if(graph=="Heatmap")
	MatrixHeat(b)
else
	Matrixggplot(b)
}

ProcessoShiny=function(caminho,reject,graph="Heatmap",method="media",linguagem="en"){
a=leitura(caminho)
a=Palavras(a,linguagem)
b=ConversorMatriz(a,reject)
b=MatrizDistanciasPalavras(b,method)
if(graph=="Heatmap")
	c=MatrixHeat(b)
else
	c=MatrixggplotShiny(b)
return(c)
#ggplotly(c)
}


DiferencaMatrizes=function(m1,m2){
lix=which(colnames(m1) %in% colnames(m2))
if(length(lix)==0)
  return(matrix())
df=data.frame(1,1)

cont=1
for(i in lix){
df[cont,]=c(i,which(colnames(m1)[i]==colnames(m2)))
cont=cont+1
}
#rownames
lir=which(rownames(m1) %in% rownames(m2))
dfr=data.frame(1,1)

cont=1
for(i in lir){
dfr[cont,]=c(i,which(rownames(m1)[i]==rownames(m2)))
cont=cont+1
}
z=m1[dfr[,1],df[,1]]-m2[dfr[,2],df[,2]]

#z=m1[which(rownames(m1)==rownames(m2)),which(colnames(m1)==colnames(m2))]-m2[which(rownames(m1)==rownames(m2)),which(colnames(m1)==colnames(m2))]
return(z)
}


ChecarPlagio=function(arq1,arq2){
arq1=Palavras(arq1,"en")
arq2=Palavras(arq2,"en")
arq1=ConversorMatriz(arq1,1)
arq2=ConversorMatriz(arq2,1)
arq1=MatrizDistanciasPalavras(arq1)
arq2=MatrizDistanciasPalavras(arq2)
z3=DiferencaMatrizes(arq1,arq2)
return(z3)
}

Percentual_Similaridade=function(z,treshhold){
percent=(sum(abs(z)<treshhold)-nrow(z))/((nrow(z)-1)*(ncol(z)))
return(percent)
}

DistanceMatrix=function(caminho,linguagem="en",reject=1,method="media"){
a=leitura(caminho)
a=Palavras(a,linguagem)
b=ConversorMatriz(a,reject)
b=MatrizDistanciasPalavras(b,method)
return(b)

}

AnaliseLocal=function(arq1){
arq1=Palavras(arq1,"en")
arq1=ConversorMatriz(arq1,1)
arq1=MatrizDistanciasPalavras(arq1)
return(arq1)


}


AnaliseGrupo=function(m1,list1,tresh){
NewList=lapply(list1,AnaliseLocal)
vec=c()
for(i in 1:length(NewList)){
	vec[i]=Percentual_Similaridade(DiferencaMatrizes(m1,NewList[[i]]),tresh)

}
return(vec)
}


BestCluster=function(DistanceMatrix,MaxCluster){
require(cluster)
d=DistanceMatrix
set.seed(100)

z=kmeans(d,2)
set.seed(100)
for(i in 3:MaxCluster){
  set.seed(100)
  
	z1=kmeans(d,i)
	if((z1$betweenss/z1$totss) > (z$betweenss/z$totss) )
		z=z1
	else
		return(z)


}
return(z)

}

Clustering=function(d,MaxCluster){
z=BestCluster(d,MaxCluster)
clusplot(d,z$cluster,lines=0,labels=4,diss=FALSE,shade=FALSE)


}
