#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
require(plotly)
library(shiny)
source("Analise_texto.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
   # Application title
   titlePanel("Text Distance Analysis"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Insira o arquivo PDF a ser analisado.csv",
                  accept = c(
                    "text/pdf",".pdf") ),
        fileInput("file2", "Insira o arquivo PDF a ser analisado.csv",
                  accept = c(
                    "text/pdf",".pdf") ),
         sliderInput("Palavras",
                     "Palavras de frequencia igual ou menor a serem rejeitadas da analise",
                     min = 0,
                     max = 50,
                     value = 10),
        selectInput(inputId = "linguagens",label = "Lingua do PDF",choices = c("en","pt")),
        selectInput("escolhas",label = "Métrica",choices = c("Media","Desvio Padrao","Ambos")),
        tabsetPanel(
        #  tabPanel("Analysys",selectInput("selecionador",choices = 1:13,label="Cluster"))
          tabPanel("Analysys",uiOutput("selecionador"))
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id="Referenciador",
         tabPanel("DistanceMatrix",plotlyOutput("distPlot")),
         tabPanel("DistanceMatrix2",plotlyOutput("distPlot2")),
         tabPanel("cluster",plotOutput("Saida")),
         tabPanel("cluster2",plotOutput("Saida2")),
         tabPanel("Analysis",verbatimTextOutput("Informacoes"),tableOutput('Tabela')),
         tabPanel("Analysis2",verbatimTextOutput("Informacoes2"),tableOutput('Tabela2')),
         tabPanel("TableGraph",plotlyOutput("TableGraph")),
         tabPanel("TableGraph2",plotlyOutput("TableGraph2")),
         tabPanel("DistanceMatrixComp",plotlyOutput("DistanceGraphComparative")),
         tabPanel("TableGraphComp",plotlyOutput("TableGraphComparative"))
         
         
         
         
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=1000*1024^2)
  
  Calculo=reactive({
    if(!is.null(input$file1)){
      if(input$escolhas=="Media")
        d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="media",linguagem=input$linguagens)
      else if(input$escolhas=="Desvio Padrao")
        d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="desvio",linguagem=input$linguagens)
      else if(input$escolhas=="Ambos")
        d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="ambos",linguagem=input$linguagens)
    #  z=BestCluster(d,20)
    #return(z)
      return(d)
  }})
  
  Clusterizacao=eventReactive(c(input$linguagens,input$Palavras,input$file1$datapath,input$escolhas),{
    d=Calculo()
    z=BestCluster(Calculo(),20)
    return(z)
  })
  
  #Matriz=Calculo()
  
  output$selecionador=renderUI({
      if(!is.null(input$file1) & !is.null(input$file2)){
      #  if(input$escolhas=="Media")
      #    d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="media",linguagem=input$linguagens)
      #  else if(input$escolhas=="Desvio Padrao")
      #    d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="desvio",linguagem=input$linguagens)
        #z=BestCluster(Calculo(),20)
        if(input$Referenciador=="Analysis" | input$Referenciador=="TableGraph") {
          z=Clusterizacao()
          selectInput("selecionador",choices = 1:max(as.numeric(z$cluster)),label="Cluster")
        }
        else if(input$Referenciador=="Analysis2" | input$Referenciador=="TableGraph2") {
          z=Clusterizacao2()
          selectInput("selecionador",choices = 1:max(as.numeric(z$cluster)),label="Cluster")
        }
        else
        {
          z=Clusterizacao()
          z1=Clusterizacao2()
          v=min( c( max( as.numeric( z$cluster ) ),max( as.numeric( z1$cluster) ) ) ) 
          selectInput("selecionador",choices = 1:v,label="Cluster")
          
          
        }
        #sliderInput(inputId = "separacoes",min =2,max =0.5*nrow(w),value = 2,label="numero de subconjuntos do dataset"  ) 
      }
    }
  )
  
  output$Saida<-renderPlot({
    if(!is.null(input$file1)){
  #    if(input$escolhas=="Media")
   #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="media",linguagem=input$linguagens)
  #    else if(input$escolhas=="Desvio Padrao")
   #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="desvio",linguagem=input$linguagens)
      d=Calculo()
      #z=BestCluster(d,20)
      z=Clusterizacao()
      clusplot(d,z$cluster,lines=0,labels=4,diss=FALSE,shade=FALSE)
    }
    
    
  })
  
  output$TableGraph<-renderPlotly(
    {
      a=leitura(input$file1$datapath)
      a=Palavras(a,input$linguagens)
      
      z=Clusterizacao()
      vec=z$cluster[which(as.numeric(z$cluster)==as.numeric(input$selecionador))]
      vec=names(vec)
      vec2=c()
      for(i in 1:length(vec)){
        vec2[i]=sum(vec[i]==a) 
      }
      df=data.frame(vec,vec2)
      names(df)=c("Words","Frequency") 
      df=df[order(df[,2]),]
      grafico=ggplot(data=df,aes(x=Words,y=Frequency,col="red")) + geom_point()
      ggplotly(grafico)
      
    }
  )
  
   output$distPlot <- renderPlotly({
     if(!is.null(input$file1)){
      # caminho=input$file1$datapath
      # reject=input$Palavras
      # a=leitura(caminho)
      # a=Palavras(a)
      # b=ConversorMatriz(unlist(a),reject)
      # b=MatrizDistanciasPalavras(b,method)
       
     if(input$escolhas=="Media")
       grafico= ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="media",linguagem=input$linguagens)
     else if(input$escolhas=="Desvio Padrao")
       grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="desvio",input$linguagens)
     else if(input$escolhas=="Ambos")
       grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="ambos",input$linguagens)
     ggplotly(grafico)
     }
   })
   
   output$Informacoes<-renderPrint({
     if(!is.null(input$file1)){
     #  if(input$escolhas=="Media")
    #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="media",linguagem=input$linguagens)
    #   else if(input$escolhas=="Desvio Padrao")
    #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="desvio",linguagem=input$linguagens)
       #z=BestCluster(Calculo(),20)
       #a=leitura(input$file1$datapath)
       #a=Palavras(a,input$linguagens)
       
       z=Clusterizacao()
       vec=z$cluster[which(as.numeric(z$cluster)==as.numeric(input$selecionador))]
       vec=names(vec)
       
       print(vec)
     
   }})
   
   output$Tabela<-renderTable({
     if(!is.null(input$file1)){
       #  if(input$escolhas=="Media")
       #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="media",linguagem=input$linguagens)
       #   else if(input$escolhas=="Desvio Padrao")
       #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="desvio",linguagem=input$linguagens)
       #z=BestCluster(Calculo(),20)
       a=leitura(input$file1$datapath)
       a=Palavras(a,input$linguagens)
       
       z=Clusterizacao()
       vec=z$cluster[which(as.numeric(z$cluster)==as.numeric(input$selecionador))]
       vec=names(vec)
       vec2=c()
       for(i in 1:length(vec)){
        vec2[i]=sum(vec[i]==a) 
       }
       df=data.frame(vec,vec2)
       names(df)=c("Words","Frequency") 
       df=df[order(df[,2]),]
       return(df)
       #print(vec)
       
     }})
   
   ### copy for second file
   
   Calculo2=reactive({
     if(!is.null(input$file2)){
       if(input$escolhas=="Media")
         d=DistanceMatrix(caminho=input$file2$datapath,reject=input$Palavras,method="media",linguagem=input$linguagens)
       else if(input$escolhas=="Desvio Padrao")
         d=DistanceMatrix(caminho=input$file2$datapath,reject=input$Palavras,method="desvio",linguagem=input$linguagens)
       else if(input$escolhas=="Ambos")
         d=DistanceMatrix(caminho=input$file2$datapath,reject=input$Palavras,method="ambos",linguagem=input$linguagens)
       #  z=BestCluster(d,20)
       #return(z)
       return(d)
     }})
   
   Clusterizacao2=eventReactive(c(input$linguagens,input$Palavras,input$file1$datapath,input$escolhas),{
     d=Calculo2()
     z=BestCluster(Calculo2(),20)
     return(z)
   })
   
   #Matriz=Calculo()
   
   output$selecionador2=renderUI({
     if(!is.null(input$file2)){
       #  if(input$escolhas=="Media")
       #    d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="media",linguagem=input$linguagens)
       #  else if(input$escolhas=="Desvio Padrao")
       #    d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="desvio",linguagem=input$linguagens)
       #z=BestCluster(Calculo(),20)
       z=Clusterizacao2()
       selectInput("selecionador",choices = 1:max(as.numeric(z$cluster)),label="Cluster") 
       #sliderInput(inputId = "separacoes",min =2,max =0.5*nrow(w),value = 2,label="numero de subconjuntos do dataset"  ) 
     }
   }
   )
   
   output$Saida2<-renderPlot({
     if(!is.null(input$file2)){
       #    if(input$escolhas=="Media")
       #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="media",linguagem=input$linguagens)
       #    else if(input$escolhas=="Desvio Padrao")
       #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="desvio",linguagem=input$linguagens)
       d=Calculo2()
       #z=BestCluster(d,20)
       z=Clusterizacao2()
       clusplot(d,z$cluster,lines=0,labels=4,diss=FALSE,shade=FALSE)
     }
     
     
   })
   
   output$TableGraph2<-renderPlotly(
     {
       a=leitura(input$file2$datapath)
       a=Palavras(a,input$linguagens)
       
       z=Clusterizacao2()
       vec=z$cluster[which(as.numeric(z$cluster)==as.numeric(input$selecionador))]
       vec=names(vec)
       vec2=c()
       for(i in 1:length(vec)){
         vec2[i]=sum(vec[i]==a) 
       }
       df=data.frame(vec,vec2)
       names(df)=c("Words","Frequency") 
       df=df[order(df[,2]),]
       grafico=ggplot(data=df,aes(x=Words,y=Frequency,col="red")) + geom_point()
       ggplotly(grafico)
       
     }
   )
   
   output$TableGraphComparative<-renderPlotly(
     {
       a2=leitura(input$file1$datapath)
       a2=Palavras(a2,input$linguagens)
       a=leitura(input$file2$datapath)
       a=Palavras(a,input$linguagens)
       
       z=Clusterizacao2()
       z1=Clusterizacao()
       #vec=z$cluster[which(as.numeric(z$cluster)==as.numeric(input$selecionador))]
       #vec=names(vec)
       #vec2=c()
       #for(i in 1:length(vec)){
      #   vec2[i]=sum(vec[i]==a) 
      # }
      # df=data.frame(vec,vec2)
       vec=z1$cluster[which(as.numeric(z1$cluster)==as.numeric(input$selecionador))]
       vec=names(vec)
       vec2=c()
       for(i in 1:length(vec)){
         vec2[i]=sum(vec[i]==a2) 
       }
       df1=data.frame(vec,vec2)
       contidos=z$cluster[which(names(z$cluster) %in% df1[,1])]
       print(contidos)
       if(length(contidos)>0){
       frequency=table(as.numeric(contidos))
       print(frequency)
       index = min(which(max(as.numeric(frequency))==as.numeric(frequency) ) )
       #print(index)
       index=as.numeric(names(frequency[index]) )
       vec=z$cluster[which(as.numeric(z$cluster)==index)]
       #vec=z$cluster[index]
       vec=names(vec)
       vec2=c()
       for(i in 1:length(vec)){
         vec2[i]=sum(vec[i]==a) 
       }
       df=data.frame(vec,vec2)
       print(df)
       names(df)=c("Words","Frequency")
       names(df1)=c("Words","Frequency")
       df=df[order(df[,2]),]
       df1=df1[order(df1[,2]),]
       df=data.frame(df,"file1")
       df1=data.frame(df1,"file2")
       #print(df)
       #print("df1")
       #print(df1)
       #print("df")
       names(df)=c("Words","Frequency","File")
       names(df1)=c("Words","Frequency","File")
       ender1= which((df[,1] %in% df1[,1]) )
       ender2=which((df1[,1] %in% df[,1]) )
       df=df[ender1,]
       df1=df1[ender2,]
       print("df pos filtro")
       print(df)
       print("df1 pos filtro")
       print(df1)
       df=rbind(df,df1)
       }
       else
         df=data.frame()
       if(nrow(df)==0){
         df=data.frame("SemIntersecao",1,"Ambos")
         names(df)=c("Words","Frequency","File")
         grafico=ggplot(data=df,aes(x=Words,y=Frequency,col=File)) + geom_point()
         ggplotly(grafico)
        # df[1,]=c("SemIntercessao",1,"red")
         #df[1,1]="SemIntercessao" 
       }
       #print(df)
       else{
        grafico=ggplot(data=df,aes(x=Words,y=Frequency,col=File)) + geom_point()
        ggplotly(grafico)
       }
     }
   )
   
   output$DistanceGraphComparative<-renderPlotly(
     {
       a=Calculo()
       b=Calculo2()
       c=DiferencaMatrizes(a,b)
       c=MatrixggplotShiny(c)
       ggplotly(c)
     })
   
   output$distPlot2 <- renderPlotly({
     if(!is.null(input$file2)){
       # caminho=input$file1$datapath
       # reject=input$Palavras
       # a=leitura(caminho)
       # a=Palavras(a)
       # b=ConversorMatriz(unlist(a),reject)
       # b=MatrizDistanciasPalavras(b,method)
       
       if(input$escolhas=="Media")
         grafico= ProcessoShiny(caminho=input$file2$datapath,reject=input$Palavras,graph="gg",method="media",linguagem=input$linguagens)
       else if(input$escolhas=="Desvio Padrao")
         grafico=ProcessoShiny(caminho=input$file2$datapath,reject=input$Palavras,graph="gg",method="desvio",input$linguagens)
       else if(input$escolhas=="Ambos")
         grafico=ProcessoShiny(caminho=input$file2$datapath,reject=input$Palavras,graph="gg",method="ambos",input$linguagens)
       ggplotly(grafico)
     }
   })
   
   output$Informacoes2<-renderPrint({
     if(!is.null(input$file2)){
       #  if(input$escolhas=="Media")
       #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="media",linguagem=input$linguagens)
       #   else if(input$escolhas=="Desvio Padrao")
       #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="desvio",linguagem=input$linguagens)
       #z=BestCluster(Calculo(),20)
       #a=leitura(input$file1$datapath)
       #a=Palavras(a,input$linguagens)
       
       z=Clusterizacao2()
       vec=z$cluster[which(as.numeric(z$cluster)==as.numeric(input$selecionador))]
       vec=names(vec)
       
       print(vec)
       
     }})
   
   output$Tabela2<-renderTable({
     if(!is.null(input$file2)){
       #  if(input$escolhas=="Media")
       #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="media",linguagem=input$linguagens)
       #   else if(input$escolhas=="Desvio Padrao")
       #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="desvio",linguagem=input$linguagens)
       #z=BestCluster(Calculo(),20)
       a=leitura(input$file2$datapath)
       a=Palavras(a,input$linguagens)
       
       z=Clusterizacao2()
       vec=z$cluster[which(as.numeric(z$cluster)==as.numeric(input$selecionador))]
       vec=names(vec)
       vec2=c()
       for(i in 1:length(vec)){
         vec2[i]=sum(vec[i]==a) 
       }
       df=data.frame(vec,vec2)
       names(df)=c("Words","Frequency") 
       df=df[order(df[,2]),]
       return(df)
       #print(vec)
       
     }})
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

