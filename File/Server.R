##############################################################################################################

#SERVER

###############################################################################################################

# create the server functions for the dashboard  
server <- function(input, output,session) { 
  
  output$dateText2 <- renderText({
    paste("input$date2 is", as.character(input$date2))
  })
  
  #################################################################################################################
  
  #FILE
  
  #################################################################################################################
  
  # elaborazione regioni
  
  file_regioni<-"https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-regioni.json"%>% 
    GET() %>% 
    content() %>% 
    jsonlite::fromJSON(simplifyVector = FALSE)
  
  d_reg<-data.frame("data"=as.character(),"stato"=as.character(),"codice_regione"=as.character(),"denominazione_regione"=as.character(),
                    "lat"=as.numeric(),"long"=as.numeric(),"ricoverati_con_sintomi"=as.numeric(),"terapia_intensiva"=as.numeric(),
                    "totale_ospedalizzati"=as.numeric(),"isolamento_domiciliare"=as.numeric(),"totale_attualmente_positivi"=as.numeric(),
                    "nuovi_attualmente_positivi"=as.numeric(),"dismessi_guariti"=as.numeric(),"deceduti"=as.numeric(),"totale_casi"=as.numeric(),
                    "tamponi"=as.numeric())
  
  for(i in 1:length(file_regioni)){
    d_temp<-json_data_frame <- as.data.frame(file_regioni[[i]])
    d_reg<-rbind(d_reg,d_temp)
  }
  d_reg$totale_attualmente_positivi[d_reg$totale_attualmente_positivi==0]<-NA
  names(d_reg)[3]<-"reg_istat_code_num"
  names(d_reg)[4]<-"reg_name"
  d_reg$reg_name<-as.character(d_reg$reg_name)
  d_reg$data<-as.POSIXct(d_reg$data)
  d_reg$stato<-as.character(d_reg$stato)
  
  for(i in 1:nrow(d_reg)){
    if(is.na(d_reg$totale_attualmente_positivi[i])){
      d_reg$colore[i]<-"transparent"
    }else{
      if(d_reg$totale_attualmente_positivi[i]<=10){
        d_reg$colore[i]<-"#a4a4f5"
      }
      if(d_reg$totale_attualmente_positivi[i]>10 && d_reg$totale_attualmente_positivi[i]<=20 ){
        d_reg$colore[i]<-"#8c8cf5"
      }
      if(d_reg$totale_attualmente_positivi[i]>20 && d_reg$totale_attualmente_positivi[i]<=50){
        d_reg$colore[i]<-"#6e6ef0"
      }
      if(d_reg$totale_attualmente_positivi[i]>50 && d_reg$totale_attualmente_positivi[i]<=100){
        d_reg$colore[i]<-"#5151f0"
      }
      if(d_reg$totale_attualmente_positivi[i]>100 && d_reg$totale_attualmente_positivi[i]<=250){
        d_reg$colore[i]<-"#3a3af0"
      }
      if(d_reg$totale_attualmente_positivi[i]>250 && d_reg$totale_attualmente_positivi[i]<=500){
        d_reg$colore[i]<-"#1a1aed"
      }
      if(d_reg$totale_attualmente_positivi[i]>500 && d_reg$totale_attualmente_positivi[i]<=750){
        d_reg$colore[i]<-"#0808d1"
      }
      if(d_reg$totale_attualmente_positivi[i]>750 && d_reg$totale_attualmente_positivi[i]<=1250){
        d_reg$colore[i]<-"#0707ad"
      }
      if(d_reg$totale_attualmente_positivi[i]>1250 && d_reg$totale_attualmente_positivi[i]<=2500){
        d_reg$colore[i]<-"#070785"
      }
      if(d_reg$totale_attualmente_positivi[i]>2500 && d_reg$totale_attualmente_positivi[i]<=3500){
        d_reg$colore[i]<-"#050563"
      }
      if(d_reg$totale_attualmente_positivi[i]>3500 && d_reg$totale_attualmente_positivi[i]<=4000 ){
        d_reg$colore[i]<-"#040447"
      }
      if(d_reg$totale_attualmente_positivi[i]>4000){
        d_reg$colore[i]<-"#000000"
      }
    }
    
    
  }
  
  
  
  # elaborazione nazione
  d_reg2<-d_reg
  d_reg2[is.na(d_reg2)]<-0
  
  d_naz<-aggregate(list(d_reg2$ricoverati_con_sintomi,d_reg2$terapia_intensiva,d_reg2$totale_ospedalizzati,d_reg2$isolamento_domiciliare,
                        d_reg2$totale_attualmente_positivi,d_reg2$nuovi_attualmente_positivi,d_reg2$dimessi_guariti,d_reg2$deceduti,d_reg2$totale_casi),by=list(d_reg2$stato,d_reg2$data),FUN=sum)
  
  names(d_naz)<-c("stato","data","ricoverati_con_sintomi","terapia_intensiva","totale_ospedalizzati","isolamento_domiciliare","totale_attualmente_positivi","nuovi_attualmente_positivi","dimessi_guariti","deceduti","totale_casi")
  d_naz$variazione_rs_ass[1]<-NA
  d_naz$variazione_rs_per[1]<-NA
  d_naz$variazione_ti_ass[1]<-NA
  d_naz$variazione_ti_per[1]<-NA
  d_naz$variazione_to_ass[1]<-NA
  d_naz$variazione_to_per[1]<-NA
  d_naz$variazione_id_ass[1]<-NA
  d_naz$variazione_id_per[1]<-NA
  d_naz$variazione_nap_per[1]<-NA
  d_naz$variazione_dg_ass[1]<-NA
  d_naz$variazione_dg_per[1]<-NA
  d_naz$variazione_d_ass[1]<-NA
  d_naz$variazione_d_per[1]<-NA
  d_naz$variazione_tc_ass[1]<-NA
  d_naz$variazione_tc_per[1]<-NA
  
  
  for(i in 1:(nrow(d_naz)-1)){
    d_naz$variazione_rs_ass[i+1]<-d_naz$ricoverati_con_sintomi[i+1]-d_naz$ricoverati_con_sintomi[i]
    d_naz$variazione_rs_per[i+1]<-(d_naz$ricoverati_con_sintomi[i+1]-d_naz$ricoverati_con_sintomi[i])/d_naz$ricoverati_con_sintomi[i]*100
    d_naz$variazione_ti_ass[i+1]<-d_naz$terapia_intensiva[i+1]-d_naz$terapia_intensiva[i]
    d_naz$variazione_ti_per[i+1]<-(d_naz$terapia_intensiva[i+1]-d_naz$terapia_intensiva[i])/d_naz$terapia_intensiva[i]*100
    d_naz$variazione_to_ass[i+1]<-d_naz$totale_ospedalizzati[i+1]-d_naz$totale_ospedalizzati[i]
    d_naz$variazione_to_per[i+1]<-(d_naz$totale_ospedalizzati[i+1]-d_naz$totale_ospedalizzati[i])/d_naz$totale_ospedalizzati[i]*100
    d_naz$variazione_id_ass[i+1]<-d_naz$isolamento_domiciliare[i+1]-d_naz$isolamento_domiciliare[i]
    d_naz$variazione_id_per[i+1]<-(d_naz$isolamento_domiciliare[i+1]-d_naz$isolamento_domiciliare[i])/d_naz$isolamento_domiciliare[i]*100
    d_naz$variazione_nap_per[i+1]<-(d_naz$nuovi_attualmente_positivi[i+1]/d_naz$totale_attualmente_positivi[i])*100
    d_naz$variazione_dg_ass[i+1]<-d_naz$dimessi_guariti [i+1]-d_naz$dimessi_guariti [i]
    d_naz$variazione_dg_per[i+1]<-(d_naz$dimessi_guariti [i+1]-d_naz$dimessi_guariti [i])/d_naz$dimessi_guariti [i]*100
    d_naz$variazione_d_ass[i+1]<-d_naz$deceduti[i+1]-d_naz$deceduti[i]
    d_naz$variazione_d_per[i+1]<-(d_naz$deceduti[i+1]-d_naz$deceduti[i])/d_naz$deceduti[i]*100
    d_naz$variazione_tc_ass[i+1]<-d_naz$totale_casi[i+1]-d_naz$totale_casi[i]
    d_naz$variazione_tc_per[i+1]<-(d_naz$totale_casi[i+1]-d_naz$totale_casi[i])/d_naz$totale_casi[i]*100
  }
  
  gat1<-gather(d_naz,"Statistica","Valore",ricoverati_con_sintomi:variazione_tc_per,na.rm=TRUE)
  gat1$Statistica<-toupper(gat1$Statistica)
  gat1$Statistica<-gsub("_"," ",gat1$Statistica)
  Ospedalizzati<-gat1[gat1$Statistica=="RICOVERATI CON SINTOMI" | gat1$Statistica=="TERAPIA INTENSIVA" ,]
  Positivi<-gat1[gat1$Statistica=="ISOLAMENTO DOMICILIARE" | gat1$Statistica=="TOTALE OSPEDALIZZATI" ,]
  Casi<-gat1[gat1$Statistica=="DECEDUTI" | gat1$Statistica=="DIMESSI GUARITI"| gat1$Statistica=="TOTALE ATTUALMENTE POSITIVI" ,]
  
  
  
  filtrata_naz<-d_naz[d_naz$data==max(d_naz$data),]
  
  # elaborazione province
  
  file_province<-"https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-province.json"%>%
    GET() %>% 
    content() %>% 
    jsonlite::fromJSON(simplifyVector = FALSE)
  
  
  d_prov<-data.frame("data"=as.character(),"stato"=as.character(),"codice_regione"=as.character(),"denominazione_regione"=as.character(),
                     "codice_provincia"=as.numeric(),"denominazione_provincia"=as.character(),"sigla_provincia"=as.character(),
                     "lat"=as.numeric(),"long"=as.numeric(),"totale_casi"=as.numeric())
  
  for(i in 1:length(file_province)){
    if(is.null(file_province[[i]]$totale_casi)){
      file_province[[i]]$totale_casi<-0
    }
    d_temp<-json_data_frame <- as.data.frame(file_province[[i]])
    d_prov<-rbind(d_prov,d_temp)
  }
  names(d_prov)[5]<-"prov_istat_code_num"
  d_prov$denominazione_regione<-as.character(d_prov$denominazione_regione)
  d_prov$denominazione_provincia<-as.character(d_prov$denominazione_provincia)
  d_prov$sigla_provincia<-as.character(d_prov$sigla_provincia)
  d_prov$data<-as.POSIXct(d_prov$data)
  d_prov$stato<-as.character(d_prov$stato)
  d_prov$totale_casi[d_prov$totale_casi==0]<-NA
  
  d_prov$lat[which(d_prov$denominazione_provincia=="In fase di definizione/aggiornamento")]<-NA
  d_prov$long[which(d_prov$denominazione_provincia=="In fase di definizione/aggiornamento")]<-NA
  
  
  
  for(i in 1:nrow(d_prov)){
    if(is.na(d_prov$totale_casi[i])){
      d_prov$colore[i]<-"transparent"
    }else{
      if(d_prov$totale_casi[i]<=10){
        d_prov$colore[i]<-"#a4a4f5"
      }
      if(d_prov$totale_casi[i]>10 && d_prov$totale_casi[i]<=20 ){
        d_prov$colore[i]<-"#8c8cf5"
      }
      if(d_prov$totale_casi[i]>20 && d_prov$totale_casi[i]<=50){
        d_prov$colore[i]<-"#6e6ef0"
      }
      if(d_prov$totale_casi[i]>50 && d_prov$totale_casi[i]<=100){
        d_prov$colore[i]<-"#5151f0"
      }
      if(d_prov$totale_casi[i]>100 && d_prov$totale_casi[i]<=150){
        d_prov$colore[i]<-"#3a3af0"
      }
      if(d_prov$totale_casi[i]>150 && d_prov$totale_casi[i]<=200){
        d_prov$colore[i]<-"#1a1aed"
      }
      if(d_prov$totale_casi[i]>200 && d_prov$totale_casi[i]<=400){
        d_prov$colore[i]<-"#0808d1"
      }
      if(d_prov$totale_casi[i]>400 && d_prov$totale_casi[i]<=550){
        d_prov$colore[i]<-"#0707ad"
      }
      if(d_prov$totale_casi[i]>550 && d_prov$totale_casi[i]<=750){
        d_prov$colore[i]<-"#070785"
      }
      if(d_prov$totale_casi[i]>750 && d_prov$totale_casi[i]<=900){
        d_prov$colore[i]<-"#050563"
      }
      if(d_prov$totale_casi[i]>900 && d_prov$totale_casi[i]<=1400 ){
        d_prov$colore[i]<-"#040447"
      }
      if(d_prov$totale_casi[i]>1400 ){
        d_prov$colore[i]<-"#000000"
      }
    }
  }
  
  d_reg <- data.table(d_reg)
  d_prov <- data.table(d_prov)
  d_naz <- data.table(d_naz)
  Ospedalizzati<-data.table(Ospedalizzati)
  Casi<- data.table(Casi)
  Positivi<-data.table(Positivi)
  
  ####################################################################################################################
  
  #INIZIO ELABORAZIONE
  
  ###################################################################################################################
  
  #as.data.frame(DT[Denominazione %like% maiuscoloNome()])
  
  output$visuale <- renderPrint({ input$SceltaVisuale })
  
  Giorno_Naz<-reactive({
    if(nrow(as.data.frame(d_naz[d_naz$data %like% input$date2,]))!=0){
      filtrata_naz<-as.data.frame(d_naz[d_naz$data %like% input$date2,])
    }
    else{
      if(nrow(as.data.frame(d_naz[d_naz$data %like% input$date2,]))==0){
        filtrata_naz<-as.data.frame(d_naz[d_naz$data==max(d_naz$data),])
      }
    }
  })
  Giorno_Reg<-reactive({
    if(nrow(as.data.frame(d_reg[d_reg$data %like% input$date2,]))!=0){
      filtrata_reg<-as.data.frame(d_reg[d_reg$data %like% input$date2,])
    }else{
      if(nrow(as.data.frame(d_reg[d_reg$data %like% input$date2,]))==0){
        filtrata_reg<-as.data.frame(d_reg[d_reg$data==max(d_reg$data),])
      }
    }
  })
  Giorno_Prov<-reactive({
    if(nrow(as.data.frame(d_prov[d_prov$data %like% input$date2,]))!=0){
      filtrata_prov<-as.data.frame(d_prov[d_prov$data %like% input$date2,])
      
    }else{
      if(nrow(as.data.frame(d_prov[d_prov$data %like% input$date2,]))==0){
        filtrata_prov<-as.data.frame(d_prov[d_prov$data==max(d_prov$data),])
      }
    }
  })
  
  Giorno_Osp<-reactive({
    if(nrow(as.data.frame(Ospedalizzati[Ospedalizzati$data %like% input$date2,]))!=0){
      Ospedalizzati_fil<-as.data.frame(Ospedalizzati[Ospedalizzati$data %like% input$date2,])
    }else{
      if(nrow(as.data.frame(Ospedalizzati[Ospedalizzati$data %like% input$date2,]))==0){
        Ospedalizzati_fil<-as.data.frame(Ospedalizzati[Ospedalizzati$data==max(Ospedalizzati$data),])
      }
    }
  })
  Giorno_Pos<-reactive({
    if(nrow(as.data.frame(Positivi[Positivi$data %like%input$date2,]))!=0){
      Positivi_fil<-as.data.frame(Positivi[Positivi$data %like%input$date2,])
    }else{
      if(nrow(as.data.frame(Positivi[Positivi$data %like%input$date2,]))==0){
        Positivi_fil<-as.data.frame(Positivi[Positivi$data==max(Positivi$data),])
      }
    }
  })
  Giorno_Cas<-reactive({
    if(nrow(as.data.frame(Casi[Casi$data %like% input$date2,]))!=0){
      Casi_fil<-as.data.frame(Casi[Casi$data %like% input$date2,])
    }else{
      if(nrow(as.data.frame(Casi[Casi$data %like% input$date2,]))==0){
        Casi_fil<-as.data.frame(Casi[Casi$data==max(Casi$data),])
      }
    }
  })
  output$giornoAnalisi<-renderTable({
    
    data.frame("Data"=paste0("Giorno visualizzato: ",as.Date(unique(Giorno_Reg()$data))))
  })
  
  #creating the valueBoxOutput content
  output$totale_casi_naz <- renderValueBox({
    valueBox(
      formatC(Giorno_Naz()$totale_casi, format="d", big.mark=',')
      ,paste('Totale casi verificati')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "black")
    
  })
  
  output$positivi_naz <- renderValueBox({
    valueBox(
      formatC(Giorno_Naz()$totale_attualmente_positivi, format="d", big.mark=',')
      ,paste('Attualmente positivi')
      ,icon = icon("procedures",lib='glyphicon')
      ,color = "blue")
    
  })
  
  
  output$dimessi_naz <- renderValueBox({
    valueBox(
      formatC(Giorno_Naz()$dimessi_guariti, format="d", big.mark=',')
      ,'Dimessi guariti'
      ,icon = icon("check",lib='glyphicon')
      ,color = "navy")
    
  })
  
  output$decessi_naz <- renderValueBox({
    valueBox(
      formatC(Giorno_Naz()$deceduti, format="d", big.mark=',')
      ,'Totale decessi',
      color = "red")
    
  })
  
  
  #choose and creating map
  
  
  sceltagrafico<-reactive({
    if(input$SceltaVisuale=="Regione"){
      mytext <- paste(
        "Regione: ",Giorno_Reg()$reg_name, "<br/>",
        "Attualmente positivi: ", Giorno_Reg()$totale_attualmente_positivi, "<br/>", 
        "Guariti: ", Giorno_Reg()$dimessi_guariti, "<br/>", 
        "Decessi: ", Giorno_Reg()$deceduti, sep="") %>%
        lapply(htmltools::HTML)
      
      leaflet(Giorno_Reg()) %>% 
        addTiles()  %>% 
        setView( lat=42, lng=10.5 , zoom=4.5) %>%
        #addProviderTiles("Esri.WorldImagery") %>%
        addCircleMarkers(~long, ~lat, 
                         fillColor = ~colore , fillOpacity = 1, color="white", radius=8, stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        )
      
    }else{
      if(input$SceltaVisuale=="Provincia"){
        mytext <- paste(
          "Provincia: ",Giorno_Prov()$denominazione_provincia,"<br/>",
          "Attualmente positivi: ", Giorno_Prov()$totale_casi, "<br/>" ) %>%
          lapply(htmltools::HTML)
        
        leaflet(Giorno_Prov()) %>% 
          addTiles()  %>% 
          setView( lat=42, lng=10.5 , zoom=4.5) %>%
          #addProviderTiles("Esri.WorldImagery") %>%
          addCircleMarkers(~long, ~lat, 
                           fillColor = ~colore , fillOpacity = 1, color="white", radius=8, stroke=FALSE,
                           label = mytext,
                           labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
          )
        
      }
    }
  })
  
  output$mappa<-renderLeaflet({
    sceltagrafico()
  })
  
  
  
  # table
  
  output$value <- renderPrint({ input$checkbox })
  
  
  sceltatabella<-reactive({
    if(input$SceltaVisuale=="Regione"){
      as.data.frame(Giorno_Reg()[,c(4,7,8,9,10,11,12,13,14,15)])
      
    }else{
      if(input$SceltaVisuale=="Provincia"){
        as.data.frame(Giorno_Prov()[,c(4,6,10)])
      }
    }
  })
  
  
  sceltaOutput<-reactive({
    if(input$checkbox=="TRUE"){
      sceltatabella()
    }
  })
  
  output$tabella<-renderTable({ sceltaOutput()
  })
  
  
  #creating series plot
  
  fig <- plot_ly(d_naz, x = d_naz$data)
  fig <- fig %>% add_lines(y = d_naz$totale_casi, name = "Totale Casi", line = list(shape = "spline"))
  fig <- fig %>% add_lines(y = d_naz$dimessi_guariti, name = "Guariti", line = list(shape = "spline"))
  fig <- fig %>% add_lines(y = d_naz$deceduti, name = "Deceduti", line = list(shape = "spline"))
  fig <- fig %>% add_lines(y = d_naz$totale_attualmente_positivi, name = "Attualmente positivi", line = list(shape = "spline"))
  fig <- fig %>% layout(
    title = "Andamento Coronavirus",
    xaxis = list(
      rangeselector = list(
        buttons = list(
          list(
            count = 3,
            label = "3 days",
            step = "day",
            stepmode = "backward"),
          list(
            count = 6,
            label = "6 days",
            step = "day",
            stepmode = "backward"),
          list(
            count = 10,
            label = "10 days",
            step = "day",
            stepmode = "backward"),
          list(
            count = 1,
            label = "month",
            step = "month",
            stepmode = "todate"),
          list(step = "all"))),
      
      rangeslider = list(type = "Data")),
    
    yaxis = list(title = "Numero Persone"))
  
  fig <-fig %>% layout(plot_bgcolor='rgb(236, 240, 245)') %>% 
    layout(paper_bgcolor='rgb(236, 240, 245)')
  
  output$serie<-renderPlotly({
    fig
  })
  
  
  fig2 <- plot_ly(d_naz, x = d_naz$data)
  fig2 <- fig2 %>% add_lines(y = d_naz$variazione_tc_per, name = "Variazione Totale Casi",text = paste("Nuovi:",d_naz$variazione_tc_ass),line = list(shape = "spline"))
  fig2 <- fig2 %>% add_lines(y = d_naz$variazione_dg_per, name = "Variazione Guariti",text = paste("Nuovi:",d_naz$variazione_dg_ass),line = list(shape = "spline"))
  fig2 <- fig2 %>% add_lines(y = d_naz$variazione_d_per, name = "Variazione Deceduti",text = paste("Nuovi:",d_naz$variazione_d_ass),line = list(shape = "spline"))
  fig2 <- fig2 %>% add_lines(y = d_naz$variazione_nap_per, name = "Variazione Attualmente positivi",text = paste("Nuovi:",d_naz$nuovi_attualmente_positivi),line = list(shape = "spline"))
  fig2 <- fig2 %>% layout(
    title = "Andamento Variazione Percentuale Coronavirus",
    xaxis = list(
      rangeselector = list(
        buttons = list(
          list(
            count = 3,
            label = "3 days",
            step = "day",
            stepmode = "backward"),
          list(
            count = 6,
            label = "6 days",
            step = "day",
            stepmode = "backward"),
          list(
            count = 10,
            label = "10 days",
            step = "day",
            stepmode = "backward"),
          list(
            count = 1,
            label = "month",
            step = "month",
            stepmode = "todate"),
          list(step = "all"))),
      
      rangeslider = list(type = "Data")),
    
    yaxis = list(title = "Numero Persone"))
  
  fig2 <-fig2 %>% layout(plot_bgcolor='rgb(236, 240, 245)') %>% 
    layout(paper_bgcolor='rgb(236, 240, 245)')
  
  output$serieVariazioni<-renderPlotly({
    fig2
  })
  
  
  #creating pie chart
  
  #Ospedalizzati 
  
  colorsOsp <- c('rgb(100,149,237)', 'rgb(0,0,255)')
  
  output$ospedalizzati<-renderPlotly({
    Giorno_Osp() %>% plot_ly(labels = ~Statistica, values = ~Valore, marker = list(colors = colorsOsp,
                                                                                   line = list(color = '#FFFFFF', width = 1)))%>% 
      add_pie(hole = 0.6)%>% 
      layout(title = "Totale Ospedalizzati",  showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
      layout(plot_bgcolor='#ecf0f5') %>% 
      layout(paper_bgcolor='#ecf0f5') %>%
      layout(legend = list(orientation = 'h'))
    
    
  })
  
  #Positivi
  
  colorsPos <- c('rgb(0,0,128', 'rgb(220,20,60)')
  
  output$positivi<-renderPlotly({
    Giorno_Pos() %>% plot_ly(labels = ~Statistica, values = ~Valore, marker = list(colors = colorsPos,
                                                                                   line = list(color = '#FFFFFF', width = 1))) %>% 
      add_pie(hole = 0.6)%>% 
      layout(title = "Attualmente Positivi",  showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
      layout(plot_bgcolor='#ecf0f5') %>% 
      layout(paper_bgcolor='#ecf0f5')%>%
      layout(legend = list(orientation = 'h'))
  })
  
  #Casi
  
  colorsCas <- c('rgb(138,43,226)', 'rgb(0,128,0)','rgb(139,0,0)')
  
  output$casi<-renderPlotly({
    Giorno_Cas()%>% plot_ly(labels = ~Statistica, values = ~Valore, marker = list(colors = colorsCas,
                                                                                  line = list(color = '#FFFFFF', width = 1)))%>% 
      add_pie(hole = 0.6)%>% 
      layout(title = "Totale Casi",  showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
      layout(plot_bgcolor='#ecf0f5') %>% 
      layout(paper_bgcolor='#ecf0f5')%>%
      layout(legend = list(orientation = 'h'))
  })
  
  
}