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
      d_reg$radius[i]<-0
    }else{
      if(d_reg$totale_attualmente_positivi[i]<=10){
        d_reg$colore[i]<-"#a4a4f5"
        d_reg$radius[i]<-2
      }
      if(d_reg$totale_attualmente_positivi[i]>10 && d_reg$totale_attualmente_positivi[i]<=20 ){
        d_reg$colore[i]<-"#8c8cf5"
        d_reg$radius[i]<-3
      }
      if(d_reg$totale_attualmente_positivi[i]>20 && d_reg$totale_attualmente_positivi[i]<=50){
        d_reg$colore[i]<-"#6e6ef0"
        d_reg$radius[i]<-4
      }
      if(d_reg$totale_attualmente_positivi[i]>50 && d_reg$totale_attualmente_positivi[i]<=100){
        d_reg$colore[i]<-"#5151f0"
        d_reg$radius[i]<-5
      }
      if(d_reg$totale_attualmente_positivi[i]>100 && d_reg$totale_attualmente_positivi[i]<=250){
        d_reg$colore[i]<-"#3a3af0"
        d_reg$radius[i]<-6
      }
      if(d_reg$totale_attualmente_positivi[i]>250 && d_reg$totale_attualmente_positivi[i]<=500){
        d_reg$colore[i]<-"#1a1aed"
        d_reg$radius[i]<-7
      }
      if(d_reg$totale_attualmente_positivi[i]>500 && d_reg$totale_attualmente_positivi[i]<=750){
        d_reg$colore[i]<-"#0808d1"
        d_reg$radius[i]<-8
      }
      if(d_reg$totale_attualmente_positivi[i]>750 && d_reg$totale_attualmente_positivi[i]<=1250){
        d_reg$colore[i]<-"#0707ad"
        d_reg$radius[i]<-9
      }
      if(d_reg$totale_attualmente_positivi[i]>1250 && d_reg$totale_attualmente_positivi[i]<=2500){
        d_reg$colore[i]<-"#070785"
        d_reg$radius[i]<-10
      }
      if(d_reg$totale_attualmente_positivi[i]>2500 && d_reg$totale_attualmente_positivi[i]<=3500){
        d_reg$colore[i]<-"#050563"
        d_reg$radius[i]<-11
      }
      if(d_reg$totale_attualmente_positivi[i]>3500 && d_reg$totale_attualmente_positivi[i]<=4000 ){
        d_reg$colore[i]<-"#040447"
        d_reg$radius[i]<-12
      }
      if(d_reg$totale_attualmente_positivi[i]>4000){
        d_reg$colore[i]<-"#000000"
        d_reg$radius[i]<-13
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
      d_prov$radius[i]<-0
    }else{
      if(d_prov$totale_casi[i]<=10){
        d_prov$colore[i]<-"#a4a4f5"
        d_prov$radius[i]<-2
      }
      if(d_prov$totale_casi[i]>10 && d_prov$totale_casi[i]<=20 ){
        d_prov$colore[i]<-"#8c8cf5"
        d_prov$radius[i]<-3
      }
      if(d_prov$totale_casi[i]>20 && d_prov$totale_casi[i]<=50){
        d_prov$colore[i]<-"#6e6ef0"
        d_prov$radius[i]<-4
      }
      if(d_prov$totale_casi[i]>50 && d_prov$totale_casi[i]<=100){
        d_prov$colore[i]<-"#5151f0"
        d_prov$radius[i]<-5
      }
      if(d_prov$totale_casi[i]>100 && d_prov$totale_casi[i]<=150){
        d_prov$colore[i]<-"#3a3af0"
        d_prov$radius[i]<-6
      }
      if(d_prov$totale_casi[i]>150 && d_prov$totale_casi[i]<=200){
        d_prov$colore[i]<-"#1a1aed"
        d_prov$radius[i]<-7
      }
      if(d_prov$totale_casi[i]>200 && d_prov$totale_casi[i]<=400){
        d_prov$colore[i]<-"#0808d1"
        d_prov$radius[i]<-8
      }
      if(d_prov$totale_casi[i]>400 && d_prov$totale_casi[i]<=550){
        d_prov$colore[i]<-"#0707ad"
        d_prov$radius[i]<-9
      }
      if(d_prov$totale_casi[i]>550 && d_prov$totale_casi[i]<=750){
        d_prov$colore[i]<-"#070785"
        d_prov$radius[i]<-10
      }
      if(d_prov$totale_casi[i]>750 && d_prov$totale_casi[i]<=900){
        d_prov$colore[i]<-"#050563"
        d_prov$radius[i]<-11
      }
      if(d_prov$totale_casi[i]>900 && d_prov$totale_casi[i]<=1400 ){
        d_prov$colore[i]<-"#040447"
        d_prov$radius[i]<-12
      }
      if(d_prov$totale_casi[i]>1400 ){
        d_prov$colore[i]<-"#000000"
        d_prov$radius[i]<-13
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
  
  
  Mortalita<-reactive({
    round(Giorno_Naz()$deceduti/Giorno_Naz()$totale_casi*100,2)
  })
  
  Guarigione<-reactive({
    round(Giorno_Naz()$dimessi_guariti/Giorno_Naz()$totale_casi*100,2)
  })
  
  output$tassoMortalita <- renderPrint({
    paste0(Mortalita(),"%")
  })
  
  output$tassoGuarigione <- renderPrint({
    paste0(Guarigione(),"%")
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
      (paste0('Totale casi ',"<br/>",formatC(Giorno_Naz()$totale_casi, format="d", big.mark=','),"<br/>")%>%
         lapply(htmltools::HTML))
      ,paste0('+',formatC(Giorno_Naz()$variazione_tc_ass, format="d", big.mark=','))
      ,icon = icon("stats",lib='glyphicon')
      ,color = "black")
    
  })
  
  output$positivi_naz <- renderValueBox({
    valueBox(
      (paste0('Positivi',"<br/>",formatC(Giorno_Naz()$totale_attualmente_positivi, format="d", big.mark=','),"<br/>")%>%
         lapply(htmltools::HTML))
      ,paste0('+',formatC(Giorno_Naz()$nuovi_attualmente_positiv, format="d", big.mark=','))
      ,icon = icon("procedures",lib='glyphicon')
      ,color = "blue")
    
  })
  
  output$dimessi_naz <- renderValueBox({
    valueBox(
      (paste0('Dimessi guariti',"<br/>",formatC(Giorno_Naz()$dimessi_guariti, format="d", big.mark=','),"<br/>")%>%
         lapply(htmltools::HTML))
      ,paste0('+',formatC(Giorno_Naz()$variazione_dg_ass, format="d", big.mark=','))
      ,icon = icon("check",lib='glyphicon')
      ,color = "navy")
    
  })
  
  
  output$decessi_naz <- renderValueBox({
    valueBox(
      (paste0('Totale decessi',"<br/>",formatC(Giorno_Naz()$deceduti, format="d", big.mark=','),"<br/>")%>%
         lapply(htmltools::HTML))
      ,paste0('+',formatC(Giorno_Naz()$variazione_d_ass, format="d", big.mark=',')),
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
                         fillColor = ~colore , fillOpacity = 1, color="white", radius=~radius, stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        )
      
    }else{
      if(input$SceltaVisuale=="Provincia"){
        mytext <- paste(
          "Provincia: ",Giorno_Prov()$denominazione_provincia,"<br/>",
          "Attualmente positivi: ", Giorno_Prov()$totale_casi, "<br/>" ) %>%
          lapply(htmltools::HTML)
        
        leaflet(Giorno_Prov()[!is.na(Giorno_Prov()[,8]),]) %>% 
          addTiles()  %>% 
          setView( lat=42, lng=10.5 , zoom=4.5) %>%
          addCircleMarkers(~long, ~lat, 
                           fillColor = ~colore , fillOpacity = 1, color="white", radius=~radius, stroke=FALSE,
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
      #tab<-as.data.frame(Giorno_Reg()[,c(4,7,8,9,10,11,12,13,14,15)])
      data.frame("Regione"=Giorno_Reg()[,4],"Ricoverati"=Giorno_Reg()[,7],"Terapia Intensiva"=Giorno_Reg()[,8],"Totale Ospedalizzati"=Giorno_Reg()[,9],
                 "Isolamento domiciliare"=Giorno_Reg()[,10],"Attualmente Positivi"=Giorno_Reg()[,11],
                 "Nuovi attualmente Positivi"=Giorno_Reg()[,12],"Dimessi guariti"=Giorno_Reg()[,13],"Deceduti"=Giorno_Reg()[,14],
                 "Totale Casi"=Giorno_Reg()[,15])
      
    }else{
      if(input$SceltaVisuale=="Provincia"){
        data.frame("Regione"=Giorno_Prov()[!is.na(Giorno_Prov()[,8]),4],"Provincia"=Giorno_Prov()[!is.na(Giorno_Prov()[,8]),6],"Totale Casi"=Giorno_Prov()[!is.na(Giorno_Prov()[,8]),10])
        #as.data.frame(Giorno_Prov()[,c(4,6,10)])
      }
    }
  })
  
  
  sceltaOutput<-reactive({
    if(input$checkbox=="TRUE"){
      sceltatabella()
    }
  })
  
  output$tabella<- DT::renderDataTable({
    
    DT::datatable(sceltaOutput(), rownames = FALSE, options = list(
      columnDefs = list(list(className = 'dt-center')),
      pageLength =10,
      lengthMenu = c(5, 10, 15, 20)
    ))
    
    
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
  
  
  
  
  
  ########################################################################################################################
  
                                              #WORLD
  
  ########################################################################################################################
  world_spdf <- readOGR( 
    dsn= paste0(getwd()) , 
    layer="TM_WORLD_BORDERS_SIMPL-0.3",
    verbose=FALSE
  )
  

  
  
  Confermati<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",header=T,sep=",")
  Confermati<-gather(Confermati,"Date","Confermati",X1.22.20:names(Confermati)[ncol(Confermati)],na.rm=TRUE)
  a<-seq(as.Date("2020/1/22"), by = "day", length.out = length(unique(Confermati$Date)))
  data_presente<-unique(Confermati$Date)
  Confermati$Data<-as.character(seq(1,nrow(Confermati)))
  for(i in 1:length(data_presente)){
    Confermati$Data[which(Confermati$Date==data_presente[i])]<-as.character(a[i])
  }
  Confermati$Province.State<-as.character(Confermati$Province.State)
  Confermati$Country.Region<-as.character(Confermati$Country.Region)
  Confermati$Country.Region[Confermati$Province.State=="St Martin"]<-"Saint Martin"
  Confermati$Country.Region[Confermati$Province.State=="Channel Islands"]<-"Jersey"
  Confermati$Country.Region[Confermati$Province.State=="Saint Barthelemy"]<-"Saint Barthelemy"
  Confermati$Country.Region[Confermati$Province.State=="Macau"]<-"Macau"
  Confermati$Country.Region[Confermati$Province.State=="Faroe Islands"]<-"Faroe Islands"
  Confermati$Country.Region[Confermati$Province.State=="Gibraltar"]<-"Gibraltar"
  Confermati$Country.Region[Confermati$Province.State=="Hong Kong"]<-"Hong Kong"
  Confermati$Country.Region[Confermati$Country.Region=="Taiwan*"]<-"Taiwan"
  Confermati$Country.Region[Confermati$Country.Region=="Moldova"]<-"Republic of Moldova"
  Confermati$Country.Region[Confermati$Country.Region=="Holy See"]<-"Vatican"
  Confermati$Country.Region[Confermati$Country.Region=="Czechia"]<-"Czech Republic"
  Confermati$Country.Region[Confermati$Country.Region=="Congo (Kinshasa)"]<-"Democratic Republic of the Congo"
  Confermati$Country.Region[Confermati$Country.Region=="US"]<-"United States"
  
  
  
  
  Confermati<-Confermati[,c(7,2,6)]
  Confermati$Country.Region<-as.character(Confermati$Country.Region)
  Confermati<-aggregate(Confermati$Confermati,by=list(Confermati$Data,Confermati$Country.Region),FUN=sum)
  names(Confermati)<-c("date","Nazione","Confermati")
  
  
  
  
  Guariti<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv",header=T,sep=",")
  Guariti<-gather(Guariti,"Date","Guariti",X1.22.20:names(Guariti)[ncol(Guariti)],na.rm=TRUE)
  a<-seq(as.Date("2020/1/22"), by = "day", length.out = length(unique(Guariti$Date)))
  data_presente<-unique(Guariti$Date)
  Guariti$Data<-as.character(seq(1,nrow(Guariti)))
  for(i in 1:length(data_presente)){
    Guariti$Data[which(Guariti$Date==data_presente[i])]<-as.character(a[i])
  }
  Guariti$Province.State<-as.character(Guariti$Province.State)
  Guariti$Country.Region<-as.character(Guariti$Country.Region)
  Guariti$Country.Region[Guariti$Province.State=="St Martin"]<-"Saint Martin"
  Guariti$Country.Region[Guariti$Province.State=="Channel Islands"]<-"Jersey"
  Guariti$Country.Region[Guariti$Province.State=="Saint Barthelemy"]<-"Saint Barthelemy"
  Guariti$Country.Region[Guariti$Province.State=="Macau"]<-"Macau"
  Guariti$Country.Region[Guariti$Province.State=="Faroe Islands"]<-"Faroe Islands"
  Guariti$Country.Region[Guariti$Province.State=="Gibraltar"]<-"Gibraltar"
  Guariti$Country.Region[Guariti$Province.State=="Hong Kong"]<-"Hong Kong"
  Guariti$Country.Region[Guariti$Country.Region=="Taiwan*"]<-"Taiwan"
  Guariti$Country.Region[Guariti$Country.Region=="Moldova"]<-"Republic of Moldova"
  Guariti$Country.Region[Guariti$Country.Region=="Holy See"]<-"Vatican"
  Guariti$Country.Region[Guariti$Country.Region=="Czechia"]<-"Czech Republic"
  Guariti$Country.Region[Guariti$Country.Region=="Congo (Kinshasa)"]<-"Democratic Republic of the Congo"
  Guariti$Country.Region[Guariti$Country.Region=="US"]<-"United States"
  
  Guariti<-Guariti[,c(7,2,6)]
  Guariti$Country.Region<-as.character(Guariti$Country.Region)
  Guariti<-aggregate(Guariti$Guariti,by=list(Guariti$Data,Guariti$Country.Region),FUN=sum)
  names(Guariti)<-c("date","Nazione","Guariti")
  
  
  Decessi<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv",header=T,sep=",")
  Decessi<-gather(Decessi,"Date","Decessi",X1.22.20:names(Decessi)[ncol(Decessi)],na.rm=TRUE)
  a<-seq(as.Date("2020/1/22"), by = "day", length.out = length(unique(Decessi$Date)))
  data_presente<-unique(Decessi$Date)
  Decessi$Data<-as.character(seq(1,nrow(Decessi)))
  for(i in 1:length(data_presente)){
    Decessi$Data[which(Decessi$Date==data_presente[i])]<-as.character(a[i])
  }
  Decessi$Province.State<-as.character(Decessi$Province.State)
  Decessi$Country.Region<-as.character(Decessi$Country.Region)
  Decessi$Country.Region[Decessi$Province.State=="St Martin"]<-"Saint Martin"
  Decessi$Country.Region[Decessi$Province.State=="Channel Islands"]<-"Jersey"
  Decessi$Country.Region[Decessi$Province.State=="Saint Barthelemy"]<-"Saint Barthelemy"
  Decessi$Country.Region[Decessi$Province.State=="Macau"]<-"Macau"
  Decessi$Country.Region[Decessi$Province.State=="Faroe Islands"]<-"Faroe Islands"
  Decessi$Country.Region[Decessi$Province.State=="Gibraltar"]<-"Gibraltar"
  Decessi$Country.Region[Decessi$Province.State=="Hong Kong"]<-"Hong Kong"
  Decessi$Country.Region[Decessi$Country.Region=="Taiwan*"]<-"Taiwan"
  Decessi$Country.Region[Decessi$Country.Region=="Moldova"]<-"Republic of Moldova"
  Decessi$Country.Region[Decessi$Country.Region=="Holy See"]<-"Vatican"
  Decessi$Country.Region[Decessi$Country.Region=="Czechia"]<-"Czech Republic"
  Decessi$Country.Region[Decessi$Country.Region=="Congo (Kinshasa)"]<-"Democratic Republic of the Congo"
  Decessi$Country.Region[Decessi$Country.Region=="US"]<-"United States"
  
  Decessi<-Decessi[,c(7,2,6)]
  Decessi$Country.Region<-as.character(Decessi$Country.Region)
  Decessi<-aggregate(Decessi$Decessi,by=list(Decessi$Data,Decessi$Country.Region),FUN=sum)
  names(Decessi)<-c("date","Nazione","Decessi")
  
  Totale<-cbind(Confermati,"Guariti"=Guariti$Guariti,"Decessi"=Decessi$Decessi)
  Totale$Positivi<-Totale$Confermati-Totale$Guariti-Totale$Decessi
  
  
  for(i in 1:nrow(Totale)){
    if(is.na(Totale$Confermati[i])){
      Totale$Confermati[i]<-"transparent"
    }else{
      if(Totale$Confermati[i]<=10){
        Totale$colore[i]<-"#a4a4f5"
      }
      if(Totale$Confermati[i]>10 && Totale$Confermati[i]<=20 ){
        Totale$colore[i]<-"#8c8cf5"
      }
      if(Totale$Confermati[i]>20 && Totale$Confermati[i]<=50){
        Totale$colore[i]<-"#6e6ef0"
      }
      if(Totale$Confermati[i]>50 && Totale$Confermati[i]<=100){
        Totale$colore[i]<-"#5151f0"
      }
      if(Totale$Confermati[i]>100 && Totale$Confermati[i]<=250){
        Totale$colore[i]<-"#3a3af0"
      }
      if(Totale$Confermati[i]>250 && Totale$Confermati[i]<=500){
        Totale$colore[i]<-"#1a1aed"
      }
      if(Totale$Confermati[i]>500 && Totale$Confermati[i]<=750){
        Totale$colore[i]<-"#0808d1"
      }
      if(Totale$Confermati[i]>750 && Totale$Confermati[i]<=1250){
        Totale$colore[i]<-"#0707ad"
      }
      if(Totale$Confermati[i]>1250 && Totale$Confermati[i]<=2500){
        Totale$colore[i]<-"#070785"
      }
      if(Totale$Confermati[i]>2500 && Totale$Confermati[i]<=3500){
        Totale$colore[i]<-"#050563"
      }
      if(Totale$Confermati[i]>3500 && Totale$Confermati[i]<=6000 ){
        Totale$colore[i]<-"#040447"
      }
      if(Totale$Confermati[i]>6000 && Totale$Confermati[i]<=10000 ){
        Totale$colore[i]<-"#03032b"
      }
      if(Totale$Confermati[i]>10000 && Totale$Confermati[i]<=20000 ){
        Totale$colore[i]<-"#000024"
      }
      if(Totale$Confermati[i]>20000 ){
        Totale$colore[i]<-"#000000"
      }
    }

  }
  
  
  world_spdf@data[["NAME"]]<-as.character(world_spdf@data[["NAME"]])
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="Brunei Darussalam"]<-"Brunei"
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="Iran (Islamic Republic of)"]<-"Iran"
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="Korea, Republic of"]<-"Korea, South"
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="The former Yugoslav Republic of Macedonia"]<-"North Macedonia"
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="Holy See (Vatican City)"]<-"Vatican"
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="Viet Nam"]<-"Vietnam"
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="Korea, Democratic People's Republic of"]<-"Korea, North"
  
  
  
  Giorno_World<-reactive({
    if(nrow(as.data.frame(Totale[Totale$date %like% input$dateworld,]))!=0){
      mondo_fil<-as.data.frame(Totale[Totale$date %like% input$dateworld,])
    }else{
      if(nrow(as.data.frame(Totale[Totale$date %like% input$dateworld,]))==0){
        mondo_fil<-as.data.frame(Totale[Totale$date==max(Totale$date),])
      }
    }
  })
  
  
  output$giornoAnalisi_world<-renderTable({
    data.frame("Data"=paste0("Giorno visualizzato: ",as.Date(unique(Giorno_World()$date))))
  })
  
  

  
  Mappa_world<-reactive({ for(i in 1:nrow(Giorno_World())){
    testo<-paste0("Nazione: ",Giorno_World()$Nazione[i],"<br/>",
                  "Confermati: ",max(na.omit(Giorno_World()$Confermati[i]),0),"<br/>",
                  "Positivi: ",max(na.omit(Giorno_World()$Positivi[i]),0),"<br/>",
                  "Guariti: ",max(na.omit(Giorno_World()$Guariti[i]),0),"<br/>",
                  "Decessi: ",max(na.omit(Giorno_World()$Decessi[i]),0),"<br/>") %>%
      lapply(htmltools::HTML)
    if(Giorno_World()$Nazione[i] %in% world_spdf@data[["NAME"]]){
      world_spdf@data[["Confermati"]][world_spdf@data[["NAME"]]==Giorno_World()$Nazione[i]]<-max(na.omit(Giorno_World()$Confermati[i]),0)
      world_spdf@data[["Positivi"]][world_spdf@data[["NAME"]]==Giorno_World()$Nazione[i]]<-max(na.omit(Giorno_World()$Positivi[i]),0)
      world_spdf@data[["Guariti"]][world_spdf@data[["NAME"]]==Giorno_World()$Nazione[i]]<-max(na.omit(Giorno_World()$Guariti[i]),0)
      world_spdf@data[["Decessi"]][world_spdf@data[["NAME"]]==Giorno_World()$Nazione[i]]<-max(na.omit(Giorno_World()$Decessi[i]),0)
      world_spdf@data[["Testo"]][world_spdf@data[["NAME"]]==Giorno_World()$Nazione[i]]<-testo
      world_spdf@data[["colore"]][world_spdf@data[["NAME"]]==Giorno_World()$Nazione[i]]<-Giorno_World()$colore[i]
    }else{
      if(!Giorno_World()$Nazione[i] %in% world_spdf@data[["NAME"]]){
        testo<-paste0("Nazione: ",world_spdf@data[["NAME"]],"<br/>",
                      "Confermati: 0","<br/>"
        )%>%
          lapply(htmltools::HTML)
        world_spdf@data[["Testo"]][world_spdf@data[["NAME"]]==Giorno_World()$Nazione[i]]<-testo
      }
    }
  }
    return(world_spdf)
    
    
  })
  
  
  
  
  grafico_mappa_world<-reactive({
    testo<-paste0(Mappa_world()@data$Testo)%>%lapply(htmltools::HTML)
    leaflet(Mappa_world()) %>% 
      addTiles()  %>% 
      setView( lat=10, lng=0 , zoom=2) %>%
      addPolygons( fillColor = Mappa_world()@data$colore , fillOpacity = 1, color="white", stroke=FALSE ,label = testo,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      )
    
    
  })
  
  output$mappa_world<-renderLeaflet({
    grafico_mappa_world()
  })
  
  

  output$tabella_world<- DT::renderDataTable({
    DT::datatable(Giorno_World()[,c(2,3,6,4,5)], rownames = FALSE, options = list(
      columnDefs = list(list(className = 'dt-center')),
      pageLength =10,
      lengthMenu = c(5, 10, 15, 20)
    ))
    
    
  })
  
  
}