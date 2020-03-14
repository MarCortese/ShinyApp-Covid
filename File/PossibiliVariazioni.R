



#######################################################################################################################

# FILE MAPPE

######################################################################################################################

#regioni

mappa_regioni<-"https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_IT_regions.geojson" %>%
  GET() %>%
  content() %>%
  jsonlite::fromJSON(simplifyVector = FALSE)

#province
mappa_province <- "https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_IT_provinces.geojson" %>%
  GET() %>%
  content() %>%
  jsonlite::fromJSON(simplifyVector = FALSE)

#######################################################################################################################







# MAPPA CON HIGHCHART

output$mappa_regione_HIGH <- renderHighchart({

  highchart(type = "map") %>%
    hc_title(text = "Diffusione coronavirus regioni") %>%
    hc_subtitle(text = "Dati: Protezione civile") %>%
    hc_add_series_map(map = mappa_regioni, df = Giorno_Reg(), joinBy = "reg_istat_code_num",
                      value = "totale_attualmente_positivi", name="Casi Coranavirus")%>%
    hc_colorAxis(stops = color_stops())%>%
    hc_mapNavigation(enabled = TRUE) %>%
    hc_tooltip(useHTML = TRUE, headerFormat = "",
               pointFormat = '{point.properties.reg_name} casi totali:{point.totale_casi}
                          dimessi guariti:{point.dimessi_guariti}
                          terapia intesiva:{point.terapia_intensiva}
                          ricoverati con sintomi:{point.ricoverati_con_sintomi}
                          totale ospedalizzati:{point.totale_ospedalizzati}
                          isolamento domiciliare:{point.isolamento_domiciliare}
                          totale attualmente positivi:{point.totale_attualmente_positivi}
                          nuovi attualemnte positivi:{point.nuovi_attualmente_positivi}
                          deceduti:{point.deceduti}',borderColor = "navy")



})


output$mappa_provincia_HIGH <- renderHighchart({
  highchart(type = "map") %>%
    hc_title(text = "Diffusione coronavirus province") %>%
    hc_subtitle(text = "Dati: Protezione civile") %>%
    hc_add_series_map(map = mappa_province, df = Giorno_Prov(), joinBy = "prov_istat_code_num", value = "totale_casi", name="Casi Coranavirus") %>%
    hc_colorAxis(stops = color_stops())%>%
    hc_mapNavigation(enabled = TRUE) %>%
    hc_tooltip(useHTML = TRUE, headerFormat = "",
               pointFormat = '{point.properties.prov_name} casi totali:{point.totale_casi}',borderColor = "navy")
})


#MAPPA CON PLOTLY
output$mappa_reg_PLOTLY<-renderPlotly({
  Giorno_Reg() %>%
    plot_ly(
      lat = ~lat,
      lon = ~long,
      marker = list(
        color = 'rgb(220,20,60)',
        size = 20
      ),
      type = 'scattermapbox',
      mode="markers",
      hovertext = paste0(Giorno_Reg()$reg_name,": ",Giorno_Reg()$totale_attualmente_positivi," positivi","\n",
                         Giorno_Reg()$dimessi_guariti," dimessi","\n",
                         Giorno_Reg()$deceduti," deceduti","\n",
                         Giorno_Reg()$ricoverati_con_sintomi," ricoverati","\n",
                         Giorno_Reg()$terapia_intensiva," terpaia intensiva","\n",
                         Giorno_Reg()$isolamento_domiciliare," isolamento","\n")) %>%
    layout(
      mapbox = list(
        style = 'open-street-map',
        zoom =4,
        center = list(lon = 10.5, lat = 43))) %>%
    layout(plot_bgcolor='rgb(236, 240, 245)') %>%
    layout(paper_bgcolor='rgb(236, 240, 245)')
})


output$mappa_prov_PLOTLY<-renderPlotly({
  Giorno_Prov() %>%
    plot_ly(
      lat = ~lat,
      lon = ~long,
      marker = list(
        color = 'rgb(220,20,60)',
        size = 10
      ),
      type = 'scattermapbox',
      mode="markers",
      hovertext = paste0(Giorno_Prov()$denominazione_provincia,": ",Giorno_Prov()$totale_casi," casi")) %>%
    layout(
      mapbox = list(
        style = 'open-street-map',
        zoom =4,
        center = list(lon = 10.5, lat = 43))) %>%
    layout(plot_bgcolor='rgb(236, 240, 245)') %>%
    layout(paper_bgcolor='rgb(236, 240, 245)')
})




  
  ########################################################################################################################
  
  #WORLD
  
  ########################################################################################################################
  world_spdf <- readOGR( 
    dsn= paste0(getwd()) , 
    layer="TM_WORLD_BORDERS_SIMPL-0.3",
    verbose=FALSE
  )
  
  mondo<-read.csv("https://cowid.netlify.com/data/full_data.csv",header=T,sep=",")
  mondo<-mondo%>%arrange(mondo$date)
  mondo$date<-as.Date(mondo$date)
  
  
  
  
  for(i in 1:nrow(mondo)){
    if(is.na(mondo$total_cases[i])){
      mondo$total_cases[i]<-"transparent"
    }else{
      if(mondo$total_cases[i]<=10){
        mondo$colore[i]<-"#a4a4f5"
      }
      if(mondo$total_cases[i]>10 && mondo$total_cases[i]<=20 ){
        mondo$colore[i]<-"#8c8cf5"
      }
      if(mondo$total_cases[i]>20 && mondo$total_cases[i]<=50){
        mondo$colore[i]<-"#6e6ef0"
      }
      if(mondo$total_cases[i]>50 && mondo$total_cases[i]<=100){
        mondo$colore[i]<-"#5151f0"
      }
      if(mondo$total_cases[i]>100 && mondo$total_cases[i]<=250){
        mondo$colore[i]<-"#3a3af0"
      }
      if(mondo$total_cases[i]>250 && mondo$total_cases[i]<=500){
        mondo$colore[i]<-"#1a1aed"
      }
      if(mondo$total_cases[i]>500 && mondo$total_cases[i]<=750){
        mondo$colore[i]<-"#0808d1"
      }
      if(mondo$total_cases[i]>750 && mondo$total_cases[i]<=1250){
        mondo$colore[i]<-"#0707ad"
      }
      if(mondo$total_cases[i]>1250 && mondo$total_cases[i]<=2500){
        mondo$colore[i]<-"#070785"
      }
      if(mondo$total_cases[i]>2500 && mondo$total_cases[i]<=3500){
        mondo$colore[i]<-"#050563"
      }
      if(mondo$total_cases[i]>3500 && mondo$total_cases[i]<=4000 ){
        mondo$colore[i]<-"#040447"
      }
      if(mondo$total_cases[i]>4000){
        mondo$colore[i]<-"#000000"
      }
    }


  }
  


  
  
  world_spdf@data[["NAME"]]<-as.character(world_spdf@data[["NAME"]])
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="Democratic Republic of the Congo"]<-"Democratic Republic of Congo"
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="Faroe Islands"]<-"Faeroe Islands"
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="Brunei Darussalam"]<-"Brunei"
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="Iran (Islamic Republic of)"]<-"Iran"
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="Korea, Republic of"]<-"South Korea"
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="The former Yugoslav Republic of Macedonia"]<-"Macedonia"
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="Saint Barthelemy"]<-"Saint Barthlemy"
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="Saint Martin"]<-"Saint Martin (French part)"
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="Holy See (Vatican City)"]<-"Vatican"
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="Viet Nam"]<-"Vietnam"
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="Korea, Democratic People's Republic of"]<-"North Korea"
  world_spdf@data[["NAME"]][world_spdf@data[["NAME"]]=="Republic of Moldova"]<-"Moldova"



  Giorno_World<-reactive({
    if(nrow(as.data.frame(mondo[mondo$date %like% input$dateworld,]))!=0){
      mondo_fil<-as.data.frame(mondo[mondo$date %like% input$dateworld,])
    }else{
      if(nrow(as.data.frame(mondo[mondo$date %like% input$dateworld,]))==0){
        mondo_fil<-as.data.frame(mondo[mondo$date==max(mondo$date),])
      }
    }
  })
  

  

  
  
  
  Mappa_world<-reactive({ for(i in 1:nrow(Giorno_World())){
    testo<-paste0("Nazione: ",Giorno_World()$location[i],"<br/>",
                  "Casi totali: ",max(na.omit(Giorno_World()$total_cases[i]),0),"<br/>",
                  "Nuovi casi: ",max(na.omit(Giorno_World()$new_cases[i]),0),"<br/>",
                  "Nuovi decessi: ",max(na.omit(Giorno_World()$new_deaths[i]),0),"<br/>",
                  "Decessi totali: ",max(na.omit(Giorno_World()$total_deaths[i]),0),"<br/>") %>%
      lapply(htmltools::HTML)
     if(Giorno_World()$location[i] %in% world_spdf@data[["NAME"]]){
       world_spdf@data[["TotaleCasi"]][world_spdf@data[["NAME"]]==Giorno_World()$location[i]]<-max(na.omit(Giorno_World()$total_cases[i]),0)
       world_spdf@data[["NuoviCasi"]][world_spdf@data[["NAME"]]==Giorno_World()$location[i]]<-max(na.omit(Giorno_World()$new_cases[i]),0)
       world_spdf@data[["NuoviDecessi"]][world_spdf@data[["NAME"]]==Giorno_World()$location[i]]<-max(na.omit(Giorno_World()$new_deaths[i]),0)
       world_spdf@data[["TotaleDecessi"]][world_spdf@data[["NAME"]]==Giorno_World()$location[i]]<-max(na.omit(Giorno_World()$total_deaths[i]),0)
       world_spdf@data[["Testo"]][world_spdf@data[["NAME"]]==Giorno_World()$location[i]]<-testo
       world_spdf@data[["colore"]][world_spdf@data[["NAME"]]==Giorno_World()$location[i]]<-Giorno_World()$colore[i]
  }else{
    if(!Giorno_World()$location[i] %in% world_spdf@data[["NAME"]]){
    testo<-paste0("Nazione: ",world_spdf@data[["NAME"]],"<br/>",
                  "Casi totali: 0","<br/>"
                  )%>%
      lapply(htmltools::HTML)
    world_spdf@data[["Testo"]][world_spdf@data[["NAME"]]==Giorno_World()$location[i]]<-testo
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
  
  
  dataframe<-reactive({
    data.frame("Nazione"=Giorno_World()$location[i],"CasiTotali"=max(na.omit(Giorno_World()$total_cases[i]),0),
               "NuoviCasi"=max(na.omit(Giorno_World()$new_cases[i]),0),
               "NuoviDecessi"=max(na.omit(Giorno_World()$new_deaths[i]),0),
               "DecessiTotali"=max(na.omit(Giorno_World()$total_deaths[i]),0))
  })
  

  
  
  output$tabella_world<- DT::renderDataTable({
    DT::datatable(Giorno_World()[,c(1,2,5,3,6,4)], rownames = FALSE, options = list(
      columnDefs = list(list(className = 'dt-center')),
      pageLength =10,
      lengthMenu = c(5, 10, 15, 20)
    ))


  })
