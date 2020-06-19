# load the required packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(highcharter)
library(rjson)
library(httr)
library(plotly)
library(quantmod)
library(tidyr)
library(leaflet)
library(xts)
library(data.table)
library(rgdal)
library(shinyalert)
library(shinybusy)
library(shinyjs)

#setwd("C:/Users/m.cortese/Desktop/Progetti/Corona")
#############################################################################################################
#file fissi

regioni<-read.csv("CoordinateRegioni.csv",sep=";",header=T,stringsAsFactors = F)
province<-read.csv("Coordinate.csv",sep=";",header=T,stringsAsFactors = F)
#demos<-read.csv("Demografia.csv",header=T,sep=";",dec=",",stringsAsFactors = F)




#############################################################################################################

                                            #UI

#############################################################################################################


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(
  title ="Analisi Covid-19 Italia" ,
  tags$li(actionLink("openModal", label = " Attendi il caricamento dati", icon = icon("exclamation-triangle")),
          class = "dropdown")

  
)


#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    style = "position: fixed; overflow: visible;",
    menuItem("Generiche", tabName = "analisi", icon = icon("dashboard")),
    menuItem("Analisi Nazionale", tabName = "trend", icon = icon("chart-line")),
    menuItem("Analisi Regionale", tabName = "reg", icon = icon("chart-line")),
    menuItem("Nel mondo",tabName = "mondo", icon = icon("map")),
    menuItem("Trend Nazioni",tabName = "nazioni", icon = icon("map")),
    menuItem("Informazioni",tabName = "info", icon = icon("question-circle")),
    
    HTML(paste0(
      "<br><br><br><br><br><br><br><br><br>",
      "<table align='center'; style='margin-left:auto; margin-right:auto;'>",
      "<tr>",
      "<td style='padding: 5px;'><a href='https://www.facebook.com/marcobrad.patagarrocortese' target='_blank'><i class='fab fa-facebook-square fa-lg'></i></a></td>",
      "<td style='padding: 5px;'><a href='https://www.twitter.com/cortese_mar' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
      "<td style='padding: 5px;'><a href='https://www.linkedin.com/in/marco-cortese-kr23-mi08' target='_blank'><i class='fab fa-linkedin fa-lg'></i></a></td>",
      "<td style='padding: 5px;'><a href='https://github.com/MarCortese' target='_blank'><i class='fab fa-github fa-lg'></i></a></td>",
      "</tr>",
      "</table>",
      "<br>"),
      HTML(paste0(
        "<p style = 'text-align: center;'><medium> - Marco Cortese - <script>document.write(yyyy);</script></medium></p>",
        "<br><br><br><br><br><br><br>",
        #img(src="https://lh3.googleusercontent.com/proxy/mOgE8r2tx12tABtu5ikMHzU2s4lxWIIKk0EU0HgAUA6ZoN92T1kBULy9BLYar-0-nsoX9xvfDYBYFopRBjcBHxK-V1rDjMnJoZtmCP6EvrgRJCAZ7GOT",height = "100px",style="display: block; margin-left: auto; margin-right: auto;"),
        "<br>"
      )
      )),
    useShinyalert(),  # Set up shinyalert
    actionButton("preview", "", 
                 style="color: #36393d; background-color: #36393d; border-color: #36393d")
      )
)


# fluidRow


frow1 <- fluidRow(

      valueBoxOutput("totale_casi_naz",3),
      valueBoxOutput("positivi_naz",3),
      valueBoxOutput("dimessi_naz",3),
      valueBoxOutput("decessi_naz",3)  

)

frow2<- fluidRow(
  column(4,
         radioButtons("SceltaVisuale", label = h3(""),
                      choices = list("Regione" = "Regione", "Provincia" = "Provincia"), 
                      selected = "Regione", inline=T),
         hr(),
         dateInput('date2',
                   label = '',
                   value = as.character(Sys.Date()),
                   min = "2020-02-24", max = Sys.Date(),
                   startview = 'month', language = 'it', weekstart = 1
         )),
  column(4,
         tags$style(type='text/css', '#tassoMortalita {background-color: rgb(236, 240, 245); borders-color:rgb(236, 240, 245); color: navy;}'),
         tags$style(type='text/css', '#tassoGuarigione {background-color: rgb(236, 240, 245); borders-color:rgb(236, 240, 245); color: navy;}'),
         h5("Tasso di mortalità",align="center"),
         h5(verbatimTextOutput("tassoMortalita"),align="center"),
         h5("Tasso di guarigione",align="center"),
         h5(verbatimTextOutput("tassoGuarigione"),align="center")
         #verbatimTextOutput("mostradata")
         ),
  column(4,
         hr(),
         hr(),
         hr(),
         tableOutput("giornoAnalisi")
  )
  

)



frow3 <- fluidRow(
  leafletOutput("mappa",height =400),
  hr(),
  hr()
)

frow4<- fluidRow(
  column(4,  
         plotlyOutput("ospedalizzati")
  ),
  column(4,
         plotlyOutput("positivi")
  ),
  column(4,
         plotlyOutput("casi")
  )
)
frow5<-fluidRow(
  h4(checkboxInput("checkbox", label = "Tabella", value = FALSE)),
  div(style = 'overflow-x: scroll', DT::dataTableOutput('tabella'))
  
)

frow6 <- fluidRow(
  plotlyOutput("serie",height =500),
  hr(),
  plotlyOutput("serieTassi",height =500))

frow7 <- fluidRow(
  radioButtons("variabile", label = h3("Scegli variabile"),
               choices = list("Positivi" = "pos", "Guariti" = "gu", "Decessi" = "dec","Contagi"="con"), 
               selected = "pos"),
  plotlyOutput("VariabiliPlot",height =500)
  #plotlyOutput("serieVariazioni",height =500)
)
frow8<- fluidRow(

  selectizeInput(
    'regione', 'Seleziona la regione', choices = regioni$Regioni,
    options = list(
      placeholder = '',
      onInitialize = I('function() { this.setValue(""); }')
    )
  )

)
frow9aa<-fluidRow(
  tableOutput('dt_reg'),
  #hr(),
  tableOutput('dt_reg_tassi')
  #tableOutput('dt_reg_perc')
)
frow9a<-fluidRow(
  hr(),
  hr(),
  column(4),
  column(4,
         plotlyOutput("TortaRegione")),
  column(4)
)



frow9<- fluidRow(
  plotlyOutput("serieReg",height =500),
  tableOutput('dt_prov'),
  plotlyOutput("serieProv",height =500),
  h5(verbatimTextOutput("messaggio"),align="center")
  
)


frow10<- fluidRow(
  tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/1PWfsNs0bDw", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA,style="display: block; margin-left: auto; margin-right: auto;"),
  hr(),
  hr()
)

frow11 <- fluidRow(
  h2("I dati analizzati provengono direttamente dalla Protezione Civile Italiana.",align="center"),
  tags$img(src="https://pngimage.net/wp-content/uploads/2018/06/logo-protezione-civile-png-2.png",width="200",style="display: block; margin-left: auto; margin-right: auto;")
)

frow12<- fluidRow(
  h2("In questo momento di grande scompiglio vi invitiamo a prestare la massima attenzione alle misure si sicurezza e prevenzione. Diffidate da alcune informazioni che circolano sui social e nel web. ",align="justify"),
  hr(),
  h3("Ogni sera alle 18:00 sarà possibile seguire in diretta le parole del Primo Ministro Giuseppe Conte o della Protezione civile presso i canali youtube.",align="justify"),
  HTML(paste0(
    "<br>",
    "<table style='margin-left:auto; margin-right:auto;'>",
    "<tr>",
    "<td style='padding: 5px;'><a href='https://www.youtube.com/user/governoit/videos' target='_blank'><i class='fab fa-youtube-square fa-lg'></i></a></td>",
    "<td style='padding: 5px;'><a href='https://www.youtube.com/channel/UC4fru33Tzpu0UhCIHChiNFA/videos' target='_blank'><i class='fab fa-youtube-square fa-lg'></i></a></td>",
    "</tr>",
    "</table>",
    "<br>")),
  h2("Voci più autorevoli:", align="center"),
  hr(),
  column(4,
         h3(helpText(   a("Protezione Civile",     href="http://www.protezionecivile.gov.it/home"))),
         h3(helpText(   a("Il Governo",     href="http://www.governo.it/it/il-governo"))),
         h3(helpText(   a("Ministero della salute",     href="http://www.salute.gov.it/portale/rapportiInternazionali/menuContenutoRapportiInternazionali.jsp?lingua=italiano&area=rapporti&menu=mondiale"))),
         h3(helpText(   a("Organizzazione Mondiale della Sanità",     href="http://www.euro.who.int/en/home")))
         ),
  column(4,
         h3(helpText(   a("Rai News",     href="https://www.rainews.it/"))),
         h3(helpText(   a("Sky tg 24",     href="https://tg24.sky.it/"))),
         h3(helpText(   a("Ospedale Sacco Milano",     href="https://www.asst-fbf-sacco.it/news"))),
         h3(helpText(   a("Roberto Burioni",     href="https://www.facebook.com/robertoburioniMD/")))
         ),
  column(4,
         h3(helpText(   a("Roberta Villa",     href="https://www.instagram.com/robivil/?hl=it"))),
         h3(helpText(   a("Gianluca Pistore",     href="https://www.instagram.com/gianlucapistore/?hl=it")))
         ),
  hr(),
  hr(),
)

frow13<-fluidRow(
  h2("Ordinanze emanate:",align="center"),
  h3(("Per seguire le ordinaze emanate dal governo i visita il seguente link:")),
  h3(helpText(   a("Ordinanze",     href="http://www.governo.it/it/approfondimento/coronavirus-la-normativa/14252"))),
  h3(("Per seguire le ordinaze emanate dalle regioni visita il seguente link:")),
  h3(helpText(   a("Ordinanze",     href="http://www.regioni.it/newsletter/n-3785/del-25-02-2020/coronavirus-ordinanze-ultime-circolari-regionali-e-note-esplicative-20851/"))),
)

frow14<-fluidPage(
  
  h3("Per le analisi mondiali i dati sono stati reperiti al seguente link:" ,align="center"),
  h3(helpText(   a("Dati",     href="https://github.com/CSSEGISandData/COVID-19")),align="center"),
  hr(),
  h3("Per ciò che concerne l'Italia potremmo quindi trovare delle differenze con i dati presenti nella prima pagina.",align="center"),
  hr(),
  hr(),
  column(4,
         dateInput('dateworld',
                   label = '',
                   value = as.character(Sys.Date()),
                   min = "2020-01-21", max = Sys.Date(),
                   startview = 'month', language = 'it', weekstart = 1)
  ),
  column(4),
  column(4,
         tableOutput("giornoAnalisi_world")
         ),
         leafletOutput("mappa_world",height =700),
  
  div(style = 'overflow-x: scroll', DT::dataTableOutput('tabella_world'))
  
  
)

frow15<-fluidRow(
  selectizeInput(
  'nazione', 'Seleziona la nazione', choices = NULL,
  options = list(
    placeholder = '',
    onInitialize = I('function() { this.setValue(""); }')
    )
  ),
  hr(),
  plotlyOutput("serieNazWorld",height =500)
  
)



 body <- dashboardBody(  
   
   # tags$head(tags$style(HTML( '
   #                              /* logo */
   #                              .skin-blue .main-header .logo {
   #                              background-color: #000E28;
   #                              }
   # 
   #                              /* logo when hovered */
   #                              .skin-blue .main-header .logo:hover {
   #                              background-color: #000E28;
   #                              }
   # 
   #                              /* navbar (rest of the header) */
   #                              .skin-blue .main-header .navbar {
   #                              background-color: #000E28;
   #                              }
   # 
   #                              /* main sidebar */
   #                              .skin-blue .main-sidebar {
   #                              background-color: #000E28;
   #                              }
   # 
   #                              /* active selected tab in the sidebarmenu */
   #                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
   #                              background-color: #0887B2;
   #                              }
   #                              
   #                              /* other links in the sidebarmenu when hovered */
   #                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
   #                              background-color: #0887B2;
   #                              }'))),
   # 
   

  tabItems(tabItem(tabName = "analisi",frow1, frow2,  frow3,frow4,frow5),
                               tabItem(tabName = "trend",frow6,frow7),
                               tabItem(tabName = "reg",frow8,frow9aa,frow9a,frow9),
                               tabItem(tabName = "info",frow10,frow11,frow12,frow13),
                               tabItem(tabName = "mondo",frow14),
                               tabItem(tabName ="nazioni",frow15)))

ui <- dashboardPage(title = 'Analisi Covid-19 Italia', header, sidebar, body, skin='black')






##############################################################################################################

#SERVER

###############################################################################################################

server <- function(input, output,session) { 
  
  observeEvent(input$openModal, {
    showModal(
      modalDialog(title = "Esito",
                  strong("Dati Caricati"),
                  img(src="https://1.bp.blogspot.com/-rQIx6VXc4v4/WymIJew8KQI/AAAAAAAgZbo/1_UPg6fJLck781dhlOJyPQHzK3_4zbg2QCLcBGAs/s1600/AW1238190_00.gif",height = "40px",style="display: block; margin-left: auto; margin-right: auto;")
      )
    )
  })
  
    observeEvent(!input$preview, {
      shinyalert("", "Dati caricati con successo", type = "success")
    })
    


  
  output$dateText2 <- renderText({
    paste("input$date2 is", as.character(input$date2))
  })
  output$regioneOut <- renderPrint({ input$regione })
  output$provinciaOut <- renderPrint({ input$provincia })
  
  
  #################################################################################################################
  
  #FILE
  
  #################################################################################################################
  
  # elaborazione regioni
  
  # file_regioni<-"https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-regioni.json"%>% 
  #   GET() %>% 
  #   content() %>% 
  #   jsonlite::fromJSON(simplifyVector = FALSE)
  # 
  # d_reg<-data.frame("data"=as.character(),"stato"=as.character(),"codice_regione"=as.character(),"denominazione_regione"=as.character(),
  #                   "lat"=as.numeric(),"long"=as.numeric(),"ricoverati_con_sintomi"=as.numeric(),"terapia_intensiva"=as.numeric(),
  #                   "totale_ospedalizzati"=as.numeric(),"isolamento_domiciliare"=as.numeric(),"totale_positivi"=as.numeric(),"variazione_nuovi_positivi"=as.numeric(),
  #                   "nuovi_positivi"=as.numeric(),"dimessi_guariti"=as.numeric(),"deceduti"=as.numeric(),"totale_casi"=as.numeric(),
  #                   "tamponi"=as.numeric(),stringsAsFactors = F)
  # 
  # for(i in 1:length(file_regioni)){
  #   d_temp<-data.frame("data"=as.character(file_regioni[[i]]$data),"stato"=as.character(file_regioni[[i]]$stato),"codice_regione"=as.character(file_regioni[[i]]$codice_regione),"denominazione_regione"=as.character(file_regioni[[i]]$denominazione_regione),
  #                               "lat"=as.numeric(file_regioni[[i]]$lat),"long"=as.numeric(file_regioni[[i]]$long),"ricoverati_con_sintomi"=as.numeric(file_regioni[[i]]$ricoverati_con_sintomi),"terapia_intensiva"=as.numeric(file_regioni[[i]]$terapia_intensiva),
  #                               "totale_ospedalizzati"=as.numeric(file_regioni[[i]]$totale_ospedalizzati),"isolamento_domiciliare"=as.numeric(file_regioni[[i]]$isolamento_domiciliare),"totale_positivi"=as.numeric(file_regioni[[i]]$totale_positivi),
  #                               "variazione_nuovi_positivi"=as.numeric(file_regioni[[i]]$variazione_totale_positivi),"nuovi_positivi"=as.numeric(file_regioni[[i]]$nuovi_positivi),"dimessi_guariti"=as.numeric(file_regioni[[i]]$dimessi_guariti),"deceduti"=as.numeric(file_regioni[[i]]$deceduti),"totale_casi"=as.numeric(file_regioni[[i]]$totale_casi),
  #                               "tamponi"=as.numeric(file_regioni[[i]]$tamponi),stringsAsFactors = F)
  #   #d_temp<-json_data_frame <- as.data.frame(file_regioni[[i]])
  #   d_reg<-rbind(d_reg,d_temp)
  # }
  d_reg<-read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv",sep=",",header=T,stringsAsFactors = F)
  for(i in 1:nrow(d_reg)){
    if(substring(d_reg$data[i],1,8)=="20202020"){
      d_reg$data[i]<-paste0("2020",substring(d_reg$data[i],9,22))
    }
  }
  d_reg$data<-as.POSIXlt(d_reg$data)
  
  d_reg$data<-as.POSIXct(d_reg$data)
  d_reg$stato<-as.character(d_reg$stato)
  d_reg$codice_regione<-as.numeric(as.character(d_reg$codice_regione))
  d_reg$denominazione_regione<-as.character(d_reg$denominazione_regione)
  d_reg$lat<-as.numeric(as.character(d_reg$lat))
  d_reg$long<-as.numeric(as.character(d_reg$long))
  d_reg$ricoverati_con_sintomi<-as.numeric(as.character(d_reg$ricoverati_con_sintomi))
  d_reg$terapia_intensiva<-as.numeric(as.character(d_reg$terapia_intensiva))
  d_reg$totale_ospedalizzati<-as.numeric(as.character(d_reg$totale_ospedalizzati))
  d_reg$isolamento_domiciliare<-as.numeric(as.character(d_reg$isolamento_domiciliare))
  d_reg$totale_positivi<-as.numeric(as.character(d_reg$totale_positivi))
  #d_reg$variazione_nuovi_positivi<-as.numeric(as.character(d_reg$variazione_nuovi_positivi))
  d_reg$variazione_nuovi_positivi<-as.numeric(as.character(d_reg$variazione_totale_positivi))
  d_reg$nuovi_positivi<-as.numeric(as.character(d_reg$nuovi_positivi))
  d_reg$dimessi_guariti<-as.numeric(as.character(d_reg$dimessi_guariti))
  d_reg$deceduti<-as.numeric(as.character(d_reg$deceduti))
  d_reg$totale_casi<-as.numeric(as.character(d_reg$totale_casi))
  d_reg$tamponi<-as.numeric(as.character(d_reg$tamponi))
  d_reg$denominazione_regione[d_reg$denominazione_regione=="Emilia-Romagna"]<-"Emilia Romagna"
  
  
  
  d_reg$totale_positivi[d_reg$totale_positivi==0]<-NA
  names(d_reg)[3]<-"reg_istat_code_num"
  names(d_reg)[4]<-"reg_name"
  d_reg$reg_name<-as.character(d_reg$reg_name)
  d_reg$data<-as.POSIXct(d_reg$data)
  d_reg$stato<-as.character(d_reg$stato)

  for(i in 1:nrow(d_reg)){
    if(is.na(d_reg$totale_positivi[i])){
      d_reg$colore[i]<-"transparent"
      d_reg$radius[i]<-0
    }else{
      if(d_reg$totale_positivi[i]<=10){
        d_reg$colore[i]<-"#a4a4f5"
        d_reg$radius[i]<-2
      }
      if(d_reg$totale_positivi[i]>10 && d_reg$totale_positivi[i]<=20 ){
        d_reg$colore[i]<-"#8c8cf5"
        d_reg$radius[i]<-3
      }
      if(d_reg$totale_positivi[i]>20 && d_reg$totale_positivi[i]<=50){
        d_reg$colore[i]<-"#6e6ef0"
        d_reg$radius[i]<-4
      }
      if(d_reg$totale_positivi[i]>50 && d_reg$totale_positivi[i]<=100){
        d_reg$colore[i]<-"#5151f0"
        d_reg$radius[i]<-5
      }
      if(d_reg$totale_positivi[i]>100 && d_reg$totale_positivi[i]<=250){
        d_reg$colore[i]<-"#3a3af0"
        d_reg$radius[i]<-6
      }
      if(d_reg$totale_positivi[i]>250 && d_reg$totale_positivi[i]<=500){
        d_reg$colore[i]<-"#1a1aed"
        d_reg$radius[i]<-7
      }
      if(d_reg$totale_positivi[i]>500 && d_reg$totale_positivi[i]<=750){
        d_reg$colore[i]<-"#0808d1"
        d_reg$radius[i]<-8
      }
      if(d_reg$totale_positivi[i]>750 && d_reg$totale_positivi[i]<=1250){
        d_reg$colore[i]<-"#0707ad"
        d_reg$radius[i]<-9
      }
      if(d_reg$totale_positivi[i]>1250 && d_reg$totale_positivi[i]<=2500){
        d_reg$colore[i]<-"#070785"
        d_reg$radius[i]<-10
      }
      if(d_reg$totale_positivi[i]>2500 && d_reg$totale_positivi[i]<=3500){
        d_reg$colore[i]<-"#050563"
        d_reg$radius[i]<-11
      }
      if(d_reg$totale_positivi[i]>3500 && d_reg$totale_positivi[i]<=4000 ){
        d_reg$colore[i]<-"#040447"
        d_reg$radius[i]<-12
      }
      if(d_reg$totale_positivi[i]>4000){
        d_reg$colore[i]<-"#000000"
        d_reg$radius[i]<-13
      }
    }
    
    
  }
  
  # d_reg$mese<-as.numeric(substr(as.character(d_reg$data),6,7))
  # d_reg$giorno<-as.numeric(substr(as.character(d_reg$data),9,10))
  
  
  
  # elaborazione nazione
  d_reg2<-d_reg
  d_reg2[is.na(d_reg2)]<-0
  
  d_naz<-aggregate(list(d_reg2$ricoverati_con_sintomi,d_reg2$terapia_intensiva,d_reg2$totale_ospedalizzati,d_reg2$isolamento_domiciliare,
                        d_reg2$totale_positivi,d_reg2$variazione_nuovi_positivi,d_reg2$nuovi_positivi,d_reg2$dimessi_guariti,d_reg2$deceduti,d_reg2$totale_casi),by=list(d_reg2$stato,d_reg2$data),FUN=sum)
  
  names(d_naz)<-c("stato","data","ricoverati_con_sintomi","terapia_intensiva","totale_ospedalizzati","isolamento_domiciliare","totale_positivi","variazione_positivi","nuovi_positivi","dimessi_guariti","deceduti","totale_casi")
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
    d_naz$variazione_nap_per[i+1]<-(d_naz$nuovi_positivi[i+1]/d_naz$totale_positivi[i])*100
    d_naz$variazione_dg_ass[i+1]<-d_naz$dimessi_guariti [i+1]-d_naz$dimessi_guariti [i]
    d_naz$variazione_dg_per[i+1]<-(d_naz$dimessi_guariti[i+1]-d_naz$dimessi_guariti [i])/d_naz$dimessi_guariti [i]*100
    d_naz$variazione_d_ass[i+1]<-d_naz$deceduti[i+1]-d_naz$deceduti[i]
    d_naz$variazione_d_per[i+1]<-(d_naz$deceduti[i+1]-d_naz$deceduti[i])/d_naz$deceduti[i]*100
    d_naz$variazione_tc_ass[i+1]<-d_naz$totale_casi[i+1]-d_naz$totale_casi[i]
    d_naz$variazione_tc_per[i+1]<-(d_naz$totale_casi[i+1]-d_naz$totale_casi[i])/d_naz$totale_casi[i]*100
  }
  # d_naz$mese<-as.numeric(substr(as.character(d_naz$data),6,7))
  # d_naz$giorno<-as.numeric(substr(as.character(d_naz$data),9,10))
  
  
  gat1<-gather(d_naz,"Statistica","Valore",ricoverati_con_sintomi:variazione_tc_per,na.rm=TRUE)
  gat1$Statistica<-toupper(gat1$Statistica)
  gat1$Statistica<-gsub("_"," ",gat1$Statistica)
  Ospedalizzati<-gat1[gat1$Statistica=="RICOVERATI CON SINTOMI" | gat1$Statistica=="TERAPIA INTENSIVA" ,]
  Positivi<-gat1[gat1$Statistica=="ISOLAMENTO DOMICILIARE" | gat1$Statistica=="TOTALE OSPEDALIZZATI" ,]
  Casi<-gat1[gat1$Statistica=="DECEDUTI" | gat1$Statistica=="DIMESSI GUARITI"| gat1$Statistica=="TOTALE POSITIVI" ,]
  
  gat2<-gather(d_reg,"Statistica","Valore",ricoverati_con_sintomi:deceduti,na.rm=TRUE)
  gat2$Statistica<-toupper(gat2$Statistica)
  gat2$Statistica<-gsub("_"," ",gat2$Statistica)
  gat2<-gat2[gat2$data==max(gat2$data),]
  gat2<-gat2[gat2$Statistica!="TOTALE OSPEDALIZZATI",]
  gat2<-gat2[gat2$Statistica!="TOTALE POSITIVI",]
  gat2<-gat2[gat2$Statistica!="VARIAZIONE NUOVI POSITIVI",]
  gat2<-gat2[gat2$Statistica!="NUOVI POSITIVI",]
  
  gat2$colore[gat2$Statistica=="RICOVERATI CON SINTOMI"]<-"rgba(34,113,179,1)"
  gat2$colore[gat2$Statistica=="TERAPIA INTENSIVA"]<-"rgba(37,40,80,1)"
  gat2$colore[gat2$Statistica=="ISOLAMENTO DOMICILIARE"]<-"rgba(0,127,255,1)"
  gat2$colore[gat2$Statistica=="DECEDUTI"]<-"rgba(255,0,0,1)"
  gat2$colore[gat2$Statistica=="DIMESSI GUARITI"]<-"rgba(80,200,120,1)"
  
  
  filtrata_naz<-d_naz[d_naz$data==max(d_naz$data),]
  
  # elaborazione province
  
  # file_province<-"https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-province.json"%>%
  #   GET() %>% 
  #   content() %>% 
  #   jsonlite::fromJSON(simplifyVector = FALSE)
  d_prov<-read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv",sep=",",header=T,stringsAsFactors = F)
  
  d_prov<-unique(d_prov)
  
  for(i in 1:nrow(d_prov)){
    if(substring(d_prov$data[i],1,8)=="20202020"){
      d_prov$data[i]<-paste0("2020",substring(d_prov$data[i],9,22))
    }
  }
  
  # d_prov<-data.frame("data"=as.character(),"stato"=as.character(),"codice_regione"=as.character(),"denominazione_regione"=as.character(),
  #                    "codice_provincia"=as.numeric(),"denominazione_provincia"=as.character(),"sigla_provincia"=as.character(),
  #                    "lat"=as.numeric(),"long"=as.numeric(),"totale_casi"=as.numeric())
  # 
  # for(i in 1:length(file_province)){
  #   if(is.null(file_province[[i]]$totale_casi)){
  #     file_province[[i]]$totale_casi<-0
  #   }
  #   d_temp<-json_data_frame <- as.data.frame(file_province[[i]])
  #   d_prov<-rbind(d_prov,d_temp)
  # }
  names(d_prov)[5]<-"prov_istat_code_num"
  d_prov$denominazione_regione<-as.character(d_prov$denominazione_regione)
  d_prov$denominazione_regione[d_prov$denominazione_regione=="Emilia-Romagna"]<-"Emilia Romagna"
  d_prov$denominazione_provincia<-as.character(d_prov$denominazione_provincia)
  d_prov$denominazione_provincia[d_prov$denominazione_provincia=="ForlÃ¬-Cesena"]<-"Forlì-Cesena"
  d_prov$sigla_provincia<-as.character(d_prov$sigla_provincia)
  d_prov$data<-as.POSIXct(d_prov$data)
  d_prov$stato<-as.character(d_prov$stato)
  d_prov$totale_casi<-as.numeric(as.character(d_prov$totale_casi))
  
  d_prov$totale_casi[d_prov$totale_casi==0]<-NA
  
  d_prov$lat[which(d_prov$denominazione_provincia=="In fase di definizione/aggiornamento")]<-NA
  d_prov$long[which(d_prov$denominazione_provincia=="In fase di definizione/aggiornamento")]<-NA
  

  
  # demos_reg_p<-aggregate(demos$Popolazione,by=list(demos$reg,demos$cod_reg),FUN=sum)
  # names(demos_reg_p)<-c("reg","reg_istat_code_num","Popolazione")
  # demos_reg_s<-aggregate(demos$Superficie,by=list(demos$reg,demos$cod_reg),FUN=sum)
  # names(demos_reg_s)<-c("reg","reg_istat_code_num","Superficie")
  # 
  # demos_reg<-cbind(demos_reg_p,"Superficie"=demos_reg_s$Superficie)
  # demos_reg$reg_istat_code_num[demos_reg$reg=="P.A. Bolzano"]<-41
  # demos_reg$reg_istat_code_num[demos_reg$reg=="P.A. Trento"]<-42
  # d_reg$reg_istat_code_num[d_reg$reg_name=="P.A. Bolzano"]<-41
  # d_reg$reg_istat_code_num[d_reg$reg_name=="P.A. Trento"]<-42
  # 
  # 
  # d_reg<-inner_join(d_reg,demos_reg,by="reg_istat_code_num")
  # 
  # 
  # 
  # names(demos)[c(7,8)]<-c("codice_regione","prov_istat_code_num")
  # d_prov$codice_regione[d_prov$denominazione_regione=="P.A. Bolzano"]<-41
  # d_prov$codice_regione[d_prov$denominazione_regione=="P.A. Trento"]<-42
  # d_prov<-left_join(d_prov,demos,by=c("denominazione_provincia"))
  # 
  # d_reg$CasiPercentuale<-d_reg$totale_casi/max(na.omit(d_reg$Popolazione),1)*1000
  # d_reg$casiSuperficie<-d_reg$totale_casi/max(na.omit(d_reg$Superficie),1)
  # d_reg$PositiviPercentuale<-d_reg$totale_positivi/max(na.omit(d_reg$Popolazione),1)*1000
  # d_reg$PositiviSuperficie<-d_reg$totale_positivi/max(na.omit(d_reg$Superficie),1)
  # 
  # 
  # d_prov$CasiPercentuale<-d_prov$totale_casi/max(na.omit(d_prov$Popolazione),1)*1000
  # d_prov$casiSuperficie<-d_prov$totale_casi/max(na.omit(d_prov$Superficie),1)


  Reg <- data.table(d_reg)
  Prov <- data.table(d_prov)
  Gat2<-data.table(gat2)
  
  RegioneSerie<-reactive({
    as.data.frame(Reg[reg_name %like% input$regione])
  })
  ProvinciaSerie<-reactive({
    as.data.frame(Prov[denominazione_regione %like% input$regione])
  })
  RegionePie<-reactive({
    as.data.frame(Gat2[reg_name %like% input$regione])
  })
  
  
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
  
  #################################################################################################################
  
  # Pagina 1  Analisi Territoriale
  
  ##################################################################################################################
  
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
  
  output$mostradata<-renderPrint({
    Giorno_Naz()$data
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
    
    data.frame("Data"=paste0("Giorno visualizzato: ",as.Date(unique(as.character(Giorno_Naz()$data)))))
  })
  
  output$totale_casi_naz <- renderValueBox({
    if(Giorno_Naz()$variazione_tc_ass>0){
      valueBox(
        (paste0('Totale casi',"<br/>",formatC(Giorno_Naz()$totale_casi, format="d", big.mark='.',decimal.mark = ','),"<br/>")%>%
           lapply(htmltools::HTML))
        ,paste0('+',formatC(Giorno_Naz()$nuovi_positivi, format="d", big.mark='.',decimal.mark = ',')," contagi")
        ,icon = icon("procedures",lib='glyphicon')
        ,color = "black")
    }else{
      valueBox(
        (paste0('Totale casi',"<br/>",formatC(Giorno_Naz()$totale_casi, format="d", big.mark='.',decimal.mark = ','),"<br/>")%>%
           lapply(htmltools::HTML))
        ,paste0(formatC(Giorno_Naz()$nuovi_positivi, format="d", big.mark='.',decimal.mark = ',')," contagi")
        ,icon = icon("procedures",lib='glyphicon')
        ,color = "black")
    }
  })
  
  output$positivi_naz <- renderValueBox({
    if(Giorno_Naz()$variazione_positivi>0){
      valueBox(
        (paste0('Positivi',"<br/>",formatC(Giorno_Naz()$totale_positivi, format="d", big.mark='.',decimal.mark = ','),"<br/>")%>%
           lapply(htmltools::HTML))
        ,paste0('+',formatC(Giorno_Naz()$variazione_positivi, format="d", big.mark='.',decimal.mark = ',')," variazione positivi")
        ,icon = icon("procedures",lib='glyphicon')
        ,color = "blue")
    }else{
      valueBox(
        (paste0('Positivi',"<br/>",formatC(Giorno_Naz()$totale_positivi, format="d", big.mark='.',decimal.mark = ','),"<br/>")%>%
           lapply(htmltools::HTML))
        ,paste0(formatC(Giorno_Naz()$variazione_positivi, format="d", big.mark='.',decimal.mark = ',')," variazione positivi")
        ,icon = icon("procedures",lib='glyphicon')
        ,color = "blue")
    }
  })
  
  output$dimessi_naz <- renderValueBox({
    if(Giorno_Naz()$variazione_dg_ass>0){
      valueBox(
        (paste0('Dimessi guariti',"<br/>",formatC(Giorno_Naz()$dimessi_guariti, format="d", big.mark='.',decimal.mark = ','),"<br/>")%>%
           lapply(htmltools::HTML))
        ,paste0('+',formatC(Giorno_Naz()$variazione_dg_ass, format="d", big.mark='.',decimal.mark = ',')," nuovi guariti")
        ,icon = icon("check",lib='glyphicon')
        ,color = "navy")
    }else{
      valueBox(
        (paste0('Dimessi guariti',"<br/>",formatC(Giorno_Naz()$dimessi_guariti, format="d", big.mark='.',decimal.mark = ','),"<br/>")%>%
           lapply(htmltools::HTML))
        ,paste0(formatC(Giorno_Naz()$variazione_dg_ass, format="d", big.mark='.',decimal.mark = ',')," nuovi guariti")
        ,icon = icon("check",lib='glyphicon')
        ,color = "navy")
    }
  })
  
  
  output$decessi_naz <- renderValueBox({
    if(Giorno_Naz()$variazione_d_ass>0){
      valueBox(
        (paste0('Totale decessi',"<br/>",formatC(Giorno_Naz()$deceduti, format="d", big.mark='.',decimal.mark = ','),"<br/>")%>%
           lapply(htmltools::HTML))
        ,paste0('+',formatC(Giorno_Naz()$variazione_d_ass, format="d", big.mark='.',decimal.mark = ',')," nuovi decessi"),
        color = "red")
    }else{
      valueBox(
        (paste0('Totale decessi',"<br/>",formatC(Giorno_Naz()$deceduti, format="d", big.mark='.',decimal.mark = ','),"<br/>")%>%
           lapply(htmltools::HTML))
        ,paste0(formatC(Giorno_Naz()$variazione_d_ass, format="d", big.mark='.',decimal.mark = ',')," nuovi decessi"),
        color = "red")
    }
    
  })
  
  
  #choose and creating map
  
  
  sceltagrafico<-reactive({
    if(input$SceltaVisuale=="Regione"){
      mytext <- paste(
        "Regione: ",Giorno_Reg()$reg_name, "<br/>",
        "Attualmente positivi: ", Giorno_Reg()$totale_positivi, "<br/>", 
        "Guariti: ", Giorno_Reg()$dimessi_guariti, "<br/>", 
        "Decessi: ", Giorno_Reg()$deceduti, sep="") %>%
        lapply(htmltools::HTML)
      
      leaflet(Giorno_Reg()) %>% 
        addTiles()  %>% 
        setView( lat=42, lng=10.5 , zoom=4.5) %>%
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
        
        leaflet(Giorno_Prov()) %>%#[!is.na(Giorno_Prov()[,8]),]) %>% 
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
  
  
  
  
  # table
  
  output$value <- renderPrint({ input$checkbox })
  
  
  sceltatabella<-reactive({
    if(input$SceltaVisuale=="Regione"){
      data.frame("Regione"=Giorno_Reg()[,4],"Ricoverati"=Giorno_Reg()[,7],"Terapia Intensiva"=Giorno_Reg()[,8],"Totale Ospedalizzati"=Giorno_Reg()[,9],
                 "Isolamento domiciliare"=Giorno_Reg()[,10],"Contagi"=Giorno_Reg()[,13],"Attualmente Positivi"=Giorno_Reg()[,11],"Variazione Positivi"=Giorno_Reg()[,12],
                 "Dimessi guariti"=Giorno_Reg()[,14],"Deceduti"=Giorno_Reg()[,15],
                 "Totale Casi"=Giorno_Reg()[,16],"Tamponi effettuati"=Giorno_Reg()[,17])

    }else{
      if(input$SceltaVisuale=="Provincia"){
        data.frame("Regione"=Giorno_Prov()[!is.na(Giorno_Prov()[,8]),4],"Provincia"=Giorno_Prov()[!is.na(Giorno_Prov()[,8]),6],"Totale Casi"=Giorno_Prov()[!is.na(Giorno_Prov()[,8]),10])
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
  
  ###################################################################################################################
  
  # Pagina 2 Analisi Trend Nazionale
  
  ##################################################################################################################
  
  #creating series plot
  
  fig <- plot_ly(d_naz, x = d_naz$data)
  fig <- fig %>% add_lines(y = d_naz$totale_casi, name = "Totale Casi", line = list(color='rgba(37,40,80,1)',shape = "spline"))
  fig <- fig %>% add_lines(y = d_naz$dimessi_guariti, name = "Guariti", line = list(color='rgba(80,200,120,1)',shape = "spline"))
  fig <- fig %>% add_lines(y = d_naz$deceduti, name = "Deceduti", line = list(color='rgba(255,0,0,1)',shape = "spline"))
  fig <- fig %>% add_lines(y = d_naz$totale_positivi, name = "Attualmente positivi", line = list(color='rgba(34,113,179,1)',shape = "spline"))
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
  fig <- fig %>% layout(legend = list(x = 0, y = 0.9))
  
  output$serie<-renderPlotly({
    fig
  })
  
  ##
  
  guarigione_1 <- list(
    xref = 'paper',
    yref = 'y',
    x = 0.05,
    y = round(d_naz$dimessi_guariti[1]/d_naz$totale_casi[1],4)*100,
    xanchor = 'right',
    yanchor = 'middle',
    text = ~paste(round(d_naz$dimessi_guariti[1]/d_naz$totale_casi[1],4)*100, '%'),
    font = list(family = 'Arial',
                size = 14,
                color = 'rgba(67,67,67,1)'),
    showarrow = FALSE)

  decessi_1 <- list(
    xref = 'paper',
    yref = 'y',
    x = 0.05,
    y = round(d_naz$deceduti[1]/d_naz$totale_casi[1],4)*100,
    xanchor = 'right',
    yanchor = 'middle',
    text = ~paste(round(d_naz$deceduti[1]/d_naz$totale_casi[1],4)*100, '%'),
    font = list(family = 'Arial',
                size = 14,
                color = 'rgba(67,67,67,1)'),
    showarrow = FALSE)

  guarigione_2 <- list(
    xref = 'paper',
    x = 0.95,
    y = round(d_naz$dimessi_guariti[nrow(d_naz)]/d_naz$totale_casi[nrow(d_naz)],4)*100,
    xanchor = 'left',
    yanchor = 'middle',
    text = paste(round(d_naz$dimessi_guariti[nrow(d_naz)]/d_naz$totale_casi[nrow(d_naz)],4)*100, '%'),
    font = list(family = 'Arial',
                size = 14,
                color = 'rgba(67,67,67,1)'),
    showarrow = FALSE)

  decessi_2 <- list(
    xref = 'paper',
    x = 0.95,
    y = round(d_naz$deceduti[nrow(d_naz)]/d_naz$totale_casi[nrow(d_naz)],4)*100,
    xanchor = 'left',
    yanchor = 'middle',
    text = paste(round(d_naz$deceduti[nrow(d_naz)]/d_naz$totale_casi[nrow(d_naz)],4)*100, '%'),
    font = list(family = 'Arial',
                size = 12,
                color = 'rgba(67,67,67,1)'),
    showarrow = FALSE)

  tas <- plot_ly(d_naz, x = d_naz$data)
  tas <- tas %>% add_lines(y = round(d_naz$dimessi_guariti/d_naz$totale_casi,4)*100, name = "Tasso Guarigione", line = list(color='rgba(80,200,120,1)', shape = "spline"))
  tas <- tas %>% add_lines(y = round(d_naz$deceduti/d_naz$totale_casi,4)*100, name = "Tasso Mortalità", line = list(color='rgba(255,0,0,1)',shape = "spline"))
  tas <- tas %>% layout(title = "Andamento Tassi",
                        annotations = guarigione_1)
  tas <-tas %>% layout(plot_bgcolor='rgb(236, 240, 245)') %>%
    layout(paper_bgcolor='rgb(236, 240, 245)')
  tas <- tas %>% layout(legend = list(x = 0, y = 0.9))
  tas <- tas %>% add_trace(x = c(d_naz$data[1], d_naz$data[nrow(d_naz)]),
                           y = c(round(d_naz$dimessi_guariti[1]/d_naz$totale_casi[1],4)*100, round(d_naz$dimessi_guariti[nrow(d_naz)]/d_naz$totale_casi[nrow(d_naz)],4)*100),
                           name="Valore Guariti%",type = 'scatter', mode = 'markers', marker = list(color = 'rgba(80,200,120, 1)', size = 8))
  tas <- tas %>% add_trace(x = c(d_naz$data[1], d_naz$data[nrow(d_naz)]),
                           y = c(round(d_naz$deceduti[1]/d_naz$totale_casi[1],4)*100, round(d_naz$deceduti[nrow(d_naz)]/d_naz$totale_casi[nrow(d_naz)],4)*100),
                           name="Valore Decessi%",type = 'scatter', mode = 'markers', marker = list(color = 'rgba(255,0,0,1)', size = 12))
  tas <- tas %>% layout(annotations = decessi_1)
  tas <- tas %>% layout(annotations = guarigione_2)
  tas <- tas %>% layout(annotations = decessi_2)
  tas
  output$serieTassi<-renderPlotly({
    tas
  })
  
  
  
  
  sceltaplot<-reactive({
    if(as.character(input$variabile)=="pos"){
      plot_ly(d_naz, x = d_naz$data, y = d_naz$variazione_positivi, type = 'bar', text= paste0("Nuovi positivi: ",d_naz$variazione_positivi),
              marker = list(color = 'rgb(158,202,225)',
                            line = list(color = 'rgb(8,48,107)',
                                        width = 1.5))) %>% 
        layout( title = "Andamento giornaliero Nuovi Positivi") %>%
        layout(plot_bgcolor='rgb(236, 240, 245)') %>% 
        layout(paper_bgcolor='rgb(236, 240, 245)')
    }else{
    if(as.character(input$variabile)=="gu"){
      plot_ly(d_naz, x = d_naz$data, y = d_naz$variazione_dg_ass, type = 'bar', text= paste0("Nuovi guariti: ",d_naz$variazione_dg_ass),
              marker = list(color = 'rgb(26,148,49)',
                            line = list(color = 'rgb(26,102,46)',
                                        width = 1.5))) %>% 
        layout( title = "Andamento giornaliero Guariti") %>%
        layout(plot_bgcolor='rgb(236, 240, 245)') %>% 
        layout(paper_bgcolor='rgb(236, 240, 245)')
    }else{
    if(as.character(input$variabile)=="dec"){
      plot_ly(d_naz, x = d_naz$data, y = d_naz$variazione_d_ass, type = 'bar', text= paste0("Nuovi decessi: ",d_naz$variazione_d_ass),
              marker = list(color = 'rgb(250,0,0)',
                            line = list(color = 'rgb(200,8,21)',
                                        width = 1.5))) %>% 
        layout( title = "Andamento giornaliero Decessi") %>%
        layout(plot_bgcolor='rgb(236, 240, 245)') %>% 
        layout(paper_bgcolor='rgb(236, 240, 245)')
    }else{
          if(as.character(input$variabile)=="con"){
            plot_ly(d_naz, x = d_naz$data, y = d_naz$nuovi_positivi, type = 'bar', text= paste0("Contagi: ",d_naz$nuovi_positivi),
                    marker = list(color = 'rgb(128,128,128)',
                                  line = list(color = 'rgb(0,0,0)',
                                              width = 1.5))) %>% 
              layout( title = "Andamento giornaliero Contagi") %>%
              layout(plot_bgcolor='rgb(236, 240, 245)') %>% 
              layout(paper_bgcolor='rgb(236, 240, 245)')
          }}
      }
    }
  })
  
  output$VariabiliPlot<-renderPlotly({
    sceltaplot()
  })
  
  

  
  ############################################################################################################
  
  # Pagina 3  Analisi Reg
  
  ############################################################################################################
  
  d_reg_ult<-reactive({
    if(input$regione!=""){
    as.data.frame(RegioneSerie()[RegioneSerie()$data==(max(RegioneSerie()$data)),])
    }
    })
  

  
  dataset_regione<-reactive({
    if(input$regione!=""){
    data.frame("Totale_Casi"=d_reg_ult()$totale_casi,"Contagi"=d_reg_ult()$nuovi_positivi,
               "Positivi"=d_reg_ult()$totale_positivi,"Guariti"=d_reg_ult()$dimessi_guariti,
               "Decessi"=d_reg_ult()$deceduti
               )
    }
    
  })
  
  output$dt_reg<-renderTable({
    dataset_regione()
  })
  
  
  dataset_regione_tassi<-reactive({
    if(input$regione!=""){
      data.frame("Tasso_Guarigione"=paste0(round((d_reg_ult()$dimessi_guariti/d_reg_ult()$totale_casi)*100,2),"%"),
                 "Tasso_Mortalita"=paste0(round((d_reg_ult()$deceduti/d_reg_ult()$totale_casi)*100,2),"%"))
    }
    
  })
  
  output$dt_reg_tassi<-renderTable({
    dataset_regione_tassi()
  })
  
  
  
  # dataset_regione_percentuale<-reactive({
  #   if(input$regione!=""){
  #     data.frame("Casi_su_1000_abitanti"=round((d_reg_ult()$CasiPercentuale),4),
  #                "Positivi_su_1000_abitanti"=round(d_reg_ult()$PositiviPercentuale,4),
  #                "Casi_su_mq"=round((d_reg_ult()$casiSuperficie),4),
  #                "Positivi_su_mq"=round(d_reg_ult()$PositiviSuperficie,4)
  #                )
  #   }
  #   
  # })
  # 
  # output$dt_reg_perc<-renderTable({
  #   dataset_regione_percentuale()
  # })
  

  
  d_prov_ultimi2<-d_prov[d_prov$data>=(max(d_prov$data)-(3600*24)),]
  province<-unique(d_prov_ultimi2$denominazione_provincia)
  for(i in 1: length(province)){
    d_prov_ultimi2$nuovi_contagi[d_prov_ultimi2$denominazione_provincia==province[i]]<-d_prov_ultimi2$totale_casi[d_prov_ultimi2$denominazione_provincia==province[i]][2]-d_prov_ultimi2$totale_casi[d_prov_ultimi2$denominazione_provincia==province[i]][1]
  }
  d_prov_ultimo<-d_prov_ultimi2[d_prov_ultimi2$data==max(d_prov_ultimi2$data),]
  d_prov_ultimo<-d_prov_ultimo[d_prov_ultimo$denominazione_provincia!="In fase di definizione/aggiornamento",]
  
  
  
  dataset_province1<-reactive({
    if(input$regione!=""){
      d_prov_ultimo[d_prov_ultimo$denominazione_regione==input$regione]
    }
  })
  dataset_province2<-reactive({
    if(input$regione!=""){
      data.frame("Provincia"=dataset_province1()$denominazione_provincia,"Totale_Casi"=dataset_province1()$totale_casi,
                 "Nuovi_Contagi"=dataset_province1()$nuovi_contagi)
    }
  })

  output$dt_prov<-renderTable({
    dataset_province2()
  })
  # 
  # 
  # 
  # 
  # output$dt_reg_perc<-renderTable({
  #   dataset_regione_percentuale()
  # })
  
  
  output$TortaRegione<-renderPlotly({
    if(input$regione!=""){
    RegionePie() %>% plot_ly(labels = ~Statistica, values = ~Valore, marker = list(colors = ~colore,
                                                                                   line = list(color = '#FFFFFF', width = 1)))%>% 
      add_pie(hole = 0.6)%>% 
      layout(title = "",  showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
      layout(plot_bgcolor='#ecf0f5') %>% 
      layout(paper_bgcolor='#ecf0f5') %>%
      layout(legend = list(orientation = 'h'))
    }
  })
  
  
  
 
  SerieReg<-reactive({
    if(input$regione!=""){
      plot_ly(RegioneSerie(), x = RegioneSerie()$data)%>% add_lines(y = RegioneSerie()$totale_casi, 
                                                                    name = "Totale Casi", line = list(color='rgba(37,40,80,1)',shape = "spline"))%>% 
        add_lines(y = RegioneSerie()$dimessi_guariti, name = "Guariti", 
                  line = list(color='rgba(80,200,120,1)',shape = "spline"))%>% 
        add_lines(y = RegioneSerie()$deceduti, name = "Deceduti", 
                  line = list(color='rgba(255,0,0,1)',shape = "spline")) %>%
        add_lines(y = RegioneSerie()$totale_positivi, name = "Attualmente positivi",
                  line = list(color='rgba(34,113,179,1)',shape = "spline"))%>% 
        layout(
          title = paste0("Andamento ",unique(RegioneSerie()$reg_name)),
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
          yaxis = list(title = "Numero Persone"))%>% 
        layout(plot_bgcolor='rgb(236, 240, 245)') %>% 
        layout(paper_bgcolor='rgb(236, 240, 245)')%>% 
        layout(legend = list(x = 0, y = 0.9))
    }
    
  })

  
  SerieProv<-reactive({
    if(input$regione!=""){
      fig <- ProvinciaSerie()
      fig <- fig %>% plot_ly(x = as.Date(ProvinciaSerie()$data), y = ~totale_casi, color = ~denominazione_provincia,type="bar")%>%
        layout(legend = list(x = 0, y = 0.9)) %>%
        layout(plot_bgcolor='rgb(236, 240, 245)') %>% 
        layout(paper_bgcolor='rgb(236, 240, 245)') %>%
        layout(yaxis=list(title = "Numero Persone")) %>%
        layout(
          title = paste0("Andamento province ",unique(RegioneSerie()$reg_name)))
    }
    
  })
  
  output$serieReg<-renderPlotly({
    SerieReg()
  })
  
  output$serieProv<-renderPlotly({
    SerieProv()
  })
  
  Messaggio<-reactive({
    if(input$regione!=""){
      paste0("**Nota bene eventuali casi di 'In fase di definizione/aggiornamento' dipendono dalle fasi di verifica della positività**")
    }
  })
  output$messaggio<-renderPrint({
    Messaggio()
  }
  )
  
  
  


  
  ########################################################################################################################
  
  # Pagina 4 #WORLD
  
  ########################################################################################################################
  world_spdf <- readOGR( 
    dsn= paste0(getwd()) , 
    layer="TM_WORLD_BORDERS_SIMPL-0.3",
    verbose=FALSE
  )
  

  
  Confermati<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",header=T,sep=",")
  if("X1.22.20" %in% names(Confermati)){
    Confermati<-gather(Confermati,"Date","Confermati",X1.22.20:names(Confermati)[ncol(Confermati)],na.rm=TRUE)
  }else{
    if("X1.22.2020" %in% names(Confermati)){
      Confermati<-gather(Confermati,"Date","Confermati",X1.22.2020:names(Confermati)[ncol(Confermati)],na.rm=TRUE)
    }
  }
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
  
  
  
  
  Guariti<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",header=T,sep=",")

  if("X1.22.20" %in% names(Guariti)){
    Guariti<-gather(Guariti,"Date","Guariti",X1.22.20:names(Guariti)[ncol(Guariti)],na.rm=TRUE)
  }else{
    if("X1.22.2020" %in% names(Confermati)){
      Guariti<-gather(Guariti,"Date","Guariti",X1.22.2020:names(Guariti)[ncol(Guariti)],na.rm=TRUE)
    }
  }
  #Guariti<-gather(Guariti,"Date","Guariti",X1.22.2020:names(Guariti)[ncol(Guariti)],na.rm=TRUE)
  a<-seq(as.Date("2020/1/22"), by = "day", length.out = length(unique(Guariti$Date)))
  data_presente<-unique(Guariti$Date)
  Guariti$Data<-as.character(seq(1,nrow(Guariti)))
  for(i in 1:length(data_presente)){
    Guariti$Data[which(Guariti$Date==data_presente[i])]<-as.character(a[i])
  }
  names(Guariti)[1]<-"Province.State"
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
  
  
  Decessi<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",header=T,sep=",")
  
  if("X1.22.20" %in% names(Decessi)){
    Decessi<-gather(Decessi,"Date","Decessi",X1.22.20:names(Decessi)[ncol(Decessi)],na.rm=TRUE)
  }else{
    if("X1.22.2020" %in% names(Decessi)){
      Decessi<-gather(Decessi,"Date","Decessi",X1.22.2020:names(Decessi)[ncol(Decessi)],na.rm=TRUE)
    }
  }
  
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
  
  
  Con_Dec<-inner_join(Confermati,Decessi,by=c("date","Nazione"))
  Totale<-inner_join(Guariti,Con_Dec,by=c("date","Nazione"))
  
  
  
  
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
 
 
 dataframe<-reactive({
   data.frame("Nazione"=Giorno_World()$Nazione[i],
              "Confermati"=max(na.omit(Giorno_World()$Confermati[i]),0),
              "Positivi"=max(na.omit(Giorno_World()$Positivi[i]),0),
              "Guariti"=max(na.omit(Giorno_World()$Guariti[i]),0),
              "Decessi"=max(na.omit(Giorno_World()$Decessi[i]),0))
 })
 
  
 output$tabella_world<- DT::renderDataTable({
   DT::datatable(Giorno_World()[,c(2,3,6,4,5)], rownames = FALSE, options = list(
     columnDefs = list(list(className = 'dt-center')),
     pageLength =10,
     lengthMenu = c(5, 10, 15, 20)
   ))
   
   
 })
 
 #######################################################################################################################
 
 #Pagina 5 Trend Nazioni
 
 #######################################################################################################################
 
 updateSelectizeInput(session, 'nazione', choices = unique(Totale$Nazione), server = TRUE)
 
 Totale$date<-as.Date(Totale$date)
 TotaleWorld<-data.table(Totale)

 
 Nazione_fil<-reactive({
   as.data.frame(TotaleWorld[TotaleWorld$Nazione %like% input$nazione])
 })
 
 
 SerieNazWorld<-reactive({
   if(input$nazione!=""){
     plot_ly(Nazione_fil(), x = Nazione_fil()$date)%>% add_lines(y = Nazione_fil()$Confermati, 
                                                                   name = "Confermati", line = list(color='rgba(37,40,80,1)',shape = "spline"))%>% 
       add_lines(y = Nazione_fil()$Guariti, name = "Guariti", 
                 line = list(color='rgba(80,200,120,1)',shape = "spline"))%>% 
       add_lines(y = Nazione_fil()$Decessi, name = "Deceduti", 
                 line = list(color='rgba(255,0,0,1)',shape = "spline")) %>%
       add_lines(y = Nazione_fil()$Positivi, name = "Attualmente positivi",
                 line = list(color='rgba(34,113,179,1)',shape = "spline"))%>% 
       layout(
         title = paste0("Andamento ",unique(Nazione_fil()$Nazione)),
         xaxis = list(
           rangeselector = list(
             buttons = list(
               list(
                 count = 7,
                 label = "7 days",
                 step = "day",
                 stepmode = "backward"),
               list(
                 count = 15,
                 label = "15 days",
                 step = "day",
                 stepmode = "backward"),
               list(
                 count = 30,
                 label = "30 days",
                 step = "day",
                 stepmode = "backward"),
               list(
                 count = 2,
                 label = "2 months",
                 step = "month",
                 stepmode = "todate"),
               list(step = "all"))),
           rangeslider = list(type = "Data")),
         yaxis = list(title = "Numero Persone"))%>% 
       layout(plot_bgcolor='rgb(236, 240, 245)') %>% 
       layout(paper_bgcolor='rgb(236, 240, 245)')%>% 
       layout(legend = list(x = 0, y = 0.9))
   }
   
 })
 
 output$serieNazWorld<-renderPlotly({
   SerieNazWorld()
 })
 
 
 
 
 #######################################################################################################################
 
 #Pagina 5 Informazioni 
 
 ########################################################################################################################
 
  
}


shinyApp(ui, server)



