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
    menuItem("Informazioni",tabName = "info", icon = icon("question-circle")),
    menuItem("Nel mondo",tabName = "mondo", icon = icon("map")),
    
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
        "<p style = 'text-align: center;'><small>&copy; - Marco Cortese - <script>document.write(yyyy);</script></small></p>",
        "<br><br><br><br><br><br><br>",
        img(src="https://www.technologyhub.it/public/uploads/sites/7/Healthy-Reply-LOGO-RGB.png",height = "40px",style="display: block; margin-left: auto; margin-right: auto;"),
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
         h5(verbatimTextOutput("tassoGuarigione"),align="center"),
  ),
  column(4,
         hr(),
         hr(),
         hr(),
         tableOutput("giornoAnalisi")
  )
  
  
)



frow3 <- fluidRow(
  leafletOutput("mappa",height =700),
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
frowProva<-fluidRow(
  hr(),
  hr(),
  column(4),
  column(4,
         plotlyOutput("TortaRegione")),
  column(4)
)



frow9<- fluidRow(
  plotlyOutput("serieReg",height =500),
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
         h3(helpText(   a("Dario Bressanini",     href="https://www.instagram.com/dario.bressanini/"))),
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


body <- dashboardBody(  
  
  
  tabItems(tabItem(tabName = "analisi",frow1, frow2,  frow3,frow4,frow5),
           tabItem(tabName = "trend",frow6,frow7),
           tabItem(tabName = "reg",frow8,frowProva,frow9),
           tabItem(tabName = "info",frow10,frow11,frow12,frow13),
           tabItem(tabName = "mondo",frow14)))

ui <- dashboardPage(title = 'Analisi Covid-19 Italia', header, sidebar, body, skin='black')