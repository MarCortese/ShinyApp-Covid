
setwd("C:/Users/m.cortese/Desktop/Progetti/Corona")

#################################################################################################################

                                              #PACCHETTI

################################################################################################################

library(RColorBrewer)
library(dplyr)
library(highcharter)
library(rjson)
library(httr)
library(plotly)
library(quantmod)
library(tidyr)
library(leaflet)
#################################################################################################################

                                                  #FILE

#################################################################################################################

#regioni

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
names(d_reg)[3]<-"reg_istat_code_num"
names(d_reg)[4]<-"reg_name"
d_reg$reg_name<-as.character(d_reg$reg_name)
d_reg$data<-as.POSIXct(d_reg$data)
d_reg$stato<-as.character(d_reg$stato)
filtrata_reg<-d_reg[d_reg$data==max(d_reg$data),]

#nazione

d_naz<-aggregate(list(d_reg$ricoverati_con_sintomi,d_reg$terapia_intensiva,d_reg$totale_ospedalizzati,d_reg$isolamento_domiciliare,
                      d_reg$totale_attualmente_positivi,d_reg$nuovi_attualmente_positivi,d_reg$dimessi_guariti,d_reg$deceduti,d_reg$totale_casi),by=list(d_reg$stato,d_reg$data),FUN=sum)

names(d_naz)<-c("stato","data","ricoverati_con_sintomi","terapia_intensiva","totale_ospedalizzati","isolamento_domiciliare","totale_attualmente_positivi","nuovi_attualmente_positivi","dimessi_guariti","deceduti","totale_casi")
filtrata_naz<-d_naz[d_naz$data==max(d_naz$data),]

#province

file_province<-"https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-province.json"%>%
GET() %>% 
  content() %>% 
  jsonlite::fromJSON(simplifyVector = FALSE)


d_prov<-data.frame("data"=as.character(),"stato"=as.character(),"codice_regione"=as.character(),"denominazione_regione"=as.character(),
              "codice_provincia"=as.numeric(),"denominazione_provincia"=as.character(),"sigla_provincia"=as.character(),
              "lat"=as.numeric(),"long"=as.numeric(),"totale_casi"=as.numeric())

for(i in 1:length(file_province)){
  d_temp<-json_data_frame <- as.data.frame(file_province[[i]])
  d_prov<-rbind(d_prov,d_temp)
}
names(d_prov)[5]<-"prov_istat_code_num"
d_prov$denominazione_regione<-as.character(d_prov$denominazione_regione)
d_prov$denominazione_provincia<-as.character(d_prov$denominazione_provincia)
d_prov$sigla_provincia<-as.character(d_prov$sigla_provincia)
d_prov$data<-as.POSIXct(d_prov$data)
d_prov$stato<-as.character(d_prov$stato)
filtrata_prov<-d_prov[d_prov$data==max(d_prov$data),]

#######################################################################################################################

                                                  #MAPPE

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
highchart(type = "map") %>% 
  hc_title(text = "Diffusione coronavirus regioni") %>% 
  hc_subtitle(text = "Dati: Protezione civile") %>% 
  hc_add_series_map(map = mappa_regioni, df = filtrata_reg, joinBy = "reg_istat_code_num", 
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


highchart(type = "map") %>% 
  hc_title(text = "Diffusione coronavirus province") %>% 
  hc_subtitle(text = "Dati: Protezione civile") %>% 
  hc_add_series_map(map = mappa_province, df = filtrata_prov, joinBy = "prov_istat_code_num", value = "totale_casi", name="Casi Coranavirus") %>%
  hc_colorAxis(stops = color_stops())%>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_tooltip(useHTML = TRUE, headerFormat = "",
             pointFormat = '{point.properties.prov_name} casi totali:{point.totale_casi}',borderColor = "navy")


##################################################################################################################

                                          #GRAFICI

###################################################################################################################


fig <- plot_ly(d_naz, x = d_naz$data)
fig <- fig %>% add_lines(y = d_naz$totale_casi, name = "Totale Casi")
fig <- fig %>% add_lines(y = d_naz$dimessi_guariti, name = "Guariti")
fig <- fig %>% add_lines(y = d_naz$deceduti, name = "Deceduti")
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

fig

gat1<-gather(filtrata_naz,"Statistica","Valore",ricoverati_con_sintomi:totale_casi,na.rm=TRUE)
gat1$Statistica<-toupper(gat1$Statistica)
gat1$Statistica<-gsub("_"," ",gat1$Statistica)

ospedalizzati<-gat1[1:2,]
positivi<-gat1[3:4,]
totale<-gat1[c(5,7:8),]

colors <- c('rgb(100,149,237)', 'rgb(0,0,255)')


fig_osp <- ospedalizzati %>% plot_ly(labels = ~Statistica, values = ~Valore, marker = list(colors = colors,
                                                                                           line = list(color = '#FFFFFF', width = 1)))
fig_osp <- fig_osp %>% add_pie(hole = 0.6)
fig_osp <- fig_osp %>% layout(title = "Totale Ospedalizzati",  showlegend = T,
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig_osp

colors <- c('rgb(0,0,128', 'rgb(220,20,60)')
fig_pos <- positivi %>% plot_ly(labels = ~Statistica, values = ~Valore, marker = list(colors = colors,
                                                                                      line = list(color = '#FFFFFF', width = 1)))
fig_pos <- fig_pos %>% add_pie(hole = 0.6)
fig_pos <- fig_pos %>% layout(title = "Attualmente Positivi",  showlegend = T,
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig_pos


colors <- c('rgb(138,43,226)', 'rgb(0,128,0)','rgb(139,0,0)')
fig_casi <- totale %>% plot_ly(labels = ~Statistica, values = ~Valore, marker = list(colors = colors,
                                                                                     line = list(color = '#FFFFFF', width = 1)))
fig_casi <- fig_casi %>% add_pie(hole = 0.6)
fig_casi <- fig_casi %>% layout(title = "Totale Casi",  showlegend = T,
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig_casi


par(mfrow=c(2,2))



############
#Percentuale casi su popolazione italiana

d_naz
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



#
#scheda con ordinarze governo e regioni

#http://www.regioni.it/newsletter/n-3785/del-25-02-2020/coronavirus-ordinanze-ultime-circolari-regionali-e-note-esplicative-20851/

#tabella con regioni e link


#numeri utili



############################################################################################################










###########################################################################################################

                                      #PROVA PIE CHART E SUB PIE CHART

############################################################################################################


browsers<-structure(list(browser = structure(c(3L, 3L, 3L, 3L, 2L, 2L, 
                                               2L, 1L, 5L, 5L, 4L), .Label = c("Chrome", "Firefox", "MSIE", 
                                                                               "Opera", "Safari"), class = "factor"), version = structure(c(5L, 
                                                                                                                                            6L, 7L, 8L, 2L, 3L, 4L, 1L, 10L, 11L, 9L), .Label = c("Chrome 10.0", 
                                                                                                                                                                                                  "Firefox 3.5", "Firefox 3.6", "Firefox 4.0", "MSIE 6.0", "MSIE 7.0", 
                                                                                                                                                                                                  "MSIE 8.0", "MSIE 9.0", "Opera 11.x", "Safari 4.0", "Safari 5.0"
                                                                                                                                            ), class = "factor"), share = c(10.85, 7.35, 33.06, 2.81, 1.58, 
                                                                                                                                                                            13.12, 5.43, 9.91, 1.42, 4.55, 1.65), ymax = c(10.85, 18.2, 51.26, 
                                                                                                                                                                                                                           54.07, 55.65, 68.77, 74.2, 84.11, 85.53, 90.08, 91.73), ymin = c(0, 
                                                                                                                                                                                                                                                                                            10.85, 18.2, 51.26, 54.07, 55.65, 68.77, 74.2, 84.11, 85.53, 
                                                                                                                                                                                                                                                                                            90.08)), .Names = c("browser", "version", "share", "ymax", "ymin"
                                                                                                                                                                                                                                                                                            ), row.names = c(NA, -11L), class = "data.frame")


gat1$Macro<-c(rep("Ospedalizzati",3),)



donuts <- function(x, group = 1, labels = NA, col = NULL, radius = c(.7, 1)) {
  group <- rep_len(group, length(x))
  ug  <- unique(group)
  tbl <- table(group)[order(ug)]
  
  col <- if (is.null(col))
    seq_along(ug) else rep_len(col, length(ug))
  col.main <- Map(rep, col[seq_along(tbl)], tbl)
  col.sub  <- lapply(col.main, function(x) {
    al <- head(seq(0, 1, length.out = length(x) + 2L)[-1L], -1L)
    Vectorize(adjustcolor)(x, alpha.f = al)
  })
  
  plot.new()
  
  par(new = TRUE)
  pie(x, border = NA, radius = radius[2L],
      col = unlist(col.sub), labels = labels)
  
  par(new = TRUE)
  pie(x, border = NA, radius = radius[1L],
      col = unlist(col.main), labels = NA)
}

par(mfrow = c(1,2), mar = c(0,4,0,4))
with(browsers,
     donuts(share, browser, sprintf('%s: %s%%', version, share),
            col = c('cyan2','red','orange','green','dodgerblue2'))
)
with(mtcars,
     donuts(mpg, interaction(gear, cyl), rownames(mtcars))
)



library(plotrix)

# browser data without "ymax" and "ymin"
browsers <-structure(
    list(
      browser = structure(
        c(3L, 3L, 3L, 3L, 2L, 2L,
          2L, 1L, 5L, 5L, 4L),
        .Label = c("Chrome", "Firefox", "MSIE",
                   "Opera", "Safari"),
        class = "factor"
      ),
      version = structure(
        c(5L,
          6L, 7L, 8L, 2L, 3L, 4L, 1L, 10L, 11L, 9L),
        .Label = c(
          "Chrome 10.0",
          "Firefox 3.5",
          "Firefox 3.6",
          "Firefox 4.0",
          "MSIE 6.0",
          "MSIE 7.0",
          "MSIE 8.0",
          "MSIE 9.0",
          "Opera 11.x",
          "Safari 4.0",
          "Safari 5.0"
        ),
        class = "factor"
      ),
      share = c(10.85, 7.35, 33.06, 2.81, 1.58,
                13.12, 5.43, 9.91, 1.42, 4.55, 1.65)
    ),
    .Names = c("browser", "version", "share")
    ,
    row.names = c(NA,-11L),
    class = "data.frame"
  )

# aggregate data for the browser pie chart
browser_data <-
  aggregate(browsers$share,
            by = list(browser = browsers$browser),
            FUN = sum)

# order version data by browser so it will line up with browser pie chart
version_data <- browsers[order(browsers$browser), ]

browser_colors <- c('#85EA72', '#3B3B3F', '#71ACE9', '#747AE6', '#F69852')

# adjust these as desired (currently colors all versions the same as browser)
version_colors <-
  c(
    '#85EA72',
    '#3B3B3F',
    '#3B3B3F',
    '#3B3B3F',
    '#71ACE9',
    '#71ACE9',
    '#71ACE9',
    '#71ACE9',
    '#747AE6',
    '#F69852',
    '#F69852'
  )

# format labels to display version and % market share
version_labels <- paste(version_data$version, ": ", version_data$share, "%", sep = "")

# coordinates for the center of the chart
center_x <- 0.5
center_y <- 0.5

plot.new()

# draw version pie chart first
version_chart <-
  floating.pie(
    xpos = center_x,
    ypos = center_y,
    x = version_data$share,
    radius = 0.35,
    border = "white",
    col = version_colors
  )

# add labels for version pie chart
pie.labels(
  x = center_x,
  y = center_y,
  angles = version_chart,
  labels = version_labels,
  radius = 0.38,
  bg = NULL,
  cex = 0.8,
  font = 2,
  col = "gray40"
)

# overlay browser pie chart
browser_chart <-
  floating.pie(
    xpos = center_x,
    ypos = center_y,
    x = browser_data$x,
    radius = 0.25,
    border = "white",
    col = browser_colors
  )

# add labels for browser pie chart
pie.labels(
  x = center_x,
  y = center_y,
  angles = browser_chart,
  labels = browser_data$browser,
  radius = 0.125,
  bg = NULL,
  cex = 0.8,
  font = 2,
  col = "white"
)

gat1_new<-gat1[c(1,2,4,7,8),]
gat1_new$Macro1<-c(rep("TOTALE OSPEDALIZZATI",2),"ISOLAMENTO DOMICILIARE","DIMESSI","DECEDUTI")
gat1_new$Macro2<-c(rep("ATTUALMENTE POSITIVI",3),"DIMESSI","DECEDUTI")

Statistic<-gat1_new[,c(3,4)]
statistic_colors <-
  c(
    '#9400d3',
    '#1e90ff',
    '#6495ed',
    '#006400',
    '#8b0000'
  )
Macro1<-gat1_new[,c(5,4)]
Macro2<-gat1_new[,c(6,4)]

Statistic_chart <-
  floating.pie(
    xpos = center_x,
    ypos = center_y,
    x = Statistic$Valore,
    radius = 0.35,
    border = "white",
    col = statistic_colors
  )
pie.labels(
  x = center_x,
  y = center_y,
  angles = Statistic$Valore,
  labels = Statistic$Statistica,
  radius = 0.38,
  bg = NULL,
  cex = 0.8,
  font = 2,
  col = "gray40"
)



unique(d_reg$reg_name)
coordinate_regioni$Regioni
mappa_regioni

regioni_nomi<-vector()
for(i in 1:length(mappa_regioni[["features"]])){
  regioni_nomi[i]<-mappa_regioni[["features"]][[i]][["properties"]][["reg_name"]]
}

codificaNomi<-function(dataset){
  dataset$reg_name<-gsub("Bolzano","Trentino-Alto Adige",dataset$reg_name)
  dataset$reg_name<-gsub("Emilia Romagna","Emilia-Romagna",dataset$reg_name)
  dataset$reg_name<-gsub("Friuli Venezia Giulia","Friuli-Venezia Giulia",dataset$reg_name)
  dataset$reg_name<-gsub("Trento","Trentino-Alto Adige",dataset$reg_name)

  return(dataset)
}


d_reg<-codificaNomi(d_reg)
filtrata_reg<-codificaNomi(filtrata_reg)

names(filtrata_reg)[3]




library(rjson)
library(plotly)

url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)
url2<- "https://raw.githubusercontent.com/plotly/datasets/master/fips-unemp-16.csv"
df <- read.csv(url2, colClasses=c(fips="character"))
fig <- plot_ly() 
fig <- fig %>% add_trace(
  type="choroplethmapbox",
  geojson=mappa_regioni,
  locations=d_reg$reg_name,
  z=d_reg$totale_attualmente_positivi,
  colorscale="Viridis",
  zmin=0,
  zmax=12,
  marker=list(line=list(
    width=0),
    opacity=0.5
  )
)
fig <- fig %>% layout(
  mapbox=list(
    style="carto-positron",
    zoom =2,
    center=list(lon= 45, lat=37))
)
fig



coordinate_regioni<-read.csv("CoordinateRegioni.csv",header=T,stringsAsFactors = F,sep=";")
coordinate_regioni$Latitudine<-gsub("N","",coordinate_regioni$Latitudine)
coordinate_regioni$Longitudine<-gsub("E","",coordinate_regioni$Longitudine)
coordinate_regioni$Latitudine<-as.numeric(coordinate_regioni$Latitudine)
coordinate_regioni$Latitudine<-as.numeric(coordinate_regioni$Latitudine)




col<-c("#ffe4c4","#deb887","#ff7f50","#d2691e","#ff4500","#ff8c00","#dc143c","#ff0000","#b22222","#8b0000","#800000")

testo<-paste0(filtrata_reg$reg_name,": ",filtrata_reg$totale_attualmente_positivi," positivi","\n",
              filtrata_reg$dimessi_guariti," dimessi","\n",
              filtrata_reg$deceduti," deceduti","\n",
              filtrata_reg$ricoverati_con_sintomi," ricoverati","\n",
              filtrata_reg$terapia_intensiva," terpaia intensiva","\n",
              filtrata_reg$isolamento_domiciliare," isolamento","\n")
  
filtrata_reg %>%
  plot_ly(
    lat = ~lat,
    lon = ~long,
    marker = list(color = col,  size = 20),
    type = 'scattermapbox',
    mode="markers",
    hovertext = testo) %>%
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom =3.5,
      center = list(lon = 10.5, lat = 43))) %>%
  layout(plot_bgcolor='rgb(236, 240, 245)') %>% 
  layout(paper_bgcolor='rgb(236, 240, 245)') 




library(plotly)
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                           "Fruits", total.fruits, "Veggies", total.veggies,
                           "<br>", "Wheat", wheat, "Corn", corn))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'italy',
  projection = list(type = 'italy'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)




fig <- plot_geo(filtrata_reg, locationmode = 'Italy')
fig <- fig %>% add_trace(
  z = ~totale_attualmente_positivi, text = paste0(filtrata_reg$reg_name,":",filtrata_reg$totale_attualmente_positivi), locations = ~reg_name,
  color = ~totale_attualmente_positivi, colors = 'Purples'
)
fig <- fig %>% colorbar(title = "Millions USD")
fig <- fig %>% layout(
  title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)',
  geo = g
)

fig




df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_ebola.csv')
# restrict from June to September
df <- subset(df, Month %in% 6:9)
# ordered factor variable with month abbreviations
df$abbrev <- ordered(month.abb[df$Month], levels = month.abb[6:9])
# September totals
df9 <- subset(df, Month == 9)

# common plot options
g <- list(
  scope = 'africa',
  showframe = F,
  showland = T,
  landcolor = toRGB("grey90")
)

g1 <- c(
  g,
  resolution = 50,
  showcoastlines = T,
  countrycolor = toRGB("white"),
  coastlinecolor = toRGB("white"),
  projection = list(type = 'Mercator'),
  list(lonaxis = list(range = c(-15, -5))),
  list(lataxis = list(range = c(0, 12))),
  list(domain = list(x = c(0, 1), y = c(0, 1)))
)

g2 <- c(
  g,
  showcountries = F,
  bgcolor = toRGB("white", alpha = 0),
  list(domain = list(x = c(0, .6), y = c(0, .6)))
)

fig <- df %>% plot_geo(
  locationmode = 'country names', sizes = c(1, 600), color = I("black")
)
fig <- fig %>% add_markers(
  y = ~Lat, x = ~Lon, locations = ~Country,
  size = ~Value, color = ~abbrev, text = ~paste(Value, "cases")
)
fig <- fig %>% add_text(
  x = 21.0936, y = 7.1881, text = 'Africa', showlegend = F, geo = "geo2"
)
fig <- fig %>% add_trace(
  data = df9, z = ~Month, locations = ~Country,
  showscale = F, geo = "geo2"
)
fig <- fig %>% layout(
  title = 'Ebola cases reported by month in West Africa 2014<br> Source: <a href="https://data.hdx.rwlabs.org/dataset/rowca-ebola-cases">HDX</a>',
  geo = g1, geo2 = g2
)

fig






library(leaflet)
library(RColorBrewer)
mybins <- c(0,10,20,50,100,500,Inf)
mypalette <-c("#ffe4c4","#deb887","#ff7f50","#d2691e","#ff4500","#ff8c00","#dc143c","#ff0000","#b22222","#8b0000","#800000")


# Prepare the text for tooltips:
mytext <- paste(
  "Regione: ",d_reg$reg_name,"<br/>", 
  "Casi: ", d_reg$totale_attualmente_positivi, 
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map
leaflet(mappa_regioni) %>% 
  addTiles()  %>% 
  setView( lat=43, lng=10.5 , zoom=3.8) %>%
  addPolygons( 
    fillColor = ~mypalette(d_reg$totale_attualmente_positivi), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=d_reg$totale_attualmente_positivi, opacity=0.9, title = "Population (M)", position = "bottomleft" )

 
destinazione<-paste0(getwd(),"/world_shape_file.zip")
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile=destinazione)
system(paste0("unzip", destinazione))
library(rgdal)
world_spdf <- readOGR( 
  dsn= getwd() , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

# Clean the data object
library(dplyr)
world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)


data(quakes)
quakes <-  head(quakes, 100)
mybins <- seq(4, 6.5, by=0.5)
mypalette <- colorBin( palette="YlOrBr", domain=quakes$mag, na.color="transparent", bins=mybins)

# Prepare the text for the tooltip:


# Final Map
~mypalette(mag)
m <-  



m
%>%
  addLegend( pal=mypalette,values=~mag, opacity=0.9, title = "Magnitude", position = "bottomright" )

m 
"#deb887", #•burlywood

mypalette <-c("#ffffff","#ffe4c4","#faebd7","#ffe4e1","#ffb6c1","#ffa07a","#f08080","#ffc0cb","#dda0dd","#fa8072","#ee82ee",
              )

brewer.pal(n = 9, name = "Reds")[1]

for(i in 1:nrow(d_prov)){
  if(d_prov$totale_casi[i]==0){
    d_prov$colore[i]<-"#ffffff"
  }
  if(d_prov$totale_casi[i]<=10){
    d_prov$colore[i]<-brewer.pal(n = 9, name = "Reds")[1]
  }
  if(d_prov$totale_casi[i]>10 && d_prov$totale_casi[i]<=20 ){
    d_prov$colore[i]<-brewer.pal(n = 9, name = "Reds")[2]
  }
  if(d_prov$totale_casi[i]>20 && d_prov$totale_casi[i]<=50){
    d_prov$colore[i]<-brewer.pal(n = 9, name = "Reds")[3]
  }
  if(d_prov$totale_casi[i]>50 && d_prov$totale_casi[i]<=100){
    d_prov$colore[i]<-brewer.pal(n = 9, name = "Reds")[4]
  }
  if(d_prov$totale_casi[i]>100 && d_prov$totale_casi[i]<=150){
    d_prov$colore[i]<-brewer.pal(n = 9, name = "Reds")[5]
  }
  if(d_prov$totale_casi[i]>150 && d_prov$totale_casi[i]<=200){
    d_prov$colore[i]<-brewer.pal(n = 9, name = "Reds")[6]
  }
  if(d_prov$totale_casi[i]>200 && d_prov$totale_casi[i]<=400){
    d_prov$colore[i]<-brewer.pal(n = 9, name = "Reds")[7]
  }
  if(d_prov$totale_casi[i]>400 && d_prov$totale_casi[i]<=550){
    d_prov$colore[i]<-brewer.pal(n = 9, name = "Reds")[8]
  }
  if(d_prov$totale_casi[i]>550 && d_prov$totale_casi[i]<=750){
    d_prov$colore[i]<-brewer.pal(n = 9, name = "Reds")[9]
  }
  if(d_prov$totale_casi[i]>750 && d_prov$totale_casi[i]<=900){
    d_prov$colore[i]<-"#330107"
  }
  if(d_prov$totale_casi[i]>900 && d_prov$totale_casi[i]<=1400 ){
    d_prov$colore[i]<-"#140003"
  }
  if(d_prov$totale_casi[i]>1400 ){
    d_prov$colore[i]<-"#000000"
  }
  
}

Tempi<-cut(filtrata_prov$totale_casi,breaks = c(-1,0,10,20,50,100,150,300,500,750,1000,1500,3000,5000,10000,15000,20000))

mytext <- paste(
  "Provincia: ",filtrata_prov$denominazione_provincia,"<br/>",
  "Attualmente positivi: ", filtrata_prov$totale_casi, "<br/>" ) %>%
  lapply(htmltools::HTML)

m<-leaflet(filtrata_prov) %>% 
  addTiles()  %>% 
  setView( lat=42, lng=10.5 , zoom=4.5) %>%
  #addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~long, ~lat, 
                   fillColor = ~colore , fillOpacity = 1, color="white", radius=8, stroke=FALSE,
                   label = mytext,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  )
m

mypalette<-colorBin(filtrata_prov$colore,filtrata_prov$totale_casi)
data(quakes)
quakes <-  head(quakes, 100)
mypalette <- colorBin( palette="YlOrBr", domain=filtrata_prov$totale_casi, na.color="transparent")





d_naz
mytext <- paste(
  "Provincia: ",filtrata_prov$denominazione_provincia,"<br/>",
  "Attualmente positivi: ", filtrata_prov$totale_casi, "<br/>" ) %>%
  lapply(htmltools::HTML)



leaflet(filtrata_prov) %>% 
  addTiles()  %>% 
  setView( lat=42, lng=10.5 , zoom=4.5) %>%
  #addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~long, ~lat, 
                   fillColor = ~colore , fillOpacity = 1, color="white", radius=~radius, stroke=FALSE,
                   label = mytext,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  )
