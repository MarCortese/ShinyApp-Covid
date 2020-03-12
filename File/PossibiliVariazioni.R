



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