library(shiny)
library(lubridate)
source("./global.R")

shinyServer(function(input,output,session) {
  
  observeEvent(input$jumpToP1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
  })
  
  observeEvent(input$jumpToP2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  output$frame3 = renderUI({
    anioi = format(as.POSIXlt(Sys.time() - 86000, tz = "UTC"),"%Y")
    aniof = format(as.POSIXlt(Sys.time(), tz = "UTC"),"%Y")
    mesi = format(as.POSIXlt(Sys.time() - 86000, tz = "UTC"),"%m")
    mesf = format(as.POSIXlt(Sys.time(), tz = "UTC"),"%m")
    diai = format(as.POSIXlt(Sys.time() - 86000, tz = "UTC"),"%d")
    diaf = format(as.POSIXlt(Sys.time(), tz = "UTC"),"%d")
    horai = format(as.POSIXlt(Sys.time() - 86000, tz = "UTC"),"%H")
    horaf = format(as.POSIXlt(Sys.time(), tz = "UTC"),"%H")
    
    my_test = tags$iframe(src = paste0("https://www.ogimet.com/getbufr.php?ecenter=SKBO&type=ALL&ord=REV&ano=",anioi,
                                       "&mes=",mesi,"&day=",diai,"&hora=",horai,
                                       "&anof=",aniof,"&mesf=",mesf,"&dayf=",diaf,"&horaf=",horaf,
                                       "&res=list&enviar=Ver"),height=600, width=900)
    print(my_test)
  })

  conteoDiaVarA = reactive({
    conteoDiaVar2 = conteoDiaVar[which(conteoDiaVar$variable == input$variable),]
    conteoDiaVar2$anio_mes = sprintf("%s-%04d",month(conteoDiaVar2$diaCom, label = TRUE),year(conteoDiaVar2$diaCom))
    conteoDiaVar2 = conteoDiaVar2[which(conteoDiaVar2$anio_mes == input$anio_mes),]
    conteoDiaVar2$variable = NULL
    conteoDiaVar2$anio_mes = NULL
    return(conteoDiaVar2)
  })
  
  conteoDia_Temp = reactive({
    conteoDia2 = conteoDia
    conteoDia2$anio_mes = sprintf("%s-%04d",month(conteoDia2$diaCom, label = TRUE),year(conteoDia2$diaCom))
    conteoDia2 = conteoDia2[which(conteoDia2$anio_mes == input$anio_mes),]
    conteoDia2$anio_mes = NULL
    return(conteoDia2)
  })
  
  conteoDiaVariableEst2_Temp = reactive({
    conteoDiaVariableEst3 = conteoDiaVariableEst2
    conteoDiaVariableEst3$anio_mes = sprintf("%s-%04d",month(conteoDiaVariableEst3$diaCom, label = TRUE),year(conteoDiaVariableEst3$diaCom))
    conteoDiaVariableEst3 = conteoDiaVariableEst3[which(conteoDiaVariableEst3$anio_mes == input$anio_mes),]
    conteoDiaVariableEst3$anio_mes = NULL
    return(conteoDiaVariableEst3)
  })
  
  estBUFR = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo)
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  estBUFR_prec = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo[!is.na(conteoDiaVariableEst2_Temp()$prec)])
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  estBUFR_presion = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo[!is.na(conteoDiaVariableEst2_Temp()$presion)])
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  estBUFR_hum = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo[!is.na(conteoDiaVariableEst2_Temp()$hum)])
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  estBUFR_pRocio = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo[!is.na(conteoDiaVariableEst2_Temp()$pRocio)])
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  estBUFR_dirV = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo[!is.na(conteoDiaVariableEst2_Temp()$dirV)])
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  estBUFR_velV = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo[!is.na(conteoDiaVariableEst2_Temp()$velV)])
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  estBUFR_temp = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo[!is.na(conteoDiaVariableEst2_Temp()$temp)])
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  estBUFR_tempMax = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo[!is.na(conteoDiaVariableEst2_Temp()$tempMax)])
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  estBUFR_tempMin = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo[!is.na(conteoDiaVariableEst2_Temp()$tempMin)])
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  
  
  
  ## Elaboracion del mapa
  output$mapBUFR = renderLeaflet({
    estBUFR2 = estBUFR()
    
    estBUFR_prec2 = estBUFR_prec()
    estBUFR_presion2 = estBUFR_presion()
    estBUFR_hum2 = estBUFR_hum()
    estBUFR_pRocio2 = estBUFR_pRocio()
    estBUFR_dirV2 = estBUFR_dirV()
    estBUFR_velV2 = estBUFR_velV()
    estBUFR_temp2 = estBUFR_temp()
    estBUFR_tempMax2 = estBUFR_tempMax()
    estBUFR_tempMin2 = estBUFR_tempMin()
    
    
    pal = colorFactor(palette = c("blue", "red"),levels = c("SI","NO"))
    
    leaflet() %>% 
      addTiles(group = "Google Maps") %>%
      addProviderTiles("Esri.WorldImagery",group = "WorldImagery") %>%
      addProviderTiles(providers$Stamen.TonerLines,group = "WorldImagery") %>%
      addProviderTiles(providers$Stamen.TonerLabels,group = "WorldImagery") %>%
      addCircleMarkers(data = estBUFR2,group = "BUFR - AUTOMATICAS",
                                         lng = estBUFR2$longitud,lat = estBUFR2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      addCircleMarkers(data = estBUFR_prec2,group = "BUFR Prec",
                                         lng = estBUFR_prec2$longitud,lat = estBUFR_prec2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR_prec2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      addCircleMarkers(data = estBUFR_presion2,group = "BUFR Presion",
                                         lng = estBUFR_presion2$longitud,lat = estBUFR_presion2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR_presion2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      addCircleMarkers(data = estBUFR_hum2,group = "BUFR Humedad",
                                         lng = estBUFR_hum2$longitud,lat = estBUFR_hum2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR_hum2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      addCircleMarkers(data = estBUFR_pRocio2,group = "BUFR Punto Rocio",
                                         lng = estBUFR_pRocio2$longitud,lat = estBUFR_pRocio2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR_pRocio2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      addCircleMarkers(data = estBUFR_dirV2,group = "BUFR Direccion Viento",
                                         lng = estBUFR_dirV2$longitud,lat = estBUFR_dirV2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR_dirV2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      addCircleMarkers(data = estBUFR_velV2,group = "BUFR Velocidad Viento",
                                         lng = estBUFR_velV2$longitud,lat = estBUFR_velV2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR_velV2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      addCircleMarkers(data = estBUFR_temp2,group = "BUFR Temperatura",
                                         lng = estBUFR_temp2$longitud,lat = estBUFR_temp2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR_temp2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      addCircleMarkers(data = estBUFR_tempMax2,group = "BUFR Temp Max",
                                         lng = estBUFR_tempMax2$longitud,lat = estBUFR_tempMax2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR_tempMax2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      addCircleMarkers(data = estBUFR_tempMin2,group = "BUFR Temp Min",
                                         lng = estBUFR_tempMin2$longitud,lat = estBUFR_tempMin2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR_tempMin2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      AddSearchButton( group = "BUFR - AUTOMATICAS", zoom = 20,  # For add search box in the map.
                       textPlaceholder = "Search accesion name...") %>%
      addLayersControl(
        baseGroups = c("Google Maps","WorldImagery"),
        overlayGroups = c("BUFR - AUTOMATICAS","BUFR Prec","BUFR Presion","BUFR Humedad",
                          "BUFR Punto Rocio","BUFR Direccion Viento","BUFR Velocidad Viento",
                          "BUFR Temperatura","BUFR Temp Max","BUFR Temp Min"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("BUFR Prec","BUFR Presion","BUFR Humedad",
                  "BUFR Punto Rocio","BUFR Direccion Viento","BUFR Velocidad Viento",
                  "BUFR Temperatura","BUFR Temp Max","BUFR Temp Min")) %>% 
      leaflet::addLegend("bottomleft", 
                         pal = pal,
                         title = "Metadato en OSCAR/Surface",
                         values = estBUFR2$OSCAR,
                         opacity = 1)
  })

  ## Elaboracion del grafico de conteo de estaciones en el dia
  output$graficoCantidades = renderPlotly({
    y1 <- list(
      tickfont = list(color = "blue"),
      titlefont = list(color = "blue"),
      # overlaying = "y",
      side = "left",
      anchor="free",
      #position=1,
      title = "Conteo datos")
    
    y2 <- list(
      tickfont = list(color = "red"),
      titlefont = list(color = "red"),
      overlaying = "y",
      side = "right",
      # anchor="free",
      position=1,
      title = "Conteo estaciones")
    
    plotgraph = plot_ly(conteoDia_Temp(),x=~diaCom,y=~conteo,type = "bar",name = "Valor",marker = list(color = "cyan3")) %>% 
      add_trace(y=~Est,type = "scatter",yaxis = "y2",mode = 'lines+markers',type = "scatter",name = "Estación",marker = list(color = "red")) %>% 
      layout(legend = list(orientation = 'v',y = -0.2),
             yaxis =y1,
             yaxis2 = y2,
             xaxis = list(title = 'dia')
             #xaxis = list(title = list(font = list(color = "#ffffff")))
      )
  })
  
  ## Elaboracion del grafico de conteo de estaciones en el dia por cada variable
  output$graficoCantidadesVar = renderPlotly({
    y1 <- list(
      tickfont = list(color = "green"),
      titlefont = list(color = "green"),
      # overlaying = "y",
      side = "left",
      anchor="free",
      #position=1,
      title = "Conteo datos")
    
    y2 <- list(
      tickfont = list(color = "red"),
      titlefont = list(color = "red"),
      overlaying = "y",
      side = "right",
      # anchor="free",
      position=1,
      title = "Conteo estaciones")
    
    plot_ly(conteoDiaVarA(),x=~diaCom,y=~conteo,type = "bar",name = "Valor",marker = list(color = "green")) %>% 
      add_trace(y=~Est,type = "scatter",yaxis = "y2",mode = 'lines+markers',type = "scatter",name = "Estación",marker = list(color = "red")) %>% 
      layout(legend = list(orientation = 'v',y = -0.2),
             yaxis =y1,
             yaxis2 = y2,
             xaxis = list(title = 'dia')
             #xaxis = list(title = list(font = list(color = "#ffffff")))
      )
    # print(plotgrapha)
  })
  
  
  output$graficoQualityWDQMS = renderPlotly({
    
  })
  
  ## Se genera grafica y visualizacion del metadato de la estacion seleccionada en el mapa
  observeEvent(input$mapBUFR_marker_click,{
    codigoZ = input$mapBUFR_marker_click$id # El codigo viene de la dinamizacion con el mapa
    
    CodTemp = reactive({
      temp = conteoDiaVariableEst2_Temp()[which(conteoDiaVariableEst2_Temp()$codigo == codigoZ),]
      return(temp)
    })
    
    output$text <- renderText({
      paste("Estación ",CNE_IDEAM$nombre[which(CNE_IDEAM$CODIGO == codigoZ)])
    })
    
    output$text2 <- renderText({
      paste("Metadato para la Estación ",CNE_IDEAM$nombre[which(CNE_IDEAM$CODIGO == codigoZ)])
    })
    
    
    ## Elaboracion del grafico de seguimiento de variables por estacion, una vez dandole click a la estacion en el lienzo del mapa    
    output$graficoEstBUFR = renderPlotly({
      graph = plot_ly(CodTemp(),x = ~diaCom,y = ~prec,name = "precipitacion",type = 'scatter',mode = 'lines+markers') %>% 
        add_trace(y = ~dirV,name = "direccion del viento") %>% 
        add_trace(y = ~velV,name = "velocidad del viento") %>% 
        add_trace(y = ~hum,name = "humedad") %>% 
        add_trace(y = ~presion,name = "presion") %>% 
        add_trace(y = ~temp,name = "temperatura") %>%
        add_trace(y = ~tempMax,name = "temperatura maxima") %>% 
        add_trace(y = ~tempMin,name = "temperatura minima") %>%
        add_trace(y = ~pRocio,name = "punto de rocio") %>%
        layout(legend = list(orientation = 'h',x = 0.5,xanchor = "center"),
               yaxis = list(title = 'Cantidad datos'),
               xaxis = list(title = list(font = list(color = "#ffffff"))))#,
               # legend = list(orientation = 'h',y = 1.13))
    })
    
    ## Despliegue del metadato de la estacion seleccionada en el mapa, desde OSCAR - SURFACE
    output$frame = renderUI({
      my_test = tags$iframe(src = paste0("https://oscar.wmo.int/surface/#/search/station/stationReportDetails/0-170-0-",codigoZ),height=500, width=1050)
      print(my_test)
    })
    
  })
  
  ################################
  ############ WDQMS #############
  ################################
  
  observeEvent(input$preview4,{
    shinyalert("Importante!","Las gráficas de barras apiladas indican por mes y por variable el porcentaje de estaciones en cada rango de calidad de los datos registrados que aparecen en la página 
                              web de WDQMS, utilizando el valor de calidad mas alto de los cuatro Centros de Monitoreo (DWD, ECMWF, JMA, NCEP).\n 
                              Están acondicionadas por los filtros de AÑO, MES y VARIABLE.")
  })
  
  observeEvent(input$preview5,{
    shinyalert("Importante!","La tabla enlista las estaciones desplegadas en WDQMS y sus correspondientes valores absolutos de diferencias de calidad por cada Centro de Monitoreo (DWD, ECMWF, JMA, NCEP). 
                              Así mismo, se muestra la diferencia mas alta de los 4 Centros de Monitoreo, clasificadas con los colores de calidad que aparecen en la página Web de WDQMS.\n 
                              La actualización de la tabla está acondicionada por el filtro de fecha.")
  })
  
  observeEvent(input$preview6,{
    shinyalert("Importante!","La tabla enlista para cada día del mes de interés, sus correspondientes valores absolutos de diferencias de calidad por cada Centro de Monitoreo (DWD, ECMWF, JMA, NCEP).
                              Así mismo, se muestra la diferencia mas alta de los 4 Centros de Monitoreo, clasificadas con los colores de calidad que aparecen en la página Web de WDQMS.\n 
                              La actualización de la tabla está acondicionada por el filtro de Estación.")
  })
  
  output$frame2 = renderUI({
    my_test = tags$iframe(src = "https://wdqms.wmo.int/nwp/land_surface",height=800, width=1000)
    print(my_test)
  })
  
  Quality_WDQMS = reactive({
    QualityA = pivotFinalTemp[which(pivotFinalTemp$anio == input$anio_WDQMS & 
                                      pivotFinalTemp$mes == input$mes_WDQMS & 
                                      pivotFinalTemp$variable == input$variable_WDQMS),]
    return(QualityA)
  })
  
  Availability_WDQMS = reactive({
    QualityB = pivot_availability[which(pivot_availability$anio == input$anio_WDQMS & 
                                        pivot_availability$mes == input$mes_WDQMS & 
                                        pivot_availability$variable == input$variable_WDQMS &
                                        pivot_availability$center == input$center),]
    return(QualityB)
  })
  
  Quality_Centers = reactive({
    qualityCentersA = qualityCenters[which(qualityCenters$anio == input$anio_WDQMS &
                                             qualityCenters$mes == input$mes_WDQMS &
                                             qualityCenters$variable == input$variable_WDQMS),]
    return(qualityCentersA)
  })
  
  observe({
    updateSelectInput(session, inputId = "selFechas",label = "Seleccione Fecha", 
                      choices = c(unique(Quality_Centers()$date)))
  })
  
  Quality_CentersDate = reactive({
    LL = Quality_Centers()
    AAA = LL[LL$date == input$selFechas,]
    AAA$variable = NULL;AAA$anio = NULL;AAA$mes = NULL;
    AAA$date = NULL;AAA$wigosid = NULL
    return(AAA)
  })
  
  observe({
    updateSelectInput(session, inputId = "selEstacion",label = "Seleccione Estacion", 
                      choices = c(unique(Quality_Centers()$name)))
  })
  
  Quality_CentersName = reactive({
    BBB = Quality_Centers()[Quality_Centers()$name == input$selEstacion,]
    BBB$variable = NULL;BBB$anio = NULL;BBB$mes = NULL;BBB$wigosid = NULL;BBB$name = NULL
    return(BBB)
  })
  
  #output$table = renderTable(Quality_CentersDate())
  
  output$Qwdqms = renderPlotly({
    # AA = Quality_WDQMS()
    plot_ly(Quality_WDQMS(),x = ~fecha,y = ~Rojo_porc,name = "> 10" , type = 'bar',text = ~paste0("Porcentaje: ",Rojo_porc,"% \n Total estaciones: ",Rojo_ests),hoverinfo = 'text', marker = list(color = "rgb(253, 29, 10)")) %>% 
      add_trace(y = ~Naranja_porc,yaxis = "y1", type = 'bar',name = "5 < x <= 10",text = ~paste0("Porcentaje: ",Naranja_porc,"% \n Total estaciones: ",Naranja_ests),hoverinfo = 'text',marker = list(color = "rgb(253, 180, 10)")) %>% 
      add_trace(y = ~Amarillo_porc,yaxis = "y1", type = 'bar',name = "1 < x <= 5",text = ~paste0("Porcentaje: ",Amarillo_porc,"% \n Total estaciones: ",Amarillo_ests),hoverinfo = 'text',marker = list(color = "rgb(248, 250, 120)")) %>% 
      add_trace(y = ~VerdeClaro_porc,yaxis = "y1", type = 'bar',name = "0.5 < x <= 1",text = ~paste0("Porcentaje: ",VerdeClaro_porc,"% \n Total estaciones: ",VerdeClaro_ests),hoverinfo = 'text',marker = list(color = "rgb(57, 249, 10)")) %>% 
      add_trace(y = ~Verde_porc,yaxis = "y1", type = 'bar',name = "<= 0.5",text = ~paste0("Porcentaje: ",Verde_porc,"% \n Total estaciones: ",Verde_ests),hoverinfo = 'text',marker = list(color = "rgb(8, 128, 41)")) %>% 
      add_trace(y = ~mean,type = "scatter",yaxis = "y2",mode = 'lines+markers',type = "scatter",name = "Promedio",marker = list(color = "red")) %>%
      add_trace(y = ~median,type = "scatter",yaxis = "y2",mode = 'lines+markers',type = "scatter",name = "Mediana",marker = list(color = "blue")) %>%
      layout(barmode = 'stack',
             legend = list(orientation = 'v',y = -0.2),
             yaxis2 = list(overlaying = "y", side = "right",title = "Media-Mediana Calidad"),
             yaxis = list(title = 'Porcentaje Calidad (%)'))
    # print(graph)
  })
  
  output$Awdqms = renderPlotly({
    # AA = Quality_WDQMS()
    plot_ly(Availability_WDQMS(),x = ~date,y = ~yellow_porc,name = "No match in OSCAR/Surface" , type = 'bar',text = ~paste0("Porcentaje: ",yellow_porc,"% \n Total estaciones: ",yellow_count),hoverinfo = 'text', marker = list(color = "rgb(233, 250, 32)")) %>% 
      add_trace(y = ~grey_porc,yaxis = "y1", type = 'bar',name = "OSCAR schedule issue",text = ~paste0("Porcentaje: ",grey_porc,"% \n Total estaciones: ",grey_count),hoverinfo = 'text',marker = list(color = "rgb(209, 210, 199)")) %>% 
      add_trace(y = ~black_porc,yaxis = "y1", type = 'bar',name = "Not received in period",text = ~paste0("Porcentaje: ",black_porc,"% \n Total estaciones: ",black_count),hoverinfo = 'text',marker = list(color = "rgb(0, 0, 0)")) %>% 
      add_trace(y = ~red_porc,yaxis = "y1", type = 'bar',name = "Availability issues (< 30%)",text = ~paste0("Porcentaje: ",red_porc,"% \n Total estaciones: ",red_count),hoverinfo = 'text',marker = list(color = "rgb(255, 0, 0)")) %>% 
      add_trace(y = ~orange_porc,yaxis = "y1", type = 'bar',name = "Availability issues (≥ 30%)",text = ~paste0("Porcentaje: ",orange_porc,"% \n Total estaciones: ",orange_count),hoverinfo = 'text',marker = list(color = "rgb(255, 158, 0)")) %>% 
      add_trace(y = ~green_porc,yaxis = "y1", type = 'bar',name = "Normal (≥ 80%)",text = ~paste0("Porcentaje: ",green_porc,"% \n Total estaciones: ",green_count),hoverinfo = 'text',marker = list(color = "rgb(74, 212, 34)")) %>% 
      add_trace(y = ~purple_porc,yaxis = "y1", type = 'bar',name = "More than 100%",text = ~paste0("Porcentaje: ",purple_porc,"% \n Total estaciones: ",purple_count),hoverinfo = 'text',marker = list(color = "rgb(246, 55, 182)")) %>%
      layout(barmode = 'stack',
             legend = list(orientation = 'v',y = -0.2),
             # yaxis2 = list(overlaying = "y", side = "right",title = "Media-Mediana Calidad"),
             yaxis = list(title = 'Porcentaje Disponibilidad (%)'))
    # print(graph)
  })
  
  output$tFechas = renderDataTable({datatable(Quality_CentersDate(),options = list(scrollX = T,scrollY = T,pageLength = 15,paging = FALSE),rownames = F) %>% 
      formatStyle("DifMax",
                  backgroundColor = styleInterval(c(0.5,1,5,10,1000),
                                                  c("rgb(8, 128, 41)",
                                                    "rgb(57, 249, 10)",
                                                    "rgb(248, 250, 120)",
                                                    "rgb(253, 180, 10)",
                                                    "rgb(253, 29, 10)",
                                                    "rgb(253, 29, 10)")))
  })
  
  output$tEstacion = renderDataTable({datatable(Quality_CentersName(),options = list(scrollX = T,scrollY = T,pageLength = 15,paging = FALSE),rownames = F) %>% 
      formatStyle("DifMax",
                  backgroundColor = styleInterval(c(0.5,1,5,10,1000),
                                                  c("rgb(8, 128, 41)",
                                                    "rgb(57, 249, 10)",
                                                    "rgb(248, 250, 120)",
                                                    "rgb(253, 180, 10)",
                                                    "rgb(253, 29, 10)",
                                                    "rgb(253, 29, 10)")))
  })
  
})