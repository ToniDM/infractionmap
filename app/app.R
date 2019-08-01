# Data visualization contest with R: first edition
# Shiny app - Map of infractions
# Antonio Diaz

#----------------------------------------------------------------------
# Load - Data and Packages
#----------------------------------------------------------------------
#Load packages
library(shiny) #shiny
library(shinyWidgets) #UI Widgets
library(data.table) #Work with data
library(leaflet) #Create map
library(lubridate) #Work with date/time

#Load data
dd = readRDS('data/dd.rds')

#----------------------------------------------------------------------
# UI
#----------------------------------------------------------------------
ui = navbarPage('1st data visualization contest with R: fines register in Gijón from years 2015 to 2017',id='nav',
                #Map
                tabPanel('Interactive map',
                         div(class="outer",
                             tags$head(
                               # Include our custom CSS
                               includeCSS("styles.css")),
                             
                             leafletOutput("mymap", height = "100%"),
                             
                             absolutePanel(id = "controls", class = "panel panel-default",
                                           fixed = TRUE,
                                           draggable = TRUE, 
                                           top = 80, 
                                           left = "auto", 
                                           right = 20, 
                                           bottom = "auto",
                                           width = 330, height = "auto",
                                           
                                           h3("Filters"),
                                           sliderInput('fechas',label='Period selection',min=min(date(dd$date)),max=max(date(dd$date)),value=c(min(date(dd$date)),max(date(dd$date)))),
                                           sliderInput('hora',label='Time range',min=0,max=24,value=c(0,24),post='h'),
                                           pickerInput('tipo', label = "Type of infraction", choices = sort(unique(dd$type2)), options = list(`actions-box` = TRUE),multiple = T, selected = unique(dd$type2)),
                                           checkboxGroupButtons(
                                             inputId = "calif",
                                             label = "Infraction calification",
                                             choices = c('Minor','Major','Serious'),
                                             selected= c('Minor','Major','Serious'),
                                             justified = TRUE,
                                             checkIcon = list(yes = icon("ok",lib = "glyphicon"))
                                           ),
                                           sliderInput('importe',label='Fee',min=min(dd$fee),max=max(dd$fee),value=c(min(dd$fee),max(dd$fee)),post='€'),
                                           sliderInput('punt',label='Points',min=min(dd$points),max=max(dd$points),value=c(min(dd$points),max(dd$points))),
                                           
                                           h4(textOutput("num")," infractions selected"),
                                           h4(textOutput("dinero"),"€ of fees"),
                                           h4(textOutput("pu"),"points lost")
                             ),
                             
                             tags$div(id="cite",
                                      tags$a(href = "https://rusersasturias.github.io/contest/", "'Data visualization contest with R: first edition '"), ' by Antonio Diaz (April, 2019).   ',position = 'right')
                         )
                         
                ),
                #Help page
                tabPanel('Help',
                         h4("Interactive map of fines register in Gijon from years 2015 to 2017"),
                         helpText("User can apply the following filters:",br(),
                                  '-Period selection',br(),
                                  '-Time range (from 0h to 24h)',br(),
                                  '-Type of infraction',br(),
                                  '-Calification of infraction',br(),
                                  '-Fee',br(),
                                  '-Points',br(),br(),
                                  'Every single point in the map, contains a tooltip with information of:',br(),
                                  '-Number of fines',br(),
                                  '-Type of infraction (more usual)',br(),
                                  '-Average fee',br(),
                                  '-Average discount',br(),
                                  '-Average points',br(),
                                  '-If type is speed, speed and speeding in that point',br(),br(),
                                  'From original dataset provided with 183212 fees, some are excluded due to:',br(),
                                  '-Coordinates were not found (even in google API) - 45 cases',br(),
                                  '-Location out of Asturias - 5 cases',br(),
                                  '-No information of place/street in dataset - 19 cases',br(),  
                                  'Finally 183.143 infractions represented',br(),br(),
                                  'Data preprocessing was done for:',br(),
                                  '-Enrich latitude and longitude fields, filling missing values using Google map API',br(),
                                  '-Create a new more detailed classification of infractions based on texts from explicative field. Resulting in 13 categories',br(),br(),
                                  'Category description:',br(),
                                  '- alcohol: drive under the influence of drugs or alcohol',br(),
                                  '- distraction: use of mobile phone or other not allowed devices',br(),
                                  '- driving: drive recklessly',br(),
                                  '- lane change: change lane in inappropriate moment or place',br(),
                                  '- no collaboration: do not show documentation to authorities or do not collaborate with them',br(),
                                  '- other',br(),
                                  '- overtaking: in a not allowed place',br(),
                                  '- parking: in a not allowed place(or exceed time)',br(),
                                  '- restricted area: access to a restricted area without permission',br(),
                                  '- seat belt: not use the seat belt',br(),
                                  '- sign post: omit a traffic signal',br(),
                                  '- speed: exceed speed limits',br(),
                                  '- traffic light: omit a traffic light',br()
                         ),br(),br(),br(),
                         h4("Mapa interactivo que representa la ubicacion de las multas ocurridas en gijon entre 2015 y  2017"),
                         helpText("El usuario puede aplicar los siguientes filtros:",br(),
                                  '-Fecha',br(),
                                  '-Tramo horario',br(),
                                  '-Tipo de multa',br(),
                                  '-Gravedad de la multa',br(),
                                  '-Importe de sancion',br(),
                                  '-Puntos de sancion',br(),br(),
                                  'En cada punto señalado en el mapa se muestra la siguiente informacion:',br(),
                                  '-Numero de multas',br(),
                                  '-Tipo de multa más habitual en ese punto',br(),
                                  '-Promedio de importe de sancion',br(),
                                  '-Promedio de descuento aplicado',br(),
                                  '-Promedio de puntos perdidos',br(),
                                  '-Si es de tipo velocidad, velocidad y exceso de velocidad medios en ese punto',br(),br(),
                                  'Partiendo de la base original de 183212 multas proporcionadas para la competicion, se excluyen:',br(),
                                  '-45 por no lograr extraer coordenadas',br(),
                                  '-5 por obtener una ubicacion fuera de asturias',br(),
                                  '-19 por no estar informado el lugar de la infraccion',br(),  
                                  'Resultando 183.143 infracciones representadas en el mapa',br(),br(),
                                  'Se realiza un preprocesado de los datos donde:',br(),
                                  '-Se enriquecen los campos longitud y latitud, añadiendo los valores faltantes haciendo uso de la API de google maps',br(),
                                  '-Se crea una nueva clasificacion de multas mas detallada en base a los textos explicativos, con 13 categorias',br(),br(),
                                  'Descripcion de categorias:',br(),
                                  '- alcohol - alcohol: conducir bajo la influencia de alcohol o drogas',br(),
                                  '- distraction - distracción: usar un telefono movil u otro dispositivo no permitido',br(),
                                  '- driving - conduccion: conducir de manera temeraria ',br(),
                                  '- lane change - cambio de carril: cambiar de carril en un lugar o momento no permitido',br(),
                                  '- no collaboration - no colaboracion: no mostrar documentacion solicitada a las autoridades o no colaborar',br(),
                                  '- other - otros',br(),
                                  '- overtaking - adelantamiento: realizar un adelantamiento en un lugar no permitido',br(),
                                  '- parking - aparcamiento: aparcar en una zona no permitida (o exceder el tiempo)',br(),
                                  '- restricted area - area restringida a vehiculos: acceder a una zona restringida sin permiso',br(),
                                  '- seat belt - cinturon de seguridad: no usar el cinturon de seguridad',br(),
                                  '- sign post - señalizacion: hacer caso omiso de señales de trafico',br(),
                                  '- speed - velocidad: exceder los limites de velocidad',br(),
                                  '- traffic light - semoforo: hacer caso omiso de señales luminosas',br()
                                  ),
                         
                                  h5(tags$a(href = "https://rusersasturias.github.io/contest/", "'Data visualization contest with R: first edition '"), ' by Antonio Diaz (April, 2019).   ',position = 'right')
                         
                         )
)

#----------------------------------------------------------------------
# Server
#----------------------------------------------------------------------
server = function(input, output, session) {
  
  #Mode calculation function
  moda = function(x) {
    ux = unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  #Color palette by type
  pal <- colorFactor(
    palette = c('black','hotpink','orange','brown','yellow','green','purple','blue','red','cyan','maroon','yellowgreen','burlywood4'),
    domain = unique(dd$type2)
  )
  pal2 <- colorFactor(
    palette = c('black','black','black','black','black','black','black','black','black','black','black','black','black'),
    domain = unique(dd$type2)
  )
  
  #Map
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
      ) %>%
      setView(lng = -5.660906, lat = 43.532661, zoom = 14)
  })
  
  # This observer is responsible for maintaining the circles,totals and legend, according to the selection the user has chosen
  observe({
    fecha = input$fechas
    hora = input$hora
    tipo = input$tipo
    euro = input$importe
    punto = input$punt
    cali = input$calif
    
    #Create grouped table by position
    resumen = dd[between(date(dd$date),fecha[1],fecha[2]) & 
                   between(hour(dd$date),hora[1],hora[2]) & 
                   between(fee,euro[1],euro[2]) &
                   between(points,punto[1],punto[2]) &
                   calification%in%cali &
                   type2%in%tipo,.(multas=.N,
                                   tipo=moda(type2),
                                   euros=round(mean(fee,na.rm=TRUE),1),
                                   euros_dis=round(mean(disccount,na.rm=TRUE),1),
                                   puntos = round(mean(points,na.rm=TRUE),1),
                                   velocidad = round(mean(speed_num,na.rm=TRUE),1),
                                   velocidad_lim = round(mean(speed_limit_num,na.rm=TRUE),1),
                                   velocidad_ex = round(mean(speed_num,na.rm=TRUE)-mean(speed_limit_num,na.rm=TRUE),1),
                                   numero = first(number),
                                   lugar = first(place)),
                 by=.(latitude,longitude)]
    
    #Tooltip information
    content = paste(sep="<br/>",
                    paste('<b>',resumen[,lugar],", ",resumen[,numero],'</b>'),
                    paste('Type of fine (more usual): ',resumen[,tipo]),
                    paste('Fines: ',resumen[,multas]),
                    paste('Average fee: ',resumen[,euros],'€'),
                    paste('Average discount: ',resumen[,euros_dis],'€'),
                    paste('Average points: ',resumen[,puntos]),
                    ifelse(resumen[,tipo]=='speed',paste('Average speed: ',resumen[,velocidad],'km/h'),""),
                    ifelse(resumen[,tipo]=='speed',paste('Average speeding: ',resumen[,velocidad_ex],'km/h'),""))
    
    #Totals
    output$num = renderText({sum(resumen[,multas])})
    output$dinero = renderText({sum(resumen[,euros])})
    output$pu = renderText({sum(resumen[,puntos])})
    
    #Add circles
    leafletProxy('mymap')%>%
      clearShapes() %>%
      addCircles(lng = resumen[,longitude],
                 lat = resumen[,latitude], 
                 radius = resumen[,multas]/max(resumen[,multas])*100+10,
                 popup=content,
                 color = pal(resumen[,tipo]),
                 stroke=FALSE,
                 fillOpacity = 0.6,
                 group = 'color') %>%
      addLegend("bottomleft", group = "color", pal = pal, layerId="colorLegend", values = resumen[,tipo],
                title = "Infraction",opacity = 0.6)
  })
  
}

shinyApp(ui, server)