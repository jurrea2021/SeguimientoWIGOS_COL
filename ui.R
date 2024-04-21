library(shiny)
library(shinydashboard)

# my_theme = create_theme(
#   adminlte_color(
#     light_blue = "#E05227"
#   )
# )

shinyUI(fluidPage(
  theme = shinytheme("united"),
  navbarPage(id = "inTabset",
             windowTitle = "WIGOS Colombia",
             title = div(tags$a(img(src="wigos_components_web-01_0.png", height=55), href= "http://www.ideam.gov.co/"),
                         style = "position: relative; top: -16px;"),
  tabPanel(icon = icon("home"),title = strong(" "),
           fluidRow(
             column(2,
                    div(tags$a(img(src="349742861_230889493006896_8861073658765929231_n.jpg", height=200), href= "http://dhime.ideam.gov.co/webgis/home/"),
                         style = "position: relative; top: -16px;")),
             column(8,
                    br(),
                    br(),
                    br(),
                    p(strong("IMPLEMENTACIÓN DE ",shiny::span(strong("WIGOS"),style = "color:red")," EN EL IDEAM"), style = "font-size:50px;text-align : center")
             ),
             column(2,
                    div(tags$a(img(src="271749354_287578106737301_2016233505493951851_n.png", height=200), href= "https://wmo.int/es"),
                        style = "position: relative; top: -16px;"))
           ),
           fluidRow(
             column(4,
                    p("WIGOS (WMO Integrated Global Observing System) es un marco general para la coordinación y evolución de todos los sistemas de Observación y contribuciones de la OMM.",style = "text-align:justify"),
                    p("Entre estos sistemas de observación, se encuentran los Sistema Mundial de Observación del Clima (GCOS), Global Observing System (GOS), el WMO Hydrological Observing System (WHOS)",style = "text-align:justify"),
                    p("El cual el Instituto de Meteorología, Hidrología y Estudios Ambientales (IDEAM), tiene su propia red de estaciones hidrometeorológicas en Colombia y se encuentra registrada a algunos sistemas de obervación",style = "text-align:justify"),
                    p("La implementación WIGOS requiere una estrecha coordinación a nivel nacional, regional y mundial. El apoyo de los centros regionales pertinentes es fundamental para garantizar que 
                      los Miembros, a través de sus puntos focales nacionales, conozcan y puedan implementar el WIGOS de conformidad con las prácticas y los procedimientos enumerados en los Reglamentos Técnicos",style = "text-align:justify"),
                    p("El IDEAM, de acuerdo a las políticas de WIGOS, mencionadas en el OMM N° 1160, debe implementar algunas consideraciones a su red de estaciones, en torno al intercambio internacional de datos, lo cual favorece la
                      predicción meteorológica numérica mundial (NWP), realizados por sistemas mundiales que dependen del acceso de los conjuntos de observaciones coherentes, siendo proporcionados por los sistemas de observación
                      como los basados en la superficie y que son administrados por los Servicios Meteorológicos e Hidrológicos Nacionales que son miembros de la OMM; siendo el IDEAM miembro activo (Mundial, Manual del Sistema
                      mundial integrado de sistemas de observación de la OMM (OMM-N° 1160): Anexo VIII del Reglamento técnico de la OMM, 2021).",style = "text-align:justify")
                    ),
             column(4,
                    p("El IDEAM al ser un Servicio o Centro Meteorológico Nacional (NMC), hace parte de la red integrada para la recopilación, el intercambio y la distribución de información a nivel mundial - 
                      Sistema Mundial de Telecomunicaciones (SMT); con miras a satisfacer, de manera eficiente y eficaz, las necesidades de todos los Servicios Meteorológicos Nacionales y también las necesidades 
                      de los Servicios Meteorológicos Mundiales y Regionales. Centros Meteorológicos Especializados",style = "text-align:justify"),
                    p("El SMT es una red integrada de enlaces de telecomunicaciones terrestres y satelitales de circuitos punto a punto y circuitos multipunto, que interconectan centros de telecomunicaciones 
                      meteorológicas operados por países para transmisiones confiables las 24 horas del día y casi en tiempo real. recopilación y distribución de todos los datos, pronósticos y alertas 
                      meteorológicos y relacionados. Esta red de comunicación segura permite el intercambio de información en tiempo real, fundamental para la previsión y alerta de peligros hidrometeorológicos.",style = "text-align:justify"),
                    br(),
                    div(tags$a(img(src="GTS.png", height=400,width = 600)),style = "position: relative; top: -16px;")
                    ),
             column(4,
                    p("WIGOS dispone de un conjunto de herramientas y sistemas que facilitan el seguimiento de las redes de observación terrestre en el mundo. La Subdirección de Hidrología del IDEAM desde el Grupo de Planeación
                       Operativa, viene liderando desde mediados del año 2021 la implementación técnica y el funcionamiento de estas herramientas, enfocadas a la red de estaciones automáticas y sinópticas del Instituto, en
                       colaboración de la Subdirección de Meteorología y de la Oficina de Infromática del IDEAM. ",style = "text-align:justify"),
                    p("Las herramientas de WIGOS implementadas en el IDEAM, corresponden a la actualización y usabilidad de las herramientas web de OSCAR/Surface y WDQMS; así como la implementación del sistema de transmisión de
                       datos de estaciones hidrometeorológicas del IDEAM al Centro Regional de Transmisión (CRT) de Brasilia, mediante mensajes BUFR; y finalmente la implementación de la nueva codificación de estaciones propuesta por WIGOS.",style = "text-align:justify"),
                    p("En la presente herramienta se puede hacer el seguimiento al estado de las herramientas WIGOS a nivel NACIONAL. En los siguientes botones se accede a los seguimeintos para WDQMS y para BUFR:"),
                    div(actionButton('jumpToP1',strong('WDQMS - WIGOS Data Quality Monitoring System'),style="color: white; background-color: #E05227; border-color: #18BC9C"),align ="center"),
                    br(),
                    div(actionButton('jumpToP2',strong('BUFR - OSCAR SURFACE'),style="color: white; background-color: #E05227; border-color: #18BC9C"),align ="center")
                    )
           )
           ),
  tabPanel(strong("SEGUIMIENTO WDQMS"),value = "panel1",
           tabsetPanel(
             tabPanel(strong("PÁGINA WDQMS"),
                      column(3,
                             p("La implementación WIGOS requiere una estrecha coordinación a nivel nacional, regional y mundial. 
                               El apoyo de los centros regionales pertinentes es fundamental para garantizar que los Miembros, 
                               a través de sus puntos focales nacionales, conozcan y puedan implementar el WIGOS de conformidad 
                               con las prácticas y los procedimientos enumerados en los Reglamentos Técnicos.")
                      ),
                      column(9,
                             htmlOutput("frame2")
                      )
             ),
             tabPanel(strong("WDQMS COLOMBIA"),
                      dashboardPage(skin = "red",
                                    dashboardHeader(title = "MENU",disable = F),
                                    dashboardSidebar(
                                      selectInput(inputId = 'anio_WDQMS',
                                                  label = 'ANIO',
                                                  choices = c("2023" = 2023,
                                                              "2024" = 2024
                                                  )),
                                      selectInput(inputId = 'mes_WDQMS',
                                                  label = 'MES',
                                                  choices = c("enero" = "enero",
                                                              "febrero" = "febrero",
                                                              "marzo" = "marzo",
                                                              "abril" = "abril",
                                                              "mayo" = "mayo",
                                                              "junio" = "junio",
                                                              "julio" = "julio",
                                                              "agosto" = "agosto",
                                                              "septiembre" = "septiembre",
                                                              "octubre" = "octubre",
                                                              "noviembre" = "noviembre",
                                                              "diciembre" = "diciembre"
                                                  )),
                                      selectInput(inputId = 'variable_WDQMS',
                                                  label = 'VARIABLE',
                                                  choices = c("Presion" = "pressure",
                                                              "Temperatura" = "temperature",
                                                              "Viento Zonal" = "zonal wind",
                                                              "Viento Meridional" = "meridional wind",
                                                              "Humedad" = "humidity"
                                                  ))
                                    ),
                                    dashboardBody(#use_theme(my_theme),
                                      tabsetPanel(
                                        tabPanel(strong("CALIDAD"),
                                                 br(),
                                                 fluidRow(
                                                   column(1,
                                                          botonAyuda("preview4")
                                                          ),
                                                   column(10,plotlyOutput('Qwdqms')),
                                                   column(1)
                                                 ),
                                                 fluidRow(
                                                   br(),
                                                   column(6,
                                                          box(width = 12,
                                                              fluidRow(
                                                                column(6,
                                                                       selectInput(inputId = "selFechas",
                                                                                   label = "Seleccione Fecha",
                                                                                   choices = NULL)
                                                                       ),
                                                                column(3),
                                                                column(3,
                                                                       botonAyuda("preview5")
                                                                       )
                                                              ),
                                                              
                                                              div(dataTableOutput("tFechas"),style = "font-size:80%; overflow-y: scroll;height:500px"))
                                                   ),
                                                   column(6,
                                                          box(width = 12,
                                                              fluidRow(
                                                                column(7,
                                                                       selectInput(inputId = "selEstacion",
                                                                                   label = "Seleccione Estacion",
                                                                                   choices = NULL)
                                                                       ),
                                                                column(2),
                                                                column(3,
                                                                       botonAyuda("preview6")
                                                                       )
                                                              ),
                                                              
                                                              div(dataTableOutput("tEstacion"),style = "font-size:80%; overflow-y: scroll;height:500px"))
                                                   )
                                                 )
                                                 ),
                                        tabPanel(strong("DISPONIBILIDAD"),
                                                 fluidRow(
                                                   br(),
                                                   column(1,
                                                          botonAyuda("preview7")
                                                          ),
                                                   column(10,
                                                          selectInput(inputId = 'center',
                                                                      label = 'CENTRO PRONOSTICO',
                                                                      choices = c("ECMWF" = "ECMWF",
                                                                                  "JMA" = "JMA",
                                                                                  "DWD" = "DWD",
                                                                                  "NCEP" = "NCEP")
                                                                      ),
                                                          plotlyOutput('Awdqms')),
                                                   column(1)
                                                 )
                                                 )
                                      )
                                      
                                    )
                      )
             )
           )
  ),
  
  tabPanel(strong("SEGUIMIENTO BUFR"),value = "panel2",
           tabsetPanel(
             tabPanel(strong("Evidencia transmision BUFR"),
                      column(4,
                             
                             ),
                      column(8,
                             htmlOutput("frame3")
                             )
                      ),
             tabPanel(strong("BUFR Estaciones Automaticas"),
                      fluidRow(
                        #br(),
                        column(6,
                               fluidRow(
                                 column(2,
                                        br(),
                                        botonAyuda("preview1")
                                        ),
                                 column(8),
                                 column(2,
                                        br(),
                                        botonAyuda("preview2")
                                        )
                               ),
                               selectInput(inputId = 'anio_mes',
                                           label = 'PERIODO DE SEGUIMIENTO',
                                           choices = c("Octubre de 2023" = "oct-2023",
                                                       "Noviembre de 2023" = "nov-2023",
                                                       "Diciembre de 2023" = "dic-2023",
                                                       "Enero de 2024" = "ene-2024",
                                                       "Febrero de 2024" = "feb-2024",
                                                       "Marzo de 2024" = "mar-2024",
                                                       "Abril de 2024" = "abr-2024"
                                           )),
                               tabsetPanel(
                                 tabPanel(strong("Nacional"),
                                          plotlyOutput('graficoCantidades')
                                 ),
                                 tabPanel(strong("Variable"),
                                          selectInput(inputId = 'variable',
                                                      label = 'VARIABLE',
                                                      choices = c("Precipitacion" = "prec",
                                                                  "Presion" = "presion",
                                                                  "Temperatura" = "temp",
                                                                  "Temperatura Maxima" = "tempMax",
                                                                  "Temperatura Minima" = "tempMin",
                                                                  "Direccion del Viento" = "dirV",
                                                                  "Velocidad del Viento" = "velV",
                                                                  "Humedad Relativa" = "hum",
                                                                  "Punto de Rocio" = "pRocio"
                                                      )),
                                          #8,
                                          plotlyOutput('graficoCantidadesVar')
                                 )
                               )
                        ),
                        column(6,
                               leafletOutput("mapBUFR",height="70vh")#, width = "50%", height = "50%") 
                        )
                      ),
                      fluidRow(
                        column(2,
                               botonAyuda("preview3")
                               ),
                        column(8,
                               tabsetPanel(
                                 tabPanel(strong("BUFR"),
                                          h4(strong(textOutput("text")), align = "center"),
                                          plotlyOutput('graficoEstBUFR')
                                 ),
                                 tabPanel(strong("Metadato OSCAR-SURFACE"),value = "panel2",
                                          h4(strong(textOutput("text2")), align = "center"),
                                          htmlOutput("frame")
                                 )
                               )
                        ),
                        column(2#,
                               #div(actionButton('jumpToP2',strong('OSCAR - SURFACE'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center")
                               )
                      )
                      )
           )
  )
  
 ),
  div(style = "margin-bottom: 30px;"), # this adds breathing space between content and footer
  ###############################################.             
  ##############Footer----    
  ###############################################.
  #Copyright warning
  tags$footer( "© Instituto de Hidrología, Meteorología y Estudios Ambientales (Grupo de Planeación Operativa) v1.0 2024", 
               style = "
              position:fixed;
              text-align:center;
              left: 0;
              bottom:0;
              width:100%;
              z-index:1000;  
              height:30px; /* Height of the footer */
              color: white;
              padding: 5px;
              font-weight: bold;
              background-color: #E05227"
  )
  
  )
)
