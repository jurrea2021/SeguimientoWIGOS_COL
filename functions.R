filtroVariablesBUFR = function(id,labelId) {
  selectInput(id,
              label = labelId,
              choices = c("Precipitacion" = "prec",
                          "Presion" = "presion",
                          "Temperatura" = "temp",
                          "Temperatura Maxima" = "tempMax",
                          "Temperatura Minima" = "tempMin",
                          "Direccion del Viento" = "dirV",
                          "Velocidad del Viento" = "velV",
                          "Humedad Relativa" = "hum",
                          "Punto de Rocio" = "pRocio"
              ))
}

botonAyuda = function(id) {
  div(useShinyalert(), # Set up shinyalert
      actionButton(id, strong("Ayuda"),icon('question-circle'),style="color: #1C2322; background-color: #E05227; border-color: #18BC9C"))
} 
