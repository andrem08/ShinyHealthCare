source('Interface/NutritionalInfo.R')
source('Interface/BmiInterface.R')

server <- function (input, output, session){
  output$myImage <- renderImage({
    list(src = 'Interface/www/Shiny_Health_Care_Image.png', width = '700px', height = '700px', style="display: block; margin-left: auto; margin-right: auto;")
  }, deleteFile = FALSE)


  #Observe if you select a vitamin
  observeEvent(input$selectVit, observeSelectizeVit(input, output, session))

  observeEvent(input$BMI_button, bmiFunction(input, output, session))

  observeEvent(input$select_nutritional_food , observeButtonNutritionalFood(input, output, session))
}
