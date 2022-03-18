source('Interface/NutritionalInfo.R')
source('Interface/BmiInterface.R')
source('Interface/SleepScheduleInterface.R')

function (input, output, session){

  regFormula <- reactive({
    nutritional_DF [[which(nutritional_DF[[1]] == input$select_nutritional_food) + 1]]
  })

  output$myImage <- renderImage({
    list(src = 'Interface/www/Shiny_Health_Care_Image.png', width = '700px', height = '700px', style="display: block; margin-left: auto; margin-right: auto;")
  }, deleteFile = FALSE)

  observeEvent(input$sleep_button_file, observeSleepButton(input, output, session))

  observeEvent(input$sleep_button_add, addHour(input, output, session))
  observeEvent(input$sleep_button_remove, removeHour(input, output, session))
  observeEvent(input$create_sleep_file, newFile(input, output, session))

  observeEvent(input$sleep_file, correctFile(input, output ))
  #Observe if you select a vitamin
  observeEvent(input$select_vit, observeSelectizeVit(input, output, session))

  observeEvent(input$BMI_button, bmiFunction(input, output, session))

  output$nutritional_food_table <- renderDataTable(regFormula())

  observeEvent(input$sleep_select_age_who, observeSelectSleepWho(input, output, session))

  downloadVitamins(input, output, session)
}
