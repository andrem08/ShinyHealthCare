source('Interface/NutritionalInfo.R')
source('Interface/BmiInterface.R')
source('Interface/SleepScheduleInterface.R')
source('Interface/Heartbeat.R')

function (input, output, session){

  user_nutritional_food <- reactive({
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

  output$nutritional_food_table <- renderDataTable(user_nutritional_food())

  observeEvent(input$sleep_select_age_who, observeSelectSleepWho(input, output, session))

  observeEvent(input$heart_rate_numeric_button, calculateHeartFrequency(input, output, session))

  #Download statistics
  # downloadVitamins(input, output, session)
  # output$download_nut <- downloadToPdf('StatisticReports/nutritionalTableStatistics.Rmd')

  downloadFiles <- function (path){
    return (
      content = function(file) {
        src <- normalizePath(path)
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, path, overwrite = TRUE)

        out <- render(path, switch(
          'PDF',
          PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ))
        file.rename(out, file)
    }

    )
  }

  output$download_vit <- downloadHandler(
    filename = function() { paste('my-report', sep = '.', 'pdf')},
    downloadFiles('vitamins_statistics.Rmd'))

  output$download_nut <- downloadHandler(
    filename = function() { paste('my-report', sep = '.', 'pdf')},
    downloadFiles('nutritional_table_statistics.Rmd'))
}
