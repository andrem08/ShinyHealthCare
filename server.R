source('RFunctions/NutritionalInfo.R')
source('RFunctions/BmiFunction.R')
source('RFunctions/SleepScheduleFunction.R')
source('RFunctions/Heartbeat.R')

library('writexl')
server <- function (input, output, session){

  options(warn=-1)

  user_nutritional_food <- reactive({
    nutritional_DF [[which(nutritional_DF[[1]] == input$select_nutritional_food) + 1]]
  })

  output$myImage <- renderImage({
    list(src = 'RFunctions/www/Shiny_Health_Care_Image.png', width = '600px', height = '600px', style="display: block; margin-left: auto; margin-right: auto;")
  }, deleteFile = FALSE)

  observeEvent(input$heart_pa_file, correctHeartPAFile(input, output))
  observeEvent(input$sleep_button_file, observeSleepButton(input, output, session))

  observeEvent(input$sleep_button_add, addHour(input, output, session))
  observeEvent(input$sleep_button_remove, removeHour(input, output, session))
  observeEvent(input$create_sleep_file, newSleepFile(input, output, session))

  observeEvent(input$sleep_file, correctSleepFile(input, output))
  #Observe if you select a vitamin
  observeEvent(input$select_vit, observeSelectizeVit(input, output, session))

  bmiTableStart(input, output, session)
  observeEvent(input$BMI_button, bmiFunction(input, output, session))

  output$nutritional_food_table <- renderDataTable(user_nutritional_food())

  observeEvent(input$sleep_select_age_who, observeSelectSleepWho(input, output, session))

  observeEvent(input$heart_rate_numeric_button, calculateHeartFrequency(input, output, session))


  observeEvent(input$heart_pa_button_file, heartPACalculation(input, output, session))

  displayVisPlot()

  #Download statistics
  downloadFilesPdf <- function (path){
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
    filename = function() { paste('vitamins_table', sep = '.', 'pdf')},
    downloadFilesPdf('vitamins_statistics.Rmd'))

  output$download_nut <- downloadHandler(
    filename = function() { paste('nutritional_table', sep = '.', 'pdf')},
    downloadFilesPdf('nutritional_table_statistics.Rmd'))

  output$sleep_download_pdf <- downloadHandler(
    filename = function() { paste('sleep_schedule', sep = '.', 'pdf')},
    downloadFilesPdf('your_sleep_schedule.rmd'))

  output$download_sleep_who <- downloadHandler(
    filename = function() { paste('sleep_schedule_who', sep = '.', 'pdf')},
    downloadFilesPdf('sleep_schedule.rmd'))

  output$download_heart_beat <- downloadHandler(
    filename = function() { paste('heart_rate', sep = '.', 'pdf')},
    downloadFilesPdf('heart_rate.rmd'))

  output$heart_pa_download_pdf <- downloadHandler(
    filename = "heart_rate_plot.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "heartbeat_plot.rmd")
      file.copy("heartbeat_plot.rmd", tempReport, overwrite = TRUE)

      params <- list(n = input$slider)
      out <- render(tempReport, output_file = file, params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  output$download_bmi <- downloadHandler(
    filename = function() { paste('bmi_statistics', sep = '.', 'pdf')},
    downloadFilesPdf('bmi_statistics.rmd'))

  output$sleep_download_xlsx <- downloadHandler(
    filename = function() { "sleep_table.xlsx"},
    content = function(file) {write_xlsx(month_table, path = file)}
  )
}
