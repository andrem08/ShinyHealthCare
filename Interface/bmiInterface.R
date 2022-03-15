bmiInterface <- function (){
  tabPanel(
     ('Calculate your BMI'), icon = icon('weight'),
       fluidRow(
         column(
           p(strong(h3('BMI calculator:'))),
           p('Body mass index (BMI) is a value derived from the mass (weight) and height of a person.
           The BMI is defined as the body mass divided by the square of the body height, and is expressed
           in units of kg/m2, resulting from mass in kilograms and height in metres.',
              br(),br(),
             'The BMI is a convenient rule of thumb used to broadly categorize a person as underweight,
             normal weight, overweight, or obese. This calculation is used both for men and women, aged 18 or older.',
             br(),br(),
             'Now for calculate your BMI you\'ll need to insert your height and your weight. You can chose the metric system you\'re going to use: ',
             br(),
             style = stylePanel),
           width = 9
         )
       ),
     fluidRow(
       column(4,
              h3(strong('Filter: ')),
              fluidRow(
                column(5,
                       selectInput('type_height_BMI', 'Choose the metric system', choices = c('Centimeters', 'Inches', 'Feet'))
                ),
                column(5,
                       numericInput('height_BMI', 'Insert your height', value = 0)
                ), style = smallStylePanel),
              br(),
              fluidRow(
                column(5,
                       selectInput('type_weight_BMI', 'Choose the metric system', choices = c('Kilograms', 'Lbs'))
                ),
                column(5,
                       numericInput('weight_BMI', 'Insert your weight', value = 0), br()
                ), style = smallStylePanel),
              actionButton('BMI_button', 'Calculate your BMI')
       ),
       column(4,
              verbatimTextOutput('BMI_results')
       ),
       column(4,
              dataTableOutput('BMI_table')
       )
     )
  )
}
bmiFunction <- function (input, output, session){
  #Primeiro transformar a altura em metros
  height <- switch(
    input$type_height_BMI,
    'Inches' = input$height_BMI * 0.0254,
    'Feet' = input$height_BMI * 0.3048,
    'Centimeters' = input$height_BMI * 0.01
  )

  weight <- switch(
    input$type_weight_BMI,
    'Lbs' = input$weight_BMI * 0.453592,
    'Kilograms' = input$weight_BMI * 1
  )
  if (height == 0)
    height <- 1

  bmi <- weight/(height*height)
  bmi <- trunc(bmi*10, digits = 2)
  bmi <- bmi/10
  interval_bmi <- c('Less than 16', '16 - 17', '17 - 18.5', '18.5 - 25', '25 - 30', '30 - 35', '35 - 40', 'More than 40')
  labs_bmi <- c('Severe Thinness', 'Moderate Thinness', 'Mild Thinness', 'Normal', 'Overweight', 'Obese Class I', 'Obese Class II', 'Obese Class III')
  bmi_table <- data.frame(bmi = interval_bmi, classification = labs_bmi)
  output$BMI_results <- renderText({paste('Your BMI is', bmi)})

  output$BMI_table <- renderDataTable(bmi_table)
}