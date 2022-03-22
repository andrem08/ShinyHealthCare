
#Body mass index Interface.
bmiInterface <- function (){
  tabPanel(
     ('Calculate your BMI'), icon = icon('weight'),
       fluidRow(
         #Texto
         column(
           p(strong(h3('BMI calculator'))),
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
           width = 11
         )
       ),
     fluidRow(
       column(4,
              wellPanel(
                # Botões do filter.
                 h3(strong('Filter: ')),
                 fluidRow(
                   # NumericInput para inserir a altura
                   # selectInput para selecionar o tipo
                   column(6,
                           numericInput('height_BMI', 'Insert your height: ', value = 0)
                   ),
                   column(6,
                          selectInput('type_height_BMI', '', choices = c('Centimeters', 'Inches', 'Feet'))
                   )),
                 br(),
                 fluidRow(
                   # NumericInput para inserir o peso
                   # selectInput para selecionar o tipo
                   column(5,
                          numericInput('weight_BMI', 'Insert your weight:', value = 0)
                   ),
                   column(5,
                          selectInput('type_weight_BMI', '', choices = c('Kilograms', 'Lbs')), br()
                   )),
                # Realiza todos os cálculos dados os inputs
                 actionButton('BMI_button', 'Calculate your BMI')
              ),
              # Botão de download em pd
              wellPanel(
                  h4(strong('Downloads: ')),
                  downloadButton('download_bmi', 'Download the table:')
               ),
       ),
       # Tabela BMI
       column(4,
              withSpinner(dataTableOutput('BMI_table'), type = 6, color = '#0000ff')
       ),
       # Texto com os resultados e estatisticas, em um output dinamico
       column(4,
              h3(strong('BMI: ')),
              uiOutput('BMI_results')
       )
     )
  )
}
#Tabela bmi contendo os intervalos e classificações
# Decidi não retirar da web expecificamente do BMI. No caso nao teria nenhuma diferença
bmi_table <<- data.frame(
  bmi = c('Less than 16', '16 - 17', '17 - 18.5', '18.5 - 25', '25 - 30', '30 - 35', '35 - 40', 'More than 40'),
  classification = c('Severe Thinness', 'Moderate Thinness', 'Mild Thinness', 'Normal', 'Overweight', 'Obese Class I', 'Obese Class II', 'Obese Class III')
)
# Resultados e estatísticas para outros usos
bmi_text <<- NULL

# função para mostrar a tabela BMI inicialmente para o usuário
bmiTableStart <- function (input, output, session){
      output$BMI_table <- DT::renderDataTable(bmi_table)
}

# Cálculo das alturas dado os inputs pelo usuario.
# Imprime os resultados na tela BMI dependendo
# de seu BMI. Tabmém deixa selecionado na tabela a linha
# classificatoria de seu BMI
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
  # Para nao dar erro na divisão por 0
  if (height == 0)
    height <- 1

  # Calculo do BMI
  bmi <- weight/(height*height)
  bmi <- trunc(bmi*10, digits = 2)
  bmi <- bmi/10

  if(weight <= 0 || height <= 0){
    output$BMI_results <- renderUI({p(h3('Error, wrong inputs.'))})

    output$BMI_table <- DT::renderDataTable(bmi_table)
  }
  else{
    # Cada caso de bmi, resulta em uma frase única
    results <- case_when(
        bmi < 16 ~ 'Indicating you are very underweight for adults of your height.
        Weighing too little can contribute to a weakened immune system,
        fragile bones and feeling tired. Talk with your healthcare
        provider to determine an appropriate diet.',

        bmi >= 16 & bmi < 17 ~ 'Indicating you are underweight for adults of your height.
         Talk with your healthcare provider to determine possible causes of underweight and
         if you need to gain weight.',

        bmi >= 17 & bmi < 18.5 ~ 'Indicating you are a little underweight for adults of your height.
         Talk with your healthcare provider to determine possible causes of underweight and if
         you need to gain weight.',

        bmi >= 18.5 & bmi < 25 ~ 'Indicating your weight is normal for adults of your height.
         Maintaining a healthy weight may reduce the risk of chronic diseases associated with
         overweight and obesity.',

        bmi >= 25 & bmi < 30 ~ 'You are a little overweight for adults of your height.
        Consuming fewer calories, making healthy food choices, exercising
         more and controlling your weight can help reduce body mass.',

        bmi >= 30 & bmi < 35 ~ 'You are overweight for adults of your height.
        Consuming fewer calories, making healthy food choices, exercising
         more and controlling your weight can help reduce body mass.',

        bmi >= 35 & bmi < 40 ~ 'You are obese for adults of your height.
        Class II obesity can be very harmful to your health.
        Try to avoid gaining additional weight. Talk with your healthcare
         provider to determine appropriate ways to lose weight.',

        bmi >= 40 ~ 'You are morbidly obese for adults of your height.
        Class III obesity can contribute to the development of several
         serious health conditions, such as Type 2 diabetes and heart disease.
        The good news is that class III obesity is manageable and treatable.
        Talk with your healthcare provider to determine appropriate ways to lose weight.'
      )

    #Mostra os resultados em texto
    output$BMI_results <- renderUI({
      wellPanel(
      tagList(
        uiOutput('BMI_results_text1'),
        uiOutput('BMI_results_text2')
      )
      )
    })
    bmi_text <<- c(toString(bmi), results)
    output$BMI_results_text1 <- renderUI({p(h3('Your BMI is:', bmi))})
    output$BMI_results_text2 <- renderUI({p(results)})

    #Seleciona a classificação na sua tabela
    output$BMI_table <- DT::renderDataTable(
      bmi_table,
      selection = list(selected = case_when(
        bmi < 16 ~ 1,
        bmi >= 16 & bmi < 17 ~ 2,
        bmi >= 17 & bmi < 18.5 ~ 3,
        bmi >= 18.5 & bmi < 25 ~ 4,
        bmi >= 25 & bmi < 30 ~ 5,
        bmi >= 30 & bmi < 35 ~ 6,
        bmi >= 35 & bmi < 40 ~ 7,
        bmi >= 40 ~ 8
      ),target="row")
  )
  }
}

# END