source('RFunctions/HomePage.R')
source('RFunctions/InfoPage.R')
source('RFunctions/NutritionalInfo.R')
source('RFunctions/BmiFunction.R')
# source('RFunctions/SleepScheduleFunction.R')
source('RFunctions/Heartbeat.R')
source('RFunctions/MoreHealthInfo.R')
source('RFunctions/GetReddit.R')

source('RFunctions/GetGTrands.R')


ui <<- fluidPage(
  theme = shinytheme('flatly'),
  setBackgroundColor(
  ),

  navbarPage((''),
             #Apresentação do aplicativo, as estatísticas e sobre o autor
             welcome_page(),
             vitaminsInterface(),
             bmiInterface(),
             # sleepFunction(),
             heartbeatInterface(),
             navbarMenu(
               title = 'More information',
               healthInformationInterface(),
               getRedditInterface(),
               get_gtrands()
             ),
             #Informaçoes e ajuda
             howToUse(),
             # tabPanel(title = "Quit", value = 'stop', icon = icon("circle"))
          )
)