source('RFunctions/HomePage.R')
source('RFunctions/InfoPage.R')
source('RFunctions/NutritionalInfo.R')
source('RFunctions/BmiFunction.R')
source('RFunctions/SleepScheduleFunction.R')
source('RFunctions/Heartbeat.R')


ui <<- fluidPage(
  theme = shinytheme('flatly'),
  setBackgroundColor(
  ),
  navbarPage(('MyShinyStatistics'),
             #Apresentação do aplicativo, as estatísticas e sobre o autor
             welcome_page(),
             vitaminsInterface(),
             bmiInterface(),
             sleepFunction(),
             heartbeatInterface(),
             #Informaçoes e ajuda
             howToUse(),
          )
)