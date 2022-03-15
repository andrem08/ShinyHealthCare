source('Interface/welcomePage.R')
source('Interface/HowToUse.R')
source('Interface/VitaminsInterface.R')
source('Interface/bmiInterface.R')
source('Interface/SleepScheduleInterface.R')
source('Interface/Heartbeat.R')


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