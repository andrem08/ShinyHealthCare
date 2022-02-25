
#Ao selecionar um dos files, esta função carrega os dados
# na variavel global dados, atualiza os sliders input e
# atualiza os nomes dos paineis de gráfico
observeSelectQuiSq <- function (input, output, session){

    #Os arquivos que o usuario baixar
    # Caso ele nao tenha nenhum, o programa vai enviar um de exemplo
    #Carregar o arquivo selecionado, nomear as linhas e
    # remover a primeira coluna, que contem descrições
    file <- input$file
    if(is.null(file)){
        dados <<- read.xlsx('Dados/FrequenciaEnjoo.xlsx', 1)
        rownames(dados) <<- dados[, 1]
        dados <<- dados[-1]
    }
    else{
        dados <<- read.xlsx(file$datapath, 1)
        rownames(dados) <<- dados[, 1]
        dados <<- dados[-1]
    }

    #Atualizar os comprimentos dos sliders conforme o tamanho da tabela
    # modificando o número máximo de linhas e colunas
    updateSliderInput(session = session, inputId = 'sliderCol' ,max = ncol(dados ), value = ncol(dados ))
    updateSliderInput(session = session, inputId = 'sliderLin' ,max = nrow(dados), value = nrow(dados))

    #Atualiza o titulo dos paineis
    output$firstPlot <- renderText((rownames(dados)[1]))
    output$secondPlot <- renderText((rownames(dados)[2]))

    #Constroi a tabela de contingência
    output$tabCont <- DT::renderDT(datatable(dados, options = table_opt))
    output$newTabCont <- DT::renderDT(datatable(dados, options = table_opt))


  }

#Recebe os dados já carregados com os arquivos, pega
# um numero, determinado pelo usuario, de linhas e colunas
# cada uma aleatoriamente escolhida. Em seguida, constroi
# o teste Qui-Quadrado e por fim constroi cada um dos gráficos
observeButtonQuiSq <- function (input, output, session){
    dadosApp <- dados

    #Remover aleatoriamente linhas e colunas conforme os sliders dado pelo usuário
    if(input$sliderCol < ncol(dadosApp)) dadosApp <- dadosApp[-sample(seq_len(ncol(dadosApp)), ncol(dadosApp) - input$sliderCol, replace = F)]
    if(input$sliderLin < nrow(dadosApp)) dadosApp <- dadosApp[-sample(seq_len(nrow(dadosApp)), nrow(dadosApp) - input$sliderLin, replace = F), ]

    #Atualiza novamente os tab panels
    output$firstPlot <- renderText((rownames(dadosApp)[1]))
    output$secondPlot <- renderText((rownames(dadosApp)[2]))
    #Atualiza com a nova tabela de contingência
    output$newTabCont <- DT::renderDT(datatable(dadosApp, options = table_opt))

    #Calcula o teste Qui-Quadrado
    #Output imprime o valor de p, a quantidade de graus de liberdade e
    # o valor de Qui-Quadrado
    quiSq <- chisq.test(dadosApp, simulate.p.value = FALSE)
    output$textEstat <- renderPrint(cat('Valor de p: ', quiSq$p.value, '\n',
                              quiSq$parameter, ' graus de liberdade.\n',
                              'Valor de Qui-Quadrado: ',quiSq$statistic , sep = ''))

    #Data para os gráficos
    data <- data.frame(matrix(ncol = ncol(dadosApp), nrow = 2))
    names(data) <- names(dadosApp)
    rownames(data) <- c('Esperado', 'Observado')
    data2 <- data <- t(t(data))

    #Insere os dados observados e os dados esperados nos dados
    # dos gráficos.
    data[1, ] <- as.integer(quiSq$expected[1,])
    data[2, ] <- as.integer(quiSq$observed[1, ])
    data2[1, ] <- as.integer(quiSq$expected[2,])
    data2[2, ] <- as.integer(quiSq$observed[2, ])

    #Construção dos graficos de observações e expectativas
    output$plotOut <- renderPlot(barplot(data, legend = rownames(data), col = c('steelblue', 'orange'),  beside = TRUE, main = rownames(dadosApp)[1], las = 1), height = 500, width = 1000)
    output$plotOut2 <- renderPlot(barplot(data2, legend = rownames(data2), col = c('steelblue', 'orange'),beside = TRUE, main = rownames(dadosApp)[2], las = 1), height = 500, width = 1000)
}
