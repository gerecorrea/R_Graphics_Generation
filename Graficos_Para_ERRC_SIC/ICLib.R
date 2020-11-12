extract_data <- function (mylocaldata){
  ### procedimento para recuperar o nome do usuario
  # separa a string com o nome do usuario em partes
  homename <- matrix(strsplit(mylocaldata$useraddress, "/"))
  username <- c()
  # considerei que o seguinte formato: /home/<usuario>/
  # recuperar o segundo nome do home do usuario
  for (i in 1:nrow(homename)) {username <- c(username,(unlist(homename[i])[3]))}
  # formatar a lista de nome em uma matrix com uma única coluna
  username <- matrix (username, nc = 1)
  
  # formatar os dados para o graficos 
  mylocaldata_data <- data.frame (username, mylocaldata$useraddress, mylocaldata$waiting_time, mylocaldata$score ) #pguei score tbm
  
  # remove linhas cujo username é NA
  return (mylocaldata_data[!is.na(mylocaldata_data$username),])
}

statsWtUsers <- function (mydata) {
  stat <- c()
  #numOfuser <- 3
  rStats <- matrix (NA, nr = numOfuser, nc = 8)
  for ( i in 1:numOfuser) {
    # Waiting_time das requisicoes do usuario listUser[i]
    userWt <- subset(mydata, mydata$username == listUser[i])[3]
    # core do usuário responsável listUser[i] pela req (novo)
    userScore <- subset(mydata, mydata$username == listUser[i])[4] 
    
    # estatísticas do listUser[i], exclui-se as requisições com waiting_time 0
    
    ############## AQUI ABAIXO MODIFIQUEI PARA TER A OPÇÃO DE PEGAR COM OU SEM FILA: #################
    #Só com situação de fila:
    #rStats[i,] = c (listUser[i], nrow(userWt), min(userWt[userWt > 0]), max(userWt[userWt > 0]), mean(unlist(userWt[userWt > 0])), median(unlist(userWt[userWt > 0])), sd(unlist(userWt[userWt > 0])), mean(unlist(userScore)) )
    #Independente se em fila ou não:
    rStats[i,] = c (listUser[i], nrow(userWt), min(userWt), max(userWt), mean(unlist(userWt)), median(unlist(userWt)), sum(unlist(userWt)), mean(unlist(userScore)))
    ########################################################################################
  }

  # retorna uma matriz com as estatisticas do Waiting_time do usuários que tem mediana diferente de NA
  return (rStats[!is.na(rStats[,6]),])
}

#
#
#
#
