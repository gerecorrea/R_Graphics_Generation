# 
job_queen <- function (mydata, jobname, queen_list) {
  list_index <- subset(mydata, mydata$id == jobname)
  start_in_queen <- as.numeric(rownames(list_index)[1])
  last_stay_in_queen <- as.numeric(rownames(list_index)[length(rownames(list_index))])
  
  queen_df <- data.frame(queen_list)
  tmp1 <- queen_df[queen_df$Inicio > start_in_queen,]
  tmp2 <- queen_df[queen_df$Inicio > last_stay_in_queen,]
  ret <- data.frame(tmp1$NroFila[1]-1,tmp2$NroFila[1]-1)
  colnames(ret) <- c("Fila_Entrada", "Ultima_fila")
  return (ret)
}

# retorna uma lista com o número da fila e as respectivas posicoes de inicio/fim 
make_queen_position <- function (mydata) {
  index_queen <- subset(mydata, mydata$id == "Fila Jobs n:")
  pos_list <- c()
  for (i in 1:nrow(subset(mydata, mydata$id == "Fila Jobs n:"))) {
    start_pos <- as.numeric(rownames(index_queen)[i])
    stop_pos <- as.numeric(rownames(index_queen)[i+1]) - 2 
    pos_list <- c(pos_list, i, start_pos, stop_pos)
  }
  queen_positions <- t(matrix (pos_list, nr =3))
  colnames(queen_positions) <- c ("NroFila", "Inicio", "Fim")
  return (queen_positions)
}

# retorna todo o conteúdo da fila número "position"
get_queen <- function (mydata, position) {
  index_queen <- subset(mydata, mydata$id == "Fila Jobs n:")
  start_pos <- as.numeric(rownames(index_queen)[position])
  stop_pos <- as.numeric(rownames(index_queen)[position+1]) - 2 
  return (mydata[start_pos:stop_pos,])
}


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
  mylocaldata_data <- data.frame (username, mylocaldata$useraddress, mylocaldata$waiting_time )
  
  # remove linhas cujo username é NA
  return (mylocaldata_data[!is.na(mylocaldata_data$username),])
}

statsWtUsers <- function (mydata) {
  stat <- c()
  #numOfuser <- 3
  rStats <- matrix (NA, nr = numOfuser, nc = 7)
  for ( i in 1:numOfuser) {
    # Waiting_time das requisicoes do usuario listUser[i]
    userWt <- subset(mydata, mydata$username == listUser[i])[3]
    
    # estatísticas do listUser[i], exclui-se as requisições com waiting_time 0
    rStats[i,] = c (listUser[i], nrow(userWt), min(userWt[userWt > 0]), max(userWt[userWt > 0]), mean(unlist(userWt[userWt > 0])), median(unlist(userWt[userWt > 0])), sd(unlist(userWt[userWt > 0])) )
  }

  # retorna uma matriz com as estatisticas do Waiting_time do usuários que tem mediana diferente de NA
  return (rStats[!is.na(rStats[,6]),])
}

