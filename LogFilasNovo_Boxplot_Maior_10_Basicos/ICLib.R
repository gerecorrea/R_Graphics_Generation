
# retorna a lista de jobs c/ suas respectivas posições na fila
getAllJobList <- function (mydata, mydata2, local_listOfQueue, local_numOfjobs) {
  maxCol <- max(local_listOfQueue[1:nrow(local_listOfQueue)-1,4])+10
  allJobQueue <- matrix (NA, nr = nrow(local_listOfQueue), nc = maxCol)
  myrows <- 1
  lpos <- c()
  for (i in 1:local_numOfjobs) {
    lpos <- pos_jobQueue (mydata, sprintf("%s", mydata2[i,1]), local_listOfQueue)
    if (!is.na(lpos)) {
      #print (lpos)
      # print (length(lpos))
      dataRaw <- c(lpos,rep(NA,(maxCol-length(lpos))) )
      allJobQueue[myrows,] <- dataRaw
      myrows <- myrows + 1
    }
  }
  return(allJobQueue)
}


# retorna a posição do jobname na fila e o tamanho da fila
pos_jobQueue <- function (mydata, jobname, list_queue) {
  job_queues <- job_queue (mydata, jobname, list_queue)
  if (is.na(job_queues[2])) {
    return (NA)
  }

  # print(job_queens[1])
  # print(job_queens[2])
  
  lposition <- c()
  for (i in as.numeric(job_queues[1]):as.numeric(job_queues[2])) {
    tmpqueue <- get_queue(mydata, i)
    myjob <- subset(tmpqueue, tmpqueue[1] == jobname)
    # print (tmpqueen)
    # print (as.numeric(rownames(myjob)))
    # print (as.numeric(rownames(tmpqueen)[1]))
    tpos <- (as.numeric(rownames(tmpqueue)[1])-as.numeric(rownames(myjob)))*(-1)
    # print ( tpos )
    # print (as.numeric(rownames(tmpqueen)[1])-as.numeric(rownames(myjob))+2)
    lposition <- c (lposition, tpos)
  }
  lposition <- c(jobname, lposition)
  return (lposition)
}


# retorna os números da fila de entrada e da última fila que o processo jobname pertence
job_queue <- function (mydata, jobname, queue_list) {
  list_index <- subset(mydata, mydata$id == jobname)
  start_in_queue <- as.numeric(rownames(list_index)[1])
  last_stay_in_queue <- as.numeric(rownames(list_index)[length(rownames(list_index))])
  
  queue_df <- data.frame(queue_list)
  tmp1 <- queue_df[queue_df$Inicio > start_in_queue,]
  tmp2 <- queue_df[queue_df$Inicio > last_stay_in_queue,]
  ret <- data.frame(tmp1$NroFila[1]-1,tmp2$NroFila[1]-1)
  colnames(ret) <- c("Fila_Entrada", "Ultima_fila")
  return (ret)
}

# retorna a lista de filas contendo: o número da fila e as respectivas posicoes de inicio/fim 
make_queue_position <- function (mydata) {
  index_queue <- subset(mydata, mydata$id == "Fila Jobs n:")
  pos_list <- c()
  for (i in 1:nrow(subset(mydata, mydata$id == "Fila Jobs n:"))) {
    start_pos <- as.numeric(rownames(index_queue)[i])
    stop_pos <- as.numeric(rownames(index_queue)[i+1]) - 2 
    pos_list <- c(pos_list, i, start_pos, stop_pos, (stop_pos-start_pos))
  }
  queue_positions <- t(matrix (pos_list, nr =4))
  colnames(queue_positions) <- c ("NroFila", "Inicio", "Fim", "Tamanho")
  return (queue_positions)
}

# retorna os Jobs da fila número "position"
get_queue <- function (mydata, position) {
  index_queue <- subset(mydata, mydata$id == "Fila Jobs n:")
  start_pos <- as.numeric(rownames(index_queue)[position])
  stop_pos <- as.numeric(rownames(index_queue)[position+1]) - 2 
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

