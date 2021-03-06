
# retorna a lista de jobs c/ suas respectivas posições na fila
# (Score) | (Tamanho da fila) | (Nome do Job) | lista de posicoes na fila
getAllJobList <- function (mydata, mydata2, local_listOfQueue, local_numOfjobs) {
  maxCol <- max(local_listOfQueue[1:nrow(local_listOfQueue)-1,4])+extraCol
  #print ()
  allJobQueue <- matrix (NA, nr = nrow(local_listOfQueue), nc = maxCol)
  myrows <- 1
  lpos <- c()
  #Local_numOfjobs <- 500
  for (i in 1:local_numOfjobs) {
    lpos <- pos_jobQueue (mydata, sprintf("%s", mydata2[i,1]), local_listOfQueue)
    # print (lpos)
    # print (sprintf("%s", mydata2[i,1]))
    # print(local_listOfQueue)
    # print ("#######")
    # print (lpos)
    # print ("#######")
    if (!is.na(lpos)) {
      # print (sprintf("%s", lpos))
      # if (length(lpos) > maxCol) {
      #   print (sprintf("Tamano posicao %d (%s)", length(lpos), mydata2[i,1]))
      #   print (lpos)
      # }
  
      # print (lpos)
      # print (length(lpos))
      nroQueueJob <- job_queue (mydata, lpos[1], local_listOfQueue)
      #print (sprintf("Nome(%s) - Fila(%s/%s)\n", lpos[1], nroQueueJob[1], nroQueueJob[2]))
      # myQueue <- get_queue(mydata, as.numeric(job_queue (mydata, lpos[1], local_listOfQueue)[1]))
      myQueue <- get_queue(mydata, as.numeric(nroQueueJob[1]))
      #print (sprintf("Queue(%s)\n", myQueue$walltime[1]))
      # =lenghtQueue <- data.frame(myQueue$walltime[1])
      lenghtQueue <- myQueue$walltime[1]

      scoreJobFirst <- subset(myQueue, myQueue$id == lpos[1])$score
      # print(scoreJobFirst)
      #print (length(lpos))
      #print ("###$$%%**$$####")
      # print (subset(myQueue, myQueue$id == lpos[1]))
      # print (as.double(scoreJobFirst))
      # print (as.numeric(as.character(scoreJobFirst)))
      # scoreJobFirst <- myQueue$score[2]
      
      # lenghtQueue <- data.frame(get_queue(mydata, as.numeric(job_queue (mydata, lpos[1], listOfQueue)[1])) )$walltime[1]
      # print ("###$$**$$#### nroQueueJob[1]")
      # print (nroQueueJob[1])
      # print ("###$$**$$#### lenghtQueue")
      # print (lenghtQueue)
      # print ("###$$*&&*$$####")
      # print (local_listOfQueue[nroQueueJob[1],])
      # print (local_listOfQueue[nroQueueJob[1],4])
      # print ("###$$----$$####")
      nasrep <- as.numeric((maxCol-length(lpos)-2)) 
      if (nasrep < 0) {
        print ("Incremente o valor de extraCOL (linha 8 do arquivo ic-plotv2.R) - Gabiarra :-)")
      } 
      # print (maxCol-length(lpos)-2)
      # dataRaw <- c(lenghtQueue, as.numeric(as.character(scoreJobFirst)), lpos, rep(NA,(maxCol-length(lpos)-2)) )
      dataRaw <- c(lenghtQueue, as.numeric(as.character(scoreJobFirst)), lpos, rep(NA,nasrep))
      #print(nasrep)
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
    mtmp <<- c(mtmp, tpos)
    # print(tpos)
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

