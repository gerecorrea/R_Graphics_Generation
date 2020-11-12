library("readxl")
library(gplots)
# library(plotly)
# library(plotrix)
library(RColorBrewer)
source ("ICLibv2.R")

extraCol <- 30

# nome dos arquivos de entrada
# filenames <- c("FCFS_jobs.xlsx", "SCORE1440_scoreOk_jobs.xlsx", "SCORE10800_scoreOk_jobs.xlsx")
# fileFilas <- c("FCFS_logFilas.csv", "SCORE1440_logFilas.csv", "SCORE10800_logFilas.csv")

filenames <- c("v2_FCFS_jobs.xlsx", "v2_SCORE1440_jobs.xlsx", "v2_SCORE10800_jobs.xlsx")
fileFilas <- c("v2_FCFS_logFilas.csv", "v2_SCORE1440_logFilas.csv", "v2_SCORE10800_logFilas.csv")

# nome das planilhas (considerei o uso somente da primeira planilha por arquivo)
mysheets <- c(excel_sheets(filenames[1]), excel_sheets(filenames[2]), excel_sheets(filenames[3]))

# leitura dos arquivos de entrada
FCFS      <- read_excel(filenames[1], sheet=mysheets[1])
FCFSFila  <- read.csv(fileFilas[1])
SCO11440  <- read_excel(filenames[2], sheet=mysheets[2])
SCO11Fila <- read.csv(fileFilas[2])
SCO10800  <- read_excel(filenames[3], sheet=mysheets[3])
SCO10Fila <- read.csv(fileFilas[3])

# extrai dados p/ os graficos
FCFS_data <- extract_data(FCFS)
SCO1_data <- extract_data(SCO11440)
SCO2_data <- extract_data(SCO10800)

# Imprime todos os jobs da fila 12
print(get_queue(FCFSFila, 20))

# Imprime o nome da fila que o "w0!1663258" entrou e a última vez que ele aparece
# print (job_queue (FCFSFila, "w0!1658582", listOfQueue))
# Imprime as posições que o jobname esteve na fila ao longo do tempo

# número de usuários diferentes no sistema
numOfuser <- length (unique(FCFS_data$username))
# número total de jobs do sistema
numOfjobs <- nrow (FCFS_data)
# lista de usuários
listUser <- as.character(unique(FCFS_data$username))

# retorna a lista de jobs c/ suas respectivas posições na fila
# listOfQueue <- make_queue_position (FCFSFila)
# summary(listOfQueue[1:nrow(listOfQueue),4])
# allJobQueueFCFS <- getAllJobList (FCFSFila, FCFS, listOfQueue, nrow(FCFS_data))

mtmp <<- c()
mtmp1 <<- c()
listOfQueue <- make_queue_position (SCO10Fila)
summary(listOfQueue[1:nrow(listOfQueue),4])
allJobQueueSCO10 <- getAllJobList (SCO10Fila, SCO10800, listOfQueue, nrow(SCO1_data))
df <- data.frame (as.numeric(allJobQueueSCO10[,1]), 
                  allJobQueueSCO10[,2], 
                  allJobQueueSCO10[,3], 
                  as.numeric(allJobQueueSCO10[,4]))
colnames(df) <- c("TamFila", "Score", "Job", "Pos_Entrada")


############### Gráfico 1 - boxplot para filas maior que 10 - do Pillon #############################
### Com fila maior que 10 Jobs
InQueue10 <- na.omit(df[df$TamFila > 10,])
colnames(InQueue10) <- c("TamFila", "Score", "Job", "Pos_Entrada")

faixa0_25   <- InQueue10[as.numeric(as.character(InQueue10$Score)) < 25,]
faixa25_50  <- InQueue10[as.numeric(as.character(InQueue10$Score)) >= 25 & as.numeric(as.character(InQueue10$Score)) < 50,]
faixa50_75  <- InQueue10[as.numeric(as.character(InQueue10$Score)) >= 50 & as.numeric(as.character(InQueue10$Score)) < 75,]
faixa75_100 <- InQueue10[as.numeric(as.character(InQueue10$Score)) >= 75,]

maxnrow <- max (nrow(faixa0_25), nrow(faixa25_50), nrow(faixa50_75), nrow(faixa75_100))
# maxScore <- max (na.omit(as.numeric(as.character(faixa0_25$Score))), 
#                  na.omit(as.numeric(as.character(faixa25_50$Score))), 
#                  na.omit(as.numeric(as.character(faixa50_75$Score))), 
#                  na.omit(as.numeric(as.character(faixa75_100$Score))))

boxData <- data.frame(c(faixa0_25$TamFila/faixa0_25$Pos_Entrada, rep(NA, maxnrow-max(nrow(faixa0_25))+1)),
                      c(faixa25_50$TamFila/faixa25_50$Pos_Entrada, rep(NA, maxnrow-max(nrow(faixa25_50))+1)),
                      c(faixa50_75$TamFila/faixa50_75$Pos_Entrada, rep(NA, maxnrow-max(nrow(faixa50_75))+1)),
                      c(faixa75_100$TamFila/faixa75_100$Pos_Entrada, rep(NA, maxnrow-max(nrow(faixa75_100))+1))
                      )
colnames(boxData) <- c("0-25", "25-50", "50-75", "75-100")

maxRation <-max(na.omit(boxData$`0_25`),
                na.omit(boxData$`25_50`),
                na.omit(boxData$`50_75`),
                na.omit(boxData$`75_100`))

summary(boxData/maxRation)

############################################
### Gráficos ####

pdf ("graph-v02.pdf", width = 11)
par(mfrow = c(1,1))

boxplot(boxData, main = c("Deslocamento na fila para tarefas com filas maiores que 10"), xlab = "Intervalos de score dos usuários", ylab = "Razão de deslocamento na fila")
############################### FIM GRÁFICO 1 ###########################

# AGORA ABAIXO GRÁFICOS DE GEREMIAS CORRÊA:

################################### Gráfico 2 - boxplot com razão do professor para filas > 5 (tentativa gere) #############
### Com fila maior que 5 Jobs
InQueue50 <- na.omit(df[df$TamFila > 5,])
colnames(InQueue10) <- c("TamFila", "Score", "Job", "Pos_Entrada")

faixa0_25   <- InQueue50[as.numeric(as.character(InQueue50$Score)) < 25,]
faixa25_50  <- InQueue50[as.numeric(as.character(InQueue50$Score)) >= 25 & as.numeric(as.character(InQueue50$Score)) < 50,]
faixa50_75  <- InQueue50[as.numeric(as.character(InQueue50$Score)) >= 50 & as.numeric(as.character(InQueue50$Score)) < 75,]
faixa75_100 <- InQueue50[as.numeric(as.character(InQueue50$Score)) >= 75,]

maxnrow <- max (nrow(faixa0_25), nrow(faixa25_50), nrow(faixa50_75), nrow(faixa75_100))
# maxScore <- max (na.omit(as.numeric(as.character(faixa0_25$Score))), 
#                  na.omit(as.numeric(as.character(faixa25_50$Score))), 
#                  na.omit(as.numeric(as.character(faixa50_75$Score))), 
#                  na.omit(as.numeric(as.character(faixa75_100$Score))))

boxData <- data.frame(c(faixa0_25$TamFila/faixa0_25$Pos_Entrada, rep(NA, maxnrow-max(nrow(faixa0_25))+1)),
                      c(faixa25_50$TamFila/faixa25_50$Pos_Entrada, rep(NA, maxnrow-max(nrow(faixa25_50))+1)),
                      c(faixa50_75$TamFila/faixa50_75$Pos_Entrada, rep(NA, maxnrow-max(nrow(faixa50_75))+1)),
                      c(faixa75_100$TamFila/faixa75_100$Pos_Entrada, rep(NA, maxnrow-max(nrow(faixa75_100))+1))
)
colnames(boxData) <- c("0-25", "25-50", "50-75", "75-100")

maxRation <-max(na.omit(boxData$`0_25`),
                na.omit(boxData$`25_50`),
                na.omit(boxData$`50_75`),
                na.omit(boxData$`75_100`))

summary(boxData/maxRation)

boxplot(boxData, main = c("Descalocamento nas filas para tarefas com filas maiores que 5"), xlab = "Intervalos de score dos usuários", ylab = "Razão de deslocamento na fila")

# print (job_queue (SCO10Fila, "w0!1658564", listOfQueue))
# print (get_queue (SCO10Fila, 1))
# print (job_queue (SCO10Fila, "w0!1658608", listOfQueue))
# print (get_queue (SCO10Fila, 4))

#Isso aqui das 3 abaixo pode remover, né?
#listOfQueue <- make_queue_position (SCO11Fila)
#summary(listOfQueue[1:nrow(listOfQueue),4])
#allJobQueueSCO11 <- getAllJobList (SCO11Fila, SCO11440, listOfQueue, nrow(SCO2_data))
################### FIM GRÁFICO 2 #####################################

######### Gráfico 3 - boxplot filas mas com razão de 0 a 1, onde 1 furou toda a fila e 0 não furou nada ################
#Desenvolvido por Geremias Corrêa

#Formula: 1 - (pos entrada/ tam fila)
#Ex: fila tamanho 75 e entro na posição 25, logo razão = 1 - (0,33) = 0,666. Portanto, cortei 66% da fila!.

dev.off()

pdf ("graph-v03.pdf", width = 11)
InQueue50 <- na.omit(df[df$TamFila > 10,])
colnames(InQueue10) <- c("TamFila", "Score", "Job", "Pos_Entrada")

faixa0_25   <- InQueue50[as.numeric(as.character(InQueue50$Score)) < 25,]
faixa25_50  <- InQueue50[as.numeric(as.character(InQueue50$Score)) >= 25 & as.numeric(as.character(InQueue50$Score)) < 50,]
faixa50_75  <- InQueue50[as.numeric(as.character(InQueue50$Score)) >= 50 & as.numeric(as.character(InQueue50$Score)) < 75,]
faixa75_100 <- InQueue50[as.numeric(as.character(InQueue50$Score)) >= 75,]

maxnrow <- max (nrow(faixa0_25), nrow(faixa25_50), nrow(faixa50_75), nrow(faixa75_100))
# maxScore <- max (na.omit(as.numeric(as.character(faixa0_25$Score))), 
#                  na.omit(as.numeric(as.character(faixa25_50$Score))), 
#                  na.omit(as.numeric(as.character(faixa50_75$Score))), 
#                  na.omit(as.numeric(as.character(faixa75_100$Score))))

boxData <- data.frame(c(1-(faixa0_25$Pos_Entrada/faixa0_25$TamFila), rep(NA, maxnrow-max(nrow(faixa0_25))+1)),
                      c(1-(faixa25_50$Pos_Entrada/faixa25_50$TamFila), rep(NA, maxnrow-max(nrow(faixa25_50))+1)),
                      c(1-(faixa50_75$Pos_Entrada/faixa50_75$TamFila), rep(NA, maxnrow-max(nrow(faixa50_75))+1)),
                      c(1-(faixa75_100$Pos_Entrada/faixa75_100$TamFila), rep(NA, maxnrow-max(nrow(faixa75_100))+1))
)

colnames(boxData) <- c("0-25", "25-50", "50-75", "75-100")

maxRation <-max(na.omit(boxData$`0_25`),
                na.omit(boxData$`25_50`),
                na.omit(boxData$`50_75`),
                na.omit(boxData$`75_100`))

summary(boxData/maxRation)

boxplot(boxData, main = c("Descalocamento nas filas para tarefas com filas maiores que 10"), xlab = "Intervalos de score dos usuários", ylab = "Razão de deslocamento na fila")

dev.off()

pdf ("graph-v04.pdf", width = 11)

######################################### Fim gráfico 3 ########################################################

######### Gráfico 4 - boxplot fila, mesmo do 3, mas com porcertagem 0 a 100 ################
#Desenvolvido por Geremias Corrêa

#Formula: 1 - (pos entrada/ tam fila)
#Ex: fila tamanho 75 e entro na posição 25, logo razão = 1 - (0,33) = 0,666. Portanto, cortei 66% da fila!.

InQueue50 <- na.omit(df[df$TamFila > 10,])
colnames(InQueue10) <- c("TamFila", "Score", "Job", "Pos_Entrada")

faixa0_25   <- InQueue50[as.numeric(as.character(InQueue50$Score)) < 25,]
faixa25_50  <- InQueue50[as.numeric(as.character(InQueue50$Score)) >= 25 & as.numeric(as.character(InQueue50$Score)) < 50,]
faixa50_75  <- InQueue50[as.numeric(as.character(InQueue50$Score)) >= 50 & as.numeric(as.character(InQueue50$Score)) < 75,]
faixa75_100 <- InQueue50[as.numeric(as.character(InQueue50$Score)) >= 75,]

maxnrow <- max (nrow(faixa0_25), nrow(faixa25_50), nrow(faixa50_75), nrow(faixa75_100))
# maxScore <- max (na.omit(as.numeric(as.character(faixa0_25$Score))), 
#                  na.omit(as.numeric(as.character(faixa25_50$Score))), 
#                  na.omit(as.numeric(as.character(faixa50_75$Score))), 
#                  na.omit(as.numeric(as.character(faixa75_100$Score))))

boxData <- data.frame(c(100*(1-(faixa0_25$Pos_Entrada/faixa0_25$TamFila)), rep(NA, maxnrow-max(nrow(faixa0_25))+1)),
                      c(100*(1-(faixa25_50$Pos_Entrada/faixa25_50$TamFila)), rep(NA, maxnrow-max(nrow(faixa25_50))+1)),
                      c(100*(1-(faixa50_75$Pos_Entrada/faixa50_75$TamFila)), rep(NA, maxnrow-max(nrow(faixa50_75))+1)),
                      c(100*(1-(faixa75_100$Pos_Entrada/faixa75_100$TamFila)), rep(NA, maxnrow-max(nrow(faixa75_100))+1))
)

colnames(boxData) <- c("0-25", "25-50", "50-75", "75-100")

maxRation <-max(na.omit(boxData$`0_25`),
                na.omit(boxData$`25_50`),
                na.omit(boxData$`50_75`),
                na.omit(boxData$`75_100`))

summary(boxData/maxRation)

boxplot(boxData, main = c("Descalocamento nas filas para tarefas com filas maiores que 10"), xlab = "Intervalos de score dos usuários", ylab = "Porcentagem de ganho de fila")

######################################### Fim gráfico 4 ########################################################

######### Gráfico 5 - boxplot fila, mesmo do 4, mas para filas > 5 ################
#Desenvolvido por Geremias Corrêa

#Formula: 1 - (pos entrada/ tam fila)
#Ex: fila tamanho 75 e entro na posição 25, logo razão = 1 - (0,33) = 0,666. Portanto, cortei 66% da fila!.

InQueue50 <- na.omit(df[df$TamFila > 5,])
colnames(InQueue10) <- c("TamFila", "Score", "Job", "Pos_Entrada")

faixa0_25   <- InQueue50[as.numeric(as.character(InQueue50$Score)) < 25,]
faixa25_50  <- InQueue50[as.numeric(as.character(InQueue50$Score)) >= 25 & as.numeric(as.character(InQueue50$Score)) < 50,]
faixa50_75  <- InQueue50[as.numeric(as.character(InQueue50$Score)) >= 50 & as.numeric(as.character(InQueue50$Score)) < 75,]
faixa75_100 <- InQueue50[as.numeric(as.character(InQueue50$Score)) >= 75,]

maxnrow <- max (nrow(faixa0_25), nrow(faixa25_50), nrow(faixa50_75), nrow(faixa75_100))
# maxScore <- max (na.omit(as.numeric(as.character(faixa0_25$Score))), 
#                  na.omit(as.numeric(as.character(faixa25_50$Score))), 
#                  na.omit(as.numeric(as.character(faixa50_75$Score))), 
#                  na.omit(as.numeric(as.character(faixa75_100$Score))))

boxData <- data.frame(c(100*(1-(faixa0_25$Pos_Entrada/faixa0_25$TamFila)), rep(NA, maxnrow-max(nrow(faixa0_25))+1)),
                      c(100*(1-(faixa25_50$Pos_Entrada/faixa25_50$TamFila)), rep(NA, maxnrow-max(nrow(faixa25_50))+1)),
                      c(100*(1-(faixa50_75$Pos_Entrada/faixa50_75$TamFila)), rep(NA, maxnrow-max(nrow(faixa50_75))+1)),
                      c(100*(1-(faixa75_100$Pos_Entrada/faixa75_100$TamFila)), rep(NA, maxnrow-max(nrow(faixa75_100))+1))
)

colnames(boxData) <- c("0-25", "25-50", "50-75", "75-100")

maxRation <-max(na.omit(boxData$`0_25`),
                na.omit(boxData$`25_50`),
                na.omit(boxData$`50_75`),
                na.omit(boxData$`75_100`))

summary(boxData/maxRation)

boxplot(boxData, main = c("Descalocamento nas filas para tarefas com filas maiores que 5"), xlab = "Intervalos de score dos usuários", ylab = "Porcentagem de ganho de fila")

######################################### Fim gráfico 5 ########################################################


dev.off()
