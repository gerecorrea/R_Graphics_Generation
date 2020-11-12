library("readxl")
library(gplots)
library(plotly)
library(plotrix)
library(RColorBrewer)
source ("ICLibv2.R")

# nome dos arquivos de entrada
filenames <- c("FCFS_jobs.xlsx", "SCORE1440_scoreOk_jobs.xlsx", "SCORE10800_scoreOk_jobs.xlsx")
fileFilas <- c("FCFS_logFilas.csv", "SCORE1440_logFilas.csv", "SCORE10800_logFilas.csv")
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

listOfQueue <- make_queue_position (SCO10Fila)
summary(listOfQueue[1:nrow(listOfQueue),4])
allJobQueueSCO10 <- getAllJobList (SCO10Fila, SCO10800, listOfQueue, nrow(SCO1_data))
df <- data.frame (as.numeric(allJobQueueSCO10[,1]), 
                  allJobQueueSCO10[,2], 
                  allJobQueueSCO10[,3], 
                  as.numeric(allJobQueueSCO10[,4]))
colnames(df) <- c("TamFila", "Score", "Job", "Pos_Entrada")


### Com fila maior que 10 Jobs
InQueue10 <- na.omit(df[df$TamFila > 10,])
colnames(InQueue10) <- c("TamFila", "Score", "Job", "Pos_Entrada")

faixa0_25   <- InQueue10[as.numeric(as.character(InQueue10$Score)) < 25,]
faixa25_50  <- InQueue10[as.numeric(as.character(InQueue10$Score)) > 25 & as.numeric(as.character(InQueue10$Score)) < 50,]
faixa50_75  <- InQueue10[as.numeric(as.character(InQueue10$Score)) > 50 & as.numeric(as.character(InQueue10$Score)) < 75,]
faixa75_100 <- InQueue10[as.numeric(as.character(InQueue10$Score)) > 75,]

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
colnames(boxData) <- c("0_25", "25_50", "50_75", "75_100")

maxRation <-max(na.omit(boxData$`0_25`),
                na.omit(boxData$`25_50`),
                na.omit(boxData$`50_75`),
                na.omit(boxData$`75_100`))

summary(boxData/maxRation)

############################################
### Gráficos ####

pdf ("graph-v02.pdf", width = 11)
par(mfrow = c(1,1))

boxplot(boxData, main = c("Fila > 10"), xlab = "Score", ylab = "razão de deslocamento na fila")

### Com fila maior que 50 Jobs
InQueue50 <- na.omit(df[df$TamFila > 50,])
colnames(InQueue10) <- c("TamFila", "Score", "Job", "Pos_Entrada")

faixa0_25   <- InQueue50[as.numeric(as.character(InQueue50$Score)) < 25,]
faixa25_50  <- InQueue50[as.numeric(as.character(InQueue50$Score)) > 25 & as.numeric(as.character(InQueue50$Score)) < 50,]
faixa50_75  <- InQueue50[as.numeric(as.character(InQueue50$Score)) > 50 & as.numeric(as.character(InQueue50$Score)) < 75,]
faixa75_100 <- InQueue50[as.numeric(as.character(InQueue50$Score)) > 75,]

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
colnames(boxData) <- c("0_25", "25_50", "50_75", "75_100")

maxRation <-max(na.omit(boxData$`0_25`),
                na.omit(boxData$`25_50`),
                na.omit(boxData$`50_75`),
                na.omit(boxData$`75_100`))

summary(boxData/maxRation)

boxplot(boxData, main = c("Fila > 50"), xlab = "Score", ylab = "razão de deslocamento na fila")

# print (job_queue (SCO10Fila, "w0!1658564", listOfQueue))
# print (get_queue (SCO10Fila, 1))
# print (job_queue (SCO10Fila, "w0!1658608", listOfQueue))
# print (get_queue (SCO10Fila, 4))

listOfQueue <- make_queue_position (SCO11Fila)
summary(listOfQueue[1:nrow(listOfQueue),4])
allJobQueueSCO11 <- getAllJobList (SCO11Fila, SCO11440, listOfQueue, nrow(SCO2_data))



colName <- c ("username", "NumOfJobs", "Min", "Max", "Mean", "Median", "StandartD")
SWtUsersFCFS <- statsWtUsers(FCFS_data)
colnames(SWtUsersFCFS) <- colName
SWtUsersSCO1 <- statsWtUsers(SCO1_data)
colnames(SWtUsersSCO1) <- colName
SWtUsersSCO2 <- statsWtUsers(SCO2_data)
colnames(SWtUsersSCO2) <- colName



# Paleta de cores
mypalette <- brewer.pal(3,"YlGn")

##############
# ardazemar = posicao 31
arda <- matrix (c (SWtUsersFCFS[31,], SWtUsersSCO1[31,], SWtUsersSCO2[31,]), nr = 7)
# matommasi = posicao 1
mato <- matrix (c (SWtUsersFCFS[1,], SWtUsersSCO1[1,], SWtUsersSCO2[1,]), nr = 7)
# gcasiez = posicao 29
gcas <- matrix (c (SWtUsersFCFS[29,], SWtUsersSCO1[29,], SWtUsersSCO2[29,]), nr = 7)

#### Gráfico do Gere Replicado no R ####
ddata <- t(data.frame (arda, mato, gcas))
colnames(ddata) <- colName

# maximo
upper = as.numeric(ddata[,3])
# minimo
lower = as.numeric(ddata[,4])
# dataframe p/ barplot2
bardata <- data.frame(as.numeric(ddata[,5]), upper, lower)

# construcao do grafico
bp <- barplot2(bardata[order(bardata[,1]),1],
               plot.ci = TRUE, 
               ci.u = bardata[order(bardata[,1]),2], 
               ci.l = bardata[order(bardata[,1]),3],
               beside = TRUE, horiz = FALSE,
               ylab = "Waiting_time (s)",
               xlab = "Mediana de Waiting_time por Usuário (0s removidos)",
               #col = vtall[order(as.numeric(vtall[,2])),3],
               col = mypalette,
               main = c("Waiting_Time - FCFS"),
               ylim = c(0, max(bardata[,3]))
)
text(bp,-30, c ("",ddata[1,1],"", "",ddata[4,1],"", "",ddata[7,1],""), cex=1, pos=1, xpd=TRUE)
legend(1, max(bardata[,3]), c ("FCFS","Score 1440","Score 10800"),
       fill = mypalette)

### FCFS  ####
# mediana + desvio padrao
upper = as.numeric(SWtUsersFCFS[,3])
# mediana - desvio padrao
lower = as.numeric(SWtUsersFCFS[,4])
# dataframe p/ barplot2
bardata <- data.frame(as.numeric(SWtUsersFCFS[,5]), upper, lower)

# construcao do grafico
bp <- barplot2(bardata[order(bardata[,1]),1],
               plot.ci = TRUE, 
               ci.u = bardata[order(bardata[,1]),2], 
               ci.l = bardata[order(bardata[,1]),3],
               beside = TRUE, horiz = FALSE,
               ylab = "Waiting_time (s)",
               xlab = "Mediana de Waiting_time por Usuário (0s removidos)",
               #col = vtall[order(as.numeric(vtall[,2])),3],
               #col = cor,
               main = c("Waiting_Time - FCFS")#,
               #ylim = c(0, 2000)
)

#### SCO1 ####
# mediana + desvio padrao
upper = as.numeric(SWtUsersSCO1[,3])
# mediana - desvio padrao
lower = as.numeric(SWtUsersSCO1[,4])
# dataframe p/ barplot2
bardata <- data.frame(as.numeric(SWtUsersSCO1[,5]), upper, lower)

# construcao do grafico
bp <- barplot2(bardata[order(bardata[,1]),1],
               plot.ci = TRUE, 
               ci.u = bardata[order(bardata[,1]),2], 
               ci.l = bardata[order(bardata[,1]),3],
               beside = TRUE, horiz = FALSE,
               #col = vtall[order(as.numeric(vtall[,2])),3],
               #col = cor,
               ylab = "Waiting_time (s)",
               xlab = "Mediana de Waiting_time por Usuário (0s removidos)",
               main = c("Waiting_Time - SCO1")#,
               #ylim = c(0, 2000)
)

#### SCO2 ####
# mediana + desvio padrao
upper = as.numeric(SWtUsersSCO2[,3])
# mediana - desvio padrao
lower = as.numeric(SWtUsersSCO2[,4])
# dataframe p/ barplot2
bardata <- data.frame(as.numeric(SWtUsersSCO2[,5]), upper, lower)

# construcao do grafico
bp <- barplot2(bardata[order(bardata[,1]),1],
               plot.ci = TRUE, 
               ci.u = bardata[order(bardata[,1]),2], 
               ci.l = bardata[order(bardata[,1]),3],
               beside = TRUE, horiz = FALSE,
               ylab = "Waiting_time (s)",
               xlab = "Mediana de Waiting_time por Usuário (0s removidos)",
               #col = vtall[order(as.numeric(vtall[,2])),3],
               #col = cor,
               main = c("Waiting_Time - SCO2")#,
               #ylim = c(0, 2000)
)

dev.off()
