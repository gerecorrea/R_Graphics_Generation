#Código com gráficos WT e também de log de filas
#Realizado pelo Pillon em 06/07/2020.
library("readxl")
library(gplots)
library(plotly)
library(plotrix)
library(RColorBrewer)
source ("ICLib.R")

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

posQueen <- make_queen_position (FCFSFila)
#print(queen_data)
print (job_queen (FCFSFila, "w0!1663258", posQueen))
fila_data <- get_queen(FCFSFila, 12)
print (fila_data)

# número de usuários diferentes no sistema
numOfuser <- length (unique(FCFS_data$username))
# número total de jobs do sistema
numOfjbos <- nrow (FCFS_data)
# lista de usuários
listUser <- as.character(unique(FCFS_data$username))

colName <- c ("username", "NumOfJobs", "Min", "Max", "Mean", "Median", "StandartD")
SWtUsersFCFS <- statsWtUsers(FCFS_data)
colnames(SWtUsersFCFS) <- colName
SWtUsersSCO1 <- statsWtUsers(SCO1_data)
colnames(SWtUsersSCO1) <- colName
SWtUsersSCO2 <- statsWtUsers(SCO2_data)
colnames(SWtUsersSCO2) <- colName




############################################
### Gráficos ####
pdf ("graph.pdf", width = 11)
par(mfrow = c(1,1))

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
