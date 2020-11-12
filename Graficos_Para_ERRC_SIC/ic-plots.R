
#Autor: Geremias Corrêa
#Adaptado de Maurício Pillon
#Objetivo: criação dos gráficos para artigo da IC e SIC da bolsa

#Anotações:
  #MODIFICAR OS USUÁRIOS TESTE ATÉ TER ALGUNS BONS PRA SAÍDA!
  #3 usuários reais alvo - dados daquele inicial, com fictícios pode alterar!: 
    #Ddelabroye (ruim, media 244, 100+ jobs em fila), 
    #Gfurzin (usuário até bom, média 49, 41 jobs em fila)
    #Elavoeie (Um terceiro bom, média de 53.9, 17 jobs em fila de 40)
    #Obs: os melhores são jbouchoucha e Ardazemar, mas eles possuem poucos jobs em fila para comparação.
    #Obs²: A saida do DDelabroye e Elavoeie não são boas, pegar outro?
  # 3 usuários ficítios, o perfeito, o médio e o horrível! (esse teremos com certeza)
    #userPerfeito (100 jobs, todos perfeição no envio)
    #userMeioTermo (100 jobs, metade perfeito metade horrível)
    #userHorrivel (100 jobs, todos horríveis)

  #Os dados de entrada são novos, com três fictícios. Mantive o padrão pelo .xslx ao invés de .csv, talvez mudar dps
  #Arrumei diversos detalhes pontuais que estavam errados, assim como os que precisei adaptar por precisar.

#Mais anotações (importantes) dessa construção no README_Graficos.txt

#ADAPTANDO O CÓDIGO PARA OUTRAS SAÍDAS:
  #A saída atual está para todos os jobs e média (não mediana)
  #Editar a entrada nos arquivos em questão, caso seja outras base de dados na linha "filenames <- c(..."
    #Lembrando que caso altere a disposição dos usuários e etc talvez se modifique também!
  #Caso queira pegar somente com fila ou independente disso, no ICLib deixei um trecho onde só descmonetar o requerido (atualmente só com fila)
    #Lembrando que isso também pode alterar a ordem de disposição dos usuários!
  #Caso queira pegar a média ou mediana, alternar de 6 para 5 (nos dois gráficos), respectivamente, a linha abaixo na parte válida do código:
    #ex: bardata <- data.frame(as.numeric(ddata[,6]), upper, lower)
  #Editar o nome de saída do pdf na parte para tal, caso queira
  #OBs geral: A disposição dos usuários muda pq na que pego situações com fila ele pode nem ser adicionado, pois não tem job com fila


library("readxl")
library(gplots)
library(plotly)
library(plotrix)
library(RColorBrewer)
source ("ICLib.R")

# nome dos arquivos de entrada
filenames <- c("v2_FCFS_jobs.xlsx", "v2_SCORE1440_jobs.xlsx", "v2_SCORE10800_jobs.xlsx") #Novos arquivos
# nome das planilhas (considerei o uso somente da primeira planilha por arquivo)
mysheets <- c(excel_sheets(filenames[1]), excel_sheets(filenames[2]), excel_sheets(filenames[3]))

# leitura dos arquivos de entrada
FCFS     <- read_excel(filenames[1], sheet=mysheets[1])
SCO1440 <- read_excel(filenames[2], sheet=mysheets[2])
SCO10800 <- read_excel(filenames[3], sheet=mysheets[3])

# extrai dados p/ os graficos
FCFS_data <- extract_data(FCFS)
SCO1_data <- extract_data(SCO1440)
SCO2_data <- extract_data(SCO10800)

# número de usuários diferentes no sistema
numOfuser <- length (unique(FCFS_data$username))
# número total de jobs do sistema
numOfjbos <- nrow (FCFS_data)
# lista de usuários
listUser <- as.character(unique(FCFS_data$username))

#Crio matrizes de cada algoritmos com os seguintes campos para cada usuário em relação aos jobs deles executados.
colName <- c ("username", "NumOfJobs", "Min", "Max", "Mean", "Median", "somaWT", "mediaScore")
SWtUsersFCFS <- statsWtUsers(FCFS_data) #a função statsWtUsers puxa todo os dados e retorna uma matriz com os dados requeridos (ver ICLib.R).
colnames(SWtUsersFCFS) <- colName
SWtUsersSCO1 <- statsWtUsers(SCO1_data)
colnames(SWtUsersSCO1) <- colName
SWtUsersSCO2 <- statsWtUsers(SCO2_data)
colnames(SWtUsersSCO2) <- colName

############################################
### Gráficos ####
#Caso não queira gerar o pdf, comentar as duas linhas abaixo e a do dev.off()
pdf ("graph.pdf", width = 11) #Gera o arquivo pdf (não obrigatório) de tam 11
par(mfrow = c(1,1)) #Faz em um gráfico somente

# Paleta de cores
mypalette <- brewer.pal(3,"Purples") #Ver mais aqui: http://applied-r.com/rcolorbrewer-palettes/

##############
#Agora pegando os dados dos usuários específicos:
#Obs: para saber a posição, basta abrir os arquivos SWtUsersSCO01 ou SWtUsersSCO02 ou SWtUsersFCFS
#Então olhar o id, coluna mais a esquerda, do usuário que deseja.

#Usuários reais escolhidos (3 - olhar cabeçalho do script):
# Ddelabroye (290 jobs total) = posição 4 - variou pouco, mas vou deixar ele, pq pode ser por conta de ser o v2 e tarefa peq.
user1 <- matrix (c (SWtUsersFCFS[4,], SWtUsersSCO1[4,], SWtUsersSCO2[4,]), nr = 8)
# Gfurzin (61 jobs) = posicao 34 - média de score 45.93 - fixo.
user2 <- matrix (c (SWtUsersFCFS[34,], SWtUsersSCO1[34,], SWtUsersSCO2[34,]), nr = 8)
#Gfurzin obteve boa saída de dados, manter.
# Ardazemar (27 jobs total), posição 47, score médio 68.22 - fixo
user3 <- matrix (c (SWtUsersFCFS[47,], SWtUsersSCO1[47,], SWtUsersSCO2[47,]), nr = 8)

#Usuários testados acima interessantes para analisar
# Gfurzin, score médio 45.93 e com boa saída, posição 24
# Arzezamar, score medio 68.22 e com ótima saída pro 10800, posição 34.
# Usuários muito ruins como mmatomasi, gcasiez e pjacqout ficaram na mesma (possuem tarefas curtas e o walltime ajudou?)
# Ddelabroye tem 290 jobs e um score médio de 2.7 e manteve basicamente sua mediana (por conta do walltime? no v1 talvez piore, verificar)

#Usuários fictícios escolhidos (3 - olhar cabeçalho do script):
# UserHorrivel = posição 5 - média de score 4.8
userFic1 <- matrix (c (SWtUsersFCFS[5,], SWtUsersSCO1[5,], SWtUsersSCO2[5,]), nr = 8)
# UserMeioTermo = posição 8 - média de score 96.14 (sou bonzinho pra dar score pelo visto)
userFic2 <- matrix (c (SWtUsersFCFS[8,], SWtUsersSCO1[8,], SWtUsersSCO2[8,]), nr = 8)
# UserPerfeito = posição 1 - média de score 99
userFic3 <- matrix (c (SWtUsersFCFS[1,], SWtUsersSCO1[1,], SWtUsersSCO2[1,]), nr = 8)

################## GRÁFICO 1 - USUÁRIOS REAIS ####################
ddata <- t(data.frame (user1, user2, user3)) #Pega a transposta deles porque saída tava invertida antes pra user1, ... 
colnames(ddata) <- colName

#Modifiquei para pegar os max e min da mediana dos usuários, porque tirei o max e min dos
# maximo
upper = as.numeric(ddata[,3])
# minimo
lower = as.numeric(ddata[,4])
# dataframe p/ barplot2 - 
#Com 5 pego a média, com 6 a mediana, 7 a soma:
bardata <- data.frame(as.numeric(ddata[,5]), upper, lower)

# construcao do grafico
#Obs: eu acho que esse order ali não tem sentido algum
#bp <- barplot2(bardata[order(bardata[,1]),1], #O antigo do pillon, o order estragava tudo a saída, arrumei.
bp <- barplot2(bardata[,1],#Quero todas as linhas da coluna 1 (mediana), sem max e min
               #Comentando esse plot.ci abaixo tiramos os max e min (existem, só não mostro no gráfico)
               #plot.ci = TRUE, 
               #ci.u = bardata[order(bardata[,1]),2], 
               #ci.l = bardata[order(bardata[,1]),3],
               beside = TRUE, horiz = FALSE,
               ylab = "Waiting time (s)",
               #xlab = "Mediana de waiting time de usuários específicos em situação de filas",
               #col = vtall[order(as.numeric(vtall[,2])),3],
               col = mypalette,
               main = c("Media do waiting time de 3 usuários reais"),
               ylim = c(0, max(bardata[,1])) #Alterei visualmente porque nenhuma mediana ultrapassa esse valor
)
#Para texto identificando o nome dos usuários.
#Aqui trocar por usuário real 1, 2 e 3 dps.
text(bp,-30, c ("",ddata[1,1],"", "",ddata[4,1],"", "",ddata[7,1],""), cex=1, pos=1, xpd=TRUE)
#Para legendas dos nomes dos algoritmos.
legend(1, max(bardata[,1]), c ("FCFS","SCORE1440","SCORE10800"), 
       fill = mypalette,xjust = -2) #antes bardata[,3] por isso sumia, agora tranquilo
######################################################################

################## GRÁFICO 2 - USUÁRIOS FICTÍCIOS ####################
ddataFic <- t(data.frame (userFic1, userFic2, userFic3))
colnames(ddataFic) <- colName

#Setar esses max e min de acordo os dados botidos de mediana dps...
# maximo
upper = as.numeric(ddataFic[,3])
# minimo
lower = as.numeric(ddataFic[,4])
# dataframe p/ barplot2
#Com 5 pego a média, com 6 a mediana, 7 a soma:
bardata2 <- data.frame(as.numeric(ddataFic[,5]), upper, lower)

# construcao do grafico
#bp <- barplot2(bardata2[order(bardata2[,1]),1],
bp <- barplot2(bardata2[,1],
               #Comentando esse plot.ci abaixo tiramos os max e min (existem, só não mostro no gráfico)
               #plot.ci = TRUE, 
               #ci.u = bardata2[order(bardata2[,1]),2], 
               #ci.l = bardata2[order(bardata2[,1]),3],
               beside = TRUE, horiz = FALSE,
               ylab = "Waiting time (s)",
               #xlab = "Médiana de waiting time de usuários específicos em situação de filas",
               #col = vtall[order(as.numeric(vtall[,2])),3],
               col = mypalette,
               main = c("Médiana do waiting time de 3 usuários fictícios"),
               ylim = c(0, max(bardata2[,1])) #Arrumei de olho mesmo, antes era max(bardata2[,3])
)
namesUsersFic <- c("User horrível","User médio","User perfeito"); #Modifico para nao printar o original ruim. Escolher aqui
#Para texto identificando o nome dos usuários.
#Aqui trocar por usuário fictício 1, fictício 2 e fictício 3
text(bp,-30, c ("",namesUsersFic[1],"", "",namesUsersFic[2],"", "",namesUsersFic[3],""), cex=1, pos=1, xpd=TRUE)
#PAra legenda dos algoritmos
legend(1, max(bardata2[,1]), c ("FCFS","SCORE1440","SCORE10800"),
       fill = mypalette, xjust=-2)
######################################################################

################## GRÁFICO 3 - USUÁRIOS 3 REAIS E 3 FICTÍCIOS - tudo num só ####################
ddataTodos <- t(data.frame (user1, user2, user3, userFic1, userFic2, userFic3)) #Pega a transposta deles porque saída tava invertida antes pra user1, ... 
colnames(ddataTodos) <- colName

#Modifiquei para pegar os max e min da mediana dos usuários, porque tirei o max e min dos
# maximo
upper = as.numeric(ddataTodos[,3])
# minimo
lower = as.numeric(ddataTodos[,4])
# dataframe p/ barplot2 - 
#Com 5 pego a média, com 6 a mediana, 7 a soma:
bardata3 <- data.frame(as.numeric(ddataTodos[,5]), upper, lower)

# construcao do grafico
#Obs: eu acho que esse order ali não tem sentido algum
#bp <- barplot2(bardata[order(bardata[,1]),1], #O antigo do pillon, o order estragava tudo a saída, arrumei.
bp <- barplot2(bardata3[,1],#Quero todas as linhas da coluna 1 (mediana), sem max e min
               #Comentando esse plot.ci abaixo tiramos os max e min (existem, só não mostro no gráfico)
               #plot.ci = TRUE, 
               #ci.u = bardata[order(bardata[,1]),2], 
               #ci.l = bardata[order(bardata[,1]),3],
               beside = TRUE, horiz = FALSE,
               ylab = "Waiting time (s)",
               #xlab = "Mediana de waiting time de usuários específicos em situação de filas",
               #col = vtall[order(as.numeric(vtall[,2])),3],
               col = mypalette,
               main = c("Media de waiting time de usuários"),
               ylim = c(0, max(bardata3[,1])) #Alterei visualmente porque nenhuma mediana ultrapassa esse valor
)
#Para texto identificando o nome dos usuários.
#Aqui trocar por usuário real 1, 2 e 3 dps.
#text(bp,-30, c ("",ddataTodos[1,1],"", "",ddataTodos[4,1],"", "",ddataTodos[7,1],"", "",ddataTodos[10,1],"", "",ddataTodos[13,1],"", "",ddataTodos[16,1],""), cex=1, pos=1, xpd=TRUE)
text(bp,-30, c ("","Real 1","", "","Real 2","", "","Real 3","", "","Fictício 1","", "","Fictício 2","", "","Fictício 3",""), cex=1.4, pos=1, xpd=TRUE) #cex é o tamanho do texto
#User real 1 = ddelabroye; user real 2 = gfursin; user real 3 = ardazemar; 
#user fictício 1 = userHorrivel; user fictício 2 = userMeioTermo; user fictício 3 = userPerfeito
#Para legendas dos nomes dos algoritmos.
legend(1, max(bardata3[,1]), c ("FCFS","SCORE1440","SCORE10800"), 
       fill = mypalette,xjust = 0) #antes bardata[,3] por isso sumia, agora tranquilo
    #xjust = - 2 fica centralizado.
######################################################################

################## GRÁFICO 4 - USUÁRIOS 3 REAIS E 3 FICTÍCIOS - Apenas a soma do wt (não média). ####################
ddataTodos <- t(data.frame (user1, user2, user3, userFic1, userFic2, userFic3)) #Pega a transposta deles porque saída tava invertida antes pra user1, ... 
#uso o ddataTodos do graf3 pq não muda os dados.
colnames(ddataTodos) <- colName

#Modifiquei para pegar os max e min da mediana dos usuários, porque tirei o max e min dos
# maximo
upper = as.numeric(ddataTodos[,3])
# minimo
lower = as.numeric(ddataTodos[,4])
# dataframe p/ barplot2 - 
#Com 5 pego a média, com 6 a mediana, 7 a soma:
bardata4 <- data.frame(as.numeric(ddataTodos[,7]), upper, lower)

# construcao do grafico
#Obs: eu acho que esse order ali não tem sentido algum
#bp <- barplot2(bardata[order(bardata[,1]),1], #O antigo do pillon, o order estragava tudo a saída, arrumei.
bp <- barplot2(bardata4[,1],#Quero todas as linhas da coluna 1 (soma), sem max e min
               #Comentando esse plot.ci abaixo tiramos os max e min (existem, só não mostro no gráfico)
               #plot.ci = TRUE, 
               #ci.u = bardata[order(bardata[,1]),2], 
               #ci.l = bardata[order(bardata[,1]),3],
               beside = TRUE, horiz = FALSE,
               ylab = "Waiting time (s)",
               #xlab = "Mediana de waiting time de usuários específicos em situação de filas",
               #col = vtall[order(as.numeric(vtall[,2])),3],
               col = mypalette,
               main = c("Soma de waiting time de usuários"),
               ylim = c(0, max(bardata4[,1])) #Alterei visualmente porque nenhuma mediana ultrapassa esse valor
)
#Para texto identificando o nome dos usuários.
#Aqui trocar por usuário real 1, 2 e 3 dps.
#text(bp,-30, c ("",ddataTodos[1,1],"", "",ddataTodos[4,1],"", "",ddataTodos[7,1],"", "",ddataTodos[10,1],"", "",ddataTodos[13,1],"", "",ddataTodos[16,1],""), cex=1, pos=1, xpd=TRUE)
text(bp,-30, c ("","Real 1","", "","Real 2","", "","Real 3","", "","Fictício 1","", "","Fictício 2","", "","Fictício 3",""), cex=1.4, pos=1, xpd=TRUE) #cex é o tamanho do texto
#User real 1 = ddelabroye; user real 2 = gfursin; user real 3 = ardazemar; 
#user fictício 1 = userHorrivel; user fictício 2 = userMeioTermo; user fictício 3 = userPerfeito
#Para legendas dos nomes dos algoritmos.
legend(1, max(bardata4[,1]), c ("FCFS","SCORE1440","SCORE10800"), 
       fill = mypalette,xjust = -2) #antes bardata[,3] por isso sumia, agora tranquilo
#xjust = - 2 fica centralizado.
######################################################################

################## GRÁFICO 5 - USUÁRIOS 3 REAIS E 3 FICTÍCIOS - mostrando a variação percentual, sendo a base 100%. ####################
ddataTodos <- t(data.frame (user1, user2, user3, userFic1, userFic2, userFic3)) #Pega a transposta deles porque saída tava invertida antes pra user1, ... 
#uso o ddataTodos do graf3 pq não muda os dados.
colnames(ddataTodos) <- colName

#Modifiquei para pegar os max e min da mediana dos usuários, porque tirei o max e min dos
# maximo
upper = as.numeric(ddataTodos[,3])
# minimo
lower = as.numeric(ddataTodos[,4])
# dataframe p/ barplot2 - 
#Com 5 pego a média, com 6 a mediana, 7 a soma:
bardata5 <- data.frame(as.numeric(ddataTodos[,7]), upper, lower)
values<- c(100, (bardata5[1,1]/bardata5[2,1])*100, (bardata5[1,1]/bardata5[3,1])*100,100, (bardata5[4,1]/bardata5[5,1])*100, (bardata5[4,1]/bardata5[6,1])*100,
           100, (bardata5[7,1]/bardata5[8,1])*100, (bardata5[7,1]/bardata5[9,1])*100,100, (bardata5[10,1]/bardata5[11,1])*100, (bardata5[10,1]/bardata5[12,1])*100,
           100, (bardata5[13,1]/bardata5[14,1])*100, (bardata5[13,1]/bardata5[15,1])*100,100, (bardata5[16,1]/bardata5[7,1])*100,(bardata5[16,1]/bardata5[18,1])*100)

# construcao do grafico
#Obs: eu acho que esse order ali não tem sentido algum
#bp <- barplot2(bardata[order(bardata[,1]),1], #O antigo do pillon, o order estragava tudo a saída, arrumei.
bp <- barplot2(values,#Quero todas as linhas da coluna 1 (soma), sem max e min
               #Comentando esse plot.ci abaixo tiramos os max e min (existem, só não mostro no gráfico)
               #plot.ci = TRUE, 
               #ci.u = bardata[order(bardata[,1]),2], 
               #ci.l = bardata[order(bardata[,1]),3],
               beside = TRUE, horiz = FALSE,
               ylab = "Percentual de variação do waiting time",
               #xlab = "Mediana de waiting time de usuários específicos em situação de filas",
               #col = vtall[order(as.numeric(vtall[,2])),3],
               col = mypalette,
               main = c("Variação percentual de redução do waiting time pelos algoritmos em relação ao FCFS"), #FCFS é padrão 100%
               ylim = c(0, max(values)+200) #Alterei visualmente porque nenhuma mediana ultrapassa esse valor
)
#Para texto identificando o nome dos usuários.
#Aqui trocar por usuário real 1, 2 e 3 dps.
#text(bp,-30, c ("",ddataTodos[1,1],"", "",ddataTodos[4,1],"", "",ddataTodos[7,1],"", "",ddataTodos[10,1],"", "",ddataTodos[13,1],"", "",ddataTodos[16,1],""), cex=1, pos=1, xpd=TRUE)
text(bp,-30, c ("","Real 1","", "","Real 2","", "","Real 3","", "","Fictício 1","", "","Fictício 2","", "","Fictício 3",""), cex=1.4, pos=1, xpd=TRUE) #cex é o tamanho do texto
#User real 1 = ddelabroye; user real 2 = gfursin; user real 3 = ardazemar; 
#user fictício 1 = userHorrivel; user fictício 2 = userMeioTermo; user fictício 3 = userPerfeito
#Para legendas dos nomes dos algoritmos.
legend(1, max(values)+200, c ("FCFS","SCORE1440","SCORE10800"), 
       fill = mypalette) #antes bardata[,3] por isso sumia, agora tranquilo
#xjust = - 2 fica centralizado.
######################################################################

################## GRÁFICO 6 - USUÁRIOS 3 REAIS E 3 FICTÍCIOS - mostrando o ganho percentual. ####################
ddataTodos <- t(data.frame (user1, user2, user3, userFic1, userFic2, userFic3)) #Pega a transposta deles porque saída tava invertida antes pra user1, ... 
#uso o ddataTodos do graf3 pq não muda os dados.
colnames(ddataTodos) <- colName

#Modifiquei para pegar os max e min da mediana dos usuários, porque tirei o max e min dos
# maximo
upper = as.numeric(ddataTodos[,3])
# minimo
lower = as.numeric(ddataTodos[,4])
# dataframe p/ barplot2 - 
#Com 5 pego a média, com 6 a mediana, 7 a soma:
bardata5 <- data.frame(as.numeric(ddataTodos[,7]), upper, lower)
values<- c(0, 100-(bardata5[2,1]/bardata5[1,1])*100, 100-(bardata5[3,1]/bardata5[1,1])*100,0, 100-(bardata5[5,1]/bardata5[4,1])*100, 100-(bardata5[6,1]/bardata5[4,1])*100,
           0, 100-(bardata5[8,1]/bardata5[7,1])*100, 100-(bardata5[9,1]/bardata5[7,1])*100,0, 100-(bardata5[11,1]/bardata5[10,1])*100, 100-(bardata5[12,1]/bardata5[10,1])*100,
           0, 100-(bardata5[14,1]/bardata5[13,1])*100, 100-(bardata5[15,1]/bardata5[13,1])*100,0, 100-(bardata5[17,1]/bardata5[16,1])*100,100-(bardata5[18,1]/bardata5[16,1])*100)


# construcao do grafico
#Obs: eu acho que esse order ali não tem sentido algum
#bp <- barplot2(bardata[order(bardata[,1]),1], #O antigo do pillon, o order estragava tudo a saída, arrumei.
bp <- barplot2(values,#Quero todas as linhas da coluna 1 (soma), sem max e min
               #Comentando esse plot.ci abaixo tiramos os max e min (existem, só não mostro no gráfico)
               #plot.ci = TRUE, 
               #ci.u = bardata[order(bardata[,1]),2], 
               #ci.l = bardata[order(bardata[,1]),3],
               beside = TRUE, horiz = FALSE,
               ylab = "Percentual de redução do waiting time",
               #xlab = "Mediana de waiting time de usuários específicos em situação de filas",
               #col = vtall[order(as.numeric(vtall[,2])),3],
               col = mypalette,
               main = c("Redução percentual do waiting time"),
               ylim = c(-20, max(values)+20) #Alterei visualmente porque nenhuma mediana ultrapassa esse valor
)
#Para texto identificando o nome dos usuários.
#Aqui trocar por usuário real 1, 2 e 3 dps.
#text(bp,-30, c ("",ddataTodos[1,1],"", "",ddataTodos[4,1],"", "",ddataTodos[7,1],"", "",ddataTodos[10,1],"", "",ddataTodos[13,1],"", "",ddataTodos[16,1],""), cex=1, pos=1, xpd=TRUE)
text(bp,-25, c ("","Real 1","", "","Real 2","", "","Real 3","", "","Fictício 1","", "","Fictício 2","", "","Fictício 3",""), cex=1.4, pos=1, xpd=TRUE) #cex é o tamanho do texto
#User real 1 = ddelabroye; user real 2 = gfursin; user real 3 = ardazemar; 
#user fictício 1 = userHorrivel; user fictício 2 = userMeioTermo; user fictício 3 = userPerfeito
#Para legendas dos nomes dos algoritmos.
legend(1, max(values)+20, c ("FCFS","SCORE1440","SCORE10800"), 
       fill = mypalette) #antes bardata[,3] por isso sumia, agora tranquilo
#xjust = - 2 fica centralizado.
######################################################################

################## GRÁFICO 7 - USUÁRIOS 3 REAIS E 3 FICTÍCIOS - mostrando o ganho percentual, tirando o FCFS dele. ####################
ddataTodos <- t(data.frame (user1, user2, user3, userFic1, userFic2, userFic3)) #Pega a transposta deles porque saída tava invertida antes pra user1, ... 
#uso o ddataTodos do graf3 pq não muda os dados.
colnames(ddataTodos) <- colName

#Modifiquei para pegar os max e min da mediana dos usuários, porque tirei o max e min dos
# maximo
upper = as.numeric(ddataTodos[,3])
# minimo
lower = as.numeric(ddataTodos[,4])
# dataframe p/ barplot2 - 
#Com 5 pego a média, com 6 a mediana, 7 a soma:
bardata5 <- data.frame(as.numeric(ddataTodos[,7]), upper, lower)
values<- c(100-(bardata5[2,1]/bardata5[1,1])*100, 100-(bardata5[3,1]/bardata5[1,1])*100, 100-(bardata5[5,1]/bardata5[4,1])*100, 100-(bardata5[6,1]/bardata5[4,1])*100,
           100-(bardata5[8,1]/bardata5[7,1])*100, 100-(bardata5[9,1]/bardata5[7,1])*100, 100-(bardata5[11,1]/bardata5[10,1])*100, 100-(bardata5[12,1]/bardata5[10,1])*100,
           100-(bardata5[14,1]/bardata5[13,1])*100, 100-(bardata5[15,1]/bardata5[13,1])*100, 100-(bardata5[17,1]/bardata5[16,1])*100,100-(bardata5[18,1]/bardata5[16,1])*100)


# construcao do grafico
#Obs: eu acho que esse order ali não tem sentido algum
#bp <- barplot2(bardata[order(bardata[,1]),1], #O antigo do pillon, o order estragava tudo a saída, arrumei.
bp <- barplot2(values,#Quero todas as linhas da coluna 1 (soma), sem max e min
               #Comentando esse plot.ci abaixo tiramos os max e min (existem, só não mostro no gráfico)
               #plot.ci = TRUE, 
               #ci.u = bardata[order(bardata[,1]),2], 
               #ci.l = bardata[order(bardata[,1]),3],
               beside = TRUE, horiz = FALSE,
               ylab = "Percentual de redução do waiting time",
               #xlab = "Mediana de waiting time de usuários específicos em situação de filas",
               #col = vtall[order(as.numeric(vtall[,2])),3],
               col = mypalette,
               main = c("Redução percentual do waiting time"),
               ylim = c(-20, max(values)+20) #Alterei visualmente porque nenhuma mediana ultrapassa esse valor
)
#Para texto identificando o nome dos usuários.
#Aqui trocar por usuário real 1, 2 e 3 dps.
#text(bp,-30, c ("",ddataTodos[1,1],"", "",ddataTodos[4,1],"", "",ddataTodos[7,1],"", "",ddataTodos[10,1],"", "",ddataTodos[13,1],"", "",ddataTodos[16,1],""), cex=1, pos=1, xpd=TRUE)
text(bp,-30, c ("Real 1","","Real 2", "","Real 3", "","Fictício 1", "","Fictício 2", "","Fictício 3",""), cex=1.4, pos=1, xpd=TRUE) #cex é o tamanho do texto
#Problema de identação dos usuários em relação aos seus dados.

#User real 1 = ddelabroye; user real 2 = gfursin; user real 3 = ardazemar; 
#user fictício 1 = userHorrivel; user fictício 2 = userMeioTermo; user fictício 3 = userPerfeito
#Para legendas dos nomes dos algoritmos.
legend(1, max(values)+20, c ("SCORE1440","SCORE10800"), 
       fill = mypalette) #antes bardata[,3] por isso sumia, agora tranquilo
#xjust = - 2 fica centralizado.
######################################################################

dev.off()
