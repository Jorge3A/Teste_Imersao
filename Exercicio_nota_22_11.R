########################### EXERC�CIO DE APRENDIZAGEM E FIXA��O #################
#DATA DA �LTIMA ATUALIZA��O: 26/11/2022

#PASSO 1 - LIMPAR O CONSOLE
rm(list=ls(all=TRUE))

# PASSO 2 - DEFINIR O DIRETORIO DE TRABALHO 
setwd("C:/Users/Jorge/Desktop/Especializa��o_HOC")

# PASSO 3 - INSTALA��O DOS PACOTES NECESS�RIOS
install.packages("tidyverse", dependencies = T)
install.packages("data.table", dependencies = T)
install.packages("ggplot2", dependencies = T)
install.packages('dplyr', dependencies = T)
install.packages('car', dependencies = T)
install.packages("PerformanceAnalytics")
install.packages("readxl")
install.packages("rhandsontable")


#PASSO 4 - CARREGAMENTO DAS BIBLIOTECAS
library(data.table)
library(dplyr)
library(ggplot2)
library(car)
library(PerformanceAnalytics)

#PASSO 5 - INSTALAR OS PACOTES
require(ggplot2)
require(haven)
require(dplyr)
require(tidyverse) 
require(stats)
require(vcd)
require(corrplot)
library(tidyverse)
library(caret)
library(leaps)

#PASSO 6 - IMPORTAR O BANCO DE DADOS NO FORMATO XLSX
#OBS 1: ANTES DE USAR O COMANDO ABAIXO, MUDAR O DIRET�RIO PARA A PASTA ONDE SE
       #LOCALIZA O BANCO DE DADOS
#OBS 2: MUDAR A \ POR / NO CAMINHO DO DIRET�RIO
library(readxl)
data <- read_excel("C:/Users/Jorge/Desktop/Especializa��o_HOC/Banco_de_Dados/Expec_Vida.xlsx")

#PASSO 7 - "LIBERAR" O BANCO DE DADOS ATRAV�S DA FUN��O attach
attach(data)
head(data)
dim(data)
class(data)

#PASSO 8 - EXPLORAR O BANCO DE DADOS
#Padroniza��o das vari�veis do banco de dados
names(data)
glimpse(data)

#PASSO 9 - INSPE��O DOS DADOS E SEUS COMPONENTES 
#OBS: TODO OBJETO TEM UMA CLASSE (data.frame (dados), factor, numeric,etc)
class(data$Expec_Vida)
class(data$IDH)
class(data$Pais)
class(data$Status)
#OBS: Abrir o banco de dados
View(data)

#PASSO 10 - CONVERTER character EM numeric
data$Expec_Vida <- as.numeric(data$Expec_Vida)
class(data$Expec_Vida)

#PASSO 11 - REALIZAR AN�LISE DESCRITIVA
summary(data[4:21])
table(data$Status)
prop.table(table(data$Status))
sapply(data[4:21], median)
sapply(data[4:21], mean)

#PASSO 12 - Mostrar a estrutura das v�ri�veis do banco de dados
str(data)

#PASSO 13 - CONVERTER A VARIAVEL STATUS PARA BIN�RIA
Status2 <- c("Em_Desenvolvimento"=0, "Desenvolvido"=1)
#MOstra a var�avel Status do banco de dados
data$binStatus <- Status2[data$Status]
View(data)

#Gr�fico de setores Variavel Status
tabela <- table(data$Status)
pie(tabela)

#Histograma Expectativa de Vida
hist(data$Expec_Vida)

#Histograma do IDH
hist(data$IDH)

#Histograma do IMC
hist(data$IMC)

# Gr�fico de pontos IDH x Expec_Vida (dispers�o/scatter)
ggplot(data = data,
       mapping = aes(
         x = IDH,
         y = Expec_Vida))+
  geom_point()

# Gr�fico de Pontos Escolaridade x Expec_Vida
ggplot(data = data,
       mapping = aes(
         x = Anos_escolaridade,
         y = Expec_Vida))+
  geom_point()

# Gr�fico de Pontos IMC x IDH
ggplot(data = data,
       mapping = aes(
         x = IDH,
         y = IMC))+
  geom_point()

# Gr�fico de pontos IMC x Expec_Vida (dispers�o/scatter)
ggplot(data = data,
       mapping = aes(
         x = IMC,
         y = Expec_Vida))+
  geom_point()

#Passo 15 Investigando Multicolinearidade

cor(data[4:18])

M<-cor(data[4:18])
M

# Definir 2 casas decimais
head(round(M,2))

corrplot(M, method = "circle")

# Passo 16 Gr�fico de Dispers�o

# Nome das vari�veis
names(data)

x<-data$IDH
y<-data$Anos_escolaridade

#Gr�fico Renda x Anos de Escolaridade
plot(x,y,xlab="Renda", ylab="Escolaridade")
qplot(x,y,xlab="Renda", ylab="Anos de estudo") # forma alternativa

#Gr�fico Cobertura Vacinal DTP x Cobretura Vacinal He_B
qplot(data$Cobert_Vac_DTP,data$Cobert_Vac_Hep_B,xlab="Difteria", ylab="Hepatite B") 

# Passo 17 Correla��o entre renda e anos de escolaridade

cor(x,y)

cor.test(x,y) # teste e IC para \rho

# Cobert_Vac_DTP x Cobert_Vac_Hep_B
cor(data$Cobert_Vac_DTP, data$Cobert_Vac_Hep_B)

cor.test(data$Cobert_Vac_DTP,data$Cobert_Vac_Hep_B) # teste e IC para \rho

# Gr�fico da Preval_Magreza_10_19_anos x Preval_Magreza_5_9_anos
qplot(data$Preval_Magreza_10_19_anos,data$Preval_Magreza_5_9_anos,xlab="Preval_Magreza_10_19_anos", ylab="Preval_Magreza_5_9_anos")

# Passo 18 Correla��o entre Prevalencia de magreza 10_19 anos x Prevalencia de magreza 5_9 anos
cor(data$Preval_Magreza_10_19_anos,data$Preval_Magreza_5_9_anos)

cor.test(data$Preval_Magreza_10_19_anos,data$Preval_Magreza_5_9_anos) # teste e IC para \rho

## Passo 19 Correla��o entre Despesa Total x Expectativa de vida
#Grafico de dispers�o Despesa Total x Expectativa de vida
plot(data$Despesa_total, data$Expec_Vida)
qplot(data$Despesa_total, data$Expec_Vida,xlab="Despesa_total", ylab="Expec_Vida") 

# Correla��o Despesa Total x Expectativa de vida
cor.test(data$Despesa_total, data$Expec_Vida)

# Gr�fico boxplot Despesa Total x Expectativa de vida
boxplot(data$Expec_Vida ~ data$Status, xlab="", ylab="Expectativa de vida")

# Gr�fico boxplot Expectativa de vida nos pa�ses
ggplot(data, aes(x = Expec_Vida, y = Status, color=Expec_Vida)) +
  geom_boxplot() +
  theme_minimal() +
  labs(y = "",
       x = "Expectativa de vida")

# Passo 20 An�lise descritiva do desfecho

# Exemplo de uso de histograma
hist(data$Expec_Vida, main = "Histograma da expectativa de vida",
     xlab = "Expectativa de vida", ylab = "Frequ�ncia")

boxplot(data$Expec_Vida, main = "Boxplot da expectativa de vida", ylab = "Expectativa de vida")

# Descritivas da expectativa de vida
median(data$Expec_Vida)
quantile(data$Expec_Vida, 0.25) # Primeiro quartil
quantile(data$Expec_Vida, 0.75) # Terceiro quartil
percentis <- quantile(data$Expec_Vida, seq(0, 1, 0.1))

#Histograma da Expectativa de vida
hist(data$Expec_Vida, main = "Histograma da expectativa de vida",
     xlab = "Expectativa de vida", ylab = "Frequ�ncia")

# Uso de linhas verticais sobrepondo o gr�fico inicial
abline(v = percentis, lty = 2, col = 2)

# Passo 21 Modelo de regress�o
#Selecionando apenas vari�veis num�ricas
banco1 <-data[4:22]
View(banco1)
str(banco1)

#Passo 22 Gerando a matriz de correla��o e os gr�ficos de dispers�o 
chart.Correlation(R = banco1[,3:18], 
                  histogram = F, method = "pearson")

#Passo 23 Definindo as vari�veis preditoras pelo m�todo Forward
str(data)
data$Status <- as.factor(data$Status)
class(data$Status)

# Passo 24 definindo o intercept only model 
baseModel = lm(Expec_Vida~1, data = banco1)

# Passo 24 definindo o modelo com todos os preditores
independentes <- lm(Expec_Vida ~ ., data = banco1)

#Modelo Final
modelo_final <- step(baseModel,scope=list(upper=independentes,lower=~1),direction="forward")
stepreg1 <- step(independentes, direction = "forward", k = 5)
summary(stepreg1)

#Outra alternativa de an�lise
#Gerarando um modelo sem vari�veis explicativas, somente com o intercepto
# Modelo somente com o intercepto
MRL.Vazio <- lm(formula = Expec_Vida ~ 1, banco1)
summary(MRL.Vazio)

#Gerarando um modelo com todas as vari�veis explicativas.
MRL.Completo <- lm(formula = Expec_Vida ~ ., banco1)
summary(MRL.Completo)

#Regress�o M�todo Forward
MRL.Forward <- step(MRL.Vazio,
                    scope= list(lower= MRL.Vazio, upper=MRL.Completo),
                    direction = "forward")
summary(MRL.Forward)

#Passo 25 calcular o VIF para cada vari�vel preditora no modelo
library(car)
library(carData)
vif(MRL.Forward)
vif_values <- vif(MRL.Forward)

#Gr�fico de barras horizontais para exibir cada valor VIF
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")

#add vertical line at 4
abline(v = 5, lwd = 4, lty = 2)

#Akaike's An Information Criterion (AIC)                    

# Verificando as suposi��es do Modelo

#  Gr�fico 1: vari�ncia constante e linearidade
#  Gr�fico 2: normalidade
#  Gr�fico 3: somente vari�ncia constante
#  Gr�fico 4: observa��es at�picas
#
names(MRL.Forward)
#  Gr�ficos para verificar a adequa��o do ajuste
# Gr�fico 1: vari�ncia constante e linearidade
par(mfrow=c(2,2))
plot(MRL.Forward, which=c(1:4), add.smooth=T,pch=16)

#  Um teste simples para verificar vari�ncia n�o-constante
#   Este teste n�o � completamente correto pois algum peso
#   deve ser utilizado e os graus de liberdade ajustados
#   Veja Faraway (2004)
#
summary(lm(abs(residuals(MRL.Forward))~fitted(MRL.Forward)))
#
#  Gr�fico para verificar independ�ncia (na sequ�ncia de 
#    coleta dos dados), se fizer sentido.
#
par(mfrow=c(1,1))
plot(MRL.Forward$residuals,pch=16,ylab="Residuals")

##########################################

# 
par(mfrow=c(1,1))
boxplot(MRL.Forward$residuals)

#  Teste para verificar normalidade 
shapiro.test(MRL.Forward$residuals)

#  Um teste simples para verificar vari�ncia n�o-constante
#   Este teste n�o � completamente correto pois algum peso
#   deve ser utilizado e os graus de liberdade ajustados
#   Veja Faraway (2004)

library(car)

qqPlot(MRL.Forward$residuals,  xlab="Percentil da N(0,1)", ylab="Res�duo studentizado")

outlierTest(MRL.Forward)

data[108,]
fitted(MRL.Forward)[108]

library(car) 
avPlots(MRL.Forward, ask=FALSE, id.method="identify")

influencePlot(MRL.Forward, id.method="identify", sub="Circle size is proportional to Cook's distance")

#
#  1- Excluir as observa��es at�picas o  caso 12 (at�pico)
#     Somente v�lido se houver uma justificativa para exclu�-lo

banco2<-banco1[-12,]
dim(banco2)

model_outlier1 <- lm(Expec_Vida ~ IDH + Mortalidade_Adultos + Obito_HIV_AIDS + Despesa_total, data=banco2)  

summary(model_outlier1)

par(mfrow=c(2,2))
plot(model_outlier1, which=c(1:4), add.smooth=T,pch=20)
shapiro.test(model_outlier1$resid)
summary(lm(abs(residuals(model_outlier1))~fitted(model_outlier1)))

#  1- Excluir as observa��es at�picas o  caso 108 (at�pico)
#     Somente v�lido se houver uma justificativa para exclu�-lo

banco2<-banco1[-108,]
dim(banco2)

model_outlier1 <- lm(Expec_Vida ~ IDH + Mortalidade_Adultos + Obito_HIV_AIDS + Despesa_total, data=banco2)

summary(model_outlier1)

par(mfrow=c(2,2))
plot(model_outlier1, which=c(1:4), add.smooth=T,pch=20)
shapiro.test(model_outlier1$resid)
summary(lm(abs(residuals(model_outlier1))~fitted(model_outlier1)))

outlierTest(model_outlier1)

data[12,]
fitted(MRL.Forward)[12]

# Retirando as duas observa��es 12 e 108
banco2<-banco1[c(-12, -108),]
dim(banco2)

model_outlier1 <- lm(Expec_Vida ~ IDH + Mortalidade_Adultos + Obito_HIV_AIDS + Despesa_total, data=banco2)  

summary(model_outlier1)

par(mfrow=c(2,2))
plot(model_outlier1, which=c(1:4), add.smooth=T,pch=20)
shapiro.test(model_outlier1$resid)
summary(lm(abs(residuals(model_outlier1))~fitted(model_outlier1)))

outlierTest(model_outlier1)

qqPlot(model_outlier1,  xlab="Percentil da N(0,1)", ylab="Res�duo studentizado")

summary(model_outlier1)

summary(MRL.Forward)
#
# Observe que a qualidade do ajuste melhorou em todos os aspectos.

# A conclus�o continua mostrando uma associa��o entre as vari�veis.
# No entanto, observa-se que houve varia��o nos betas.

par(mfrow=c(2,2))
hist(data$Expec_Vida)
hist(data$IMC)
hist(data$IDH)
hist(data$Anos_escolaridade)

