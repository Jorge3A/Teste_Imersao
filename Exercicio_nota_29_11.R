########################### EXERCÍCIO DE APRENDIZAGEM E FIXAÇÃO #################
#DATA DA ÚLTIMA ATUALIZAÇÃO: 29/11/2022

#PASSO 1 - LIMPAR O CONSOLE
rm(list=ls(all=TRUE))

# PASSO 2 - DEFINIR O DIRETORIO DE TRABALHO 
setwd("C:/Users/Jorge/Desktop/Especialização_HOC")

# PASSO 3 - INSTALAÇÃO DOS PACOTES NECESSÁRIOS
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
#OBS 1: ANTES DE USAR O COMANDO ABAIXO, MUDAR O DIRETÓRIO PARA A PASTA ONDE SE
       #LOCALIZA O BANCO DE DADOS
#OBS 2: MUDAR A \ POR / NO CAMINHO DO DIRETÓRIO
library(readxl)
data <- read_excel("C:/Users/Jorge/Desktop/Especialização_HOC/Banco_de_Dados/Expec_Vida.xlsx")

#PASSO 7 - "LIBERAR" O BANCO DE DADOS ATRAVÉS DA FUNÇÃO attach
attach(data)
head(data)
dim(data)
class(data)

#PASSO 8 - EXPLORAR O BANCO DE DADOS
#Padronização das variáveis do banco de dados
names(data)
glimpse(data)

#PASSO 9 - INSPEÇÃO DOS DADOS E SEUS COMPONENTES 
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

#PASSO 11 - REALIZAR ANÁLISE DESCRITIVA
summary(data[4:21])
table(data$Status)
prop.table(table(data$Status))
sapply(data[4:21], median)
sapply(data[4:21], mean)

#PASSO 12 - Mostrar a estrutura das váriáveis do banco de dados
str(data)

#PASSO 13 - CONVERTER A VARIAVEL STATUS PARA BINÁRIA
Status2 <- c("Em_Desenvolvimento"=0, "Desenvolvido"=1)
#MOstra a varíavel Status do banco de dados
data$binStatus <- Status2[data$Status]
View(data)

#Gráfico de setores Variavel Status
tabela <- table(data$Status)
pie(tabela)

#Histogramas
par(mfrow=c(2,2))
hist(data$Expec_Vida)
hist(data$IMC)
hist(data$IDH)
hist(data$Anos_escolaridade)

# Gráfico de pontos IDH x Expec_Vida (dispersão/scatter)
ggplot(data = data,
       mapping = aes(
         x = IDH,
         y = Expec_Vida))+
  geom_point()

##ggplot(data = data,
       mapping = aes(
         x = IDH, 
         y = Expec_Vida))+
        col = (data$Status)


# Gráfico de Pontos Escolaridade x Expec_Vida
ggplot(data = data,
       mapping = aes(
         x = Anos_escolaridade,
         y = Expec_Vida))+
  geom_point()

# Gráfico de Pontos IMC x IDH
ggplot(data = data,
       mapping = aes(
         x = IDH,
         y = IMC))+
  geom_point()

# Gráfico de pontos IMC x Expec_Vida (dispersão/scatter)
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
corrplot(M, method = "number")

# Passo 16 Gráfico de Dispersão

# Nome das variáveis
names(data)

x<-data$IDH
y<-data$Anos_escolaridade

#Gráfico Renda x Anos de Escolaridade
plot(x,y,xlab="Renda", ylab="Escolaridade")
qplot(x,y,xlab="Renda", ylab="Anos de estudo") # forma alternativa

#Gráfico Cobertura Vacinal DTP x Cobretura Vacinal He_B
qplot(data$Cobert_Vac_DTP,data$Cobert_Vac_Hep_B,xlab="Difteria", ylab="Hepatite B") 

# Passo 17 Correlação entre renda e anos de escolaridade

cor(x,y)

cor.test(x,y) # teste e IC para \rho

# Cobert_Vac_DTP x Cobert_Vac_Hep_B
cor(data$Cobert_Vac_DTP, data$Cobert_Vac_Hep_B)

cor.test(data$Cobert_Vac_DTP,data$Cobert_Vac_Hep_B) # teste e IC para \rho

# Gráfico da Preval_Magreza_10_19_anos x Preval_Magreza_5_9_anos
qplot(data$Preval_Magreza_10_19_anos,data$Preval_Magreza_5_9_anos,xlab="Preval_Magreza_10_19_anos", ylab="Preval_Magreza_5_9_anos")

# Passo 18 Correlação entre Prevalencia de magreza 10_19 anos x Prevalencia de magreza 5_9 anos
cor(data$Preval_Magreza_10_19_anos,data$Preval_Magreza_5_9_anos)

cor.test(data$Preval_Magreza_10_19_anos,data$Preval_Magreza_5_9_anos) # teste e IC para \rho

## Passo 19 Correlação entre Despesa Total x Expectativa de vida
#Grafico de dispersão Despesa Total x Expectativa de vida
plot(data$Despesa_total, data$Expec_Vida)
qplot(data$Despesa_total, data$Expec_Vida,xlab="Despesa_total", ylab="Expec_Vida") 

# Correlação Despesa Total x Expectativa de vida
cor.test(data$Despesa_total, data$Expec_Vida)

# Gráfico boxplot Despesa Total x Expectativa de vida
boxplot(data$Expec_Vida ~ data$Status, xlab="", ylab="Expectativa de vida")

# Gráfico boxplot Expectativa de vida nos países
ggplot(data, aes(x = Expec_Vida, y = Status, color=Expec_Vida)) +
  geom_boxplot() +
  theme_minimal() +
  labs(y = "",
       x = "Expectativa de vida")

# Passo 20 Análise descritiva do desfecho

# Exemplo de uso de histograma
hist(data$Expec_Vida, main = "Histograma da expectativa de vida",
     xlab = "Expectativa de vida", ylab = "Frequência")

boxplot(data$Expec_Vida, main = "Boxplot da expectativa de vida", ylab = "Expectativa de vida")

# Descritivas da expectativa de vida
median(data$Expec_Vida)
quantile(data$Expec_Vida, 0.25) # Primeiro quartil
quantile(data$Expec_Vida, 0.75) # Terceiro quartil
percentis <- quantile(data$Expec_Vida, seq(0, 1, 0.1))

#Histograma da Expectativa de vida
hist(data$Expec_Vida, main = "Histograma da expectativa de vida",
     xlab = "Expectativa de vida", ylab = "Frequência")

# Uso de linhas verticais sobrepondo o gráfico inicial
abline(v = percentis, lty = 2, col = 2)

# Passo 21 Modelo de regressão
#Selecionando apenas variáveis numéricas
banco1 <-data[4:22]
View(banco1)
str(banco1)

#Passo 22 Gerando a matriz de correlação e os gráficos de dispersão 
chart.Correlation(R = banco1[,3:18], 
                  histogram = F, method = "pearson")

#Passo 23 Definindo as variáveis preditoras pelo método Forward
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

#Outra alternativa de análise
#Gerarando um modelo sem variáveis explicativas, somente com o intercepto
# Modelo somente com o intercepto
MRL.Vazio <- lm(formula = Expec_Vida ~ 1, banco1)
summary(MRL.Vazio)

#Gerarando um modelo com todas as variáveis explicativas.
MRL.Completo <- lm(formula = Expec_Vida ~ ., banco1)
summary(MRL.Completo)

#Regressão Método Forward
MRL.Forward <- step(MRL.Vazio,
                    scope= list(lower= MRL.Vazio, upper=MRL.Completo),
                    direction = "forward")
summary(MRL.Forward)

#Passo 25 calcular o VIF para cada variável preditora no modelo
library(car)
library(carData)
vif(MRL.Forward)
vif_values <- vif(MRL.Forward)

#Gráfico de barras horizontais para exibir cada valor VIF
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")

#add vertical line at 4
abline(v = 5, lwd = 4, lty = 2)

#Akaike's An Information Criterion (AIC)                    

# Verificando as suposições do Modelo

#  Gráfico 1: variância constante e linearidade
#  Gráfico 2: normalidade
#  Gráfico 3: somente variância constante
#  Gráfico 4: observações atípicas
#
names(MRL.Forward)
#  Gráficos para verificar a adequação do ajuste
# Gráfico 1: variância constante e linearidade
par(mfrow=c(2,2))
plot(MRL.Forward, which=c(1:4), add.smooth=T,pch=16)

#  Um teste simples para verificar variância não-constante
#   Este teste não é completamente correto pois algum peso
#   deve ser utilizado e os graus de liberdade ajustados
#   Veja Faraway (2004)
#
summary(lm(abs(residuals(MRL.Forward))~fitted(MRL.Forward)))
#
#  Gráfico para verificar independência (na sequência de 
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

#  Um teste simples para verificar variância não-constante
#   Este teste não é completamente correto pois algum peso
#   deve ser utilizado e os graus de liberdade ajustados
#   Veja Faraway (2004)

library(car)

qqPlot(MRL.Forward$residuals,  xlab="Percentil da N(0,1)", ylab="Resíduo studentizado")

outlierTest(MRL.Forward)

data[108,]
fitted(MRL.Forward)[108]

library(car) 
avPlots(MRL.Forward, ask=FALSE, id.method="identify")

influencePlot(MRL.Forward, id.method="identify", sub="Circle size is proportional to Cook's distance")

#
#  1- Excluir as observações atípicas o  caso 12 (atípico)
#     Somente válido se houver uma justificativa para excluí-lo

banco2<-banco1[-12,]
dim(banco2)

model_outlier1 <- lm(Expec_Vida ~ IDH + Mortalidade_Adultos + Obito_HIV_AIDS + Despesa_total, data=banco2)  

summary(model_outlier1)

par(mfrow=c(2,2))
plot(model_outlier1, which=c(1:4), add.smooth=T,pch=20)
shapiro.test(model_outlier1$resid)
summary(lm(abs(residuals(model_outlier1))~fitted(model_outlier1)))

#  1- Excluir as observações atípicas o  caso 108 (atípico)
#     Somente válido se houver uma justificativa para excluí-lo

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

# Retirando as duas observações 12 e 108
banco2<-banco1[c(-12, -108),]
dim(banco2)

model_outlier1 <- lm(Expec_Vida ~ IDH + Mortalidade_Adultos + Obito_HIV_AIDS + Despesa_total, data=banco2)  

summary(model_outlier1)

par(mfrow=c(2,2))
plot(model_outlier1, which=c(1:4), add.smooth=T,pch=20)
shapiro.test(model_outlier1$resid)
summary(lm(abs(residuals(model_outlier1))~fitted(model_outlier1)))

outlierTest(model_outlier1)

qqPlot(model_outlier1,  xlab="Percentil da N(0,1)", ylab="Resíduo studentizado")

summary(model_outlier1)

summary(MRL.Forward)
#
# Observe que a qualidade do ajuste melhorou em todos os aspectos.

# A conclusão continua mostrando uma associação entre as variáveis.
# No entanto, observa-se que houve variação nos betas.

dffits <- as.data.frame(dffits(MRL.Forward))

