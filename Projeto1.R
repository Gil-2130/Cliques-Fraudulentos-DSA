################################## Projeto 01 ##################################

# Detecção de Fraudes no Tráfego de Cliques em Propagandas de Aplicações Mobile

# Introdução:

# O risco de fraude está em toda parte, mas para as empresas que anunciam
# online, a fraude de cliques pode acontecer em um volume avassalador,
# resultando em dados de cliques enganosos e dinheiro desperdiçado. Os canais de
# anúncios podem aumentar os custos simplesmente quando pessoas ou bots
# clicam nos anúncios em grande escala, o que na prática não gera o resultado
# esperado.

# Com mais de 1 bilhão de dispositivos móveis em uso todos os meses, a
# China é o maior mercado móvel do mundo e, portanto, sofre com grandes
# volumes de tráfego fraudulento.

# A TalkingData (https://www.talkingdata.com), a maior plataforma de Big
# Data independente da China, cobre mais de 70% dos dispositivos móveis ativos
# em todo o país. Eles lidam com 3 bilhões de cliques por dia, dos quais 90% são
# potencialmente fraudulentos. Sua abordagem atual para impedir fraudes de
# cliques para desenvolvedores de aplicativos é medir a jornada do clique de um
# usuário em todo o portfólio e sinalizar endereços IP que produzem muitos cliques,
# mas nunca acabam instalando aplicativos.

# Com essas informações, eles criaram
# uma lista negra de IPs e uma lista negra de dispositivos.
# Embora bem-sucedidos, eles querem estar sempre um passo à frente dos
# fraudadores e pediram a sua ajuda para desenvolver ainda mais a solução. Você
# está desafiado a criar um algoritmo que possa prever se um usuário fará o
# download de um aplicativo depois de clicar em um anúncio de aplicativo para
# dispositivos móveis.

# Para este projeto contruiremos um modelo de aprendizado de máquina para determinar
# se um clique é fraudulento ou não

# Não temos informaçoes suficientes sobre os tipos de aplicativos, dispositivos,
# Sistemas Operacionais e os canais de marketing que a empresa usa para podermos associá-los aos labels.

# Bibliotecas necessárias para o projeto
# caso algum desses pacotes não esteja instalado, realize a instalação
# 'install.packages' + nome do pacote
library(kableExtra)
library(lubridate)
library(Amelia)
library(dplyr)
library(ggplot2)
library(viridis)
library(gridExtra)
library(paletteer)
library(plotrix)
library(caTools)
library(DMwR)
library(randomForest)
library(forcats)
library(e1071)
library(caret)
library(ROCR)

# Carregando a amostra de Dataset fornecida pelo Kaggle
# https://www.kaggle.com/c/talkingdata-adtracking-fraud-detection/data?select=sample_submission.csv
# Devido ao tamanho do arquivo, iremos trabalhar apenas com a amostra dos dados (train_sample)
sample_df <- read.csv('train_sample.csv')
View(sample_df)
str(sample_df)

# segundo informções do kaggle, as variaveis 'ip','app','os', 'channel' e 'device'
# são codificadas ou seja; estão com o numéricas mas são categóricas.
# Também temos as variaveis 'click_time' e 'attributed_time' que são do tipo
# character, iremos tranformá-las em fatoriais do tipo data
#
#
# Drop da variavle 'IP' pois não iremos utilizá-la
sample_df$ip <- NULL

# Tranformação de variaveis inteiras em categóricas
sample_df$app <- as.factor(sample_df$app)
sample_df$device <- as.factor(sample_df$device)
sample_df$os <- as.factor(sample_df$os)
sample_df$channel <- as.factor(sample_df$channel)

# Transformação variavel Target
sample_df$is_attributed <- as.factor(sample_df$is_attributed)

# Verificando o dataset após transformaçoes
str(sample_df)


# Transformando as colunas 'click_time' e 'attributed_time' em data
# usando o pacote lubridate
sample_df$click_time <- ymd_hms(sample_df$click_time)
sample_df$attributed_time <- ymd_hms(sample_df$attributed_time)
str(sample_df)


# Também criaremos uma coluna especificando o tempo entre o 'click_time' e o 'attributed_time'
sample_df$inter_time <- as.numeric(sample_df$attributed_time - sample_df$click_time)
sample_df$inter_time[is.na(sample_df$inter_time)] <- 0
str(sample_df)
View(sample_df)

# Nosso dataset após sofrer modificações,contém dados missing.
# Iremos verificar e em seguida removê-los
missmap(sample_df,
        main = 'TalkingData - Detecção de cliques Fraudulentos - Mapeamento de Dados Ausentes',
        col = c('yellow', 'black'),
        legend = FALSE)

# O gráfico acima é um pouco pesado, então sugiro que aguarde a renderização do mesmo
# Agora que o gráfico está pronto, fica claro que apenas a coluna 'attributed_time'
# possui valoers ausentes

# Análise Exploratória

# Plot das variaveis categóricas
# gráficos de colunas em grid

# Plot por Aplicativo
plot_app <- sample_df %>%
    group_by(app) %>%
    count(sort = T) %>%
    head(5) %>%
    ggplot(aes(x = reorder(app, -n), y = n, fill = -n)) +
    geom_col(show.legend = F) +
    labs(title = 'Quantidades de Cliques P/Aplicativo',
         x = 'App',
         y = NULL) +
    scale_fill_viridis(option = 'viridis', direction = 1) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))

# plot por dispositivos
plot_device <- sample_df %>%
    group_by(device) %>%
    count(sort = T) %>%
    head(5) %>%
    ggplot(aes(x = reorder(device, -n), y = n, fill = -n)) +
    geom_col(show.legend = F) +
    labs(title = 'Qauntidade de Cliques P/Dispositivos',
         x = 'Devices',
         y = NULL) +
    scale_fill_viridis(option = 'viridis', direction = 1) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))

# plot por sistema operacional
plot_os <- sample_df %>%
    group_by(os) %>%
    count(sort = T) %>%
    head(5) %>%
    ggplot(aes(x = reorder(os, -n), y = n, fill = -n)) +
    geom_col(show.legend = F) +
    labs(title = 'Quantidade de Cliques P/SO',
         x ='OS',
         y = NULL) +
    scale_fill_viridis(option = 'viridis', direction = 1) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))

# Plot por canal de Marketing
plot_channel <- sample_df %>%
    group_by(channel) %>%
    count(sort = T) %>%
    head(5) %>%
    ggplot(aes(x = reorder(channel, -n), y = n, fill = -n)) +
    geom_col(show.legend = F) +
    labs(title = 'Quantidades de cliques P/Canais',
         x = 'Canais',
         y = NULL) +
    scale_fill_viridis(option = 'viridis', direction = 1) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot_app, plot_device, plot_os, plot_channel,
             ncol = 2,
             nrow = 2)


# Série temporal, plotada em facet por dia. do horário que houve mais cliques.
# Gráfico de pontos e linhas em facet-grid

sample_df %>%
    mutate(dia_semana = wday(click_time, label = T)) %>%
    mutate(hora = hour(click_time)) %>%
    group_by(hora, dia_semana) %>%
    count() %>%
    ungroup() %>%
    ggplot(aes(x = hora, y = n, color = dia_semana)) +
    geom_point(show.legend = F) + geom_line(show.legend = F) +
    facet_grid(~ dia_semana) +
    labs(title = 'Cliques Por dia e Horário',
         y = NULL) +
    theme(plot.title = element_text(hjust = 0.5))


# Média de tempo entre clique e download para os dias da semana
# Barplot

sample_df %>%
    filter(is_attributed == 1) %>%
    mutate(dia_semana = wday(attributed_time, label = T)) %>%
    group_by(dia_semana) %>%
    summarise(media = mean(inter_time)) %>%
    ungroup() %>%
    ggplot(aes(x = reorder(dia_semana, media), y = media, fill = dia_semana)) +
    geom_col(show.legend = F) + coord_flip() +
    labs(title ='Média de tempo até o download',
         subtitle = 'Medição em segundo por dia da semana',
         x = NULL, y = NULL) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))

# Contagem de cliques que se converteram em download(1) X não se converteram(0)
# Piechart

cor <- as.vector(paletteer_c('viridis::viridis', n = 2, direction = -1))

tabela <- table(sample_df$is_attributed)
tabela <- prop.table(tabela) * 100

download <- c('Sim', 'Não')
labels <- paste(download, tabela)
labels <- paste(labels, '%', sep = '')

pie3D(tabela, labels = labels,
      col = cor,
      main = 'Cliques para Download',
      theta = pi/4)

#################################MACHINE LEARNING###############################
#
#
#
#
#
# Split dos dados de treino e teste
# Utilizaremos a função sample.split do pacote caTools para dividir o dataset em treino e teste
# proporção de 70% para dados de treino e 30% para teste
sample_split <- sample_df[, -c(5, 6, 8)]

indice <- sample.split(sample_split$is_attributed, SplitRatio = .7)

treino <- subset(sample_split, indice == T)
teste <- subset(sample_split, indice == F)

#As colunas de data precisam ser retiradas pois como sabemos, não efetuamos nennhum tipo de calculo nelas
#e por consequente, também não serão úteis na criação do modelo.


############################BALANCEAMENTO DE CLASSE#######################
#
#
#
#
#
# Conforme nossa análise exploratória, identificamos que a variavel target sofre com desbalanceamento,
# visto que a quantidade de cliques fraudulentos é maior que o de cliques de download.
# Iremos utilizar a técnica SMOTE para aplicar o balanceamento e diminuir a desigualdade

d_treino <- as.vector(round(prop.table(table(treino$is_attributed)) * 100,4))
d_teste <- as.vector(round(prop.table(table(teste$is_attributed)) * 100, 4))

unbalanced_data <- data.frame(treino = paste(d_treino, '%'),
                              teste = paste(d_teste, '%'),
                              row.names = c('is_attributed = 0 (apps não baixados)',
                                            'is_attibuted = 1 (apps baixados)'))

smote_treino <- SMOTE(is_attributed ~ ., data = treino)

balanc_treino <- as.vector(round(prop.table(table(smote_treino$is_attributed)) * 100, 4))

balanced <- data.frame(proporcao = paste(balanc_treino, '%'),
                       row.names = c('is_attributed = 0 (apps não baixados)',
                                     'is-attributed = 1 (apps baixados)'))


##############################FEATURE SELECTION##############################
#
#
#
#
#
# utilizaremos o Random Forest para identificar as variaveis mais relevantes para nosso modelo.

# Obs; primeiramente utilizamos os parâmetros com 50 categorias, porém obtivemoso erro;
# "Error in randomForest.default(m, y, ...) :"
# "Can not handle categorical predictors with more than 53 categories."
# Então alteramos para 40 categorias e não obtivemos mais erros

func1 <- smote_treino %>%
    select(-is_attributed) %>%
    mutate(app = fct_lump(app, n = 40),
           device = fct_lump(app, n = 40),
           os = fct_lump(app, n = 40),
           channel = fct_lump(app, n = 40))

func2 <- smote_treino %>%
    select(is_attributed)

treino_funcs <- cbind(func1, func2)


modelo_funcs <- randomForest(is_attributed ~ .,
                             data = treino_funcs,
                             importance = T)

varImpPlot(modelo_funcs,
           main = 'Feature Selection com Random Forest')


# Treinando o modelo
modelo <- naiveBayes(is_attributed ~ app
                     + channel
                     + os,
                     data = smote_treino)

previsão <- predict(modelo, teste)


###########################AVALIAÇÂO DO MODELO###########################
#
#
#
#
#
# Confusion Matrix
# Iremos Avaliar a performance do modelo
confusionMatrix(teste$is_attributed, previsão)

# Conclusão: nosso modelo teve uma boa acurácia e baixos erros. No entanto salientamos que trabalhamos
# apenas com uma pequena amostra dos dados, pois o dataset é muito grande para uma máquina modesta como
# a minha.


##############################CURVA ROC##############################
#
#
#
#
#
# Para Melhorar a ilustração, criaremos a curva ROC (Receiver Operations Characteristic)
# Exemplificando; 
# taxa de VP = Sensibilidade = Quantidade de acertos do modelo para a opção positiva (0- não fez download, 1- fex o download)
# operação => acertos / (acertos + erros) => 29314 / (29314 + 618) = 97,97% de acurácia e precisão de 99,95%

# Taxa de FP = Especificidade = Quatidade de vezes que o modelo errou, fazendo previsões erroneamente.
# operação => erros / (erros + acertos) => 12 / (12 + 56) = 82,35%

# Ou seja; nosso modelo está preciso e com uma taxa de falsos positivos bem pequena também!

prev_model <- prediction(as.numeric(previsão), teste$is_attributed)
perf_model <- performance(prev_model, "tpr", "fpr")

plot(perf_model, col = 'red',
     main = 'Curva ROC',
     ylab = 'Sensibilidade (97,97)',
     xlab = '1-Especificidade (82,35%)')


###########################CONSIDERAÇÕES FINAIS#######################
# 
#
# O Objetivo principal é detectar o máximo posível de VP (Verdadeiros Positivos), e minimizar
# a taxa de falsos positivos.
#
# De acordo com a curva ROC, alcançamos esse objetivo!


# Salvando o Projeto
library(rmarkdown)
rmarkdown::render("Projeto1.R", "pdf_document")
