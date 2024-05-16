################################################################################
#                   REGRESSÃO LOGÍSTICA BINÁRIA - PARTE CONCEITUAL             #
################################################################################
#Estabelecendo uma função para a probabilidade de ocorrência de um evento
prob <- function(z){
  prob = 1 / (1 + exp(-z))
}

#Plotando a curva sigmóide teórica de ocorrência de um evento para um range
#do logito z entre -5 e +5
data.frame(z = -5:5) %>%
  ggplot() +
  stat_function(aes(x = z, color = "Prob. Evento"),
                fun = prob,
                size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  labs(x = "Logito z",
       y = "Probabilidade") +
  theme_bw()

################################################################################
#                     Dataset para Regressão Logística                         #
################################################################################
# Carregar o conjunto de dados a partir de um arquivo .rds
dataset_news_gaza_israel_reg_log <- readRDS("dataset_news_gaza_israel.rds")

# Calcular a soma das emoções positivas e negativas para cada linha do conjunto de dados
# e criar uma nova variável categórica indicando se a notícia é predominantemente positiva ou não
dataset_news_gaza_israel_reg_log <- dataset_news_gaza_israel_reg_log |> 
  mutate(
    sum_emo_pos = rowSums(select(dataset_news_gaza_israel_reg_log, c(word_anticipation, word_trust, word_joy, word_surprise))),
    sum_emo_neg = rowSums(select(dataset_news_gaza_israel_reg_log, c(word_anger, word_disgust, word_fear, word_sadness))),
    emo_positiva = as.factor(ifelse(sum_emo_pos > sum_emo_neg, "1", "0"))
  ) |> 
  
  # Selecionar apenas as colunas relevantes para análise
  select(
    emo_positiva,
    nomes_paises,
    ano,
    viewCount,
    likeCount,
    commentCount,
    words,
    topicos
  )
#------------------------------------------------------------------------------#
# Substituir todos os valores NA por 0 no conjunto de dados
dataset_news_gaza_israel_reg_log[is.na(dataset_news_gaza_israel_reg_log)] <- 0
#------------------------------------------------------------------------------#
# Adicionando novas variáveis categóricas baseadas nos cortes dos quantis das contagens de palavras, visualizações, likes e comentários
dataset_news_gaza_israel_reg_log <- dataset_news_gaza_israel_reg_log |> 
  dplyr::mutate(
    # Criando a variável 'words_cut' com cortes dos quantis das contagens de palavras
    count_words = as.factor(
      cut(
        words, 
        breaks = quantile(words, probs = 0:10/10, na.rm = TRUE), 
        right = FALSE, 
        labels = c(
          "Words D1 [195,297)",
          "Words D2 [297,311)",
          "Words D3 [311,322)",
          "Words D4 [322,335)",
          "Words D5 [335,347)",
          "Words D6 [347,360)",
          "Words D7 [360,373)",
          "Words D8 [373,387)",
          "Words D9 [387,404)",
          "Words D10 [404,488)"
        )
      )
    ),
    # Criando a variável 'count_views' com cortes dos quantis das contagens de visualizações
    count_views = as.factor(
      cut(
        viewCount, 
        breaks = quantile(viewCount, probs = 0:10/10, na.rm = TRUE), 
        right = FALSE, 
        labels = c(
          "Views D1 [0,1.280)",
          "Views D2 [1.280,2.510)",
          "Views D3 [2.510,4.340)",
          "Views D4 [4.340,6.840)",
          "Views D5 [6.840,10.700)",
          "Views D6 [10.700,16.700)",
          "Views D7 [16.700,26.700)",
          "Views D8 [26.700,46.900)",
          "Views D9 [46.900,98.400)",
          "Views D10 [98.400,3.660.000)"
        )
      )
    ),
    # Criando a variável 'count_like' com cortes dos quantis das contagens de likes
    count_like = as.factor(
      cut(
        likeCount, 
        breaks = quantile(likeCount, probs = 0:10/10, na.rm = TRUE), 
        right = FALSE, 
        labels = c(
          "Like D1 [0,13)",
          "Like D2 [13,29)",
          "Like D3 [29,50)",
          "Like D4 [50,83)",
          "Like D5 [83,136)",
          "Like D6 [136,216)",
          "Like D7 [216,347)",
          "Like D8 [347,606)",
          "Like D9 [606,1.250)",
          "Like D10 [1.250,36.400)"
        )
      )
    ),
    # Criando a variável 'count_comment' com cortes dos quantis das contagens de comentários
    count_comment = as.factor(
      cut(
        commentCount, 
        breaks = quantile(commentCount, probs = 0:10/10, na.rm = TRUE), 
        right = FALSE, 
        labels = c(
          "Comment D1 [0,13)",
          "Comment D2 [13,29)",
          "Comment D3 [29,50)",
          "Comment D4 [50,83)",
          "Comment D5 [83,136)",
          "Comment D6 [136,216)",
          "Comment D7 [216,347)",
          "Comment D8 [347,606)",
          "Comment D9 [606,1.250)",
          "Comment D10 [1.250,36.400)"
        )
      )
    )
  ) |> 
  # Removendo as colunas 'words', 'viewCount', 'likeCount', 'commentCount'
  select(-c(words, viewCount, likeCount, commentCount)) |> 
  # Removendo as linhas com valores NA
  drop_na()
################################################################################
# Configurando o idioma e formatação para o gtsummary
theme_gtsummary_language("pt", big.mark = ".", decimal.mark = ",")

# Gerando a tabela resumida com gtsummary
dataset_news_gaza_israel_reg_log |>  
  dplyr::rename(polaridade = emo_positiva) |>   
  dplyr::mutate(polaridade = as.factor(case_when(polaridade == 0 ~ "Negativa", TRUE ~ "Positiva"))) |>  
  dplyr::mutate(polaridade = fct_relevel(polaridade, "Positiva", "Negativa")) %>%
  tbl_summary(
    by = polaridade,
    label = list(
      ano = "Ano de publicação da notícia",
      nomes_paises = "País do jornal",
      topicos = "Temas das notícias por Modelagem de Tópicos (LDA)",
      canal = "Jornais",
      count_words = "Decis da quantidade de palavras das notícias",
      count_views = "Decis da quantidade de visualizações das notícias",
      count_like = "Decis da quantidade de Likes nos vídeos",
      count_comment = "Decis da quatidade de comentários nas notícias"
    )
  ) |> 
  add_n() |> 
  modify_header(label ~ "**Variáveis (X)**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Polaridade das notícias (Y)**") |>  
  bold_labels()|> 
  as_flex_table(font.size = 10) |>  
  flextable::save_as_docx(path = "tabela_amostra.docx")

################################################################################
#                      ANÁLISE DE REGRESSÃO LOGÍSTICA                          #
################################################################################
#------------------------ PROCEDIMENTO N-1 DUMMIES ----------------------------#

#Dummizando as variáveis ano, nomes_paises, topicos, count_words, count_views,
# count_like E count_comment. O código abaixo, automaticamente, fará:
# a) a dummização das variáveis originais;
# b)removerá as variáveis dummizadas originais;
# c) estabelecerá como categorias de referência as categorias de label 1 de cada variável original.
reg_log_dummies <- dummy_columns(.data = dataset_news_gaza_israel_reg_log,
                                 select_columns = c("ano", 
                                                    "nomes_paises",
                                                    "topicos", 
                                                    "count_words", 
                                                    "count_views",
                                                    "count_like",
                                                    "count_comment"),
                                 remove_selected_columns = T,
                                 remove_first_dummy = T)

################################################################################
#                             REESTIMANDO O MODELO                             #
################################################################################
# Criando um modelo de regressão logística com variáveis dummy
modelo_reg_log_dummies <- glm(
  formula = emo_positiva ~ .,  # Fórmula da regressão logística
  data = reg_log_dummies,      # Conjunto de dados
  family = "binomial"          # Distribuição binomial para a família
)

# Parâmetros do modelo_reg_log_dummies 
summary(modelo_reg_log_dummies)

# Valor do LL do modelo_reg_log_dummies
logLik(modelo_reg_log_dummies)

# Procedimento Stepwise
# Realizando a seleção de variáveis usando o método stepwise
step_modelo_reg_log_dummies <- step(
  object = modelo_reg_log_dummies,  # Modelo de regressão logística
  k = qchisq(p = 0.05, df = 1, lower.tail = FALSE)  # Critério de seleção (teste qui-quadrado)
)

# Carregando o modelo stepwise de um arquivo .rds
step_modelo_reg_log_dummies <- readRDS("step_modelo_reg_log_dummies.rds")

#Parâmetros dostep_modelo_reg_log_dummies
summary(step_modelo_reg_log_dummies)

# Criar o gráfico de coeficientes personalizado
coefplot(step_modelo_reg_log_dummies, 
         intercept = FALSE, 
         vertical = FALSE,
         pointSize = 2,
         color = "blue",
         linetype = 1) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),  # Estilo do título do gráfico
    plot.caption = element_text(size = 10),  # Estilo da legenda do gráfico
    axis.title.y = element_text(size = 12, face = "bold"),  # Estilo do título do eixo y
    axis.title.x = element_text(size = 12, face = "bold"),  # Estilo do título do eixo x
    strip.text.x = element_text(size = 10, colour= "white", face= "bold"),  # Estilo do texto do eixo x
    strip.background = element_rect(colour = "black", fill= "black"),  # Cor de fundo do eixo x
    axis.text.x = element_text(face = "bold", color = "black", size = 10),  # Estilo do texto do eixo x
    axis.text.y = element_text(face = "bold", color = "black", size = 10)) +  # Estilo do texto do eixo y
  labs(x = "Coeficiente", y = "Variável")  # Alterar os rótulos dos eixos

# Extraindo a tabela do step_modelo_reg_log_dummies
# Transformando o resultado do modelo em um dataframe e realizando cálculos e formatações adicionais
tidy(step_modelo_reg_log_dummies) |> 
  
  # Convertendo para um dataframe
  as.data.frame() %>% 
  
  # Calculando e arredondando (exp(B)-1) x 100 (Efeitos Marginais: Probablidade de chances)
  # e Odds Ratio Exp(B): Razão de Chance do evento
  dplyr::mutate("(exp(B)-1) x 100" = round((exp(estimate)-1)*100, 2),
                "Odds Ratio Exp(B)" = round((exp(estimate)), 2)) |>  
  
  # Arredondando valores de p-value, estimativas dos parâmetros, erro padrão e Z(Wald)
  dplyr::mutate("Sig (P>|z|)" = round(p.value, 3),
                "Estimativas dos parâmetros (βx)" = round(estimate, 3),
                "Erro Padrão" = round(std.error, 3),
                "Z(Wald)" = round(statistic, 3)) |>  
  
  # Removendo prefixos dos nomes das variáveis explicativas
  dplyr::mutate(term = gsub("nomes_paises_", "", term),
                term = gsub("ano_", "", term),
                term = gsub("topicos_", "", term),
                term = gsub("count_words_", "", term),
                term = gsub("count_views_", "", term),
                term = gsub("count_comment_", "", term)) |>  
  
  # Renomeando a coluna de variáveis explicativas
  dplyr::rename("Variáveis explicativas (Xi)" = term) |>  
  
  # Selecionando as colunas desejadas
  dplyr::select(`Variáveis explicativas (Xi)`, `Estimativas dos parâmetros (βx)`,
                `Odds Ratio Exp(B)`, `(exp(B)-1) x 100`, `Z(Wald)`, `Sig (P>|z|)`,
                `Erro Padrão`) -> tidy_summary 
tidy_summary

# Criando uma tabela flextable com o resumo do modelo de regressão
flextable(tidy_summary) |>  
  # Aplicando negrito no cabeçalho da tabela
  bold(part = "header") |>  
  # Salvando a tabela em um arquivo .docx
  save_as_docx(path = "tabela_regressao.docx")

################################################################################
#                     CONSTRUÇÃO DE UMA MATRIZ DE CONFUSÃO                     #
################################################################################
# Calculando a matriz de confusão
confusionMatrix <- confusionMatrix(
  # Criando uma tabela de contingência entre as previsões do modelo e os valores reais
  table(predict(step_modelo_reg_log_dummies, type = "response") >= 0.5, # Previsões do modelo com base na probabilidade de ser positiva
        dataset_news_gaza_israel_reg_log$emo_positiva == 1)             # Valores reais (1 para emoção positiva)
  [2:1, 2:1]  # Reordenando a matriz para exibir a forma convencional (verdadeiro negativo, falso negativo, verdadeiro positivo, falso positivo)
)

# Exibindo a matriz de confusão
confusionMatrix

#------------------------------------------------------------------------------#
# Criando um data frame com a matriz de confusão
tabela_matriz_confusao <- data.frame(
  # Obtendo a matriz de confusão do objeto confusionMatrix e selecionando as células relevantes
  confusionMatrix(
    table(
      predict(step_modelo_reg_log_dummies, type = "response") >= 0.5, # Previsões do modelo
      dataset_news_gaza_israel_reg_log$emo_positiva == 1              # Valores reais
    )[2:1, 2:1]  # Reordenando a matriz para exibir a forma convencional
  )$table
)  |> 
  # Renomeando as colunas
  dplyr::rename(
    "Referencia" = Var1,  # Rótulo da referência (verdadeiro negativo, verdadeiro positivo)
    "Predicao" = Var2     # Rótulo da previsão (negativo, positivo)
  ) %>%
  # Convertendo os fatores para representar os rótulos de forma mais clara
  dplyr::mutate(
    Referencia = as.factor(
      case_when(
        Referencia == "TRUE" ~ "Positiva",  # Se a referência for verdadeira, é positiva
        TRUE ~ "Negativa"                   # Caso contrário, é negativa
      )
    ),
    Predicao = as.factor(
      case_when(
        Predicao == "FALSE" ~ "Negativa",  # Se a previsão for falsa, é negativa
        TRUE ~ "Positiva"                  # Caso contrário, é positiva
      )
    )
  )
# Retorna o dataframe resultante da Matriz de Confusão
tabela_matriz_confusao
#------------------------------------------------------------------------------#
# Cria um novo dataframe chamado df_tabela_matriz_confusao
df_tabela_matriz_confusao <- tabela_matriz_confusao |> 
  # Adiciona uma coluna chamada AcertoErro
  mutate(AcertoErro = as.factor(ifelse(tabela_matriz_confusao$Predicao == tabela_matriz_confusao$Referencia, "Acertos", "Erros"))) %>%
  # Agrupa os dados pela coluna Predicao
  group_by(Predicao) %>%
  # Calcula a proporção de cada categoria (Acertos ou Erros) em relação ao total de previsões para cada rótulo de previsão
  mutate(prop =  scales::percent(Freq / sum(Freq), accuracy = .1, trim = FALSE))

# Retorna o dataframe resultante df_tabela_matriz_confusao
df_tabela_matriz_confusao

#------------------------------------------------------------------------------#
# Cria um gráfico da Matriz de Confusão com as métricas: 
# a) Acurácia é a proporção de todas as previsões corretas (tanto positivas 
# quanto negativas) em relação ao total de previsões.
# b) Sensitividade é a proporção de previsões positivas corretas em relação ao 
# total de casos positivos reais. 
# c) Especificidade é a proporção de previsões negativas corretas em relação
# ao total de casos negativos reais. 
# Essas métricas são usadas para avaliar o desempenho de modelos de classificação,
# como modelos de regressão logística.

# Cria um gráfico de heatmap usando ggplot2 com base nos dados da matriz de confusão
ggplot(data = df_tabela_matriz_confusao, mapping = aes(x = Referencia, y = Predicao,
                                                       fill = AcertoErro, alpha = prop)) +
  # Adiciona retângulos coloridos para representar as células da matriz de confusão
  geom_tile() +
  # Adiciona texto dentro das células com contagens e proporções, destacando acertos e erros
  geom_text(aes(label = paste(prettyNum(Freq, big.mark = "."), "\n", paste0("(", prop, ")"))),
            vjust = 0.5, colour = "black", size = 5 , fontface = "bold", alpha = 1) +
  # Define as cores dos retângulos para representar acertos e erros
  scale_fill_manual(values = c(Acertos = "darkgreen", Erros = "darkred"),
                    guide = guide_legend(override.aes = list(alpha = 0.7))) +
  # Configurações de tema e legenda
  theme_bw() +
  guides(alpha = "none") +
  labs(
    x = "Real",  # Rótulo do eixo x para a categoria real
    y = "Predito",  # Rótulo do eixo y para a categoria prevista
    fill = "",  # Legenda para os retângulos (acertos e erros)
    title = paste("Acurácia:",  # Título do gráfico com informações sobre Acurácia, Sensitividade e Especificidade
                  scales::percent(confusionMatrix[["overall"]]["Accuracy"], accuracy = 1),
                  "|",
                  "Sensitividade:",
                  scales::percent(confusionMatrix[["byClass"]]["Sensitivity"], accuracy = 1),
                  "|",
                  "Especificidade:",
                  scales::percent(confusionMatrix[["byClass"]]["Specificity"], accuracy = 1))) +
  theme(
    legend.position = "bottom",  # Posição da legenda
    legend.title = element_text(size = 10, face = "bold"),  # Estilo do título da legenda
    legend.text = element_text(size = 10, face = "bold"),  # Estilo do texto da legenda
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  # Estilo do título do gráfico
    plot.caption = element_text(size = 10),  # Estilo da legenda do gráfico
    axis.title.y = element_text(size = 12, face = "bold"),  # Estilo do título do eixo y
    axis.title.x = element_text(size = 12, face = "bold"),  # Estilo do título do eixo x
    strip.text.x = element_text(size = 10, colour= "white", face= "bold"),  # Estilo do texto do eixo x
    strip.background = element_rect(colour = "black", fill= "black"),  # Cor de fundo do eixo x
    axis.text.x = element_text(face = "bold", color = "black", size = 10),  # Estilo do texto do eixo x
    axis.text.y = element_text(face = "bold", color = "black", size = 10)) +  # Estilo do texto do eixo y
  # Inverte a ordem das categorias no eixo x para corresponder ao layout da matriz de confusão
  xlim(rev(levels(df_tabela_matriz_confusao$Referencia))) -> plot_matriz_confusao

plot_matriz_confusao

ggsave(plot =  plot_matriz_confusao, "plot_matriz_confusao.svg", height = 6, width = 8, units = "in", dpi = 600)
ggsave(plot =  plot_matriz_confusao, "plot_matriz_confusao.png", height = 6, width = 8, units = "in", dpi = 600)
ggsave(plot =  plot_matriz_confusao, "plot_matriz_confusao.jpeg", height = 6, width = 8, units = "in", dpi = 600)
ggsave(plot =  plot_matriz_confusao, "plot_matriz_confusao.pdf", height = 6, width = 8, units = "in", dpi = 600)

################################################################################
#                          CONSTRUÇÃO DA CURVA ROC                             #
################################################################################
# A curva ROC é uma representação gráfica da relação entre a 
# Sensitividade (taxa de verdadeiros positivos) e a Especificidade (taxa 
# de verdadeiros negativos) para diferentes pontos de corte em um modelo 
# de classificação binária. A AUC (Área sob a curva) é uma métrica que 
# quantifica o desempenho geral do modelo de classificação. O 
# Coeficiente de Gini é outra medida de desempenho frequentemente usada, 
# calculada como (AUC - 0.5) / 0.5.

# Calcula a curva ROC usando a função 'roc' do pacote 'pROC'
ROC <- roc(response = dataset_news_gaza_israel_reg_log$emo_positiva, 
           predictor = step_modelo_reg_log_dummies$fitted.values)

# Cria o gráfico da curva ROC usando 'ggroc' e adiciona um segmento diagonal
ggroc(ROC, color = "#440154FF", size = 1) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
               color="grey40",
               size = 0.2) +
  # Adiciona rótulos aos eixos e ao título do gráfico, incluindo métricas como AUC e coeficiente de Gini
  labs(x = "Especificidade",
       y = "Sensitividade",
       title = paste("Área abaixo da curva:",  # AUC
                     round(ROC$auc, 3),
                     "|",
                     "Coeficiente de Gini:",  # Coeficiente de Gini
                     round((ROC$auc[1] - 0.5) / 0.5, 3))) +
  theme_bw() +  # Define o tema do gráfico como tema base preto e branco
  theme(
    legend.position = "none",  # Remove a legenda
    plot.title = element_text(size = 12, face = "bold"),  # Estilo do título do gráfico
    plot.subtitle = element_text(size = 12, face = "bold"),  # Estilo do subtítulo do gráfico
    plot.caption = element_text(size = 10),  # Estilo da legenda do gráfico
    axis.title.y = element_text(size = 10, face = "bold"),  # Estilo do título do eixo y
    axis.title.x = element_text(size = 10, face = "bold"),  # Estilo do título do eixo x
    strip.text.x = element_text(size = 10, colour= "white", face= "bold"),  # Estilo do texto do eixo x
    strip.background = element_rect(colour = "black", fill= "black"),  # Cor de fundo do eixo x
    axis.text.x = element_text(face = "bold", color = "black", size = 10),  # Estilo do texto do eixo x
    axis.text.y = element_text(face = "bold", color = "black", size = 10)  # Estilo do texto do eixo y
  ) -> plot_roc  # Salva o gráfico como 'plot_roc'

plot_roc  # Exibe o gráfico

ggsave(plot =  plot_roc, "plot_roc.svg", height = 8, width = 10, units = "in", dpi = 600)
ggsave(plot =  plot_roc, "plot_roc.png", height = 8, width = 10, units = "in", dpi = 600)
ggsave(plot =  plot_roc, "plot_roc.jpeg", height = 8, width = 10, units = "in", dpi = 600)
ggsave(plot =  plot_roc, "plot_roc.pdf", height = 8, width = 10, units = "in", dpi = 600)
#------------------------------------ FIM -------------------------------------#