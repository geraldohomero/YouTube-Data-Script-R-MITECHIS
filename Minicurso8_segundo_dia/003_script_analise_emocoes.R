################################################################################
#                            ANÁLISE DE SENTIMENTOS                            #
#                      Emoções Predominantes nas notícias                      #
################################################################################
library(sentimentr)

emo_data_news_gaza_israel <- dataset_news_gaza_israel  |>  # Inicia a sequência de operações encadeadas, atribuindo o resultado a uma variável chamada emo_data_news
  get_sentences() |>  # Extrai sentenças do conjunto de dados
  emotion_by() |>  # Calcula as emoções presentes em cada sentença
  filter(emotion_type %in% c("anger", "disgust", "surprise", "trust", "fear", "anticipation", "joy", "sadness")) |>  # Filtra as emoções desejadas
  pivot_wider(names_from = emotion_type,  # Transforma os dados de emoção em um formato mais amplo
              values_from = c(word_count, sd, ave_emotion, emotion_count)) |>  
  rename(word_count = word_count_anger,  # Renomeia as colunas para uma melhor legibilidade
         anger = ave_emotion_anger,
         anticipation = ave_emotion_anticipation,
         disgust = ave_emotion_disgust,
         fear = ave_emotion_fear,
         joy = ave_emotion_joy,
         sadness = ave_emotion_sadness,
         surprise = ave_emotion_surprise,
         trust = ave_emotion_trust,
         word_anger = emotion_count_anger,
         word_anticipation = emotion_count_anticipation,
         word_disgust = emotion_count_disgust,
         word_fear = emotion_count_fear,
         word_joy = emotion_count_joy,
         word_sadness = emotion_count_sadness,
         word_surprise = emotion_count_surprise,
         word_trust = emotion_count_trust)  |>  
  select(-c(word_count_anticipation,  # Remove colunas desnecessárias
            word_count_disgust,
            word_count_fear,
            word_count_sadness,
            word_count_surprise,
            word_count_trust,
            word_count_joy, 
            sd_anger,
            sd_anticipation,
            sd_disgust,
            sd_fear,
            sd_joy,
            sd_sadness,
            sd_surprise,
            sd_trust)) |>   
  mutate(emo_news = as.factor(factor(ifelse(anger > anticipation 
                                            & anger > disgust 
                                            & anger > fear 
                                            & anger > joy 
                                            & anger > sadness 
                                            & anger > surprise 
                                            & anger > trust, "Raiva",
                                            ifelse(anticipation > anger 
                                                   & anticipation > disgust 
                                                   & anticipation > fear 
                                                   & anticipation > joy 
                                                   & anticipation > sadness 
                                                   & anticipation > surprise 
                                                   & anticipation > trust, "Antecipação",
                                                   ifelse(disgust > anger 
                                                          & disgust > anticipation 
                                                          & disgust > fear 
                                                          & disgust > joy 
                                                          & disgust > sadness 
                                                          & disgust > surprise 
                                                          & disgust > trust, "Nojo",
                                                          ifelse(fear > anger 
                                                                 & fear > anticipation 
                                                                 & fear > disgust 
                                                                 & fear > joy 
                                                                 & fear > sadness 
                                                                 & fear > surprise 
                                                                 & fear > trust, "Medo",
                                                                 ifelse(joy > anger 
                                                                        & joy > anticipation 
                                                                        & joy > disgust 
                                                                        & joy > fear 
                                                                        & joy > sadness 
                                                                        & joy > surprise 
                                                                        & joy > trust, "Alegria",
                                                                        ifelse(sadness > anger 
                                                                               & sadness > anticipation 
                                                                               & sadness > disgust 
                                                                               & sadness > fear 
                                                                               & sadness > joy 
                                                                               & sadness > surprise 
                                                                               & sadness > trust, "Tristeza",
                                                                               ifelse(surprise > anger 
                                                                                      & surprise > anticipation 
                                                                                      & surprise > disgust 
                                                                                      & surprise > fear 
                                                                                      & surprise > joy 
                                                                                      & surprise > sadness 
                                                                                      & surprise > trust, "Surpresa",
                                                                                      ifelse(trust > anger 
                                                                                             & trust > anticipation 
                                                                                             & trust > disgust 
                                                                                             & trust > fear 
                                                                                             & trust > joy 
                                                                                             & trust > sadness 
                                                                                             & trust > surprise, "Confiança", "#"))))))))))) |> 
  bind_cols(dataset_news_gaza_israel)

summary(emo_data_news_gaza_israel$emo_news)
#------------------------------------------------------------------------------#
emo_data_news_gaza_israel |>  # Selecionar o conjunto de dados sobre as notícias relacionadas ao conflito em Gaza e Israel
  dplyr::select(word_anger, word_anticipation, word_disgust, word_fear, word_joy, word_sadness, word_surprise, word_trust) |>  # Selecionar as colunas de contagem de palavras para cada emoção
  summarise(Raiva = sum(word_anger),  # Resumir a contagem total de palavras associadas à emoção de raiva
            Antecipação = sum(word_anticipation),  # Resumir a contagem total de palavras associadas à emoção de antecipação
            Nojo = sum(word_disgust),  # Resumir a contagem total de palavras associadas à emoção de nojo
            Medo = sum(word_fear),  # Resumir a contagem total de palavras associadas à emoção de medo
            Alegria = sum(word_joy),  # Resumir a contagem total de palavras associadas à emoção de alegria
            Tristeza = sum(word_sadness),  # Resumir a contagem total de palavras associadas à emoção de tristeza
            Surpresa = sum(word_surprise ),  # Resumir a contagem total de palavras associadas à emoção de surpresa
            Confiança = sum(word_trust)) |>  # Resumir a contagem total de palavras associadas à emoção de confiança
  pivot_longer(cols = Raiva:Confiança,  # Transformar os resultados resumidos em um formato longo para facilitar a visualização
               names_to = "emo",  # Nomear a coluna que conterá as emoções
               values_to = "freq") |>  # Nomear a coluna que conterá a frequência das emoções
  dplyr::mutate(emo = factor(emo,  # Transformar a coluna 'emo' em um fator para ordenação correta das emoções
                             levels = c("Raiva", "Medo", "Nojo", "Tristeza", "Surpresa", "Antecipação", "Confiança", "Alegria")))|>  # Definir a ordem das emoções
  ggplot() +  # Criar um objeto ggplot
  aes(x = emo, y = freq,  fill = emo) +  # Definir as variáveis estéticas x, y e preenchimento (fill)
  geom_col(position=position_dodge()) +  # Adicionar barras ao gráfico com a função de posicionamento dodge
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +  # Definir os rótulos do eixo y em milhares
  scale_fill_brewer(palette = "RdBu") +  # Definir a paleta de cores para o preenchimento
  labs(  # Adicionar rótulos e títulos ao gráfico
    x = "Emoções",  # Rótulo do eixo x
    y = "Frequência em milhares",  # Rótulo do eixo y
    title = "Emoções nas notícias sobre o conflito Israel vs Hamas",  # Título do gráfico
    subtitle = "Emoções ordenadas das mais negativas para as mais positivas",  # Subtítulo do gráfico
    caption = "Fonte: Resultados originais da pesquisa, 2024.") +  # Fonte dos dados
  theme_bw() +  # Aplicar o tema de fundo branco ao gráfico
  theme(  # Personalizar as configurações do tema
    legend.position = "none",  # Remover a legenda
    plot.title = element_text(size = 18, face = "bold"),  # Personalizar o estilo do título
    plot.subtitle = element_text(size = 12, face = "bold"),  # Personalizar o estilo do subtítulo
    plot.caption = element_text(size = 10),  # Personalizar o estilo da legenda
    axis.title.y = element_text(size = 10, face = "bold"),  # Personalizar o estilo do rótulo do eixo y
    axis.title.x = element_text(size = 10, face = "bold"),  # Personalizar o estilo do rótulo do eixo x
    strip.text.x = element_text(size = 10, colour= "white", face= "bold"),  # Personalizar o estilo do texto dos painéis x
    strip.background = element_rect(colour = "black", fill= "black"),  # Personalizar o fundo dos painéis
    axis.text.x = element_text(face = "bold", color = "black", size = 10),  # Personalizar o estilo do texto do eixo x
    axis.text.y = element_text(face = "bold", color = "black", size = 10)) -> plot_emo_news_gaza_israel  # Personalizar o estilo do texto do eixo y e atribuir o gráfico a uma variável

plot_emo_news_gaza_israel

ggsave(plot =  plot_emo_news_gaza_israel, "plot_emo_news_gaza_israel.svg", height = 6, width = 10, units = "in", dpi = 600)
ggsave(plot =  plot_emo_news_gaza_israel, "plot_emo_news_gaza_israel.png", height = 6, width = 10, units = "in", dpi = 600)
ggsave(plot =  plot_emo_news_gaza_israel, "plot_emo_news_gaza_israel.jpeg", height = 6, width = 10, units = "in", dpi = 600)
ggsave(plot =  plot_emo_news_gaza_israel, "plot_emo_news_gaza_israel.pdf", height = 6, width = 10, units = "in", dpi = 600)
#------------------------------------ FIM -------------------------------------#