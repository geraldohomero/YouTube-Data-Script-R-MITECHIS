################################################################################
#                Mineração do Texto com Modelagem de Tópicos                   #
#         "Latent Dirichlet Allocation" - Modelo de Mistura Latente (LDA)      #
################################################################################
# Criar um corpus a partir dos dados de notícias sobre Gaza e Israel
corpus_news_gaza_israel <- corpus(dataset_news_gaza_israel) 

# Contar o número de documentos no corpus
ndoc(corpus_news_gaza_israel)

# Tokenizar o corpus, removendo pontuação, números, URLs, separadores e símbolos
tokens_news_gaza_israel <- quanteda::tokens(corpus_news_gaza_israel,
                                            remove_punct = TRUE,
                                            remove_numbers = TRUE,
                                            remove_url = TRUE,
                                            remove_separators = TRUE,
                                            remove_symbols = TRUE)
#-------------------------------------------------------------------------------
# Criar uma lista de stopwords em inglês lendo de um arquivo de texto
stopwords_en <- read_lines("stopwords_en.txt") 

# Remover stopwords da lista padrão de stopwords em inglês e da lista personalizada
# Os padrões de stopwords podem ser encontrados nos pacotes quanteda e marimo
news_gaza_israel_tokens <- quanteda::tokens_remove(tokens_news_gaza_israel,
                                                   pattern = c(stopwords("en"),    # Remover stopwords padrão em inglês
                                                               stopwords_en))  |>  # Remover stopwords personalizadas lidas do arquivo stopwords_en.txt
  tokens_remove(pattern = stopwords("en", source = "marimo"))  # Remover stopwords adicionais do pacote marimo
#-------------------------------------------------------------------------------
# Converte a lista de tokens em uma matriz de documentos-características (Document-Feature Matrix - DFM)
dtm_news_gaza_israel <- dfm(news_gaza_israel_tokens) |> 
  
  # Aplica um filtro à DFM para remover termos pouco frequentes e muito frequentes
  dfm_trim(min_termfreq = 0.8,          # Remove termos que ocorrem em menos de 80% dos documentos
           termfreq_type = "quantile",  # Calcula a frequência do termo usando um quantil
           max_docfreq = 0.1,           # Remove termos que ocorrem em mais de 10% dos documentos
           docfreq_type = "prop")       # Usa uma proporção para calcular a frequência do documento

# Exibe a DFM resultante após aplicar o filtro
dtm_news_gaza_israel

topfeatures(dtm_news_gaza_israel, n = 20) # Palavras que mais aparecem n = numero de palavras para mostrar
#------------------------------------------------------------------------------#
# Vamos exibir algumas informações usando a textplot_wordcloud() função.
library(quanteda.textplots)

set.seed(100)
textplot_wordcloud(dtm_news_gaza_israel, min_count = 500, random_order = FALSE,
                   rotation = .25) 

# plotar em cores com algumas opções adicionais 
# Criar uma nuvem de palavras a partir de uma matriz de documentos de termos (dtm)
# Criar uma nuvem de palavras usando a paleta de cores Viridis
textplot_wordcloud(
  dtm_news_gaza_israel,   # Matriz de documentos de termos
  min_count = 500,        # Contagem mínima de uma palavra para ser incluída na nuvem
  random_order = FALSE,   # Definir a ordem das palavras na nuvem (aleatória ou não)
  rotation = 0.25,        # Rotação das palavras na nuvem (em radianos)
  color = viridisLite::viridis(n = 500, option = "D")   # Paleta de cores Viridis
)
#-------------------------------------------------------------------------------
# Script para calcular o número perfeito de tópicos para o modelo LDA:
# Executar a análise para encontrar o número ideal de tópicos:
# dtm_news_gaza_israel: Matriz DFM (Document-Feature Matrix) contendo os dados textuais a serem analisados.
# topics: Vetor especificando os possíveis números de tópicos a serem considerados na análise.
# metrics: Vetor especificando as métricas a serem usadas para avaliar o número ideal de tópicos.
# method: Método usado para estimar o número ideal de tópicos.
# control: Lista de opções de controle para o método de estimativa de tópicos.
# mc.cores: Número de núcleos a serem usados para computação paralela.
# verbose: Se verdadeiro, mostra informações detalhadas durante o processo de estimativa de tópicos.

n_topicos <- FindTopicsNumber(
  dtm_news_gaza_israel, # Matriz DFM contendo os dados textuais
  topics = seq(from = 5, to = 30, by = 5), # Números possíveis de tópicos
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"), # Métricas para avaliar os tópicos
  method = "Gibbs", # Método de estimativa de tópicos. É uma técnica de Monte Carlo usada para simular distribuições de probabilidade complexas.
  control = list(seed = 77), # Opções de controle para o método
  mc.cores = 2L, # Número de núcleos para computação paralela
  verbose = TRUE # Mostrar informações detalhadas durante o processo
)

saveRDS(n_topicos, "n_topicos.rds")

# Exibir o gráfico com as indicações do número de tópicos
FindTopicsNumber_plot(n_topicos) 
#------------------------------------------------------------------------------#
# Criação de um modelo de Análise de Tópicos Latentes (LDA)
# Argumentos:
# - dtm_news_gaza_israel: Matriz de Documentos x Termos
# - k: Número de tópicos a serem extraídos
topicos_news_gaza_israel <- textmodel_lda(dtm_news_gaza_israel, k = 10)
#------------------------------------------------------------------------------#
# A função seededlda::terms() é usada para extrair os termos mais relevantes para cada tópico identificado pelo modelo de LDA.
# Ela recebe dois argumentos:
# - O primeiro argumento, 'topicos_news_gaza_israel', é o modelo de tópico gerado pelo treinamento do LDA.
# - O segundo argumento, '10', especifica que queremos extrair os 10 termos mais importantes para cada tópico.

seededlda::terms(topicos_news_gaza_israel, 15)
#------------------------------------------------------------------------------#
# A função topicos_news_gaza_israel$theta é usada para acessar as probabilidades posteriores por documento por tópico,
# o que representa a distribuição de tópicos para cada documento.

topicos_news_gaza_israel$theta |> ## theta: Probabilidade posteriores por documento por tópico
  head(n = 10) |> # Exibe as 10 primeiras linhas para visualização
  as.data.frame() |> # Converte o resultado em um dataframe para facilitar a manipulação
  set_names(paste("Topic", 1:10)) |> # Renomeia as colunas com o prefixo "Topic" seguido do número do tópico
  rownames_to_column("document")  # Renomeia a coluna de nomes das linhas para "document"
#------------------------------------------------------------------------------#
# Extração dos principais termos de cada tópico com base nos valores de phi
# Atribui o resultado à variável top_termos
top_termos <- topicos_news_gaza_israel$phi |>  # Acessa a matriz phi do modelo de tópicos
  as.table() |>  # Converte para um formato de tabela
  as.data.frame() |>  # Converte para um data frame
  rename(topicos = Var1,   # Renomeia a primeira coluna para 'topicos'
         termos = Var2,    # Renomeia a segunda coluna para 'termos'
         phi = Freq) |>    # Renomeia a terceira coluna para 'phi'
  group_by(topicos) |>     # Agrupa os dados por tópico
  top_n(10, phi) |>        # Seleciona os 10 principais termos de cada tópico com base nos valores de phi
  ungroup()                # Remove o agrupamento dos dados por tópico

top_termos
#-------------------------------------------------------------------------------
# Vamos criar uma variável no dataset e salvar o "topic" referente a cada documento
dataset_news_gaza_israel$topicos <- seededlda::topics(topicos_news_gaza_israel) 
#-------------------------------------------------------------------------------
summary(dataset_news_gaza_israel$topicos)
#------------------------------------------------------------------------------#
# Resumir as frequências relativa e absoluta dos temas usando o pacote "gtsummary"
dataset_news_gaza_israel |> # Passa o conjunto de dados para a próxima função
  select(topicos) |>        # Seleciona apenas a variável 'topicos' do conjunto de dados
  filter(!is.na(topicos))|> # Filtra as observações onde a variável 'topicos' não é NA
  tbl_summary()             # Cria um resumo tabular das variáveis selecionadas
#------------------------------------------------------------------------------#
top_termos |>   # Usa o dataframe top_termos como entrada
  mutate(topicos = factor(topicos,
                      levels = c("topic10", "topic3", "topic9", "topic2",
                                 "topic8", "topic6", "topic1", "topic4", "topic5", "topic7"))) |> 
  ggplot(aes(x = reorder_within(termos, phi, topicos, sep = "_"),  # Define os eixos x e y, onde os termos são reordenados dentro de cada tópico com base na frequência phi
             y =  phi,  # Define a frequência phi como a altura das barras
             fill = factor(topicos))) +  # Usa a variável de tópicos para colorir as barras
  geom_bar(stat = 'identity', show.legend = FALSE) +  # Cria um gráfico de barras, onde a altura das barras é representada pela frequência phi
  facet_wrap(~ topicos, scales = "free", ncol = 5, # Cria painéis separados para cada tópico, com escala livre e 5 colunas por linha
             labeller = as_labeller(c(topic1 = "Aliados\nn = 832 (8.2%)",
                                      topic2 = "Reféns do Conflito\nn = 1.203 (12%)",
                                      topic3 = "Yemenitas\nn = 669 (6.6%)",
                                      topic4 = "Batalhas\nn = 1130 (11%)",
                                      topic5 = "Conflito Irâ\nn = 1088 (11%)",
                                      topic6 = "Perdas Humanas\nn = 1002 (9.9%)",
                                      topic7 = "Crise Refugiados\nn = 1269 (12%)",
                                      topic8 = "Manifestações\nn = 687 (6.8%)",
                                      topic9 = "Negociações da ONU\nn = 1526 (15%)",
                                      topic10 = "Ocupação Violenta\nn = 749 (7.4%)"))) +  
  scale_x_discrete(labels = function(x) gsub("_.+$", "", x)) +  # Define os rótulos do eixo x, removendo o sufixo "_[número do tópico]"
  coord_flip() + # Inverte os eixos para que os rótulos do eixo x sejam legíveis
  labs(
    x = "Top 10 termos por Tópicos",
    y = "phi (Frequência Termo x Tópico)",
    title = "Tópicos das notícias acerca do conflito Israel vs Hamas",
    subtitle = "Análise de 10 mil notícias publicadas entre 07/10/2023 e 30/04/2024",
    caption = "Fonte: Tuguna Digital Lab - Laboratório de Humanidades Digitais | UEMG Barbacena") +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold"),
    plot.caption = element_text(size = 10),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, colour= "white", face= "bold"),
    strip.background = element_rect(colour = "black", fill= "black"),
    strip.text.x.top = element_text(colour = "white", face = "bold", size = 14),
    axis.text.x = element_text(face = "bold", color = "black", size = 10),
    axis.text.y = element_text(face = "bold", color = "black", size = 10)) -> plot_topicos_news_gaza_israel

plot_topicos_news_gaza_israel
#------------------------------------------------------------------------------#
# Salvar o gráfico como um arquivo SVG (Scalable Vector Graphics)
ggsave(
  plot = plot_topicos_news_gaza_israel,  # Gráfico a ser salvo
  filename = "plot_topicos_news_gaza_israel.svg",  # Nome do arquivo de saída
  height = 6,  # Altura do gráfico em polegadas
  width = 16,  # Largura do gráfico em polegadas
  units = "in",  # Unidade de medida (polegadas)
  dpi = 600  # Resolução da imagem em pontos por polegada
)

# Salvar o gráfico como um arquivo PNG (Portable Network Graphics)
ggsave(
  plot = plot_topicos_news_gaza_israel,
  filename = "plot_topicos_news_gaza_israel.png",
  height = 6,
  width = 16,
  units = "in",
  dpi = 600
)

# Salvar o gráfico como um arquivo JPEG (Joint Photographic Experts Group)
ggsave(
  plot = plot_topicos_news_gaza_israel,
  filename = "plot_topicos_news_gaza_israel.jpeg",
  height = 6,
  width = 16,
  units = "in",
  dpi = 600
)

# Salvar o gráfico como um arquivo PDF (Portable Document Format)
ggsave(
  plot = plot_topicos_news_gaza_israel,
  filename = "plot_topicos_news_gaza_israel.pdf",
  height = 6,
  width = 16,
  units = "in",
  dpi = 600
)
################################################################################
#               Visualização do Resultados em Nuvem de Palavras                #
################################################################################
topicos_news_gaza_israel$phi |>  # Acessa a matriz phi do modelo de tópicos
  as.table() |>  # Converte para um formato de tabela
  as.data.frame() |>  # Converte para um data frame
  rename(topicos = Var1,   # Renomeia a primeira coluna para 'topicos'
         termos = Var2,    # Renomeia a segunda coluna para 'termos'
         phi = Freq) |>    # Renomeia a terceira coluna para 'phi'
  group_by(topicos) |>     # Agrupa os dados por tópico
  top_n(50, phi) |>        # Seleciona os 10 principais termos de cada tópico com base nos valores de phi
  ungroup() |>          
  ggplot(aes(label = termos, fontface = "bold", size = phi*1000, color = topicos)) +
  geom_text_wordcloud(area_corr = TRUE) +
  scale_size_area(max_size = 20) +
  facet_wrap(~topicos, scales = "free", ncol = 5, nrow = 2,
             labeller = as_labeller(c(topic1 = "Aliados\nn = 832 (8.2%)",
                                      topic2 = "Reféns do Conflito\nn = 1.203 (12%)",
                                      topic3 = "Yemenitas\nn = 669 (6.6%)",
                                      topic4 = "Batalhas\nn = 1130 (11%)",
                                      topic5 = "Conflito Irâ\nn = 1088 (11%)",
                                      topic6 = "Perdas Humanas\nn = 1002 (9.9%)",
                                      topic7 = "Crise Refugiados\nn = 1269 (12%)",
                                      topic8 = "Manifestações\nn = 687 (6.8%)",
                                      topic9 = "Negociações da ONU\nn = 1526 (15%)",
                                      topic10 = "Ocupação Violenta\nn = 749 (7.4%)"))) +  
  scale_colour_viridis_d() +
  theme_bw() +
  theme(strip.background = element_rect(fill = "black"),
        strip.text.x = element_text(face = "bold", colour = "white", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        plot.caption = element_text(size = 10, face = "bold.italic")) +
  labs(title = "Nuvem de palavras dos tópicos das notícias acerca do conflito Israel vs Hamas") -> plot_wordcloud_topicos_news_gaza_israel

plot_wordcloud_topicos_news_gaza_israel

# Salvar o gráfico como um arquivo PDF (Portable Document Format)
ggsave(
  plot = plot_wordcloud_topicos_news_gaza_israel,
  filename = "plot_wordcloud_topicos_news_gaza_israel.pdf",
  height = 8,
  width = 23,
  units = "in",
  dpi = 600
)
################################################################################
#               Visualização do Resultados em Série Temporal                   #
################################################################################
# Selecionar as colunas 'topicos' e 'data' do conjunto de dados
dataset_news_gaza_israel |> 
  select(topicos, data) |> 
  # Adicionar uma coluna 'topicos' para ser usada como legenda e definir a ordem das categorias dos tópicos
  mutate(topicos = factor(topicos, levels = c("topic1", "topic2", "topic3", "topic4", "topic5",
                                              "topic6", "topic7", "topic8", "topic9", "topic10"))) |> 
  # Agrupar os dados por 'data' e 'topicos'
  group_by(data, topicos) |> 
  # Contar o número de ocorrências de cada combinação de 'data' e 'topicos'
  count() |> 
  filter(topicos == "topic1") |> 
  # Criar o gráfico de linha e ponto
  ggplot(aes(data, n)) +
  geom_line(size = .5) +  # Adicionar linhas
  geom_point(size = .3, col = "red", show.legend = F) +  # Adicionar pontos
  theme_bw() +  # Aplicar um tema de fundo branco
  # Personalizar as configurações do tema
  theme(
    legend.position = "none",
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold"),
    plot.caption = element_text(size = 10),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, colour= "white", face= "bold"),
    strip.background = element_rect(colour = "black", fill= "black"),
    strip.text.x.top = element_text(colour = "white", face = "bold", size = 14),
    axis.text.x = element_text(face = "bold", color = "black", size = 10),
    axis.text.y = element_text(face = "bold", color = "black", size = 10)
  ) +
  scale_x_date(date_breaks = "years",       # Definir os intervalos principais de rótulos como anos
               date_labels = "%Y",          # Formato dos rótulos do eixo x (apenas ano)
               date_minor_breaks = "1 year") +  # Definir os intervalos menores de rótulos como anos
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +  # Definir intervalo de valores no eixo y
  labs(x = "Ano", y = "Frequência", 
       title = "Proporções dos tópicos ao longo do tempo em intervalo anual",
       caption = "Fonte: Tuguna Digital Lab - Laboratório Experimental em Humanidades Digitais\nUniversidade do Estado de Minas Gerais - UEMG\nUnidade Barbacena") +  # Adicionar rótulos
  facet_wrap(~topicos, scales = "free", ncol = 5, nrow = 2,
             labeller = as_labeller(c(topic1 = "Aliados\nn = 928 (9.1%)",
                                      topic2 = "Presioneiros\nn = 1.225 (12%)",
                                      topic3 = "Presioneiros\nn = 1.225 (12%)",
                                      topic4 = "Presioneiros\nn = 1.225 (12%)",
                                      topic5 = "Presioneiros\nn = 1.225 (12%)",
                                      topic6 = "Presioneiros\nn = 1.225 (12%)",
                                      topic7 = "Presioneiros\nn = 1.225 (12%)",
                                      topic8 = "Presioneiros\nn = 1.225 (12%)",
                                      topic9 = "Presioneiros\nn = 1.225 (12%)",
                                      topic10 = "Presioneiros\nn = 1.225 (12%)"))) -> serie_temporal_temas_linhas

serie_temporal_temas_linhas

ggsave(plot = serie_temporal_temas_linhas, "serie_temporal_temas_linhas1.jpeg", height = 6, width = 10, units = "in", dpi = 600)
#------------------------------------------------------------------------------#
# De toda forma, parece que gráficos de linhas não ficam bem para esses dados. 
# Então vamos fazer um gráfico de colunas.

dataset_news_gaza_israel |> 
  select(topicos, ano) |> 
  # Adicionar uma coluna 'topicos' para ser usada como legenda e definir a ordem das categorias dos tópicos
  mutate(topicos = factor(topicos, levels = c("topic1", "topic2", "topic3", "topic4", "topic5",
                                              "topic6", "topic7", "topic8", "topic9", "topic10"))) |> 
  # Agrupar os dados por 'ano' e 'topicos'
  group_by(ano, topicos) |> 
  # Contar o número de ocorrências de cada combinação de 'ano' e 'topicos'
  count() |> 
  # Criar o gráfico de linha e ponto
  ggplot(aes(x = ano, y = n, fill = topicos)) +
  geom_col(size = .5) +  # Adicionar colunas
  theme_bw() +  # Aplicar um tema de fundo branco
  # Personalizar as configurações do tema
  theme(
    legend.position = "none",
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold"),
    plot.caption = element_text(size = 10),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, colour= "white", face= "bold"),
    strip.background = element_rect(colour = "black", fill= "black"),
    strip.text.x.top = element_text(colour = "white", face = "bold", size = 14),
    axis.text.x = element_text(face = "bold", color = "black", size = 10),
    axis.text.y = element_text(face = "bold", color = "black", size = 10)
  ) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +  # Definir intervalo de valores no eixo y
  labs(x = "Ano", y = "Frequência", 
       title = "Proporções dos tópicos ao longo do tempo em intervalo anual",
       caption = "Fonte: Tuguna Digital Lab - Laboratório Experimental em Humanidades Digitais\nUniversidade do Estado de Minas Gerais - UEMG\nUnidade Barbacena") +  # Adicionar rótulos
  facet_wrap(~topicos, scales = "free", ncol = 5, nrow = 2,
             labeller = as_labeller(c(topic1 = "Aliados\nn = 832 (8.2%)",
                                      topic2 = "Reféns do Conflito\nn = 1.203 (12%)",
                                      topic3 = "Yemenitas\nn = 669 (6.6%)",
                                      topic4 = "Batalhas\nn = 1130 (11%)",
                                      topic5 = "Conflito Irâ\nn = 1088 (11%)",
                                      topic6 = "Perdas Humanas\nn = 1002 (9.9%)",
                                      topic7 = "Crise Refugiados\nn = 1269 (12%)",
                                      topic8 = "Manifestações\nn = 687 (6.8%)",
                                      topic9 = "Negociações da ONU\nn = 1526 (15%)",
                                      topic10 = "Ocupação Violenta\nn = 749 (7.4%)")))  -> serie_temporal_temas_colunas
serie_temporal_temas_colunas

################################################################################
#     Visualização do Resultados em comparação com as métricas do YouTube      #
################################################################################
engajamento <- dataset_news_gaza_israel |>  
  select(topicos, viewCount, likeCount, commentCount) %>% 
  mutate(topicos = recode(topicos,
                          topic1 = "Aliados",
                          topic2 = "Reféns do Conflito",
                          topic3 = "Yemenitas",
                          topic4 = "Batalhas",
                          topic5 = "Conflito Irâ",
                          topic6 = "Perdas Humanas",
                          topic7 = "Crise Refugiados",
                          topic8 = "Manifestações",
                          topic9 = "Negociações da ONU",
                          topic10 = "Ocupação Violenta")) |> 
  group_by(topicos) |>  
  summarise(view = sum(viewCount, na.rm = T),
            likes = sum(likeCount, na.rm = T),
            comments = sum(commentCount, na.rm = T))

engajamento
#-------------------------------------------------------------------------------
# Vamos extrair as médias das variaveis e correlação entre elas colocar no gráfico
mean(engajamento$likes)
mean(engajamento$comments)
min(engajamento$likes)
min(engajamento$comments)
max(engajamento$likes)
max(engajamento$comments)
cor.test(engajamento$likes, engajamento$comments)
#-------------------------------------------------------------------------------
grafico_engajamento <- ggplot(engajamento, aes(x = likes, y = comments, fill = as.factor(topicos))) +  # Iniciar um objeto ggplot com dados de engajamento
  geom_label_repel(aes(label = topicos),  # Adicionar rótulos repelidos com base nos tópicos
                   color = "black",        # Definir a cor do texto dos rótulos como branco
                   fontface = "bold",      # Definir a espessura da fonte como negrito
                   nudge_x = 0.15,         # Ajustar a posição horizontal dos rótulos
                   box.padding = 0.5,      # Definir o preenchimento interno dos rótulos
                   nudge_y = 1,            # Ajustar a posição vertical dos rótulos
                   segment.curvature = -0.1,  # Curvatura da linha de conexão dos rótulos
                   segment.ncp = 3,        # Parâmetro de suavização da curvatura
                   segment.angle = 20) +   # Ângulo de curvatura da linha de conexão
  geom_point(aes(size = view), shape = 21, stroke = 2) +  # Adicionar pontos ao gráfico com tamanho variável
  geom_hline(yintercept = 194462.8, linetype = "dashed", color = "grey") +  # Adicionar linha horizontal pontilhada
  geom_vline(xintercept = 534312.2, linetype = "dashed", color = "grey") +  # Adicionar linha vertical pontilhada
  geom_text(aes(x = 750000, y = 97000, label = "p = 0.653*"), size = 5, col = "black") +  # Adicionar texto de anotação
  scale_x_continuous(expand = c(0.025,0.025), breaks = seq(427736, 800000, 30000), labels = comma_format(big.mark = ".", decimal.mark = ",")) +  # Ajustar escala e rótulos do eixo x
  scale_y_continuous(expand = c(0.025,0.025), breaks = seq(63267, 352659, 50000), labels = comma_format(big.mark = ".", decimal.mark = ",")) +  # Ajustar escala e rótulos do eixo y
  scale_fill_manual(values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a")) +  # Definir cores de preenchimento manualmente
  labs(
    x = "Soma de likes por temas",  # Definir rótulo do eixo x
    y = "Soma de comentários por temas",  # Definir rótulo do eixo y
    title = "Engajamento da audiência por tópicos temáticos",  # Definir título do gráfico
    caption = "Fonte: Tuguna Digital Lab - Laboratório Experimental em Humanidades Digitais\nUniversidade do Estado de Minas Gerais - UEMG\nUnidade Barbacena") +  # Definir legenda
  theme_classic() +  # Aplicar o tema clássico
  theme(  # Personalizar o tema
    legend.position = "none",  # Remover a legenda
    plot.title = element_text(size = 16, face = "bold"),  # Definir estilo do título do gráfico
    plot.caption = element_text(size = 10, face = "bold"),  # Definir estilo da legenda
    axis.title.y = element_text(size = 10, face = "bold"),  # Definir estilo do título do eixo y
    axis.title.x = element_text(size = 10, face = "bold"),  # Definir estilo do título do eixo x
    axis.text.x = element_text(face = "bold", color = "black", size = 10),  # Definir estilo do texto do eixo x
    axis.text.y = element_text(face = "bold", color = "black", size = 10))  # Definir estilo do texto do eixo y

grafico_engajamento

ggsave(plot = grafico_engajamento, "grafico03_engajamento.jpeg", height = 8, width = 12, units = "in", dpi = 600)

#------------------------------------- FIM ------------------------------------#