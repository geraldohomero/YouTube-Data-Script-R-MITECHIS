################################################################################
#       Construção do dataset das transcrições dos vídeos/notícias             #
################################################################################
# Bibliotecas necessárias para o script
library(readtext) # Para ler arquivos de texto
library(dplyr) # Para manipulação de dados
library(stringr) # Para manipulação de strings
library(cld2) # Para detecção de idiomas
library(textstem) # Para stemming de palavras em textos
library(tidyverse) # Conjunto de pacotes para manipulação e visualização de dados

# Leitura dos arquivos de texto do diretório "news_gaza_israel"
data_news_gaza_israel <- readtext("news_gaza_israel/*.txt") |> 
  
  # Renomeação da coluna de identificação para "videoId"
  rename(videoId = doc_id) |>                                              
  
  # Extração dos primeiros 11 caracteres da coluna "videoId"
  mutate(videoId = substr(videoId, 1, 11),
         
         # Substituição de caracteres especiais por caracteres padrão na coluna "text"
         text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"), # corrige erros de codificação     
         text = str_replace_all(text, "<a(.*?)>", " "),             # remove links       
         text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),      # remove borrões/sujeiras de html        
         text = str_replace_all(text, "&#[:digit:]+;", " "),        # remove borrões/sujeiras de html contendo números       
         text = str_replace_all(text, "<[^>]*>", " "),              # mmmmm, remove mais (repetições) borrões/sujeiras de html       
         text = str_replace_all(text, "\\d", " "),                  # remove números - pacote stringr do tidyverse       
         text = str_replace_all(text, c("♪" = " ", "foreign" = " ", # remove palavras e caracteres listados       
                                        "uh" = " ")),    
         text = str_replace_all(text, "\\[.*?\\]", " "),            # remove caracteres e palavras entre [], como "Music" e "Applause"     
         text = str_replace_all(text, "[^[:alnum:]]", " "),         # remove todo tipo de caracteres não alfanuméricos     
         text = str_replace_all(text, "[[:punct:]]", " "),          # remove pontuação, para garantir :)     
         
         # Remoção de espaços extras na coluna "text"
         text = str_squish(text),                                        
         
         # Conversão de todas as letras para minúsculas na coluna "text"
         text = stri_trans_tolower(text),                                
         
         # Aplicação de stemming nas palavras da coluna "text", Lematização do texto: reduz o texto a radicais de verbo e substantivos
         text = lemmatize_strings(text),
         text = str_replace_all(text, c("um" = "UN", "gaz" = "Gaza"))) |> # Substitui palavras alteradas pela Lematização para o termo correto
  
  # 'str_count()' conta o número de ocorrências de um padrão em uma string.
  # O padrão "\\S+" corresponde a qualquer sequência contínua de caracteres que não seja espaço em branco.
  mutate(words = str_count(text, "\\S+")) |> 
  
  # Detecção do idioma do texto e conversão para fator
  mutate(idioma = as.factor(detect_language(text))) |>             
  
  # Filtragem para manter apenas textos em inglês
  filter(idioma == "en") |>  
  
  # Remoção da coluna de idioma
  select(-idioma)
#-------------------------------------------------------------------------------
# Medidas resumo do número de palavras das notícias
summary(data_news_gaza_israel$words)
boxplot(data_news_gaza_israel$words)
#-------------------------------------------------------------------------------
saveRDS(data_news_gaza_israel, "data_news_gaza_israel.rds")
data_news_gaza_israel <- readRDS("data_news_gaza_israel.rds")
#-------------------------------------------------------------------------------
# Importar a tabela de Excel com os metadados dos vídeos e selecionar as variáveis que serão usadas nas análises.
library(readxl)
library(dplyr)
library(countrycode)

# Ler o arquivo Excel e carregar os dados da segunda planilha
metadados_canais_gaza_israel <- read_excel("metadados_gaza-israel.xlsx", sheet = 1) |> 
  
  # Adicionar uma coluna com os nomes dos países em Português
  mutate(nomes_paises = as.factor(countrycode(country, "iso2c", "country.name"))) |>
  
  # Selecionar as colunas relevantes
  select(id, country, nomes_paises)
#------------------------------------------------------------------------------#
library(readxl)
library(dplyr)

# Ler o arquivo Excel e carregar os dados da primeira planilha
dataset_news_gaza_israel <- read_excel("metadados_gaza-israel.xlsx", sheet = 2) |> 
  
  # Converter a coluna 'publishedAt' para formato de data
  mutate(data = as.Date(publishedAt)) |>  
  
  # Criar uma nova coluna 'ano' como fator, representando o ano de publicação
  mutate(ano = as.factor(year(publishedAt))) |> 
  
  # Converter a coluna 'channelTitle' para fator
  mutate(canal = as.factor(channelTitle)) |>  
  
  
  # Juntar com os metadados dos vídeos usando o ID do canal
  inner_join(metadados_canais_gaza_israel, by = c("channelId" = "id")) |> 
  
  # Selecionar as colunas desejadas
  dplyr::select(channelId, country, nomes_paises, videoId, data, ano, canal,
                videoTitle, videoCategoryLabel, durationSec, viewCount, likeCount, commentCount) |> 
  inner_join(data_news_gaza_israel, by = "videoId") |> 
  select(-words)
################################################################################
#                  Visualização dos Metadados das Notícias                     #
#                            Análise Exploratória                              #
################################################################################
# Calcular a porcentagem de cada ponto
dataset_news_gaza_israel %>%
  mutate(data = floor_date(data, unit = "year") %>% as.Date()) %>%
  group_by(data) %>%
  count() %>%
  mutate(percentage = n / 10155 * 100) -> dados_porcentagem
#------------------------------------------------------------------------------#
ggplot(dados_porcentagem, aes(data, n)) +
  geom_line(size = 1, color = "black") +  # Defina a cor da linha
  geom_point(shape = 21, color = "blue", fill = "white", size = 2, stroke = 3) +  # Adicionar pontos personalizados
  geom_text(aes(label = paste0(round(percentage, 2), "%")), vjust = -1, size = 3, color = "black") +  # Adicionar rótulos com porcentagem nos pontos
  theme_classic() +                         # Aplicar um tema clássico
  scale_x_date(date_breaks = "years",       # Definir os intervalos principais de rótulos como anos
               date_labels = "%Y",          # Formato dos rótulos do eixo x (apenas ano)
               date_minor_breaks = "1 year") +  # Definir os intervalos menores de rótulos como anos
  labs(x = "Ano", y = "Número de Observações", title = "Tendência ao Longo do Tempo")  # Adicionar rótulos
#------------------------------------------------------------------------------#
paises_news_gaza_israel <- dataset_news_gaza_israel %>% 
  group_by(nomes_paises) %>% 
  summarise(n = n()) %>% 
  dplyr::mutate(perc = scales::percent(n / sum(n), accuracy = .1, trim = FALSE))
#------------------------------------------------------------------------------#
# Criar o gráfico de treemap
treemap_paises_news_gaza_israel <- ggplot(paises_news_gaza_israel, aes(
  area = n,  # Define o tamanho de cada retângulo do treemap com base na contagem de notícias
  fill = nomes_paises,  # Define as cores dos retângulos com base nos nomes dos países
  subgroup = nomes_paises,  # Define os subgrupos como os nomes dos países
  label = paste(  # Define os rótulos para cada retângulo
    nomes_paises,  # Nome do país
    "\n",  # Quebra de linha
    prettyNum(n, big.mark = "."),  # Formata a contagem de notícias com separador de milhar
    "\n",  # Quebra de linha
    paste0("(", perc, ")")  # Adiciona a porcentagem entre parênteses
  )
)) +
  geom_treemap() +  # Adiciona os retângulos do treemap
  geom_treemap_subgroup_border(colour = "black") +  # Adiciona as bordas dos subgrupos
  geom_treemap_text(  # Adiciona os textos no centro de cada retângulo
    colour = "white",  # Cor do texto
    place = "centre",  # Posição do texto (centro)
    size = 15,  # Tamanho do texto
    fontface = "bold"  # Estilo do texto (negrito)
  ) +
  theme(  # Define o tema do gráfico
    legend.position = "none",  # Remove a legenda
    plot.title = element_text(size = 18, face = "bold"),  # Título do gráfico
    plot.subtitle = element_text(size = 12, face = "bold")  # Subtítulo do gráfico
  ) +
  scale_fill_viridis_d() +  # Define a paleta de cores
  labs(  # Adiciona rótulos ao gráfico
    title = "Composição da amostra das notícias por país dos jornais",  # Título do gráfico
    subtitle = "Amostra agregada dos anos de 2009 a 2024",  # Subtítulo do gráfico
    caption = "Fonte: Tuguna Digital Lab - Laboratório de Humanidades Digitais | UEMG Barbacena"  # Fonte dos dados
  ) -> treemap_paises_news_gaza_israel # Armazena o gráfico em uma variável chamada treemap_paises_news_gaza_israel

treemap_paises_news_gaza_israel

# Salvar o gráfico em formato SVG
ggsave(
  plot = treemap_paises_news_gaza_israel,  # Gráfico a ser salvo
  filename = "treemap_paises_news_gaza_israel.svg",  # Nome do arquivo de saída
  height = 8,  # Altura do gráfico em polegadas
  width = 16,  # Largura do gráfico em polegadas
  units = "in",  # Unidade de medida (polegadas)
  dpi = 600  # Resolução da imagem em pontos por polegada
)

# Salvar o gráfico em formato PNG
ggsave(plot = treemap_paises_news_gaza_israel, "treemap_paises_news_gaza_israel.png", height = 8, width = 16, units = "in", dpi = 600)

# Salvar o gráfico em formato JPEG
ggsave(plot = treemap_paises_news_gaza_israel, "treemap_paises_news_gaza_israel.jpeg", height = 8, width = 16, units = "in", dpi = 600)

# Salvar o gráfico em formato PDF
ggsave(plot = treemap_paises_news_gaza_israel, "treemap_paises_news_gaza_israel.pdf", height = 8, width = 16, units = "in", dpi = 600)
#------------------------------------ FIM -------------------------------------#