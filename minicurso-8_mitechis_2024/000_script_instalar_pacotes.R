################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS            #
################################################################################
#Pacotes utilizados
pacotes <- c("car",                 # Oferece funções para diagnóstico e modelagem de regressão.
             "caret",               # Uma ferramenta abrangente para treinar e avaliar modelos de aprendizado de máquina.
             "cld2",                # Fornece funções para detecção de idiomas em texto.  
             "countrycode",         # Fornece os nomes os países a partir dos códigos ISO (P.ex. BR = Brasil).
             "cowplot",             # Facilita a criação de gráficos compostos e complexos, permitindo a combinação de múltiplos gráficos em um layout.
             "factoextra",          # Oferece funções auxiliares para análise de fatores.
             "FactoMineR",          # Usado para análise de componentes principais e outras técnicas de análise fatorial.
             "fastDummies",         # Utilizado para criar variáveis fictícias rapidamente a partir de variáveis categóricas em um conjunto de dados. 
             "ggrepel",             # Oferece ferramentas para evitar a sobreposição de rótulos em gráficos ggplot2.
             "ggthemes",            # Fornece temas adicionais para gráficos ggplot2.
             "ggwordcloud",         # Usado para criar nuvens de palavras com ggplot2.
             "gtsummary",           # Facilita a criação de tabelas resumidas e estatísticas descritivas.
             "jtools",              # Oferece ferramentas para a análise de modelos lineares, incluindo diagnósticos e visualizações.
             "kableExtra",          # Estende a funcionalidade de tabelas em Markdown, permitindo personalização avançada.         
             "knitr",               # Usado para criar relatórios dinâmicos em R Markdown.
             "ldatuning",           # Fornece ferramentas para ajuste de hiperparâmetros em modelos de tópicos LDA.   
             "lmtest",              # Contém funções para testar hipóteses específicas em modelos lineares.
             "lubridate",           # Projetado para facilitar a manipulação de datas e horários em R.
             "magick",              # Fornece funcionalidades para trabalhar com imagens em R.
             "NLP",                 # Oferece ferramentas para processamento de linguagem natural em R.
             "nnet",                # Implementa redes neurais artificiais para modelagem preditiva.
             "pROC",                # Usado para análise de curvas ROC e cálculo de áreas sob a curva.
             "plotly",              # Permite a criação de gráficos interativos usando a biblioteca Plotly.js.
             "qdapRegex",           # Oferece extensões para expressões regulares em R.
             "quanteda",            # Pacote para análise de texto e mineração de dados em R.
             "quanteda.textmodels", # Fornece modelos de texto específicos para o pacote quanteda.
             "quanteda.textplots",  #  Oferece funções para visualização de dados textuais em R.
             "quanteda.textstats",  # Contém funções para calcular estatísticas textuais em R.
             "readtext",            # Usado para importar e processar dados de texto em diferentes formatos.
             "readxl",              # Oferece funções para ler arquivos Excel em R.
             "reshape2",            # Útil para remodelar e transformar conjuntos de dados em R.
             "rgl",                 # Permite a criação de gráficos tridimensionais em R.
             "rnaturalearth",       # Fornece conjuntos de dados geográficos naturais para uso em R.
             "ROCR",                # Usado para avaliação de modelos de classificação usando curvas ROC.
             "scales",              # Oferece ferramentas para ajustar e transformar escalas em gráficos ggplot2.
             "seededlda",           # Implementa modelos de tópicos LDA com inicialização determinística.
             "sentimentr",          # Oferece análise de sentimentos em texto em R.
             "sf",                  # Oferece classes e métodos para manipulação e análise de dados espaciais.
             "sjPlot",              # Fornece funções para criar gráficos e tabelas para modelos estatísticos.
             "SnowballC",           # Implementa algoritmos de stemming para várias línguas.
             "stopwords",           # Fornece listas de palavras comuns para serem excluídas em análise de texto.
             "stringi",             # Oferece funções para manipulação de strings em R.
             "textmineR",           # Pacote para mineração de texto em R.
             "textstem",            # Usado para stemming de palavras em textos.
             "tidytext",            # Facilita a análise de texto com o tidyverse em R.
             "tidyverse",           # Um conjunto de pacotes R que compartilham uma filosofia de design coesa para manipulação e visualização de dados.
             "treemapify")          # Usado para criar treemaps em R.

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
#------------------------------------ FIM -------------------------------------#