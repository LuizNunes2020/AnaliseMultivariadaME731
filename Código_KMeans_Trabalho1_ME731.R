# Carregar as bibliotecas 
library(tidyverse)
library(cluster)
library(FactoMineR)  
library(factoextra) 
library(aplpack)

# Carregar os dados 
dados <- read_csv(file.choose())

# Converter colunas de porcentagem para valores numéricos
dados <- dados %>%
  mutate(across(c(kill_assists_survived_traded, headshot_percentage, clutch_success_percentage), 
                ~ as.numeric(str_remove(., "%")) / 100))

# Selecionar as variáveis numéricas para a análise PCA
dados_numericos <- dados %>% select(rating, 
                                    average_combat_score,
                                    kill_deaths,
                                    kill_assists_survived_traded,
                                    average_damage_per_round,
                                    kills_per_round, 
                                    assists_per_round,
                                    first_kills_per_round,
                                    first_deaths_per_round,
                                    headshot_percentage,
                                    clutch_success_percentage)

# Imputar a média para valores ausentes (NA)
dados_numericos <- dados_numericos %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Executar a Análise de Componentes Principais (PCA)
resultado_pca <- PCA(dados_numericos, scale.unit = TRUE, graph = FALSE)
importancia_variaveis <- resultado_pca$var$contrib
nomes_variaveis <- rownames(resultado_pca$var$coord)

# Ordenar as contribuições de forma decrescente e obter os índices das 5 mais importantes
indices_importantes <- order(importancia_variaveis[, 1], decreasing = TRUE)[1:5]  
variaveis_importantes <- nomes_variaveis[indices_importantes]

# Selecionar as cinco variáveis mais importantes para o algoritmo K-means
dados_para_kmeans <- dados_numericos %>% select(all_of(variaveis_importantes))

# Função para calcular a soma dos quadrados dentro dos clusters para o gráfico de cotovelo
wss <- function(k) {
  kmeans(dados_para_kmeans, k, nstart = 25)$tot.withinss
}

# Valores de k que vamos testar
k.values <- 1:10

# Computar a soma dos quadrados dentro dos clusters para cada k
wss_values <- map_dbl(k.values, wss)

# Gráfico de Cotovelo 
fviz_nbclust(dados_para_kmeans, kmeans, method = "wss") +
  labs(
    title = "Gráfico de Cotovelo",
    subtitle = "Método do Cotovelo para Determinação de k",
    x = "Número de Clusters (k)",
    y = "Soma dos Quadrados das Distâncias (WSS)"
  )

# Executar o K-means com k = 3 
melhor_kmeans <- kmeans(dados_para_kmeans, centers = 3, nstart = 25)

# Gráfico de silhueta para k=3
sil_kmeans <- silhouette(melhor_kmeans$cluster, dist(dados_para_kmeans))

# Calcular a largura média da silhueta
media_silhueta <- mean(sil_kmeans[, 3])

# Criar o gráfico de silhueta 
fviz_silhouette(sil_kmeans) + 
  labs(title = paste("Gráfico de Silhueta para K-means com k = 3 (Largura média: ", 
                     round(media_silhueta, 4), ")", sep = ""))

# Gráfico dos clusters
fviz_cluster(melhor_kmeans, data = dados_para_kmeans, ellipse.type = "norm") +
  labs(title = "Visualização dos clusters com K-means (k = 3)")