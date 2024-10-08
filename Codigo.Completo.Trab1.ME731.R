# Carregar as bibliotecas 
library(tidyverse)
library(cluster)
library(FactoMineR)  
library(factoextra) 
library(aplpack)

# Carregar os dados 
dados <- read_csv(file.choose()) #baixe o arquivo "dados_valorant.csv"

# Converter colunas de porcentagem para valores numéricos
dados <- dados %>%
  mutate(across(c(kill_assists_survived_traded, headshot_percentage,clutch_success_percentage), 
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

# Remover todos os dados faltantes (NA)
dados_numericos <- dados_numericos %>%
  drop_na()

# Executar a Análise de Componentes Principais (PCA)
resultado_pca <- PCA(dados_numericos, scale.unit = TRUE, graph = FALSE)
importancia_variaveis <- resultado_pca$var$contrib
nomes_variaveis <- rownames(resultado_pca$var$coord)

# Ordenar as contribuições de forma decrescente e obter os índices das 5 mais importantes
indices_importantes <- order(importancia_variaveis[, 1], decreasing = TRUE)[1:5]

# Obter os nomes das cinco variáveis mais importantes
variaveis_importantes <- nomes_variaveis[indices_importantes]

# Selecionar as cinco variáveis mais importantes para o algoritmo PAM
dados_para_pam <- dados_numericos %>% select(all_of(variaveis_importantes))

# # Determinação do número k de agrupamentos
pams <- list()

for(i in 2:(nrow(dados_para_pam)-1)){
  pam_model <- pam(dados_para_pam, i)
  pams[[i-1]] <- c(i, pam_model$silinfo$avg.width) # Armazenar o número de clusters e a largura média da silhueta
}

pams_df <- do.call(rbind, pams)
colnames(pams_df) <- c("k", "s_barra_k")

pams_df %>%
  data.frame() %>%
  arrange(desc(s_barra_k)) %>%
  head() %>%
  mutate(s_barra_k = round(s_barra_k, 4))


# Aplicação do PAM com k=2
melhor_pam <- pam(dados_para_pam, 2)  
melhor_pam$medoids
melhor_pam$id.med   

# Informações de cada grupo
melhor_pam$clusinfo
melhor_pam$silinfo

# Faces de Chernoff dos medoids (índices)
faces(dados_para_pam[melhor_pam$id.med,], labels = melhor_pam$id.med)

# Faces de Chernoff do primeiro grupo
faces(dados_para_pam[melhor_pam$clustering == 1,], labels = "", face.type = 1)

# Faces de Chernoff do segundo grupo - primeira metade
n_grupo2 <- sum(melhor_pam$clustering == 2) 
metade <- floor(n_grupo2 / 2) # arredondar para baixo

faces(dados_para_pam[melhor_pam$clustering == 2,][1:metade,], labels = "", face.type = 1)

# Faces de Chernoff do segundo grupo - segunda metade
faces(dados_para_pam[melhor_pam$clustering == 2,][(metade + 1):n_grupo2,], labels = "", face.type = 1)

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
