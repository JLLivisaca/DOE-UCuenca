#### ANÁLISIS DE CONGLOMERADOS - PARTE 2
library(dplyr)
library(kableExtra)

# Cargamos el data set
casos <- c("C1", "C2", "C3", "C4")
variables <- c("V1", "V2", "V3", "V4")
puntuaciones <- matrix(c(1, 2, 2, 1,
                         5, 7, 7, 5,
                         2, 2, 1, 1,
                         7, 5, 7, 5), nrow = 4, byrow = TRUE)

data <- data.frame(caso = rep(casos, each = length(variables)),
                   variable = rep(variables, times = length(casos)),
                   puntuacion = as.vector(puntuaciones))

# Lo guardamos con un nuevo nombre: df
df <- puntuaciones
head(df) %>%
      kable(caption = "Base de Datos: Arrestos por cada 100,000 residentes por asalto, asesinato y violación en cada uno de los 50 estados de EE. UU. en 1973. " ,
            align = "c",
            digits = 2) %>%
      kable_classic_2(html_font = "sans-serif",
                      lightable_options = c("hover", "striped")) %>%
      row_spec(0,
               bold = T,
               color = "white",
               background = "#219B6D")


### NOTA: Para remover registros con algun valor perdido, si se llevare a presentar, podemos hacer uso de:
df <- na.omit(df)
df <- as.data.frame(df)
# Normalizamos la data
df <- scale(df)
head(df) %>%
      kable(caption = "Base de Datos Normalizada " ,
            align = "c",
            digits = 6) %>%
      kable_classic_2(html_font = "sans-serif",
                      lightable_options = c("hover", "striped")) %>%
      row_spec(0,
               bold = T,
               color = "white",
               background = "#219B6D")




#### MÉTODO 4: AGGLOMERATIVE CLUSTERING
# Obtenemos la dissimilarity matrix (matriz de disimilitud o de distancia)
# df = the standardized data
res.dist <- dist(df, method = "euclidean")

### Funcion de Vinculacion.
### Se toma la informacion de dist()  y agrupa los objetos segun su similitud. 
### Es un proceso iterativo hasta que todos los registros esten vinculados a un 
### arbol jerarquico. Almacena el Arbol Jerarquico
res.hc <- hclust(d = res.dist, method = "ward.D2")

### Visualizamos el Dendograma o arbol jerarquico
# cex: label size
library("factoextra")
fviz_dend(res.hc, cex = 0.5)

# Calculamos las Distancias Cofeneticas. cophentic distance
res.coph <- cophenetic(res.hc)

# Correlation between cophenetic distance and
# the original distance
cor(res.dist, res.coph)

##Volvemos a ejecutar la funcion de vinculacion, pero vamos a 
## cambiar el metodo a “average.” Luego volvemos a calcular las 
## distancias cofeneticas y la correlacion entre estas con las 
## distancias orginales

### Volvemos a calcular la vinculacion, pero con el metodo de vinculacion promedio
res.hc2 <- hclust(res.dist, method = "average")
### Calculamos la correlacion
cor(res.dist, cophenetic(res.hc2))

# Cortamos el dendograma (arbol jerarquico ) en 4 grupos
grp <- cutree(res.hc, k = 6)
head(grp, n = 4)

# Para ver el Number of members in each cluster
table(grp)

# Para obtener los nombres de los miembros del cluster 1
rownames(df)[grp == 1]

# Para obtener los nombres de los miembros del cluster 3
rownames(df)[grp == 3]

# VISUZALICIÓN DEL DENDOGRAMA RECORTADO
library(factoextra)
# Cut in 4 groups and color by groups
fviz_dend(
      res.hc,
      k = 2,
      # Cut in four groups
      cex = 0.5,
      # label size
      k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
      color_labels_by_k = TRUE,
      # color labels by groups
      rect = TRUE # Add rectangle around groups
)

# VISUALIZACIÓN DE LAS AGRUPACIONES AGLOMERATIVAS EN UN GRÁFICO DE
# DISPERSIÓN / usa datos convexos
library(factoextra)
fviz_cluster(
      list(data = df, cluster = grp),
      palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
      ellipse.type = "convex",
      # Concentration ellipse
      repel = TRUE,
      # Avoid label overplotting (slow)
      show.clust.cent = FALSE,
      ggtheme = theme_minimal()
)

###### AGGLOMERATIVE CLUSTERING (UTILIZANDO LIBRERÍA CLUSTER)
library(cluster)
library(factoextra)
# Agglomerative Nesting (Hierarchical Clustering)
res.agnes <- agnes(
      x = df,
      # data matrix
      stand = TRUE,
      # Standardize the data
      metric = "euclidean",
      # metric for distance matrix
      method = "ward" # Linkage method
)

### Para visualizar la salida

fviz_dend(res.agnes, cex = 0.6, k = 2) # por los 2 clústers es mejor esto

#### DIVISIVE ANALYSIS CLUSTERING
## Lo inverso de la agrupación aglomerativa es la agrupación divisiva,
## que también se conoce como DIANA (Análisis de división) y 
## funciona de manera “de arriba hacia abajo.”

library(cluster)
library(factoextra)
# DIvisive ANAlysis Clustering
res.diana <- diana(x = df, # data matrix
                   stand = TRUE, # standardize the data
                   metric = "euclidean" # metric for distance matrix
)

### Para visualizar la salida
fviz_dend(res.diana, cex = 0.6, k = 2) # Es mejor aplicar a dos grupos, pero se pueden hacer 3


library(dendextend)
# Calculamos la matriz de disimilitud o de distancia
res.dist <- dist(df, method = "euclidean")
# Ejecuamos los agrupamientos jerarquicos
hc1 <- hclust(res.dist, method = "average")
hc2 <- hclust(res.dist, method = "ward.D2")
# Creamos los dos dendogramas
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)
# Creamos una lista que contenga los dos dendogramas
dend_list <- dendlist(dend1, dend2)

# Comparando Visualmente los dos dendogramas
tanglegram(dend1, dend2) # esta función compara los nodos de los dendogramas y se 
tanglegram(
      dend1,
      dend2,
      highlight_distinct_edges = FALSE,
      # Turn-off dashed lines
      common_subtrees_color_lines = FALSE,
      # Turn-off line colors
      common_subtrees_color_branches = TRUE,
      # Color common branches
      main = paste("entanglement =", round(entanglement(dend_list), 2))
)# entanglement es el entrelazamiento y es muy poco

## MATRIZ DE CORRELACIÓN ENTRE UNA LISTA DE DENDOGRAMAS

### Estas matriz se pueden usar cuando sean mas de dos dendogramas, 
### primero deben de estar en una lsita

# Matriz de Correlacion Cofenetica
cor.dendlist(dend_list, method = "cophenetic") 

# Matriz de Correlacion de Baker
cor.dendlist(dend_list, method = "baker") 

### Para tener la correlacion entre solo dos dendogramas, se puede asi:
# Cophenetic correlation coefficient
cor_cophenetic(dend1, dend2)

# Baker correlation coefficient
cor_bakers_gamma(dend1, dend2)

### Para comparar multiples dendogramas, se puede asi:
# Creamos multiples Dendogramas encadenando. Cada dendograma utiliza un metodo distinto
dend1 <- df %>% dist %>% hclust("complete") %>% as.dendrogram
dend2 <- df %>% dist %>% hclust("single") %>% as.dendrogram
dend3 <- df %>% dist %>% hclust("average") %>% as.dendrogram
dend4 <- df %>% dist %>% hclust("centroid") %>% as.dendrogram

# Elaboramos la Matriz de Correlaciones
dend_list <- dendlist(
      "Complete" = dend1,
      "Single" = dend2,
      "Average" = dend3,
      "Centroid" = dend4
)
cors <- cor.dendlist(dend_list)
# Print correlattion matrix
round(cors, 2) %>%
      kable(caption = "Matriz de Correlacion de Multiples Dendogramas",
            align = "c",
            digits = 6) %>%
      kable_classic_2(html_font = "sans-serif",
                      lightable_options = c("hover", "striped")) %>%
      row_spec(0,
               bold = T,
               color = "white",
               background = "#219B6D")

# Visualize the correlation matrix using corrplot package
library(corrplot)
corrplot(cors, "pie", "lower")


###### VISUALIZACIÓN DE DENDOGRAMAS

# Calculamo las distancias y los agrupamiento jerárquico
dd <- dist((df), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")

## VISUALIZACIÓN
library(factoextra)
fviz_dend(hc, cex = 0.5)

### Personalizandolo 
fviz_dend(hc, cex = 0.5,
          main = "Dendrogram - ward.D2",
          xlab = "Objects", 
          ylab = "Distance", 
          sub = "")

### Para poner en una posicion horizontal el dendograma
fviz_dend(hc, cex = 0.5, horiz = TRUE)

### Realizamos la Particion o corte del Arbol en multiples grupos. 
fviz_dend(hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          rect_fill = TRUE)

### Para cambiar el tema del arbol, usamos ggtheme
fviz_dend(hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          ggtheme = theme_gray() # Change theme
)
### Se permiten en ggtheme = : theme_gray(), theme_bw(), theme_minimal(), 
### theme_classic(), theme_void()] 

## Los valores permitidos para k_color incluyen paletas de 
## cerveza del paquete RColorBrewer (p. Ej., “RdBu,” “Blues,” 
## “Dark2,” “Set2,”…;) Y paletas de revistas científicas del 
## paquete ggsci R (p. Ej., “Npg,” “aaas,”“ Lanceta ”,“ jco ”,
## “ ucscgb ”,“ uchicago ”,“ simpsons ”y“ rickandmorty ”). 
## En el código R a continuación, cambiaremos los colores del 
##grupo usando “jco” (revista de oncología clínica) paleta de color:
fviz_dend(hc,
          cex = 0.5,
          k = 4,
          # Cut in four groups
          k_colors = "jco")

### Poniendolo horizontal
fviz_dend(
      hc,
      k = 4,
      cex = 0.4,
      horiz = TRUE,
      k_colors = "jco",
      rect = TRUE,
      rect_border = "jco",
      rect_fill = TRUE
)

### Para crear un dendograma circular
fviz_dend(
      hc,
      cex = 0.5,
      k = 4,
      k_colors = "jco",
      type = "circular"
)

### Para trazar un árbol de tipo filogénico, 
### use type = "phylogenic" y repel = TRUE (para evitar la
## superposición de etiquetas).
library(igraph)
fviz_dend(
      hc,
      k = 2,
      k_colors = "jco",
      type = "phylogenic",
      repel = TRUE
)

## OTRO GRÁFICO
library(igraph)
fviz_dend(
      hc,
      k = 2,
      # Cut in four groups
      k_colors = "jco",
      type = "phylogenic",
      repel = TRUE,
      phylo_layout = "layout.gem"
)


### Trazado de un subarbol

#### Cortar el dendrograma y visualizar la versión truncada:
# Create a plot of the whole dendrogram, and extract the dendrogram data
dend_plot <- fviz_dend(hc,
                       k = 2,
                       # Cut in four groups
                       cex = 0.5,
                       # label size
                       k_colors = "jco")
dend_data <- attr(dend_plot, "dendrogram") # Extract dendrogram data
# Cut the dendrogram at height h = 2
dend_cuts <- cut(dend_data, h = 2)
# Visualize the truncated version containing
# two branches
fviz_dend(dend_cuts$upper)

##### Trazar subárboles dendrogramas:
# Plot the whole dendrogram
print(dend_plot)

# Plot subtree 1
fviz_dend(dend_cuts$lower[[1]], main = "Subtree 1")

# Plot subtree 2
fviz_dend(dend_cuts$lower[[2]], main = "Subtree 2")



