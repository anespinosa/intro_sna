# ---------------------------------------------------------------------------- #
rm(list=ls())

library(here)
setwd(here::here('001vignettes'))

# ---------------------------------------------------------------------------- #
## DESCARGAR LOS DATOS DESDE classicnets:
# install.packages("devtools")
# library(devtools)
# devtools::install_github("anespinosa/classicnets")
library(classicnets)
data("eies")
?eies

## DESCARGAR LOS DATOS DESDE EL PC:
load('eies.rda')

#### ROSSI AND MAGNANI ####
# install.packages("igraph")
library(igraph)

## DESCARGAR LOS DATOS DESDE EL PC:
load('gaucs.Rda')

# ---------------------------------------------------------------------------- #
as.data.frame(cbind(label = eies$label, 
                    citations = eies$citations, 
                    discipline = eies$discipline))

# Matrices
eies$time1
eies$time2
eies$messages

# ---------------------------------------------------------------------------- #
# install.packages("devtools")
# devtools::install_github("anespinosa/netmem")
library(netmem)
netmem::matrix_report(eies$time1)

# ---------------------------------------------------------------------------- #
A <- matrix(c(1,0,0,0,
              0,1,0,0,
              0,1,1,1,
              0,0,0,0,
              0,0,0,1), byrow = TRUE, ncol = 4)
B <- matrix(c(1,1,1,0,0,
              0,0,1,0,0,
              0,0,1,1,0,
              0,0,0,1,1), byrow = TRUE, ncol = 5)

netmem::matrix_report(A)
netmem::matrix_report(B)

# ---------------------------------------------------------------------------- #
# Lista de objetos igraph
gaucs

# Cada red por separada
gaucs$coauthor
gaucs$facebook
gaucs$leisure
gaucs$lunch
gaucs$work

# Ejercicio: Todas las redes tienen la misma cantidad de investigadores?

# Todas las redes en un mismo objeto
gaucs$`_flat_`

# ---------------------------------------------------------------------------- #
# Utilizaremos EIES
igraph::graph.adjacency(eies$time1) # WARNING!

eiesT1 <- eies$time1
rownames(eiesT1) <- eies$label
colnames(eiesT1) <- rownames(eiesT1) # por qué?
matrix_report(eiesT1)

igraph::graph.adjacency(eiesT1) # WARNING!
igraph::graph.adjacency(eiesT1, weighted = TRUE)

# Agregar atributos
g1 <- igraph::graph.adjacency(eiesT1, weighted = TRUE)
V(g1)$citations <- eies$citations
V(g1)$disciplines <- eies$discipline
g1

# Visualización:
plot(igraph::graph.adjacency(eiesT1, weighted = TRUE))

# ---------------------------------------------------------------------------- #
library(network)
network::network(eiesT1)
network::plot.network(network::network(eiesT1))

sna <- network::network(eiesT1, multiple = TRUE)

# Agregar algunos atributos
sna %v% 'citations' <- eies$citations
sna %v% 'discipline' <- eies$discipline
sna %e% 'messages' <- eies$messages
sna

# ---------------------------------------------------------------------------- #
gaucs$work <- igraph::upgrade_graph(gaucs$work)
matrix <- igraph::get.adjacency(gaucs$work, sparse = FALSE)
dim(matrix)
head(matrix)
matrix[1,] # emisor
matrix[,1] # receptor
table(matrix[1,] == matrix[,1]) # Ejercicio: Por qué ambos son TRUE? 

ncol(matrix) # número de nodos
sum(matrix)/2 # número de vínculos

vcount(gaucs$work) == ncol(matrix)
ecount(gaucs$work) == sum(matrix)/2 # Ejercicio: Por qué la matriz se divide por dos?

# ---------------------------------------------------------------------------- #
matrix_report(eies$time1)

# Ejercicio: En cuál de los dos momentos del tiempo hay más vínculos de amistad? 
table(eies$time1) # tiempo 1
table(eies$time2) # tiempo 2

# Ejercicio: Qué tanto cambiaron los vínculos del tiempo 1 al tiempo 2? 
table(eies$time1, eies$time2)

# ---------------------------------------------------------------------------- #
# Matriz de adjacencia
matrix <- ifelse(eies$time1 > 2, 1, 0)
dim(matrix)
head(matrix)
matrix[1,] # Emisor
matrix[,1] # Receptor
table(matrix[1,] == matrix[,1]) # Ejercicio: Por qué algunos son TRUE y otros FALSE? 

ncol(matrix) # número de nodos
sum(matrix) # número de aristas
matrix_report(matrix)

# ---------------------------------------------------------------------------- #
dim(A)
rowSums(A)
colSums(A)

A %*% t(A) # Ejercicio: Qué significa esta nueva red?
t(A) %*% A # Ejercicio: Qué significa esta nueva red?

# ---------------------------------------------------------------------------- #
(edgelist <- netmem::matrix_to_edgelist(eiesT1, digraph = TRUE))
igraph::get.edgelist(g1)
network::as.edgelist(sna)
# Ejercicio: Qué pasa con los casos aislados? Cuáles son algunas de las diferencias entre estas funciones? Por qué en algunos casos es preferible utilizar listas de relaciones en vez de matrices?

# ---------------------------------------------------------------------------- #
igraph::graph.edgelist(edgelist)
igraph::graph.data.frame(edgelist, 
                         vertices = as.data.frame(cbind(
                           label = eies$label, 
                           citations = eies$citations, 
                           discipline = eies$discipline
                         )))
network::network(edgelist)
network::network(edgelist, 
                 vertex.attr = as.data.frame(cbind(
                   label = eies$label, 
                   citations = eies$citations, 
                   discipline = eies$discipline
                 )))

# ---------------------------------------------------------------------------- #
(adjlist <- netmem::matrix_adjlist(eiesT1))
igraph::get.adjlist(g1)

# Ejercicio: En qué contexto puede ser útil utilizar listas de adyacencias?

