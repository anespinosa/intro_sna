# ---------------------------------------------------------------------------- #
rm(list=ls())
setwd(here::here('001vignettes'))

# ---------------------------------------------------------------------------- #
#### FREEMAN AND FREEMAN ####

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
# Matriz de adjacencia
matrix <- ifelse(eies$time1 > 2, 1, 0)
dim(matrix)
head(matrix)
matrix[1,] # Emisor
matrix[,1] # Receptor

ncol(matrix) # número de nodos
sum(matrix) # número de aristas

# Lista de relaciones
edgelist <- matrix_to_edgelist(matrix, digraph = TRUE)

# ---------------------------------------------------------------------------- #
sum(matrix)/(ncol(matrix)*(ncol(matrix)-1))

library(netmem)
netmem::gen_density(matrix, directed = TRUE)

library(igraph)
g1 <- igraph::graph.adjacency(matrix, mode=c("directed"))
igraph::graph.density(g1)

library(sna)
sna1 <- network::network(matrix)
sna::gden(sna1, mode="digraph")

# ---------------------------------------------------------------------------- #
rowSums(matrix)
colSums(matrix)

igraph::degree(g1, mode = c('out'))
igraph::degree(g1, mode = c('in'))

sna::degree(sna1, gmode = "digraph", cmode = 'outdegree')
sna::degree(sna1, gmode = "digraph", cmode = 'indegree')

# Distribuciones de grados nodales
plot(table(rowSums(matrix)))
plot(table(colSums(matrix)))

# ---------------------------------------------------------------------------- #
((1/2)*sum(diag(matrix%*%matrix)))/sum(matrix)
# Es la mutualidad alta en esta red?

# ---------------------------------------------------------------------------- #
netmem::dyadic_census(matrix)
igraph::dyad.census(g1) 
sna::dyad.census(sna1)

# ---------------------------------------------------------------------------- #
igraph::triad.census(g1)
sna::triad.census(sna1)
netmem::triad_uman(matrix)

# ---------------------------------------------------------------------------- #
sna::gtrans(sna1) # weak transitivity
netmem::trans_coef(matrix)

igraph::transitivity(g1) # global transitivity
netmem::trans_coef(matrix, method = c('global'))

# ---------------------------------------------------------------------------- #
sna::components(sna1)
sna::components(sna1, connected = 'weak')
sna::components(sna1, connected = 'strong')

igraph::components(g1)
igraph::components(g1, mode = c('weak'))
igraph::components(g1, mode = c('strong'))

netmem::components_id(matrix) # strong component!

# Ejercicio: Cuántos casos "aislados" existen si es que se consideran componentes fuertes (i.e., "strong"). Son realmente aislados?

# ---------------------------------------------------------------------------- #
time1 <- ifelse(eies$time1 > 2, 1, 0)
time2 <- ifelse(eies$time2 > 2, 1, 0)

netmem::jaccard(time1, time2)

# ---------------------------------------------------------------------------- #
set.seed(12354)
rownames(matrix) <- eies$label
colnames(matrix) <- eies$label
plot(graph.adjacency(matrix, mode = c('directed')),
     edge.arrow.size=0.3, vertex.size=5, vertex.label=NA)

# Elegir un actor de forma aleatoria!
actor <- sample(eies$label, 1) 
ego_net(matrix, ego = actor)

# Explorando algunas redes!
plot(graph.adjacency(ego_net(matrix, ego = 'Lin Freeman'),
                     mode = c('directed')),
     edge.arrow.size=0.3,vertex.size=5,
     main = "Ego-red de Linton Freeman")
plot(graph.adjacency(ego_net(matrix, ego = 'Ev Rogers'),
                     mode = c('directed')),
     edge.arrow.size=0.3,vertex.size=5,
     main = "Ego-red de Everett Rogers")
plot(graph.adjacency(ego_net(matrix, ego = 'Nick Mullins'),
                     mode = c('directed')),
     edge.arrow.size=0.3,vertex.size=5,
     main = "Ego-red de Nicholas Mullins")
plot(graph.adjacency(ego_net(matrix, ego = 'Mark Granovetter'),
                     mode = c('directed')),
     edge.arrow.size=0.3,vertex.size=5,
     main = "Ego-red de Mark Granovetter")

# ---------------------------------------------------------------------------- #
matrix <- get.adjacency(gaucs$work, sparse = FALSE)
egoU4 <- ego_net(matrix, ego = 'U4')
redundancy <- mean(rowSums(egoU4))
effective_size <- ncol(egoU4) - redundancy
(efficiency <- effective_size/ncol(egoU4)) # Qué tan redundante es la red de U4?

# ---------------------------------------------------------------------------- #
library(netmem)
addegoU4 <- rbind(egoU4, U4=rep(1, nrow(egoU4)))
addegoU4 <- cbind(addegoU4, U4=rep(1, nrow(addegoU4)))
diag(addegoU4) <- 0
eb_constraint(addegoU4, ego="U4")

igraph::constraint(graph.adjacency(addegoU4))['U4']

# ---------------------------------------------------------------------------- #
g <- igraph::graph.adjacency(matrix)
deg <- igraph::degree(g)
clos <- igraph::closeness(g) # CUIDADO!
betw <- igraph::betweenness(g)
ev <- igraph::evcent(g)$vector

sort(betw, decreasing = TRUE)
sort(eigen_centrality(g)$vector, decreasing = TRUE)

op <- par(mfrow = c(1, 3))
plot(deg, betw, xlab="Degree", ylab="Betweenness", col="blue") 
plot(deg, ev, xlab="Degree", ylab="Eigenvector", col="blue") 
plot(betw, ev, xlab="Betweenness", ylab="Eigenvector", col="blue")

cor(ev, deg)
cor(ev, betw)

set.seed(1234)
par(mfrow = c(2, 2))
layout <- layout.fruchterman.reingold(g)
plot(g,layout = layout,
     vertex.label=NA,
     edge.arrow.size=0.1,
     main="EIES")
plot(g, layout=layout, 
     vertex.label=NA,
     vertex.size=deg*0.75, 
     edge.arrow.size=0.1,
     vertex.label.cex=0.6, main="Degree Centrality")

plot(g, layout=layout,
     vertex.label=NA,
     vertex.size=betw*0.05, 
     edge.arrow.size=0.1,
     vertex.label.cex=0.6, main="Betweenness Centrality")

plot(g, layout=layout,
     vertex.label=NA,
     vertex.size=ev*20, 
     edge.arrow.size=0.1,
     vertex.label.cex=0.6, main="Eigenvector Centrality")
