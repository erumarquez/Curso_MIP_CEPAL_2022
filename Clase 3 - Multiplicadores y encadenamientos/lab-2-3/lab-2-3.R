# -------------------------------------------------------------------------------------------
# Curso: Análisis de estructuras productivas y cadenas de valor con tecnicas insumo producto 
# R Lab - Parte 3
# -------------------------------------------------------------------------------------------

# Clase 2 ---------------------------------------------------------

# Carga de matrices -------------------------------------------------------

rm(list=ls()) # Limpiar el espacio de trabajo

"%+%" <- function(x,y) paste(x,y,sep="")
#
setwd("C:/Users/Matia/Google Drive/CEPAL/Curso-IP/Curso IP 2022/Curso IP 2022/Clase 3 - Multiplicadores y encadenamientos/lab-2-3/") # Cambiar directorio
getwd()
data_dir <- getwd() %+% "/data/"
results_dir <- getwd() %+% "/results/"
#
str_ctry <- "ARG" # Cambiar en funcion del pais 
str_yr <- "2018"
tbl_IP <- as.matrix(read.csv(data_dir %+% str_ctry %+% "_" %+% str_yr %+% ".csv",row.names = 1,header=TRUE,sep=",",dec=".")) # Traer a R el archivo ARG_2018.csv de la carpeta data
dim(tbl_IP)
tbl_ind <- data.frame(read.csv(data_dir %+% "tbl_ind.csv",header=TRUE,sep=",",dec=".")) # Traer el archivo tbl_ind, hago un data frame porque tiene texto

q_ind <- 45 # Numero de industrias
q_f <- 8 # Tipos de demanda final
u <- matrix(c(1),nrow=q_ind,ncol=1) # Vector de unos (1)
u
I <- diag(c(u)) # Matriz identidad
u_f <- matrix(c(1),nrow=q_f,ncol=1) # Vector de unos del largo de la demanda final
u_f

Z <- tbl_IP[1:45,1:45] # Transacciones intermedias
Z <- tbl_IP[1:q_ind,1:q_ind] # Transacciones intermedias

dimnames(Z) <- list(tbl_ind[,'cod'],tbl_ind[,'cod'])
Z[1:5,1:5]
dim(Z)
class(Z)

F <- tbl_IP[1:q_ind,(q_ind+1):(q_ind+q_f)] # Demanda final
colnames(F) <- c("HH_cons","NPISH_cons","Gov_cons","FBKF","Invent","Cons_ext","Cons_nores","Expo")
rownames(F) <- rownames(Z)
F[1:5,]
dim(F)

f <- F%*%u_f # Demanda final (como vector)
f

m <- matrix(tbl_IP[q_ind+1,1:q_ind],ncol=1) # Importaciones intermedias
dim(m)
m_f <- matrix(tbl_IP[q_ind+1,(q_ind+1):(q_ind+q_f)],ncol=1) # Importaciones finales

Y <- tbl_IP[(q_ind+2):(q_ind+3),1:q_ind] # Matriz de valor agregado (VA + impuestos netos de subsidios)
y <-matrix(colSums(Y),ncol=1) # Vector de Valor agregado (VA)
x <- matrix(tbl_IP[q_ind+4,1:q_ind],ncol=1) # VBP
rownames(y) <- rownames(x) <- rownames (m) <- rownames(Z) # la asignacion va en el sentido de la flecha

# Controles
round(x-colSums(Z)-m-y,2) # Sistema de ingreso (debe ser 0, pero no lo es)
adj <- x-colSums(Z)-m-y # definimos la diferencia (minima!) como ajuste
y <- y+adj # y la imputamos al valor agregado
round(x-colSums(Z)-m-y,2) # ahora el sistema de ingreso cierra, tautologicamente

round(x-rowSums(Z)-f,2) # Sistema de gasto (al igual que el sistema de ingreso, deberia cerrar)
adj_f <- x-rowSums(Z)-f # definimos la diferencia como ajuste
f <- f+adj_f # la imputamos a la demanda final
round(x-rowSums(Z)-f,2) # Sistema de gasto (ahora cierra)

# Sistema de gasto en terminos intensivos ---------------------------------

inv_x <- 1/x
inv_x[is.infinite(inv_x)] <- 0
A <- Z%*%diag(c(inv_x)) # A es "Z/x" (matriz de requerimientos directos de insumos por unidad de producto)
round(A[1:5,1:5],2)

round(A%*%x+f-x,2) # Control: el sistema de gasto se cumple

B <- solve(I-A) # Matriz de requerimientos directos e indirectos por unidad de producto
round(B%*%f-x,2) # control

# Sistema de ingreso en terminos intensivos -------------------------------

a_y <- y/x # Valor agregado por unidad de producto bruto
a_y[is.nan(a_y)] <- 0

a_m <- m/x # Insumos intermedios importados por unidad de producto
a_m[is.nan(a_m)] <- 0

p <- t(t(a_y+a_m)%*%B) # Vector de precios relativos

# Alternativa: sistema de ingresos con matriz D (matriz de cuotas de mercado) --------

D <- diag(c(inv_x))%*%Z # Matriz de cuotas de mercado
G <- solve(I-D) # Matriz de Ghosh
G[1:5,1:5]
round(t(t(y+m)%*%G)-x,2)

# Lab 3 ---------------------------------------------------------

#install.packages('tidyverse')
library(tidyverse)

# Multiplicadores ---------------------------------------------------------

# Multiplicador de produccion

mult_prod <- t(t(u)%*%B) # Incremento en el producto bruto de la economia por unidad de demanda final
cbind(mult_prod,tbl_ind['desc_short'])[order(-mult_prod[,1]),,drop=FALSE]

# Multiplicador de empleo

tbl_empn <- data.frame(read.csv(data_dir%+%"EMPN.csv",dec=".",sep="|",header=FALSE)) # tabla con datos de empleo
colnames(tbl_empn) <- c('var','ctry','partner','sector','year','value')
l <- matrix(filter(tbl_empn,var=="EMPN",ctry==str_ctry,year==str_yr)$value[1:q_ind],ncol=1)
sum(l)==filter(tbl_empn,var=="EMPN",ctry==str_ctry,year==str_yr,sector=="DTOTAL")$value # control

tbl_rta <- data.frame(read.csv(data_dir%+%"LABR.csv",dec=".",sep="|",header=FALSE)) # tabla con datos de empleo
colnames(tbl_rta) <- c('var','ctry','partner','sector','year','value')
rta <- matrix(filter(tbl_rta,var=="LABR",ctry==str_ctry,year==str_yr)$value[1:q_ind],ncol=1)
sum(rta)==filter(tbl_rta,var=="LABR",ctry==str_ctry,year==str_yr,sector=="DTOTAL")$value # control. Da mal, pero por apenas 1 centavo

w <- rta/l # salarios por trabajador

a_l <- l/x
a_l[is.nan(a_l)] <- 0
mult_emp <- t(t(a_l)%*%B)
round(mult_emp,2)

emp_vi <- t(t(a_l)%*%B%*%diag(c(f))) # empleo directo e indirecto de cada rama (verticalmente integrado)
rownames(emp_vi) <- rownames(Z)
round(sum(emp_vi)-sum(l),2)
tbl_emp_vi <- cbind(tbl_ind['desc_short'],emp_vi,l,emp_vi-l)

# Requerimientos importados

a_m <- m/x
mult_imp <- t(t(a_m)%*%B)

imp_vi <- t(t(a_m)%*%B%*%diag(c(f))) # importaciones por sector de demanda final (integracion vertical)
round(sum(m)-sum(imp_vi),2) # el total de impo es identico
data.frame(tbl_ind$desc,m,imp_vi,m-imp_vi) # importaciones intermedias del sector vs importaciones intermedias verticalmente integradas

bc_vi <- cbind(F[,'Expo']-m,F[,'Expo']-imp_vi,m-imp_vi) # resultado comercial estandar y verticalmente integrado
dimnames(bc_vi) <- list(tbl_ind$desc_short,c('Balance Comercial','Balance Comercial VI','Diferencia'))

# Requerimientos importados en las exportaciones

expo <- matrix(rowSums(F[,c('Expo','Cons_nores')]),ncol=1)
mult_impo_expo <- t(t(a_m)%*%B%*%diag(c(expo)))/expo
mult_impo_expo[is.nan(mult_impo_expo)] <- 0
rownames(mult_impo_expo) <- rownames(Z)
cbind(mult_impo_expo,tbl_ind['desc_short'])[order(-mult_impo_expo[,1]),,drop=FALSE]

# Multiplicador de produccion cerrado respecto al consumo

a_c <- w%*%(F[,'HH_cons']/sum(F[,'HH_cons'])) # 
f_z <- f - t(a_c)%*%l
B_cl <- solve(I - A - t(a_c)%*%diag(c(a_l)))
mult_prod_cl <- t(t(u)%*%B_cl)
round(x - B_cl%*%f_z, 5) # el vector de vbp total es el mismo que en el modelo abierto

# Multiplicador de empleo en el modelo cerrado respecto al consumo

mult_emp_cl <- t(t(a_l)%*%B_cl)
sum(l)-t(mult_emp_cl)%*%f_z # el gasto exogeno genera la totalidad de los empleos, a traves de los requerimientos tecnicos y el consumo inducido

# Linkages ----------------------------------------------------------------

bl <- t(t(u)%*%B) # Backward linkages (BL)
fl <- t(t(G%*%u)) # Forward linkages (FL)
bl_n <- bl/mean(bl) # BL normalizados
fl_n <- fl/mean(fl) # FL normalizados

BL_FL <- cbind(tbl_ind['desc_short'],"bl"=c(bl_n), "fl"=c(fl_n),"y"=y)

tax <- NULL

for (i in 1:5){ #ejemplo: como hacer un loop
  print(i)
}

for (i in 1:q_ind){

  tax <- rbind(tax,ifelse(BL_FL[i,'bl']>1&BL_FL[i,'fl']>1,"Clave",
                        ifelse(BL_FL[i,'bl']>1&BL_FL[i,'fl']<1,"Impulsor",
                               ifelse(BL_FL[i,'bl']<1&BL_FL[i,'fl']>1,"Impulsado",
                                      ifelse(BL_FL[i,'bl']<1&BL_FL[i,'fl']<1,"Independiente","Otros")))))
  }

BL_FL <- cbind(BL_FL,tax)
write.csv(BL_FL,results_dir%+%"BL_FL.csv")

plot(bl_n,fl_n,type="p",xlab="Backward Linkages",ylab="Forward Linkages")
abline(h=1)
abline(v=1)
text(bl_n,fl_n,labels=BL_FL[,'desc_short'],cex=0.6,pos=4)
dev.off()

# Backward linkage en el modelo cerrado

bl_cl <- t(t(u)%*%B_cl)
cbind(bl_cl,bl) # Los multiplicadores cerrados son siempre mas elevados

# Tabla resumen

tbl_indic <- cbind(tbl_ind$desc_short,mult_prod,mult_emp,mult_imp,mult_prod_cl,mult_emp_cl,mult_impo_expo,l,emp_vi,bc_vi[,c(2:3)],BL_FL[,c(2,3,5)])
colnames(tbl_indic) <- c('sector','mult_prod','mult_emp','mult_impo','mult_prod_cl','mult_emp_cl','Hummels','Empleo','Empleo_vi','BC Estándar','BC Vert Int','BL','FL','Tipologia')
write.csv(tbl_indic,results_dir%+%'Indicadores por Sector.csv')

# Grafo ------------------------------------------------------------------

#install.packages("igraph")
library(igraph)

A_trade <- A-diag(c(diag(A))) # Matriz A sin su diagonal
alpha <- 7 # Linkages minimos entre sectores, para reducir la cantidad de sectores en el grafo
A_trade_filter <- A_trade
dimnames(A_trade_filter) <- list(tbl_ind[,'desc_short'],tbl_ind[,'desc_short'])
a_cut_off <- mean(A_trade)*alpha
A_trade_filter[which(A_trade_filter<a_cut_off)] <- 0 # Eliminar los linkages menores que alpha veces el promedio
dim(A_trade_filter)
#
G <- graph_from_adjacency_matrix(A_trade_filter, mode = c("directed"), weighted=TRUE) # crear el grafo
V(G)$color <- ifelse(BL_FL[,'tax']=="Clave","orange","lightblue")
G <- delete.vertices(simplify(G), degree(G)==0) # eliminar los nodos sin linkages
#V(G)$color <- ifelse(BL_FL[V(G)$name,] %in% tbl_SKey[,'desc_short'], 'orange', 'lightblue')
V(G)$size <- log(degree(G)+1)*5
V(G)$label.dist <- 1.2
# plotear
png(results_dir%+%"A_net.png",width=15,height=15,units="cm",res=500)
par(mar=c(0.5,1.5,0.5,1.5))
plot(G, edge.arrow.size=0.6, edge.color="#9ecae1", vertex.label.color="black", edge.curved=0.1, vertex.label.cex=0.6)
dev.off()

