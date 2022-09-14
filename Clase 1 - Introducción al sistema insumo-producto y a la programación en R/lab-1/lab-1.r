# -------------------------------------------------------------------------------------------
# Curso: An?lisis de estructuras productivas y cadenas de valor con tecnicas insumo producto 
# R Lab 1
# -------------------------------------------------------------------------------------------

# Introduccion a R --------------------------------------------------------

# asignacion --------------------------------------------------------------

x <- 1
x = 1
x
y = x
y = 4

# comentario

# operadores --------------------------------------------------------------

# aritmeticos

y + x # suma
z <- y + x # asigno el resultado de una suma
y - x # resta
y * x # multiplicacion
4 / 8 # division
2**5 # exponente
2^5 # exponente

# relacionales

x == 1
x == y
y > x
y!=x # distinto

# vectores atomicos --------------------------------------------------------------

a <- c(4,5,1,3) # es un vector "atomico", sus componentes de igual tipo e indivisibles
class(a) # tipos: character, numeric, logical, complex, integer

a[2] # elemento numero 2
a[1:3] # elementos 1 a 3
a[c(2,4)] # elementos 2 y 4
a[-2] # todos menos el 2
a*2 # si aplico una operacion a un vector se aplica a todos sus componentes

b = c("a","b","c","d")
class(b)
names(a)
names(a) <- b
a
a['b'] # ahora que tienen nombres puedo llamar tambien por nombre

# matrices ----------------------------------------------------------------

matrix(a)
mat <- matrix(a,nrow=2,ncol=2) # los elementos se acomodan por columnas
mat[2] # no se suele usar
mat[1,2] # objeto de la fila 1 y columna 2
dim(mat)
mat*2

# Armar una matriz IP en R ------------------------------------------------

Z <- matrix(c(3,6,5,10), ncol=2) # Matriz de transacciones intermedias
Z
Z [1,2] # elemento en posicion (1,2) de la matriz Z

class(Z)
dim(Z)

colnames(Z) <- c("agric", "manuf")
rownames(Z) <- colnames (Z)
Z
Z[,"agric"]

f <- matrix(c(7,4),ncol=1) # demanda final
dimnames(f) <- list(rownames(Z),c("f"))
f
x <- rowSums(Z)+f # producto bruto
colnames(x) <- c("x")
x

m <- matrix(c(2,1),ncol=1) # importaciones intermedias
dimnames(m) <- list(rownames(Z),c("m"))

y <- x-colSums(Z)-m # valor agregado
colnames(y) <- c("y")
y
cbind(y,m,f,y+m-f)

IO_matrix <- rbind(cbind(Z,f,x),cbind(t(m),0,0),cbind(t(y),0,0),cbind(t(x),0,0))
IO_matrix

write.csv(IO_matrix,'C:/Users/Usuario/Desktop/CAPACITACIONES CEPAL 2022/Curso IP 2022/Clase 1 - Introducción al sistema insumo-producto y a la programación en R/lab-1/results/IO_matrix.csv')

# Control: sistemas de gasto e costo-ingreso
rowSums(Z)+f-x # sistema de gasto
colSums(Z)+y+m-x # sistema de costo-ingreso

A <- Z%*%diag(c(1/x)) # Requerimientos intermedios por unidad de producto. Usamos %*% para multiplicar matrices

# Sistema de gasto en terminos intensivos

A%*%x+f-x # Sistema de gasto en terminos intensivos

# Sistema de costo-ingreso en terminos intensivos 

a_m <- m/x
a_y <- y/x
u <- matrix(c(1),nrow=2,ncol=1)
t(u)%*%A+t(a_m)+t(a_y) # Sistema de ingreso en terminos intensivos

