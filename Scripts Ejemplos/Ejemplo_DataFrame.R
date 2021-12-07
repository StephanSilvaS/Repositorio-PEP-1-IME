# Crear el data frame.
Instancia <- 1:6
Quicksort <- c(23.2, 22.6, 23.4, 23.3, 21.8, 23.9)
Bubblesort <- c(31.6, 29.3, 30.7, 30.8, 29.8, 30.3)
Radixsort <- c(30.1, 28.4, 28.7, 28.3, 29.9, 29.1)
Mergesort <- c(25.0, 25.7, 25.7, 23.7, 25.5, 24.7)
df <- data.frame(Instancia, Quicksort, Bubblesort, Radixsort, Mergesort)

# Aplicar funciones en df para examinar el dataframe.

# Imprime en la consola las dimensiones del dataframe (nro. de filas y nro. de columnas, respectivamente).
dim(df)

# Imprime en la consola la estructura del dataframe.
str(df)

# Imprime en la consola datos relativos a cada columna presente en el dataframe,
# como el valor minimo presente en la columna, mediana, valor maximo, etc.
summary(df)

# Imprime en la consola los nombres de las columnas en el orden definido.
colnames(df)

# Las funciones head() y tail() muestran por defecto 6 FILAS, sin embargo, 
# se puede ajustar el numero de filas usando el argumento "n = ".

# Mostrara las primeras dos filas.
head(df, n = 2)

# Mostrara las ultimas tres filas.
tail(df, n = 3)

# Despues de que las 6 primeras funciones hayan sido impresas en la consola,
# la funcion View() abre la tabla en una ventana anexa.
View(df)