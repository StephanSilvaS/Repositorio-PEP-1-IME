# esta función tiene la limitante de que, al trabajar con dos proporciones,
# no permite establecer un valor nulo distinto de cero para la diferencia.

# Fijar valores conocidos (hombres, mujeres)
n <- c(c(48, 42))
exitos <- c(26, 20)
alfa <- 0.05
valor_nulo <- 0.0 # NO SE PERMITE ESTABLECER UN VALOR NULO DISTINTO DE CERO PARA
                  # LA DIFERENCIA

# Prueba de Wilson en R.
prueba <- prop.test(exitos, n = n, alternative = "two.sided",
                    conf.level = 1 - alfa)

print(prueba)
