crear_poblacion <- function(popSize, n_variables, n_max){
  # Esta función crea una matriz binaria en la que el número de 1s por
  # fila no supera un valor máximo.
  # Argumentos:
  #   popSize:     número total de individuos (número de filas de la matriz).
  #   n_variables: longitud de los individuos (número de columnas de la matriz).
  #   n_max:       número máximo de 1 que puede contener un individuo.
  
  # Matriz donde almacenar los individuos generados.
  poblacion <- matrix(data = NA, nrow = popSize, ncol = n_variables)
  
  # Bucle para crear cada individuo.   
  for(i in 1:popSize){
    # Se selecciona con (igual probabilidad ) el número de valores = 1 que puede
    # tener el individuo.
    numero_1s <- sample(x = 1:n_max, size = 1)
    
    # Se crea un vector con todo ceros que representa el individuo.
    individuo <- rep(0, times = n_variables)
    
    # Se sustituyen (numero_1s) posiciones aleatorias por unos.
    individuo[sample(x = 1:n_variables, size = numero_1s)] <- 1
    
    # Se añade el nuevo individuo a la población.
    poblacion[i,] <- individuo
  }
  return(poblacion)
}

