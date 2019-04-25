
#Las librerias que necesitaremos hoy

library(dplyr)
library(ggplot2)
library(purrr)
library(broom)
library(tidyr)
library(datasets)


#####################################################################
#1) Usando nest y unnest del paquete tidyr
#####################################################################

#Anidando nuestros datos

iris <- datasets::iris

names(iris) <- c("sep_length", "sep_width", "pet_length", "pet_width", "species")

#cada datapoint representa las mediciones hechas a una flor de una especis particular
iris %>% glimpse()

#Nuestro dataset original es de la clase data.frame
iris %>% class()

iris_nested <- iris %>% nest(-species)

iris_nested

iris_nested %>% glimpse()

#Ahora tenemos un tibble (que es un data.frame tambien)
#tibble tiene la ventaja de que puede almacenar listas en sus columnas
iris_nested %>% class()


#Exploraremos las columnas de nuestro dataset anidado
iris_nested$species

iris_nested$data

#para seleccionar elementos de la columna "data" la tratamos como una lista
#de ahi el nombre "List Column'
iris_nested$data[[2]]

#Desanidando nuestros datos 
iris_nested %>% unnest()


#####################################################################
#2) Usando map (purrr) para iterar sobre una lista
#####################################################################


#creamos dos listas
num_list <- list(16, 49, 144, 25)

chr_list <- list("Juan", "Maria", "Jose", "Ana")

#esto va a dar un error, porque debo correr la funcion para cada uno de los elementos
sqrt(num_list)

#una opcion es iterar uno a uno por los elementos de la lista, 
#pero se vuelve tedioso para listas largas
sqrt(num_list[[1]])
sqrt(num_list[[2]])

#vamos a utilizar la funcion map, de la libreria purrr
#el primer argumento es la lista sobre la cual queremos iterar
#el segundo es la funcion que deseamos aplicarle

map(.x = num_list, .f = sqrt)

#como con otras funciones de R, no es necesario epecificar los argumentos,
# siempre y cuando mantengamos el orden

map(chr_list, nchar)

#tambien podemos utilizar funciones anonimas con la virgulilla (~)

map(num_list, ~.x * 2 + 5)

map(chr_list, ~ paste("Hola", .x))

#adidionalmente, podemos utilizar pipes para "introducir" la lista en el map

chr_list %>% map(~ paste("Hola", .x))

#el resultado de map siempre va a ser una lista
#sin embargo, si conocemos que el tipo de resultado que esperamos recibir
#podemos especificarlo para obtener un vector
#map_dbl() y map_chr() son dos de las variantes de map

chr_list %>% map_chr(~ paste("Hola", .x))
num_list %>% map_dbl(~.x * 2 + 5)



#####################################################################
#3) Integrando map() y nest()
#####################################################################

#vamos a utilizar nuestro dataset iris_nested
iris_nested %>% glimpse()

iris_nested$data[[1]]

#la columna data es una lista y por lo tanto podemos utilizar map() en ella

#como seleccionamos una columna en el dataset original
iris$sep_length

#como la seleccionamos en el dataset anidado
map(iris_nested$data, ~ .x$sep_length)
iris_nested$data %>% map(~ .x$sep_length)

#como obtenemos el promedio de una columna en el dataset original
mean(iris$sep_length)

#como lo logramos en el dataset anidado
#como la seleccionamos en el dataset anidado
map_dbl(iris_nested$data, ~ mean(.x$sep_length))


#podemos almacenar este resultado como una nueva columna en el dataset anidado
#utilizaremos el verbo mutate de dplyr

iris_nested %>% 
  mutate(sep_len_mean = map_dbl(data, ~ mean(.x$sep_length)))

#tambien podemos crear nuevos columnas-listas 

iris_nested <- iris_nested %>% 
  mutate(sep_len_mean = map_dbl(data, ~ mean(.x$sep_length)),
         sep_len_double = map(data, ~ (.x$sep_length * 2)))

iris_nested %>% glimpse()

iris_nested$sep_len_double



#####################################################################
#4) Utilizando la libreria broom
#####################################################################

#utilizaremos una regresion lineal simple
#para modelar pet_length a partir de sep_length
iris_mod <- lm(pet_length ~ sep_length, data = iris)

#el resultado del modelo es un objeto del cual es un tanto dificil extraer informacion
iris_mod

iris_mod$coefficients

#mucha informacion
iris_mod$...

#broom nos permite extraer informacion del modelo de una manera mas sencilla con 3 funciones
#tidy(), glance(), augment()


#tidy() nos devuelve los resultados del modelo de manera ordenada en un tibble
iris_mod_tidy <- tidy(iris_mod)

iris_mod_tidy %>% filter(term == "sep_length") %>% select(estimate)


#glance() nos devuelve un resumen del modelo en un tibble de una sola fila
iris_mod_glance <- glance(iris_mod)

iris_mod_glance$r.squared

#augment() nos devuelve las predicciones del modelo para cada data point
iris_mod_pred <- augment(iris_mod)

iris_mod_pred$.fitted


#podemos estar seguros de que una regresion para todos los datos es la mejor idea?

library(ggplot2)

#como se ve una sola linea de regresion para todas las especies
iris %>% 
  ggplot(aes(x = sep_length, y = pet_length)) +
  geom_point() +
  geom_smooth(method = lm)

iris %>% 
  ggplot(aes(x = sep_length, y = pet_length, col = species)) +
  geom_point()

#como se ve una linea de regresion para cada especie
iris %>% 
  ggplot(aes(x = sep_length, y = pet_length, col = species)) +
  geom_point() +
  geom_smooth(method = lm)

#pareciera que es mejor idea ajustar un modelo para cada especie
#para lograrlo utilizaremos una combinacion de tidyr, purrr y broom


#####################################################################
#5) Utilizando todo junto, iremos agregando paso a paso
#####################################################################


iris_nested
#************************************
#a) crear los modelos
#************************************

iris_nested <- iris_nested %>%
  mutate(mod = map(data, ~lm(pet_length ~ sep_length, data = .x)))

iris_nested

iris_nested$mod


iris_nested$mod[[1]]

#************************************
#b) Extraer R cuadrado con glance()
#************************************

#como lo hariamos uno a uno
iris_nested$mod[[1]] %>% glance() %>% .$r.squared


#como hacerlo para todos con map()
iris_nested <- iris_nested %>%
  mutate(mod = map(data, ~lm(pet_length ~ sep_length, data = .x)),
         mod_glance = map(mod, glance),
         r_sq = map_dbl(mod_glance, ~.x$r.squared))


#explorando las nuevas columnas
iris_nested$mod_glance
iris_nested$r_sq


#************************************
#c) Extraer coeficientes con tidy()
#************************************

#como lo hariamos uno a uno
iris_nested$mod[[1]] %>% tidy() %>% filter(term == "sep_length") %>% .$estimate

#como hacerlo para todos con map()
iris_nested <- iris_nested %>%
  mutate(mod = map(data, ~lm(pet_length ~ sep_length, data = .x)),
         mod_glance = map(mod, glance),
         r_sq = map_dbl(mod_glance, ~.x$r.squared),
         mod_tidy = map(mod, tidy),
         coef = map_dbl(mod_tidy, ~ filter(.x, term == "sep_length") %>% .$estimate))



#explorando las nuevas columnas
iris_nested$mod_tidy
iris_nested$coef

#************************************
#b) Seleccionar las columnas de interes
#************************************

iris_nested_final <- iris_nested %>% select(-mod_glance, - mod_tidy)

iris_nested_final


#############################################
#6) Ejemplo con clustering
############################################

cust_data <- read_csv("cust_data.csv")

cust_data %>% glimpse()

cust_clust <- cust_data %>% group_by(receiver_country) %>% nest() %>% 
  mutate(clust_object = map(data, ~ .x %>%
                              dplyr::select(tnx_tot, prin_avg, mths_transacted) %>% 
                              scale() %>%
                              kmeans(centers = 3, nstart = 10)),
         clust = map(clust_object, ~.x$cluster)) %>% 
  select(-clust_object) %>% 
  unnest()

cust_clust %>% glimpse()

cust_clust %>% group_by(receiver_country, clust) %>% 
  summarise(cust_count = n(), 
            avg_trx = mean(trx_tot),
            prin_avg = mean(prin_avg),
            prin_tot = sum(prin_tot))




