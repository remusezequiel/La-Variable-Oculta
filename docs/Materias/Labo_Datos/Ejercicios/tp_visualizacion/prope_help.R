
datos <- read.csv("ar_properties.csv")

# filtros que queremos
pais      <- is.element( datos$l1,            "Argentina")
region    <- is.element( datos$l2,            "Capital Federal")
moneda    <- is.element( datos$currency,      "USD")
operacion <- is.element( datos$operation_type,"Venta")

# los demas sin los na
propiedad    <- !is.na( datos$property_type )
lat          <- !is.na( datos$lat ) 
lon          <- !is.na( datos$lon ) 
price        <- !is.na( datos$price )
sup_total    <- !is.na( datos$surface_total )
sup_cubierta <- !is.na( datos$surface_covered ) 
barrio       <- !is.na( datos$l3 ) 

# condicion multiple
# vector logico con TRUEs en los registros seleccionados
new_vec <- (pais  & region & moneda       & propiedad & operacion & lat 
            & lon & price  & sup_cubierta & sup_total & barrio)  

# nueva base
new_df <- datos[ new_vec, c(3,4,10,17,19,23) ]

# Agrego al data.frame precio por metro cuadrado
new_df$preciom2 <- new_df$price / new_df$surface_total

# Creo la variable precio por metro cuadrado
preciom2 <- new_df$preciom2

# Filtros para acercamientos
filtroPt1 <- new_df$surface_total<10000
filtroPt2 <- new_df$surface_total<5000 & new_df$preciom2<5000
filtroPt3 <- new_df$surface_total<1000 & new_df$preciom2<5000
filtroPt4 <- new_df$surface_total<700  & new_df$preciom2<2000

# Creemos superficies firtradas dentro



# Tomo los valores del promedio y la desviación standar
promedio <- mean(as.integer(new_df$preciom2))
sd <- sd(as.integer(preciom2))

pm2 <- new_df$preciom2
prom_men_sd <- promedio - sd
prom_mas_sd <- promedio + sd

sd_menos = table(new_df[pm2 <= prom_men_sd,]$property_type)
p_prom   = table(new_df[(pm2 > prom_men_sd) & (pm2 < prom_mas_sd),]$property_type)
sd_mas   = table(new_df[new_df$preciom2 <= (promedio + sd),]$property_type)
