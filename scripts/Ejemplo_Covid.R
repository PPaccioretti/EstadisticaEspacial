## -------------------------------------------------------------------------------------------------------------------------------------------------
#| code-fold: true
#| code-summary: "Carga paquetes"

library(sf)
library(spdep)
library(tmap)
library(INLA)



## -------------------------------------------------------------------------------------------------------------------------------------------------
datos <- st_read("data/Base_07_07_radios.gpkg", quiet = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------------------------
datos$E <- datos$Poblacion * sum(datos$Casos) / sum(datos$Poblacion)


## -------------------------------------------------------------------------------------------------------------------------------------------------
tm_shape(datos) +
  tm_polygons(fill = 'E')


## -------------------------------------------------------------------------------------------------------------------------------------------------
datos$SIR <- datos$Casos / datos$E


## -------------------------------------------------------------------------------------------------------------------------------------------------
tm_shape(datos) +
  tm_polygons(fill = 'SIR')


## -------------------------------------------------------------------------------------------------------------------------------------------------
datos$re_u <- 1:nrow(datos)
datos$re_v <- 1:nrow(datos)



## -------------------------------------------------------------------------------------------------------------------------------------------------
# Generación de lista con vecindarios
nb <- poly2nb(datos)

head(nb)

plot(st_geometry(datos), border = "grey", lwd = 0.5)
plot(
  nb,
  coordinates(as(datos, "Spatial")),
  add = TRUE,
  col = "blue",
  points = FALSE,
  lwd = 0.5
)



## -------------------------------------------------------------------------------------------------------------------------------------------------
# Generación de vecindarios para INLA
file_adj <- tempfile("map.adj1")
nb2INLA(file_adj, nb)
g <- inla.read.graph(filename = file_adj)





## -------------------------------------------------------------------------------------------------------------------------------------------------

# Ajuste del Modelo inflado en ceros
summary(datos)


formula  <-
  Casos ~ 
  Fragmentacion +  
  HogaresNBI + 
  HogaresHacinamiento +
  HogaresJefe.Univ. + 
  Bancos + 
  f(re_u, model = "besag", graph = g) + 
  f(re_v, model = "iid")


res_inflpoi <-
  inla(
    formula,
    family = "zeroinflatedpoisson1",
    data = datos,
    E = E,
    control.predictor = list(compute = TRUE)
  )
summary(res_inflpoi)



## -------------------------------------------------------------------------------------------------------------------------------------------------
datos$RR <- res_inflpoi$summary.fitted.values[, "mean"]
datos$RR_LI <- res_inflpoi$summary.fitted.values[, "0.025quant"]
datos$RR_LS <- res_inflpoi$summary.fitted.values[, "0.975quant"]

popup_vars <- c(
  "Fragmentación (Nivel)" = 'Fragmentacion',
  "Valor Tierra Medio ($/m2)" = 'ValorTierra',
  "Hogares NBI (%)" = 'HogaresNBI',
  "Hogares Hacinamiento (%)" = 'HogaresHacinamiento',
  "Hogares Jefe Univ.(%)" = 'HogaresJefe.Univ.',
  "Bancos",
  "Casos",
  "Población" = 'Poblacion',
  "E",
  "SIR",
  "RR_LI",
  "RR",
  "RR_LS"
)



## -------------------------------------------------------------------------------------------------------------------------------------------------
#| column: screen-inset-right
#| fig-height: 6

tmap_mode('view')
mapas <-
  tm_basemap(c(Urbano = "OpenStreetMap", Satelite = "Esri.WorldImagery")) +
  tm_shape(datos,
           name = 'Casos')  +
  tm_polygons(
    fill = 'Casos',
    fill.scale = tm_scale_intervals(
      style = "fixed",
      breaks = c(0, 1, 2, 3, 5, 10, 15, 25, 36),
      values = "brewer.yl_or_br"),
    fill.legend = tm_legend(title =  "Casos por Radio",
                            scientific = TRUE,
                            format = "f",
                            digits = 0
    ),
    col = "gray50",
    col_alpha = .5,
    popup.vars = popup_vars
  ) +
  
  tm_shape(datos,
           name = 'Tasa de Infección - SIR')  +
  tm_polygons(
    fill = "SIR",
    fill.scale = tm_scale_intervals(
      style = "fixed",
      breaks = c(0, 5, 10, 15, 20, 25, 30, 50, 60, 90),
      values = "brewer.yl_or_br"),
    fill.legend = tm_legend( 
      title = 'SIR',
      scientific = TRUE,
      format = "f",
      digits = 0
    ),
    col = "gray50",
    col_alpha = .5,
    popup.vars = popup_vars
  ) +
  
  tm_shape(datos,
           name = 'Riesgo Relativo')  +
  tm_polygons(
    fill = "RR",
    fill.scale = tm_scale_intervals(
      style = "fixed",
      breaks = c(0.4, 1, 2, 4, 8, 10, 15, 20, 30, 50, 85, 96),
      values = "brewer.yl_or_br"),
    fill.legend = tm_legend( 
       title = "Riesgo Relativo",
      scientific = TRUE,
      format = "f",
      digits = 1
    ),
    col = "gray50",
    col_alpha = .5,
    popup.vars = popup_vars
  )
mapas



## -------------------------------------------------------------------------------------------------------------------------------------------------
img <-
  "https://ig.conae.unc.edu.ar/wp-content/uploads/sites/68/2022/04/cropped-cromas-68-1.png"

map <-
  tmap_leaflet(mapas) %>%
  leafem::addLogo(img,
                  url = "https://ig.conae.unc.edu.ar/",
                  width = 136,
                  height = 50) %>%
  leaflet::addMiniMap(position = "bottomleft",
                      width = 150,
                      height = 150)
map

# mapshot(map, paste0("Mapa_radios_", format(Sys.time(), "%d_%m_%Y"), ".html"))

