## -------------------------------------------------------------------------------------------------------------------------
library(sf)
library(spdep)
library(tmap)
library(INLA)



## -------------------------------------------------------------------------------------------------------------------------
datos <- st_read("data/Base_07_07_radios.gpkg", quiet = TRUE)


## -------------------------------------------------------------------------------------------------------------------------
datos$E <- datos$Poblacion * sum(datos$Casos) / sum(datos$Poblacion)


## -------------------------------------------------------------------------------------------------------------------------
tm_shape(datos) +
  tm_polygons(col = 'E')


## -------------------------------------------------------------------------------------------------------------------------
datos$SIR <- datos$Casos / datos$E


## -------------------------------------------------------------------------------------------------------------------------
tm_shape(datos) +
  tm_polygons(col = 'SIR')


## -------------------------------------------------------------------------------------------------------------------------
datos$re_u <- 1:nrow(datos)
datos$re_v <- 1:nrow(datos)



## -------------------------------------------------------------------------------------------------------------------------
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



## -------------------------------------------------------------------------------------------------------------------------
# Generación de vecindarios para INLA
file_adj <- tempfile("map.adj1")
nb2INLA(file_adj, nb)
g <- inla.read.graph(filename = file_adj)





## -------------------------------------------------------------------------------------------------------------------------

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



## -------------------------------------------------------------------------------------------------------------------------
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



## -------------------------------------------------------------------------------------------------------------------------
tmap_mode('view')
mapas <-
  tm_basemap(c(Urbano = "OpenStreetMap", Satelite = "Esri.WorldImagery")) +
  tm_shape(datos,
           name = 'Casos')  +
  tm_polygons(
    col = "Casos",
    id = "Casos",
    border.col = "gray50",
    border.alpha = .5,
    style = "fixed",
    title = "Casos por Radio",
    palette = "YlOrBr",
    breaks = c(0, 1, 2, 3, 5, 10, 15, 25, 36),
    popup.vars = popup_vars,
    legend.format = list(
      scientific = TRUE,
      format = "f",
      digits = 0
    )
  ) +
  
  tm_shape(datos,
           name = 'Tasa de Infección - SIR')  +
  tm_polygons(
    col = "SIR",
    id = "SIR",
    border.col = "gray50",
    border.alpha = .5,
    style = "fixed",
    palette = hcl.colors(7, "ag_GrnYl"),
    breaks = c(0, 5, 10, 15, 20, 25, 30, 50, 60, 90),
    popup.vars = popup_vars,
    legend.format = list(
      scientific = TRUE,
      format = "f",
      digits = 0
    )
  ) +
  
  tm_shape(datos,
           name = 'Riesgo Relativo')  +
  tm_polygons(
    col = "RR",
    id = "RR",
    style = "fixed",
    palette = "viridis",
    alpha = 0.8,
    title = "Riesgo Relativo",
    breaks = c(0.4, 1, 2, 4, 8, 10, 15, 20, 30, 50, 85, 96),
    popup.vars = popup_vars,
    border.col = "gray50",
    border.alpha = .5,
    legend.format = list(
      scientific = TRUE,
      format = "f",
      digits = 1
    )
  )
mapas



## -------------------------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------------------------

tmap_mode('plot')

boundary_box <- matrix(c(4357000, 6508000, 4405900, 6560000))

mapacasos <-
  tm_shape(datos, bbox = boundary_box)  +
  tm_polygons(
    col = "Casos",
    id = "Casos",
    border.col = "gray50",
    border.alpha = .5,
    style = "fixed",
    title = "Casos por Radio",
    palette = "YlOrBr",
    breaks = c(0, 1, 2, 3, 5, 10, 15, 25, 36),
    legend.format = list(
      scientific = TRUE,
      format = "f",
      digits = 0
    )
  ) + 
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("left", "bottom"))

mapacasos


mapaSIR <-
  tm_shape(datos, bbox = boundary_box)  +
  tm_polygons(
    col = "SIR",
    id = "SIR",
    border.col = "gray50",
    border.alpha = .5,
    style = "fixed",
    palette = "YlOrBr",
    breaks = c(0, 5, 10, 15, 20, 25, 30, 50, 60, 90),
    legend.format = list(
      scientific = TRUE,
      format = "f",
      digits = 0
    )
  ) + 
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("left", "bottom"))

mapaSIR


mapaRR <-
  tm_shape(datos, bbox = boundary_box)  +
  tm_polygons(
    col = "RR",
    id = "RR",
    style = "fixed",
    palette = "YlOrBr",
    title = "Riesgo Relativo",
    breaks = c(0.4, 1, 2, 4, 8, 10, 15, 20, 30, 50, 85, 96),
    border.col = "gray50",
    border.alpha = .5,
    legend.format = list(
      scientific = TRUE,
      format = "f",
      digits = 1
    )
  ) + 
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("left", "bottom"))

mapaRR

