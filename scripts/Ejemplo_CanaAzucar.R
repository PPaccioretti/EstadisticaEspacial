## -------------------------------------------------------------------------------------------------------------------------------------------------
#| code-fold: true
#| code-summary: "Carga paquetes y funciones"
library(nlme)
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)


resumir_modelo <- function(modelo) {
  
  rmse <- function(modelo) {
    sqrt(mean(modelo$residuals^2))
  }
  
  aic <- AIC(modelo)
  bic <- BIC(modelo)
  my_rmse <- rmse(modelo)
  regresora <- paste(attr(modelo$terms,"term.labels"), collapse = ", ")
  
  conCor <- ifelse(length(modelo$modelStruct), 'Si', 'No')
  data.frame('Indice' = regresora,
             'ConCorr' = conCor,
             'AIC' = aic,
             'BIC' = bic,
             'RMSE' = my_rmse, 
             check.names = FALSE)
}

diferenciaNormalizada <- function(x, y) {
  (x - y) / (x + y)
}

# tmap::tmap_options(basemap.server = c(
#   'Satelital' = leaflet::providers$Esri.WorldImagery,
#   'OSM' = leaflet::providers$OpenStreetMap))


## -------------------------------------------------------------------------------------------------------------------------------------------------
datos <- read.table("data/No_quemadas.txt")



## -------------------------------------------------------------------------------------------------------------------------------------------------
datos <- datos |> 
  mutate(
    ndvi = diferenciaNormalizada(Banda4, Banda3),
    gndvi = diferenciaNormalizada(Banda4, Banda2),
    nci = diferenciaNormalizada(Banda5, Banda2)
  )



## -------------------------------------------------------------------------------------------------------------------------------------------------
datos_sf <-
  sf::st_as_sf(datos,
               coords = c('X_coord', 'Y_coord'),
               crs = 32720)

tmap_mode('view')



## -------------------------------------------------------------------------------------------------------------------------------------------------
tm_shape(datos_sf) +
  tm_dots(fill = 'TCH',
          fill.scale = tm_scale_continuous(values = "carto.ag_grn_yl"),
          size = 0.5)


## -------------------------------------------------------------------------------------------------------------------------------------------------
tm_shape(datos_sf) +
  tm_dots(fill = 'ndvi',
          fill.scale = tm_scale_continuous(values = "tableau.classic_green"),
          size = 0.5)


## -------------------------------------------------------------------------------------------------------------------------------------------------
#| fig-subcap: ["Histograma", "Gráfico de cajas"]
#| layout-ncol: 2

ggplot(datos, aes(TCH)) +
  geom_histogram(aes(y = after_stat(count / sum(count))),
                 bins = 15) +
  labs(y = 'Frecuencia Relativa')

ggplot(datos, aes(EDAD, TCH)) +
  geom_boxplot(width = 0.25)


## -------------------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-ndvi
#| fig-cap: "TCH en función de NDVI"
#| column: margin
#| echo: true
ggplot(datos, aes(ndvi, TCH)) +
  geom_point()



## -------------------------------------------------------------------------------------------------------------------------------------------------

modelo_ndvi <- gls(TCH ~ ndvi, 
                   data = datos, 
                   method = 'REML')
summary(modelo_ndvi)





## -------------------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-gndvi
#| fig-cap: "TCH en función de gNDVI"
#| column: margin
#| echo: true
ggplot(datos, aes(gndvi, TCH)) +
  geom_point()



## -------------------------------------------------------------------------------------------------------------------------------------------------
modelo_gndvi <- gls(TCH ~ gndvi, 
                    data = datos, 
                    method = 'REML')
summary(modelo_gndvi)


## -------------------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-nci
#| fig-cap: "TCH en función de NCI"
#| column: margin
#| echo: true
ggplot(datos, aes(nci, TCH)) +
  geom_point()


## -------------------------------------------------------------------------------------------------------------------------------------------------
modelo_nci <-  gls(TCH ~ nci, 
                   data = datos, 
                   method = 'REML')
summary(modelo_nci)


## -------------------------------------------------------------------------------------------------------------------------------------------------
rbind(
  resumir_modelo(modelo_ndvi),
  resumir_modelo(modelo_gndvi),
  resumir_modelo(modelo_nci)
  )



## -------------------------------------------------------------------------------------------------------------------------------------------------
#| label: modelo-corr-ndvi

modelo_ndvi_conCorr <- gls(
  TCH ~ ndvi,
  correlation = corExp(
    form =  ~ as.numeric(as.character(X_coord)) + 
      as.numeric(as.character(Y_coord)),
    metric = "euclidean",
    nugget = FALSE
  ),
  data = datos,
  method = 'REML'
)
summary(modelo_ndvi_conCorr)



## -------------------------------------------------------------------------------------------------------------------------------------------------
#| label: modelo-corr-gndvi
modelo_gndvi_conCorr <- gls(
  TCH ~ gndvi,
  correlation = corExp(
    form =  ~ as.numeric(as.character(X_coord)) + 
      as.numeric(as.character(Y_coord)),
    metric = "euclidean",
    nugget = FALSE
  ),
  data = datos,
  method = 'REML'
)
summary(modelo_gndvi_conCorr)


## -------------------------------------------------------------------------------------------------------------------------------------------------
#| label: modelo-corr-nci
modelo_nci_conCorr <- gls(
  TCH ~ nci,
  correlation = corExp(
    form =  ~ as.numeric(as.character(X_coord)) +
      as.numeric(as.character(Y_coord)),
    metric = "euclidean",
    nugget = FALSE
  ),
  data = datos,
  method = 'REML'
)
summary(modelo_nci_conCorr)



## -------------------------------------------------------------------------------------------------------------------------------------------------

rbind(
  resumir_modelo(modelo_ndvi),
  resumir_modelo(modelo_ndvi_conCorr),
  resumir_modelo(modelo_gndvi),
  resumir_modelo(modelo_gndvi_conCorr),
  resumir_modelo(modelo_nci),
  resumir_modelo(modelo_nci_conCorr)
  )




## -------------------------------------------------------------------------------------------------------------------------------------------------
ef_fijos_ndvo_iid <- summary(modelo_ndvi)
ef_fijos_ndvo_iid$tTable


## -------------------------------------------------------------------------------------------------------------------------------------------------
ef_fijos_ndvo_corr <- summary(modelo_ndvi_conCorr)
ef_fijos_ndvo_corr$tTable


## ----obs-pred-lm----------------------------------------------------------------------------------------------------------------------------------
#| column: page
#| layout-ncol: 3
#| layout-nrow: 1

predichos_lm <- datos
predichos_lm$pred_ndvi_iid <- predict(modelo_ndvi)
predichos_lm$pred_ndvi_esp <- predict(modelo_ndvi_conCorr)


ggplot(predichos_lm, aes(pred_ndvi_iid, TCH)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot(predichos_lm, aes(pred_ndvi_esp, TCH)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot(predichos_lm, aes(pred_ndvi_esp, pred_ndvi_iid)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)



## -------------------------------------------------------------------------------------------------------------------------------------------------
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install(c("graph", "Rgraphviz"), dep = TRUE)
# 
# install.packages(
#   "INLA",
#   repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/testing"),
#   dep = TRUE
# )
# https://www.r-inla.org/download-install
library(INLA)
library(inlabru)



## -------------------------------------------------------------------------------------------------------------------------------------------------
inla.setOption(inla.mode = 'experimental')



## ----generacion-grilla-inla-----------------------------------------------------------------------------------------------------------------------

loc <- st_coordinates(datos_sf)

mesh <- INLA::inla.mesh.2d(
  loc = loc,
  offset = c(1000, 4000),
  cutoff = 800,
  max.edge = c(1000, 2000),
  max.n = 10000)


ggplot(datos_sf) +
  gg(mesh) +
  geom_sf()


spde <- INLA::inla.spde2.pcmatern(
  mesh = mesh,
  prior.range = c(2000, 0.05),
  prior.sigma = c(200, 0.01)
)



## ----eval=FALSE-----------------------------------------------------------------------------------------------------------------------------------
#| code-fold: true
#| code-summary: "Ajuste MJB usando INLA sin funciones de inlabru"
#|
## proj_obs <- inla.mesh.projector(mesh, loc = loc)
## proj_pred <- inla.mesh.projector(mesh, loc = mesh$loc)
## 
## A_obs <- inla.spde.make.A(mesh, loc = loc)
## A_pred <- inla.spde.make.A(mesh, loc = proj_pred$loc)
## idx <- 1:spde$n.spde
## 
## stack_obs <-
##   inla.stack(
##     data = list(y = datos_sf$TCH),
##     A = list(A_obs, 1),
##     effects = list(c(
##       list(Intercept = 1),
##       inla.spde.make.index("spatial", spde$n.spde)
##     ),
##     covar = datos_sf$ndvi),
##     tag = "obs"
##   )
## stack_pred <-
##   inla.stack(
##     data = list(y = NA),
##     A = list(A_pred),
##     effects = list(c(
##       list(Intercept = 1),
##       inla.spde.make.index("spatial", mesh$n)
##     )),
##     tag = "pred"
##   )
## stack <- inla.stack(stack_obs, stack_pred)
## 
## 
## formula <- y ~ -1 + Intercept + covar +
##     f(spatial, model = spde)
## 
## result1 <- inla(
##   formula,
##   data = inla.stack.data(stack_obs, spde = spde),
##   family = "gaussian",
##   control.predictor = list(A = inla.stack.A(stack_obs),
##                            compute = TRUE)
## )
## summary(result1)
## 
## 
## plot(datos_sf$TCH,
##      result1$summary.fitted.values[inla.stack.index(stack_obs, "obs")$data, "mean"],
##      main = "Observations vs posterior predicted values at the data locations")
## 
## #
## # field_pred <- inla.mesh.project(proj_pred,
## #                                 result1$summary.fitted.values[inla.stack.index(stack, "pred")$data, "mean"])
## # field_pred_sd <- inla.mesh.project(proj_pred,
## #                                    result1$summary.fitted.values[inla.stack.index(stack, "pred")$data, "sd"])
## #
## # image(inla.mesh.project(mesh,
## #                         field = field_pred,
## #                         dims = c(200, 200)),
## #       main = "Posterior field mean")
## # image(inla.mesh.project(mesh,
## #                         field = field_pred_sd,
## #                         dims = c(200, 200)),
## #       main = "Prediction standard deviation")


## ----ajuste-inlabru-------------------------------------------------------------------------------------------------------------------------------
#| column: page-right
#| layout-align: center
#|
ndvi_bru_spde <-
  bru(
    TCH ~ Intercept(1) + ndvi + site(main = coordinates, model = spde),
    family = "gaussian",
    data = as_Spatial(datos_sf)
  )

summary(ndvi_bru_spde)


## -------------------------------------------------------------------------------------------------------------------------------------------------
#| column: screen-inset-shaded
#| layout-nrow: 1
plot(ndvi_bru_spde, "Intercept")
plot(ndvi_bru_spde, "ndvi")


## -------------------------------------------------------------------------------------------------------------------------------------------------
#| fig-height: 12


spde.posterior(ndvi_bru_spde, "site", what = "matern.covariance") -> covplot
spde.posterior(ndvi_bru_spde, "site", what = "matern.correlation") -> corplot
spde.posterior(ndvi_bru_spde, "site", what = "range") -> rngplot
spde.posterior(ndvi_bru_spde, "site", what = "log.range") -> lgrngplot
spde.posterior(ndvi_bru_spde, "site", what = "variance") -> varplot
spde.posterior(ndvi_bru_spde, "site", what = "log.variance") -> lgvarplot

multiplot(plot(covplot), plot(corplot),
          plot(rngplot), plot(lgrngplot),
          plot(varplot), plot(lgvarplot))


## -------------------------------------------------------------------------------------------------------------------------------------------------
pred_mesh <-
  predict(ndvi_bru_spde, as_Spatial(datos_sf), ~ Intercept + ndvi + site)
predichos <- st_as_sf(pred_mesh)

ggplot(predichos, aes(mean, TCH)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)


## -------------------------------------------------------------------------------------------------------------------------------------------------
#| code-fold: true
#| code-summary: "Predicción espacial de TCH"

# No aplica para esta aplicación.
# pred_mesh <-
#   predict(ndvi_bru_spde, pixels(mesh), ~ Intercept + ndvi + site)
# 
# ggplot() +
#   gg(pred_mesh)
# ggplot(datos_sf) +
#   gg(pred_mesh) +
#   gg(mesh) +
#   geom_sf()




## -------------------------------------------------------------------------------------------------------------------------------------------------
predichos_lm$pred_ndvi_inla <- predichos$mean

ggplot(predichos_lm, aes(pred_ndvi_iid, TCH)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot(predichos_lm, aes(pred_ndvi_esp, TCH)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot() +
  geom_point(data = predichos_lm, 
             aes(pred_ndvi_iid, TCH, color = 'REML iid')) +
  geom_point(data = predichos_lm, 
             aes(pred_ndvi_esp, TCH, color = 'REML Corr')) +
  geom_point(data = predichos_lm, 
             aes(pred_ndvi_inla, TCH, color = 'INLA Corr')) +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = 'TCH Predichos', y = 'TCH Observado', color = 'Estimación')


