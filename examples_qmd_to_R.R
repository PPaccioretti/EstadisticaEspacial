dir.create('scripts')
knitr::purl(
  input = "Ej01.qmd",
  output = "scripts/Ejemplo_CanaAzucar.R", 
  documentation = 1L
  )

knitr::purl(
  input = "Ej02.qmd",
  output = "scripts/Ejemplo_Covid.R", 
  documentation = 1L
)


archivos <- list.files("data/", full.names = TRUE)
archivos <- archivos[!agrepl("trigo_hpv.gpkg|limites2", archivos, fixed = FALSE)]


dir.create('scripts/data')
file.copy(from = archivos,
          to = paste0('scripts/', archivos))

