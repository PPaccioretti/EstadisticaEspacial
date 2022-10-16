Cambio_unidades <-  function(img){
  # bands <- img$selec('total_precipitation')$multiply(1000)
  
  Precip = img$expression(
    'P * 1000', list(
      P= img$select('total_precipitation')
    )
  )$rename(ee$String$cat(paste0('Precipitacion', "_"), img$date()$format('MMM')))
  
  
  Temp = img$expression(
    'T - 273.15', list(
      T= img$select('temperature_2m')
    )
  )$rename(ee$String$cat(paste0('Temperatura', "_"), img$date()$format('MMM')))
  
  
  Punto_rocio = img$expression(
    'Td - 273.15', list(
      Td= img$select('dewpoint_temperature_2m')
    )
  )$rename(ee$String$cat(paste0('Punto rocio', "_"), img$date()$format('MMM')))
  
  
  
  Viento = img$expression(
    'sqrt((u*3.6)**2 + (v*3.6)**2)', list(
      u = img$select('u_component_of_wind_10m'),
      v = img$select('v_component_of_wind_10m')
    )
  )$rename(ee$String$cat(paste0('Viento', "_"), img$date()$format('MMM')))
  
  direccion_viento <- img$expression(
    'mod(180 + (180/pi) * atan2(v,u), 360)', list(
      u = img$select('u_component_of_wind_10m'),
      v = img$select('v_component_of_wind_10m'),
      pi = pi
    )
  )$rename(ee$String$cat(paste0('DirViento', "_"), img$date()$format('MMM')))
  
  
  Hr = img$expression(
    '100 * (exp(17.625*dp/(243.04 + dp ))/exp(17.625 *t/(243.04 + t )))', list(
      dp = Punto_rocio,
      t = Temp
    )
  )$rename(ee$String$cat(paste0('Humedad relativa', "_"), img$date()$format('MMM')))
  
  
  Tot_evaporation = img$expression(
    'te * 1000', list(
      te = img$select('total_evaporation')
    )
  )$rename(ee$String$cat(paste0('total_evaporation', "_"), img$date()$format('MMM')))
  
  
  
  
  img$
    addBands(Precip)$
    addBands(Temp)$
    addBands(Punto_rocio)$
    addBands(Viento)$
    addBands(Hr)$
    addBands(Tot_evaporation)$
    addBands(direccion_viento)$
    select(ee$String$cat(paste0('Precipitacion', "_"), img$date()$format('MMM')),
           ee$String$cat(paste0('Temperatura', "_"), img$date()$format('MMM')),
           ee$String$cat(paste0('Punto rocio', "_"), img$date()$format('MMM')),
           ee$String$cat(paste0('Viento', "_"), img$date()$format('MMM')),
           ee$String$cat(paste0('Humedad relativa', "_"), img$date()$format('MMM')),
           ee$String$cat(paste0('total_evaporation', "_"), img$date()$format('MMM')),
           ee$String$cat(paste0('DirViento', "_"), img$date()$format('MMM')),
           'u_component_of_wind_10m',
           'v_component_of_wind_10m')$
    copyProperties(img,list('system:time_start',
                           'system:time_end'))
  
}



# renombrar <- function(img) {
#   mifecha <- img$date()$format('MMM')
#   name <- ee$String$cat(paste0(band, "_"), mifecha)
#   img$select(band)$reproject("EPSG:4326")$rename(name)
# }


# siembra <- as.Date("2020-12-27")
# v6 <- as.Date("2021-01-20") 
# v10 <- as.Date("2021-01-31")
# v14 <- as.Date("2021-02-13")
# r1 <- as.Date("2021-02-28")
# 
# makeInterval <- function(x, semi) {
#   c(x - semi, x + semi) 
# }
# myDates <- makeInterval(v14, 20)
# start <- rgee::rdate_to_eedate(myDates[1])
# end <- rgee::rdate_to_eedate(myDates[2])


# var uv0 = ERA5.select(['u_component_of_wind_10m', 'v_component_of_wind_10m']);
# var uv10 = uv0.clip(bbox);
# 
# var scale = Map.getScale() * 10; //25000
# var numPixels = 1e10;
# 
# var samples = uv10.rename(['u10', 'v10']).sample({
#   region: bbox, 
#   scale: scale, 
#   numPixels: numPixels, 
#   geometries: true
# });
# 
# var scaleVector = 0.1;
# 
# var vectors = samples.map(function(f) {
#    u = ee$Number(img$get('u_component_of_wind_10m'))$multiply(0.1);
#    v = ee$Number(img$get('v_component_of_wind_10m'))$multiply(0.1);
# 
#   origin = img$geometry();
# 
#   #// translate
#   proj = origin$projection()$translate(u, v);
#   end = ee$Geometry$Point(origin$transform(proj)$coordinates());
# 
#   #// construct line
#   geom = ee$Algorithms$GeometryConstructors$LineString(c(origin, end), NULL,T);
# 
#   img$setGeometry(geom);
# });
# 
# var uv10final = uv10.pow(2).reduce(ee.Reducer.sum()).sqrt();