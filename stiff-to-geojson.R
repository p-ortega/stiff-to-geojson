stiff.to.geojson = function(data, r, s){
  
  library(sp)
  library(geojsonio)
  
  # data = data[complete.cases(c('x', 'y', 'Na', 'Ca', 'Mg', 'Cl', 'HCO3', 'SO4')),]
  row.names(data) = data$id
  
  # Convierte iones a meq/l
  eq = data.frame(ion = c('Na', 'Ca', 'Mg', 'NH4', 'K', 'HCO3', 
                          'CO3', 'Cl', 'SO4', 'NO3', 'PO4'), 
                  eq_weight = c(23,20,12,18,39,61,30,35,48,62,97))
  
  data$Na_meq = data$Na/eq$eq_weight[eq$ion == 'Na'] 
  data$Ca_meq = data$Ca/eq$eq_weight[eq$ion == 'Ca']
  data$Mg_meq = data$Mg/eq$eq_weight[eq$ion == 'Mg']
  data$Cl_meq = data$Cl/eq$eq_weight[eq$ion == 'Cl']
  data$HCO3_meq = data$HCO3/eq$eq_weight[eq$ion == 'HCO3'] 
  data$SO4_meq = data$SO4/eq$eq_weight[eq$ion == 'SO4'] 
  
  # contador de muestras
  n = nrow(data)
  
  # Crea lista
  lista = list()
  ids = list()
  
  #Inicia tiempo 
  start.time = Sys.time()
  
  # Iteracion de stiffs
  for(i in 1:n){
    
    # Centroide Stiff
    x0 = data$x[i]
    y0 = data$y[i]
    
    # Margenes
    ybase = y0 - (1000*r)
    ytop  = y0 + (1000*r)
    xright = x0 + (1000*s)
    xleft  = x0 - (1000*s)
    
    # Excepcion para datos con NA y/o ceros en determinadas variables
    if(x0 == 0 || y0 == 0 || is.na(x0) == TRUE || is.na(y0) == TRUE || data$Na[i] == 0 || data$Ca[i] == 0||
       data$Mg[i] == 0 || data$Cl[i] == 0|| data$HCO3[i] == 0 || data$SO4[i] == 0 ||
       is.na(data$Na[i]) == TRUE ||is.na(data$Ca[i]) == TRUE || is.na(data$Mg[i]) == TRUE ||
       is.na(data$HCO3[i]) == TRUE|| is.na(data$SO4[i]) == TRUE || is.na(data$Cl[i]) == TRUE )
    {
      lista[i] = lista[i-1]
      next
    }
    
    # Coordenadas iones
    xNa = x0 + data$Na_meq[i]*s
    xCa = x0 + data$Ca_meq[i]*s
    xMg = x0 + data$Mg_meq[i]*s
    xCl = x0 - data$Cl_meq[i]*s
    xHCO3 = x0 - data$HCO3_meq[i]*s
    xSO4 = x0 - data$SO4_meq[i]*s
    
    # Coordendas Stiff en matriz
    stiff.coord = matrix(c(x0, xSO4, xHCO3, xCl, xNa, xCa, xMg, x0, x0, 
                           ybase, ybase, y0, ytop, ytop, y0, ybase, ybase, ytop), 
                         nrow = 9 , ncol = 2)
    # Poligono sp de stiff
    stiff.polygon = Polygon(stiff.coord)
    stiff.polygons = Polygons(list(stiff.polygon), ID = data$id[i])
    ids[i] = data$id[i]
    
    lista[i] = list(stiff.polygons)
    print(paste("Stiff",i, "ready"))
    
  }
  # Devuelve el tiempo total de las iteraciones
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  # Convierte lista de poligonos en Spatial polygons y en Spatial polygons con atributos 
  print("creating stiffs")
  sps = SpatialPolygons(unique(lista))
  print("adding atributte table")
  # Subset de datos, removiendo aquellos no incluidos en los poligos por tener NA
  data.cleaned = subset(data, data$id %in% ids)
  sps.d = SpatialPolygonsDataFrame(sps, data.cleaned)
  
  # Plotea stiff para chequear escalas
  Print("plotting stiffys lindis")
  plot(sps.d)
  
  # Escribe .geojson
  print("writing file")
  geojson_write(sps.d, file = "stiff")
}
