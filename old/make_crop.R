make_crop<- function(map,limit) {
  
  if(missing(limit)) {
    limit <- 1
  } else {
    limit <- limit
  }
  
  
  map <- map %>%
    filter(mg_ca<limit)  

  require(tidyverse)
  require(Cairo)
  require(rgl)
  
  options(bitmapType="cairo")
  
  #prepare image for data selection
  pal <- colorRampPalette(c('black',
                            'deepskyblue4',
                            'aquamarine'))
  
  x11(type = "cairo",
      antialias = "default", 
      # width =5 ,     
      height = 5)# Apply x11 function
  
  
  
  
  
  print(
    map %>%
      ggplot()+
      aes(x=x,
          y=y,
          col=mg_ca)+
      geom_point(size=0.5)+
      # scale_colour_gradientn(colors = pal(100))+
      viridis::scale_color_viridis(option = "G") +
      coord_fixed()+
      scale_x_continuous(breaks=seq(round(map$x %>% min(),0),round(map$x %>% max,0), by = 1))+
      scale_y_continuous(breaks=seq(round(map$y %>% min(),0),round(map$y %>% max,0), by = 1))
  )
  
  
  frame <- gglocator(n = 2, mercator = FALSE)
  
  new_map <- map %>% 
    filter(x>min(frame$x),
           x<max(frame$x),
           y>min(frame$y),
           y<max(frame$y)
    )
  
  rm(frame,pal)
  
  print(
    new_map %>%
      ggplot()+
      aes(x=x,
          y=y,
          col=mg_ca)+
      geom_point(size=1)+
      # scale_colour_gradientn(colors = pal(100))+
      viridis::scale_color_viridis(option = "G") +
      coord_fixed()+
      scale_x_continuous(breaks=seq(round(map$x %>% min(),0),round(map$x %>% max,0), by = 1))+
      scale_y_continuous(breaks=seq(round(map$y %>% min(),0),round(map$y %>% max,0), by = 1))
  )
  
  
  
  
  
  return(new_map)
}
# FIND THE LARGEST nearest DISTANCE, divide by total range to define point size