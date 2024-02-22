make_trace<- function(map,buffer,point_num,limit, resolution) {

  if(missing(limit)) {
    limit <- 1
  } else {
    limit <- limit
  }
  
  
  if(missing(resolution)) {
    resolution <- 100
  } else {
    resolution <- resolution
  }

  point_size <- resolution/100
  
# Change x,y data to mm if not done so already
  if(max(map$x)>1000){
    
   map <- map %>% 
     mutate(x=x/1000,
            y=y/1000) #changing to millimetres
    
  }
  ## Open these for testing
  # buffer <- 0.1
  # point_num <- 2
  # map <- C7_M

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
  
  
  map <- map %>%
    filter(mg_ca<limit)
    
  
 

  
print(
  map %>%
    ggplot()+
    aes(x=x,
        y=y,
        col=mg_ca)+
    geom_point(size=point_size)+
    # scale_colour_gradientn(colors = pal(100))+
    viridis::scale_color_viridis(option = "G") +
    coord_fixed()+
    scale_x_continuous(breaks=seq(round(map$x %>% min(),0),round(map$x %>% max,0), by = 1))+
    scale_y_continuous(breaks=seq(round(map$y %>% min(),0),round(map$y %>% max,0), by = 1))
)


  line <- gglocator(n = point_num, mercator = FALSE)  %>%
    SpatialPoints() %>%
    as("SpatialLines")
  



# Calculate interpolated points and their distances along the line
  L <- SDraw::lineLength(line)
  
  InPo_ID <- seq(0,L,by=0.05)# in mm

  InPo <-
    gInterpolate(line, InPo_ID)

# change points to sf objects to work with st_distance()
  temp1 <- map %>% select(x,y) %>%
    SpatialPoints() %>%
    st_as_sf()

  temp2 <- InPo %>%
    st_as_sf()

  #calculate distances and give the raw data names according to the original points and the interpolated points
  distances <- st_distance(x=temp1,y=temp2) %>%
    data.frame()

    names(distances) <- InPo_ID
  
    rownames(distances) <- rownames(map)

# bring together the distances with the original points
  near <- distances %>%
    mutate(ID=as.numeric(rownames(.))) %>%
    select(ID,everything()) %>%
    gather('closest','dist',-ID) %>%
    group_by(ID) %>%
    arrange(dist) %>%
    slice(1) %>%
    arrange(ID) %>%
    cbind(map %>% select(x,y,mg_ca)) %>%
    filter(dist<buffer)



  # this step resets the distance to the shell edge
  min_closest <- near %>%
    pull(closest) %>%
    as.numeric %>%
    min()

  near <- near %>%
    mutate(closest=as.numeric(closest),
           closest=closest-min_closest)

  #Markers to show distance on line
Markers <-  InPo %>%
  as_tibble() %>%
  mutate(seq = seq(0,L,by=0.05)) %>% 
  filter(seq>=min_closest) %>% 
  mutate(seq = floor(seq(0,L-min_closest,by=0.05))) %>%
  group_by(seq) %>%
  summarise(x=first(x),
            y=first(y))
  
  
p_map <-
    ggplot() +
    geom_point(data=map,aes(x,y,col=mg_ca),inherit.aes = FALSE, size=point_size) +
  geom_point(
    data = near,
    mapping = aes(x, y),
    col = 'firebrick2',
    alpha = 0.07,
    inherit.aes = FALSE
  ) +
  geom_path(
      data = InPo %>% as_tibble(),
      aes(x, y),
      col = "firebrick2",
      inherit.aes = FALSE
    ) +
  geom_point(
    data = Markers,
    aes(x, y),
    col = "grey90",
    inherit.aes = FALSE,
    size=point_size*4,
    shape=21,
    fill = NA
  ) +
    # geom_text(
    #   data = Markers,
    #   aes(x, y, label = seq),
    #   col = "grey90",
    #   inherit.aes = FALSE
    #   # hjust=-0
    # ) +
    ggtitle("Location of traceline") +
    viridis::scale_color_viridis(option = "G") +
    labs(col = "Mg/Ca") +
    # scale_x_continuous(n.breaks = )+# doesn't work for each shell
    coord_fixed()+
  scale_x_continuous(breaks=seq(round(map$x %>% min(),0),round(map$x %>% max,0), by = 1))+
  scale_y_continuous(breaks=seq(round(map$y %>% min(),0),round(map$y %>% max,0), by = 1))


p_trace <-
  near %>%
    group_by(closest) %>%
    summarise(
      sd=sd(mg_ca),
      mg_ca=mean(mg_ca),
      n=n()) %>% 
    ggplot()+
    aes(x=as.numeric(closest),y=mg_ca)+
    geom_ribbon(aes(ymax=mg_ca+sd,ymin=mg_ca-sd),fill="firebrick2", alpha=0.2)+
    geom_line(col="firebrick2")+
  xlab("Distance to edge [mm]")+
  ylab("Mg/Ca")+
  # ylim(min(map$mg_ca),max(map$mg_ca))+
    xlim(0,NA)+
    ggtitle("Mg/Ca ratios along traceline",subtitle=paste("within ",buffer," millimetres range"))


p <-   p_map+plot_spacer()+p_trace+plot_layout(widths=c(NA,0.1,NA))


rm(temp1,temp2,distances, markers)
# rgl.close()

  return(p)
}
