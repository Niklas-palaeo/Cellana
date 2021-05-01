make_trace<- function(map,buffer) {
  
  
  #prepare image for data selection
  pal <- colorRampPalette(c('black','deepskyblue4','aquamarine'))

  plot(map[,1:2],col=pal(10)[as.numeric(cut(map$mg_ca,breaks = 10))],pch = 20,asp=1)
  
  #data selection
  line <- locator() %>% 
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
    cbind(map) %>% 
    filter(dist<buffer)
  
  
  
  # this step resets the distance to the shell edge
  min_closest <- near %>% pull(closest) %>% as.numeric %>% min()
  near <- near %>% 
    mutate(closest=as.numeric(closest),
           closest=closest-min_closest)
  
p_map <-
    ggplot() +
    geom_point(data=map,aes(x,y,col=mg_ca),inherit.aes = FALSE) +
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
    ggtitle("Location of traceline") +
    viridis::scale_color_viridis(option = "G") +
    labs(col = "Mg/Ca") +
    scale_x_continuous(breaks=c(13,14,15))+
    coord_fixed()
  
  
p_trace <-
  near %>% 
    group_by(closest) %>%
    summarise(
      sd=sd(mg_ca),
      mg_ca=mean(mg_ca),
      n=n()) %>% 
    ggplot()+
    aes(x=as.numeric(closest),y=mg_ca)+
    geom_ribbon(aes(ymax=mg_ca+sd,ymin=mg_ca-sd),fill="firebrick2",alpha=0.2)+
    geom_line(col="firebrick2")+
  xlab("Distance to edge [mm]")+
  ylab("Mg/Ca")+
  ylim(0.15,0.35)+
    xlim(0,NA)+
    ggtitle("Mg/Ca ratios along traceline",subtitle=paste("within ",buffer," microns range"))
  
  
p <-   p_map+ plot_spacer()+p_trace+plot_layout(widths=c(0.2,0.1,0.7))

rm(temp1,temp2,distances)


  return(p)
}
