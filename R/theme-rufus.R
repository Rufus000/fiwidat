theme_rufus=function(base_size = 18){
  theme_bw(base_size =base_size)+
  theme(plot.margin = unit(c(.5,.5,.5,.5 ), "cm"))+
  theme( axis.text.y =element_text (colour="black", angle=90, hjust=0.5,size=16),
         axis.text.x = element_text(colour="black",size=16),
         axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
         panel.grid.minor = element_blank())
}
