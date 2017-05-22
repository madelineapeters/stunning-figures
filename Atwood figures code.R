cbbPalette <- c("#009E73", "#F0E442", "#0072B2")
fst_plot<-ggplot(data=fit_data, aes(x=generation, y=fst))+geom_smooth(aes(colour = factor(set)), size=2, se=FALSE)+xlab("Generation")+guides(color="none")+
  ylab("Average Fit of neutral loci")+scale_colour_manual(values=cbbPalette)+ylim(-0.01,0.15)+theme(text = element_text(size=15))
print(fst_plot)

FL_r_plot<-ggplot(data=FL_r, aes(x=gen, y=r))+geom_smooth(aes(colour = factor(set)), size=2)+
  xlab("Generation")+ylab("Spatial autocorrelation coefficient r")+theme_classic()+guides(color="none")+
  scale_color_brewer(palette= "Set1")+ theme(text = element_text(size=15),plot.title = element_text(hjust = 0.5))
print(FL_r_plot)

Neut_r_plot<-ggplot(data=Neut_r, aes(x=gen, y=r))+geom_point(aes(colour = factor(set)), size=2)+ggtitle("2D spatial autocorrelation of neutral loci")+
  xlab("Generation")+ylab("Correlation coefficient r")+theme_classic()+guides(color="none")+scale_color_brewer(palette= "Set1")+ theme(text = element_text(size=15))
print(Neut_r_plot)

scatterplot3d(x=offspring_map_350$X_pos, y=offspring_map_350$Y_pos, z=offspring_map_350$FLday, color="darkgreen", xlab="X position", ylab="Y position", zlab="Flowering day", type="p")

library(rgl)
plot3d(x=offspring_map_350$X_pos, y=offspring_map_350$Y_pos, z=offspring_map_350$FLday, col="darkgreen", size=3, xlab="X position", ylab="Y position", zlab="Flowering day")
dir.create("animation")
for (i in 1:90) {
  view3d(userMatrix=rotationMatrix(2*pi * i/90, 1, -1, 1))
  rgl.snapshot(filename=paste("animation/frame-",
                              sprintf("%03d", i), ".png", sep=""))
}