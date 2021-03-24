# Validation of results with SPAM fulldata
library(magclass)
library(mrcommons)
library(luplot)
library(luscale)
library(ggplot2)
library(magpie4)
library(RColorBrewer)
install.packages(c("ggforce","ggrepel"))
library(ggforce)



outputdir<- c("/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_dynamic_nocc__2021-03-18_22.33.33/",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_dynamic_cc__2021-03-18_22.27.55/",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_free_nocc__2021-03-18_22.35.28/",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_free_cc__2021-03-18_22.29.49/",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_regional_nocc__2021-03-18_22.31.43/",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_regional_cc__2021-03-18_22.26.02/")
sce <- c("Dynamic_nocc","Dynamic_cc","CapitalShare_zero_nocc","CapitalShare_zero_cc","Regional_Static_nocc","Regional_Static_cc")

# Prepares spam data
historical_2000<-get(load("/p/projects/landuse/users/mbacca/DataSets/Historical/Total_Areas/SPAMMagObj_y_Phys2000_.Rda"))
historical_2005<-get(load("/p/projects/landuse/users/mbacca/DataSets/Historical/Total_Areas/SPAMMagObj_y_Phys2005_.Rda"))
historical_2010<-get(load("/p/projects/landuse/users/mbacca/DataSets/Historical/Total_Areas/SPAMMagObj_y_Phys2010_.Rda"))

intersect_m<-intersect(intersect(getNames(historical_2000),getNames(historical_2005)),getNames(historical_2010))
historical_2000<-historical_2000[,,intersect_m]
historical_2005<-historical_2005[,,intersect_m]
historical_2010<-historical_2010[,,intersect_m]

historical2<-mbind(mbind(historical_2000,historical_2005),historical_2010)


plotting_crop<-function(historical2,outputdir,sce){

  corr1<-data.frame(matrix(0,200,ncol = 7 ))
  corr2<-data.frame(matrix(0,200,ncol = 7 ))

  aux3<-1
  aux2<-1
  plot0<-list()
  plot2<-list()

  for(i in 1:length(outputdir)){

    # Reads gdx
    gdx<-paste0(outputdir[i],"fulldata.gdx")
    output<-croparea(gdx,product_aggr = FALSE,level="grid",dir=outputdir[i])

    historical<-historical2

    getCells(historical)<-getCells(output)

    inter_years<-(intersect(getYears(historical,as.integer = TRUE),getYears(output,as.integer = TRUE)))
    names<-intersect(getNames(historical),getNames(output))
    cells<-getCells(output)
    getCells(output)<-getCells(historical)
    inter_cells<-(intersect(getCells(historical),getCells(output)))

    output1<-as.ggplot(output[inter_cells,inter_years,names])
    historical1<-as.ggplot(historical[inter_cells,inter_years,names])


    is.na(historical1$Value)<-0
    is.na(output1$Value)<-0

    #saveRDS(output[inter_cells,inter_years,names],file=paste0(outputdir[i],sce[i],"1.Rda"))
    plot_data<-merge(historical1,output1,by=c("Cell","Region","Year","Data1"))
    plot_data$Value.x<-plot_data$Value.x
    plot_data$Value.y<-plot_data$Value.y*1e6
    plot_data$id<-paste0(plot_data$Region,plot_data$Cell)


    Regions<-levels(unique(plot_data$Region))
    rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
    r <- rf(32) #color palette

    #To plot the correlation plots of the land use change
    for (ye in inter_years)
    {
      #for (reg in Regions){
      for(prod in names){

        input_data1<-subset(plot_data,Year==ye & Data1==prod )$Value.x
        output_data1<-subset(plot_data,Year==ye & Data1==prod )$Value.y
        #& Region==reg

        corr1[aux2,1]<-sce[i]
        corr1[aux2,2]<-ye
        #corr1[aux2,3]<-reg
        corr1[aux2,4]<-prod
        corr1[aux2,5]<-cor(input_data1,output_data1)^2
        #corr1[aux2,4]<-cor(input_data1,output_data1)^2
        corr1[aux2,6]<-qualityMeasure(pd=output_data1,od = input_data1,measures="MAE",p_value = FALSE)
        #corr1[aux2,5]<-mae(obs=input_data1,sim=output_data1)
        corr1[aux2,7]<- qualityMeasure(pd=output_data1,od = input_data1,measures="Willmott refined",p_value = FALSE)

        limA<-max(input_data1)
        limB<-max(output_data1)
        limmax<-max(c(limA,limB))

        limA<-min(input_data1)
        limB<-min(output_data1)
        limmin<-min(c(limA,limB))


        plot0[[aux2]]<-ggplot(subset(plot_data,Year==ye & Data1==prod),aes(x=Value.x,y=Value.y))+theme_bw()+xlim(-0.05,1.7e5)+ylim(-0.05,1.7e5)#+xlim(0,limmax+limmax*0.05)+ylim(0,limmax+limmax*0.05)#limmax+limmax*0.05+facet_wrap(vars(Data1), ncol=5)
        plot0[[aux2]]<-plot0[[aux2]]+geom_bin2d(bins=50) + scale_fill_gradientn(colours=r,trans="log")+ coord_fixed(ratio=1) +geom_abline(intercept=0,slope=1)+geom_vline(xintercept = 0)+geom_hline(yintercept = 0)
        #plot0[[aux2]]<-plot0[[aux2]]+geom_point()+ coord_fixed(ratio=1) +geom_abline(intercept=0,slope=1)+geom_vline(xintercept = 0)+geom_hline(yintercept = 0)
        plot0[[aux2]]<-plot0[[aux2]]+labs(x=paste("SPAM",'(ha)'),y=paste("MAGPIE",'(ha)'),title=paste("Physical area of", prod,"in",ye))
        plot0[[aux2]]<-plot0[[aux2]]+theme(axis.text.x = element_text(color = "grey20", size = 20),
                                           axis.title.x = element_text(color = "grey20", size = 22),
                                           axis.text.y = element_text(color = "grey20", size = 20),
                                           axis.title.y = element_text(color = "grey20", size = 22),
                                           plot.title = element_text(size = 26, face="bold"),
                                           legend.text=element_text(size=16),
                                           #legend.position = c(0.8, 0.5),
                                           legend.title=element_text(size=18),
                                           legend.background = element_blank(),
                                           legend.box.background = element_rect(colour = "black"))

        plot0[[aux2]]#+
        #geom_text_repel(aes(label=ifelse(Value.x>10000 | Value.y>10000 ,as.character(id),'')),box.padding = unit(0.1, "lines"),
        #               point.padding = unit(0.5, "lines"),segment.color = 'grey50',
        #              fontface = 'bold', color = 'black',size=7)

        png(paste0(outputdir[i],corr1[aux2,1],"_",prod,"in",ye,"_.png"),width=700,height=700)
        print(plot0[[aux2]])
        dev.off()

        aux2=aux2+1
      }
    }

    ######part for landchange
    #--------------------------------------------------------------
    #To plot the correlation plots of the land use change
    dif_historical<-new.magpie(cells_and_regions = inter_cells,years = inter_years[2:length(inter_years)],names = names)
    dif_output<-new.magpie(cells_and_regions = inter_cells,years = inter_years[2:length(inter_years)],names = names)

    #Area calculation
    for (y in 2:length(inter_years))
    {
      dif_historical[,inter_years[y],]<-historical[inter_cells,inter_years[y],names]-historical[inter_cells,inter_years[y-1],names]
      dif_output[,inter_years[y],]<-output[inter_cells,inter_years[y],names]-output[inter_cells,inter_years[y-1],names]
    }

    #preparation of data for plotting
    dif_historical<-as.ggplot(dif_historical)
    dif_output<-as.ggplot(dif_output)


    #colnames(dif_historical)<-paste0(colnames(dif_historical),"_historical")
    #colnames(dif_output)<-paste0(colnames(dif_output),"_output")

    #fixes data to be plotted
    plot_data2<-merge(dif_historical,dif_output,by=c("Cell","Region","Year","Data1"))
    plot_data2$Value.x<-plot_data2$Value.x/1e6

    qadrants<-NULL



    for (ye in 2:length(inter_years))
    {
      #for (reg in Regions){
      for(prod in names){

        plot_data3<-subset(plot_data2,Year==inter_years[ye] & Data1==prod)

        limA<-max(plot_data3$Value.x)
        limB<-max(plot_data3$Value.y)
        limmax<-max(c(limA,limB))

        limA<-min(plot_data3$Value.x)
        limB<-min(plot_data3$Value.y)
        limmin<-min(c(limA,limB))

        plot2[[aux3]]<-ggplot(plot_data3,aes(x=Value.x,y=Value.y))+theme_bw()#+xlim(-limmax-0.05,limmax+0.05)+ylim(-limmax-0.05,limmax+0.05)#+geom_point()#
        plot2[[aux3]]<-plot2[[aux3]]+stat_bin2d(bins=50) + scale_fill_gradientn(colours=r, trans="log")+ coord_fixed(ratio=1) +geom_abline(intercept=0,slope=1)+geom_vline(xintercept = 0)+geom_hline(yintercept = 0)
        plot2[[aux3]]<-plot2[[aux3]]+labs(x=paste('Area historical (mio. ha)'),y=paste('Area MAgPIE (mio. ha)'),title=paste("Difference of",prod,"area between",inter_years[ye],"and",inter_years[ye]-5))
        plot2[[aux3]]<-plot2[[aux3]]+facet_zoom(xlim=c(-1,1), ylim=c(-1,1))+theme(axis.text.x = element_text(color = "grey20", size = 22),
                                                                                  axis.title.x = element_text(color = "grey20", size = 24),
                                                                                  axis.text.y = element_text(color = "grey20", size = 22),
                                                                                  axis.title.y = element_text(color = "grey20", size = 24),
                                                                                  plot.title = element_text(size = 28, face="bold"),
                                                                                  legend.text=element_text(size=20),
                                                                                  legend.title=element_text(size=20),
                                                                                  legend.position = c(0.1, 0.8),legend.background = element_blank(),
                                                                                  legend.box.background = element_rect(colour = "black") )


        q1<-length((subset(plot_data3,(Value.x>0 & Value.y>0)))$Cell)
        q2<-length(subset(plot_data3,Value.x<0 & Value.y>0)$Cell)
        q3<-length(subset(plot_data3,Value.x<0 & Value.y<0)$Cell)
        q4<-length(subset(plot_data3,Value.x>0 & Value.y<0)$Cell)
        zeros<-length(subset(plot_data3,Value.x==0 & Value.y==0)$Cell)
        zeros1<-length((subset(plot_data3,(Value.x>0 & Value.y==0)))$Cell)
        zeros2<-length((subset(plot_data3,(Value.x==0 & Value.y>0)))$Cell)
        zeros3<-length(subset(plot_data3,Value.x<0 & Value.y==0)$Cell)
        zeros4<-length(subset(plot_data3,Value.x==0 & Value.y<0)$Cell)
        qadrants<-rbind(qadrants,cbind(sce[i],inter_years[ye],prod,q1,q2,q3,q4,zeros,zeros1,zeros2,zeros3,zeros4))


        png(paste0(outputdir[i],sce[i],"Difference_Land_",prod,"in",ye,"_.png"),width=1321,height=1011)
        print(plot2[[aux3]])
        dev.off()

        corr2[aux3,1]<-sce[i]
        corr2[aux3,2]<-inter_years[ye]
        #corr1[aux2,3]<-reg
        corr2[aux3,4]<-prod
        corr2[aux3,5]<-cor(plot_data3$Value.x,plot_data3$Value.y)^2
        #corr1[aux2,4]<-cor(input_data1,output_data1)^2
        corr2[aux3,6]<-qualityMeasure(pd=plot_data3$Value.x,od = plot_data3$Value.y,measures="MAE",p_value = FALSE)
        #corr1[aux2,5]<-mae(obs=input_data1,sim=output_data1)
        corr2[aux3,7]<- qualityMeasure(pd=plot_data3$Value.x,od = plot_data3$Value.y,measures="Willmott refined",p_value = FALSE)

        aux3=aux3+1

      }
      write.csv(corr1,paste0(outputdir[i],sce[i],"Correlation.csv"))
      write.csv(corr2,paste0(outputdir[i],sce[i],"Correlation2.csv"))
      write.csv(qadrants,paste0(outputdir[i],sce[i],"Difference_Land_.csv"))
    }


    #--------------------------------------------------------------





    # }
  }
  out<-list(corr1,corr2)
  return(out)
}

a<-plotting_crop(historical2,outputdir,sce)

corr1<-a[[1]]
corr2<-a[[2]]

colnames(corr1)<-c("Scenario","Year","Region","Type_of_Crop","R2","MAE","Willmott")

aux<-1
plot_1<-list()
plot_11<-list()
plot_111<-list()

corr1[is.na(corr1)]<-0


#for(i in 1:length(Regions)){
#& Region==Regions[aux]

plot_1[[aux]]<-ggplot(subset(corr1,R2!=0 & Type_of_Crop %in% c("tece","trce","maiz","soybean","rice_pro")),aes(x=Year,y=R2,color=as.factor(Scenario)))+geom_point(size=4)+geom_line()+ theme_bw()+facet_wrap(vars(Type_of_Crop), ncol=3)
plot_1[[aux]]<-plot_1[[aux]]+theme(axis.text.x = element_text(color = "grey20", size = 14),
                                   axis.title.x = element_text(color = "grey20", size = 18),
                                   axis.text.y = element_text(color = "grey20", size = 14),
                                   axis.title.y = element_text(color = "grey20", size = 18),
                                   plot.title = element_text(size = 22, face="bold"),
                                   legend.text=element_text(size=18),
                                   legend.title=element_text(size=18),
                                   legend.background = element_blank(),
                                   legend.box.background = element_rect(colour = "black"),
                                   strip.text = element_text(size = 20)) #+
plot_1[[aux]]<-plot_1[[aux]]+labs(x=paste('Year'),y=paste('R2'),color="Scenarios",title=paste("R2 MAgPIE-SPAM per crop type"))+ scale_x_continuous(breaks=seq(2000, 2010, 5))

plot_11[[aux]]<-ggplot(subset(corr1,R2!=0 & Type_of_Crop %in% c("tece","trce","maiz","soybean","rice_pro")),aes(x=Year,y=MAE,color=as.factor(Scenario)))+geom_point(size=4)+geom_line()+ theme_bw()+facet_wrap(vars(Type_of_Crop), ncol=3)
plot_11[[aux]]<-plot_11[[aux]]+theme(axis.text.x = element_text(color = "grey20", size = 14),
                                     axis.title.x = element_text(color = "grey20", size = 18),
                                     axis.text.y = element_text(color = "grey20", size = 14),
                                     axis.title.y = element_text(color = "grey20", size = 18),
                                     plot.title = element_text(size = 22, face="bold"),
                                     legend.text=element_text(size=18),
                                     legend.title=element_text(size=18),
                                     legend.background = element_blank(),
                                     legend.box.background = element_rect(colour = "black"),
                                     strip.text = element_text(size = 20)) #+
# scale_color_manual(labels=c("CS0.5_ip0.5_im1","CS0.5_ip0.5_im0","CS0.5_ip1_im0.5","CS0.5_ip0_im0.5","CS1_ip0.5_im0.5","CS0.5_ip0.5_im0.5","CS0_ip0.5_im0.5"),
#                    values=c("#F8766D","#BB9D00","#00B81F","#00C0B8","#00A5FF","#E76BF3","#FF6C90"))
plot_11[[aux]]<-plot_11[[aux]]+labs(x=paste('Year'),y=paste('MAE (ha)'),color="Scenarios",title=paste("MAE MAgPIE-SPAM per crop type"))+ scale_x_continuous(breaks=seq(2000, 2010, 5))


plot_111[[aux]]<-ggplot(subset(corr1,R2!=0 ),aes(x=Year,y=Willmott,color=as.factor(Scenario)))+geom_point(size=3)+geom_line()+ theme_bw()+facet_wrap(vars(Type_of_Crop), ncol=3)+ylim(-1,1)
plot_111[[aux]]<-plot_111[[aux]]+theme(axis.text.x = element_text(color = "grey20", size = 14),
                                       axis.title.x = element_text(color = "grey20", size = 18),
                                       axis.text.y = element_text(color = "grey20", size = 14),
                                       axis.title.y = element_text(color = "grey20", size = 18),
                                       plot.title = element_text(size = 22, face="bold"),
                                       legend.text=element_text(size=18),
                                       legend.title=element_text(size=18),
                                       legend.background = element_blank(),
                                       legend.box.background = element_rect(colour = "black")) #+
# scale_color_manual(labels=c("CS0.5_ip0.5_im1","CS0.5_ip0.5_im0","CS0.5_ip1_im0.5","CS0.5_ip0_im0.5","CS1_ip0.5_im0.5","CS0.5_ip0.5_im0.5","CS0_ip0.5_im0.5"),
#                    values=c("#F8766D","#BB9D00","#00B81F","#00C0B8","#00A5FF","#E76BF3","#FF6C90"))
plot_111[[aux]]<-plot_111[[aux]]+labs(x=paste('Year'),y=paste('Willmott'),color="Scenarios",title=paste("Willmott MAgPIE-SPAM per crop type"))

aux<-aux+1
#}



outputdir1<-outputdir[1]
dir.create(paste0(outputdir1,"images"))
dir.create(paste0(outputdir1,"images/Performance"))

png(paste0(outputdir1,"images/Performance/","Statistics_try_%03d.png"),width=1014,height=750)


print(plot_1)
print(plot_11)
print(plot_111)

dev.off()
