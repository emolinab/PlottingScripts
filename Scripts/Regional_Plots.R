
#This script contains all necessary functions to plot grid and regional level values to see effect on capital
library(ggplot2)
library(magpie4)
library(luplot)
library(luscale)

#Reads the gdx
tag <- "March25"

gdx_c<-c("/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_dynamic_nocc__2021-03-18_22.33.33/fulldata.gdx",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_dynamic_cc__2021-03-18_22.27.55/fulldata.gdx",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_free_nocc__2021-03-18_22.35.28/fulldata.gdx",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_free_cc__2021-03-18_22.29.49/fulldata.gdx",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_regional_nocc__2021-03-18_22.31.43/fulldata.gdx",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_regional_cc__2021-03-18_22.26.02/fulldata.gdx"
        )

scenario <- c("Dynamic_nocc","Dynamic_cc","CapitalShare_zero_nocc","CapitalShare_zero_cc","Regional_Static_nocc","Regional_Static_cc")
folder <- "/p/projects/landuse/users/mbacca/Plotting_scripts/Results/March25/"
mobility <- FALSE
colors<- c("firebrick4","darksalmon","forestgreen","darkseagreen2","cornflowerblue","cadetblue1")
#Capital stocks regional level

.CapitalStocksRegion<-function(gdx_c,mobility=FALSE,scenario,folder,tag,colors) {

  Stocks_plot <- NULL
  aux<-1
      for (gdx in gdx_c){
      ImmobileStocks <- dimSums(readGDX(gdx,"p38_capital_immobile"),dim=3)
      MobileStocks <- readGDX(gdx,"p38_capital_mobile")

      Stocks <- if (mobility == FALSE) ImmobileStocks + MobileStocks
      getNames(Stocks) <- "Capital Stocks (mio. USD05)"
      Stocks_reg <- superAggregate(Stocks,aggr_type = "sum", level="regglo")
      Stocks_plot_aux<- as.ggplot(Stocks_reg)
      Stocks_plot_aux$Scenario<-scenario[aux]
      Stocks_plot <- rbind(Stocks_plot,Stocks_plot_aux)
      aux <- aux+1
      }

      a<-ggplot(Stocks_plot)+geom_line(aes(x=Year,y=Value,color=as.factor(Scenario)),size=1)+theme_bw()+facet_wrap(~Region,scales = "free")+
      labs(y = "Capital Stocks (mio. USD05)",y = "Year")+ scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
        theme(text= element_text(size=18),axis.text.x = element_text(size=12),axis.text.y = element_text(size=15),legend.text=element_text(size=11),legend.title =element_text(size=20) )+
        scale_colour_manual(values=colors)+ labs(color = "Scenario")


      png(paste0(folder,"CapitalStocks",tag,".png"),height=600,width=1000)
      print(a)
      dev.off()
}

.CapInvShare<-function(gdx_c,scenario,folder,tag,colors) {

  costs_yes <- c("Land Conversion",
                "TC",
                "N Fertilizer",
                "P Fertilizer",
                "GHG Emissions",
                "AEI",
                "Land transition matrix",
                "Input Factors")
  Share_plot2 <- NULL
  Overall_plot2 <- NULL
  aux<-1
      for (gdx in gdx_c){
      Capital <- superAggregate(readGDX(gdx,"ov_cost_inv")[,,"level"],aggr_type = "sum", level="regglo")
      #Overall <- superAggregate(dimSums(readGDX(gdx,"ov_cost_prod")[,,"level"],dim=3),aggr_type = "sum", level="regglo")+Capital
      Overall <- dimSums(costs(gdx,level="regglo",sum=FALSE)[,,costs_yes],dim=3)

      Share<-Capital/Overall
      getNames(Share) <- "Capital Share of VoP"
      Share_plot<- as.ggplot(Share)
      Share_plot$Scenario<-scenario[aux]
      Share_plot2<- rbind(Share_plot2,Share_plot)

      overal_plot<-as.ggplot(Overall)
      overal_plot$Scenario<-scenario[aux]
      overal_plot2<- rbind(overal_plot2,overal_plot)

      aux <- aux+1
      }

      a<-ggplot(Share_plot2)+geom_line(aes(x=Year,y=Value,color=as.factor(Scenario)),size=1)+theme_bw()+facet_wrap(~Region,scales = "free")+
      labs(y = "Capital Share of VoP",y = "Year")+ scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
        theme(text= element_text(size=18),axis.text.x = element_text(size=12),axis.text.y = element_text(size=15),legend.text=element_text(size=11),legend.title =element_text(size=20) )+
        scale_colour_manual(values=colors)+ labs(color = "Scenario")

      b<-ggplot(overal_plot2)+geom_line(aes(x=Year,y=Value,color=as.factor(Scenario)),size=1)+theme_bw()+facet_wrap(~Region,scales = "free")+
        labs(y = "Value of Production mio. 05USD",y = "Year")+ scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
          theme(text= element_text(size=18),axis.text.x = element_text(size=12),axis.text.y = element_text(size=15),legend.text=element_text(size=11),legend.title =element_text(size=20) )+
          scale_colour_manual(values=colors)+ labs(color = "Scenario")



      png(paste0(folder,"CapitalShareVoP_",tag,".png"),height=600,width=1000)
      print(a)
      dev.off()
}

.FoodExpenditure<-function(gdx_c,scenario,folder,tag,colors) {

  FoodExpenditure_plot2<-NULL
  aux<-1
      for (gdx in gdx_c){

      FoodExpenditure <- FoodExpenditure(gdx,level="regglo")

      getNames(FoodExpenditure ) <- "Food Expenditure USD05pc"
      FoodExpenditure_plot<- as.ggplot(FoodExpenditure)
      FoodExpenditure_plot$Scenario<-scenario[aux]
      FoodExpenditure_plot2<- rbind(FoodExpenditure_plot2,FoodExpenditure_plot)
      aux <- aux+1
      }

      a<-ggplot(FoodExpenditure_plot2)+geom_line(aes(x=Year,y=Value,color=as.factor(Scenario)),size=1)+theme_bw()+facet_wrap(~Region,scales = "free")+
      labs(y = "Food Expenditure USD05pc",y = "Year")+ scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
        theme(text= element_text(size=18),axis.text.x = element_text(size=12),axis.text.y = element_text(size=15),legend.text=element_text(size=11),legend.title =element_text(size=20) )+
        scale_colour_manual(values=colors)+ labs(color = "Scenario")


      png(paste0(folder,"FoodExpenditure_",tag,".png"),height=600,width=1000)
      print(a)
      dev.off()
}

a<-.CapitalStocksRegion(gdx_c,mobility,scenario,folder,tag,colors)
b<-.CapInvShare(gdx_c,scenario,folder,tag,colors)
c<-.FoodExpenditure(gdx_c,scenario,folder,tag,colors)
