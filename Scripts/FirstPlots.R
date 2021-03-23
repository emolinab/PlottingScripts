
#This script contains all necessary functions to plot grid and regional level values to see effect on capital
library(ggplot2)
library(magpie4)
library(luplot)
library(luscale)

#Reads the gdx
tag <- "meetingMarch25"

gdx_c<-c("/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_dynamic_nocc__2021-03-18_22.33.33/fulldata.gdx",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_dynamic_cc__2021-03-18_22.27.55/fulldata.gdx")

scenario <- c("Dynamic_nocc","Dynamic_cc")
folder <- "/p/projects/landuse/users/mbacca/Plotting_scripts/Results/March25/"
mobility <- FALSE
#Capital stocks regional level

.CapitalStocksRegion<-function(gdx_c,mobility=FALSE,scenario,folder,tag){

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
  scale_colour_manual(values=c("brown3","brown1"))+ labs(color = "Scenario")


png(paste0(folder,"CapitalStocks",tag,".png"),height=600,width=1000)
print(a)
dev.off()
}


a<-.CapitalStocksRegion(gdx_c,mobility,scenario,folder,tag)
