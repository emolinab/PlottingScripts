
#This script contains all necessary functions to plot grid and regional level values to see effect on capital
library(ggplot2)
library(magpie4)
library(luplot)

#Reads the gdx
tag <- "meetingMarch25"

gdx_c<-c("/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/output/Sticky_LPJmL5_dynamic_nocc__2021-03-18_22.33.33/",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/output/Sticky_LPJmL5_dynamic_cc__2021-03-18_22.27.55",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/output/Sticky_LPJmL5_regional_nocc__2021-03-18_22.31.43",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/output/Sticky_LPJmL5_regional_nocc__2021-03-18_22.31.43")

folder <- "/p/projects/landuse/users/mbacca/Plotting_scripts/Results/"
mobility <- FALSE
#Capital stocks regional level

.CapitalStocksRegion<-function(gdx_c,mobility=FALSE,folder){

for (gdx in gdx_c){
ImmobileStocks <- readGDX(gdx,"p38_capital_immobile")
MobileStocks <- readGDX(gdx,"p38_capital_mobile")

Stocks <- if (mobility=FALSE) ImmobileStocks + MobileStocks else mbind(ImmobileStocks,MobileStocks)
Stocks_plot <- as.ggplot(Stocks)
}
return(Stocks_plot)
}
