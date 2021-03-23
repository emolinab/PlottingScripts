
#This script contains all necessary functions to plot grid and regional level values to see effect on capital
library(ggplot2)
library(magpie4)
library(luplot)

#Reads the gdx
tag <- "meetingMarch25"
gdx_c<-"/p/projects/landuse/users/mbacca/magpie_versions/"
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
}
