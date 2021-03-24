# Capital per ha
library(ggplot2)
library(magpie4)
library(luplot)
library(luscale)

#Reads the gdx
tag <- "March25"

gdx_c<-c("/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_dynamic_nocc__2021-03-18_22.33.33/",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_dynamic_cc__2021-03-18_22.27.55/",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_free_nocc__2021-03-18_22.35.28/",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_free_cc__2021-03-18_22.29.49/",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_regional_nocc__2021-03-18_22.31.43/",
         "/p/projects/landuse/users/mbacca/magpie_versions/sticky_lpjml5/magpie/output/Sticky_LPJmL5_regional_cc__2021-03-18_22.26.02/"
        )

scenario <- c("Dynamic_nocc","Dynamic_cc","CapitalShare_zero_nocc","CapitalShare_zero_cc","Regional_Static_nocc","Regional_Static_cc")
folder <- "/p/projects/landuse/users/mbacca/Plotting_scripts/Results/March25/"
mobility <- FALSE
folder <- "/p/projects/landuse/users/mbacca/Plotting_scripts/Results/March25/"
#Capital stocks regional level

.plotCapitalHa<-function(gdx_c,scenario,mobility,tag,folder){
 aux<-1
   for(gd in gdx_c){
     gdx<-paste0(gd,"fulldata.gdx")
     ImmobileStocks <- dimSums(readGDX(gdx,"p38_capital_immobile"),dim=3)
     MobileStocks <- readGDX(gdx,"p38_capital_mobile")
     Stocks <- if (mobility == FALSE) ImmobileStocks + MobileStocks
     Area<-croparea(gdx,product_aggr = TRUE,level="cell")

     Stocks_Area<-Stocks/Area
     Stocks_Area[!is.finite(Stocks_Area)]<-0

     Stocks_area_grid<-gdxAggregate(gdx,Stocks_Area,weight = NULL, from= "cell",to="grid", absolute = FALSE, dir = gd)
     getNames(Stocks_area_grid)<-"Stocks per ha"

     write.magpie(Stocks_area_grid, file_name=paste0(folder,"Stocks_area_",scenario[aux],"_",tag,".nc"),file_type = "nc")
     aux<-aux+1
   }
}

.plotCapitalUnused<-function(gdx_c,scenario,mobility,tag,folder){
  aux<-1
    for(gd in gdx_c){
      gdx<-paste0(gd,"fulldata.gdx")
      ImmobileStocks <- readGDX(gdx,"p38_capital_immobile")
      MobileStocks <- readGDX(gdx,"p38_capital_mobile")

       #capital needed
       cap_needed_f<- readGDX(gdx,"i38_capital_need")
       cap_needed_f<- gdxAggregate(gdx,cap_needed_f,weight = NULL, from= "reg",to="cell", absolute = FALSE)

      #Production <- production(gdx,level="cell",products="kcr")
      Production <- (readGDX(gdx,"ov_prod")[,,"level"])[,,getNames(ImmobileStocks)]
      cap_needed <- Production*cap_needed_f

      #Investments
      inv_mobile <- dimSums(readGDX(gdx,"ov38_investment_mobile")[,,"level"],dim=3)
      inv_immobile <- readGDX(gdx,"ov38_investment_immobile")[,,"level"]

      Unused_capital_mo<-MobileStocks+inv_mobile-dimSums(cap_needed[,,"mobile"],dim=3)
      Unused_capital_immo<-dimSums(ImmobileStocks+inv_immobile-cap_needed[,,"immobile"],dim=3)

      Unused_capital<- Unused_capital_mo+Unused_capital_immo
      #Area
      Area<-croparea(gdx,product_aggr = TRUE,level="cell")
      Unused_ha<-Unused_capital/Area
      Unused_ha[!is.finite(Unused_ha)]<-0
      Unused_ha[Unused_ha<0]<-0

      Unused_ha_grid<-gdxAggregate(gdx,Unused_ha,weight = NULL, from= "cell",to="grid", absolute = FALSE, dir = gd)
      getNames(Unused_ha_grid)<-"Unused Capital (USD05) per ha"

      write.magpie(Unused_ha_grid, file_name=paste0(folder,"UnusedStocks_",scenario[aux],"_",tag,".nc"),file_type = "nc")
      aux<-aux+1
    }
}

.YieldComparison<-function(gdx_c,folder,tag){
  #lpj overall yield
  gdx<-paste0(gdx_c[1],"/fulldata.gdx")
  gd<-gdx_c[1]

  yield<-read.magpie("/p/projects/landuse/users/mbacca/Additional_data_sets/YieldPjmL/lpj_yields.cs3")[,,"pasture",invert=TRUE]
  yield_2020<-yield[,2020,]

  yield_diff<-(yield-yield_2020)
  getYears(yield_diff)<-getYears(yield)
  yield_diff<-yield_diff/yield*100
  yield_diff[!is.finite(yield_diff)]<-0

  mapping<-as.data.frame(getNames(yield_diff))
  colnames(mapping)<-"Yields"
  mapping$all<-"All"
  weight<-yield_diff
  weight[,,]<-1

  Yield_Average<-toolAggregate(yield_diff,rel=mapping,weight = weight, from= "Yields",to="all",dim=3)
  getNames(Yield_Average)<-"Average yield change"

  Yield_Average<-toolAggregate(yield_diff,rel=mapping,weight = weight, from= "Yields",to="all",dim=3)
  Yield_Average<-gdxAggregate(gdx,Yield_Average,weight = NULL, from= "cell",to="grid", absolute = FALSE, dir = gd)

  write.magpie(Yield_Average, file_name=paste0(folder,"AverageYieldChange_",tag,".nc"),file_type = "nc")


}

a<-.plotCapitalHa(gdx_c,scenario,mobility,tag,folder)
b<-.plotCapitalUnused(gdx_c,scenario,mobility,tag,folder)
c<-.YieldComparison(gdx_c,folder,tag)
