# Probably will not need this code
BenthosPB<-GOM.EMAX[RPATH == 'Macrobenthos',weighted.mean(PB)]
BenthosTL<-GOM.EMAX[RPATH == 'Macrobenthos',weighted.mean(TL)]
BenthosQB<-GOM.EMAX[RPATH == 'Macrobenthos',weighted.mean(QB)]
GOM.EMAX<-GOM.EMAX[RPATH == 'Macrobenthos', PB:=BenthosPB,]
GOM.EMAX<-GOM.EMAX[RPATH == 'Macrobenthos', QB:=BenthosQB,]