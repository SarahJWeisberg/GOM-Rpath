#RPath model PREBAL
#Purpose: This script runs PREBAL diagnostics on balanced Rpath model
#         of the Gulf of Maine (GoM) in 1980-85

#Author: Sarah J. Weisberg; sarah.j.weisberg@stonybrook.edu

# Sun Jul 10 11:02:48 2022 ------------------------------


#Load packages
library(Rpath); library(data.table);library(dplyr);library(here)

#Load balanced model
source(here("R/rpath_balance_attempt_3.R"))


#Biomass span
#Isolate living groups
living_biomass<-REco$Biomass[1:56]
living_groups<-groups[1:56]
living_groups<-living_groups[order(living_TL)]
log10(max(living_biomass)/min(living_biomass)) #6.06 - good

#Biomass decomposition
#Isolate living groups
living_TL<-REco$TL[1:56]
plot(log10(living_biomass[order(living_TL)]), ylab = "Log(Biomass)",xlab = "Group in TL order",pch=19)

reg<-lm(log10(living_biomass)~order(living_TL))
abline(reg)

#Vital rate decomposition
living_PB<-REco$PB[1:56]
living_QB<-REco$QB[1:56]
plot(log10(living_PB[order(living_TL)]), ylab = "Log(PB)",xlab = "Group in TL order",pch=19)
plot(log10(living_QB[order(living_TL)]), ylab = "Log(QB)",xlab = "Group in TL order",pch=19)
#text(order(living_TL),log10(living_PB[order(living_TL)]),living_groups)
#reg<-lm(log10(living_biomass)~order(living_TL))
#abline(reg)

#ratios
#load classification data
class<-params_start[,c(1,4,5)]
final<-as.data.frame(REco$Biomass)
final$RPATH<-rownames(final)
final<-cbind(final,REco$PB,REco$QB)
final<-merge(class,final,by="RPATH")
colnames(final)<-c("RPATH","Class","Diet","Biomass","PB","QB")

#predator/prey
sp<-filter(final,Class == "Pelagic (Small)" | Class== "Pelagic (Small; Round)")
zp<-filter(final,Class == "Zooplankton")
pp<-filter(final,Class == "Primary Producer")
dm<-filter(final,Class == "Demersal (Flat)" | Class == "Demersal (Round)" | Class == "Demersal (round)")
bi<-filter(final,Class == "Invertebrate (Benthic)")
sh<-filter(final,Class == "Shark" | Class == "HMS")
w<-filter(final,Class == "Whale")

pred_prey<-cbind()

#fish types
pl<-filter(final,Class == "Pelagic (Small)" | Class == "Pelagic (Small; Round)" | Class == "Pelagic (Medium; Round)")
ff<-filter(final,Class == "Demersal (Flat)")
hms<-filter(final,Class == "HMS")
af<-rbind(dm,pl)
sha<-filter(final,Class == "Shark")

#invertebrate types
pi<-filter(final, Class == "Invertebrate (Pelagic)")
ai<-rbind(bi,pi,zp)

#diet types
pisc<-filter(final, Diet == "Pisc")
benth<-filter(final, Diet == "Benth")
plank<-filter(final, Diet == "Plank")

#biomass sums
sums<-rbind(sum(sp$Biomass),sum(zp$Biomass),sum(pp$Biomass),sum(dm$Biomass),sum(bi$Biomass),sum(sh$Biomass),sum(w$Biomass),
            sum(pl$Biomass),sum(ff$Biomass),sum(hms$Biomass),sum(af$Biomass),sum(sha$Biomass),sum(pi$Biomass),
            sum(ai$Biomass),sum(pisc$Biomass),sum(benth$Biomass),sum(plank$Biomass))
sums<-as.data.frame(cbind(sums,c("SP","ZP","PP","DM","BI","SH","W","PL","FF","HMS","AF","SHA","PI","AI","PISC","BENTH","PLANK")))
colnames(sums)<-c("Biomass","Type")
sums$Biomass<-as.numeric(sums$Biomass)
ratios_1<-as.data.frame(rbind(sums$Biomass[1]/sums$Biomass[2],sums$Biomass[2]/sums$Biomass[3],
                              sums$Biomass[1]/sums$Biomass[3],sums$Biomass[4]/sums$Biomass[5],
                              sums$Biomass[6]/sums$Biomass[1],sums$Biomass[7]/sums$Biomass[2]))
ratios_1<-cbind(ratios_1,c("SP:ZP",'ZP:PP','SP:PP','DM:BI','SH:SP','W:ZP'))
colnames(ratios_1)<-c("Ratio","Groups")

barplot(ratios_1$Ratio, names.arg = ratios_1$Groups)          

#fish group sums
ratios_2<-as.data.frame(rbind(sums$Biomass[4]/sums$Biomass[8],
                              sums$Biomass[1]/sums$Biomass[11],sums$Biomass[10]/sums$Biomass[11],
                              sums$Biomass[12]/sums$Biomass[11],sums$Biomass[4]/sums$Biomass[11]))
ratios_2<-cbind(ratios_2,c("DM:PL",'SP:AF','HMS:AF','Sharks:AF','DM:AF'))
colnames(ratios_2)<-c("Ratio","Groups")

barplot(ratios_2$Ratio, names.arg = ratios_2$Groups)     

#invert group sums
ratios_3<-as.data.frame(rbind(sums$Biomass[5]/sums$Biomass[14],
                              sums$Biomass[13]/sums$Biomass[14],sums$Biomass[2]/sums$Biomass[14],
                              sums$Biomass[3]/sums$Biomass[14],sums$Biomass[2]/sums$Biomass[5]))
ratios_3<-cbind(ratios_3,c("BI:AI",'PI:AI','ZP:AI','PP:AI','ZP:BI'))
colnames(ratios_3)<-c("Ratio","Groups")

barplot(ratios_3$Ratio, names.arg = ratios_3$Groups)     

#sums by diet type
ratios_4<-as.data.frame(rbind(sums$Biomass[15]/sums$Biomass[17],
                              sums$Biomass[16]/sums$Biomass[17],sums$Biomass[15]/sums$Biomass[16]))
ratios_4<-cbind(ratios_4,c("Pisc:Plank","Benth:Plank","Pisc:Benth"))
colnames(ratios_4)<-c("Ratio","Groups")

barplot(ratios_4$Ratio, names.arg = ratios_4$Groups)  

#production ratios
P<-living_biomass*living_PB
#all groups have P below PP - good

#look at average P/B ratios by functional group
mean(sp$PB)/mean(pp$PB)
mean(sp$PB)/mean(zp$PB)
mean(zp$PB)/mean(pp$PB)
mean(dm$PB)/mean(bi$PB)
mean(sh$PB)/mean(sp$PB)
mean(w$PB)/mean(zp$PB)
