library(bnlearn)
library(readr)
alarm10000 <- read_csv("C:/Users/Eilder Jorge/Downloads/alarm10000.txt")
alarm10000 <- as.data.frame(alarm10000)
data(alarm)

for(x in 1:37){
  alarm10000[,x]<-as.factor(alarm10000[,x])
}

bnOriginalBic<-hc(alarm)
bnBic<-hc(alarm10000)
bnOriginalK2<-hc(alarm,score="k2")
bnK2<-hc(alarm10000,score="k2")
bnOriginalBDE1<-hc(alarm,score="bde",iss=1)
bnBDE1<-hc(alarm10000,score="bde",iss=1)
bnOriginalBDE10<-hc(alarm,score="bde",iss=10)
bnBDE10<-hc(alarm10000,score="bde",iss=10)


graphviz.plot(bnOriginalBic)
graphviz.plot(bnBic)
graphviz.plot(bnOriginalK2)
graphviz.plot(bnK2)
graphviz.plot(bnOriginalBDE1)
graphviz.plot(bnBDE1)
graphviz.plot(bnOriginalBDE10)
graphviz.plot(bnBDE10)

bnOriginalMi<-pc.stable(alarm,test="mi")
bnMi<-pc.stable(alarm10000,test="mi")
bnOriginalX2<-pc.stable(alarm,test="x2")
bnX2<-pc.stable(alarm10000,test="x2")
bnOriginalSpx2<-pc.stable(alarm,test="sp-x2")
bnSpx2<-pc.stable(alarm10000,test="sp-x2")

graphviz.plot(bnOriginalMi)
graphviz.plot(bnMi)
graphviz.plot(bnOriginalX2)
graphviz.plot(bnX2)
graphviz.plot(bnOriginalSpx2)
graphviz.plot(bnSpx2)

bnOriginalMMHC<-mmhc(alarm)
bnMMHC<-mmhc(alarm10000)
bnOriginalRsmax2<-rsmax2(alarm)
bnRsmax2<-rsmax2(alarm10000)

graphviz.plot(bnOriginalMMHC)
graphviz.plot(bnMMHC)
graphviz.plot(bnOriginalRsmax2)
graphviz.plot(bnRsmax2)

bn

originalAlarmBn <- empty.graph(names(alarm))
modelstring(originalAlarmBn) <- paste("[HIST|LVF][CVP|LVV][PCWP|LVV][HYP][LVV|HYP:LVF]",
                          "[LVF][STKV|HYP:LVF][ERLO][HRBP|ERLO:HR][HREK|ERCA:HR][ERCA][HRSA|ERCA:HR]",
                           "[ANES][APL][TPR|APL][ECO2|ACO2:VLNG][KINK][MINV|INT:VLNG][FIO2]",
                          "[PVS|FIO2:VALV][SAO2|PVS:SHNT][PAP|PMB][PMB][SHNT|INT:PMB][INT]",
                          "[PRSS|INT:KINK:VTUB][DISC][MVS][VMCH|MVS][VTUB|DISC:VMCH]",
                         "[VLNG|INT:KINK:VTUB][VALV|INT:VLNG][ACO2|VALV][CCHL|ACO2:ANES:SAO2:TPR]",
                          "[HR|CCHL][CO|HR:STKV][BP|CO:TPR]", sep = "")

par(mfrow = c(1,1), omi = rep(0, 4), mar = c(1, 0, 1, 0))

graphviz.compare(bnBic,bnK2)
hamming(bnK2,bnBic)
arcs(bnBic)
arcs(bnK2)

graphviz.compare(bnBic,bnBDE1)
hamming(bnBDE1,bnBic)
arcs(bnBDE1)

graphviz.compare(bnBic,bnBDE10)
arcs(bnBDE10)
hamming(bnBDE10,bnBic)

graphviz.compare(bnBDE1,bnBDE10)
hamming(bnBDE10,bnBDE1)


graphviz.compare(bnBic,bnMi)
hamming(bnBic,bnMi)
arcs(bnMi)

graphviz.compare(bnK2,bnSpx2)
hamming(bnK2,bnSpx2)
arcs(bnSpx2)


graphviz.compare(bnBic,bnMMHC)
hamming(bnBic,bnMMHC)
arcs(bnMMHC)


graphviz.compare(bnMi,bnX2)
arcs(bnX2)
hamming(bnMi,bnX2)

graphviz.compare(bnMi,bnSpx2)
hamming(bnMi,bnSpx2)

graphviz.compare(bnX2,bnRsmax2)
arcs(bnRsmax2)
hamming(bnX2,bnRsmax2)

graphviz.compare(bnMMHC,bnRsmax2)
hamming(bnMMHC,bnRsmax2)

graphviz.compare(originalAlarmBn,bnOriginalMMHC)
hamming(originalAlarmBn,bnOriginalMMHC)
