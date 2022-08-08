library(meta)
#reproduction of forest plot in https://www.ahajournals.org/doi/full/10.1161/STROKEAHA.119.025080
studlab=c("TNK-S2B","Australian TNK","Australian TNK","TNK-S2B","ATTEST","EXTEND-IA TNK","TNK-S2B","Nor-Test")
low="Tenecteplase 0.1 mg/kg"
mid="Tenecteplase 0.25 mg/kg"
high="Tenecteplase 0.4 mg/kg"
intervention=c(low,low,mid,mid,mid,mid,high,high)
Ee=c(14,9,18,15,13,49,7,354)
Ec=c(5,5,5,5,10,41,3,345)
Ne=c(31,25,25,31,47,101,19,549)
Nc=c(12,13,12,12,49,101,7,551)
df=data.frame(studlab,intervention,Ee,Ec,Ne,Nc)

m.bin=metabin(event.e=Ee,n.e=Ne,event.c=Ec,n.c=Nc,studlab=studlab,data=df,sm="RD",method="MH",fixed=F,random=T)
forest(m.bin)
sub=update.meta(m.bin,subgroup=intervention,tau.common=F)
forest(sub,label.left = "Favors alteplase",label.right="Favors tenecteplase",layout="RevMan5",subgroup.name=c(low,mid,high),print.subgroup.name =F)
?forest
?forest

#addition of AcT, NOR-TEST2, TASTE-A, and EXTEND-IA TNK 2

studlab=c("TNK-S2B","Australian TNK","Australian TNK","TNK-S2B","ATTEST","EXTEND-IA TNK","TNK-S2B","NOR-TEST","NOR-TEST2","TASTE-A","AcT")
low="Tenecteplase 0.1 mg/kg"
mid="Tenecteplase 0.25 mg/kg"
high="Tenecteplase 0.4 mg/kg"
intervention=c(low,low,mid,mid,mid,mid,high,high,high,mid,mid)
Ee=c(14,9,18,15,13,49,7,354,31,23,296)
Ec=c(5,5,5,5,10,41,3,345,52,20,266)
Ne=c(31,25,25,31,47,101,19,549,96,55,802)
Nc=c(12,13,12,12,49,101,7,551,101,49,765)
df=data.frame(studlab,intervention,Ee,Ec,Ne,Nc)

m.bin=metabin(event.e=Ee,n.e=Ne,event.c=Ec,n.c=Nc,studlab=studlab,data=df,sm="RD",method="MH",fixed=F,random=T)
forest(m.bin)
sub=update.meta(m.bin,subgroup=intervention,tau.common=F)
forest(sub,label.left = "Favors alteplase",label.right="Favors tenecteplase",layout="RevMan5",subgroup.name=c(low,mid,high),print.subgroup.name =F)

?forest
?forest
metabias(m.bin,method.bias="peters")
funnel(m.bin)
pcurve(m.bin)

library(netmeta)

#network meta-analysis
library(readxl)
df=read_excel("C:/Users/tonynickonchuk/Desktop/Tenecteplase/tenec nma.xlsx")
p1=pairwise(treatment,responders,sampleSize,studlab=study,data=df,sm="RR")

nb1=netmetabin(p1,sm="RD",fixed=F,random=T,reference.group="Alteplase",details.chkmultiarm = F,sep.trts="vs",method="Inverse")
forest(nb1)

netrank(nb1,small.values="bad")

long_labels=c("Alteplase","Tenecteplase 0.1 mg/kg","Tenecteplase 0.25 mg/kg","Tenecteplase 0.4 mg/kg")
meta::forest(nb1,reference.group="Alteplase",sortvar=TE,smlab=paste("Tenecteplase vs Alteplase by Dosage\n% with mRS 0-1"),drop.reference.group=T,label.left="Favors alteplase",label.right="Favors tenecteplase",labels=long_labels)

m.netmeta=netmeta(TE=TE,seTE=seTE,treat1=treat1,treat2=treat2,studlab=studlab,data=p1,sm="RD",fixed=F,random=T,reference.group="Alteplase",details.chkmultiarm = T,sep.trts="vs")
summary(m.netmeta)
netgraph(m.netmeta)

#outcome of symptomatic ICH
studlab=c("TNK-S2B","Australian TNK","Australian TNK","TNK-S2B","ATTEST","EXTEND-IA TNK","TNK-S2B","Nor-Test")
low="Tenecteplase 0.1 mg/kg"
mid="Tenecteplase 0.25 mg/kg"
high="Tenecteplase 0.4 mg/kg"
intervention=c(low,low,mid,mid,mid,mid,high,high)
Ee=c(0,1,1,2,1,1,3,15)
Ec=c(0,1,2,0,2,1,1,13)
Ne=c(31,25,25,31,47,101,19,549)
Nc=c(10,12,13,10,49,101,11,551)
df=data.frame(studlab,intervention,Ee,Ec,Ne,Nc)

m.bin=metabin(event.e=Ee,n.e=Ne,event.c=Ec,n.c=Nc,studlab=studlab,data=df,sm="RD",method="MH",fixed=F,random=T)
forest(m.bin)
sub=update.meta(m.bin,subgroup=intervention,tau.common=F)
forest(sub,label.left = "Favors tenecteplase",label.right="Favors alteplase",layout="RevMan5",subgroup.name=c(low,mid,high),print.subgroup.name =F)

#outcome of death
studlab=c("TNK-S2B","Australian TNK","Australian TNK","TNK-S2B","ATTEST","EXTEND-IA TNK","TNK-S2B","Nor-Test")
low="Tenecteplase 0.1 mg/kg"
mid="Tenecteplase 0.25 mg/kg"
high="Tenecteplase 0.4 mg/kg"
intervention=c(low,low,mid,mid,mid,mid,high,high)
Ee=c(2,3,1,7,8,10,3,29)
Ec=c(2,1,2,3,6,18,3,26)
Ne=c(31,25,25,31,47,101,19,549)
Nc=c(10,12,13,10,49,101,11,551)
df=data.frame(studlab,intervention,Ee,Ec,Ne,Nc)

m.bin=metabin(event.e=Ee,n.e=Ne,event.c=Ec,n.c=Nc,studlab=studlab,data=df,sm="RR",method="MH",fixed=F,random=T)
forest(m.bin)
sub=update.meta(m.bin,subgroup=intervention,tau.common=F)
forest(sub,label.left = "Favors tenecteplase",label.right="Favors alteplase",layout="RevMan5",subgroup.name=c(low,mid,high),print.subgroup.name =F)

library(metafor)
studlab=c("Australian TNK","TNK-S2B","AcT","ATTEST","Australian TNK","EXTEND-IA TNK","TASTE-A","TNK-S2B","NOR-TEST","NOR-TEST 2","TNK-S2B")
dose=c(low,low,rep(mid,6),rep(high,3))
Ee=c(9,14,296,13,18,49,23,15,354,31,7)
Ne=c(25,31,802,47,25,101,55,31,549,96,19)
Ec=c(5,5,266,10,5,41,20,5,345,52,3)
Nc=c(13,12,765,49,12,101,49,12,551,101,7)
NIHSS=c(14.5,10,9.5,12,14.5,17,8,10,4,11.5,10)
df=data.frame(studlab,dose,Ee,Ne,Ec,Nc,NIHSS)
m.bin=metabin(Ee,Ne,Ec,Nc,studlab,data=df)
df$dose=factor(df$dose,levels=unique(df$dose))

m.qual=rma(yi=TE,sei=seTE,data=m.bin,method="ML",mods=~dose+NIHSS,test="knha")
m.qual

#simulations of weights and prices; used weights from https://www.nejm.org/doi/full/10.1056/nejm199512143332401 and https://www.nejm.org/doi/full/10.1056/nejmoa0804656; 6 different patient groups; number for each distribution is the sample size of each group reported in the papers

one=rnorm(144,76,15)
two=rnorm(147,80,18)
three=rnorm(168,76,16)
four=rnorm(165,80,21)
five=rnorm(418,78.5,15)
six=rnorm(403,78,16)

#combine all into one sample population
weight=c(one,two,three,four,five,six)

#sample with replacement from weight population
tracking=data.frame(run=1:10000,alteplase_cost=NA,tenecteplase_cost=NA)

for (i in 1:10000){
patients=sample(weight,600,replace=T)  
alteplase_dose=patients*0.9
alteplase_dose=ifelse(alteplase_dose>90,90,alteplase_dose)
alteplase_vials=ceiling(alteplase_dose/50)
alteplase_cost=ifelse(alteplase_vials==2,2874.05,1437.03)
tracking$alteplase_cost[i]=sum(alteplase_cost)
tracking$tenecteplase_cost[i]=600*2874.05
}
tracking$difference=tracking$tenecteplase_cost-tracking$alteplase_cost
quantile(tracking$difference,c(0.025,0.975))
mean(tracking$difference)
sum(tracking$difference>=100000)
19/10000
0.0019*100
