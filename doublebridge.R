library(tidyverse)
library(readxl)

sawana<-read_excel("C:/Users/liuha/Documents/Double bridge.INC/SAW Study Level QC Report Feb 2019.xlsx", sheet = "Analyze Report")
sawodw<-read_excel("C:/Users/liuha/Documents/Double bridge.INC/SAW Study Level QC Report Feb 2019.xlsx", sheet= "ODW Report")

##Merge same display name row
new<-merge(sawodw, sawana, by="Display Name")%>%
  .[!duplicated(.$`Display Name`),]

##fullname match
new$FullNamex<-gsub(" ","", new$`Full Name.x`)
new$FullNamey<-gsub(" ","", new$`Full Name.y`)

new$FullNamex<-str_replace_all(new$FullNamex, "[\r\r\r\n]", "")
new$FullNamey<-str_replace_all(new$FullNamey, "[\r\r\r\n]", "")
new<-as.data.frame(new)
new2<-within(new, Fullnamematch<-new$FullNamex == new$FullNamey)

##CRO match
new3<-within(new, cromatch<-new$`Sponsor/CRO ID`==new$`Sponsor/ CRO ID`)
##Compound name match
new4<-within(new, compoundnamematch<-new$`Compound Name`==new$`Compound name`)
##with Therapeutic Area in SAW match
new5<-within(new, TherapeuticAreamatch<-new$`Therapeutic Area`==new$`Therapeutic area`)
##with phase match
new6<-within(new, phasematch<- new$Phase.x==new$Phase.y)
##First Protocol Approval
new7<-within(new, firstprotocolmatch<-new$`First Protocol Approval.x`==new$`First Protocol Approval.y`) 
##Pediatric Study match
new8<-within(new, PediatricStudymatch<-new$`Pediatric Study.x`==new$`Pediatric Study.y`)


##Indication match
new$Indication.x<-gsub(" ","", new$Indication.x)
new$Indication.y<-gsub(" ","", new$Indication.y)
new$Indication.x<-str_replace_all(new$Indication.x,"[\r\r\r\n]", "" )
new$Indication.y<-str_replace_all(new$Indication.y, "[\r\r\r\n]", "")
new9<-within(new, Indicationmatch<-new$Indication.x==new$Indication.y)


sawreco<-data.frame(
    DisplayName=new$`Display Name`,StudyStatus=sawodw$`Study Status`,FullName=new$`Full Name.x`, FullnameSAW=new$`Full Name.y` , Fullnamematch=new2$Fullnamematch, SponsorCROID=new3$`Sponsor/CRO ID`,
    SponsorCROIDSAW=new3$`Sponsor/ CRO ID`, cromatch=new3$cromatch, CompoundName=new4$`Compound Name`, CompoundNameinSAW=new4$`Compound name`, CompoundNamematch=new4$compoundnamematch, TherapeuticArea=new5$`Therapeutic Area`,
    TherapeuticArea=new5$`Therapeutic area`, TherapeuticAreamatch= new5$TherapeuticAreamatch, phase=new6$Phase.x, phaseinSAW=new6$Phase.y, phasematch=new6$phasematch,
    FirstProtocolApproval=new7$`First Protocol Approval.x`, FirstProtocolApprovalSAW=new7$`First Protocol Approval.y`, FirstProtocolApprovalmatch= new7$firstprotocolmatch,
    PediatricStudy=new8$`Pediatric Study.x`, PediatricStudySAW=new8$`Pediatric Study.y`, PediatricStudymatch=new8$PediatricStudymatch,
    Indication=new$Indication.x, IndicationSAW=sawodw$Indication, Indicationmatch=new9$Indicationmatch
    )


for (i in 1:ncol(sawreco)) {
  
  if(class(sawreco[,i])=="logical"){
    sawreco[,i]<-as.factor(sawreco[,i])
    sawreco[,i]<-factor(sawreco[,i], levels =  c("TRUE","FALSE"), labels=c("YES", "NO"))
  }
  else
    sawreco[,i]<-sawreco[,i]
  
}


