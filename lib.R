############################################# Header #############################################
  install.packages("MASS")
  install.packages("vcd")
  library(MASS)
  library(vcd)
  data()
  View(Suicide)
############################################# Part 1 #############################################
freq <- Suicide$Freq
sex <- Suicide$sex
## Balkendiagramm der Gesamtanzahl aller Suizide abhaengig vom Geschlecht
male <- Suicide$Freq[1:153]
anzahlSuizideM <- 0
for(m in male) {
  anzahlSuizideM <- anzahlSuizideM + m
}
female <- Suicide$Freq[154:306]
anzahlSuizideW <- 0
for(f in female) {
  anzahlSuizideW <- anzahlSuizideW + f
}
# barplot(c(anzahlSuizideM,anzahlSuizideW), main="absolute Zahl an Suiziden bei Maennern und Frauen", beside = TRUE, horiz = F, col = c("blue", "red"), ylim = c(0,35000), xpd=F, border = "white", space = c(1, 0.8), names.arg = c("Maenner", "Frauen"))
# legend("topright", legend = c(anzahlSuizideM, anzahlSuizideW), fill =  c("blue", "red"))
############################################# Part 2 #############################################
######################### Tortendiagramm Anzahl aller Suizidarten (methods) unabhaengig vom Geschlecht #########################
pichen <- test(10, 90, TRUE)
rr <- pichen[1:9]
ww <- pichen[10:18]
ges <- rr + ww
# pie(prop.table(c(poison, cookgas, toxicgas, hang, drown, gun, knife, jump, other)), main="Suizidmethoden anteilig", col = c("blue", "red", "yellow", "green", "brown", "purple", "orange", "black", "darkred"), labels = c("poison", "cookgas", "toxicgas", "hang", "drown", "gun", "knife", "jump", "other"))
# legend("topleft", legend = c(round((poison/53182)*100, 2), round((cookgas/53182)*100, 2), round((toxicgas/53182)*100, 2), round((hang/53182)*100, 2), round((drown/53182)*100, 2), round((gun/53182)*100, 2), round((knife/53182)*100, 2), round((jump/53182)*100, 2), round((other/53182)*100), 2), fill =  c("blue", "red", "yellow", "green", "brown", "purple", "orange", "black", "darkred"))
######################### Tortendiagramm Anzahl aller Suizidarten (methods) abhaengig vom Geschlecht #########################
# par(mfrow=c(1,2))
#   pie(prop.table(rr), main="Suizidmethoden maennlich", col = rainbow(9), labels = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"))
#   legend("topleft",cex = 0.9, legend = c(round((toxicgasM/53182)*100, 2), round((cookgasM/53182)*100, 2), round((poisonM/53182)*100, 2), round((hangM/53182)*100, 2), round((drownM/53182)*100, 2), round((gunM/53182)*100, 2), round((knifeM/53182)*100, 2), round((jumpM/53182)*100, 2), round((otherM/53182)*100), 2), fill =  rainbow(9))
#   pie(prop.table(ww), main="Suizidmethoden weiblich", col = rainbow(9), labels = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"))
#   legend("bottomleft", cex = 0.9, legend = c(round((toxicgasW/53182)*100, 2), round((drownW/53182)*100, 2), round((poisonW/53182)*100, 2),  round((hangW/53182)*100, 2), round((gunW/53182)*100, 2), round((otherW/53182)*100, 2), round((knifeW/53182)*100, 2), round((jumpW/53182)*100, 2), round((cookgasW/53182)*100, 2)), fill =  rainbow(9))
# par(mfrow=c(1,1))
############################################# Part 3 #############################################
######################### wie viele Menschen pro Altersgruppe brachten sich um? (Torten) #########################
i <- 1
age1020freq <- 0
age2535freq <- 0
age4050freq <- 0
age5565freq <- 0
age7090freq <- 0
for (ih in Suicide$age.group){
  if(ih == "10-20"){
    age1020freq <- age1020freq + Suicide$Freq[i]
  }
  else if(ih == "25-35"){
    age2535freq <- age2535freq + Suicide$Freq[i]
  }
  else if(ih == "40-50"){
    age4050freq <- age4050freq + Suicide$Freq[i]
  }
  else if(ih == "55-65"){
    age5565freq <- age5565freq + Suicide$Freq[i]
  }
  else{
    age7090freq <- age7090freq + Suicide$Freq[i]
  }
  i <- i + 1
}
# pie(prop.table(c(age1020freq,age2535freq,age4050freq,age5565freq,age7090freq)), main = "Das Mid-life-crisis-Phaenomen", col= rainbow(5), labels = c("10-20", "25-35", "40-50", "55-65", "70-90"))
# legend("topleft", legend = c(round((age1020freq/53182)*100, 2), round((age2535freq/53182)*100, 2), round((age4050freq/53182)*100, 2), round((age5565freq/53182)*100, 2), round((age7090freq/53182)*100, 2)), fill = rainbow(5))
# plot(c("1", "2", "3", "4", "5"), c(age1020freq,age2535freq,age4050freq,age5565freq,age7090freq), type="b", , main = "Suizide je Altersgruppe", pch = 4, cex = 0.8, col=c("blue"), xlab="Altersgruppe", ylab="Anzahl Suizide")
# überarbeiten
############################################# Part 4 #############################################
######################### Suizidart abhaengig von der Altersgruppe (Balken) #########################
nachAlter <- 0
currentAge <- 10
toAppend <- 0
i2 <- 1
currentAge <- 10
for(man in Suicide$age) {
  if (toAppend != 0){
     if(Suicide$age[i2] != currentAge) {
       nachAlter <- c(nachAlter, toAppend)
       currentAge <- currentAge + 5
       toAppend <- 0
     }
  }
  toAppend <- toAppend + Suicide$Freq[i2]
  i2 <- i2 + 1
}
nachAlter <- c(nachAlter, toAppend)
# plot(nachAlter, type = "o", col = "red", ylab = "", main = "wie viele haben sich pro Alter umgebracht?")
#oder:
# polygon(all, ylim(), col="slateblue1")
############################################# Part 5 #############################################
######################### In welchem Alter bringen sich Maenner und Frauen am meisten um? #########################
maxMen <- 0
maxMen <- c(maxMen, 1)
maxWomen <- 0
maxWomen <- c(maxWomen, 1)
toAppend <- 0
i3 <- 1
currentAge <- 10
    for(man in Suicide$age[1:153]) {
      if (toAppend != 0){
        if(man != currentAge) {
          maxMen <- c(maxMen, toAppend)
          currentAge <- currentAge + 5
          toAppend <- 0
        }
      }
      toAppend <- toAppend + Suicide$Freq[i3]
      i3 <- i3 + 1
    }
maxMen <- c(maxMen, toAppend)
maxMen <- c(maxMen[2:length(maxMen)])
# plot(maxMen)
i3 <- 154
toAppend <- 0
currentAge <- 10
    for(woman in Suicide$age[154:306]) {
      if (toAppend != 0){
        if(woman != currentAge) {
          maxWomen <- c(maxWomen, toAppend)
          currentAge <- currentAge + 5
          toAppend <- 0
        }
      }
      toAppend <- toAppend + Suicide$Freq[i3]
      i3 <- i3 + 1
    }
maxWomen <- c(maxWomen, toAppend)
maxWomen <- c(maxWomen[2:length(maxWomen)])
# plot(maxWomen)
##Mittelwert
i4 <- 1
c <- 0
 for (entry in Suicide$age) {
   c <- c + (entry * Suicide$Freq[i4])
   i4 <- i4 + 1
 }
mittelwert <- c/53182
      # library
z <- 1
poi <- 0
     for (tz in Suicide$Freq){
       if(Suicide$method[z] == "poison"){
         poi <- c(poi, tz)
       }
       z <- z + 1
     }
######## Daten
      #
      # poi <- poi[2:length(poi)]
      #
      # (poi)
      #
      #
      # Spannweite <- max(poi) - min(poi)
      #
      # (Interquartilsabstand <- IQR(poi,na.rm=T,type=6))
      #
      # (quantile <- quantile(poi, probs=c(0.25,0.5,0.75),na.rm=T))
      #
######### child, middle, old, tooold
child <- test(10, 15, FALSE)
ChildM <- child[1:9]
ChildW <- child[10:18]
middle <- test(20, 40, FALSE)
MiddleM <- middle[1:9]
MiddleW <- middle[10:18]
midlife <- test(45, 60, FALSE)
MidlifeM <- midlife[1:9]
MidlifeW <- midlife[10:18]
old <- test(65, 90, FALSE)
OldM <- old[1:9]
OldW <- old[10:18]
# par(mfrow=c(1,2))
#   pie(prop.table(ChildM), main="Child boys", labels = c("", "", "", "", "", "", "", "", ""), col = rainbow(9))
#   legend("bottomleft", cex = 0.9, legend = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"), fill =  rainbow(9))
#   pie(prop.table(ChildW), main="Child girls", col = rainbow(9), labels = c("", "", "", "", "", "", "", "", ""))
#   legend("bottomleft", cex = 0.9, legend = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"), fill =  rainbow(9))
# par(mfrow=c(1,1))
#
# par(mfrow=c(1,2))
#   pie(prop.table(MiddleM), main="Middle male", col = rainbow(9), labels = c("", "", "", "", "", "", "", "", ""))
#   legend("bottomleft", cex = 0.9, legend = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"), fill =  rainbow(9))
#   pie(prop.table(MiddleW), main="Middle female", col = rainbow(9), labels = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"))
#   legend("bottomleft", cex = 0.9, legend = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"), fill =  rainbow(9))
# par(mfrow=c(1,1))
#
# par(mfrow=c(1,2))
#   pie(prop.table(MidlifeM), main="male in midlife", col = rainbow(9), labels = c("", "", "", "", "", "", "", "", ""))
#   legend("bottomleft", cex = 0.9, legend = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"), fill =  rainbow(9))
#   pie(prop.table(MidlifeW), main="female in midlife", col = rainbow(9), labels = c("", "", "", "", "", "", "", "", ""))
#   legend("bottomleft", cex = 0.9, legend = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"), fill =  rainbow(9))
# par(mfrow=c(1,1))
#
# par(mfrow=c(1,2))
#   pie(prop.table(OldM), main="old male", col = rainbow(9), labels = c("", "", "", "", "", "", "", "", ""))
#   legend("bottomleft", cex = 0.9, legend = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"), fill =  rainbow(9))
#   pie(prop.table(OldW), main="old female", col = rainbow(9), labels = c("", "", "", "", "", "", "", "", ""))
#   legend("bottomleft", cex = 0.9, legend = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"), fill =  rainbow(9))
# par(mfrow=c(1,1))
test <- function(ageseins, ageszwei, all){
  poisonM <- 0
  cookgasM <- 0
  toxicgasM <- 0
  otherM <- 0
  jumpM <- 0
  knifeM <- 0
  gunM <- 0
  drownM <- 0
  hangM <- 0
  poisonW <- 0
  cookgasW <- 0
  toxicgasW <- 0
  otherW <- 0
  jumpW <- 0
  knifeW <- 0
  gunW <- 0
  drownW <- 0
  hangW <- 0
  i5 <- 1
  for(ages in Suicide$age) {
    if(ages == ageseins || ages == ageszwei || all == TRUE){
      if(Suicide$method[i5] == "poison"){
        if(Suicide$sex[i5] == "male"){
          poisonM <- poisonM + Suicide$Freq[i5]
        }else{
          poisonW <- poisonW + Suicide$Freq[i5]
        }
      }
      else if(Suicide$method[i5] == "cookgas"){
        if(Suicide$sex[i5] == "male"){
          cookgasM <- cookgasM + Suicide$Freq[i5]
        }else{
          cookgasW <- cookgasW + Suicide$Freq[i5]
        }
      }
      else if(Suicide$method[i5] == "toxicgas"){
        if(Suicide$sex[i5] == "male"){
          toxicgasM <- toxicgasM + Suicide$Freq[i5]
        }else{
          toxicgasW <- toxicgasW + Suicide$Freq[i5]
        }
      }
      else if(Suicide$method[i5] == "other"){
        if(Suicide$sex[i5] == "male"){
          otherM <- otherM + Suicide$Freq[i5]
        }else{
          otherW <- otherW + Suicide$Freq[i5]
        }
      }
      else if(Suicide$method[i5] == "jump"){
        if(Suicide$sex[i5] == "male"){
          jumpM <- jumpM + Suicide$Freq[i5]
        }else{
          jumpW <- jumpW + Suicide$Freq[i5]
        }
      }
      else if(Suicide$method[i5] == "knife"){
        if(Suicide$sex[i5] == "male"){
          knifeM <- knifeM + Suicide$Freq[i5]
        }else{
          knifeW <- knifeW + Suicide$Freq[i5]
        }
      }
      else if(Suicide$method[i5] == "gun"){
        if(Suicide$sex[i5] == "male"){
          gunM <- gunM + Suicide$Freq[i5]
        }else{
          gunW <- gunW + Suicide$Freq[i5]
        }
      }
      else if(Suicide$method[i5] == "drown"){
        if(Suicide$sex[i5] == "male"){
          drownM <- drownM + Suicide$Freq[i5]
        }else{
          drownW <- drownW + Suicide$Freq[i5]
        }
      }
      else if(Suicide$method[i5] == "hang"){
        if(Suicide$sex[i5] == "male"){
          hangM <- hangM + Suicide$Freq[i5]
        }else{
          hangW <- hangW + Suicide$Freq[i5]
        }
      }else{
      }
    }else{
         }
    i5 <- i5 + 1
  }
  return(c(poisonM, cookgasM, toxicgasM, otherM, jumpM, knifeM, gunM, drownM, hangM, poisonW, cookgasW, toxicgasW, otherW, jumpW, knifeW, gunW, drownW, hangW))
}
zehn <- test(10,10, FALSE)
fuenfzehn <- test(15,15, FALSE)
zwanzig <- test(20,20, FALSE)
fuenfundzwanzig <- test(25,25, FALSE)
dreissig <- test(30,30, FALSE)
fuenfunddreissig <- test(35,35, FALSE)
vierzig <- test(40,40, FALSE)
fuenfundvierzig <- test(45,45, FALSE)
fuenfzig <- test(50,50, FALSE)
fuenfundfuenfzig <- test(55,55, FALSE)
sechzig <- test(60,60, FALSE)
fuenfundsechzig <- test(65,65, FALSE)
siebzig <- test(70,70, FALSE)
fuenfundsiebzig <- test(75,75, FALSE)
achtzig <- test(80,80, FALSE)
fuenfundachtzig <- test(85,85, FALSE)
neunzig <- test(90,90, FALSE)
sumfunciton <- function(param){
  temp <- 0
  for(inhalt in param){
    temp <- temp + inhalt
  }
  return(temp)
}
zehn <- sumfunciton(zehn)
fuenfzehn <- sumfunciton(fuenfzehn)
zwanzig <- sumfunciton(zwanzig)
fuenfundzwanzig <- sumfunciton(fuenfundzwanzig)
dreissig <- sumfunciton(dreissig)
fuenfunddreissig <- sumfunciton(fuenfunddreissig)
vierzig <- sumfunciton(vierzig)
fuenfundvierzig <- sumfunciton(fuenfundvierzig)
fuenfzig <- sumfunciton(fuenfzig)
fuenfundfuenfzig <- sumfunciton(fuenfundfuenfzig)
sechzig <- sumfunciton(sechzig)
fuenfundsechzig <- sumfunciton(fuenfundsechzig)
siebzig <- sumfunciton(siebzig)
fuenfundsiebzig <- sumfunciton(fuenfundsiebzig)
achtzig <- sumfunciton(achtzig)
fuenfundachtzig <- sumfunciton(fuenfundachtzig)
neunzig <- sumfunciton(neunzig)

all <- c(zehn, fuenfzehn, zwanzig, fuenfundzwanzig, dreissig, fuenfunddreissig, vierzig, fuenfundvierzig, fuenfzig, fuenfundfuenfzig, sechzig, fuenfundsechzig, siebzig, fuenfundsiebzig, achtzig, fuenfundachtzig, neunzig)