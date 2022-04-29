## Balkendiagramm der Gesamtanzahl aller Suizide abhaengig vom Geschlecht
barplot(c(anzahlSuizideM,anzahlSuizideW), main="absolute Zahl an Suiziden bei Maennern und Frauen", beside = TRUE, horiz = F, col = c("blue", "red"), ylim = c(0,35000), xpd=F, border = "white", space = c(1, 0.8), names.arg = c("Maenner", "Frauen"))
legend("topright", legend = c(anzahlSuizideM, anzahlSuizideW), fill =  c("blue", "red"))
## wichtige variabeln
(anzahlSuizideM)
(anzahlSuizideW)
## Tortendiagramm Anzahl aller Suizidarten (methods) unabhaengig vom Geschlecht
############################## übernehmen ############################
pie(prop.table(c(poison, cookgas, toxicgas, hang, drown, gun, knife, jump, other)), main="Suizidmethoden anteilig", col = c("blue", "red", "yellow", "green", "brown", "purple", "orange", "black", "darkred"), labels = c("poison", "cookgas", "toxicgas", "hang", "drown", "gun", "knife", "jump", "other"))
legend("left", legend = c(round((poison/53182)*100, 2), round((cookgas/53182)*100, 2), round((toxicgas/53182)*100, 2), round((hang/53182)*100, 2), round((drown/53182)*100, 2), round((gun/53182)*100, 2), round((knife/53182)*100, 2), round((jump/53182)*100, 2), round((other/53182)*100), 2), fill =  c("blue", "red", "yellow", "green", "brown", "purple", "orange", "black", "darkred"), title="in Prozent")
## funktionaler Zusammenhang zwischen Alter und Freq
## haeufigste Suizidart von Maennern und Frauen
 par(mfrow=c(1,2))
   pie(prop.table(rr), main="Suizidmethoden maennlich", col = rainbow(9), labels = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"))
   legend("topleft",cex = 0.9, legend = c(round((toxicgasM/53182)*100, 2), round((cookgasM/53182)*100, 2), round((poisonM/53182)*100, 2), round((hangM/53182)*100, 2), round((drownM/53182)*100, 2), round((gunM/53182)*100, 2), round((knifeM/53182)*100, 2), round((jumpM/53182)*100, 2), round((otherM/53182)*100), 2), fill =  rainbow(9))
   pie(prop.table(ww), main="Suizidmethoden weiblich", col = rainbow(9), labels = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"))
   legend("bottomleft", cex = 0.9, legend = c(round((toxicgasW/53182)*100, 2), round((drownW/53182)*100, 2), round((poisonW/53182)*100, 2),  round((hangW/53182)*100, 2), round((gunW/53182)*100, 2), round((otherW/53182)*100, 2), round((knifeW/53182)*100, 2), round((jumpW/53182)*100, 2), round((cookgasW/53182)*100, 2)), fill =  rainbow(9))
 par(mfrow=c(1,1))
## wie viele Menschen pro Altersgruppe brachten sich um? (Torten)
pie(prop.table(c(age1020freq,age2535freq,age4050freq,age5565freq,age7090freq)), main = "Das Mid-life-crisis-Phaenomen", col= rainbow(5), labels = c("10-20", "25-35", "40-50", "55-65", "70-90"))
legend("topleft", legend = c(round((age1020freq/53182)*100, 2), round((age2535freq/53182)*100, 2), round((age4050freq/53182)*100, 2), round((age5565freq/53182)*100, 2), round((age7090freq/53182)*100, 2)), fill = rainbow(5))
## wichtige Zahlen
(age1020freq)
(age2535fre)
(age4050freq)
(age5565freq)
(age7090freq)
plot(c(age1020freq,age2535freq,age4050freq,age5565freq,age7090freq), type="b", xaxt = "n", main = "Suizide je Altersgruppe", pch = 4, cex = 0.8, col=c("blue"), xlab="Altersgruppe", ylab="Anzahl Suizide")
axis(1 , at = seq(1,5, by=1), labels = c("10-20", "25-35", "40-50", "55-65", "70-90"))
## wichtige Zahlen
(age1020freq)
(age2535fre)
(age4050freq)
(age5565freq)
(age7090freq)
## Suizidart abhaengig von der Altersgruppe (Balken)
plot(all, type = "o", col = "red", , main="Suizide pro Altersgruppe")
## Zusammenhang Geschlecht und Methode
## In welchem Alter bringen sich Maenner und Frauen am meisten um?
plot(maxMen, main="maxMen")
plot(maxWomen, main="maxWomen")
##Mittelwert
mittelwert <- c/53182
######## Daten
      poi <- poi[2:length(poi)]
      (poi)
      Spannweite <- max(poi) - min(poi)
      (Interquartilsabstand <- IQR(poi,na.rm=T,type=6))
      (quantile <- quantile(poi, probs=c(0.25,0.5,0.75),na.rm=T))
######### child, middle, old, tooold
par(mfrow=c(1,2))
  pie(prop.table(ChildM), main="Child boys", labels = c("", "", "", "", "", "", "", "", ""), col = rainbow(9))
  legend("bottomleft", cex = 0.9, legend = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"), fill =  rainbow(9))
  pie(prop.table(ChildW), main="Child girls", col = rainbow(9), labels = c("", "", "", "", "", "", "", "", ""))
  legend("bottomleft", cex = 0.9, legend = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"), fill =  rainbow(9))
par(mfrow=c(1,1))
par(mfrow=c(1,2))
  pie(prop.table(MiddleM), main="Middle male", col = rainbow(9), labels = c("", "", "", "", "", "", "", "", ""))
  legend("bottomleft", cex = 0.9, legend = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"), fill =  rainbow(9))
  pie(prop.table(MiddleW), main="Middle female", col = rainbow(9), labels = c("", "", "", "", "", "", "", "", ""))
  legend("bottomleft", cex = 0.9, legend = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"), fill =  rainbow(9))
par(mfrow=c(1,1))
par(mfrow=c(1,2))
  pie(prop.table(MidlifeM), main="male in midlife", col = rainbow(9), labels = c("", "", "", "", "", "", "", "", ""))
  legend("bottomleft", cex = 0.9, legend = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"), fill =  rainbow(9))
  pie(prop.table(MidlifeW), main="female in midlife", col = rainbow(9), labels = c("", "", "", "", "", "", "", "", ""))
  legend("bottomleft", cex = 0.9, legend = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"), fill =  rainbow(9))
par(mfrow=c(1,1))
par(mfrow=c(1,2))
  pie(prop.table(OldM), main="old male", col = rainbow(9), labels = c("", "", "", "", "", "", "", "", ""))
  legend("bottomleft", cex = 0.9, legend = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"), fill =  rainbow(9))
  pie(prop.table(OldW), main="old female", col = rainbow(9), labels = c("", "", "", "", "", "", "", "", ""))
  legend("bottomleft", cex = 0.9, legend = c("poison", "cookgas", "toxicgas", "other", "jump", "knife","gun","drown","hang"), fill =  rainbow(9))
par(mfrow=c(1,1))