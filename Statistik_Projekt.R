############################################# Header #############################################

  install.packages("MASS")
  install.packages("vcd")
  
  library(MASS)
  library(vcd)
  
  data()
  
  View(airquality)

############################################# part1 #############################################

  ozone <- airquality$Ozone
  
  solar <- airquality$Solar.R
    
  wind <- airquality$Wind
    
  temp <- airquality$Temp
    
  month <- airquality$Month
    
  day <- airquality$Day



  plot(solar, month, main = "test", pch = 4, cex = 0.8, col=c("blue"), col.axis="darkred", col.main="powderblue", col.lab="mistyrose", xlab="Sonnenstrahlung", ylab="Monat")
  
  boxplot(solar)
  
  
  plot(solar, temp)
  
  test <- table(temp, solar)




################################# Am Beispiel Solar #################################

#

  hist(solar, breaks = 100, col = "orange", main = "Histogramm", xlab = "Solar",  prob=F)

#

  relatve_Häufigkeiten <- round(prop.table(table(solar)),2)
  kumulierte_relative_Häufigkeiten <- round(cumsum(prop.table(table(solar))),2)

#

  absolute_Häufigkeiten <- table(table(solar))
  kumulierte_absolute_Häufigkeiten <- cumsum(table(table(solar)))

#

  (arithmetischeMittel <- mean(solar,na.rm=T))

#

  (median <- median(solar,na.rm=T))

#
  
  (quantile <- quantile(solar, probs=c(0.25,0.5,0.75),na.rm=T))

#

  (varianz <- var(solar,na.rm=T))

#

  (Standardabweichung <- sd(solar, na.rm=T))

#

  range <- range(solar, na.rm=T) # Minimum und Maximum
  Spannweite <- max(solar, na.rm=T) - min(solar, na.rm=T)

#

  (Interquartilsabstand <- IQR(solar,na.rm=T,type=6))

#

  (Korrelationskoeffizient <- cor(solar, month, method="pearson"))

#

  plot(solar, month, main = "Korrelation")

#

  summary(lm(solar ~ month))

#

  addmargins((table(month, solar)))

#

  plot(day[1:31], solar[1:31], type="b", main = "Sonnenstrahlung im Mai", pch = 4, cex = 0.8, col=c("blue"), xlab="Tag", ylab="Sonnenstrahlung" )

  abline(solar[1:31], day[1:31], col = "red", lwd = 2)

#

  (cor(solar[1:31], temp[1:31], method="pearson", use = "complete.obs"))


################################# 3D-Darstellung mehrerer Variablen #################################


  install.packages("scatterplot3d")
  library("scatterplot3d")
  
  scatterplot3d(solar[1:31], day[1:31], wind[1:31], pch = 20, highlight.3d = TRUE, type = "h", main = "3D Scatterplot of trees dataset")


################################# Vergleich mehrerer Variablen #################################


  plot(solar[1:31], type = "o", col = "red", ylab = "", main = "Vergleich Solar, Tag, Wind")
  lines(day[1:31], type = "o", col = "blue")
  lines(wind[1:31], type = "o", col = "green")
  legend(1, 110, legend = c("Solar", "Day", "Wind"), col = c("red", "blue", "green"), lty = 1:1, cex = 0.9)

