library(dplyr)
library(haven)
library(labelled)

IRdata<-read_dta("IAIR7EFL.dta")
write.csv(IRdata, file = "IAIR7EFL.csv")

head(IRdata$v536)