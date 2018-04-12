library(install.load)
install_load("gdata","dplyr","xlsx","ggplot2", "zoo")


nbo <- read.xls("data.xlsx", sheet = 3, header = TRUE) 


nbo$NAPSZAM <- as.POSIXlt(nbo$NAP)$wday # 0: sunday, 1: monday, 2: tuesday stb.

nbo$IDOSZAK <- ifelse(nbo$NAPSZAM > 5 | nbo$NAPSZAM == 0, "hetvege", "hetkoznap")

for (i in 1:nrow(nbo)) {
        if(nbo$NAPSZAM[i] == 6) {
                nbo$sold[i-1] = nbo$sold[i-1] + nbo$sold[i]
                nbo$accepted[i-1] = nbo$accepted[i-1] + nbo$accepted[i]
                nbo$still_recommendable[i-1] = nbo$still_recommendable[i-1] + nbo$still_recommendable[i]
                nbo$refused[i-1] = nbo$refused[i-1] + nbo$refused[i]
        } 
        if (nbo$NAPSZAM[i] == 0) {
                nbo$sold[i-2] = nbo$sold[i-2] + nbo$sold[i]
                nbo$accepted[i-2] = nbo$accepted[i-2] + nbo$accepted[i]
                nbo$still_recommendable[i-2] = nbo$still_recommendable[i-2] + nbo$still_recommendable[i]
                nbo$refused[i-2] = nbo$refused[i-2] + nbo$refused[i]
        } 
}


nbo <- nbo[ which(nbo$IDOSZAK=='hetkoznap'), ]
nbo
###
