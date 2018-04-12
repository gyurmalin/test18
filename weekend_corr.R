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

nbo$gross_sales <- nbo$sold+nbo$accepted+nbo$still_recommendable

#10 days MA
mav <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)}
salesma <- mav(nbo$gross_sales, 10)
salesma <- na.omit(salesma)
plot(mav(salesma))

##maskepp
nbo$NAP <- as.Date(nbo$NAP)

nbo$roll10 = rollmean(nbo$gross_sales, 10, na.pad= TRUE)

ggplot(data=subset(nbo, !is.na(roll10)), aes(x = NAP, y = roll10)) + 
        geom_line(colour = "blue", size = 1.5, position = 'jitter') + 
        theme_bw() + scale_x_date(date_breaks="months", date_labels="%b %e") + 
        labs(title = "NBO performance - 10 days moving average", x = "Months", y = "Average daily sales",
             caption = "based on data from source system")

