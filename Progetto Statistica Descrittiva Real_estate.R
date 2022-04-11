# Task 1
data <- read.csv("realestate_texas.csv", sep=",")
attach(data)

# Task 3
# Distribuzione di frequenza
N <- dim(data)[1]
freq.ass.city <- table(city)
freq.rel.city <- table(city)/N
distr_freq_city <- cbind(freq.ass.city,freq.rel.city)

table(year)
table(month)

# Indici di posizione per esteso
# Max e min
min(sales)
max(sales)
# Mediana (indice robusto)
median(sales)
# Quantili
quantile(sales)
# Media aritmetica
mean(sales)

# Indici di posizione
summary(data)

# Indici di variabilit√† o dispersione
# Scarto interquartile
IQR(sales)
IQR(volume)
IQR(median_price)
IQR(listings)
IQR(months_inventory)

# Varianza per esteso
n <- length(sales)
mu = mean(sales)
sigma2 = sum((sales-mu)^2)/(n-1)

# Deviazione standard
sigma = sqrt(sigma2)

# Varianza con funzione diretta
var(volume)
var(median_price)
var(listings)
var(months_inventory)

# Deviazione standard con funzione diretta
sd(sales)
sd(volume)
sd(median_price)
sd(listings)
sd(months_inventory)

# Intervallo di variazione
variazione.numero_vendite <- max(sales)-min(sales)
variazione.valore_vendite <- max(volume)-min(volume)
variazione.prezzo_medio <- max(median_price)-min(median_price)
variazione.tempo_per_vendere <- max(months_inventory)-min(months_inventory)

# Task 4
# Il coefficiente di variazione
cv.sales <- (sigma/mu)*100
cv.volume <- (sd(volume)/mean(volume))*100
cv.median_price <- (sd(median_price)/mean(median_price))*100
cv.listings <- (sd(listings)/mean(listings))*100
cv.months_inventory <- (sd(months_inventory)/mean(months_inventory))*100

# Asimmetria
mu.sales <- mean(sales)
sigma.sales <- sd(sales)
n.sales <- length(sales)
m3.sales <- sum((sales-mu.sales)^3)/n.sales
asim.sales.index <- m3.sales/sigma.sales^3

mu.volume <- mean(volume)
sigma.volume <- sd(volume)
n.volume <- length(volume)
m3.volume <- sum((volume-mu.volume)^3)/n.volume
asim.volume.index <- m3.volume/sigma.volume^3

mu.median_price <- mean(median_price)
sigma.median_price <- sd(median_price)
n.median_price <- length(median_price)
m3.median_price <- sum((median_price-mu.median_price)^3)/n.median_price
asim.median_price.index <- m3.median_price/sigma.median_price^3

mu.listings <- mean(listings)
sigma.listings <- sd(listings)
n.listings <- length(listings)
m3.listings <- sum((listings-mu.listings)^3)/n.listings
asim.listings.index <- m3.listings/sigma.listings^3

mu.months_inventory <- mean(months_inventory)
sigma.months_inventory <- sd(months_inventory)
n.months_inventory <- length(months_inventory)
m3.months_inventory <- sum((months_inventory-mu.months_inventory)^3)/n.months_inventory
asim.months_inventory.index <- m3.months_inventory/sigma.months_inventory^3

# Task 5
# Suddivisione in classi
data$median_price_CL <- cut(data$median_price,
                            breaks = c(73799,100350,126900,153450,180000))

# Costruzione della distribuzione di frequenza
ni <- table(data$median_price_CL)
fi <- ni/N
Ni <- cumsum(ni)
Fi <- Ni/N

distr_freq_median_price_CL <- as.data.frame(cbind(ni,fi,Ni,Fi))

# Costruzione Grafico a Barre
    barplot(distr_freq_median_price_CL$ni,
            width = 1,
            main = "Distribuzione, per intervalli di prezzo, delle unit‡ abitative in texas dal 2010 al 2014",
            xlab = "Intervalli di prezzi considerati",
            ylab = "Numero di unit‡ abitative che rientrano nel range considerato",
            ylim = c(0,120),
            col= c(3,2,7,4),
            names.arg = c("(73799$-100350$]","(100350$-126900$]","(126900$-153450$]","(153450$-180000$]"))
      
# Indice di eterogeneit√† di Gini
gini.index <- function(x){
  ni=table(x)
  fi=ni/length(x)
  fi2=fi^2
  J = length(table(x))
  
  gini = 1-sum(fi2)
  gini.normal = gini/((J-1)/J)
  return(gini.normal)
}
        
gini.index(distr_freq_median_price_CL)

# Task 8
# Creazione colonna con Prezzo Medio in $
media <- function(x,y){
  z = (x*1000000)/y     # Conversione da Milioni di $ a $
  return(z)
}
data$prezzo.medio <- media(data$volume,data$sales)

# Task 9
# Creazione colonna con Stima delle vendite giornaliere al ritmo attuale
vend.giorn <- function(x,y){
  mese.giorni = (x *30.4167) # 30.4167= 365(giorni)/12(mesi)
  vendite.quotidiane = y/mese.giorni
  return(vendite.quotidiane)
}

data$vendite_giornliere <- vend.giorn(data$months_inventory,data$listings)

# Citt‡ con annunci pi˘ efficaci
data %>%
  group_by(city)%>%
  summarise(vendite_giornliere_somma = sum(vendite_giornliere))

# Task 10
install.packages("dplyr")
library(dplyr)

data.media.sd <- data %>%
  group_by(city,year,month)%>%
  summarise(media.mediana=mean(data$median_price),
            dev.st.mediana=sd(data$median_price),
            .groups = "keep")

# Task 10.1
library(ggplot2)
ggplot(data = data, aes(y=median_price,
                        fill=city,
                        x=city))+
  geom_boxplot()+
  labs(title = "Distribuzione prezzo mediano delle unit‡ abitative tra le citt‡ in Texas dal 2010 al 2014",
       x= "Citt‡",
      y ="Prezzo mediano di vendita in $",
      fill="Citt‡")+
  scale_y_continuous(breaks = seq(70000,180000,10000))
  
# Task 10.2
ggplot(data = data, aes(y=volume,
                        fill=city))+
  geom_boxplot()+
  labs(title = "Distribuzione del valore totale delle unit‡ immobiliari vendute tra le 4 citt‡ del Texas negli anni",
       y = "Volume totale delle vendite in Milioni di $",
       fill = "Citt‡")+
  facet_grid(~year)+
  scale_y_continuous(breaks = seq(0,90,5))+
  scale_x_continuous(breaks = NULL)
  
# Task 10.3 Grafico a barre sovrapposte
ggplot(data = data, aes(x = month,
                        y = sales,
                        fill = city))+
  geom_col()+
  labs(title = "Andamento delle vendite di unit‡ immobiliari tra 4 citt‡ del Texas, nei vari mesi degli anni",
       x= "Mesi",
       y="Unit‡ vendute",
       fill = "Citt‡ del Texas")+
  facet_grid(~year)+
  scale_x_continuous(breaks = seq(1,12,1))+
  scale_y_continuous(breaks = seq(0,1200,100))

# Task 10.3 Grafico a barre sovrapposte Normalizzato
ggplot(data = data, aes(x = month,
                        y = sales,
                        fill = city))+
  geom_col(position = "fill")+
  labs(title = "Andamento delle vendite di unit‡ immobiliari tra 4 citt‡ del Texas, nei vari mesi degli anni",
       x= "Mesi",
       y="Unit‡ vendute",
       fill = "Citt‡ del Texas")+
  facet_grid(~year)+
  scale_x_continuous(breaks = seq(1,12,1))+
  scale_y_continuous(breaks = seq(0,1,0.1))

# 10.4 Line Chart per tutte le citt√†, anni 2010 al 2014

median_price_tot<-data.frame(mesi_totali=c(1:60),
                             m_beamount_tot=c(163800,138200,122400,123200,123100,122800,124300,136800,121100,138500,150700,132500, #dataBeaumont$median_price
                                              130700,116700,120000,130000,120700,131200,132800,144600,123800,132100,111100,114000,
                                              110000,117500,129600,126200,134100,130800,127000,133600,118800,121800,134300,134700,
                                              126100,130000,121200,131400,134200,134100,128200,135000,147000,121100,140500,140000,
                                              106700,132500,129100,138300,134500,134500,142200,142400,128100,140600,124300,133800),
                             m_bryan_tot=c(151900,165300,148300,151000,150200,151700,152200,148500,149300,161400,159300,153300,    #dataBryan_College_Station$median_price
                                           147700,148500,152100,151200,148900,149400,148700,146700,156400,157300,156200,153900,
                                           140700,152000,154300,156600,153100,149300,154400,159700,170000,155200,149100,148400,
                                           155500,146900,161000,158200,161200,163700,161000,166100,156500,167300,156500,158800,
                                           155300,168500,155300,169500,165200,169600,172600,172200,180000,176100,172800,177300),
                             m_tyler_tot=c(138900,131500,130000,133200,142000,139400,135700,143100,132900,136000,130000,129400,    #dataTyler$median_price
                                           120600,128800,134200,135100,136100,145800,139200,144900,133300,133200,152600,130800,
                                           124200,134400,135700,129200,137600,148100,152100,144600,144000,136300,144800,140000,
                                           132400,144100,144800,142400,147400,155600,153100,147900,147600,155600,139600,142700,
                                           130700,150000,143600,145000,152300,155700,151500,155200,156500,144100,159400,161600),
                             m_whchita_tot=c(87200,89400,88600,105800,105200,119200,96700,96000,100000,100700,86400,112100,        #dataWichita_Falls$median_price
                                             90000,90800,100700,113600,109400,95000,102300,105200,102900,73800,91700,102300,
                                             82100,105000,97500,93000,97500,116500,116000,102300,108300,87000,118800,87500,
                                             99300,101400,85900,92200,121300,104700,102500,109100,114300,118200,100000,111100,
                                             91200,110000,94000,104700,115700,135300,102500,99600,90000,113300,108000,103800))


median_price_tot_plot <- ggplot(median_price_tot,aes(mesi_totali))+
  scale_x_continuous(breaks = seq(1,60,1))+
  scale_y_continuous(breaks = seq(70000,180000,10000))+
  geom_line(aes(y=m_beamount_tot,col="Beamount"),lwd=1)+
  geom_line(aes(y=m_bryan_tot,col="Bryan-College Station"),lwd=1)+
  geom_line(aes(y=m_tyler_tot,col="Tyler"),lwd=1)+
  geom_line(aes(y=m_whchita_tot,col="Wichita Falls"),lwd=1)+
  scale_color_manual("Citt√†",breaks = c("Beamount","Bryan-College Station","Tyler","Wichita Falls"),
                     values = c("red","blue","orange","pink"))+
  labs(title = "Andamento prezzo mediano unit√† abitative in 4 citt√† del Texas dal 2010 al 2014")+
  xlab("Mesi,da Gennaio 2010 a Dicembre 2014")+
  ylab("Prezzo mediano in $")+
  theme_light()
median_price_tot_plot 




