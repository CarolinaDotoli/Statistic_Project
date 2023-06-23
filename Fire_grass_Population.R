# R project - Biometria e Statistica/Statistical Analysis and Modelling
# Group: Maria Carolina Dotoli, Emanuele Mancini, Francesca Cipolletta 

# Descrizione Dataset: sono stati analizzati gli effetti di un incendio innescato da un fulmine, valutando successivamente
# la risposta di ricrescita degli organismi fotosintetici. Il dataset include i dati relativi al periodo immediatamente 
# successivo all'incendio (ultima settimana di luglio del 1998) e quelli relativi ai mesi di settembre e ottobre del 2001.
# Abbiamo scelto questo tipo di dataset perché ci interessava valutare la diversa risposta da parte degli organismi fotosintetici
# prima e dopo l'incendio.


# --- Installare i pacchetti necessari --- #

install.packages("tidyverse")
library(tidyverse)

install.packages("ggplot2")
library(ggplot2)

install.packages("DescTools")
library(DescTools)

install.packages("ggpubr")
library(ggpubr)

library(readr)


# Il dataset è stato preso da: https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-sev&identifier=172

# --- Caricare il set di dati --- #

fire_population <- read.csv("Data/sev172_firegrasspop_20140121.txt", header = T, sep = ",")

# La funzione header = T indica di mantere la prima riga come testata
# sep = "," indica di separare gli elementi in colonne, che sono divisi dalla virgola


# --- Controllo e descrizione variabili --- #
glimpse(fire_population) 
view(fire_population)
nrow(fire_population) # Numero di individui osservati tra il 1998 e il 2001: 15 763
ncol(fire_population) # Numero di variabili


# Descrizioni delle varaibili: 
# Nella prima colonna viene indicato l'anno relativo alle osservazioni (Year), in questo caso abbiamo due anni 1998 e 2001;
# La seconda colonna indica il sito di campionamento (Site), dove vengono suddivisi in due categorie: A= Dominato da Bouteloua eripoda (BOER),
# B = Dominato da Bouteloua gracilis (BOGR) ;
# La terza colonna riporta il tipo di trattamento adoperato sulla pianta (Treatment): B = vegetazione bruciata, C = controllo (vegetazione non bruciata), 
# P = vegetazione mista bruciata e non;
# La quarta colonna rappresenta il numero di zolla di terra campionato (Plot);
# La quinta colonna è l'altitudine (Location) divisa in 3 categorie: L = Bassa, 
# M = media, U = elevata ;
# Per quanto riguarda la sesta colonna abbiamo rappresentate le specie (Species), codificate in base all'USDA plants database;
# La settima e l'ottava colonna sono relative ai diametri di copertura vegetale (D1 e D2)
#D1 è il primo diametro misurato della zolla di terra, per calcolare la copertura basale, D2 è il secondo diametro misurato,
# perpendicolare a D1, della zolla di terra;
# L'ultima colonna indica la percentuale di foglie verdi, ovvero foglie vive, associate ad una zolla di terreno (%green).


# --- Pulizia e correzione set di dati --- #

# Controllo della colonna anno
unique(fire_population$Year)

# Trasformazione dell'anno in fattori ordinati in due livelli
fire_population$Year <- factor(fire_population$Year,
                               levels = c("1998", "2001"), 
                               ordered = T)

# Controllo della colonna del sito 
unique(fire_population$Site) # A = Dominato da Bouteloua eriopoda # U = Dominato da Bouteloua gracilis

# Trasformazione dei siti in fattori ordinati in 2 livelli
fire_population$Site <- factor(fire_population$Site, levels = c("A", "U"),
                               ordered = T)

# Controllo della location 
unique(fire_population$Location)

# Organizzazione della location in fattori ordinati in 3 livelli
fire_population$Location <- factor(fire_population$Location, 
                                   levels = c("L", "M", "U"), 
                                   ordered = T)

# Controllo tipologia di trattamento
unique(fire_population$Treatment) #B= Bruciato #C=controllo #P=Misto bruciato e non 

# Trasformazione dei trattamenti in fattori ordinati in 3 livelli
fire_population$Treatment <- factor(fire_population$Treatment,
                                    levels = c("B", "C", "P"),
                                    ordered = T)

# Controllo delle specie
unique(fire_population$Species) ##E'presente un errore, una specie viene ripetuta due volte 

# Correzione errore
fire_population$Species[fire_population$Species == "BOGR2     " ] <- "BOGR2"

# Organizzazione delle specie in fattori ordinati in 18 livelli
fire_population$Species <- factor(fire_population$Species,
                                  levels = c("BOGR2", "SPFL2", "BOER4",
                                             "PLJA", "ARPU9", "SPCR", "DAPU7", 
                                             "ARLO", "PANIC", "EPRU",
                                             "SPCE", "HIMU", "DEAD", "SPCO4", "BODA2" ,  "SCBR2", "POACE"))

# Contollo valori percentuale di verde
unique(fire_population$X.green) ##I valori dovrebbero essere divisi per 100 ma ci sono dei valori maggiori

# Analizziamo quali valori sono maggiori di 100
fire_population %>% filter(X.green > 100)

# Sono solamente 8 i valori maggiori di 100 pertanto possiamo considerarli un errore 

# Rimozoione delle righe conteneti i valori >100
fire_population <- fire_population %>% filter(X.green <= 100)


# --- Identificazione e rimozione dei valori mancanti (NA) ----

index_na <- which(is.na(fire_population), arr.ind = T)
index_na

# Rimozione valori NA
fire_population <- fire_population[-index_na, ]

# Controllo set di dati e della effettiva rimozione dei valori NA
summary(fire_population)                                             


# --- Analisi distribuzione individui nei diversi trattamenti ---#

# Creazione di una tabella di frequenza del numero di individui nei diversi
# trattamenti e classificati in base all'anno
table(fire_population$Year, fire_population$Treatment)

# Risulta già evidente che ci sia stato un aumento del numero di individui 
# Nei siti totalmente bruciati e quelli non bruciati, mentre quelli che hanno subito
# una parziale bruciatura hanno subito una riduzione 

# Calcolo della distribuzione di frequenza in percentuale 
perc_table <- prop.table(table(fire_population$Treatment, fire_population$Year))*100
print(perc_table)
# La funzione prop.table permette di ottenere le frequenze relative


# --- Rappresentazione grafica 1 ---- #

# Rappresentazione grafica distribuzione univariata della distribuzione di frequenza
# in percentuale delle piante nei diversi siti 
barplot(t(perc_table), beside=TRUE, legend=TRUE, ylim=c(0,30), col=c( "yellow", 
                                                                              "skyblue"), ylab="Frequenze percentuali", xlab="Trattamento effettuato", 
                                                                              main="Distribuzione percentuale degli individui nei diversi trattamenti")
# La funzione t permette di ruotare la matrice di dati

# Dal grafico si evince che nei siti che sono stati bruciati si è avuto un aumento consistente nel numero 
# di individui a conferma dell'ipotesi del disturbo intermedio, in maniera analoga anche nel 
# sito di controllo, seppur con un minor effetto. Le piante che hanno subito il trattamento P
# invece sono state fortemente disturbate e hanno subito una riduzione 

# Esportiamo il grafico
jpeg(file="grafico1.jpeg", width = 900, height = 900)
barplot(t(perc_table), beside=TRUE, legend=TRUE, ylim=c(0,30), col=c( "yellow", 
                                                                              "skyblue"), ylab="Frequenze osservate", xlab="Trattamento effettuato", 
                                                                              main="Distribuzione percentuale degli individui nei diversi trattamenti")
dev.off()


# --- Applicazione test ---- #

#Vogliamo analizzare se vi sia una correlazione tra il trattamento subito ed il numero degli individui 
#nei due anni, abbiamo dunque deciso di usare il test del chi-quadro 
# Formulazione ipotesi:
# Ho = non vi è correlazione tra il numero di individui ed il trattamento  
# H1 = vi è correlazione tra le due variabili

result_chitest <- chisq.test(table(fire_population$Treatment, fire_population$Year))
result_chitest

#  Il p-value risulta significativamente basso < 0.05, pertanto è possibile rifutare l'ipotesi nulla (Ho), dunque 
#il trattamento effettuato sui diversi plot influenza il numero di individui 


#--- Analisi della distribuzione di specie nei diversi trattamenti ---- #

# Creazione di un data frame con le frequenze per ogni categoria 
species_distribution <- as.data.frame(table(fire_population$Treatment,
                                   fire_population$Species, fire_population$Year))
print(species_distribution)


# --- Rappresentazione grafica 2 --- #

# Creazione di grafico a barre sovrapposte della distribuzione di frequenza delle diverse specie 
ggplot(species_distribution, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Var3) +
  labs(x = "Trattamento", y = "Frequenza", fill = "Specie") +
  theme_minimal() + scale_fill_manual(values = c("green", 
                                                        "aquamarine3", "coral4", 
                                                        "darkolivegreen4", "deeppink", "mediumblue",
                                                        "olivedrab1", "rosybrown4", "seagreen", "plum", 
                                                        "turquoise1", "yellow", "pink", "palegreen3", 
                                                        "lightcoral", "royalblue4", "magenta3")) + coord_flip() 
                                                        
# Risulta già evidente come nel sito B le piante hanno subito non solo un aumento del numero 
# di individui, ma anche un aumento di specie; a conferma dell'ipotesi del disturbo intermedio.
# Anche nel trattamento C si ha avuto un aumento del numero di specie. 

# Esportiamo il grafico 
ggsave("grafico2.jpeg")
dev.off()


# --- Analisi distribuzione delle specie nei diversi trattamenti --- #

# Creazione tabella distribuzione di frequenza delle diverse specie nei due anni
table(fire_population$Species, fire_population$Year, fire_population$Treatment)

# Dalla tabella invece si vede come il trattamento P, ovvero quello che ha subito un 
# distubro maggiore (competizione e incendio) ha subito anche una diminuzione del numero di specie
# Inoltre la tabella conferma anche quanto osservato nel grafico precedente, ovvero un aumento del numero 
# di specie nei primi due siti


# --- Rappresentazione grafica 3 --- #

# Analisi della percentuale di verde nei diversi trattamenti 
# Creazione grafico della densità di individui in funzione della loro percentuale di verde
# e suddivisi in base al trattamento e l'anno
ggplot(fire_population, aes(x = X.green, fill = Treatment)) +
  geom_density(alpha = 0.5) +
  facet_grid(Treatment ~ Year) +
  labs(x = "Percentuale di verde", y = "Densità", fill = "Trattamento") + xlim (0,120)

# È evidente come le piante nel trattamento B abbiano accresciuto la loro variabilità 
# nella percentuale di verde, in quanto nel 1998 si ha una distribuzione che presenta 
# un maggior numero di picchi legati a delle specie che presentano una classe di verde 
# ben precisa. Nelle piante del trattamento C invece si ha una maggiore variabilità delle
# classi di verde (come in B), ma vi è una notevole diminuzione di individui con precentuali 
# di verde più elevate. Infine in P si è passati da una distribuzione bimodale legato a due 
# specie dominanti, ad una distribuzione unimodale con un crollo del secondo picco legato 
# ad una percentuale di verde più elevata. 

# Esportazione del grafico 
ggsave("grafico3.jpeg")
dev.off()


# --- Rappresentazione grafica 4 --- #

# Creazione di un boxplot della percentuale di verde nei diversi trattamenti
ggplot(data = fire_population, aes(x = Treatment, y = X.green)) + 
  geom_boxplot( ) +
  labs(title = "Boxplot della percentuale di verde nei diversi trattamenti",
       x = "Trattamenti", 
       y = "Percentuale di verde") + facet_wrap(~ Year)

# Dal boxplot risulta evidente che vi è stata una riduzione della percentuale di verde
# in tutte e tre le tipologie di trattamento, tuttavia vi è minor differenza tra i valori 
# del trattamento B piuttosto cche quelle del trattamento C che ha subito una maggior diminuzione
# Trattamento B nel 1998: 
# μe= 50, Q1= 28 circa, Q3 =75; 
# Trattamento B nel 2001: 
# μe= 40 circa, Q1 = 25, Q3= 70 circa.
# Le piante del trattamento C sono quelle che hanno subito una riduzione di valori di percentuale di verde
# maggiori; 
# Trattamento C nel 1998: 
# μe= 60 circa, Q1= 50, Q3 = <82; 
# Trattamento C nel 2001:
# μe= 35 circa, Q1 = 25, Q3 = 60 circa.
# Trattamento P nel 1998: 
# μe = 60 circa, Q1= 35 circa, Q3= 78 circa; 
# Trattamento P nel 2001: 
# μe = 30 circa, Q1= 20 circa, Q3 = 55 circa. 

# Esportiamo l'immagine 
ggsave("grafico4.jpeg")
dev.off()

#Infine dopo aver visto che il numero di specie ed individui sia aumentato nei
#trattamenti B e C, tuttavia la percentuale di foglie vive associate ad ogni zolla di terreno
#nei due trattamenti sia diminuita, abbiamo comunque riscontrato una diminuzione minore 
#di percentuale di verde nelle piante appartenenti al trattamento B, ovvero che sono state bruciate, 
#come conferma l'ipotesi del disturbo intermedio. 
#Analizziamo ora come però sia variata la dimensione delle zolle di terra e come questo 
# abbia influenzato il numero di individui all'interno della popolazione 


#Avendo quindi i due diametri D1 e D2, calcoliamo la superficie di ogni zolla di terreno 
#Sapendo che D2 è perpendicolare a D1,calcoliamo la superficie moltiplicando D1/2 per D2/2 per pi greco
fire_population$SurfaceArea <- pi * (fire_population$D1/2) * (fire_population$D2/2)
fire_population$SurfaceArea


### --- Analisi della superficie della zolla di terreno nei diversi anni e trattamenti 

#Creazione di un dataset per il trattamento B, nei due diversi anni 
#Creazione dataframe trattamento B del 1998

#Filtriamo i dati delle piante che hanno subito il trattamento B 
popB <- filter(fire_population, Treatment == "B")

#Creiamo il data frame della popB riferita al 1998 
popB_98 <- filter(popB, Year == "1998")

#Poniamo in ordine crescente i valori della superfice
popB_98 <- popB_98[order(popB_98$SurfaceArea), ]

#Creiamo una nuova colonna in cui inseriamo la frequenza cumulata dell'area

popB_98 <- cbind(popB_98, Area_cumulata = cumsum( popB_98$SurfaceArea))

#Creiamo una nuova colonna contenente il numero di individui crescente 
popB_98 <- popB_98%>% mutate (Plant_number = row_number())

#Ripetiamo la medesima operazione per l'anno 2001
popB_01 <- filter(popB, Year == "2001")
popB_01 <- popB_01[order(popB_01$SurfaceArea),]
popB_01 <- cbind(popB_01, Area_cumulata = cumsum (popB_01$SurfaceArea))
popB_01 <- popB_01%>% mutate (Plant_number = row_number())


#Creazione scatter plot del numero di individui in relazione all'area occupata 
#Suddiviso per i due anni 
scatterplot_B <- ggplot() + geom_point(data = popB_98, aes(x = Area_cumulata, y = Plant_number, color="brown1")) +
  geom_point(data = popB_01, aes(x= Area_cumulata, y = Plant_number, color ="blue1")) +
  labs( x ="Superficie", y="Numero individui", colour = "Anno") + 
  scale_color_manual(labels = c("2001", "1998"), values = c("brown1", "blue1")) +
  ggtitle("Relazione tra il numero di individui e la superficie occupata nel trattamento B")


#Attuiamo lo stesso processo per il trattamento C
popC <- filter(fire_population, Treatment == "C")

#1998
popC_98 <- filter(popC, Year == "1998")
popC_98 <- popC_98[order(popC_98$SurfaceArea),]
popC_98 <- cbind(popC_98, Area_cumulata = cumsum (popC_98$SurfaceArea))
popC_98 <- popC_98%>% mutate (Plant_number = row_number())

#2001
popC_01 <- filter(popC, Year == "2001")
popC_01 <- popC_01[order(popC_01$SurfaceArea),]
popC_01 <- cbind(popC_01, Area_cumulata = cumsum (popC_01$SurfaceArea))
popC_01 <- popC_01%>% mutate (Plant_number = row_number())

#Creazione del grafico per il trattamento C
scatterplot_C <- ggplot() + geom_point(data = popC_98, aes(x = Area_cumulata, y = Plant_number, color="brown1")) +
  geom_point(data = popC_01, aes(x= Area_cumulata, y = Plant_number, color ="blue1")) +
  labs( x ="Superficie", y="Numero individui", colour = "Anno") + 
  scale_color_manual(labels = c("2001", "1998"), values = c("brown1", "blue1")) +
  ggtitle("Relazione tra il numero di individui e la superficie occupata nel trattamento C")



#Ripetiamo l'operazione per l'ultimo trattamento P
popP <- filter(fire_population, Treatment == "P")

#1998 
popP_98 <- filter(popP, Year == "1998")
popP_98 <- popP_98[order(popP_98$SurfaceArea),]
popP_98 <- cbind(popP_98, Area_cumulata = cumsum (popP_98$SurfaceArea))
popP_98 <- popP_98%>% mutate (Plant_number = row_number())


#2001
popP_01 <- filter(popP, Year == "2001")
popP_01 <- popP_01[order(popP_01$SurfaceArea),]
popP_01 <- cbind(popP_01, Area_cumulata = cumsum (popP_01$SurfaceArea))
popP_01 <- popP_01%>% mutate (Plant_number = row_number())


#Creazione grafico per il trattamento P
scatterplot_P <- ggplot() + geom_point(data = popP_98, aes(x = Area_cumulata, y = Plant_number, color="brown1")) +
  geom_point(data = popP_01, aes(x= Area_cumulata, y = Plant_number, color ="blue1")) +
  labs( x ="Superficie", y="Numero individui", colour = "Anno") + 
  scale_color_manual(labels = c("2001", "1998"), values = c("brown1", "blue1")) +
  ggtitle("Relazione tra il numero di individui e la superficie occupata nel trattamento P")


# Mettiamo a confronto i tre grafici risultanti
ggarrange(scatterplot_B, scatterplot_C, scatterplot_P, ncol = 1, nrow = 3)

#Esportiamo il grafico
png("outputs/Figure5.png", res = 300, width = 3000, heigh = 2000)



# Dunque è possibile concludere che l'ipotesi del disturbo intermedio è verificata, infatti
# gli individui all'interno dei trattamenti di bruciatura sono aumentati in modo cospicuo sia in 
# sia in numero di specie ma anche in numero di individui. Questo aumento è stato anche maggiore rispetto
# alle piante presenti nel trattamento di controllo. Le piante invece che hanno subito un trattamento misto 
# probabilmente a causa di troppi disturbi (come ad esempio incendio e competizione) hanno subito un 
# drastico crollo, sopratutto in numero di specie. Bisogna però tener presente che tutte le piante hanno
# subito una riduzione della percentuale di verde in tutti e tre i trattamenti, tuttavia questa riduzione 
# è stata di minore entità sulle piante che sono state bruciate. 
# Quindi si può concludere che un disturbo di media entità, può non solo aumentare il numero di specie e dunque 
# la biodiversità all'interno dell'ambiente ma ha anche un minor impatto sullo stato di salute delle piante 
# (percentuale di foglie verdi, che rappresenta proprio le foglie vive) della competizione che si instaura 
# all'interno di quello stesso habitat. Tuttavia nei diversi anni la dimensione dell'area della zolle di terreno relative alle specie 
# sono diminuite, e le piante presenti nel trattamento B hanno avuto una riduzione maggiore rispetto a quelle a quelle del 
# trattamento C, dunque hanno diminuito la loro estensione massima.
# Possiamo quindi concludere che l'ipotesi del disturbo intermedio è stata confermata, tuttavia 
# questa non si rispecchia nella dimensione degli individui (estensione in superficie della zolla di terra)
# i quali sono decresciuti maggiormente.

