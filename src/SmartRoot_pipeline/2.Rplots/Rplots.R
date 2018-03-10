.
# Smart root script

## ICI renseigner le chemin vers le dossier de travail
rm(list=ls())
setwd("~/Proyectos/INRA/R work/My Proyects/SmartRoot_Git")
source("./src/SmartRoot_pipeline/2.Rplots/Rplots_auxiliarFunctions.R")

### -----ATTENTION: Parametres à rentrer à la main par l'utilisateur -----
# Particularités fichier CSV
SEPARATEUR = ";"
DECIMAL="."

## path to Rstructured file (input)
PATH_RSTRUCTURED_FOLDER <-  "/data/Rstructured_files"
ID_PLANTE <- "BMEC1_R001_A1"

## path to plots file (output)
dir.create(paste0(normalizePath(path="."),"/plots/", ID_PLANTE))
PATH_PLOTS_FOLDER <- paste0(normalizePath(path="."),"/plots/", ID_PLANTE)
##########################################################################


## Read data ##########################
path.data <- paste0(normalizePath(path="."),"/",PATH_RSTRUCTURED_FOLDER, "/",ID_PLANTE, "_Rstructured.csv")
df.plante <- read.table(path.data, sep=SEPARATEUR, dec=DECIMAL, h=T)
head(df.plante)
summary(df.plante)

## Data_type modification
str(df.plante)
df.plante$Img_date <- strptime(df.plante$Img_date, format="%Y-%m-%d")

#########################################
## Plot RootLengthTroncon_vs_Date
#########################################
# This plot allows to see the lengths of the roots as function of the date (plant age) 
# grouped by distance from the base (see df.plante$Troncon)

## plot LIMITS (specified by the USER)
MAX.ROOT.LENGTH <- 20
OUTPUT <- dates.plot.limits()
DATE.MIN <- OUTPUT[1]; DATE.MAX <- OUTPUT[2]

path.export <- paste0(PATH_PLOTS_FOLDER,"/",
                      ID_PLANTE,"_RootLengthTroncon_vs_Date.pdf")

## -----UNCOMMENT next line AND dev.off() line TO EXPORT PDF -----
 pdf(file=path.export, paper="a4r", width=11, height=9)

# par(mfrow=c(2,2))
 par(mfrow=c(1,1))

 plot.SR3.length.date()

 dev.off()  
# <--

 #########################################
 ## Plot RERTroncon_vs_RootAge
 #########################################
# This plot allows to see the RER of the roots as function of their age
# grouped by distance from the base (see df.plante$Troncon)


## plot LIMITS
MAX.ROOT.AGE <- 10
MAX.RER <- 4

path.export <- paste0(PATH_PLOTS_FOLDER,"/",
                      ID_PLANTE,"_RERTroncon_vs_RootAge.pdf")

## -----UNCOMMENT next line AND dev.off() line TO EXPORT PDF -----
 pdf(file=path.export, paper="a4r", width=11, height=9)

par(mfrow=c(2,2))

plot.SR4.RER.root.age()

 dev.off()

#########################################
## Plot LogRERTroncon_vs_RootAge
#########################################
# This plot allows to see the LOG RER of the roots as function of their age
# grouped by distance from the base (see df.plante$Troncon)

## plot LIMITS
MAX.ROOT.AGE <- 10
MAX.RER <-  1 # 

path.export <- paste0(PATH_PLOTS_FOLDER,"/",
                      ID_PLANTE,"_LogRERTroncon_vs_RootAge.pdf")

## -----UNCOMMENT next line AND dev.off() line TO EXPORT PDF -----
 pdf(file=path.export, paper="a4r", width=11, height=9)

par(mfrow=c(2,2))
plot.SR4b.log.RER.root.age()

 dev.off()

#########################################
## Plot ERTroncon_vs_RootAge
#########################################
# This plot allows to see the ER of the roots as function of their age
# grouped by distance from the base (see df.plante$Troncon)

## plot LIMITS
MAX.ROOT.AGE <- 12
MAX.ER <- 4

path.export <- paste0(PATH_PLOTS_FOLDER,"/",
                      ID_PLANTE,"_ERTroncon_vs_RootAge.pdf")

## -----UNCOMMENT next line AND dev.off() line TO EXPORT PDF -----
 pdf(file=path.export, paper="a4r", width=11, height=9)

par(mfrow=c(2,2))

plot.SR5.ER.root.age()

 dev.off()
# <--

######################################################
## Plot SystemeRacinaire_RootLength_vs_SeedDistance
######################################################
# Systeme racinaire chaque jour and ER median floating of Moy_v2_v3

# plot LIMITS
MIN.DIST <- 0 # min(df.plante$Dist_base, na.rm=T) # alternativement
MAX.DIST <- 60 # fen?tre maximale de suivie sur les rhizotrons

path.export <- paste0(PATH_PLOTS_FOLDER,"/",
                      ID_PLANTE,"_SystemeRacinaire_RootLength_vs_SeedDistance.pdf")

## -----UNCOMMENT next line AND dev.off() line TO EXPORT PDF -----
pdf(file=path.export, paper="a4r", width=11, height=9)

par(mfrow=c(1,1))

plot.SR6.Systeme.Racinaire()

dev.off()

######################################################
## Plot RootDiameterHistogram
######################################################
# --> Plot SR8  
# histo diameters

# Plot limits
BREAKS <- seq(0, 0.14, by=0.01)

path.export <- paste0(PATH_PLOTS_FOLDER,"/",
                      ID_PLANTE,"_RootDiameterHistogram.pdf")
## -----UNCOMMENT next line AND dev.off() line TO EXPORT PDF -----
pdf(file=path.export, paper="a4r", width=11, height=9)

plot.SR8.histogramme.diametres()

dev.off()


######################################################
## Plot ApicalRootDiameter_and_ER_vs_RootLength
######################################################
## graph ApicalDiameter vs. bLengh grouped by N roots

## plot LIMITS
MAX.API.DIAM <- 0.1 # max(df.plante$aDiam, na.rm=T)
MAX.LENGTH <- 20
MAX.ERdt <- 3 # max(df.plante$ER_dt, na.rm=T)
N <- 3 # number of roots

palette <- c("aquamarine3", "blue", "blueviolet", "dodgerblue", "navyblue", 
             "brown1", "brown3", "deeppink", "lightcoral", "indianred1", "mediumorchid3",
             "darkgoldenrod3", "darkorange", "orangered", "tan1",
             "darkolivegreen3", "forestgreen", "greenyellow", "mediumspringgreen" , "limegreen")

path.export <- paste0(PATH_PLOTS_FOLDER,"/",
                      ID_PLANTE,"_ApicalRootDiameter_and_ER_vs_RootLength.pdf")

## -----UNCOMMENT next line AND dev.off() line TO EXPORT PDF -----
 pdf(file=path.export, paper="a4r", width=11, height=9)

par(mfrow=c(1,1))

plot.SR10.diametres.api.bLength.by.N.roots()

dev.off()

######################################################
## Plot ApicalRootDiameter_and_ER_vs_RootAge
######################################################
# graph ApicalDiameter vs. Root age dec grouped by N roots

## plot LIMITS
MAX.API.DIAM <- 0.1 # max(df.plante$aDiam, na.rm=T)
MAX.AGE <- 12
MAX.ERdt <- 3 # max(df.plante$ER_dt, na.rm=T)
N <- 2 # number of roots

path.export <- paste0(PATH_PLOTS_FOLDER,"/",
                      ID_PLANTE,"_ApicalRootDiameter_and_ER_vs_RootAge.pdf")

## -----UNCOMMENT next line AND dev.off() line TO EXPORT PDF -----
pdf(file=path.export, paper="a4r", width=11, height=9)

par(mfrow=c(1,1))

plot.SR11.diametres.api.root.age.by.N.roots()

dev.off()


