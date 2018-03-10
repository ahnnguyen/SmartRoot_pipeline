
# Smart Root script
###############################################################################
# @ author: Beatriz Moreno

rm(list=ls())

## ICI renseigner le chemin vers le dossier de travail
setwd("~/Proyectos/INRA/R work/My Proyects/SmartRoot_Git")
source("./src/SmartRoot_pipeline/1.Rstructuration/SRfiles_to_Rstructured_auxiliarFunctions.R")
# library("gplots")
# library("base64")

### -----ATTENTION: Parametres à rentrer à la main par l'utilisateur -----

# Particularités fichier CSV
SEPARATEUR = ";"
DECIMAL="."

# Identifiants plante
ID_MANIPE <- "BMEC1"
ID_RHIZOTRON <- "R001"
ID_PLANTE <- "BMEC1_R001_A1"


# Chemin des fichiers global et nodes obtenus de SmartRoot
PATH_GLOBAL <- "/data/SR_files/BMEC1_R001_global2.csv"
PATH_NODES <- "/data/SR_files/BMEC1_R001_nodes2.csv"

# Chemin du fichier contenant les heures de acquisitions de scans
PATH_DATES <- "/data/Auxiliary_files/GFBM_Horaraires_scan_corrected_BMEC1_Jsemis.csv"

### -----------------------------------------------------------------------

# ----Set INPUT paths
# path for the SmartRoot file "global"
path <- paste0(normalizePath(path="."),PATH_GLOBAL)

# path2 for the SmartRoot file "nodes"
path2 <- paste0(normalizePath(path="."),PATH_NODES)

# path.dates for the file containing the time at which scans have been taken every day (manually specified) 
# that allows to take account for the differences in the scanning time between days
path.dates  <- paste0(normalizePath(path="."),PATH_DATES)

# ----Set OUTPUT paths
# path.Rstruct for destination folder of "Rstructured" files
path.Rstruct <- paste0(normalizePath(path="."),"/data/Rstructured_files/")

# path.Sum for destination folder of "Summary" files
path.Sum <- paste0(normalizePath(path="."),"/data/Summary_files/")

# ---- Read data
df.plante <- read.table(path, sep=SEPARATEUR, dec=DECIMAL, h=T)
df.nodes <- read.table(path2, sep=SEPARATEUR, dec=DECIMAL, h=T)
df.dates <- read.table(path.dates, sep=SEPARATEUR, dec=",", h=T)  ## Specifier separateur fichier dates si autre

head(df.plante)
head(df.nodes)
head(df.dates)

str(df.plante)
str(df.nodes)
str(df.dates)

dim(df.plante)

# Correct data types
df.plante$Img <- factor(df.plante$Img)
df.plante$Root <- factor(df.plante$Root)
df.plante$rootOrder <- factor(df.plante$rootOrder)

df.dates$Minute_start <- as.numeric(df.dates$Minute_start)
df.dates$Jan <- as.numeric(df.dates$Jan)
df.dates$Jsemis <- as.numeric(df.dates$Jsemis)

# head(df.nodes)
# str(df.nodes)

## Comparison between df.plante et df.nodes
# balanced.data(df.plante, df.nodes)

## If any problems with different number of levels, TRY: 
## force levels to be the same
df.nodes$Root <- factor(df.nodes$Root , levels = levels(df.plante$Root))
df.nodes$Img <- factor(df.nodes$Img , levels = levels(df.plante$Img))

## Comparison between df.plante et df.nodes
# balanced.data(df.plante, df.nodes)

#######################################################################################
# New variables 
#######################################################################################

### ID_manipe, for the experiment identifiant
df.plante$ID_manipe <- as.factor(ID_MANIPE)

### PlantID, for the plant identifiant
df.plante$PlantID <- as.factor(ID_PLANTE)

### Root_LPos, for a new order of lateral based on LPosParent
df.plante <- root.labels.by.position (df.plante)
try(df.plante <- df.plante[order(df.plante$Root_LPos),], silent=T)
# head(df.plante)

### Img_date, for the date of the source image
### Rhizo, for the rhizotron number
df.plante <- info.from.image.names (df.plante)
df.plante$Rhizo  <- factor(ID_RHIZOTRON)

# head(df.plante)
# str(df.plante)

### Jdec, for date conversions to a day of the year and its decimal part
### Jdec_trunc, for day of the year without the decimal fraction (type int)
df.plante <- date.conversions (df.plante, df.dates)
# str(df.plante)

### Yins, for the Y coordinate of the insertion node of each root
### Dist_base, manually calculated, corresponding to LPosParent
df.plante <- distance.calculations(df.plante, df.nodes)
# str(df.plante)
# head(df.plante)

### Date_birth, for the apparition date of each root
### Date_birth_dec, for the apparition date of each root
### Root_age, for the age of each root
### Root_age_dec, for the age of each root in day of the year + decimal fraction
df.plante <- age.calculations (df.plante, df.dates)
# head(df.plante)
# summary(df.plante)

#### Now you can order each root in your data.frame by time
df.plante <- df.plante[order(df.plante$Jdec), ]
df.plante <- df.plante[order(df.plante$Root_LPos), ]
# head(df.plante)
  
### Troncon, for groups of 5 cm #############################
# T5 for the laterals in the [0,5) first cm from the base
# T10 for the laterals in the [5,10) cm interval from the base
# T15 for the laterals in the [10,15) cm interval from the base
# T20 for the laterals in the [15,20) cm interval from the base
# T25 for the laterals in the [20,25) cm interval from the base...
# et ainsi suite
df.plante <- tronconage (df.plante)

# To see the number of roots in your groups
# length(levels(factor(df.plante[df.plante$Troncon == "T40",]$Root)))

### Calcul ER_dt, RER_dt ###################################
## ER(i) = [Length(i)- Length(i-1)] / [date(i) - date(i-1)]
## RER(i) = ln [Length(i) / Length(i-1)] / [date(i) - date(i-1)]
## calcul in base of Jdec !! --> ER_dt

df.plante <- growth.rates(df.plante)
# summary(df.plante)
# head(df.plante)

############### Architectural data on last date ##########

## r, PARAMETER TO BE FIXED
R <- 10  # number of roots at each side of the filter
nLR_fixed <- 1+2*R # ...NOTE that local_LR_density will be OVERESTIMATED like that
nLR_fixed_corrected <- 2*R # we drop one root as we dont take into account spacings next to 

 ### Moy_v2_v3
 ### Local_ER_median_v2_v3
 ### Local_ER_moyenne_v2_v3
 
df.plante <- local.ER.curves(df.plante, r=R)
df.plante <- architectural.data(df.plante, r=R)

# head(df.plante)
# summary(df.plante)



### Diameters 
# bLength_bDiam, for Distance from base to basal node
# bDiam, for Diameter of basal.node
# aDiam, for  Diameter of apical.node
# aLength_aDiam, for Distance from apex to apical node
# bLength_aDiam, for Distance from base to apical node

df.plante <- diametres(df.plante) # can take a few minutes
# head(df.plante)
# str(df.plante)
# summary(df.plante)

# ################### Tableau recapitulatif par plante
tab <- tab.recapitulatif (df.plante,plantID=ID_PLANTE)
tab

# ######## EXPORT clean #########################################################
df.plante <- df.plante[order(df.plante$Jdec),]
df.plante <- df.plante[order(df.plante$Root),]
df.plante <- df.plante[order(df.plante$Yins, decreasing=T),]

# head(df.plante)
# summary(df.plante)

## substitution de ".csv" par Rstructured dans le path source du fichier
#########################################################################

## Rstructured files
path.export <- paste0(path.Rstruct,ID_PLANTE,"_Rstructured.csv")
write.table(df.plante, path.export, sep=";", dec=".")

## Summary files
path.summary <- paste0(path.Sum,ID_PLANTE,"_summary.csv")
write.table(tab, path.summary, sep=";", dec=".")





