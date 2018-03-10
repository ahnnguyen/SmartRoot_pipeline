# 
# find.plants <- function()
# {
#   attach(maize, warn.conflicts=F)
#   # split in 2 
#   maize.1 <- maize[Number_of_plants==1,]
#   maize.2 <- maize[Number_of_plants==2,] 
#   
#   # PlantID.all contains the PlantID of all oyur plants
#   PlantID.all <- c(levels(factor(maize.1$PlantID.1)),
#                    levels(factor(maize.2$PlantID.1)),levels(factor(maize.2$PlantID.2)))
#   
#   if(exists(my.PlantID))
#   {
#     cat("\nYOUR CURRENT CHOICE:\n","my.PlantID\n")
#     return(my.PlantID)
#   }else{
#     cat("\nYOUR CURRENT CHOICE:\n","NONE\n")
#   }
#  cat("YOU HAVE", sum(Number_of_plants),"plants\n" ,"PlantID.all\n")
#  return(PlantID.all)
# }
# 
# find.paths <- function()
# {
#   # path corresponding to global root data file
#   ifelse(any(PlantID.1==my.PlantID),
#          path <- maize[PlantID.1==my.PlantID,]$Global_path,
#          path <- maize[PlantID.2==my.PlantID,]$Global_path)
#   path <- as.character(path)
#   # path2 corresponding to nodes data file
#   ifelse(any(PlantID.1==my.PlantID),
#          path2 <- maize[PlantID.1==my.PlantID,]$Nodes_path,
#          path2 <- maize[PlantID.2==my.PlantID,]$Nodes_path)
#   path2 <- as.character(path2)
#   my.line <- which(maize$Global_path==path)
#   separateur <- as.character(Sep[my.line])
#   decimal <- as.character(Dec[my.line])
#   
#   return(list(path, path2, separateur, decimal, my.line))
# }


# check.number.of.plants <- function(df.plante)
# {
#   if(length(levels(df.plante$path))<=3)
#   {cat("IT'S OK: YOU HAVE ONLY ONE PLANT ON YOUR FILE!")
#   }else
#   {
#     IDplant.1 <- levels(df.plante$path)[length(levels(df.plante$path))-1]
#     IDplant.2 <- levels(df.plante$path)[length(levels(df.plante$path))]
#     cat("YOU HAVE MORE THAN ONE PLANT ON YOUR FILE!",
#         "\nYOUR CURRENT PLANT IS: ", my.PlantID,
#         "\nSELECT YOUR PLANT AGAIN:\n")
#     c(IDplant.1, IDplant.2)
#   }
# }
# 
# select.plant <- function(plant)
# {
#   # include primary root
#   df.selected.plant <- rbind(df.plante[df.plante$path==plant,], df.plante[df.plante$Root==plant,])
#   df.selected.nodes <- rbind(df.nodes[df.nodes$parentRoot==plant,],df.nodes[df.nodes$Root==plant,] )
#   
#   par(mfrow=c(1,1))
#   cat("OLD DIMENSIONS:\n", 
#       "df.plante ", dim(df.plante), "df.nodes ", dim(df.nodes),
#       "\nNEW DIMENSIONS:\n",
#       "df.plante ", dim(df.selected.plant), "df.nodes ", dim(df.selected.nodes))
#   # texplot?
#   return(list(df.selected.plant, df.selected.nodes))
# }

balanced.data <- function(df.plante, df.nodes)
{
a <- length(levels(df.plante$Root)) == length(levels(df.nodes$Root))
b <- length(levels(df.plante$Img))== length(levels(df.nodes$Img))

# Tranfo possible lorque a est b sont TRUE
if (!a || !b)
{
  if(!a)
  {
    cat("Le nombre racines ne coincide pas dans les fichers global et nodes")
  }
  if(!b)
  {
    cat("\nLe nombre jours ne coincide pas dans les fichers global et nodes")
  }
}else
{ 
  cat("CONGRATS! Le nombre jours coincide dans les fichers global et nodes")
}
}

##################################################################################

root.labels.by.position <- function(df.plante)
{
  dist_base <- try(tapply(df.plante$LPosParent, df.plante$Root, mean), silent=T)
  # if(exists(dist_base))
  # {
  lats <- (names(dist_base))
  tab <- as.data.frame(cbind(dist_base, lats))
  # dim(tab)
  colnames(tab) <-c("LPosParent", "Root")
  # str(tab)
  tab$LPosParent <- as.numeric(as.character(tab$LPosParent))
  tab$Root <- as.factor(tab$Root)
  tab <- tab[order(tab$LPosParent),]
  tab$Root_LPos <- paste(" latdist_", seq(from=0, length.out=length(tab$Root)), sep="")
  tab[tab$Root_LPos==" latdist_0", ]$Root_LPos <- " taproot_0"
  tab$Root_LPos <- factor(tab$Root_LPos, levels=tab$Root_LPos)
  
  # Integration in df.plante
  df.plante$Root_LPos <- tab$Root_LPos[match(df.plante$Root,tab$Root)]
  df.plante$Root_LPos <- factor(df.plante$Root_LPos, levels=levels(tab$Root_LPos))
  # }
  return(df.plante)
}

##################################################################################

info.from.image.names <- function(df.plante)
{
  
if (df.plante$ID_manipe[1] == "GFBM1")
{
  # [GFBM1] FOR B73H IMAGES ############################
  ## Img_date, for the date of the source image
  Img_date <- substr(df.plante$Img, 18, 21)
  Img_date <- gsub("_", "", Img_date)
  ## Add the year
  Img_date <- paste(Img_date, "-2013", sep="")
  ## Add to the dataset
  df.plante$Img_date <- strptime(Img_date, format="%d-%m-%Y")
  
  ### Img_date_as_factor
  df.plante$Img_date_factor <- factor(as.character(df.plante$Img_date))
  
  ### Rhizo, for the rhizotron number
  # MAGIC FORMULA ;D
  rhizo <- as.numeric(gsub('.*_([0-9]+).*','\\1',df.plante$Img))
  if (rhizo[1] < 10)
  {
    df.plante$Rhizo <- as.factor(paste("R0", rhizo, sep=""))
  }else
  {
    df.plante$Rhizo <- as.factor(paste("R", rhizo, sep=""))
  }
}

if (df.plante$ID_manipe[1] == "GFBM2")
{
  # [GFBM2] FOR mutant IMAGES ##########################
  ### Rhizo, for the rhizotron number
  rhizo <- paste("R", substr(sub(".*R", "", df.plante$Img), 1, 2), sep="")[1]

  root.Img <- sub("-05.*", "", df.plante$Img)
  Img_date <- substr(root.Img, nchar(root.Img[1])-1, nchar(root.Img[1]))
  
  ## Add the month and year
  Img_date <- paste(Img_date, "-05-2013", sep="")
  ## Add to the dataset
  df.plante$Img_date <- strptime(Img_date,format="%d-%m-%Y")
  ### Img_date_as_factor
  df.plante$Img_date_factor <- factor(as.character(df.plante$Img_date))
  ### Rhizo, for the rhizotron number
  # df.plante$Rhizo <- as.factor(substr(sub(".*_0", "R0", df.plante$Img), 1, 4))
}

if (df.plante$ID_manipe[1] == "BMEC1")
{
  # BMEC1 FOR excision IMAGES ##########################
  
  Img_date <- substr(df.plante$Img, 1, 5)
  ## Add the year
  Img_date <- paste(Img_date, "-2014", sep="")
  ## Add to the dataset
  df.plante$Img_date <- strptime(Img_date,format="%d_%m-%Y")
  ### Img_date_as_factor
  df.plante$Img_date_factor <- factor(as.character(df.plante$Img_date))
  ### Rhizo, for the rhizotron number
#   df.plante$Rhizo <- as.factor(substr(df.plante$Img, 12, 15))
}


return(df.plante)
}

##################################################################################

date.conversions <- function (df.plante, df.dates)
{

  df.dates$Date_format <- strptime(df.dates$Date,format="%d/%m/%Y")
  df.dates$Date_factor <- as.factor(as.character(df.dates$Date_format))
  tab.conv <- df.dates[,c("manip","Date_factor", "Jdec", "Jsemis")]
  # str(tab.conv)
  
  ### Jdec, for date conversions to a day of the year and its decimal part
  df.plante$Jdec <- NA 
  
  for (i in 1:length(levels(factor(df.plante$Img_date_factor))))
  {
    my.day <- levels(df.plante$Img_date_factor)[i]
    
    ## 2 conditions line selection MAGIC
    ind <- which(df.plante$Img_date_factor==my.day )
    df.plante[ind, ]$Jdec <- rep(tab.conv[which(tab.conv$Date_factor==my.day),"Jdec"], length(df.plante[ind, ]$Jdec))
  } 
  ### Jdec_trunc, for day of the year without the decimal fraction (type int)
  df.plante$Jdec_trunc <- NA
  df.plante$Jdec_trunc <- as.numeric(trunc(df.plante$Jdec))
  
  ### DAS, for days after sowing ou jours apres semis
  Jsemis <-tab.conv[tab.conv$manip==as.character(df.plante$ID_manipe[1]),]$Jsemis[1]
  df.plante$DAS_trunc<- df.plante$Jdec_trunc - Jsemis
  df.plante$DAS_dec<- df.plante$Jdec - Jsemis
  return(df.plante)
}

##################################################################################

distance.calculations <- function(df.plante, df.nodes)
{
  ### Yins, for the Y coordinate of the insertion node of each root
  # Hypothese: the higher node is the insertion node --> OLD
  # Hypothese: the insertion node is the node within a root whose basal distance is 0
  
  df.insertion.nodes <- df.nodes[df.nodes$bLength==0,]
  ## DANGER: DUPLICATES EXISTS USING Root COLUMN, BUT Root_id NOT AVAILABLE IN df.nodes
  ## DO NOT HAVE THE CHOICE --> WE MUST USE Root COLUMN
  df.insertion.nodes <- df.insertion.nodes[,c("Root", "bLength", "Y")]
  
  # Need to invert
  df.insertion.nodes$Y <- (- df.insertion.nodes$Y)
  df.plante$Yins <- df.insertion.nodes$Y[match(df.plante$Root,df.insertion.nodes$Root)]
  
  ### Dist_base, 
  # Y axis for pixels in these images are at the top of the image (Y=0)
  # so all them are negative
  # take only "root_X" prefix; no "lat"
  # my.primary <- df.plante[grep("root", df.plante$Root), ]
  ## WARNING: sometimes primary lacks Yins!
  ## We take the fisrt lateral
  
  try(my.primary <- df.plante[df.plante$rootOrder == 0, c("Root","Root_id", "Yins", "LPosParent")], silent=T)

  if(!is.na(my.primary$Yins[1]))
  {
  # ASSUMPTION: the distance in height are calculated as the substraction of 
  # the Y coordinate of root insertion minus the insertion of the primary (1D)
  # (in absolute value)
  df.plante$Dist_base <- -(df.plante$Yins - my.primary$Yins[1])
  }
else
{
  cat("WARNING: Yins is NA for primary root")
  
  alternative_origin <- max(df.plante$Yins, na.rm=T)
  df.plante$Dist_base <- -(df.plante$Yins - alternative_origin)
  df.plante[df.plante$rootOrder == 0, ]$Dist_base <- 0
}
  
  return(df.plante)
}

##################################################################################

age.calculations <- function(df.plante, df.dates)
{
  
  df.dates$Date_format <- strptime(df.dates$Date,format="%d/%m/%Y")
  df.dates$Date_factor <- as.factor(as.character(df.dates$Date_format))
  tab.conv <- df.dates[,c("Date_factor", "Jdec")]
  # str(tab.conv)
  
### Date_birth, for the apparition date of each root
df.birth <- aggregate(df.plante$Img_date, df.plante["Root"], min)
date_birth <- df.birth$x[match(df.plante$Root,df.birth$Root)]
df.plante$Date_birth <- strptime(substr(date_birth, 1, 10), format="%Y-%m-%d")
# verif: df.plante[df.plante$Root == levels(df.plante$Root)[20],]   

# ####################################################################################################################
# ### EXPERIMENTAL Date_birth_position, for the apparition date of each root in fonction of its position on the principal axis
# ### Calculation based on rate of displacement of the front of emergenge
# ### Ecuations can be found in "racines primaires" folder nad must be INDIVIDUALLY FOURNISHED
# ### EX R09 GFBM1 
# ### x = (y + 514.22) / 5.12, where x= day of the year (decimal) ; y=seed distance of front of emergence(cm)
# x <- (df.plante$Dist_base+ 514.22)/5.12
# df.plante$Date_birth_position <- x
# # verif: df.plante[df.plante$Root == levels(df.plante$Root)[20],]  
# ####################################################################################################################

### Date_birth_dec, for the apparition date of each root
df.plante$Date_birth_dec <- tab.conv$Jdec[match(df.plante$Date_birth, tab.conv$Date)]
# verif: df.plante[df.plante$Root == levels(df.plante$Root)[20], c(1,2,20,26,27,31)]

###  Root_age, for the age of each root
df.plante$Root_age <- difftime(time1=df.plante$Img_date, time2=df.plante$Date_birth, units=c("days"))

###  Root_age_dec, for the age of each root in day of the year + decimal fraction
df.plante$Root_age_dec <- df.plante$Jdec - df.plante$Date_birth_dec

# #####################################################################################################################
# ###  EXPERIMENTAL Root_age_position, for the age of each root in day of the year + decimal fraction
# df.plante$Root_age_position <- df.plante$Jdec - df.plante$Date_birth_position
# 
# #### COMPARE Root_age from Date_birth_dec and Root_age_position from Date_birth_position
# # df.plante[, c("Root", "Date_birth_dec", "Date_birth_position", "Root_age", "Root_age_dec", "Root_age_position")]

return(df.plante)
}

tronconage <- function (df.plante)
{
  df.plante$Troncon <- rep(NA, dim(df.plante)[1])
  
  ## FILTRE A VALEURS DE DISTANCE EXISTANTES
  df.plante <- df.plante[!is.na(df.plante$Dist_base), ]
  
  MAX <- max(df.plante$Dist_base, na.rm=T)
  
  df.plante[df.plante$Dist_base >= 0
            & df.plante$Dist_base < 5,]$Troncon <- "T05"
  if(MAX >= 5)
  {df.plante[df.plante$Dist_base >= 5
            & df.plante$Dist_base< 10,]$Troncon <- "T10"}
  if(MAX >= 10)
  {df.plante[df.plante$Dist_base >= 10
            & df.plante$Dist_base< 15,]$Troncon <- "T15"}
  if(MAX >= 15)
  {df.plante[df.plante$Dist_base >= 15
            & df.plante$Dist_base< 20,]$Troncon <- "T20"}
  if(MAX >= 20)
  {df.plante[df.plante$Dist_base >= 20
            & df.plante$Dist_base< 25,]$Troncon <- "T25"}
  if(MAX >= 25)
  {df.plante[df.plante$Dist_base >= 25
            & df.plante$Dist_base< 30,]$Troncon <- "T30"}
  if(MAX >= 30)
  {df.plante[df.plante$Dist_base >= 30
            & df.plante$Dist_base< 35,]$Troncon <- "T35"}
  if(MAX >= 35)
  {df.plante[df.plante$Dist_base >= 35
            & df.plante$Dist_base< 40,]$Troncon <- "T40"}
  
  ## last Troncons facultative
  if(MAX >= 40)
  {
    df.plante[df.plante$Dist_base >= 40
              & df.plante$Dist_base< 45,]$Troncon <- "T45"
  }
  if(MAX >= 45)
  {
    df.plante[df.plante$Dist_base >= 45
              & df.plante$Dist_base< 50,]$Troncon <- "T50"
  }
  if(MAX >= 50)
  {
    df.plante[df.plante$Dist_base >= 50
              & df.plante$Dist_base< 55,]$Troncon <- "T55"
  }
  if(MAX >= 55)
  {
    df.plante[df.plante$Dist_base >= 55
              & df.plante$Dist_base< 60,]$Troncon <- "T60"
  }
  
  df.plante$Troncon <- as.factor(df.plante$Troncon)
  return(df.plante)
  
}


architectural.data <- function(df.plante, r)
{
  
  ### global_LR_density
  #####################
  ## calcul in base of total_LR_number AND length_brached_zone
  
  ## total_LR_number for sum of LR along the RR
  total_LR_number <- length(levels(df.plante$Root)) - 1 # substract RP
  
  ## length_brached_zone
  ## previously...
  my.last.emerged <- df.plante[df.plante$Dist_base == max (df.plante$Dist_base, na.rm=T),]
  my.primary <- df.plante[df.plante$Dist_base == min (df.plante$Dist_base),]
  
  base <- my.primary$Dist_base[1]
  end_branched_zone <- my.last.emerged$Dist_base[1]
  
  length_brached_zone <- (end_branched_zone - base)
  ## so...
  global_LR_density <- total_LR_number/length_brached_zone ##  root/ cm
  
  
  ### local_LR_density
  ####################
  ## Order roots from distance to base
  tab <- df.plante[order(df.plante$Yins), c("Root", "Yins")]
  # A KEY INSTRUCTION
  tab <- unique(tab) # dim(tab)[1]
  # only laterals
  tab <- tab[grep("lat", tab$Root),] # dim(tab)[1]
  
  ## Inisialisation local_LR_density column
  tab$dL <- rep(NA, nrow(tab))
  tab$Local_LR_density <- rep(NA, nrow(tab))
  tab$Local_LR_density2 <- rep(NA, nrow(tab))
  df.plante$Local_LR_density <- rep(NA, nrow(df.plante))
  df.plante$Local_LR_density2 <- rep(NA, nrow(df.plante))
  
  fist_one_IDX <- 1+r
  last_one_IDX <- nrow(tab)[1]-r
  
  for (i in fist_one_IDX:last_one_IDX)
  {
    my.root <- tab[i,]
    dL <-  tab[i+r,]$Yins - tab[i-r,]$Yins
    density = nLR_fixed / dL
    density2 = nLR_fixed_corrected / dL
    ## Stock in df.plante one by one
    try(tab[which(tab$Root==my.root$Root),]$Local_LR_density <- density, silent=T)
    try(tab[which(tab$Root==my.root$Root),]$Local_LR_density2 <- density2, silent=T)
    try(tab[which(tab$Root==my.root$Root),]$dL <- dL, silent=T)
    try(df.plante[which(df.plante$Root==my.root$Root),]$Local_LR_density <- density,silent=T)
    try(df.plante[which(df.plante$Root==my.root$Root),]$Local_LR_density2 <- density2, silent=T)
  }
  
  ### SEE RESULTS
  # df.plante[!is.na(df.plante$Local_LR_density),]
  
  ## global_LR_density_from_points
  ################################
  global_LR_density_from_points <- mean(tab$Local_LR_density, na.rm=T)
  global_LR_density_from_points2 <- mean(tab$Local_LR_density2, na.rm=T)
  
  # ## SEE
  # df.plante$Local_LR_density 
  # ## COMPARE
  global_LR_density
  global_LR_density_from_points
  global_LR_density_from_points2
  
  return(df.plante)
}


growth.rates <- function(df.plante)
{
  ## REMEMBER: 
  # LENGTHS MUST BE ORDERED IN TIME TO MAKE THE CALCULATIONS OF GROWTH RATES
  df.plante <- df.plante[order(df.plante$Jdec),]
  df.plante <- df.plante[order(df.plante$Root),]
  
  df.plante$ER_dt <- rep(NA, dim(df.plante)[1])
  df.plante$RER_dt <- rep(NA, dim(df.plante)[1])
  
  # i index for roots in my plant
  idx <- levels(df.plante$Root)
  for (i in 1:length(idx))
  {
    my.root <- df.plante[df.plante$Root==idx[i] ,]
    my.root.comp <- my.root[complete.cases(my.root[,c("Length")]), ]
    
    # If more than one day of length record and no missing length
    if (nrow(my.root)>1 && nrow(my.root) == nrow(my.root.comp))
    {
      ## ER, RER calculation
      # ER and RER null for the first length measure so star at 2nd row
      for (j in 2:nrow(my.root))
      { 
        # temporal stock variables
        ER_dt <- NULL; RER_dt <- NULL; Ldiff<- NULL; tdiff <- NULL;  Ldiv <- NULL
        # Lengths are rounded to avoid RER or RE <0 when actually dL/dl=0
        ## Ldiff for length substraction, tdiff for time substraction
        Ldiff <- round(my.root$Length[j], digits=4) - round(my.root$Length[j-1], digits=4)
        tdiff <- my.root$Jdec[j] - my.root$Jdec[j-1]
        
        # Ldiv for RER calculation
        # Length (i-1) can not be 0, or Ldiv would be -Inf
        ifelse(round(my.root$Length[j-1], digits=4) > 0, 
               Ldiv <- round(my.root$Length[j], digits=4) / round(my.root$Length[j-1], digits=4),
               Ldiv <- NA )
        
        # The slope of ER is calculated as
        my.root$ER_dt[j] <- Ldiff / tdiff
        # Then for RER we use 
        if (!is.na(Ldiv))
        {
          my.root$RER_dt[j] <- log(Ldiv) / tdiff
        }
      } # for j
      
      ## Stock in df.plante
      df.plante[which(df.plante$Root == my.root$Root[1]),]$ER_dt <- my.root$ER_dt
      df.plante[which(df.plante$Root == my.root$Root[1]),]$RER_dt <- my.root$RER_dt
      
    } # if
  } # for i
  
  # substitute NaN and Inf by NA
  
  if(sum(is.nan(df.plante$ER_dt))>0)
  {df.plante[is.nan(df.plante$ER_dt),]$ER_dt <- NA}
  if(sum(is.nan(df.plante$RER_dt))>0)
  {df.plante[is.nan(df.plante$RER_dt),]$RER_dt <- NA}
  if(sum(is.infinite(df.plante$ER_dt))>0)
  {df.plante[is.infinite(df.plante$ER_dt),]$ER_dt <- NA}
  if(sum(is.infinite(df.plante$RER_dt))>0)
  {df.plante[is.infinite(df.plante$RER_dt),]$RER_dt <- NA}
  
  return(df.plante)
}

local.ER.curves <- function(df.plante, r)
{
  ########## Moy_v2_v3
  # Ini
  df.plante$Moy_v2_v3 <- rep(NA, nrow(df.plante))
  idx <- levels(df.plante$Root)
  for (i in 1:length(idx))
  {
    my.root <- df.plante[df.plante$Root == idx[i], ]
    # If at least 3 days of life
    if(nrow(my.root) > 3)
    {
      moy_v2_v3 <- NULL
      v2 <- my.root[my.root$Root_age == 2, ]$ER_dt[1]
      v3 <- my.root[my.root$Root_age == 3, ]$ER_dt[1]
      moy_v2_v3 <- (v2+v3)/2
      # If v2 ot v3 does not exist length(moy_v2_v3)=0
      # Stock df.plante, only one value in Root_age=3
      if(length(moy_v2_v3)>0)
      {
        n <- length(df.plante[which(df.plante$Root== my.root$Root[1]), ]$Moy_v2_v3)
        df.plante[df.plante$Root== my.root$Root[1], ]$Moy_v2_v3 <- rep(moy_v2_v3, n)
        # rep(moy_v2_v3, length(df.plante[df.plante$Root== my.root$Root[1], ]$Moy_v2_v3))
      }
    }
  }
  
  ### SEE RESULTS
  # head(df.plante[!is.na(df.plante$Moy_v2_v3),  ])
  # df.plante[!is.na(df.plante$Moy_v2_v3),  ]$Moy_v2_v3
  
  ### Local_ER_median_v2_v3 AND Local_ER_moyenne_v2_v3
  #################################################################################
  
  # J'ai proc?d? comme suit
  # - vir? (par filtrage) les NA de
  # Moy_v2_v3
  # - retenu les root_age =3
  # Puis calcul? les moyennes et m?dianes flottantes sur 21 racines successives
  
  df.plante$Local_ER_median_v2_v3 <- rep(NA, nrow(df.plante))
  df.plante$Local_ER_moyenne_v2_v3 <- rep(NA, nrow(df.plante))
  
  ## Order roots from distance to base
  tab <- df.plante[order(df.plante$Yins, decreasing=T), ] # , c("Root", "Yins", "ER_dt", "Img_date_factor")]
  # only laterals
  tab <- tab[grep("lat", tab$Root),] # dim(tab)[1]
  # vir? (par filtrage) les NA de Moy_v2_v3
  tab <- tab[!is.na(tab$Moy_v2_v3),] # MUST BE THE SAME LENGTH AS length(which(!is.na(tab$Moy_v2_v3)))
  # retenu les root_age =3
  tab <- tab[which(tab$Root_age == 3),]
  
  ## Inisialisation Local_ER_median_v2_v3 column
  tab$dL <- as.numeric(rep(NA, nrow(tab)))
  tab$Local_ER_median_v2_v3 <- as.numeric(rep(NA, nrow(tab)))
  tab$Local_ER_moyenne_v2_v3 <- as.numeric(rep(NA, nrow(tab)))
  
  fist_one_IDX <- 1+r
  last_one_IDX <- nrow(tab)[1]-r
  
  for (j in fist_one_IDX:last_one_IDX)
  {
    my.root <- tab[j,]
    dL <-  tab[j+r,]$Yins - tab[j-r,]$Yins
    neighbours <- tab[(j-r):(j+r), ] # nrow(neighbours) must be 21
    ## Local median calculation
    ## Order by ER_dt which can be NA
    neighbours <- neighbours[order(neighbours$Moy_v2_v3, na.last=NA), ]
    local_ER_median_v2_v3 <- neighbours[r+1, ]$Moy_v2_v3 # SUBSTITUTE trunc((nrow(neighbours)/2))+1 BY 
    
    ## moyenne calculation
    ## Local mean calculation
    local_ER_moyenne_v2_v3 <- mean(neighbours$Moy_v2_v3, na.rm=T)
    
    #tab[which(tab$Root==my.root$Root),]$Local_ER_moyenne_v2_v3 <- Local_ER_moyenne_v2_v3
    #tab[which(tab$Root==my.root$Root),]$dL <- dL
    # tab[which(tab$Root==my.root$Root),]$Local_ER_median_v2_v3 <- local_ER_median_v2_v3
    # tab[which(tab$Root==my.root$Root),]$dL <- dL
    ## Stock in df.plante one by one
   
    if(!is.na(local_ER_median_v2_v3))
    {try(df.plante[which(df.plante$Root==my.root$Root[1]) , ]$Local_ER_median_v2_v3[1] <- local_ER_median_v2_v3, silent=T)}
    if(!is.na(local_ER_moyenne_v2_v3))
    {try(df.plante[which(df.plante$Root==my.root$Root[1]) , ]$Local_ER_moyenne_v2_v3[1] <- local_ER_moyenne_v2_v3, silent=T)}
    
  }
  return(df.plante)
}

diametres <- function(df.plante)
{
  df.plante$bLength_bDiam <- as.numeric(rep(NA, nrow(df.plante)))
  df.plante$bDiam <- as.numeric(rep(NA, nrow(df.plante)))
  
  df.plante$aDiam <- as.numeric(rep(NA, nrow(df.plante)))
  df.plante$aLength_aDiam <- as.numeric(rep(NA, nrow(df.plante)))
  df.plante$bLength_aDiam <- as.numeric(rep(NA, nrow(df.plante)))
  
  # REMPLI
  idx_dates <- levels(df.nodes$Img)
  for (i in 1:length(idx_dates))
  {
    
    my.date <- df.nodes[df.nodes$Img==idx_dates[i], ]
    idx_lat <- levels(factor(my.date$Root))
    
    for (j in 1:length(idx_lat))
    {
      ## Recuperation of apical and basal diameters
      my.root <- my.date[my.date$Root==idx_lat[j], ]
      
      if(nrow(my.root)>=3)
      {
        #       col.my.root <- sample(colors(), 1)
        #       plot(Diam ~ bLength, my.root,
        #            ylim=c(0, 0.15), xlim=c(0,10), col=col.my.root, type="b", lty=1)
        #       mtext (3, text= idx_lat[j])
        # supposition: nodes in order
        #### apical node
        last.three <- tail(my.root, 3)
        # take the median node (at position 2 if ordered)
        last.three <- last.three[order(last.three$Diam), ]
        apical.node <- last.three[2,]
        
        #### basal node
        first.three <- head(my.root, 3)
        # take the median node (at position 2 if ordered)
        first.three <- first.three[order(first.three$Diam), ]
        basal.node <- first.three[2,]
        
        #       points(Diam ~ bLength, apical.node,
        #              ylim=c(0, 0.15), xlim=c(0,10), col="orange", type="b", lty=1)
        #       
        #       # first three nodes
        #       points(Diam ~ bLength, basal.node,
        #              ylim=c(0, 0.15), xlim=c(0,10), col="orange", type="b", lty=1)
        
        # remplissage df.plante which -> problem with factors
        
        lines <- which(df.plante$Root==basal.node$Root & df.plante$Img == basal.node$Img)
        if (length(lines)>0)
        {
        df.plante[lines, ]$bDiam <- basal.node$Diam
        df.plante[lines, ]$bLength_bDiam <- basal.node$bLength
        df.plante[lines, ]$aDiam <- apical.node$Diam
        df.plante[lines, ]$aLength_aDiam <- apical.node$aLength
        df.plante[lines, ]$bLength_aDiam <- apical.node$bLength
        }
      } # if
      
    } # for1
    
  } # for2
  
  return(df.plante)
}


tab.recapitulatif <- function(df.plante, plantID)
{
  ### j for dates
  df.plante$Img_date_factor <- factor(df.plante$Img_date_factor)
  indx_days <- levels(df.plante$Img_date_factor)
  
  # Parameters to calculate and inisialisation
  Date <- indx_days
  PlantID <- rep(plantID, length(Date))
  DAS <- as.numeric(rep(NA, length(Date)))
  Length_PR <- as.numeric(rep(NA, length(Date)))
  dLength_PR <- as.numeric(rep(NA, length(Date)))
  Length_allLR <- as.numeric(rep(NA, length(Date)))
  dLength_allLR <- as.numeric(rep(NA, length(Date)))
  N_allLR <- as.numeric(rep(NA, length(Date)))
  N_Emerged_primordia <- as.numeric(rep(NA, length(Date)))
  
  # Ini sialisation data.frame
  # tab <- data.frame (X=NA) [-1,1]
  tab <- NULL
  tab <- as.data.frame(cbind(PlantID, Date, DAS, Length_PR, dLength_PR, 
                          Length_allLR, dLength_allLR, 
                          N_allLR, N_Emerged_primordia))

#   # Change tpe factor to numeric
  tab$Length_PR <- as.numeric(tab$Length_PR)
  tab$DAS <- as.numeric(tab$DAS)
  tab$dLength_PR <- as.numeric(tab$dLength_PR)
  tab$Length_allLR <- as.numeric(tab$Length_allLR)
  tab$dLength_allLR <- as.numeric(tab$dLength_allLR)
  tab$N_allLR <- as.numeric(tab$N_allLR)
  tab$N_Emerged_primordia <- as.numeric(tab$N_Emerged_primordia)
  
  # str(tab)
  
  # Remplisage par rows (dates)
  for (i in 1:length(indx_days))
  {
    date <- strptime(indx_days[i], format="%Y-%m-%d")
    my.plant.day <- df.plante[df.plante$Img_date == date, ]
    
    ## Length_PR
    if(!is.na(my.plant.day[my.plant.day$Root_LPos == " taproot_0", ]$Length[1]))
    {tab$Length_PR[i] <- round(my.plant.day[my.plant.day$Root_LPos == " taproot_0", ]$Length[1],2)}
    ## Length_allLR
    if(!is.na(tab$Length_PR[i]))
    {tab$Length_allLR[i] <- round(sum(my.plant.day$Length) - tab$Length_PR[i],2)}
    ## N_allLR
    tab$N_allLR[i] <- length(levels(factor(my.plant.day$Root))) - 1 # substract PR
    ## DAS
    tab$DAS[i] <- my.plant.day$DAS_trunc[1]
  }
  
  # Les differentiels on les calcule de la fa?on suivante
  for (i in 2:length(indx_days))
  {
    tab$dLength_PR[i] <- tab$Length_PR[i] - tab$Length_PR[i-1]
    tab$dLength_allLR[i] <- tab$Length_allLR[i] - tab$Length_allLR[i-1]
    tab$N_Emerged_primordia[i] <- tab$N_allLR[i] - tab$N_allLR[i-1]
  }
  
  return(tab)
}