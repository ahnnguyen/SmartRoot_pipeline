dates.plot.limits <- function()
{
  if (df.plante$ID_manipe[1] == "GFBM1")
  {
    DATE.MIN <- as.Date(strptime("2013-04-03", format="%Y-%m-%d"))
    DATE.MAX <- as.Date(strptime("2013-04-30", format="%Y-%m-%d"))
    
  }
  if (df.plante$ID_manipe[1] == "GFBM2")
  {
    DATE.MIN <- as.Date(strptime("2013-05-15", format="%Y-%m-%d"))
    DATE.MAX <- as.Date(strptime("2013-05-29", format="%Y-%m-%d"))
    
  }
  if (df.plante$ID_manipe[1] == "BMEC1")
  {
    DATE.MIN <- as.Date(strptime("2014-05-02", format="%Y-%m-%d"))
    DATE.MAX <- as.Date(strptime("2014-05-16", format="%Y-%m-%d"))
  }
  OUTPUT <- c(DATE.MIN, DATE.MAX)
  return(OUTPUT)
}


plot.SR3.length.date <- function()
{
  # Indexs
  # j for Troncon
  idx_troncon <- levels(df.plante$Troncon)
  for (j in 1:(length(idx_troncon)))
  {
    # j <- 8
    df.troncon <- df.plante[df.plante$Troncon==idx_troncon[j], ]
    # i for root
    index <- levels(factor(df.troncon$Root))
    n_index <- length(index)
    
    ####### Inisialisation (empty)
    plot (0 ~ 0,type="n",
          ylim=c(0,MAX.ROOT.LENGTH), xlim=as.Date(c(DATE.MIN,DATE.MAX)),
          xlab="Date", ylab="Root length (cm)", xaxt = "n")
    dates <- as.Date(unique(df.troncon$Img_date), "%m/%d/%Y")
    axis(1, dates, format(dates, "%b %d"), cex.axis = .7)
    
    mtext(3, text=paste("Lateral roots of ", df.troncon$Rhizo[1], df.troncon$ID_manipe[1], 
                        "at the", idx_troncon[j],"portion"))
    ####### Fill in my plot
    for (i in 1:n_index)
    {
      my.root <- df.troncon[df.troncon$Root==index[i] ,]
      points (y=my.root$Length, x= as.Date(my.root$Img_date), type="l",col=rainbow(n_index)[i])
      text(labels=my.root$Root[1], cex=0.7,
           x=tail(as.Date(my.root$Img_date),n=1)+1,y=tail(my.root$Length,n=1), col=rainbow(n_index)[i])
      
    } # for i

    
  } # for j
}


plot.SR4.RER.root.age <- function()
{
  
  # Indexs
  # j for Troncon
  idx_troncon <- levels(df.plante$Troncon)
  for (j in 1:(length(idx_troncon)-1))
  {
    df.troncon <- df.plante[df.plante$Troncon==idx_troncon[j], ]
    # i for root
    index <- levels(factor(df.troncon$Root))
    n_index <- length(index)
    
    ####### Inisialisation with the first root
    my.root <- df.troncon[df.troncon$Root==index[1] ,]
    plot (RER_dt ~ Root_age, my.root, type="l", col=rainbow(1), lty="dashed",
          ylim= c(0, MAX.RER), xlim=c(0, MAX.ROOT.AGE),
          ylab="RER (cm cm-1 d-1)" , xlab="Days after root emergence")
    text(labels=my.root$Root[1], cex=0.7,
         x=tail(my.root$Root_age,n=1)+1,y=tail(my.root$RER_dt,n=1), col=rainbow(1))
    
    mtext(3, text=paste("Relative elongation rates lateral roots of", df.plante$Rhizo[4], df.plante$ID_manipe[1], 
                        "at the", idx_troncon[j],"portion"))
    ####### Fill in my plot
    for (i in 2:n_index)
    {
      my.root <- df.plante[df.plante$Root==index[i] ,]
      if(my.root$rootOrder[1]==1)
      {
        points (RER_dt ~ Root_age, my.root, type="l", col=rainbow(n_index)[i], lty="dashed")
        text(labels=my.root$Root[1], cex=0.7,
             x=tail(my.root$Root_age,n=1)+1,y=tail(my.root$RER_dt,n=1), col=rainbow(n_index)[i])
      }
    }
    
  } # for j
}


plot.SR4b.log.RER.root.age <- function()
{
  # Indexs
  # j for Troncon
  idx_troncon <- levels(df.plante$Troncon)
  for (j in 1:(length(idx_troncon)-1))
  {
    df.troncon <- df.plante[df.plante$Troncon==idx_troncon[j], ]
    # i for root
    index <- levels(factor(df.troncon$Root))
    n_index <- length(index)
    
    ####### Inisialisation (with the first root (empty)
    plot (0 ~ 0, type="n", 
          ylim= c(-2, MAX.RER), xlim=c(0, MAX.ROOT.AGE),
          ylab="log RER (cm cm-1 d-1)" , xlab="Days after root emergence")
   
    
    mtext(3, text=paste("Relative elongation rates lateral roots of", df.plante$Rhizo[4], df.plante$ID_manipe[1], 
                        "at the", idx_troncon[j],"portion"))
    ####### Fill in my plot
    for (i in 1:n_index)
    {
      my.root <- df.plante[df.plante$Root==index[i] ,]
      if(my.root$rootOrder[1]==1)
      {
        points (log10(RER_dt) ~ Root_age, my.root, type="l", col=rainbow(n_index)[i], lty="dashed")
        text(labels=my.root$Root[1], cex=0.7,
             x=tail(my.root$Root_age,n=1)+1,y=log10(tail(my.root$RER_dt,n=1)), col=rainbow(n_index)[i])
      }
    }
    
  } # for j
}


plot.SR5.ER.root.age <- function()
{
  
  # Indexs
  # j for Troncon
  idx_troncon <- levels(df.plante$Troncon)
  for (j in 1:(length(idx_troncon)-1))
  {
    df.troncon <- df.plante[df.plante$Troncon==idx_troncon[j], ]
    # i for root
    index <- levels(factor(df.troncon$Root))
    n_index <- length(index)
    
    ####### Inisialisation with the first root
    my.root <- df.troncon[df.troncon$Root==index[1] ,]
    plot (ER_dt ~ Root_age, my.root, type="l", col=rainbow(1), lty= "solid",
          ylim= c(0,MAX.ER), xlim=c(0,MAX.ROOT.AGE), 
          ylab="ER (cm d-1)", xlab="Days after root emergence")
    text(labels=my.root$Root[1], cex=0.7,
         x=tail(my.root$Root_age,n=1)+1,y=tail(my.root$ER_dt,n=1), col=rainbow(1))
    
    
    mtext(3, text=paste("Elongation rates lateral roots of", df.plante$Rhizo[1], df.plante$ID_manipe[1], 
                        "at the", idx_troncon[j],"portion"))
    ####### Fill in my plot
    for (i in 2:n_index)
    {
      my.root <- df.plante[df.plante$Root==index[i] ,]
      if(my.root$rootOrder[1]==1)
      {
        points (ER_dt ~ Root_age, my.root, type="l", col=rainbow(n_index)[i], lty= "solid")
        text(labels=my.root$Root[1], cex=0.7,
             x=tail(my.root$Root_age,n=1)+1,y=tail(my.root$ER_dt,n=1), col=rainbow(n_index)[i])
      }
    }
    
  } # for j
  
}

plot.SR6.Systeme.Racinaire <- function() 
{
  ################## Indexs
  
  ### j for dates
  indx_days <- levels(df.plante$Img_date_factor)
  
  ### colors unique
  indx.ALL <- levels(df.plante$Root)
  Roots <- levels(df.plante$Root) # for the rainbow() function
  Color <- rainbow(length(indx.ALL))[1:length(indx.ALL)]
  tab.colors <- data.frame(cbind(Roots, Color))
  
  # Start at 5 to see some laterals
  
  for (j in 1:length(indx_days))
  {
    upper_date <- strptime(indx_days[j], format="%Y-%m-%d")
    my.plante.uptodate <- df.plante[df.plante$Img_date <= upper_date, ]
    
    ### i for LATERAL roots appeared already the day j
    my.lat <- my.plante.uptodate[grep(pattern="lat", x=my.plante.uptodate$Root), ]
    index <- levels(factor((my.lat$Root)))
    
    ################# Plot (one day)
    ####### Inisialisation with the first root (the primary...never null)
    PRIMARY.COND <- min (my.plante.uptodate$Dist_base, na.rm=T) # avoid = 0
    my.root <- my.plante.uptodate[my.plante.uptodate$Dist_base== PRIMARY.COND,] 
    
    my.x <- rep(my.root[nrow(my.root), ]$Dist_base, 2)
    my.y <- c(0,my.root[nrow(my.root), ]$Length )
    
    plot (my.y ~ my.x, type="l", col="black",
          xlim=c(MIN.DIST, MAX.DIST),
          ylim=c(-20, 20), xlab="", ylab="")
    mtext(3, text=paste("Emerged lateral roots of:", df.plante$Rhizo[4], df.plante$ID_manipe[1], 
                        "\nDate:", upper_date))
    #  "\nParameters local curves: R=", r, ", fixed LR number=", nLR_fixed))
    
    mtext(1,line=2, text="Seed distance (cm)")
    mtext(2,line=2, text="Root length (cm)")
    # mtext(4,line=1, text="Local ER(J2-3) (cm/d)") 
    
    ### Extract the number of lateral roots 
    is.even <- function(x) x %% 2 == 0 
    is.odd <- function(x) x %% 2 != 0
    ## MAGIC!
    # as.numeric(gsub('.*_([0-9]+).*','\\1',index[1]))
    
    for (i in 1:length(index))
    {
      my.root <- my.plante.uptodate[my.plante.uptodate$Root==index[i] ,]
      lat.number <- as.numeric(gsub('.*_([0-9]+).*','\\1', my.root$Root[1]))
      
      ####### Fill in my plot (odd root levels)
      if (!is.na(my.root$Length[1]) && is.odd(lat.number))
      {
        my.x <- rep(my.root[nrow(my.root), ]$Dist_base, 2)
        my.y <- c(0.12,my.root[nrow(my.root), ]$Length )
        
        points (my.y ~ my.x, type="l", col=tab.colors[tab.colors$Root == my.root$Root[1],]$Color)
      }
      ####### Fill in my plot (even root levels)
      if (!is.na(my.root$Length[1]) && is.even(lat.number))
      {
        my.x <- rep(my.root[nrow(my.root), ]$Dist_base, 2)
        my.y <- c(0.12,my.root[nrow(my.root), ]$Length )
        
        points (-my.y ~ my.x, type="l", col=tab.colors[tab.colors$Root == my.root$Root[1],]$Color)
      }
    }
    
    #   ### Now we add 3 curves of local parameters
    #   ### curve1. LOCAL density
    #   ### curve2. LOCAL ER_median from moy_v2_v3
    #   ### curve3. LOCAL ER_moyenne from moy_v2_v3
  
      my.plante.uptodate <- my.plante.uptodate[order(my.plante.uptodate$Dist_base), ]
      
      #####  Curve1. LOCAL density
#       if(length(levels(factor((my.lat$Root)))) > r)
#       {
        # LOCAL LR DENSITIES IN ONE STEP
        ### plot
        ### Note that the scale of x-axis remains unchanged
        par(new=T)
        plot (Local_LR_density ~ Dist_base, my.plante.uptodate, type="l",
              xaxt="n", yaxt="n", xlab=" ", ylab=" ",
              xlim=c(MIN.DIST, MAX.DIST),ylim=c(-20, 10))
        axis(4, at=seq(4, 15, by=1))
        
    # include global densities
#         abline(h=global_LR_density, lty="longdash")
#         abline(h=global_LR_density_from_points, lty="dotted")
        # abline(h=global_LR_density_from_points2, lty="dashed")
        legend("topright", cex=0.6, 
               legend= c("Local LR density (root cm-1) ", "Global LR density", "Global LR density from points"), 
               lty= c(lty="solid", "longdash", "dotted"))
#       }
      
      ###### Curve2. LOCAL ER_median from moy_v2_v3
      ###### Curve3. LOCAL ER_moyenne from moy_v2_v3
      
      ### Note that the scale of x-axis remains unchanged
      median <- my.plante.uptodate[!is.na(my.plante.uptodate$Local_ER_median_v2_v3),]
      moyenne <- my.plante.uptodate[!is.na(my.plante.uptodate$Local_ER_moyenne_v2_v3),]
      ## median
          par(new=T)
          plot (Local_ER_median_v2_v3 ~ Dist_base, median, type="l",
                xaxt="n", yaxt="n", xlab=" ", ylab=" ",
                xlim=c(MIN.DIST, MAX.DIST),ylim=c(0, 3), col="tomato3")
      ## moyenne
      par(new=T)
      plot (Local_ER_moyenne_v2_v3 ~ Dist_base, moyenne, type="l",
            xaxt="n", yaxt="n", xlab=" ", ylab=" ",
            xlim=c(MIN.DIST, MAX.DIST),ylim=c(0, 3), col="olivedrab")
      axis(4, at=seq(0, 1, by=0.2))
    
    legend("bottomright", cex=0.6, 
           legend= c("Local median V2-3 (cm d-1) ", "Local average V2-3 (cm d-1) "), 
           col= c("tomato3", "olivedrab"), lty="solid")
      
    ####### finish plot
    
  } # boucle days
}

plot.SR7.histogramme.vitesses <- function()
{
  # unique values and true values
  # important: REMOVE NA VALUES
  df.plante.age3 <- df.plante[df.plante$Root_age==3,]
  df.plante.age3 <- df.plante.age3[grep("lat", df.plante.age3$Root),]
  df.plante.age3 <- df.plante.age3[which(!is.na(df.plante.age3$Moy_v2_v3)),]
  
  # Substituer par 0 les ER negatifs, sils existent
  df.plante.age3$Moy_v2_v3 <- round(df.plante.age3$Moy_v2_v3, 3)
  range(df.plante.age3$Moy_v2_v3, na.rm=T)
  
  if(length(which(df.plante.age3$Moy_v2_v3 < 0)> 0))
  {
    df.plante.age3[which(df.plante.age3$Moy_v2_v3 < 0) , ]$Moy_v2_v3 <- 
      rep(0, length(which(df.plante.age3$Moy_v2_v3 < 0)))
  }
  
  # VERIFY
  range(df.plante.age3$Moy_v2_v3, na.rm=T)
  
  
  # if(length(unique(df.plante.age3$Root))== length(df.plante.age3$Root))
  # {
  
    ER_20cm <- NULL; ER_40cm <- NULL
    # ALL Moy_v2_v3
    h1 <- hist(df.plante.age3$Moy_v2_v3, breaks = BREAKS, plot=FALSE)
    h1$counts=h1$counts/sum(h1$counts)
    plot(h1, ylim=c(0, 1), main="h1")
    
    # Fist 20 cm
    ER_20cm <- df.plante.age3[df.plante.age3$Dist_base <= 20, ]
    
    h2 <- hist(ER_20cm$Moy_v2_v3, breaks = BREAKS, plot=FALSE)
    h2$counts=h2$counts/sum(h2$counts)
    plot(h2, ylim=c(0, 1), main="h2")
    
    # Last 20 cm
    ER_40cm <- df.plante.age3[df.plante.age3$Dist_base > 20 
                              & df.plante.age3$Dist_base <= 40 , ]
    h3 <- hist(ER_40cm$Moy_v2_v3, breaks = BREAKS, plot=FALSE)
    h3$counts=h3$counts/sum(h3$counts)
    plot(h3, ylim=c(0, 1), main="h3")
  # }
}


plot.SR8.histogramme.diametres <- function()
{
  
  idx_dates <- levels(df.plante$Img_date_factor)
  for (i in 1:length(idx_dates))
  {
    my.date <- df.plante[df.plante$Img_date_factor==idx_dates[i], ]
    
    # aDiam
    h1 <- hist(my.date$aDiam, plot=FALSE, breaks=BREAKS)
    h1$counts=h1$counts/sum(h1$counts)
    plot(h1$counts ~ h1$mids, ylim=c(0,1), col="red", typ="b", pch=2)
    
    # bDiam
    h2 <- hist(my.date$bDiam, plot=FALSE, breaks=BREAKS)
    h2$counts=h2$counts/sum(h2$counts)
    points(h2$counts ~ h2$mids, ylim=c(0,1), typ="b", col="blue", pch=1)
    
    mtext (3, text=paste("Date:", idx_dates[i] ))
    legend("topright", legend=c("Basal diameter", "Apical diameter"), col=c("blue", "red"), pch=c(1,2))
  }
  
}

plot.SR9.ER.diametres.api <- function()
{
  # Indexs
  # j for Troncon
  idx_troncon <- levels(df.plante$Troncon)
  for (j in 1:(length(idx_troncon)-1))
  {
    df.troncon <- df.plante[df.plante$Troncon==idx_troncon[j], ]
    # i for root
    index <- levels(factor(df.troncon$Root))
    n_index <- length(index)
    
    ####### Inisialisation with the first root
    my.root <- df.troncon[df.troncon$Root==index[1] ,]
    ## Order by root_age
    my.root <- my.root[order(my.root$Root_age),]
    ## Eliminate NA values in ER_dt and aDiam
    my.root <- my.root[complete.cases(my.root[,c(34,41)]), ]
    
    plot (ER_dt ~ aDiam, my.root, col=rainbow(1), lty= "solid", type="b",
          ylim= c(0,MAX.ER), xlim=c(0,MAX.API.DIAM), pch=2,
          ylab="ERj/j-1 (cm d-1)", xlab="Apical diameter")
    
    points (ER_dt ~ aDiam, my.root[1,], type="p", pch=17, col=rainbow(1))
    
    legend("topright", pch=17, legend="First apical diameter available" )
    
    #   text(labels=my.root$Root[1], cex=0.7,
    #        x=na.omit(my.root$aDiam)[1],y=max(my.root$ER_dt, na.rm=T)+0.2, col=rainbow(1))
    
    mtext(3, text=paste("Lateral roots of", df.plante$Rhizo[1], df.plante$ID_manipe[1], 
                        "at the", idx_troncon[j],"portion"))
    ####### Fill in my plot
    for (i in 2:n_index)
    {
      my.root <- df.plante[df.plante$Root==index[i] ,]
      ## Order by root_age
      my.root <- my.root[order(my.root$Root_age),]
      ## Eliminate NA values in ER_dt and aDiam
      my.root <- my.root[complete.cases(my.root[,c(34,41)]), ]
      
      if(my.root$rootOrder[1]==1 && nrow(my.root)>=1)
      {
        points (ER_dt ~ aDiam, my.root, col=rainbow(n_index)[i], lty= "solid", type="b", pch=2)
        points (ER_dt ~ aDiam, my.root[1,], type="p", pch=17, col=rainbow(n_index)[i])
        #  x=na.omit(my.root$aDiam)[1],y=max(my.root$ER_dt, na.rm=T)+0.2, col=rainbow(n_index)[i])
        
      }
    }
    
  } # for j
  
}

plot.SR9b.ER.diametres.api.by.N.roots <- function()
{
  # Indexs
  ##
  idx_roots <- levels(df.plante$Root)
  
  j0 <- 1
  while (j0 < (length(idx_roots)-10))
  {
    ## SUBSET WITH %in%
    df.Nroots <- df.plante[df.plante$Root %in% idx_roots[j0:(j0+N-1)], ]
    # i for root
    index <- levels(factor(df.Nroots$Root))
    
    ####### Inisialisation with the first root
    my.root <- df.Nroots[df.Nroots$Root==index[1] ,]
    
    # VIDE
    plot (ER_dt ~ aDiam, my.root, col=rainbow(1), lty= "solid", type="n",
          ylim= c(0,MAX.ER), xlim=c(0,MAX.API.DIAM), pch=2,
          ylab="ERj/j-1 (cm d-1)", xlab="Apical diameter")
    
    legend("topright", pch=17, legend="First apical diameter available" )
    
    mtext(3, line=2, text=paste("Lateral roots of", df.plante$Rhizo[1], df.plante$ID_manipe[1], 
                                "grouped by", N))
    mtext(3, line=1, text=paste("From", idx_roots[j0], "to", idx_roots[j0+N-1]))
    
    ####### Fill in my plot
    for (i in 1:N)
    {
      my.root <- df.plante[df.plante$Root==index[i] ,]
      ## Order by root_age
      my.root <- my.root[order(my.root$Root_age),]
      ## Eliminate NA values in ER_dt and aDiam
      my.root <- my.root[complete.cases(my.root[,c("ER_dt","aDiam")]), ]
      
      
      if(my.root$rootOrder[1]==1 && nrow(my.root)>=1)
      {
        my.color <- sample(colors(), 1)
        points (ER_dt ~ aDiam, my.root, col=my.color, lty= "solid", type="b", pch=2)
        points (ER_dt ~ aDiam, my.root[1,] , col=my.color,  pch=17)
        text(labels=my.root$Root[1], cex=0.7, 
             x=my.root[1, ]$aDiam,y=max(my.root$ER_dt, na.rm=T)+0.1, col=my.color)
        
      }
    }
    
    j0 <- j0+N
  } # for j
}

plot.SR10.diametres.api.bLength.by.N.roots <- function()
{
  
  # Indexs
  ##
  idx_roots <- levels(df.plante$Root)
  
  j0 <- 1
  while (j0 < (length(idx_roots)-10))
  {
    ## SUBSET WITH %in%
    df.Nroots <- df.plante[df.plante$Root %in% idx_roots[j0:(j0+N-1)], ]
    # i for root
    index <- levels(factor(df.Nroots$Root))
    
    ####### Inisialisation with the first root
    my.root <- df.Nroots[df.Nroots$Root==index[1] ,]
    
    # VIDE
    plot (aDiam ~ bLength_aDiam, my.root, col=rainbow(1), lty= "solid", type="n",
          ylim= c(0,MAX.API.DIAM), xlim=c(0,MAX.LENGTH), pch=2)
    # ylab="apical d", xlab="Apical diameter")
    
    legend("topright", pch=c(1,2,17,8), legend=c("Diameter of basal node", "Diameter of apical node", "Apex position", "ERj/j-1"), cex=0.8)
    
    mtext(3, line=2, text=paste("Lateral roots of", df.plante$Rhizo[1], df.plante$ID_manipe[1], 
                                "grouped by", N))
    mtext(3, line=1, text=paste("Roots", idx_roots[j0], "to", idx_roots[j0+N-1]))
    
    ####### Fill in my plot
    for (i in 1:N)
    {
      my.root <- df.plante[df.plante$Root==index[i] ,]
      ## Order by root_age
      my.root <- my.root[order(my.root$Root_age),]
      ## Eliminate NA values in ER_dt and bLength_aDiam
      my.root <- my.root[complete.cases(my.root[,c("bLength_aDiam")]), ]
      
      
      if(my.root$rootOrder[1]==1 && nrow(my.root)>=1)
      {
        
        my.color <- sample(palette,1)
        par(new=T)
        # evo diam api
        plot (aDiam ~ bLength_aDiam, my.root, col=my.color, lty= "solid", type="b",
              ylim= c(0,MAX.API.DIAM), xlim=c(0,MAX.LENGTH), pch=2)
        # basal diam
        points (bDiam ~ bLength_bDiam, my.root[1,] , col=my.color,  pch=1, cex=1.2)
        
        dL <- tail(my.root,1)$aLength_aDiam
        points (x=tail(my.root,1)$bLength_aDiam+dL , y=tail(my.root,1)$aDiam, col=my.color,  pch=17, cex=1.2)
        text(labels=my.root$Root[1], cex=0.7, 
             x=tail(my.root,1)$bLength_aDiam+dL+1, y=tail(my.root,1)$aDiam, col=my.color)
        
        # Add ER_dt
        par(new=T)
        plot(ER_dt ~ bLength_aDiam, my.root, xlim=c(0,MAX.LENGTH), ylim=c(0, MAX.ERdt), 
             xaxt="n", yaxt="n", xlab=" ", ylab=" ",
             pch=8, type="b", col=my.color) 
        text(labels=my.root$Root[1], cex=0.7, 
             x=tail(my.root,1)$bLength_aDiam+dL+1, y=tail(my.root,1)$ER_dt, col=my.color)
      }
      
    }
    axis(4, at=seq(0, MAX.ERdt, by=0.25))
    j0 <- j0+N
  } # for j
}


plot.SR11.diametres.api.root.age.by.N.roots <- function()
{
  # Indexs
  ##
  idx_roots <- levels(df.plante$Root)
  
  j0 <- 1
  while (j0 < (length(idx_roots)-10))
  {
    ## SUBSET WITH %in%
    df.Nroots <- df.plante[df.plante$Root %in% idx_roots[j0:(j0+N-1)], ]
    # i for root
    index <- levels(factor(df.Nroots$Root))
    
    ####### Inisialisation with the first root
    my.root <- df.Nroots[df.Nroots$Root==index[1] ,]
    
    # VIDE
    plot (aDiam ~ Root_age_dec, my.root, col=rainbow(1),
          ylim= c(0,MAX.API.DIAM), xlim=c(0,MAX.AGE), pch=2, lty= "solid", type="n")
    
    legend("topright", pch=c(1,2,8), legend=c("Diameter of basal node", "Diameter of apical node", "ERj/j-1"), cex=0.8)
    
    mtext(3, line=2, text=paste("Lateral roots of", df.plante$Rhizo[1], df.plante$ID_manipe[1], 
                                "grouped by", N))
    mtext(3, line=1, text=paste("Roots", idx_roots[j0], "and", idx_roots[j0+N-1]))
    
    ####### Fill in my plot
    for (i in 1:N)
    {
      my.root <- df.plante[df.plante$Root==index[i] ,]
      ## Order by root_age
      my.root <- my.root[order(my.root$Root_age),]
      ## Eliminate NA values in ER_dt and bLength_aDiam
      # my.root <- my.root[complete.cases(my.root[,c("bLength_aDiam")]), ]
      
      if(my.root$rootOrder[1]==1 && nrow(my.root)>=1)
      {
        
        my.color <- sample(palette,1)
        par(new=T)
        # evo diam api
        plot (aDiam ~ Root_age_dec, my.root, col=my.color, lty= "solid", type="b", pch=2,
              ylim= c(0,MAX.API.DIAM), xlim=c(0,MAX.AGE))
        # basal diam
        points(bDiam ~ Root_age_dec, my.root[1,] , col=my.color,  pch=1, cex=1.2)
        
        text(labels=my.root$Root[1], cex=0.7, 
             x=tail(my.root,1)$Root_age_dec+1, y=tail(my.root,1)$aDiam, col=my.color)
        
        # Add ER_dt
        par(new=T)
        plot(ER_dt ~ Root_age_dec, my.root, xlim=c(0,MAX.AGE), ylim=c(0, MAX.ERdt), 
             xaxt="n", yaxt="n", xlab=" ", ylab=" ",
             pch=8, type="b", col=my.color) 
        text(labels=my.root$Root[1], cex=0.7, 
             x=tail(my.root,1)$Root_age_dec+1, y=tail(my.root,1)$ER_dt, col=my.color)
      }
      
    }
    
    axis(4, at=seq(0, MAX.ERdt, by=0.25))
    j0 <- j0+N
  } # for j
}