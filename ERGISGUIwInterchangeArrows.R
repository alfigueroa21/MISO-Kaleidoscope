#-------------------------------------------------------------------------------
# Copyright © 2016 the Alliance for Sustainable Energy, LLC, All Rights Reserved
#------------------------------------------------------------------------------- 
#
# Convience functions for plotting ERGIS data
#
#
# Load and transform time-series data
#
# Code originaly written by NREL, functions edited by Zaran Claes
# This code must be sourced to be used with GUI_prime.R
# Contact me at zclaes@misoenergy.org or zclaes@iastate.edu


#----------------------------------------------------------------------------
#' Plot ERGIS generation frame
#'
#' @description Draw a 1920x1080 frame of ERGIS with generator map and
#'              a dispatch stack for a single scenario
#'
#' @param t timestep of interest
draw_ergis <- function(t, scenario='10_percent', density='None',
                       types=c("Pumped Hydro Storage","Natural Gas","Hydro","Nuclear","Coal","Other", "Solar", "Wind"),
                       scaling=0.002, weight=3, ...)
{
  par(bg='black', fg='white', mar=c(0.5,0.5,1.5,1), oma=c(2,0,0,2)) #creates the background (0.5,0.5,1.5,1)
  par(fig=c(0, 1080/2048, 0, 1))  
  draw_density(t, density, ergis_generators, ergis_generation, ergis_colors, ...)
  draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario=scenario, scaling=scaling, lx=-90, ly=45)
  draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data, scenario) #draws out the map and legend
  draw_shadow(t)
  mtext("Generation & Flow", 1, 0, at=-92)
  
  #par(fig=c(360/640, 1, 0, 1), mar=c(0,4,6,1), oma=c(5,10,0,3), las=1, new=1) #5.1,4.1,2.1,2.1 #2,0,0,2
  #draw_ergis_bars(t, scenario, weight)  #edit mar for position and sizing stuff
  #mtext("Regional dispatch", 1, 5)     #creates the bar graph
  
  # draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario=scenario, scaling=scaling, lx=-80, ly=45)
  
  par(fig=c(0,1,0,1), mar=c(1.5,0,1.5,1.5))
  mtext(paste('MISO Generation', ' (', scenarios[1,scenario], ')', sep=''), font=2, cex=1.5)
  mtext(format(t, "%m-%d-2026 %H:%M EST"), 3, -1)   #creates the text at the top of the screen
}



#----------------------------------------------------------------------------
#' Plot a series of ERGIS generation frames
#'
#' @description Plot a series of ergis 1920x1080 frames with a generator map
#'              a dispatch stack bar chart for a single scenario
#'
#' @param t0 starting timestep 
#' @param tn ending timestep 
draw_ergis_series<- function(t0, tn, prefix, scenario='c_RT_loVG', density='None',
                             types=c("Pumped Hydro Storage","Natural Gas","Hydro","Nuclear","Coal","Other", "Solar", "Wind"),
                             ...)
{
  ts = unique(c_RT_netinterchange$time)
  ts = ts[order(ts)]
  
  i0 = which.min(abs(ts-t0))
  i1 = which.min(abs(ts-tn))
  
  for (i in i0:i1)
  {
    t = ts[i]
    
    png(sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M")), width=1920, height=1080, pointsize=24)
    par(bg='black', fg='white')
    draw_ergis(t, scenario=scenario, density=density, types=types, weight=3,...)
    dev.off()
  }
}


#----------------------------------------------------------------------------
#' Stacked bar plot of ERGIS ISO generation
#'
#' @description Plot a stacked bar graph of dispatch of each ERGIS ISO for
#'              a given scenario
#'
#' @param t timestep
draw_ergis_bars <- function(t, scenario='10_percent', weight=5)
{
  #creates an array of either true or false that checks if the scenario and time match
  index = c_RT_dispatch_stack_data$time==t & c_RT_dispatch_stack_data$scenario==scenario
  #makes a data fram of dispatch for only the given time and scenario
  df = data.frame(zone=c_RT_dispatch_stack_data$zone[index],
                  type=c_RT_dispatch_stack_data$Type[index],
                  value=c_RT_dispatch_stack_data$value[index])
  #must match what's in PLOTTINGGUI.R
  types = data.table::data.table(types=c("Natural Gas", "Coal", "Nuclear", "Wind", "Solar" , "Other", "Hydro", "Pumped Storage Hydro","Curtailment"),
                                 color = c("blue","grey30","purple","green","yellow","lightblue4", "lightblue1", "pink","red"))
  
  
  
  s = tidyr::spread(df, zone, value, fill=0)
  m = as.matrix(s[,2:11])  #if number of zones is changed the second number here must be changed,(10 zones, it must be 11.  5 zones, it must be 6, etc.)
  rownames(m) = s$type
  
  missing = types$type[!types$type %in% rownames(m)]
  for (i in missing) 
  {
    m = rbind(m, 0)
    rownames(m)[dim(m)[1]] = i
  }
  
  m=m[types$type,,drop=FALSE]
  m=m[,rev(c("ND&MN", "WI","IA","IL","MO", "IN", "MI", "AR", "LA","MS"))]   #change for whatever number of regions there are
  
  # aggregate Canadian ISOs
  #m=cbind(rowSums(m[,1:5]),m)
  #colnames(m)[1] = 'Canada'   
  #m = m[,-2:-6]
  
  # plot
  # types$type[10] = "VG Curtailment"
  par(lwd=1.5)  #changes outline width on graph,  m/100 changes bar scaling
  
  
  b=barplot(m/500, col=types$color, horiz=1, xlab='GW', border = "black", xlim=c(0, 50), font.axis = 1, col.axis='black', col.ticks = 'black', col.lty = 'black',lwd=2)
  #legend("topright", bty='o', legend=c(types$type,'Load'), col=par("fg"), pt.bg=c(types$color,par('fg')), pch=c(rep(22, length(types$type)),45), pt.cex=1, cex=.83)
  
  x = as.matrix(s[s$type=='Load',2:11]) #if number of zones is changed, this number must be changed, see above, same thing
  x=x[,rev(c("ND&MN", "WI","IA","IL","MO","IN", "MI","AR","LA","MS"))]
  #x = c('Canada'=sum(x[1:5]),x)[-2:-6]
  
  for (i in 1:length(x)) lines(rep(x[i]/500,2), rep(b[i],2)+c(-0.5,0.5), type='l', lty=5, lwd=weight, col='black')
}

#this function draws comparative bars with 2 scenarios
draw_comparative_ergis_bars2 <- function(t,tt,scene1,scene2, x0=.9, x1=1, weight=3)
{
  types = data.table::data.table(types=c("CC","Hydro","CT","Nuclear","PSH","Solar DG", "Solar PV", "Wind","Coal","ST"),
                                 color = c("lightgoldenrod4","blue","lavender","red","purple","gold", "yellow3", "green","grey30","grey40"))
  
  zone = rev(c("Zone1", "Zone2","Zone3","Zone4","Zone5", "Zone6", "Zone7", "Zone8", "Zone9","Zone10"))
  #creates an index of true false if either date matches
  index = c_RT_dispatch_stack_data$time==tt | c_RT_dispatch_stack_data$time==t     #checks for the given datetime, allows the dataframe to be filled with only values that fall within the range, change if # scenarios change
  #creates a data frame for dispatch from index
  df = data.frame(zone=c_RT_dispatch_stack_data$zone[index],
                  type=c_RT_dispatch_stack_data$Type[index],
                  value=c_RT_dispatch_stack_data$value[index],
                  scenario=c_RT_dispatch_stack_data$scenario[index])
  
  s = spread(df, scenario, value, fill=0)
  #needs an extra string in it to run, I have no idea why, but it doesn't show up later, the code just needs it to not crash.
  colnames(s) = c("zone", "type", "Scene1", "Scene2","jk")   #add in scenarios here and line 187
  
  s = s %>%
    mutate(new_zone = ifelse(as.character(zone) %in% c("Zone1", "Zone2","Zone3","Zone4","Zone5", "Zone6", "Zone7", "Zone8", "Zone9","Zone10"), as.character(zone))) %>%
    group_by(new_zone, type) %>% 
    summarise(Scene1 = sum(Scene1),
              Scene2 = sum(Scene2)) %>%     #add in more scenarios here
    ungroup() #turns back into a regular data frame (not really needed most the time)
  
  colnames(s) = c("zone", "type", "Scene1", "Scene2")
  zone = rev(c("Zone1", "Zone2","Zone3","Zone4","Zone5", "Zone6", "Zone7", "Zone8", "Zone9","Zone10"))
  
  # convert to GW
  s$Scene1 = s$Scene1 / 1000                       
  s$Scene2 = s$Scene2 / 1000
  #make more for each number of scenarios
  
 
  
  for(i in 1:length(zone))   #length of zone is 10, goes down to one
  {
    m=as.matrix(s[s$zone==zone[i],3:4])     #here and on line 223, change 3:5 to 3:6 if using four scenarios
    rownames(m) = s$type[s$zone==zone[i]]
    
    missing = types$type[!types$type %in% rownames(m)]
    for (j in missing) 
    {
      m = rbind(m, 0)
      rownames(m)[dim(m)[1]] = j
    }
    
    m=m[types$type,,drop=FALSE]          #mar=c(0.1,10.1,0.1,0.1)
    
    par(fig=c(x0, x1, (i-1)/10, i/10), mar=c(0,0,0,0), oma=c(6,8,2,2), lwd=0.5, new=TRUE)
    
    if (i==1)
      b=barplot(m, col=types$color, horiz=T, xlab='MW', space=0, xlim=c(0,20), col.lab=par("fg"), col.axis=par("fg"), las=1, cex.names=0.5)
    else
      b=barplot(m, col=types$color, horiz=T, space=0, xlim=c(0,20), xaxt='n', col.lab=par("fg"), col.axis=par("fg"), las=1, cex.names=0.5)
    
    y = rep(b,each=2)+c(-0.5,0.5)
    x = rep(as.matrix(s[s$zone==zone[i],3:4]), each=2)
    #s$type=='Load' & 
    
    #lines(x, y, type='l', lty=2, lwd=1, col=par('fg'))
    
    mtext(zone[i], 2, line=4, las=1)
  }
  mtext("GW", 1, 30)
  
  par(fig=c(0,1,0,1), new=TRUE)
  legend(x=6, y=1.8,inset =0, bty='n', legend=c(types$type,'Load'), col=par("fg"), pt.bg=c(types$color,par('fg')), pch=c(rep(22, length(types$type)),45), pt.cex=1, cex=0.5, y.intersp = 2)
}

#----------------------------------------------------------------------------
#' Stacked bar plot of ERGIS ISO generation
#'
#' @description Plot a set stacked bar graphs of dispatch of each ERGIS ISO for
#'              the ERGIS scenarios
#'
#' @param t timestep 
#' 
#' function for drawing comparative bars with three, similar to comparative 2, consult comments in comp2 function for explanations
draw_comparative_ergis_bars3 <- function(t,tt,ttt,scene1,scene2,scene3, x0=.9, x1=1, weight=3)
{
  types = data.table::data.table(type=c("CC","Hydro","CT","Geothermal","IC","Int. + Ind. Loads","IGCC","Nuclear","Pumped Storage Hydro","Solar","ST","Wind"),
                                 color = c("darkorange","royalblue","lavender","tan2","powderblue","khaki","mistyrose","lawngreen","deeppink1","yellow3","red2","navy"))
  
  zone = rev(c("Zone1", "Zone2","Zone3","Zone4","Zone5", "Zone6", "Zone7", "Zone8", "Zone9","Zone10"))
  
  index = c_RT_dispatch_stack_data$time==t | c_RT_dispatch_stack_data$time==tt | c_RT_dispatch_stack_data$time==ttt         #checks for the given datetime, allows the dataframe to be filled with only values that fall within the range, change if # scenarios change
  
  df = data.frame(zone=c_RT_dispatch_stack_data$zone[index],
                  type=c_RT_dispatch_stack_data$Type[index],
                  value=c_RT_dispatch_stack_data$value[index],
                  scenario=c_RT_dispatch_stack_data$scenario[index])
  
  s = spread(df, scenario, value, fill=0)
  
  colnames(s) = c("zone", "type", "Scene1", "Scene2", "Scene3","fjk")   #add in scenarios here and line 187
  
  s = s %>%
    mutate(new_zone = ifelse(as.character(zone) %in% c("Zone1", "Zone2","Zone3","Zone4","Zone5", "Zone6", "Zone7", "Zone8", "Zone9","Zone10"), as.character(zone))) %>%
    group_by(new_zone, type) %>% 
    summarise(Scene1 = sum(Scene1),
              Scene2 = sum(Scene2), 
              Scene3 = sum(Scene3)) %>%     #add in more scenarios here
    ungroup() #turns back into a regular data frame (not really needed most the time)
  
  colnames(s) = c("zone", "type", "Scene1", "Scene2", "Scene3")
  zone = rev(c("Zone1", "Zone2","Zone3","Zone4","Zone5", "Zone6", "Zone7", "Zone8", "Zone9","Zone10"))
  
  # convert to GW
  s$Scene1 = s$Scene1 / 1000                       
  s$Scene2 = s$Scene2 / 1000
  s$Scene3 = s$Scene3 / 1000
  #make more for each number of scenarios
  
  # s$RTx10 = s$RTx10 / 100
  # s$RTx30 = s$RTx30 / 100
  # s$ITx30 = s$ITx30 / 100
  # s$lowVG = s$lowVG / 100
  
  for(i in 1:length(zone))   #length of zone is 10, goes down to one
  {
    m=as.matrix(s[s$zone==zone[i],3:5])     #here and on line 223, change 3:5 to 3:6 if using four scenarios
    rownames(m) = s$type[s$zone==zone[i]]
    
    missing = types$type[!types$type %in% rownames(m)]
    for (j in missing) 
    {
      m = rbind(m, 0)
      rownames(m)[dim(m)[1]] = j
    }
    
    m=m[types$type,,drop=FALSE]          #mar=c(0.1,10.1,0.1,0.1)
    
    par(fig=c(x0, x1, (i-1)/10, i/10), mar=c(0,0,0,0), oma=c(6,8,2,2), lwd=0.5, new=TRUE)
    
    if (i==1)
      b=barplot(m, col=types$color, horiz=T, xlab='MW', space=0, xlim=c(0,20), col.lab=par("fg"), col.axis=par("fg"), las=1, cex.names=0.5)
    else
      b=barplot(m, col=types$color, horiz=T, space=0, xlim=c(0,20), xaxt='n', col.lab=par("fg"), col.axis=par("fg"), las=1, cex.names=0.5)
    
    y = rep(b,each=2)+c(-0.5,0.5)
    x = rep(as.matrix(s[s$zone==zone[i],3:5]), each=2)
    #s$type=='Load' & 
    
    #lines(x, y, type='l', lty=2, lwd=1, col=par('fg'))
    
    mtext(zone[i], 2, line=4, las=1)
  }
  mtext("GW", 1, 30)
  
  par(fig=c(0,1,0,1), new=TRUE)
  legend(x=6, y=2.5,inset =0, bty='n', legend=c(types$type,'Load'), col=par("fg"), pt.bg=c(types$color,par('fg')), pch=c(rep(22, length(types$type)),45), pt.cex=1, cex=0.5, y.intersp = 2)
}

draw_comparative_ergis_bars4 <- function(t,tt,ttt,tttt,scene1,scene2,scene3,scene4, x0=.65, x1=.95, weight=1)
{
  #types = data.table::data.table(types=c("CC","Hydro","CT","Nuclear","PSH","Coal","ST","Solar DG", "Solar PV", "Wind"),
  #                               color = c("lightgoldenrod4","blue","lavender","red","purple","grey30","grey40","gold", "yellow3", "green"))
  types = data.table::data.table(types=c("Natural Gas", "Coal", "Nuclear", "Wind", "Solar" , "Other", "Hydro", "Pumped Storage Hydro","Curtailment"),
                                            color = c("blue","grey30","purple","green","yellow","lightblue4", "lightblue1", "hotpink","red"))
                                 
  zone = rev(c("ND&MN", "WI","IA","IL","MO", "IN", "MI", "AR", "LA","MS"))
  
  index = c_RT_dispatch_stack_data$time==t | c_RT_dispatch_stack_data$time==tt | c_RT_dispatch_stack_data$time==ttt | c_RT_dispatch_stack_data$time == tttt      #checks for the given datetime, allows the dataframe to be filled with only values that fall within the range, change if # scenarios change
  
  df = data.frame(zone=c_RT_dispatch_stack_data$zone[index],
                  type=c_RT_dispatch_stack_data$Type[index],
                  value=c_RT_dispatch_stack_data$value[index],
                  scenario=c_RT_dispatch_stack_data$scenario[index])
  
  s = spread(df, scenario, value, fill=0)
  
  colnames(s) = c("zone", "type", "x10_percent", "x20_percent", "x30_percent","x40_percent")   #add in scenarios here and line 187
  
  s = s %>%
    mutate(new_zone = ifelse(as.character(zone) %in% c("ND&MN", "WI","IA","IL","MO", "IN", "MI", "AR", "LA","MS"), as.character(zone))) %>%
    group_by(new_zone, type) %>% 
    summarise(x10_percent = sum(x10_percent),
              x20_percent = sum(x20_percent), 
              x30_percent = sum(x30_percent),
              x40_percent = sum(x40_percent)) %>%     #add in more scenarios here
    ungroup() #turns back into a regular data frame (not really needed most the time)
  
  colnames(s) = c("zone", "type", "x10_percent", "x20_percent", "x30_percent","x40_percent")   #add in scenarios here and line 187
  zone = rev(c("ND&MN", "WI","IA","IL","MO", "IN", "MI", "AR", "LA","MS"))
  
  # convert to GW
  s$x10_percent = s$x10_percent / 1000                       
  s$x20_percent = s$x20_percent / 1000
  s$x30_percent = s$x30_percent / 1000
  s$x40_percent = s$x40_percent / 1000            #make more for each number of scenarios
  
  # s$RTx10 = s$RTx10 / 100
  # s$RTx30 = s$RTx30 / 100
  # s$ITx30 = s$ITx30 / 100
  # s$lowVG = s$lowVG / 100
  
  for(i in 1:length(zone))   #length of zone is 10, goes down to one
  {
    m=as.matrix(s[s$zone==zone[i],3:6])     #here and on line 223, change 3:5 to 3:6 if using four scenarios
    rownames(m) = s$type[s$zone==zone[i]]
    
    missing = types$type[!types$type %in% rownames(m)]
    for (j in missing) 
    {
      m = rbind(m, 0)
      rownames(m)[dim(m)[1]] = j
    }
    
    m=m[types$type,,drop=FALSE]          #mar=c(0.1,10.1,0.1,0.1)
    
    par(mar=c(0.1,10.1,0.1,0.1), oma=c(6,8,2,2), fig=c(x0, x1, (i-1)/12, i/12), lwd=.1, new=TRUE)

    
    if (i==1)
      b=barplot(m, col=types$color, horiz=T, xlab='MW', space=0, xlim=c(0,40), col.lab=par("fg"), col.axis=par("fg"), las=1, cex.names=0.5)
    else
      b=barplot(m, col=types$color, horiz=T, space=0, xlim=c(0,40), xaxt='n', col.lab=par("fg"), col.axis=par("fg"), las=1, cex.names=0.5)
    
    y = rep(b,each=2)+c(-0.5,0.5)
    x = rep(as.matrix(s[s$zone==zone[i],3:6]), each=2)
    #s$type=='Load' & 
    
    #lines(x, y, type='l', lty=2, lwd=1, col=par('fg'))
    
    mtext(zone[i], 2, line=4, las=1)
  }
  #mtext("GW", .75, 28)
  mtext("Dispatch by State (GW)",1,27) # Added by ALF to keep the plots consistent
  
  par(fig=c(0,1,0,1), new=TRUE)
  #legend(x=5.75, y=4.1,inset =0, bty='n', legend=c(types$type), col=par("fg"), pt.bg=c(types$color,par('fg')), pch=c(rep(22, length(types$type)),45), pt.cex=2.5, cex=0.7, y.intersp = 1, x.intersp = 1,horiz = TRUE)
  #legend(x=1, y=4.2,inset =0, bty='n', legend=c(types$type), col=par("fg"), pt.bg=c(types$color,par('fg')), pch=c(rep(22, length(types$type)),45), pt.cex=2.5, cex=0.7, y.intersp = 1, x.intersp = 1,horiz = TRUE)
  
}

#----------------------------------------------------------------------------
#' Plot chord view of ERGIS data
#'
#' @description Draw a 1920x1080 frame of ERGIS with chord diagram and
#'              a dispatch stack for a single scenario
#'
#' @param t starting timestep
#' @param max_interchange maximum interchange to scale the chord diagram
#' This function draws out the single maps with the map, bar chart, and circle diagram
draw_ergis_chord = function(t, max_interchange=0, scenario, density='None',
                            types=c("CC","Hydro","CT Gas","Nuclear","PSH", "Solar PV", "Wind","ST Coal","ST Gas"),
                            scaling=0.002, weight=3,title = " ",note = "", ...)
{
  index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
  interchange = sum(c_RT_netinterchange$value[index])
  
  # Sanity check
  # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
  max_interchange = max(interchange, max_interchange)
  #prints the datetime
  print(format(t, "%m-%d-%Y %H:%M EST"))
  #sets the background as black
  par(bg='black', fg='white')
  #par(bg='white', fg='black')gray76
  #makes a picture that takes up the space 43% of the way from the left, and all the space top to bottom,adjust this in fig=c(x,x,x,x)
  par(fig=c(0, .43, 0, 1))  
  #draws the map 
  draw_density(t, density, ergis_generators, ergis_generation, ergis_color,...)
  #draws out the generators across the map
  draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario=scenario, scaling=scaling, lx=-107, ly=45)
  #draws the arrows showing interchange between the regions
  draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data,arrow.scaling=100, scenario= scenario) #draws out the map and legend
  #draws the shadow showing night/day
  draw_shadow(t) 
  #mtext("Generation & Flow", 10, 10) #labels the map
  
  #this figure takes up the space from 45% to the left, to all the way to the right, and half of the top to bottom space,adjust this in fig=c(x,x,x,x)
  par(fig=c(.45, 1, 0, .5), mar=c(.3,.3,.3,.3), oma=c(1.3,0,0,15), new = TRUE)
  circlize::circos.clear()
  #sets the size of the gaps between zones in the circle diagram
  circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
  #draws out the chord diagram
  draw_chord_interchange_ver2(t, ergis_iso, c_RT_netinterchange, scenario, .9)
  mtext("Net interchange", 1)
  
  #this figure takes up the top right corner, adjust this in fig=c(x,x,x,x)
  par(fig=c(.62, .9, .55, .92), mar=c(.1,.1,.1,.1), oma=c(0,0,0,0), las=1, new=TRUE)
  #draws out the bar graph
  draw_ergis_bars(t, scenario, weight)
  mtext("Regional dispatch (GW)", 1, 3,color='black')

  
  
  par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5))
  #labels the graph with name and datetime
  mtext(paste(title), font=2, cex=1.5)
  mtext(format(t, "%m-%d-%y %H:%M EST"), 3, -1)
  
  par(fig=c(.7,1,0,.4), new = TRUE)
  
  
  
  
  mtext(paste(note), cex= 1, line = -6.5, col = 'darkorange4')
  
}

#the same function but with a grayish background
draw_ergis_chord_white = function(t, max_interchange=0, scenario, density='None',
                                  types=c("Pumped Storage Hydro","Natural Gas","Hydro","Nuclear","Coal","Other", "Solar", "Wind"),
                            scaling=0.002, weight=3,title = " " ,note = "",...)
{
  index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
  interchange = sum(c_RT_netinterchange$value[index])
  
  # Sanity check
  # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
  max_interchange = max(interchange, max_interchange)
  
  #print(format(t, "%m-%d-%Y %H:%M EST"), 1, 2.5)
  #print(format(t, "%H:%M EST"), 1, 2.5)
  par(bg='white', fg='black')
  #par(bg='white', fg='black')gray76
  
  par(fig=c(0, .5, 0, 1))  
  draw_density(t, density, ergis_generators, ergis_generation, ergis_color,...)
  draw_generators_single(t, types, ergis_generators, ergis_generation, ergis_colors, scenario=scenario, scaling=scaling, lx=-107, ly=45)
  draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data,arrow.scaling=1, scenario= scenario) #draws out the map and legend
  #draw_shadow(t)
  mtext("Generation & Flow", 0, 1)
  
  par(fig=c(.6, 1, 0, .52), mar=c(.3,.3,.3,.3), oma=c(1.3,0,0,15), new = TRUE,fg='black')
  #par(bg='white', fg='black')
  circlize::circos.clear()
  circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
  draw_chord_interchange_ver2(t, ergis_iso, c_RT_netinterchange, scenario, 1.3)
  mtext("Net Interchange", 1)
  
  
  par(fig=c(.7, .9, .55, .92), mar=c(.1,.1,.1,.1), oma=c(0,0,0,0), las=1, new=TRUE, fg='black')
  #par(fg='black')
  draw_ergis_bars(t, scenario, weight)
  mtext("Regional Dispatch (GW)", 1, 2.5)
  
  
  
  par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5))
  #mtext(paste('Eastern Renewable Generation Integration Study', ' (', scenarios[1,scenario], ')', sep=''), font=2, cex=1.5)
  mtext(paste(title), font=2, cex=2, 3, -.5)
  #mtext(format(t, "%m-%d-%y %H:%M EST"), 3, -1)
  mtext(format(t, "%H:%M EST"), 3, -35)
  
  par(fig=c(.7,1,0,.4), new = TRUE)
  
  
  
  
  #mtext(paste(note), cex= 1, line = -6.5, col = 'darkorange4')
  
}

draw_ergis_chord_nihal = function(t, max_interchange=0, scenario='RTO Interchange', density='None',
                                  types=c("CC","CT Gas","CT Oil","IC Oil","Interruptible Loads","Nuclear","Solar DG","Hydro","IC Gas","IC Renewable","Pumped Storage Hydro","Solar PV","ST Coal","ST Gas","CT Renewable","ST Other","Wind","IGCC","Industrial Loads","CT Other","Geothermal","ST Renewable"),
                                  scaling=0.002, weight=3, ...)
{
  index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
  interchange = sum(c_RT_netinterchange$value[index])
  
  
  # Sanity check
  # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
  max_interchange = max(interchange, max_interchange)
  
  print(format(t, "%m-%d-%Y %H:%M EST"))
  par(bg='black', fg='white')
  #par(bg='white', fg='black')
  
  
  
  par(fig=c(0, 1, 0, 1), mar=c(0,0,0,0), oma=c(0,0,0,0), new = TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
  draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, scenario, 1.3)
  mtext("Net interchange", 1)
  
  
  
  
  
  
  par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5))
  #mtext(paste('Eastern Renewable Generation Integration Study', ' (', scenarios[1,scenario], ')', sep=''), font=2, cex=1.5)
  
  
  par(fig=c(.7,1,0,.4), new = TRUE)
  
  
  
  
  
  
}

#----------------------------------------------------------------------------
#' Plot a series of ERGIS net interchange frames
#'
#' @description Plot a series of ergis 1920x1080 frames with a chord diagram
#'              and a dispatch stack bar chart and map for a single scenario
#'
#' @param t0 starting timestep 
#' @param tn ending timestep 
#' 
draw_ergis_chord_series = function(t0, tn, max_interchange=0, scenario='10_percent', prefix,title = " ", note = " ")
{
  ts = unique(c_RT_netinterchange$time)
  ts = ts[order(ts)]
  
  i0 = which.min(abs(ts-t0))
  i1 = which.min(abs(ts-tn))
  
  for (i in i0:i1)
  {
    t = ts[i]
    
    png(sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M")), width=1920, height=1080, pointsize=24)
    draw_ergis_chord(t, max_interchange, scenario, weight=3, title = title, note = note)
    dev.off()
  }
}           

draw_ergis_chord_series_white = function(t0, tn, max_interchange=0, scenario='10_percent', prefix, title = " ", note = " ")
{
  ts = unique(c_RT_netinterchange$time)
  ts = ts[order(ts)]
  
  i0 = which.min(abs(ts-t0))
  i1 = which.min(abs(ts-tn))
  
  for (i in i0:i1)
  {
    t = ts[i]
    
    png(sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M")), width=1920, height=1080, pointsize=24)
    draw_ergis_chord_white(t, max_interchange, scenario, weight=3, title = title, note = note)
    dev.off()
  }
}           


draw_ergis_chord_series_nihal = function(t0, tn, max_interchange=0, scenario='RTO Interchange', prefix)
{
  ts = unique(c_RT_netinterchange$time)
  ts = ts[order(ts)]
  
  i0 = which.min(abs(ts-t0))
  i1 = which.min(abs(ts-tn))
  
  for (i in i0:i1)
  {
    t = ts[i]
    
    png(sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M")), width=1920, height=1080, pointsize=24)
    draw_ergis_chord_nihal(t, max_interchange, scenario, weight=3)
    dev.off()
  }
}               

#----------------------------------------------------------------------------
#' Plot comparative view of ERGIS generation data
#'
#' @description Plot a 1920x1080 frame of ERGIS generation for all four
#'              scenarios in a 2x2 pattern with a dispatch bar chart.
#' 
#' @param t starting timestep 
#' Draws out the comparative map for three scenarios
draw_ergis_comparative_map3 = function(t,tt,ttt,scene1,scene2,scene3,
                                       density='None',
                                       types=c("CC","CT Gas","CT Oil","IC Oil","Interruptible Loads","Nuclear","Solar DG","Hydro","IC Gas","IC Renewable","Pumped Storage Hydro","Solar PV","ST Coal","ST Gas","CT Renewable","ST Other","Wind","IGCC","Industrial Loads","CT Other","Geothermal","ST Renewable"),
                                       scaling=0.002, weight=1,title = " ",
                                       ...)
{
  print(format(t, "%m-%d-%Y %H:%M EST"))
  
  #draws all three of the maps for the chosen scenario
  par(cex=0.65, bg='black', fg='white')
  par(fig=c(0, .15, 0.05, 0.5), mar=c(0,0,0,0),oma=c(0,0,0,0))  #using fig=c(x,x,x,x) to try and space out all the different graphs
  
  draw_density(t, 'None', ergis_generators, ergis_generation, ergis_colors, scenario=scene1, ...)
  draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario=scene1, scaling=scaling, draw_legend = FALSE)
  draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data, '10_percent', arrow.scaling=30)
  draw_shadow(t)
  text(-88,28, scene1, cex=1)
  
  par(bg = 'transparent')
  par(fig=c(.15, .3, 0.05, .95), mar=c(0,0,0,0),oma=c(0,0,0,0), new=TRUE)
  draw_density(tt, 'None', ergis_generators, ergis_generation, ergis_colors, scenario=scene2, ...)
  draw_generators(tt, types, ergis_generators, ergis_generation, ergis_colors, scenario=scene2, scaling=scaling, draw_legend = FALSE,lx=-107, ly=40.2)
  draw_interchange(tt, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data,  '20_percent', arrow.scaling=30)
  draw_shadow(tt)
  text(-88,28, scene2, cex=1)
  
  
  par(fig=c(0, .15, 0.5, .95), mar=c(0,0,0,0),oma=c(0,0,0,0), new=TRUE)
  draw_density(ttt, 'None', ergis_generators, ergis_generation, ergis_colors, scenario=scene3, ...)
  draw_generators(ttt, types, ergis_generators, ergis_generation, ergis_colors, scenario=scene3, scaling=scaling, draw_legend = FALSE)
  draw_interchange(ttt, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data,  '30_percent', arrow.scaling=30)
  draw_shadow(ttt)
  text(-88,28, scene3, cex=1)
  
  
  # par(fig=c(.15, .3, 0.5, .95), mar=c(0,0,0,0),oma=c(0,0,0,0), new=TRUE)
  # draw_density(tttt, 'None',  ergis_generators, ergis_generation, ergis_colors, scenario='40_percent', ...)
  # draw_generators(tttt, types, ergis_generators, ergis_generation, ergis_colors, scenario='40_percent', scaling=scaling, draw_legend = FALSE)
  # draw_interchange(tttt, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data, '40_percent', arrow.scaling=20)
  # draw_shadow(tttt)
  # text(-88,28, 'scen4', cex=1)
  
  
  par(cex=1)
  par(fig=c(0, .4, 0, 1), oma=c(2,0,0,2), las=1, new=TRUE)
  mtext("Generation & Flow", 1)
  
  par(cex=1)
  par(fig=c(.37, .77, 0, 1), oma=c(2,0,0,2), las=1, new=TRUE)
  mtext("Net Interchange", 1)
  
  scenario = scene1
  max_interchange = 0;
  index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
  interchange = sum(c_RT_netinterchange$value[index])
  # Sanity check
  # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
  max_interchange = max(interchange, max_interchange)
  
  
  par(fig=c(.4, .55, 0, .45), mar=c(0,0,0,0),oma=c(2,2,2,2), new = TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
  draw_chord_interchange_ver2(t, ergis_iso, c_RT_netinterchange, scenario = scene1, weight)
  text(0,-1.2, scene1, cex=1)
  
  scenario = scene2
  max_interchange = 0;
  index = c_RT_netinterchange$time==tt & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
  interchange = sum(c_RT_netinterchange$value[index])
  # Sanity check
  # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
  max_interchange = max(interchange, max_interchange)
  
  par(fig=c(.6, .75, 0, 1), mar=c(0,0,0,0),oma=c(2,2,2,2), new = TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
  draw_chord_interchange_ver2(tt, ergis_iso, c_RT_netinterchange, scenario = scene2, weight)
  text(0,-1.2, scene2, cex=1)
  
  scenario = scene3
  max_interchange = 0;
  index = c_RT_netinterchange$time==ttt & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
  interchange = sum(c_RT_netinterchange$value[index])
  # Sanity check
  # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
  max_interchange = max(interchange, max_interchange)
  
  par(fig=c(.4, .55, .5, .95), mar=c(0,0,0,0),oma=c(2,2,2,2), new = TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
  draw_chord_interchange_ver2(ttt, ergis_iso, c_RT_netinterchange, scenario = scene3, weight)
  text(0,-1.2, scene3, cex=1)
  
  # scenario = '40_percent'
  # max_interchange = 0;
  # index = c_RT_netinterchange$time==tttt & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
  # interchange = sum(c_RT_netinterchange$value[index])
  # # Sanity check
  # # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
  # max_interchange = max(interchange, max_interchange)
  # 
  # par(fig=c(.6, .75, .5, .95), mar=c(0,0,0,0),oma=c(2,2,2,2), new = TRUE)
  # circlize::circos.clear()
  # circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
  # draw_chord_interchange_ver2(tttt, ergis_iso, c_RT_netinterchange, scenario = '40_percent', weight)
  # text(0,-1.2, 'scen_4', cex=1)
  #5.1,4.1,2.1,2.1
  par(fig=c(.9, 1, .1, .9), mar=c(0,0,0,0), oma=c(2,0,0,2), las=1, new=TRUE)  #size has to be edited in draw comparative bars.
  draw_comparative_ergis_bars3(t,tt,ttt,scene1 = scene1, scene2 = scene2, scene3 = scene3, weight=3)
  # mtext("Regional dispatch", 1, 4)
  
  par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5), oma=c(2,0,0,2))
  mtext(title, 3, -1.25, font=2, cex=1.5, outer=TRUE)
  mtext(format(t, "%m-%d-%y %H:%M EST"), 3, -2.25, outer=TRUE)
  
  
}
draw_ergis_comparative_map2 = function(t,tt,scene1,scene2,
                                       density='None',
                                       types=c("CC","CT Gas","CT Oil","IC Oil","Interruptible Loads","Nuclear","Solar DG","Hydro","IC Gas","IC Renewable","Pumped Storage Hydro","Solar PV","ST Coal","ST Gas","CT Renewable","ST Other","Wind","IGCC","Industrial Loads","CT Other","Geothermal","ST Renewable"),
                                       scaling=0.002, weight=1,title = " ",
                                       ...)
{
  print(format(t, "%m-%d-%Y %H:%M EST"))
  par(cex=0.65, bg='black', fg='white')
  par(fig=c(0, .15, 0.05, 0.5), mar=c(0,0,0,0),oma=c(0,0,0,0))
  
  # draw_density(t, 'None', ergis_generators, ergis_generation, ergis_colors, scenario=scene1, ...)
  # draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario=scene1, scaling=scaling, draw_legend = FALSE)
  # draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data, '10_percent', arrow.scaling=20)
  # draw_shadow(t)
  # text(-88,28, scene1, cex=1)
  # 
  
  par(fig=c(.15, .3, 0.05, .95), mar=c(0,0,0,0),oma=c(0,0,0,0), new=TRUE)
  draw_density(tt, 'None', ergis_generators, ergis_generation, ergis_colors, scenario=scene2, ...)
  draw_generators(tt, types, ergis_generators, ergis_generation, ergis_colors, scenario=scene2, scaling=scaling, draw_legend = FALSE,lx=-107, ly=40.2)
  draw_interchange(tt, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data,  '20_percent', arrow.scaling=20)
  draw_shadow(tt)
  text(-88,28, scene2, cex=1)
  
  
  # par(fig=c(0, .15, 0.5, .95), mar=c(0,0,0,0),oma=c(0,0,0,0), new=TRUE)
  # draw_density(ttt, 'None', ergis_generators, ergis_generation, ergis_colors, scenario='30_percent', ...)
  # draw_generators(ttt, types, ergis_generators, ergis_generation, ergis_colors, scenario='30_percent', scaling=scaling, draw_legend = FALSE)
  # #draw_interchange(ttt, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data,  '30_percent', arrow.scaling=20)
  # draw_shadow(ttt)
  # text(-88,28, 'scen3', cex=1)
  # 
  
  # par(fig=c(.15, .3, 0.5, .95), mar=c(0,0,0,0),oma=c(0,0,0,0), new=TRUE)
  # draw_density(tttt, 'None',  ergis_generators, ergis_generation, ergis_colors, scenario='40_percent', ...)
  # draw_generators(tttt, types, ergis_generators, ergis_generation, ergis_colors, scenario='40_percent', scaling=scaling, draw_legend = FALSE)
  # draw_interchange(tttt, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data, '40_percent', arrow.scaling=20)
  # draw_shadow(tttt)
  # text(-88,28, 'scen4', cex=1)
  
  
  par(cex=1)
  par(fig=c(0, .4, 0, 1), oma=c(2,0,0,2), las=1, new=TRUE)
  mtext("Generation & Flow", 1)
  
  par(cex=1)
  par(fig=c(.37, .77, 0, 1), oma=c(2,0,0,2), las=1, new=TRUE)
  mtext("Net Interchange", 1)
  
  scenario = scene1
  max_interchange = 0;
  index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
  interchange = sum(c_RT_netinterchange$value[index])
  # Sanity check
  # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
  max_interchange = max(interchange, max_interchange)
  
  
  par(fig=c(.4, .55, 0, .45), mar=c(0,0,0,0),oma=c(2,2,2,2), new = TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
  draw_chord_interchange_ver2(t, ergis_iso, c_RT_netinterchange, scenario = scene1, weight)
  text(0,-1.2, scene1, cex=1)
  
  scenario = scene2
  max_interchange = 0;
  index = c_RT_netinterchange$time==tt & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
  interchange = sum(c_RT_netinterchange$value[index])
  # Sanity check
  # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
  max_interchange = max(interchange, max_interchange)
  
  par(fig=c(.6, .75, 0, 1), mar=c(0,0,0,0),oma=c(2,2,2,2), new = TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
  draw_chord_interchange_ver2(tt, ergis_iso, c_RT_netinterchange, scenario = scene2, weight)
  text(0,-1.2, scene2, cex=1)
  
  # scenario = '30_percent'
  # max_interchange = 0;
  # index = c_RT_netinterchange$time==ttt & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
  # interchange = sum(c_RT_netinterchange$value[index])
  # # Sanity check
  # # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
  # max_interchange = max(interchange, max_interchange)
  # 
  # par(fig=c(.4, .55, .5, .95), mar=c(0,0,0,0),oma=c(2,2,2,2), new = TRUE)
  # circlize::circos.clear()
  # circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
  # draw_chord_interchange_ver2(ttt, ergis_iso, c_RT_netinterchange, scenario = '30_percent', weight)
  # text(0,-1.2, 'scen_3', cex=1)
  
  # scenario = '40_percent'
  # max_interchange = 0;
  # index = c_RT_netinterchange$time==tttt & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
  # interchange = sum(c_RT_netinterchange$value[index])
  # # Sanity check
  # # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
  # max_interchange = max(interchange, max_interchange)
  # 
  # par(fig=c(.6, .75, .5, .95), mar=c(0,0,0,0),oma=c(2,2,2,2), new = TRUE)
  # circlize::circos.clear()
  # circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
  # draw_chord_interchange_ver2(tttt, ergis_iso, c_RT_netinterchange, scenario = '40_percent', weight)
  # text(0,-1.2, 'scen_4', cex=1)
  #5.1,4.1,2.1,2.1
  par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5), oma=c(2,0,0,2), las=1, new=TRUE)  #size has to be edited in draw comparative bars.
  draw_comparative_ergis_bars2(t,tt,scene1,scene2, weight=3)
  # mtext("Regional dispatch", 1, 4)
  
  par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5), oma=c(2,0,0,2))
  mtext(title, 3, -1.25, font=2, cex=1.5, outer=TRUE)
  mtext(format(t, "%m-%d-%y %H:%M EST"), 3, -2.25, outer=TRUE)
  
  
}
# draw_ergis_comparative_map4 = function(t,tt,ttt,tttt,scene1,scene2,scene3,scene4,
#                                        density='None',
#                                        #types=c("CC","Hydro","CT Gas","Nuclear","PSH","Solar DG", "Solar PV", "Wind","ST Coal","ST Gas"),
#                                        types=c("Pumped Hydro Storage","Natural Gas","Hydro","Nuclear","Coal","Other", "Solar", "Wind"),
#                                        scaling=0.002, weight=1,title = " ",
#                                        ...)
# {
#   
#   print(format(t, "%m-%d-%Y %H:%M EST"))
#   par(cex=0.65, bg='white', fg='black')
#   
#   par(fig=c(0, .15, 0.05, 0.5), mar=c(0,0,0,0),oma=c(2,2,2,0))
#   draw_density(t, 'None', ergis_generators, ergis_generation, ergis_colors, scenario=scene1, ...)
#   draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario=scene1, scaling=scaling, draw_legend = FALSE)
#   draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data,'10_percent', arrow.scaling=10000)
#   #draw_shadow(t)
#   text(-88,28, scene1, cex=1)
# 
# 
#   par(fig=c(.15, .3, 0.05, 0.5), mar=c(0,0,0,0),oma=c(0,0,0,0),new=TRUE)
#   draw_density(tt, 'None', ergis_generators, ergis_generation, ergis_colors, scenario=scene2, ...)
#   draw_generators2(tt, types, ergis_generators, ergis_generation, ergis_colors, scenario=scene2, scaling=scaling, draw_legend = FALSE,lx=-107, ly=40.2)
#   draw_interchange(tt, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data,'20_percent', arrow.scaling=10000)
#   draw_shadow(t)
#   text(-88,28, scene2, cex=1)
# 
#   # 
#   # par(fig=c(0, .15, 0.5, .95), mar=c(0,0,0,0),oma=c(0,0,0,0), new=TRUE)
#   # draw_density(ttt, 'None', ergis_generators, ergis_generation, ergis_colors, scenario=scene3, ...)
#   # draw_generators3(ttt, types, ergis_generators, ergis_generation, ergis_colors, scenario=scene3, scaling=scaling, draw_legend = FALSE)
#   # draw_interchange(ttt, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data,'30_percent', arrow.scaling=10000)
#   # #draw_shadow(t)
#   # text(-88,28, scene3, cex=1)
#   # 
#   # 
#   par(fig=c(.15, .3, 0.5, .95), mar=c(0,0,0,0),oma=c(0,0,0,0), new=TRUE)
#   draw_density(tttt, 'None',  ergis_generators, ergis_generation, ergis_colors, scenario=scene4, ...)
#   draw_generators4(tttt, types, ergis_generators, ergis_generation, ergis_colors, scenario=scene4, scaling=scaling, draw_legend = FALSE)
#   draw_interchange(tttt, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data,'40_percent', arrow.scaling=10000)
#   #draw_shadow(t)
#   text(-88,28, scene4, cex=1)
#   
#   
#   par(cex=1)
#   par(fig=c(0, .4, 0, 1), oma=c(2,0,0,2), las=1, new=TRUE)
#   mtext("Generation & Flow", 1)
#   
#   par(cex=1)
#   par(fig=c(.37, .77, 0, 1), oma=c(2,0,0,2), las=1, new=TRUE)
#   mtext("Net Interchange", 1)
#   
#   
#   scenario = scene1
#   max_interchange = 0;
#   index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
#   interchange = sum(c_RT_netinterchange$value[index])
#   # Sanity check
#   # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
#   max_interchange = max(interchange, max_interchange)
#   
#   
#   par(fig=c(.4, .55, 0, .45), mar=c(0,0,0,0),oma=c(2,2,2,2), new = TRUE)
#   circlize::circos.clear()
#   circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
#   draw_chord_interchange_ver2(t, ergis_iso, c_RT_netinterchange, scenario = scene1, weight)
#   text(0,-1.2, scene1, cex=1)
#   
#   scenario = scene2
#   max_interchange = 0;
#   index = c_RT_netinterchange$time==tt & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
#   interchange = sum(c_RT_netinterchange$value[index])
#   # Sanity check
#   # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
#   max_interchange = max(interchange, max_interchange)
#   
#   par(fig=c(.6, .75, 0, .45), mar=c(0,0,0,0),oma=c(2,2,2,2), new = TRUE)
#   circlize::circos.clear()
#   circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
#   draw_chord_interchange_ver2(tt, ergis_iso, c_RT_netinterchange, scenario = scene2, weight)
#   text(0,-1.2, scene2, cex=1)
#   
#   scenario = scene3
#   max_interchange = 0;
#   index = c_RT_netinterchange$time==ttt & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
#   interchange = sum(c_RT_netinterchange$value[index])
#   # Sanity check
#   # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
#   max_interchange = max(interchange, max_interchange)
#   
#   par(fig=c(.4, .55, .5, .95), mar=c(0,0,0,0),oma=c(2,2,2,2), new = TRUE)
#   circlize::circos.clear()
#   circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
#   draw_chord_interchange_ver2(ttt, ergis_iso, c_RT_netinterchange, scenario = scene3, weight)
#   text(0,-1.2, scene3, cex=1)
#   
#   scenario = scene4
#   max_interchange = 0;
#   index = c_RT_netinterchange$time==tttt & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
#   interchange = sum(c_RT_netinterchange$value[index])
#   # Sanity check
#   # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
#   max_interchange = max(interchange, max_interchange)
#   
#   par(fig=c(.6, .75, .5, .95), mar=c(0,0,0,0),oma=c(2,2,2,2), new = TRUE)
#   circlize::circos.clear()
#   circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
#   draw_chord_interchange_ver2(tttt, ergis_iso, c_RT_netinterchange, scenario = scene4, weight)
#   text(0,-1.2, scene4, cex=1)
#   #5.1,4.1,2.1,2.1
#   par(fig=c(0,1,0,1), mar=c(5.1,4.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)  #size has to be edited in draw comparative bars.
#   draw_comparative_ergis_bars4(t,tt,ttt,tttt, weight=3)
#   
#   par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5), oma=c(2,0,0,2))
#   mtext(title, 3, -1.25, font=2, cex=1.5, outer=TRUE)
#   #mtext(paste('Policy Studies Department', 3, -1.25, font=2, cex=1))
#   mtext(format(t, "%m-%d-%y %H:%M EST"), 3, -2.25, outer=TRUE)
#   
#   
# }

draw_ergis_comparative_map4 = function(t,tt,ttt,tttt,scene1,scene2,scene3,scene4,
                                        density='None',
                                        #types=c("CC","Hydro","CT Gas","Nuclear","PSH","Solar DG", "Solar PV", "Wind","ST Coal","ST Gas"),
                                       types=c("Natural Gas", "Coal", "Nuclear", "Wind", "Solar" , "Other", "Hydro", "Pumped Storage Hydro","Curtailment"),
                                        scaling=0.002, weight=1,title = " ",
                                        ...)
{
  print(format(t, "%m-%d-%Y %H:%M EST"))
  par(cex=.65, bg='white', fg='black')
  
  
  par(fig=c(0, .15, 0.05, 0.5), mar=c(0,0,0,0),oma=c(0,0,0,0))
  draw_density(t, 'None', ergis_generators, ergis_generation, ergis_colors, scenario=scene1, ...)
  draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario=scene1, scaling=scaling, draw_legend = TRUE, lx=-107, ly=43)
  draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data, '10_percent', arrow.scaling=.0001)
  #draw_shadow(t)
  text(-88,29, scene1, cex=1)

  par(bg='transparent')
  par(fig=c(.15, .3, 0.05, .5), mar=c(0,0,0,0),oma=c(0,0,0,0), new=TRUE)
  draw_density(tt, 'None', ergis_generators, ergis_generation, ergis_colors, scenario=scene2, ...)
  draw_generators(tt, types, ergis_generators, ergis_generation, ergis_colors, scenario=scene2, scaling=scaling, draw_legend = TRUE, lx=-107, ly=43)
  draw_interchange(tt, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data,  '20_percent', arrow.scaling=10)
  #draw_shadow(t)
  text(-88,28, scene2, cex=1)


  par(fig=c(0, .15, 0.5, .95), mar=c(0,0,0,0), new=TRUE)
  draw_density(ttt, 'None', ergis_generators, ergis_generation, ergis_colors, scenario=scene3, ...)
  draw_generators(ttt, types, ergis_generators, ergis_generation, ergis_colors, scenario=scene3, scaling=scaling, draw_legend = TRUE, lx=-107, ly=43)
  draw_interchange(ttt, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data,  '30_percent', arrow.scaling=10000)
  #draw_shadow(t)
  text(-88,28, scene3, cex=1)


  par(fig=c(.15, .3, 0.5, .95), mar=c(0,0,0,0),oma=c(0,0,0,0),new=TRUE)
  draw_density(tttt, 'None',  ergis_generators, ergis_generation, ergis_colors, scenario=scene4, ...)
  draw_generators(tttt, types, ergis_generators, ergis_generation, ergis_colors, scenario=scene4, scaling=scaling, draw_legend = TRUE, lx=-107, ly=43)
  draw_interchange(tttt, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data, '40_percent', arrow.scaling=1)
  #draw_shadow(t)
  text(-88,28, scene4, cex=1)


  par(cex=1)
  par(fig=c(0, .3, 0, 1), oma=c(2,0,0,2), las=1, new=TRUE)
  mtext("Generation & Flow",3,-37.75)

  par(cex=1)
  par(fig=c(.3, .725, 0, 1), oma=c(2,0,0,2), las=1, new=TRUE)
  mtext("Net Interchange (GW)", 1)


  scenario = scene1
  max_interchange = 0;
  index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
  interchange = sum(c_RT_netinterchange$value[index])
  # Sanity check
  # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
  max_interchange = max(interchange, max_interchange)


  par(fig=c(.25, .55, 0, .45), mar=c(0,0,0,0),oma=c(2,2,2,2), new = TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
  draw_chord_interchange_ver2(t, ergis_iso, c_RT_netinterchange, scenario = scene1, weight)
  text(.85,-.95, scene1, cex=.75)

  scenario = scene2
  max_interchange = 0;
  index = c_RT_netinterchange$time==tt & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
  interchange = sum(c_RT_netinterchange$value[index])
  # Sanity check
  # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
  max_interchange = max(interchange, max_interchange)

  par(fig=c(.45, .75, 0, .45), mar=c(0,0,0,0),oma=c(2,2,2,2), new = TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
  draw_chord_interchange_ver2(tt, ergis_iso, c_RT_netinterchange, scenario = scene2, weight)
  text(.85,-.95, scene2, cex=.75)

  scenario = scene3
  max_interchange = 0;
  index = c_RT_netinterchange$time==ttt & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
  interchange = sum(c_RT_netinterchange$value[index])
  # Sanity check
  # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
  max_interchange = max(interchange, max_interchange)

  par(fig=c(.25, .55, .5, .95), mar=c(0,0,0,0),oma=c(2,2,2,2), new = TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
  draw_chord_interchange_ver2(ttt, ergis_iso, c_RT_netinterchange, scenario = scene3, weight)
  text(.85,-.95, scene3, cex=.75)

  scenario = scene4
  max_interchange = 0;
  index = c_RT_netinterchange$time==tttt & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
  interchange = sum(c_RT_netinterchange$value[index])
  # Sanity check
  # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
  max_interchange = max(interchange, max_interchange)

  par(fig=c(.45, .75, .5, .95), mar=c(0,0,0,0),oma=c(2,2,2,2), new = TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=10 + (348/12) * (max_interchange-interchange)/max_interchange)
  draw_chord_interchange_ver2(tttt, ergis_iso, c_RT_netinterchange, scenario = scene4, weight)
  text(.85,-.95, scene4, cex=.75)
  #5.1,4.1,2.1,2.1
  par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5), oma=c(2,0,0,2), las=1, new=TRUE)  #size has to be edited in draw comparative bars.
  draw_comparative_ergis_bars4(t,tt,ttt,tttt, weight=3)

  par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5), oma=c(2,0,0,2))
  mtext(title, 3, -2, font=2, cex=1.5, outer=TRUE)
  #mtext(paste('Policy Studies Department', 3, -1.25, font=2, cex=1))
  mtext(format(t, "%m-%d-%y %H:%M EST"), 3, -3.2, outer=TRUE)

  
}



#I added this comp_map_series, but I'm not a pro at this, IDK if all the variables need to be in this function, but I left them in.  WHen implementing them just use time1, time2, and prefix, it'll be fine.
draw_ergis_comparative_map_series2 = function(t0,tn,scene1,scene2,prefix,
                                              density='None',
                                              types=c("CC","Hydro","CT","IC Oil","IC Renewable","IGCC","Industrial Loads","Interruptible Loads","Solar DG","Solar PV","ST Coal","ST Other","ST Renewable","Wind","Nuclear","PS Hydro","IC Gas","Geothermal","Qualifying Facilities"),
                                              scaling=0.002, weight=1,title = " ",
                                              ...)
  
{
  
  
  index = c_RT_netinterchange$scenario==scene1      #separates different dates and times if different scenarios have different datetimes, can add extra in same format for any extra scenarios
  df = data.frame(time=c_RT_netinterchange$time[index])
  
  index_20 = c_RT_netinterchange$scenario==scene2              #creates data frame for second scenario
  df_20 = data.frame(time=c_RT_netinterchange$time[index_20])  #we need to do this since scenarios may have different datetimes
  
  
  
  
  
  #ts = unique(c_RT_netinterchange$time)
  #ts = ts[order(ts)]
  df = as.matrix(df)    #dataframe has to be turned into a matrix to be sent through order function
  ts = unique(df)       #gets rid of duplicates in the first data frame and puts them in order
  ts = ts[order(ts)]
  ts = as.POSIXlt(ts)
  df_20 = as.matrix(df_20)  #dataframe for scenario two sorted in the same way, we 
  ts_20 = unique(df_20)       
  ts_20 = ts_20[order(ts_20)]
  ts_20 = as.POSIXlt(ts_20)
  
  
  
  
  # for (i in 1:l){
  #   if (!is.null(ts[i])&(ts[i]=="2017-01-15 00:00:00" | ts[i] == "2017-12-31 00:00:00")){
  #     ts[i] = t0
  #   }
  #   else if(!is.null(ts[i])&(ts[i]=="2017-01-15 1:00:00" | ts[i] == "2017-12-31 1:00:00")){
  #     ts[i] = tn
  #   }
  # }
  
  i0 = which.min(abs(ts-t0))    #assuming the range of dates is the same across all scenarios(Example: 2017-01-01 1:00 Am to 3:00 PM, and 2017-05-05 1:00 AM to 3:00 PM), we only need this for one scenario
  i1 = which.min(abs(ts-tn))
  
  
  for (i in i0:i1)
  {
    
    t = ts[i]
    tt = ts_20[i]            #needed to enter all the dates into draw_comparative_map
    
    
    
    png(sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M")), width=5760, height=2400, pointsize=52)
    draw_ergis_comparative_map2(t,tt,scene1,scene2, title = title)
    dev.off()
    
  }
  
}
draw_ergis_comparative_map_series3 = function(t0,tn,prefix,scene1,scene2,scene3,
                                              density='None',
                                              types=c("CC","Hydro","CT","IC Oil","IC Renewable","IGCC","Industrial Loads","Interruptible Loads","Solar DG","Solar PV","ST Coal","ST Other","ST Renewable","Wind","Nuclear","PS Hydro","IC Gas","Geothermal","Qualifying Facilities"),
                                              scaling=0.002, weight=1, title = " ",
                                              ...)
  
{
  
  
  index = c_RT_netinterchange$scenario==scene1      #separates different dates and times if different scenarios have different datetimes, can add extra in same format for any extra scenarios
  df = data.frame(time=c_RT_netinterchange$time[index])
  
  index_20 = c_RT_netinterchange$scenario==scene2
  df_20 = data.frame(time=c_RT_netinterchange$time[index_20])
  
  index_30 = c_RT_netinterchange$scenario==scene3
  df_30 = data.frame(time=c_RT_netinterchange$time[index_30])
  
  
  
  
  #ts = unique(c_RT_netinterchange$time)
  #ts = ts[order(ts)]
  df = as.matrix(df)    #dataframe has to be turned into a matrix to be sent through order function
  ts = unique(df)       #gets rid of duplicates in the first data frame and puts them in order
  ts = ts[order(ts)]
  ts = as.POSIXlt(ts)
  df_20 = as.matrix(df_20)
  ts_20 = unique(df_20)       
  ts_20 = ts_20[order(ts_20)]
  ts_20 = as.POSIXlt(ts_20)
  df_30 = as.matrix(df_30)
  ts_30 = unique(df_30)      
  ts_30 = ts_30[order(ts_30)]
  ts_30 = as.POSIXlt(ts_30)
  
  
  
  # for (i in 1:l){
  #   if (!is.null(ts[i])&(ts[i]=="2017-01-15 00:00:00" | ts[i]=="2017-12-31 00:00:00")){
  #     ts[i] = t0
  #   }
  #   else if(!is.null(ts[i])&(ts[i]=="2017-01-15 1:00:00" | ts[i] == "2017-12-31 1:00:00")){
  #     ts[i] = tn
  #   }
  # }
  
  i0 = which.min(abs(ts-t0))    #assuming the range of dates is the same across all scenarios(Example: 2017-01-01 1:00 Am to 3:00 PM, and 2017-05-05 1:00 AM to 3:00 PM), we only need this for one scenario
  i1 = which.min(abs(ts-tn))
  
  
  for (i in i0:i1)
  {
    
    t = ts[i]
    tt = ts_20[i]            #needed to enter all the dates into draw_comparative_map
    ttt = ts_30[i]
    
    
    png(sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M")), width=5760, height=2400, pointsize=52)
    draw_ergis_comparative_map3(t,tt,ttt,scene1,scene2,scene3, title = title)
    dev.off()
    
  }
  
}

draw_ergis_comparative_map_series4_white = function(t0,tn,prefix,scene1,scene2,scene3,scene4,
                                              density='None',
                                              types=c("Pumped Hydro Storage","Natural Gas","Hydro","Nuclear","Coal","Other", "Solar", "Wind"),
                                              #types=c("CC","Hydro","CT","Nuclear","PSH","Solar DG", "Solar PV", "Wind","Coal","ST"),
                                              scaling=0.002, weight=1, title = " ",
                                              ...)
  
{
  
  
  index = c_RT_netinterchange$scenario==scene1      #separates different dates and times if different scenarios have different datetimes, can add extra in same format for any extra scenarios
  df = data.frame(time=c_RT_netinterchange$time[index])
  
  index_20 = c_RT_netinterchange$scenario==scene2
  df_20 = data.frame(time=c_RT_netinterchange$time[index_20])
  
  index_30 = c_RT_netinterchange$scenario==scene3
  df_30 = data.frame(time=c_RT_netinterchange$time[index_30])
  
  index_40 = c_RT_netinterchange$scenario==scene4
  df_40 = data.frame(time=c_RT_netinterchange$time[index_40])
  
  
  #ts = unique(c_RT_netinterchange$time)
  #ts = ts[order(ts)]
  df = as.matrix(df)    #dataframe has to be turned into a matrix to be sent through order function
  ts = unique(df)       #gets rid of duplicates in the first data frame and puts them in order
  ts = ts[order(ts)]
  ts = as.POSIXlt(ts)
  df_20 = as.matrix(df_20)
  ts_20 = unique(df_20)       
  ts_20 = ts_20[order(ts_20)]
  ts_20 = as.POSIXlt(ts_20)
  df_30 = as.matrix(df_30)
  ts_30 = unique(df_30)      
  ts_30 = ts_30[order(ts_30)]
  ts_30 = as.POSIXlt(ts_30)
  df_40 = as.matrix(df_40)
  ts_40 = unique(df_40)      
  ts_40 = ts_40[order(ts_40)]
  ts_40 = as.POSIXlt(ts_40)
  
  
  # for (i in 1:l){
  #   if (!is.null(ts[i])&(ts[i]=="2017-01-15 00:00:00" | ts[i]=="2017-12-31 00:00:00")){
  #     ts[i] = t0
  #   }
  #   else if(!is.null(ts[i])&(ts[i]=="2017-01-15 1:00:00" | ts[i] == "2017-12-31 1:00:00")){
  #     ts[i] = tn
  #   }
  # }
  
  i0 = which.min(abs(ts-t0))    #assuming the range of dates is the same across all scenarios(Example: 2017-01-01 1:00 Am to 3:00 PM, and 2017-05-05 1:00 AM to 3:00 PM), we only need this for one scenario
  i1 = which.min(abs(ts-tn))
  
  
  for (i in i0:i1)
  {
    
    t = ts[i]
    tt = ts_20[i]            #needed to enter all the dates into draw_comparative_map
    ttt = ts_30[i]
    tttt = ts_40[i]
    
    png(sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M")), width=5760, height=2400, pointsize=52)
    draw_ergis_comparative_map4(t,tt,ttt,tttt,scene1,scene2,scene3,scene4, title = title)
    dev.off()
    
  }
  
}
#----------------------------------------------------------------------------
#' Plot comparative view of ERGIS net interchange data
#'
#' @description Plot a 1920x1080 frame of ERGIS net interchange for all four
#'              scenarios in a 2x2 pattern of chord diagrams with a dispatch
#'              bar chart.
#'
#' @param t starting timestep 
draw_ergis_comparative = function(t, max_interchange=0)
{
  w = 540/1920
  h = 0.5
  
  index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario=='c_RT_R30P' & c_RT_netinterchange$value > 0
  r30p_interchange = sum(c_RT_netinterchange$value[index])
  index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario=='c_RT_N30P' & c_RT_netinterchange$value > 0
  n30p_interchange = sum(c_RT_netinterchange$value[index])
  index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario=='c_RT_loVG' & c_RT_netinterchange$value > 0
  lovg_interchange = sum(c_RT_netinterchange$value[index])
  index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario=='c_RT_SRPS' & c_RT_netinterchange$value > 0
  srps_interchange = sum(c_RT_netinterchange$value[index])
  
  # Sanity check
  # if (max_interchange < max(r30p_interchange, n30p_interchange, lovg_interchange, srps_interchange)) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
  max_interchange = max(max_interchange, r30p_interchange, n30p_interchange, lovg_interchange, srps_interchange)
  
  print(format(t, "%m-%d-%Y %H:%M EST"))
  par(fig=c(0, w, 0, h), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0))    
  circlize::circos.clear()
  circlize::circos.par(gap.degree=1 + (348/12) * (max_interchange-r30p_interchange)/max_interchange)
  draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, 'c_RT_R30P')    
  text(-0.90,-0.90, 'R30P', cex=1)
  
  par(fig=c(w, 2*w, 0, h), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0), new=TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=1 + (348/12) * (max_interchange-n30p_interchange)/max_interchange)
  draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, 'c_RT_N30P')   
  text(-0.90,-0.90, 'N30P', cex=1)
  
  par(fig=c(0, w, h, 2*h), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0), new=TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=1 + (348/12) * (max_interchange-lovg_interchange)/max_interchange)
  draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, 'c_RT_loVG')    
  text(-0.90,-0.90, 'loVG', cex=1)
  
  par(fig=c(w, 2*w, h, 2*h), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0), new=TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=1 + (348/12) * (max_interchange-srps_interchange)/max_interchange)
  draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, 'c_RT_SRPS')    
  text(-0.90,-0.90, 'SRPS', cex=1)
  
  par(cex=1)
  par(fig=c(0, 2*w, 0, 2*h), oma=c(2,0,0,2), las=1, new=TRUE)
  mtext("Net interchange", 1)
  
  par(fig=c(1080/1920, 1, 0, 1), mar=c(5.1,4.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
  par(fig=c(2*w, 1, 0, 1), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0))
  draw_comparative_ergis_bars(t, weight=3)
  mtext("Regional dispatch", 1, 4)
  
  par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5), oma=c(2,0,0,2))
  mtext('ERGIS', 3, -1.25, font=2, cex=1.5, outer=TRUE)
  mtext(format(t, "%m-%d-2026 %H:%M EST"), 3, -2.25, outer=TRUE)
}



#----------------------------------------------------------------------------
#' Plot ERGIS layout for Insight Center 
#'
#' @description Plot a 5760x2400 frame of ERGIS generation, net interchange
#'              and dispatch for a single scenario
#'
#' @param t timestep of interest
draw_ergis_insight <- function(t, scenario='c_RT_loVG', density='None',
                               types=c("CC","CT","Hydro", "IC", "IGCC","Industrial Loads","Interruptible Loads", "Nuclear", "PS Hydros", "Qualifying Facilities","Solar DG", "Solar PV", "ST Coal", "ST Other", "ST Renewable", "Other"),
                               max_interchange=0,
                               scaling=0.002, weight=6, ...)
{
  par(bg='black', fg='white')
  par(fig=c(0, 2400/5760, 0, 1))
  draw_density(t, density, ergis_generators, ergis_generation, ergis_colors, scenario=scenario, ...)
  draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario=scenario, scaling=scaling)
  draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data, scenario)
  draw_shadow(t)
  mtext("Generation & Flow", 1)
  
  index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
  interchange = sum(c_RT_netinterchange$value[index])
  
  max_interchange = max(max_interchange, interchange)
  
  par(fig=c(2200/5760, 4600/5760, 0, 1), mar=c(0.5,0.5,1.5,1), oma=c(2,0,0,2), new=TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=1 + (348/12) * (max_interchange-interchange)/max_interchange)
  draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, scenario, weight/3)
  mtext("Net interchange", 1)
  
  par(fig=c(4550/5760, 1, 0, 1), mar=c(5.1,4.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
  draw_ergis_bars(t, scenario, weight)
  mtext("Regional dispatch", 1, 5)
  
  par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5))
  mtext(paste('ERGIS', scenarios[1,scenario]), font=2, cex=1.5)
  mtext(format(t, "%m-%d-2026 %H:%M EST"), 3, -1)
}

#----------------------------------------------------------------------------
#' Plot insight-center layout series of ERGIS data
#'
#' @description Plot a series of 5760x2400 frames of ERGIS generation,
#'              net interchange and dispatch for a single scenario
#'
#' @param t0 starting timestep 
#' @param tn ending timestep 
draw_ergis_insight1_series = function(t0, tn, max_interchange=0, scenario='c_RT_loVG', prefix)
{
  i0 = which.min(abs(ts-t0))
  i1 = which.min(abs(ts-tn))
  
  for (i in i0:i1)
  {
    t = ts[i]
    
    png(sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M")), width=5760, height=2400, pointsize=52)
    draw_ergis_insight(t, max_interchange, scenario)
    dev.off()
  }
}               

#----------------------------------------------------------------------------
#' Plot ERGIS layout for scenario comparison in the Insight Center 
#'
#' @description Plot a 5760x2400 frame comparing ERGIS generation, net interchange
#'              and dispatch for all four scenarios
#'
#' @param t timestep of interest
draw_ergis_comparative_insight <- function(t,
                                           density='None',
                                           max_interchange=0,
                                           types=c("CC","CT","Hydro", "IC", "IGCC","Industrial Loads","Interruptible Loads", "Nuclear", "PS Hydros", "Qualifying Facilities","Solar DG", "Solar PV", "ST Coal", "ST Other", "ST Renewable", "Other"),
                                           scaling=0.002, weight=3, ...)
{
  par(bg='black', fg='white')
  #par(bg='white', fg='black')
  par(fig=c(0, 2400/5760, 0, 1))
  print(format(t, "%m-%d-%Y %H:%M EST"))
  print(date())
  
  par(fig=c(0, 1200/5760, 0.1, 0.55), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0))
  draw_density(t, 'None', ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_R30P')
  draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_R30P', scaling=scaling)
  draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data,  'c_RT_R30P', arrow.scaling=4.0)
  draw_shadow(t)
  text(-105,25, 'RTx30', cex=0.75)
  
  par(fig=c(1200/5760, 2400/5760, 0.1, 0.55), mar=c(0.5,0.5,0.5,0.5), new=TRUE)
  draw_density(t, 'None', ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_N30P')
  draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_N30P', scaling=scaling)
  draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data, 'c_RT_N30P', arrow.scaling=4.0)
  draw_shadow(t)
  text(-105,25, 'ITx30', cex=0.75)
  
  par(fig=c(0, 1200/5760, 0.55, 1), mar=c(0.5,0.5,0.5,0.5), new=TRUE)
  draw_density(t, 'None', ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_loVG')
  draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_loVG', scaling=scaling)
  draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data, 'c_RT_loVG', arrow.scaling=4.0)
  draw_shadow(t)
  text(-105,25, 'lowVG', cex=0.75)
  
  par(fig=c(1200/5760, 2400/5760, 0.55, 1), mar=c(0.5,0.5,0.5,0.5), new=TRUE)
  draw_density(t, 'None', ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_SRPS')
  draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_SRPS', scaling=scaling)
  draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data, 'c_RT_SRPS', arrow.scaling=4.0)
  draw_shadow(t)
  text(-105,25, 'RTx10', cex=0.75)
  
  par(cex=1)
  par(fig=c(0, 2400/5760, 0, 1), oma=c(2,0,0,2), las=1, new=TRUE)
  mtext("Generation & Flow", 1)
  
  print("interchange")
  print(date())
  
  w = 1200/5760
  h = 0.5
  
  index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario=='c_RT_R30P' & c_RT_netinterchange$value > 0
  r30p_interchange = sum(c_RT_netinterchange$value[index])
  index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario=='c_RT_N30P' & c_RT_netinterchange$value > 0
  n30p_interchange = sum(c_RT_netinterchange$value[index])
  index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario=='c_RT_loVG' & c_RT_netinterchange$value > 0
  lovg_interchange = sum(c_RT_netinterchange$value[index])
  index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario=='c_RT_SRPS' & c_RT_netinterchange$value > 0
  srps_interchange = sum(c_RT_netinterchange$value[index])
  
  max_interchange = max(max_interchange, max(r30p_interchange, n30p_interchange, lovg_interchange, srps_interchange)) # per timestep normalization
  
  x0 = 2300/5760
  x1 = 3500/5760
  y0 = 0
  y1 = 0.49
  
  par(cex=0.65)
  par(fig=c(x0, x1, y0, y1), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0), new=TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=1 + (348/12) * (max_interchange-r30p_interchange)/max_interchange)
  draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, 'c_RT_R30P',link.size=2)    
  text(-0.90,-0.90, 'RTx30', cex=1)
  
  x0 = 3500/5760
  x1 = 4700/5760
  y0 = 0
  y1 = 0.49
  par(fig=c(x0, x1, y0, y1), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0), new=TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=1 + (348/12) * (max_interchange-n30p_interchange)/max_interchange)
  draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, 'c_RT_N30P',link.size=2)   
  text(-0.90,-0.90, 'ITx30', cex=1)
  
  x0 = 2300/5760
  x1 = 3500/5760
  y0 = 0.49
  y1 = 0.98
  par(fig=c(x0, x1, y0, y1), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0), new=TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=1 + (348/12) * (max_interchange-lovg_interchange)/max_interchange)
  draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, 'c_RT_loVG',link.size=2)    
  text(-0.90,-0.90, 'lowVG', cex=1)
  
  x0 = 3500/5760 
  x1 = 4700/5760 
  y0 = 0.49
  y1 = 0.98
  par(fig=c(x0, x1, y0, y1), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0), new=TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=1 + (348/12) * (max_interchange-srps_interchange)/max_interchange)
  draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, 'c_RT_SRPS',link.size=2)    
  text(-0.90,-0.90, 'RTx10', cex=1)
  
  par(cex=1)
  par(fig=c(2400/5760, 2400/5760 + 2*w, 0, 2*h), oma=c(2,0,0,2), las=1, new=TRUE)
  mtext("Net interchange", 1)
  
  par(fig=c(4500/5760, 1, 0, 1), mar=c(5.1,4.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
  draw_comparative_ergis_bars(t, x0=4500/5760, weight=weight)
  mtext("Regional dispatch", 1, 4)
  
  par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5), oma=c(2,0,0,2))
  mtext('Eastern Renewable Generation Integration Study', 3, -1.25, font=2, cex=1.25, outer=TRUE)
  mtext(format(t, "%m-%d-2026 %H:%M EST"), 3, -2.25, cex=1.25, outer=TRUE)
}

#----------------------------------------------------------------------------
#' Plot a series ERGIS layout for scenario comparison in the Insight Center 
#'
#' @description Plot a series of 5760x2400 frames comparing ERGIS generation,
#'              net interchange and dispatch for all four scenarios
#'
#' @param t0 starting timestep 
#' @param tn ending timestep 
draw_comparative_insight_series = function(t0, tn, prefix, max_interchange=0)
{
  ts = unique(c_RT_netinterchange$time)
  ts = ts[order(ts)]
  
  i0 = which.min(abs(ts-t0))
  i1 = which.min(abs(ts-tn))
  
  for (i in i0:i1)
  {
    t = ts[i]
    
    png(sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M")), width=5760, height=2400, pointsize=52)
    draw_comparative_insight(t, 'None', max_interchange)
    dev.off()
  }
}      
