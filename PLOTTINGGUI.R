###################################################################################
#Original code written by Zaran Claes, using modified functions provided by NREL. #
#                                                                                 #
#                                                                                 #
#                                                                                 #
#This code is to be used in conjunction with ERGISGUI.R, KALEIDESCOPEGUI.R, and   #
#GUI_prime.R.  This code provides a function called mapping that creates png      #
#files.  The layout of the files and the data used to create them is determined   #
#by the user editing the GUI                                                      #
#                                                                                 # 
#                                                                                 #
#For further explanation on this code and the functions used,  either look at     #
#ERGISGUI.R and KALEIDESCOPEGUI.R                                                 #
#or contact me at  either zclaes@misoenergy.org or zclaes@iastate.edu             #
###################################################################################







library("archdata") # where the data comes from
library("sp") # for spatial object
library("leaflet") # for interactive mapping
library("rgdal") # for geographic transformations
library("rgeos") # for geographic operations
library("spatstat")  # Needed for the dirichlet tesselation function
library("rworldmap") # library rworldmap provides different types of global maps, e.g:
library("ggmap") # for a static map example
library("gstat") # for IDW model
library("leaflet") # for interactive mapping
library("mapview") # for interactive mapping test with mapview(breweries91)
library("raster") # for raster operations
library(devtools)
library(data.table)
library(sp)
library(maptools)
library(NightDay)
library(scales)
library(tidyr)
library(raster)
library(MASS)
library(RColorBrewer)
library(igraph)
library(circlize)
library(dplyr)
library(magrittr)  #warnings occur with 3.3.3 to 3.3.2, but doesn't cause any errors
library(tcltk2)
#function checks the format of the date
IsDate <- function(mydate, date.format = "%Y-%m-%d %H:%M"){
  tryCatch(!is.na(as.Date(mydate, date.format)),  
           error = function(err) {FALSE})  
}
                                                                                                                                                                              #this is for colors.R
mapping <- function(a,b,c,d,tn,tnn,tw,twn,th,thn,fo,fon,fi,fin,si,sin,se,sen,ei,ein,ni,nin,on,onn,int,dis,map,gen,cen,name,title,note,nums,scene1,scene2,scene3,scene4,color)#,  typ1,typ2,typ3,typ4,typ5,typ6,typ7,typ8,typ9,typ10,typ11,typ12,typ13,typ14,typ15,typ16,typ17,typ18,typ19,typ20,typ21,typ22,col1,col2,col3,col4,col5,col6,col7,col8,col9,col10,col11,col12,col13,col14,col15,col16,col17,col18,col19,col20,col21,col22
{
 
  
 #takes in the number of scenarios, and only loads files up to that number.
  numb <- as.numeric(nums)
  if(numb > 0){
    ergis_generation <<- load_generation(tn,tnn)
  }
  if(numb > 1){
    ergis_generation <<- rbind(ergis_generation, load_generation(tw,twn))
  }
  if(numb > 2){
    ergis_generation <<- rbind(ergis_generation, load_generation(th,thn))
  }
  if(numb > 3){
    ergis_generation <<- rbind(ergis_generation, load_generation(fo,fon))
  }
  if(numb > 4){
    ergis_generation <<- rbind(ergis_generation, load_generation(fi,fin))
  }
  if(numb > 5){
    ergis_generation <<- rbind(ergis_generation, load_generation(si,sin))
  }
  if(numb > 6){
    ergis_generation <<- rbind(ergis_generation, load_generation(se,sen))
  }
  if(numb > 7){
    ergis_generation <<- rbind(ergis_generation, load_generation(ei,ein))
  }
  if(numb > 8){
    ergis_generation <<- rbind(ergis_generation, load_generation(ni,nin))
  }
  if(numb > 9){
    ergis_generation <<- rbind(ergis_generation, load_generation(on,onn))
  }
 
  #reads in the csv file for interchange and dispatch and assigns data frame to a variable
  #IMPORTANT NOTE: Dates in these files MUST be in format yyyy-mm-dd hh:mm
  c_RT_netinterchange <<- read.csv(int)
  #checks if the date is in the correct format, gives the user a warning pop-up window if it's not
  intDate = IsDate(c_RT_netinterchange$time) 
  if(!intDate){
    tkmessageBox(message="Dates in the interchange file are incorrectly formatted\nFormat must be yyyy-mm-dd hh:mm", icon= "warning",type = "ok")
  }
  c_RT_netinterchange$time <<- as.POSIXlt(format(c_RT_netinterchange$time, format = "%Y-%m-%d %H:%M", tz = "EST"))
  c_RT_dispatch_stack_data<<- read.csv(dis)
  disDate = IsDate(c_RT_dispatch_stack_data$time)
  if(!disDate){
    tkmessageBox(message="Dates in the dispatch file are incorrectly formatted\nFormat must be yyyy-mm-dd hh:mm", icon= "warning",type = "ok")
  }
  c_RT_dispatch_stack_data$time <<- as.POSIXlt(format(c_RT_dispatch_stack_data$time, format = "%Y-%m-%d %H:%M", tz = "EST"))  #converts all the time data to proper format
 
  #reads in the file for the map 
  regions<<-readShapeSpatial(map,proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
  #miso_path = 'I:/STP/PROMOD/Armando Figueroa/MISO Shape Files/'
  #lakes=maptools::readShapeSpatial(file.path(miso_path,'MISO_only_lakes_region.shp'), proj4string=sp::CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')) 
  
  #reads in the file for generators, this file contains the lat/long of each generator along with type of each generator
  ergis_generators <<- generators <<- read.csv(gen, header = TRUE, sep=',')
  #reads in csv file with location of different zones, or RTOs, or whatever is being defined
  centroids <<- read.csv(cen, header=T)
  ergis_layout = layout = as.matrix(centroids[,2:3])
  ergis_layout <<- as.matrix(centroids[,2:3])
  ergis_verts <<- sapply(centroids$ISO, toString)
  
  scenarios <<- data.frame(scene1=scene1, scene2=scene2, scene3=scene3, scene4=scene4)
  #defines the types of generation being mapped out, and the corresponding color for the legend
  #this data table of type and color must be redefined anywhere it pops up in ERGISGUI.R
  ergis_colors <<- data.table(types=c("Natural Gas", "Coal", "Nuclear", "Wind", "Solar" , "Other", "Hydro", "Pumped Storage Hydro","Curtailment"),
                              color = c("blue","grey30","purple","green","yellow","lightblue4", "lightblue1", "hotpink","red"))
  
# ergis_colors <<- data.table(type=c("CC","Hydro","CT","Geothermal","IC","Int. + Ind. Loads","IGCC","Nuclear","Pumped Storage Hydro","Solar","ST Gas","Wind","Solar PV","Solar DG","ST Coal"),
#                              color = c("grey30","blue","grey30","grey130","grey30","grey30","grey30","grey30","purple","gold","grey30","chartreuse","gold","grey30","grey30"))
  
  
  #ergis_iso should have the same zone/RTO names that are in centroids
  ergis_iso <<- c("ND&MN", "WI","IA","IL","MO", "IN", "MI", "AR", "LA","MS","North","South")
 
 #can make white background maps or black
if (color == "Black"){ 
  #checks what the user selected (single,comp2,comp3, etc). If single is chosen, it checks which scene was selected
  if (a =="Single"){
    if(b == "Scene1" & numb > 0){
      
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series(t,t2,max_interchange=0, scenario=tnn,name,title = title,note = note)  #function that creates the png files
    }
    if(b == "Scene1" & numb < 1){   #part of the error checking, creates a pop-up window, implemented for all scenes
      tkmessageBox(message="There are no Scenarios", icon= "warning",type = "ok")
      
    }
    if(b == "Scene2" & numb > 1){
      
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series(t,t2,max_interchange=0, scenario=twn,name,title = title,note = note)
    }
    if(b == "Scene2" & numb < 2){
      tkmessageBox(message="There aren't 2 Scenarios", icon= "warning",type = "ok")
      
    }
    if(b == "Scene3" & numb > 2){
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series(t,t2,max_interchange=0, scenario=thn,name,title = title,note = note)
    }
    if(b == "Scene3" & numb < 3){
      tkmessageBox(message="There aren't 3 Scenarios", icon= "warning",type = "ok")
      
    }
    if(b == "Scene4" & numb > 3){
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series(t,t2,max_interchange=0, scenario=fon,name,title = title,note = note)
    }
    if(b == "Scene4" & numb < 4){
      tkmessageBox(message="There aren't 4 Scenarios", icon= "warning",type = "ok")
      
    }
    if(b == "Scene5" & numb > 4){
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series(t,t2,max_interchange=0, scenario=fin,name,title = title,note = note)
    }
    if(b == "Scene5" & numb < 5){
      tkmessageBox(message="There aren't 5 Scenarios", icon= "warning",type = "ok")
      
    }
    if(b == "Scene6" & numb > 5){
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series(t,t2,max_interchange=0, scenario=sin,name,title = title,note = note)
    }
    if(b == "Scene6" & numb < 6){
      tkmessageBox(message="There aren't 6 Scenarios", icon= "warning",type = "ok")
      
    }
    if(b == "Scene7" & numb > 6){
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series(t,t2,max_interchange=0, scenario=sen,name,title = title,note = note)
    }
    if(b == "Scene7" & numb < 7){
      tkmessageBox(message="There aren't 7 Scenarios", icon= "warning",type = "ok")
      
    }
    if(b == "Scene8" & numb > 7){
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series(t,t2,max_interchange=0, scenario=ein,name,title = title,note = note)
    }
    if(b == "Scene8" & numb < 8){
      tkmessageBox(message="There aren't 8 Scenarios", icon= "warning",type = "ok")
      
    }
    if(b == "Scene9" & numb > 8){
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series(t,t2,max_interchange=0, scenario=nin,name,title = title,note = note)
    }
    if(b == "Scene9" & numb < 9){
      tkmessageBox(message="There aren't 9 Scenarios", icon= "warning",type = "ok")
      
    }
    if(b == "Scene10" & numb > 9){
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series(t,t2,max_interchange=0, scenario=onn,name,title = title,note = note)
    }
    if(b == "Scene10" & numb < 10){
      tkmessageBox(message="There aren't 10 Scenarios", icon= "warning",type = "ok")
      
    }
    
  }
  
  if(a=="Comparative3" & numb > 2){
   
    t = as.POSIXlt(c, tz="EST")
    t2 = as.POSIXlt(d, tz= "EST")
    draw_ergis_comparative_map_series3(t,t2,name,scene1,scene2,scene3,title = title)
    
  }
  if(a=="Comparative3" & numb < 3){
    
    tkmessageBox(message="There Aren't 3 Scenarios", icon= "warning",type = "ok")
  }
  
  if(a=="Comparative4" & numb > 3){
   
    t = as.POSIXlt(c, tz="EST")
    t2 = as.POSIXlt(d, tz= "EST")
    draw_ergis_comparative_map_series4(t,t2,name,scene1,scene2,scene3,scene4,title = title)
  }
  if(a=="Comparative4" & numb < 4){
    
    tkmessageBox(message="There Aren't 4 Scenarios", icon= "warning",type = "ok")
  }
  
  if(a == "Comparative2" & numb > 1){
    t = as.POSIXlt(c, tz="EST")
    t2 = as.POSIXlt(d, tz= "EST")
    draw_ergis_comparative_map_series2(t,t2,scene1,scene2,name,title = title)
  }
  
  if(a=="Comparative4" & numb < 2){
    
    tkmessageBox(message="There Aren't 2 Scenarios", icon= "warning",type = "ok")
  }
  if(a=="Heatmap"){
    
    contour()
  }
}

else if(color == "White"){
  if (a =="Single"){
    if(b == "Scene1" & numb > 0){
      
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series_white(t,t2,max_interchange=0, scenario=tnn,name,title = title,note = note)
    }
    if(b == "Scene1" & numb < 1){
      tkmessageBox(message="There are no Scenarios", icon= "warning",type = "ok")
      
    }
    if(b == "Scene2" & numb > 1){
      
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series_white(t,t2,max_interchange=0, scenario=twn,name,title = title,note = note)
    }
    if(b == "Scene2" & numb < 2){
      tkmessageBox(message="There aren't 2 Scenarios", icon= "warning",type = "ok")
      
    }
    if(b == "Scene3" & numb > 2){
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series_white(t,t2,max_interchange=0, scenario=thn,name,title = title,note = note)
    }
    if(b == "Scene3" & numb < 3){
      tkmessageBox(message="There aren't 3 Scenarios", icon= "warning",type = "ok")
      
    }
    if(b == "Scene4" & numb > 3){
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series_white(t,t2,max_interchange=0, scenario=fon,name,title = title,note = note)
    }
    if(b == "Scene4" & numb < 4){
      tkmessageBox(message="There aren't 4 Scenarios", icon= "warning",type = "ok")
      
    }
    if(b == "Scene5" & numb > 4){
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series_white(t,t2,max_interchange=0, scenario=fin,name,title = title,note = note)
    }
    if(b == "Scene5" & numb < 5){
      tkmessageBox(message="There aren't 5 Scenarios", icon= "warning",type = "ok")
      
    }
    if(b == "Scene6" & numb > 5){
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series_white(t,t2,max_interchange=0, scenario=sin,name,title = title,note = note)
    }
    if(b == "Scene6" & numb < 6){
      tkmessageBox(message="There aren't 6 Scenarios", icon= "warning",type = "ok")
      
    }
    if(b == "Scene7" & numb > 6){
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series_white(t,t2,max_interchange=0, scenario=sen,name,title = title,note = note)
    }
    if(b == "Scene7" & numb < 7){
      tkmessageBox(message="There aren't 7 Scenarios", icon= "warning",type = "ok")
      
    }
    if(b == "Scene8" & numb > 7){
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series_white(t,t2,max_interchange=0, scenario=ein,name,title = title,note = note)
    }
    if(b == "Scene8" & numb < 8){
      tkmessageBox(message="There aren't 8 Scenarios", icon= "warning",type = "ok")
      
    }
    if(b == "Scene9" & numb > 8){
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series_white(t,t2,max_interchange=0, scenario=nin,name,title = title,note = note)
    }
    if(b == "Scene9" & numb < 9){
      tkmessageBox(message="There aren't 9 Scenarios", icon= "warning",type = "ok")
      
    }
    if(b == "Scene10" & numb > 9){
      t = as.POSIXlt(c, tz="EST")
      t2 = as.POSIXlt(d, tz= "EST")
      draw_ergis_chord_series_white(t,t2,max_interchange=0, scenario=onn,name,title = title,note = note)
    }
    if(b == "Scene10" & numb < 10){
      tkmessageBox(message="There aren't 2 Scenarios", icon= "warning",type = "ok")
      
    }
    
  }
  
  if(a=="Comparative3" & numb > 2){
   
    t = as.POSIXlt(c, tz="EST")
    t2 = as.POSIXlt(d, tz= "EST")
    draw_ergis_comparative_map_series3(t,t2,name,scene1,scene2,scene3,title = title)
    
  }
  if(a=="Comparative3" & numb < 3){
    
    tkmessageBox(message="There Aren't 3 Scenarios", icon= "warning",type = "ok")
  }
  
  if(a=="Comparative4" & numb > 3){

    t = as.POSIXlt(c, tz="EST")
    t2 = as.POSIXlt(d, tz= "EST")
    draw_ergis_comparative_map_series4_white(t,t2,name,scene1,scene2,scene3,scene4,title = title)
  }
  if(a=="Comparative4" & numb < 4){
    
    tkmessageBox(message="There Aren't 4 Scenarios", icon= "warning",type = "ok")
  }
  
  if(a == "Comparative2" & numb > 1){
    t = as.POSIXlt(c, tz="EST")
    t2 = as.POSIXlt(d, tz= "EST")
    draw_ergis_comparative_map_series2(t,t2,scene1,scene2,name,title = title)
  }
  
  if(a=="Comparative4" & numb < 2){
    
    tkmessageBox(message="There Aren't 2 Scenarios", icon= "warning",type = "ok")
  }
  if(a=="Heatmap"){
    
    contour()
  }
  
}  
    
  
}
