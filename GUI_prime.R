
#################################################################################
#Original code written by Zaran Claes for MISO.                                 #
#                                                                               #
#                                                                               #
#                                                                               #
#This code creates the graphic user interface for kaleidoscope, it must be used #
#in conjuction with KALEIDOSCOPE.R, ERGISGUI.R, and and PLOTTINGGUI.R.          #
#Source each of the above three scripts before running this script.             #
#For further explanation on this code and the functions used,  either visit     #
#http://www.sciviews.org/recipes/tcltk/toc/                                     #
#or contact me at  either zclaes@misoenergy.org or zclaes@iastate.edu           #
#################################################################################





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
library(magrittr)
library(tcltk2)
library(tcltk)



win1 <- tktoplevel()  #creates window

tktitle(win1) <- "Kaleidoscope Plotting"  #titles the window


#creates the different tabs within the window
win1$env$nb <- tk2notebook(win1,tabs = c("Scenarios","Files","Creation"))
tkpack(win1$env$nb, fill = "both", expand = TRUE)

win1$env$tb1 <- tk2notetab(win1$env$nb, "Scenarios")
win1$env$tb2 <- tk2notetab(win1$env$nb, "Files")
win1$env$tb3 <- tk2notetab(win1$env$nb, "Creation")



#initializes variables for text entry boxes
tn_name <- tclVar("10_percent")
tw_name <- tclVar("20_percent")
th_name <- tclVar("30_percent")
fo_name <- tclVar("40_percent")
fi_name <- tclVar("Scene1")
si_name <- tclVar("Scene1")
se_name <- tclVar("Scene1")
ei_name <- tclVar("Scene1")
ni_name <- tclVar("Scene1")
on_name <- tclVar("Scene1")
num_scen <- tclVar("4")
first_comp <- tclVar("10_percent")
second_comp <- tclVar("20_percent")
third_comp <- tclVar("30_percent")
fourth_comp <- tclVar("40_percent")
cvar <- tclVar("2017-07-24 00:00:00")
dvar <- tclVar("2017-07-24 23:00:00")
filename <- tclVar("Enter Name Here")
filetitle <- tclVar("RIIA Overview using NREL's Kaleidoscope")
filenote <- tclVar("")

#assigns a variable to the info in the text box
win1$env$tn_name <-tk2entry(win1$env$tb1, width = "25", textvariable = tn_name)  
win1$env$tw_name <-tk2entry(win1$env$tb1, width = "25", textvariable = tw_name)
win1$env$th_name <-tk2entry(win1$env$tb1, width = "25", textvariable = th_name)
win1$env$fo_name <-tk2entry(win1$env$tb1, width = "25", textvariable = fo_name)
win1$env$fi_name <-tk2entry(win1$env$tb1, width = "25", textvariable = fi_name)  
win1$env$si_name <-tk2entry(win1$env$tb1, width = "25", textvariable = si_name)
win1$env$se_name <-tk2entry(win1$env$tb1, width = "25", textvariable = se_name)
win1$env$ei_name <-tk2entry(win1$env$tb1, width = "25", textvariable = ei_name)
win1$env$ni_name <-tk2entry(win1$env$tb1, width = "25", textvariable = ni_name)
win1$env$on_name <-tk2entry(win1$env$tb1, width = "25", textvariable = on_name)
win1$env$num_scen <-tk2entry(win1$env$tb3, width = "25", textvariable = num_scen)
win1$env$cvar <-tk2entry(win1$env$tb3, width = "25", textvariable = cvar)
win1$env$dvar <-tk2entry(win1$env$tb3, width = "25", textvariable = dvar)
win1$env$first_comp <- tk2entry(win1$env$tb3, width = "25", textvariable = first_comp)
win1$env$second_comp <- tk2entry(win1$env$tb3, width = "25", textvariable = second_comp)
win1$env$third_comp <- tk2entry(win1$env$tb3, width = "25", textvariable = third_comp)
win1$env$fourth_comp <- tk2entry(win1$env$tb3, width = "25", textvariable = fourth_comp)
win1$env$filename <-tk2entry(win1$env$tb3, width = "25", textvariable = filename)
win1$env$filetitle <-tk2entry(win1$env$tb3, width = "25", textvariable = filetitle)
win1$env$filenote <-tk2entry(win1$env$tb3, width = "25", textvariable = filenote)
#These next chunks of code are for the buttons to find the excel files used

labelText <- tclVar("I:/STP/PROMOD/K-scope Paper/Data for Graphs/Plots for Paper/10%_Dispatch.csv")
win1$env$label <- tk2label(win1$env$tb1, textvariable = labelText)
tkgrid(win1$env$label)
#changes the variable labelText with the tkgetOpenFile function
#(this is the text above the button showing the file path)
changeText <- function(){
  filename <- tclvalue(tkgetOpenFile()) 
  
  tclvalue(labelText) <- filename
}
#assigns the function created above to the button
win1$env$butChange <- tk2button(win1$env$tb1, text = "Select File Scene 1", command = changeText)
tkgrid.configure(win1$env$butChange)

tkgrid(tk2label(win1$env$tb1, text = "                                             Enter the name for Scene1"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$tn_name, padx = 10, pady = c(0, 1))
#This is the end of the chunk of code for the first button, each subsequent chunk is similar, just with 
#different variables.
labelText2 <- tclVar("I:/STP/PROMOD/K-scope Paper/Data for Graphs/Plots for Paper/20%_Dispatch.csv")
win1$env$label2 <- tk2label(win1$env$tb1, textvariable = labelText2)
tkgrid(win1$env$label2)

changeText2 <- function(){
  filename <- tclvalue(tkgetOpenFile()) 
  
  tclvalue(labelText2) <- filename
}

win1$env$butChange2 <- tk2button(win1$env$tb1, text = "Select File Scene 2", command = changeText2)
tkgrid(win1$env$butChange2)

tkgrid(tk2label(win1$env$tb1, text = "                                             Enter the name for Scene2", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$tw_name, padx = 10, pady = c(0, 1))

labelText3 <- tclVar("I:/STP/PROMOD/K-scope Paper/Data for Graphs/Plots for Paper/30%_Dispatch.csv")
win1$env$label3 <- tk2label(win1$env$tb1, textvariable = labelText3)
tkgrid(win1$env$label3)

changeText3 <- function(){
  filename <- tclvalue(tkgetOpenFile()) 
  
  tclvalue(labelText3) <- filename
}

win1$env$butChange3 <- tk2button(win1$env$tb1, text = "Select File Scene 3", command = changeText3)
tkgrid(win1$env$butChange3)

tkgrid(tk2label(win1$env$tb1, text = "                                             Enter the name for Scene3", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$th_name, padx = 10, pady = c(0, 1))

labelText4 <- tclVar("I:/STP/PROMOD/K-scope Paper/Data for Graphs/Plots for Paper/40%_Dispatch.csv")
win1$env$label4 <- tk2label(win1$env$tb1, textvariable = labelText4)
tkgrid(win1$env$label4)

changeText4 <- function(){
  filename <- tclvalue(tkgetOpenFile()) 
  
  tclvalue(labelText4) <- filename
}

win1$env$butChange4 <- tk2button(win1$env$tb1, text = "Select File Scene 4", command = changeText4)
tkgrid(win1$env$butChange4)

tkgrid(tk2label(win1$env$tb1, text = "                                             Enter the name for Scene4", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$fo_name, padx = 10, pady = c(0, 1))

labelText5 <- tclVar("Scenario5")
win1$env$label5 <- tk2label(win1$env$tb1, textvariable = labelText5)
tkgrid(win1$env$label5)

changeText5 <- function(){
  filename <- tclvalue(tkgetOpenFile()) 
  
  tclvalue(labelText5) <- filename
}

win1$env$butChange5 <- tk2button(win1$env$tb1, text = "Select File Scene 5", command = changeText5)
tkgrid(win1$env$butChange5)

tkgrid(tk2label(win1$env$tb1, text = "                                             Enter the name for Scene5", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$fi_name, padx = 10, pady = c(0, 1))

labelText6 <- tclVar("Scenario6")
win1$env$label6 <- tk2label(win1$env$tb1, textvariable = labelText6)
tkgrid(win1$env$label6)

changeText6 <- function(){
  filename <- tclvalue(tkgetOpenFile()) 
  
  tclvalue(labelText6) <- filename
}

win1$env$butChange6 <- tk2button(win1$env$tb1, text = "Select File Scene 6", command = changeText6)
tkgrid(win1$env$butChange6)

tkgrid(tk2label(win1$env$tb1, text = "                                             Enter the name for Scene6", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$si_name, padx = 10, pady = c(0, 1))

labelText7 <- tclVar("Scenario7")
win1$env$label7 <- tk2label(win1$env$tb1, textvariable = labelText7)
tkgrid(win1$env$label7)

changeText7 <- function(){
  filename <- tclvalue(tkgetOpenFile()) 
  
  tclvalue(labelText7) <- filename
}

win1$env$butChange7 <- tk2button(win1$env$tb1, text = "Select File Scene 7", command = changeText7)
tkgrid(win1$env$butChange7)

tkgrid(tk2label(win1$env$tb1, text = "                                             Enter the name for Scene7", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$se_name, padx = 10, pady = c(0, 1))

labelText8 <- tclVar("Scenario8")
win1$env$label8 <- tk2label(win1$env$tb1, textvariable = labelText8)
tkgrid(win1$env$label8)

changeText8 <- function(){
  filename <- tclvalue(tkgetOpenFile()) 
  
  tclvalue(labelText8) <- filename
}

win1$env$butChange8 <- tk2button(win1$env$tb1, text = "Select File Scene 8", command = changeText8)
tkgrid(win1$env$butChange8)

tkgrid(tk2label(win1$env$tb1, text = "                                             Enter the name for Scene8", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$ei_name, padx = 10, pady = c(0, 1))

labelText9 <- tclVar("Scenario9")
win1$env$label9 <- tk2label(win1$env$tb1, textvariable = labelText9)
tkgrid(win1$env$label9)

changeText9 <- function(){
  filename <- tclvalue(tkgetOpenFile()) 
  
  tclvalue(labelText9) <- filename
}

win1$env$butChange9 <- tk2button(win1$env$tb1, text = "Select File Scene 9", command = changeText9)
tkgrid(win1$env$butChange9)

tkgrid(tk2label(win1$env$tb1, text = "                                             Enter the name for Scene9", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$ni_name, padx = 10, pady = c(0, 1))

labelText10 <- tclVar("Scenario10")
win1$env$label10 <- tk2label(win1$env$tb1, textvariable = labelText10)
tkgrid(win1$env$label10)

changeText10 <- function(){
  filename <- tclvalue(tkgetOpenFile()) 
  
  tclvalue(labelText10) <- filename
}

win1$env$butChange10 <- tk2button(win1$env$tb1, text = "Select File Scene 10", command = changeText10)
tkgrid(win1$env$butChange10)

tkgrid(tk2label(win1$env$tb1, text = "                                             Enter the name for Scene10", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$on_name, padx = 10, pady = c(0, 1))

#These next sets of functions are doing the same thing, except in a different tab
labelTextint <- tclVar("C:/Users/m03439/Documents/RIIA Workshop II/40%_Interchange_base.csv")
win1$env$labelint <- tk2label(win1$env$tb2, textvariable = labelTextint)  #note the tb2 for tab 2
tkgrid(win1$env$labelint)

changeTextint <- function(){
  filename <- tclvalue(tkgetOpenFile()) 
  
  tclvalue(labelTextint) <- filename
}

win1$env$butChangeint <- tk2button(win1$env$tb2, text = "Select File Interchange", command = changeTextint)
tkgrid(win1$env$butChangeint)

tkgrid(tk2label(win1$env$tb2, text = "                                             Enter the file path for Interchange", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
# tkgrid(win1$env$int_path, padx = 10, pady = c(0, 1))





labelTextdis <- tclVar("C:/Users/m03439/Documents/RIIA Workshop II/Stack-LRZs_40%Final.csv")
win1$env$labeldis <- tk2label(win1$env$tb2, textvariable = labelTextdis)
tkgrid(win1$env$labeldis)

changeTextdis <- function(){
  filename <- tclvalue(tkgetOpenFile()) 
  
  tclvalue(labelTextdis) <- filename
}

win1$env$butChangedis <- tk2button(win1$env$tb2, text = "Select File Dispatch", command = changeTextdis)
tkgrid(win1$env$butChangedis)

tkgrid(tk2label(win1$env$tb2, text = "                                             Enter the file path for Dispatch", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
# tkgrid(win1$env$dis_path, padx = 10, pady = c(0, 1))

#labelTextmap <- tclVar("C:/Users/M04082/Documents/KscopestuffEditedbyme/MISO LRZ_2018-06-07_region.shp") # Modified 
labelTextmap <- tclVar("I:/STP/PROMOD/K-scope Paper/Automated Excel Files/Static files/MISO LRZ_2018-06-07_region.shp")
win1$env$labelmap <- tk2label(win1$env$tb2, textvariable = labelTextmap)
tkgrid(win1$env$labelmap)


changeTextmap <- function(){
  filename <- tclvalue(tkgetOpenFile()) 
  
  tclvalue(labelTextmap) <- filename
}

win1$env$butChangemap <- tk2button(win1$env$tb2, text = "Select File Map", command = changeTextmap)
tkgrid(win1$env$butChangemap)


tkgrid(tk2label(win1$env$tb2, text = "                                             Enter the file path for the map", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
# tkgrid(win1$env$map_path, padx = 10, pady = c(0, 1))

labelTextcen <- tclVar("I:/STP/PROMOD/K-scope Paper/Automated Excel Files/Static files/region-centroids.csv")
win1$env$labelcen <- tk2label(win1$env$tb2, textvariable = labelTextcen)
tkgrid(win1$env$labelcen)


changeTextcen <- function(){
  filename <- tclvalue(tkgetOpenFile()) 
  
  tclvalue(labelTextcen) <- filename
}

win1$env$butChangecen <- tk2button(win1$env$tb2, text = "Select File Centroids", command = changeTextcen)
tkgrid(win1$env$butChangecen)

tkgrid(tk2label(win1$env$tb2, text = "                                             Enter the file path for Region Centroids", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
# tkgrid(win1$env$cen_path, padx = 10, pady = c(0, 1))

labelTextgen <- tclVar("I:/STP/PROMOD/K-scope Paper/Data for Graphs/RIIA Overview Movies/Genny.csv")
win1$env$labelgen <- tk2label(win1$env$tb2, textvariable = labelTextgen)
tkgrid(win1$env$labelgen)


changeTextgen <- function(){
  filename <- tclvalue(tkgetOpenFile()) 
  
  tclvalue(labelTextgen) <- filename
}

win1$env$butChangegen <- tk2button(win1$env$tb2, text = "Select File Generators", command = changeTextgen)
tkgrid(win1$env$butChangegen)

tkgrid(tk2label(win1$env$tb2, text = "                                             Enter the file path generator coordinates", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")



tkgrid(tk2label(win1$env$tb3, text = "How many scenarios are there?", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$num_scen, padx = 10, pady = c(0, 1))
tkgrid(tk2label(win1$env$tb3, text = "Enter the First Date", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$cvar, padx = 10, pady = c(0, 1))
tkgrid(tk2label(win1$env$tb3, text = "Enter the Second Date", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$dvar, padx = 10, pady = c(0, 1))
tkgrid(tk2label(win1$env$tb3, text = "First Scene Name for comparative maps", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$first_comp, padx = 10, pady = c(0, 1))
tkgrid(tk2label(win1$env$tb3, text = "Second Scene Name for comparative maps", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$second_comp, padx = 10, pady = c(0, 1))
tkgrid(tk2label(win1$env$tb3, text = "Third Scene Name for comparative maps", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$third_comp, padx = 10, pady = c(0, 1))
tkgrid(tk2label(win1$env$tb3, text = "Fourth Scene Name for comparative maps", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$fourth_comp, padx = 10, pady = c(0, 1))

win1$env$choice <- tk2listbox(win1$env$tb3, height = 3, selectmode = "single")
tkgrid(tk2label(win1$env$tb3, text = "Would You like to graph Single Scenario,Comparative, or Heatmap", justify = "left"),
       padx = 10, pady =c(1, 1), sticky = "w")
tkgrid(win1$env$choice, padx = 10, pady = c(1, 1))
win1$env$scenario <- tk2listbox(win1$env$tb3, height = 4, selectmode = "single")
tkgrid(tk2label(win1$env$tb3, text = "What scenario would you like to graph?", justify = "left"),
       padx = 10, pady =c(1, 1), sticky = "w")
tkgrid(win1$env$scenario, padx = 10, pady = c(1, 1))
win1$env$col_choice <- tk2listbox(win1$env$tb3, height = 3, selectmode = "single")
tkgrid(tk2label(win1$env$tb3, text = "What do you want the background color to be?", justify = "left"),
       padx = 10, pady =c(1, 1), sticky = "w")
tkgrid(win1$env$col_choice, padx = 10, pady = c(1, 1))

#creates set of strings for the select boxes
choice <- c("Single","Comparative2", "Comparative3","Comparative4","Heatmap")
scenario <- c("Scene1","Scene2","Scene3","Scene4","Scene5","Scene6","Scene7","Scene8","Scene9","Scene10")
col_choice <- c("Black","White")
#creates the select boxes
for (ch in choice)
  tkinsert(win1$env$choice, "end", ch)
for (scen in scenario)
  tkinsert(win1$env$scenario, "end", scen)
for (col in col_choice)
  tkinsert(win1$env$col_choice, "end", col)
#initializes the number selected within the box, 0 is first, 1 is second, etc
tkselection.set(win1$env$choice, 0)  
tkselection.set(win1$env$scenario, 0)
tkselection.set(win1$env$col_choice, 0)
#allows the user to title the filenames
tkgrid(tk2label(win1$env$tb3, text = "What would you like to call the exported file?", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$filename, padx = 10, pady = c(0, 1))
#allows the user edit the text title in the picture
tkgrid(tk2label(win1$env$tb3, text = "Enter a title for the Pictures", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$filetitle, padx = 10, pady = c(0, 1))
#allows the user to leave a note
tkgrid(tk2label(win1$env$tb3, text = "Leave a note if you want to", justify = "left"),
       padx = 10, pady = c(1, 1), sticky = "w")
tkgrid(win1$env$filenote, padx = 10, pady = c(0, 1))
#when ok is clicked this function is executed
onOK <- function() {
 #takes in the string entered in any text box
  nums <- tclvalue(num_scen)
  tn <- tclvalue(labelText)
  tnn <- tclvalue(tn_name)
  tw <- tclvalue(labelText2)
  twn <- tclvalue(tw_name)
  th <- tclvalue(labelText3)
  thn <- tclvalue(th_name)
  fo <- tclvalue(labelText4)
  fon <- tclvalue(fo_name)
  fi <- tclvalue(labelText5)
  fin <- tclvalue(fi_name)
  si <- tclvalue(labelText6)
  sin <- tclvalue(si_name)
  se <- tclvalue(labelText7)
  sen <- tclvalue(se_name)
  ei <- tclvalue(labelText8)
  ein <- tclvalue(ei_name)
  ni <- tclvalue(labelText9)
  nin <- tclvalue(ni_name)
  on <- tclvalue(labelText10)
  onn <- tclvalue(on_name)
  int <- tclvalue(labelTextint)
  dis <- tclvalue(labelTextdis)
  map <- tclvalue(labelTextmap)
  gen <- tclvalue(labelTextgen)
  cen <- tclvalue(labelTextcen)
  c <- tclvalue(cvar)
  d <- tclvalue(dvar)
  scene1 <- tclvalue(first_comp)
  scene2 <- tclvalue(second_comp)
  scene3 <- tclvalue(third_comp)
  scene4 <- tclvalue(fourth_comp)
  
  name <- tclvalue(filename)
  title <- tclvalue(filetitle)
  note <- tclvalue(filenote)
  if(note == "Kirk"){
    #changes the cursor to the star trek enterprise
    tkconfigure(win1, cursor = "trek")
  }
  
  #takes in the input of the select boxes
  a <- choice[as.numeric(tkcurselection(win1$env$choice))+1]
  b <- scenario[as.numeric(tkcurselection(win1$env$scenario))+1]
  color <- col_choice[as.numeric(tkcurselection(win1$env$col_choice))+1]
  #all the lines above assign whatever was in the gui to a variable
  #variables are then sent through the mapping function, which is located in PLOTTINGGUI.R
  mapping(a,b,c,d,tn,tnn,tw,twn,th,thn,fo,fon,fi,fin,si,sin,se,sen,ei,ein,ni,nin,on,onn,int,dis,map,gen,cen,name,title,note,nums,scene1,scene2,scene3,scene4,color)
  
}
#creates the OK button, onOk function is mapped to the button here
win1$env$butOK <-tk2button(win1$env$tb3, text = "OK", width = -6, command = onOK)
tkgrid(win1$env$butOK, padx = 10, pady = c(5, 15))


