# MISO's Visualization Tool for Renewable Integration Assessments
This visualization tool is an extension from NREL's Kaleidoscope code [NREL's Kaleidoscope](https://github.com/NREL/kaleidoscope). It is customized for the Midcontinent Independend System Operator (MISO) system. Supporting files for processing the results from PLEXOS are also included in the Processing folder. The process for creating comparative and single maps is described below. Also, an instruction manual developed by Zaran Claes (zclaes@iastate.edu) and Shannon Foley (sfoley@iastate.edu) from Iowa State University is included as part of the package. This package is useful for visualizing renewable integration complexities in a multi-temporal framework. Altought this is a work in progress, the basic functionalities and supporting files are available. For additional questions or comments, please contact Armando L. Figueroa-Acevedo at afigueroa-acevedo@misoenergy.org.

### 1. Process 

To extend the existing capabilities of Kaleidoscope to fit MISO’s electric system terminology three key efforts were made to adapt and customize the original NREL Kaleidoscope tool, including:  

  - Customization of the existing functions in the original NREL Kaleidoscope package for the MISO system. In particular, MISO made the following modifications/developments: modify all relevant codes to fit Kaleidoscope into MISO topology and terminology; replace all map layers to reflect MISO’s existing footprint; MISO also keeps the names of the original functions to facilitate the transfer of the modifications back to the original source code. 
    
  - To streamline the process of creating individual Portable Network Graphics (PNG), MISO created a graphical user interface (GUI) for users to enter their choices of input and output. The addition of a GUI eliminates the need to change the underlying code manually should users want to make any customized modification. Furthermore, the GUI allows for the selection of a single scenario or a comparative map showing multiple scenarios. All static files can be called from the GUI (shapefile containing geographical information, e.g., state/region borders, latitude/longitude information for each region or sub-region, list of generators with corresponding fuel type).
  
  - MISO developed a supporting Python code to take the data output from PLEXOS and converted it into a form that would work with the given Kaleidoscope functions. The code extracts the solution file from PLEXOS and converts it into a CSV format suitable for Kaleidoscope. This feature removes the need for post-processing results, which also reduces the possibility of introducing errors in the process. 

<p align="center"> 
<img src="https://github.com/alfigueroa21/MISO-Kaleidoscope/blob/master/Data%20flow%20diagram.png">
</p>

### 2. Illustration of renewable integration complexity by integration level

Comparative maps allow us to visualize renewable integration complexities and directional trends. 

![alt text](https://github.com/alfigueroa21/MISO-Kaleidoscope/blob/master/Figure%205_Full_Resolution.png) 

### 3. Illustration of renewable integration complexity at the 40% integration level

Single maps allow us to illustrate potential solutions to renewable integration complexities

![alt text](https://github.com/alfigueroa21/MISO-Kaleidoscope/blob/master/Figure%206_Full_Resolution.png) 

### 4. Effects of solutions adopted to mitigate 40% renewable integration complexities

![alt text](https://github.com/alfigueroa21/MISO-Kaleidoscope/blob/master/Figure%207_Full_Resolution.png) 

