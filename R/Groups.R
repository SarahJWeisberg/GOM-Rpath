#Title: GOM Rpath Functional Groups

# Purpose: This script simply creates a data table with all of the functional 
#           groups used in the GOM Rpath model. Functional group selection is 
#           a matter of modeler choice and relates to the specific questions
#           being addressed by the model. 

# DataFile: NA

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Last modified: Tue Jun 15 15:18:36 2021 ------------------------------


#Load needed packages
library(here);library(data.table)

#Living functional groups for GOM RPath model
GOM.groups <-as.data.table(c('Phytoplankton', 'Bacteria', 'Microzooplankton', 
                             'GelZooplankton','LgCopepods', 'SmCopepods', 
                             'Micronekton', 'Illex','Loligo', 'OtherCephalopods',
                             'Macrobenthos','AmLobster','OceanPout','SmPelagics',
                             'SmFlatfishes','NShrimp','OtherShrimps',
                             'RiverHerring','AtlScallop','OtherPelagics',
                             'AtlHerring','Mesopelagics','YTFlounder','Cod',
                             'Haddock','AmPlaice','AtlMackerel','AtlHalibut',
                             'SummerFlounder', 'Cusk','OtherDemersals','HMS',
                             'OtherSkates','RedHake','Barndoor','Fourspot',
                             'SmoothDogfish','Pollock','Goosefish','SilverHake',
                             'WhiteHake','SpinyDogfish','Redfish','LittleSkate',
                             'SeaBirds','Windowpane','WinterFlounder',
                             'WitchFlounder','BlackSeaBass','Butterfish',
                             'Sharks','WinterSkate','Pinnipeds','BaleenWhales',
                             'Odontocetes', 'Megabenthos'))
setnames(GOM.groups,'V1','RPATH')
  