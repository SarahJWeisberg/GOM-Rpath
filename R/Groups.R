#Functional groups for GOM RPath model
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
