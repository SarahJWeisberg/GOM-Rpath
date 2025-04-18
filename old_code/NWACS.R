##Copying the NWACS model from Sean's code for use in estimating bioparams of various groups

library(data.table)

NWACS <- data.table(Group = c('Phytoplankton', 'Other primary producers', 'Bacteria',
                              'Microzooplankton', 'Small Copepods', 'Large Copepods',
                              'Gelatinous zooplankton', 'Micronekton', 
                              'Macrobenthos- polychaetes', 'Macrobenthos- crustaceans',
                              'Macrobenthos- molluscs', 'Macrobenthos- other', 
                              'Megabenthos- filterers', 'Megabenthos- other',
                              'Shrimp and Similar Species', 'Mesopelagics',
                              'Atlantic herring', 'Alosines', 'Atlantic menhaden (S)',
                              'Atlantic menhaden (M)', 'Atlantic menhaden (L)', 
                              'Anchovies', 'Atlantic mackerel', 'Squid', 'Butterfish',
                              'Small pelagics- other', 'Bluefish (S)', 'Bluefish (M)',
                              'Bluefish (L)', 'Striped bass (S)', 'Striped bass (M)',
                              'Striped bass (L)', 'Weakfish (S)', 'Weakfish (M)', 
                              'Weakfish (L)', 'Spiny dogfish (S)', 'Spiny dogfish (L)',
                              'Atlantic cod (S)', 'Atlantic cod (M)', 'Atlantic Cod (L)',
                              'Haddock', 'Hake', 'Atlantic croaker', 
                              'Yellowtail flounder (S)', 'Yellowtail flounder (L)',
                              'Summer flounder (S)', 'Summer flounder (L)', 'Skates',
                              'Demersal benthivores - other', 'Demersal piscivores - other',
                              'Demersal omnivores - other', 'Medium pelagics - other',
                              'Sharks - coastal', 'Sharks - pelagic', 'Large pelagics (HMS)', 
                              'Pinnipeds', 'Baleen whales', 'Odontocetes',
                              'Seabirds', 'Nearshore pisc birds', 'Detritus'),
                    Biomass = c(30.000, 1.605, 7.700, 7.000, 16.000, 17.966, 6.349, 
                                7.654, 17.452, 7.000, 8.340, 21.000, 5.500, 4.498, 
                                0.470, 0.090, 1.650, 0.200, 0.371, 2.048, 1.200,
                                1.100, 1.740, 1.267, 1.488, 1.400, 0.017, 0.160, 
                                0.509, 0.003, 0.022, 0.022, 0.006, 0.038, 0.036,
                                0.337, 0.800, 0.055, 0.144, 0.277, 0.254, 1.100,
                                0.350, 0.007, 0.187, 0.010, 0.159, 1.000, 2.300,
                                1.300, 1.100, 0.021, 0.008, 0.016, 0.070, 0.035,
                                0.464, 0.060, 0.007, 0.007, 52.6),
                    PB = c(180.700, 55.570, 91.250, 85.000, 46.000, 46.000, 40.000,
                           14.250, 2.500, 3.600, 2.200, 2.000, 1.200, 2.300, 2.000,
                           1.100, 1.100, 1.300, 1.900, 1.309, 0.756, 2.200, 0.550,
                           5.720, 1.312, 1.200, 3.900, 0.900, 0.310, 1.500, 0.526, 
                           0.317, 3.300, 0.900, 1.000, 0.321, 0.321, 1.087, 1.125, 
                           0.700, 0.700, 1.296, 0.994, 2.700, 0.850, 2.200, 1.050,
                           0.250, 0.550, 0.450, 0.550, 0.450, 0.200, 0.113, 0.579, 
                           0.075, 0.040, 0.040, 0.279, 0.279, NA),
                    QB = c(0.000, 0.000, 380.208, 283.400, 140.000, 150.000, 145.326,
                           85.497, 17.500, 21.000, 13.949, 16.059, 6.660, 15.533,
                           6.660, 3.700, 3.700, 4.400, 15.860, 6.643, 3.785, 7.333,
                           2.170, 19.000, 4.230, 4.000, 20.935, 6.093, 3.139, 
                           10.265, 3.429, 1.820, 13.520, 4.689, 2.803, 3.519, 1.810,
                           5.059, 2.603, 1.500, 3.000, 3.850, 3.550, 12.168, 2.900, 
                           10.283, 2.900, 0.900, 1.833, 1.500, 1.833, 1.838, 1.247,
                           0.690, 6.794, 5.581, 3.217, 14.301, 80.000, 80.000, NA),
                    RPATH = c('Phytoplankton', 'Phytoplankton', 'Bacteria', 
                              'Microzooplankton', 'Mesozooplankton', 'Mesozooplankton',
                              'GelZooplankton', 'Micronekton', 
                              rep('Macrobenthos', 4), 'AtlScallop', 'AmLobster',
                              'OtherShrimps', 'Mesopelagics', 'AtlHerring', 'SmPelagics',
                              rep('OtherPelagics', 3), 'SmPelagics', 'AtlMackerel',
                              'Illex', 'Butterfish', 'SmPelagics', rep('Bluefish', 3),
                              rep('OtherPelagics', 3), rep('SouthernDemersals', 3),
                              rep('SpinyDogfish', 2), rep('Cod', 3), 'Haddock',
                              'RedHake', 'SouthernDemersals', rep('YTFlounder', 2),
                              rep('SummerFlounder', 2), 'WinterSkate', 
                              rep('OtherDemersals', 3), 'OtherPelagics', rep('Sharks', 2),
                              'HMS', 'Seals', 'BalWhale', 'ToothWhale', 'Seabirds',
                              'Seabirds', 'Detritus'))

#Calculate weighted mean
NWACS[, Rpath.bio := sum(Biomass), by = RPATH]
NWACS[, PB.weight := Biomass * PB]
NWACS[, new.PB := sum(PB.weight) / Rpath.bio, by = RPATH]
NWACS[, QB.weight := Biomass * QB]
NWACS[, new.QB := sum(QB.weight) / Rpath.bio, by = RPATH]

#Drop extra columns and rename
GB.params <- unique(NWACS[, list(RPATH, new.PB, new.QB)])
setnames(GB.params, c('new.PB', 'new.QB'), c('PB', 'QB'))