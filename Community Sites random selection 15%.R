setwd("C:/Users/cahall/OneDrive - DOI/Data/Half mile sites")

library(dplyr)

#Load Pool MAster Sites
RacineSites <- read.csv("Racine half mile sites.csv")

#split main channel sites into thirds then add in side channels
RacineMainSites <- RacineSites[1:132, ]

n<- nrow(RacineMainSites)

third <- ceiling(n/3)

RAuppermainsites<- RacineMainSites[1:third,]
RAmiddlemainsites<- RacineMainSites[(third + 1):(2*third),]
RAlowermainsites<- RacineMainSites[(2 * third + 1):n,]

#select side channels in proper river position
RAmiddlesidechannel<- RacineSites[157:162,]
RAlowersidechannel<- RacineSites[163:164,]

#add in Side channels to correct portion of stem
RAlowermainsites<- rbind(RAlowermainsites,RAlowersidechannel)
RAmiddlemainsites<- rbind(RAmiddlemainsites, RAmiddlesidechannel)

#Split Dataset based trib sites
RacineTribSites <- RacineSites[c(133:156, 165:nrow(RacineSites)),]  # Remaining rows
write.csv(RacineTribSites, "Randomized Sites 2025/Racine_Trib_Sites_2025.csv")


#Randomly select x main sites for sampling
RAupperMain_Random<- RAuppermainsites %>% filter(Site %in% sample(RAuppermainsites$Site, 
                                                                size = 0.15 * nrow(RAuppermainsites)))

RAmiddleMain_Random<- RAmiddlemainsites %>% filter(Site %in% sample(RAmiddlemainsites$Site, 
                                                                  size = 0.15 * nrow(RAmiddlemainsites)))

RAlowerMain_Random<- RAlowermainsites %>% filter(Site %in% sample(RAlowermainsites$Site, 
                                                                  size = 0.15 * nrow(RAlowermainsites)))


#bind selected sites
RacineMain_Random <- rbind(RAmiddleMain_Random,RAupperMain_Random,RAlowerMain_Random)

#Randomly select x trib sites for sampling, **This percentage/number TBD**
#RacineTrib_Random<- RacineTribSites %>% filter(Site %in% sample(RacineTribSites$Site, ))

#putting randomized sites into folder
write.csv(RacineMain_Random, "Randomized Sites 2025/Racine_Main_Random_2025_15per_split.csv")



#******NEW POOL*******
#Load Pool Master Sites
RCSites <- read.csv("RC Byrd half mile sites.csv")

#split main channel sites into thirds then add in side channels
RCMainSites <- RCSites[1:164, ]

n<- nrow(RCMainSites)

third <- ceiling(n/3)

RCuppermainsites<- RCMainSites[1:third,]
RCmiddlemainsites<- RCMainSites[(third + 1):(2*third),]
RClowermainsites<- RCMainSites[(2 * third + 1):n,]

#select side channels in proper river position
RCmiddlesidechannel<- RCSites[169,]
RClowersidechannel<- RCSites[165:168,]

#add in Side channels to correct portion of stem
RClowermainsites<- rbind(RClowermainsites,RClowersidechannel)
RCmiddlemainsites<- rbind(RCmiddlemainsites, RCmiddlesidechannel)

#Split Dataset based trib sites
RCTribSites <- RCSites[c(170:nrow(RCSites)),]  # Remaining rows
write.csv(RCTribSites, "Randomized Sites 2025/RC_Byrd_Trib_Sites_2025.csv")


#Randomly select x main sites for sampling
RCupperMain_Random<- RCuppermainsites %>% filter(Site %in% sample(RCuppermainsites$Site, 
                                                                  size = 0.15 * nrow(RCuppermainsites)))

RCmiddleMain_Random<- RCmiddlemainsites %>% filter(Site %in% sample(RCmiddlemainsites$Site, 
                                                                    size = 0.15 * nrow(RCmiddlemainsites)))

RClowerMain_Random<- RClowermainsites %>% filter(Site %in% sample(RClowermainsites$Site, 
                                                                  size = 0.15 * nrow(RClowermainsites)))


#bind selected sites
RCMain_Random <- rbind(RCmiddleMain_Random,RCupperMain_Random,RClowerMain_Random)

#Randomly select x trib sites for sampling **Percentage/Number of Tribs TBD**
#RCByrdTrib_Random<- RCByrdTribSites %>% filter(Site %in% sample(RCByrdTribSites$Site, ))

#putting randomized sites into folder
write.csv(RCMain_Random, "Randomized Sites 2025/RC_Byrd_Main_Random_2025_15per_split.csv")



#******NEW POOL*******
#Load Pool MAster Sites
BVSites <- read.csv("Belleville Half Mile Sites.csv")

#Remove Main channel sites
BVMainSites <- BVSites[1:166, ]

#split main channel sites into equal thirds
n<- nrow(BVMainSites)

third <- ceiling(n/3)

BVuppermainsites<- BVMainSites[1:third,]
BVmiddlemainsites<- BVMainSites[(third + 1):(2*third),]
BVlowermainsites<- BVMainSites[(2 * third + 1):n,]

#select side channels in proper river position
BVuppersidechannel<- BVSites[167:180,]
BVmiddlesidechannel<- BVSites[181:208,]
BVlowersidechannel<- BVSites[c(209:210),]

#add in Side channels to correct portion of stem
BVuppermainsites<- rbind(BVuppermainsites, BVuppersidechannel)
BVmiddlemainsites<- rbind(BVmiddlemainsites, BVmiddlesidechannel)
BVlowermainsites<- rbind(BVlowermainsites, BVlowersidechannel)


#Split Dataset based trib sites
BVTribSites <- BVSites[c(211:nrow(BVSites)),]  # Remaining rows
write.csv(BVTribSites, "Randomized Sites 2025/Belleville_Trib_Sites_2025.csv")


#Randomly select x main sites for sampling
BVupperMain_Random<- BVuppermainsites %>% filter(Site %in% sample(BVuppermainsites$Site, 
                                                                  size = 0.15 * nrow(BVuppermainsites)))

BVmiddleMain_Random<- BVmiddlemainsites %>% filter(Site %in% sample(BVmiddlemainsites$Site, 
                                                                    size = 0.15 * nrow(BVmiddlemainsites)))

BVlowerMain_Random<- BVlowermainsites %>% filter(Site %in% sample(BVlowermainsites$Site, 
                                                                  size = 0.15 * nrow(BVlowermainsites)))


#bind selected sites
BVMain_Random <- rbind(BVmiddleMain_Random,BVupperMain_Random,BVlowerMain_Random)

#Randomly select x trib sites for sampling **Percentage/Number of Tribs TBD**
#RCByrdTrib_Random<- RCByrdTribSites %>% filter(Site %in% sample(RCByrdTribSites$Site, ))

#putting randomized sites into folder
write.csv(BVMain_Random, "Randomized Sites 2025/Belleville_Main_Random_2025_15per_split.csv")




#******NEW POOL*******
#Load Pool MAster Sites
OWSites <- read.csv("Open Water Half Mile Sites.csv")

OWMainSites <- OWSites[1:122, ]

n<- nrow(OWMainSites)

third <- ceiling(n/3)

OWuppermainsites<- OWMainSites[1:third,]
OWmiddlemainsites<- OWMainSites[(third + 1):(2*third),]
OWlowermainsites<- OWMainSites[(2 * third + 1):n,]


#Split Dataset based trib sites
OWTribSites <- OWSites[c(123:nrow(OWSites)),]  # Remaining rows
write.csv(OWTribSites, "Randomized Sites 2025/Open_Water_Trib_Sites_2025.csv")


#Randomly select x main sites for sampling
OWupperMain_Random<- OWuppermainsites %>% filter(Site %in% sample(OWuppermainsites$Site, 
                                                                  size = 0.15 * nrow(OWuppermainsites)))

OWmiddleMain_Random<- OWmiddlemainsites %>% filter(Site %in% sample(OWmiddlemainsites$Site, 
                                                                    size = 0.15 * nrow(OWmiddlemainsites)))

OWlowerMain_Random<- OWlowermainsites %>% filter(Site %in% sample(OWlowermainsites$Site, 
                                                                  size = 0.15 * nrow(OWlowermainsites)))


#bind selected sites
OWMain_Random <- rbind(OWmiddleMain_Random,OWupperMain_Random,OWlowerMain_Random)

#Randomly select x trib sites for sampling **Percentage/Number of Tribs TBD**
#RCByrdTrib_Random<- RCByrdTribSites %>% filter(Site %in% sample(RCByrdTribSites$Site, ))

#putting randomized sites into folder
write.csv(OWMain_Random, "Randomized Sites 2025/Open_Water_Main_Random_2025_15per_split.csv")



#******NEW POOL*******
#Load Pool MAster Sites
WISites <- read.csv("Willow Half Mile Sites.csv")

WIMainSites <- WISites[1:143, ]

n<- nrow(WIMainSites)

third <- ceiling(n/3)

WIuppermainsites<- WIMainSites[1:third,]
WImiddlemainsites<- WIMainSites[(third + 1):(2*third),]
WIlowermainsites<- WIMainSites[(2 * third + 1):n,]

#select side channels in proper river position
WIuppersidechannel<- WISites[152:161,]
WImiddlesidechannel<- WISites[162:167,]
WIlowersidechannel<- WISites[c(167:178, 187:190),]

#add in Side channels to correct portion of stem
WIuppermainsites<- rbind(WIuppermainsites, WIuppersidechannel)
WImiddlemainsites<- rbind(WImiddlemainsites, WImiddlesidechannel)
WIlowermainsites<- rbind(WIlowermainsites,WIlowersidechannel)


#Split Dataset based trib sites
WITribSites <- WISites[c(144:151, 179:186, 191:nrow(WISites)),]  # Remaining rows
write.csv(WITribSites, "Randomized Sites 2025/Willow_Trib_Sites_2025.csv")


#Randomly select x main sites for sampling
WIupperMain_Random<- WIuppermainsites %>% filter(Site %in% sample(WIuppermainsites$Site, 
                                                                  size = 0.15 * nrow(WIuppermainsites)))

WImiddleMain_Random<- WImiddlemainsites %>% filter(Site %in% sample(WImiddlemainsites$Site, 
                                                                    size = 0.15 * nrow(WImiddlemainsites)))

WIlowerMain_Random<- WIlowermainsites %>% filter(Site %in% sample(WIlowermainsites$Site, 
                                                                  size = 0.15 * nrow(WIlowermainsites)))


#bind selected sites
WIMain_Random <- rbind(WImiddleMain_Random,WIupperMain_Random,WIlowerMain_Random)

#Randomly select x trib sites for sampling **Percentage/Number of Tribs TBD**
#RCByrdTrib_Random<- RCByrdTribSites %>% filter(Site %in% sample(RCByrdTribSites$Site, ))

#putting randomized sites into folder
write.csv(WIMain_Random, "Randomized Sites 2025/Willow_Main_Random_2025_15per_split.csv")



#******NEW POOL*******
#Load Pool MAster Sites
WNSites <- read.csv("Winfield Half Mile Sites.csv")

WNMainSites <- WNSites[1:146, ]

n<- nrow(WNMainSites)

third <- ceiling(n/3)

WNuppermainsites<- WNMainSites[1:third,]
WNmiddlemainsites<- WNMainSites[(third + 1):(2*third),]
WNlowermainsites<- WNMainSites[(2 * third + 1):n,]

#select side channels in proper river position
WNuppersidechannel<- WNSites[147:148,]
WNmiddlesidechannel<- WNSites[149:152,]

#add in Side channels to correct portion of stem
WNuppermainsites<- rbind(WNuppermainsites, WNuppersidechannel)
WNmiddlemainsites<- rbind(WNmiddlemainsites, WNmiddlesidechannel)



#Split Dataset based trib sites
WNTribSites <- WNSites[c(153:nrow(WNSites)),]  # Remaining rows
write.csv(WNTribSites, "Randomized Sites 2025/Winfield_Trib_Sites_2025.csv")


#Randomly select x main sites for sampling
WNupperMain_Random<- WNuppermainsites %>% filter(Site %in% sample(WNuppermainsites$Site, 
                                                                  size = 0.15 * nrow(WNuppermainsites)))

WNmiddleMain_Random<- WNmiddlemainsites %>% filter(Site %in% sample(WNmiddlemainsites$Site, 
                                                                    size = 0.15 * nrow(WNmiddlemainsites)))

WNlowerMain_Random<- WNlowermainsites %>% filter(Site %in% sample(WNlowermainsites$Site, 
                                                                  size = 0.15 * nrow(WNlowermainsites)))


#bind selected sites
WNMain_Random <- rbind(WNmiddleMain_Random,WNupperMain_Random,WNlowerMain_Random)

#Randomly select x trib sites for sampling **Percentage/Number of Tribs TBD**
#RCByrdTrib_Random<- RCByrdTribSites %>% filter(Site %in% sample(RCByrdTribSites$Site, ))

#putting randomized sites into folder
write.csv(WNMain_Random, "Randomized Sites 2025/Winfield_Main_Random_2025_15per_split.csv")
