setwd("C:/Users/cahall/OneDrive - DOI/Data/Half mile sites")

install.packages("stringr")
install.packages("purrr")
library(stringr)
library(dplyr)
library(purrr)

set.seed(25)  # Ensures reproducibility for 2025

#Load Pool Master Sites
RASites <- read.csv("Racine half mile sites.csv")

#split main channel sites into thirds then add in side channels
RAMainSites <- RASites[1:132, ]

n<- nrow(RAMainSites)

third <- ceiling(n/3)

RAuppermainsites<- RAMainSites[1:third,]
RAmiddlemainsites<- RAMainSites[(third + 1):(2*third),]
RAlowermainsites<- RAMainSites[(2 * third + 1):n,]

#select side channels in proper river position
RAmiddlesidechannel<- RASites[157:162,]
RAlowersidechannel<- RASites[163:164,]

#add in Side channels to correct portion of stem
RAlowermainsites<- rbind(RAlowermainsites,RAlowersidechannel)
RAmiddlemainsites<- rbind(RAmiddlemainsites, RAmiddlesidechannel)

#Randomly select x main sites for sampling
RAupperMain_Random<- RAuppermainsites %>% filter(Site %in% sample(RAuppermainsites$Site, 
                                                                size = 0.1 * nrow(RAuppermainsites)))

RAmiddleMain_Random<- RAmiddlemainsites %>% filter(Site %in% sample(RAmiddlemainsites$Site, 
                                                                  size = 0.1 * nrow(RAmiddlemainsites)))

RAlowerMain_Random<- RAlowermainsites %>% filter(Site %in% sample(RAlowermainsites$Site, 
                                                                  size = 0.1 * nrow(RAlowermainsites)))


#bind selected sites
RAMain_Random <- rbind(RAmiddleMain_Random,RAupperMain_Random,RAlowerMain_Random)

#Split Dataset based trib sites
RATribSites <- RASites[c(133:156, 165:nrow(RASites)),]  # Remaining rows

#Randomly select x trib sites for sampling **Percentage/Number of Tribs TBD**
set.seed(25)  # Ensures reproducibility

RATrib_Random <- RATribSites %>%
  mutate(group = case_when(
    str_detect(Site, "^RASHAD") ~ "RASHAD",
    str_detect(Site, "^RASAND") ~ "RASAND",
    str_detect(Site, "^RAMILL") ~ "RAMILL",
    str_detect(Site, "^RAPOND") ~ "RAPOND",
    str_detect(Site, "^RALMIL") ~ "RALMIL",
    str_detect(Site, "^RATOMB") ~ "RATOMB",
    str_detect(Site, "^RALSAN") ~ "RALSAND",
    str_detect(Site, "^RAFORK") ~ "RAFORK",
    str_detect(Site, "^RAJOHN") ~ "RAJOHN",
    TRUE ~ "Other"
  )) %>%
  group_by(group) %>%
  group_split() %>%
  map_dfr(~ {
    group_name <- unique(.x$group)
    n_sample <- case_when(
      group_name %in% c("RASHAD", "RASAND", "RAMILL") ~ 2,
      group_name %in% c("RAPOND", "RALMIL", "RATOMB", "RALSAND", "RAFORK", "RAJOHN") ~ 1,
      TRUE ~ 0
    )
if (nrow(.x) >= n_sample & n_sample > 0) {
  slice_sample(.x, n = n_sample)
} else {
  NULL  # Skip if not enough rows or not in the target groups
}
  })

RATrib_Random <- RATrib_Random %>% select(Site, Latitude, Longtitude, Done)
RAMain_Random <- RAMain_Random %>% select(Site, Latitude, Longtitude, Done)
RARandomSites_25 <- rbind(RAMain_Random,RATrib_Random)

#putting randomized sites into folder
write.csv(RARandomSites_25, "Randomized Sites 2025/RA_Main_Random_2025_10per_Stratified.csv")






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

#Randomly select x main sites for sampling
RCupperMain_Random<- RCuppermainsites %>% filter(Site %in% sample(RCuppermainsites$Site, 
                                                                  size = 0.1 * nrow(RCuppermainsites)))

RCmiddleMain_Random<- RCmiddlemainsites %>% filter(Site %in% sample(RCmiddlemainsites$Site, 
                                                                    size = 0.1 * nrow(RCmiddlemainsites)))

RClowerMain_Random<- RClowermainsites %>% filter(Site %in% sample(RClowermainsites$Site, 
                                                                  size = 0.1 * nrow(RClowermainsites)))


#bind selected sites
RCMain_Random <- rbind(RCmiddleMain_Random,RCupperMain_Random,RClowerMain_Random)

#Split Dataset based trib sites
RCTribSites <- RCSites[c(170:nrow(RCSites)),]  # Remaining rows

#Randomly select x trib sites for sampling **Percentage/Number of Tribs TBD**
set.seed(25)  # Ensures reproducibility

RCTrib_Random <- RCTribSites %>%
  mutate(group = case_when(
    str_detect(Site, "^RCRACC") ~ "RCRACC",
    str_detect(Site, "^RCLEAD") ~ "RCLEAD",
    str_detect(Site, "^RCKYGR") ~ "RCKYGR",
    str_detect(Site, "^RCCAMP") ~ "RCCAMP",
    str_detect(Site, "^RCOLDT") ~ "RCOLDT",
    str_detect(Site, "^RCCHCK") ~ "RCCHCK",
    str_detect(Site, "^RCCRAB") ~ "RCCRAB",
    TRUE ~ "Other"
  )) %>%
  group_by(group) %>%
  group_split() %>%
  map_dfr(~ {
    group_name <- unique(.x$group)
    n_sample <- case_when(
      group_name %in% c("RCRACC") ~ 2,
      group_name %in% c("RCLEAD", "RCKYGR", "RCCAMP", "RCOLDT", "RCCHCK", "RCCRAB") ~ 1,
      TRUE ~ 0
    )
    if (nrow(.x) >= n_sample & n_sample > 0) {
      slice_sample(.x, n = n_sample)
    } else {
      NULL  # Skip if not enough rows or not in the target groups
    }
  })

RCTrib_Random <- RCTrib_Random %>% select(Site, Latitude, Longitude, Done)
RCMain_Random <- RCMain_Random %>% select(Site, Latitude, Longitude, Done)
RCRandomSites_25 <- rbind(RCMain_Random,RCTrib_Random)

#putting randomized sites into folder
write.csv(RCRandomSites_25, "Randomized Sites 2025/RC_Main_Random_2025_10per_Stratified.csv")






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

#Randomly select x main sites for sampling
BVupperMain_Random<- BVuppermainsites %>% filter(Site %in% sample(BVuppermainsites$Site, 
                                                                  size = 0.1 * nrow(BVuppermainsites)))

BVmiddleMain_Random<- BVmiddlemainsites %>% filter(Site %in% sample(BVmiddlemainsites$Site, 
                                                                    size = 0.1 * nrow(BVmiddlemainsites)))

BVlowerMain_Random<- BVlowermainsites %>% filter(Site %in% sample(BVlowermainsites$Site, 
                                                                  size = 0.1 * nrow(BVlowermainsites)))


#bind selected sites
BVMain_Random <- rbind(BVmiddleMain_Random,BVupperMain_Random,BVlowerMain_Random)

#Split Dataset based trib sites
BVTribSites <- BVSites[c(211:nrow(BVSites)),]  # Remaining rows

#Randomly select x trib sites for sampling **Percentage/Number of Tribs TBD**
set.seed(25)  # Ensures reproducibility

BVTrib_Random <- BVTribSites %>%
  mutate(group = case_when(
    str_detect(Site, "^BVLIKA") ~ "BVLIKA",
    str_detect(Site, "^BVHOCK") ~ "BVHOCK",
    str_detect(Site, "^BVMUSR") ~ "BVMUSR",
    str_detect(Site, "^BVLIMK") ~ "BVLIMK",
    str_detect(Site, "^BVDUCK") ~ "BVDUCK",
    str_detect(Site, "^BVLIHK") ~ "BVLIHK",
    str_detect(Site, "^BVLEEC") ~ "BVLEEC",
    str_detect(Site, "^BVROCK") ~ "BVROCK",
    str_detect(Site, "^BVBIGR") ~ "BVBIGR",
    TRUE ~ "Other"
  )) %>%
  group_by(group) %>%
  group_split() %>%
  map_dfr(~ {
    group_name <- unique(.x$group)
    n_sample <- case_when(
      group_name %in% c("BVLIKA", "BVHOCK", "BVMUSR") ~ 2,
      group_name %in% c("BVLIMK", "BVDUCK", "BVLIHK", "BVLEEC", "BVROCK", "BVBIGR") ~ 1,
      TRUE ~ 0
    )
    if (nrow(.x) >= n_sample & n_sample > 0) {
      slice_sample(.x, n = n_sample)
    } else {
      NULL  # Skip if not enough rows or not in the target groups
    }
  })

BVTrib_Random <- BVTrib_Random %>% select(Site, Latitude, Longitude, Done)
BVMain_Random <- BVMain_Random %>% select(Site, Latitude, Longitude, Done)
BVRandomSites_25 <- rbind(BVMain_Random,BVTrib_Random)


#putting randomized sites into folder
write.csv(BVRandomSites_25, "Randomized Sites 2025/BV_Main_Random_2025_10per_Stratified.csv")







#******NEW POOL*******
#Load Pool MAster Sites
OWSites <- read.csv("Open Water Half Mile Sites.csv")

OWMainSites <- OWSites[1:122, ]

n<- nrow(OWMainSites)

third <- ceiling(n/3)

OWuppermainsites<- OWMainSites[1:third,]
OWmiddlemainsites<- OWMainSites[(third + 1):(2*third),]
OWlowermainsites<- OWMainSites[(2 * third + 1):n,]

#Randomly select x main sites for sampling
OWupperMain_Random<- OWuppermainsites %>% filter(Site %in% sample(OWuppermainsites$Site, 
                                                                  size = 0.1 * nrow(OWuppermainsites)))

OWmiddleMain_Random<- OWmiddlemainsites %>% filter(Site %in% sample(OWmiddlemainsites$Site, 
                                                                    size = 0.1 * nrow(OWmiddlemainsites)))

OWlowerMain_Random<- OWlowermainsites %>% filter(Site %in% sample(OWlowermainsites$Site, 
                                                                  size = 0.1 * nrow(OWlowermainsites)))


#bind selected sites
OWMain_Random <- rbind(OWmiddleMain_Random,OWupperMain_Random,OWlowerMain_Random)

#Split Dataset based trib sites
OWTribSites <- OWSites[c(123:nrow(OWSites)),]  # Remaining rows

#Randomly select x trib sites for sampling **Percentage/Number of Tribs TBD**
set.seed(25)  # Ensures reproducibility

OWTrib_Random <- OWTribSites %>%
  mutate(group = case_when(
    str_detect(Site, "^OWCROK") ~ "OWCROK",
    str_detect(Site, "^OW3MIL") ~ "OW3MIL",
    str_detect(Site, "^OW5MIL") ~ "OW5MIL",
    str_detect(Site, "^OW9MIL") ~ "OW9MIL",
    str_detect(Site, "^OWTTML") ~ "OWTTML",
    str_detect(Site, "^OWTNML") ~ "OWTNML",
    str_detect(Site, "^OWETML") ~ "OWETML",
    str_detect(Site, "^OWHURC") ~ "OWHURC",
    str_detect(Site, "^OWLHRC") ~ "OWLHRC",
    TRUE ~ "Other"
  )) %>%
  group_by(group) %>%
  group_split() %>%
  map_dfr(~ {
    group_name <- unique(.x$group)
    n_sample <- case_when(
      group_name %in% c(
        "OWCROK", "OW3MIL", "OW5MIL", "OW9MIL", "OWTTML",
        "OWTNML", "OWETML", "OWHURC", "OWLHRC"
      ) ~ 1,
      TRUE ~ 0
    )
    if (nrow(.x) >= n_sample & n_sample > 0) {
      slice_sample(.x, n = n_sample)
    } else {
      NULL  # Skip if not enough rows or not in the target groups
    }
  })

OWTrib_Random <- OWTrib_Random %>% select(Site, Latitude, Longitude, Done)
OWMain_Random <- OWMain_Random %>% select(Site, Latitude, Longitude, Done)
OWRandomSites_25 <- rbind(OWMain_Random,OWTrib_Random)

#putting randomized sites into folder
write.csv(OWRandomSites_25, "Randomized Sites 2025/OW_Main_Random_2025_10per_Stratified.csv")






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

#Randomly select x main sites for sampling
WIupperMain_Random<- WIuppermainsites %>% filter(Site %in% sample(WIuppermainsites$Site, 
                                                                  size = 0.1 * nrow(WIuppermainsites)))

WImiddleMain_Random<- WImiddlemainsites %>% filter(Site %in% sample(WImiddlemainsites$Site, 
                                                                    size = 0.1 * nrow(WImiddlemainsites)))

WIlowerMain_Random<- WIlowermainsites %>% filter(Site %in% sample(WIlowermainsites$Site, 
                                                                  size = 0.1 * nrow(WIlowermainsites)))


#bind selected sites
WIMain_Random <- rbind(WImiddleMain_Random,WIupperMain_Random,WIlowerMain_Random)

#Split Dataset based trib sites
WITribSites <- WISites[c(144:151, 179:186, 191:nrow(WISites)),]  # Remaining rows

#Randomly select x trib sites for sampling **Percentage/Number of Tribs TBD**
WITrib_Random <- WITribSites %>%
  mutate(group = case_when(
    str_detect(Site, "^WIFISH") ~ "WIFISH",
    str_detect(Site, "^WIMIDC") ~ "WIMIDC",
    str_detect(Site, "^WIDANA") ~ "WIDANA",
    str_detect(Site, "^WIFNCH") ~ "WIFNCH",
    str_detect(Site, "^WINEWL") ~ "WINEWL",
    TRUE ~ "Other"
  )) %>%
  group_by(group) %>%
  group_split() %>%
  map_dfr(~ {
    group_name <- unique(.x$group)
    sample_n <- case_when(
      group_name == "WIFISH" ~ 2,
      group_name == "WIMIDC" ~ 2,
      group_name == "WIDANA" ~ 1,
      group_name == "WIFNCH" ~ 1,
      group_name == "WINEWL" ~ 1,
      TRUE ~ 0
    )
    if (nrow(.x) >= sample_n & sample_n > 0) {
      slice_sample(.x, n = sample_n)
    } else {
      NULL  # Skip if not enough rows or n == 0
    }
  })



WITrib_Random <- WITrib_Random %>% select(Site, Latitude, Longitude, Site.done)
WIMain_Random <- WIMain_Random %>% select(Site, Latitude, Longitude, Site.done)
WIRandomSites_25 <- rbind(WIMain_Random,WITrib_Random)

#putting randomized sites into folder
write.csv(WIRandomSites_25, "Randomized Sites 2025/WI_Main_Random_2025_10per_Stratified.csv")







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

#Randomly select x main sites for sampling
WNupperMain_Random<- WNuppermainsites %>% filter(Site %in% sample(WNuppermainsites$Site, 
                                                                  size = 0.1 * nrow(WNuppermainsites)))

WNmiddleMain_Random<- WNmiddlemainsites %>% filter(Site %in% sample(WNmiddlemainsites$Site, 
                                                                    size = 0.1 * nrow(WNmiddlemainsites)))

WNlowerMain_Random<- WNlowermainsites %>% filter(Site %in% sample(WNlowermainsites$Site, 
                                                                  size = 0.1 * nrow(WNlowermainsites)))


#bind selected sites
WNMain_Random <- rbind(WNmiddleMain_Random,WNupperMain_Random,WNlowerMain_Random)

#Split Dataset based trib sites
WNTribSites <- WNSites[c(153:nrow(WNSites)),]  # Remaining rows

#Randomly select x trib sites for sampling **Percentage/Number of Tribs TBD**
set.seed(25)  # Ensures reproducibility

WNTrib_Random <- WNTribSites %>%
  mutate(group = case_when(
    str_detect(Site, "^WNPOCA") ~ "WNPOCA",
    str_detect(Site, "^WNLGUC") ~ "WNLGUC",
    str_detect(Site, "^WNCOAL") ~ "WNCOAL",
    str_detect(Site, "^WNHOLP") ~ "WNHOLP",
    str_detect(Site, "^WNDOHG") ~ "WNDOHG",
    str_detect(Site, "^WNGUAC") ~ "WNGUAC",
    str_detect(Site, "^WNSECB") ~ "WNSECB",
    str_detect(Site, "^WNLLCB") ~ "WNLLCB",
    str_detect(Site, "^WNFRLN") ~ "WNFRLN",
    str_detect(Site, "^WNFARC") ~ "WNFARC",
    str_detect(Site, "^WNBILL") ~ "WNBILL",
    str_detect(Site, "^WNARMC") ~ "WNARMC",
    str_detect(Site, "^WNGALA") ~ "WNGALA",
    str_detect(Site, "^WNTTQC") ~ "WNTTQC",
    str_detect(Site, "^WNDAVC") ~ "WNDAVC",
    str_detect(Site, "^WNTYLC") ~ "WNTYLC",
    str_detect(Site, "^WNWDWB") ~ "WNWDWB",
    str_detect(Site, "^WNELKR") ~ "WNELKR",
    TRUE ~ "Other"
  )) %>%
  group_by(group) %>%
  group_split() %>%
  map_dfr(~ {
    group_name <- unique(.x$group)
    n_sample <- case_when(
      group_name == "WNPOCA" ~ 2,
      group_name == "WNLGUC" ~ 1,
      group_name == "WNCOAL" ~ 2,
      group_name == "WNHOLP" ~ 1,
      group_name == "WNDOHG" ~ 1,
      group_name == "WNGUAC" ~ 1,
      group_name == "WNSECB" ~ 1,
      group_name == "WNLLCB" ~ 1,
      group_name == "WNFRLN" ~ 1,
      group_name == "WNFARC" ~ 1,
      group_name == "WNBILL" ~ 1,
      group_name == "WNARMC" ~ 1,
      group_name == "WNGALA" ~ 1,
      group_name == "WNTTQC" ~ 1,
      group_name == "WNDAVC" ~ 1,
      group_name == "WNTYLC" ~ 1,
      group_name == "WNWDWB" ~ 1,
      group_name == "WNELKR" ~ 2,
      TRUE ~ 0
    )
    if (nrow(.x) >= n_sample & n_sample > 0) {
      slice_sample(.x, n = n_sample)
    } else {
      NULL  # Skip if not enough rows or n == 0
    }
  })

WNTrib_Random <- WNTrib_Random %>% select(Site, Latitude, Longtitude, Done)
WNMain_Random <- WNMain_Random %>% select(Site, Latitude, Longtitude, Done)
WNRandomSites_25 <- rbind(WNMain_Random,WNTrib_Random)

#write.csv(WNBackupSites, "Randomized Sites 2025/Winfield_Backup_Sites_Random_2025_10per_split.csv")

#putting randomized sites into folder
write.csv(WNRandomSites_25, "Randomized Sites 2025/WN_Main_Random_2025_10per_Stratified.csv")
