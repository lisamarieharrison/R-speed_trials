#inter-observer analysis between Vic and Lisa during Oct 2015 speed trials

setwd("C:/Users/Lisa/Documents/phd/aerial survey/data/speed trials/interobserver")
dat <- read.csv("vic_lisa.csv", skip = 1, header = T)

#vector of lisa's speed for each trial
lisa_speed <- c(80, 100, 100, 80, 80, 100, 100, 80, 80, 100, 100, 80, 80, 100, 100, 80)

#remove space after some baitfish observations
dat$Species[dat$Species == "B "] <- "B" 

#combine white sharks and other sharks 
dat$Species[dat$Species == "W"] <- "S"
dat$Species <- factor(dat$Species)

#print table for all species for each trial
for (i in unique(obs$Trial_number)) {
  
  if (is.na(i)) next()
  
  print(paste("Trial ", i))
  print(table(dat$Species[dat$Trial_number == i], dat$Speed_knots[dat$Trial_number == i]))
  
}


#table of sightings by Observer
table(dat$Species, dat$Observer)

#table of sightings by flight speed
dat$Speed_knots[dat$Observer == "Both"] <- "Both"
table(dat$Species, dat$Speed_knots)

#you can divide the whole table by 16 to get the mean number of sightings per survey
round(table(dat$Species, dat$Speed_knots)/16, 2)


#read in individual observations (not matched up against each other)
setwd("C:/Users/Lisa/Documents/phd/aerial survey/data/speed trials")
lisa <- read.csv("speed_trials_datasheet_lisa.csv", skip = 1, header = T)
vic <- read.csv("speed_trials_datasheet_vic.csv", skip = 1, header = T)
lisa <- lisa[lisa$Type == "S", ]
vic <- vic[vic$Type == "S", ]

#Lisa saw more sharks when going faster, but Vic saw more when going slower 
#could be handy to show inter-observer differences
table(vic$Species, vic$Speed_knots) #same table but only for Vic
table(lisa$Species, lisa$Speed_knots) #same table but only for Lisa



