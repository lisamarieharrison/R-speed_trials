setwd("C:/Users/Lisa/Documents/phd/aerial survey/data/speed trials")
lisa <- read.csv("speed_trials_datasheet_lisa.csv", skip = 1, header = T)
vic <- read.csv("speed_trials_datasheet_vic.csv", skip = 1, header = T)
rob <- read.csv("speed_trials_datasheet_rob.csv", skip = 1, header = T)
sally <- read.csv("speed_trials_datasheet_Sally.csv", skip = 1, header = T)

lisa_speed <- c(80, 100, 100, 80, 80, 100, 100, 80, 80, 100, 100, 80, 80, 100, 100, 80)

rob$Type[rob$Type == "S "] <- "S"
rob$Species[rob$Species == "BOT "] <- "BOT"

obs <- rbind(lisa[lisa$Type == "S", 1:32], vic[vic$Type == "S", 1:32], rob[rob$Type == "S", 1:32],
             sally[sally$Type == "S", 1:32])
obs <- obs[as.numeric(obs$Secondary) != 2 | is.na(obs$Secondary), ]
obs$Species <- as.character(obs$Species)
obs$Observer <- as.character(obs$Observer)
obs$Number[is.na(obs$Number)] <- 1

for (i in 1:nrow(obs)) {
  if (obs$Number[i] > 1 & obs$Species[i] == "B") {
    for (j in 1:obs$Number[i]) {
      obs <- rbind(obs, obs[i, ])
    }
  }
}

for (i in unique(obs$Trial_number)) {
  
  if (is.na(i)) next()
  
  print(paste("Trial ", i, "Lisa speed = ", lisa_speed[i]))
  print(table(obs$Species[obs$Trial_number == i], obs$Observer[obs$Trial_number == i]))
  
}




