#Running Anchovy Bay in a closed loop
library(here); library(data.table); library(Rpath)

#Use balanced Anchovy Bay model
load(here('data', 'AB.params.rda'))
load(here('data', 'AB.rda'))

# create a scenario
AB.base <- rsim.scenario(AB, AB.params, 1:100)

#Determine reference point for cod----
#Turn off fishing to get a proxy for b0
gear <- AB.params$model[Type == 3, Group]
for(i in 1:length(gear)){
  AB.base <- adjust.fishing(AB.base, parameter = 'ForcedEffort', group = gear[i], 
                            value = 0, sim.year = 0:100)
}
AB.b0 <- rsim.run(AB.base, method = 'RK4', 1:25)

#Extract cod data and find b0
cod <- extract.node(AB.b0, 'cod')
cod.b0  <- max(cod$Biomass)

#Set reference point of 1/2 b0
cod.ref <- .5 * cod.b0

#control rule function----
#This modifies the effort matrix of the Rsim scenario object
bio.rule <- function(Rsim.scenario, Rsim.run, group, gear, ref.point, year){
  group.num <- which(Rsim.scenario$params$spname == group)
  gear.num  <- which(Rsim.scenario$params$spname == gear) - Rsim.scenario$params$NUM_BIO
  current.effort <- Rsim.scenario$fishing$ForcedEffort[(year - 1)*12 + 1, gear.num]
  if(Rsim.run$end_state$Biomass[group.num] > ref.point){
    Rsim.scenario <- adjust.fishing(Rsim.scenario, 'ForcedEffort', group = gear, 
                                    sim.year = year + 1, value = current.effort * 1.05)
  }
  if(Rsim.run$end_state$Biomass[group.num] < ref.point){
    Rsim.scenario <- adjust.fishing(Rsim.scenario, 'ForcedEffort', group = gear, 
                                    sim.year = year + 1, value = current.effort * .75)
  }
  return(Rsim.scenario)
}

#Run multistep scenario----
AB.base <- rsim.scenario(AB, AB.params, 1:100)
AB.init <- rsim.run(AB.base, method = 'AB', 1:25)
AB.full <- copy(AB.init)
for(i in 25:99){
  AB.base <- bio.rule(AB.base, AB.full, 'cod', 'trawlers', cod.ref, i)
  AB.full <- rsim.step(AB.base, AB.full, method = 'AB', i + 1)
}

#Visualize results
rsim.plot(AB.full, groups[1:11])
cod <- extract.node(AB.full, 'cod')
plot(cod$Biomass, xlab = 'Month', ylab = 'Biomass')
abline(h = cod.ref)
