
# Packages ----------------------------------------------------------------

library(tidyverse)


# Run all simulation scripts ----------------------------------------------

# Simulations to include
sims.include <- c(
  # "Outcome",
  "Age",
  "SampleSize",
  "Retp",
  "FWIT-CIT"
)

# Retrieve paths to all the simulation files
sim.files <- list.files("code/", full.names = TRUE) |> 
  str_subset("Simulation")
sim.files <- sim.files[str_detect(sim.files, paste(sims.include, collapse = "|"))]

# Run all the files
start.time <- Sys.time()
for(file in sim.files){
  source(file)
}
(sim.time <- difftime(Sys.time(), start.time, units = "mins"))