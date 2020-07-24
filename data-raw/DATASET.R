## code to prepare `DATASET` dataset goes here

ref1 <- read.csv("Diabetes_riskfactor.csv")
ref2 <- read.csv("BodyMassIndex_cardiovascular.csv")

usethis::use_data(ref1, overwrite = TRUE)
usethis::use_data(ref2, overwrite = TRUE)
