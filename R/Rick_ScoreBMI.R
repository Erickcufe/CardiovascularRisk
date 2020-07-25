#' Risk Score Body Mass Index
#'
#' A function that return the risk level of Cardiovascular Disease in Central Latin America. Based on WHO cardiovascular disease risk.
#'
#' @param gender Binary form, 0 = female; 1 = male.
#' @param smoke Binary form, 0 = non-smoker; 1 = smoker.
#' @param age Age in years
#' @param sbp Systolic Blood Pressure in mmHG
#' @param bmi Body Mass Index (kg/m^2)
#'
#' @return
#' A data.frame with the Score of risk level of Cardiovascular Disease
#'
#'
#' @examples
#' Risk_ScoreBMI(gender = 0, smoke = 1, age = 60, sbp = 169, bmi = 29)
#'
#' Age <- c(44, 57, 66, 50, 71, 37)
#' Gender <- c(0,1,0,1,1, 1)
#' Smoking <- c(1,1,0,1,0, 1)
#' Systolic_Blood_Pressure <- c(120, 137, 144, 178, 163, 139)
#' BMI <- c(20,34,30,27,21, 19)
#' Risk_ScoreBMI(gender = Gender,smoke = Smoking, age = Age, sbp = Systolic_Blood_Pressure,bmi = BMI)
#'
#' @rdname Risk_ScoreBMI
#' @export
Risk_ScoreBMI <- function(gender, smoke, age, sbp, bmi){
  UseMethod("Risk_ScoreBMI")
}

#' @return \code{NULL}
#'
#' @rdname Risk_ScoreBMI
#' @export
Risk_ScoreBMI.vector <- function(gender, smoke, age, sbp, bmi){

  age_levels <- function(x){
    if(x < 40){x <- 8}
    if(x > 75){x <- 8}
    if(x >= 70 & x <= 74){x <- 7}
    if(x >= 65 & x <= 69){x <- 6}
    if(x >= 60 & x <= 64){x <- 5}
    if(x >= 55 & x <= 59){x <- 4}
    if(x >= 50 & x <= 54){x <- 3}
    if(x >= 45 & x <= 49){x <- 2}
    if(x >= 40 & x <= 44){x <- 1}
    return(x)
  }

  sbp_levels <- function(x){

    if(x < 120){x <- 1}
    if(x >= 180){x <- 5}
    if(x >= 160 & x <= 179){x <- 4}
    if(x >= 140 & x <= 159){x <- 3}
    if(x >= 120 & x <= 139){x <- 2}

    return(x)
  }

  bmi_levels <- function(x) {
    if(x < 20){x <- 1}
    if(x >= 20 & x <= 24){x <- 2}
    if(x >= 25 & x <= 29){x <- 3}
    if(x >= 30 & x <= 35){x <- 4}
    if(x >= 35){x <- 5}

    return(x)
  }

  age_1 <- sapply(age, age_levels)
  sbp_1 <- sapply(sbp, sbp_levels)
  bmi_1 <- sapply(bmi, bmi_levels)


  df_dummy <- data.frame(Gender = gender, Smoke = smoke,
                         Age = age,
                         SBP = sbp, BMI = bmi)
  df <- data.frame(Gender = gender, Smoke = smoke,
                   Age = age_1,
                   SBP = sbp_1, BMI = bmi_1)

  if(length(which(df$Age == 8)) > 0){
    thebad <- which(df$Age == 8)
    message(paste("The patient number:", paste(which(df$Age == 8), collapse = ","), "has the age out of range to calculate the risk score"))
    # df <- df[df$Age!= 8, ]
  }
  select <- vector()
  for (i in 1:nrow(df)) {
    if(df$Age[i] == 8){
      select[i] <- "NR"
    } else {
      dat <- CardiovascularRisk::ref2
      select[i] <- dat[dat$Gender == df$Gender[i] &
                         dat$Smoke == df$Smoke[i] & dat$Age == df$Age[i] &
                         dat$SBP == df$SBP[i] & dat$BMI == df$BMI[i],
                       "Score"]
    }
  }

  result <- data.frame(df_dummy, Score = select)
  return(result)

}

#' @return \code{NULL}
#'
#' @rdname Risk_ScoreBMI
#' @export
Risk_ScoreBMI.numeric <- function(gender, smoke, age, sbp, bmi){
  age_levels <- function(x){
    if(x < 40){x <- 8}
    if(x > 75){x <- 8}
    if(x >= 70 & x <= 74){x <- 7}
    if(x >= 65 & x <= 69){x <- 6}
    if(x >= 60 & x <= 64){x <- 5}
    if(x >= 55 & x <= 59){x <- 4}
    if(x >= 50 & x <= 54){x <- 3}
    if(x >= 45 & x <= 49){x <- 2}
    if(x >= 40 & x <= 44){x <- 1}
    return(x)
  }

  sbp_levels <- function(x){

    if(x < 120){x <- 1}
    if(x >= 180){x <- 5}
    if(x >= 160 & x <= 179){x <- 4}
    if(x >= 140 & x <= 159){x <- 3}
    if(x >= 120 & x <= 139){x <- 2}

    return(x)
  }

  bmi_levels <- function(x) {
    if(x < 20){x <- 1}
    if(x >= 20 & x <= 24){x <- 2}
    if(x >= 25 & x <= 29){x <- 3}
    if(x >= 30 & x <= 35){x <- 4}
    if(x >= 35){x <- 5}

    return(x)
  }

  age_1 <- sapply(age, age_levels)
  sbp_1 <- sapply(sbp, sbp_levels)
  bmi_1 <- sapply(bmi, bmi_levels)


  df_dummy <- data.frame(Gender = gender, Smoke = smoke,
                         Age = age,
                         SBP = sbp, BMI = bmi)
  df <- data.frame(Gender = gender, Smoke = smoke,
                   Age = age_1,
                   SBP = sbp_1, BMI = bmi_1)

  if(length(which(df$Age == 8)) > 0){
    thebad <- which(df$Age == 8)
    message(paste("The patient number:", paste(which(df$Age == 8), collapse = ","), "has the age out of range to calculate the risk score"))
    # df <- df[df$Age!= 8, ]
  }
  select <- vector()
  for (i in 1:nrow(df)) {
    if(df$Age[i] == 8){
      select[i] <- "NR"
    } else {
      dat <- CardiovascularRisk::ref2
      select[i] <- dat[dat$Gender == df$Gender[i] &
                         dat$Smoke == df$Smoke[i] & dat$Age == df$Age[i] &
                         dat$SBP == df$SBP[i] & dat$BMI == df$BMI[i],
                       "Score"]
    }
  }

  result <- data.frame(df_dummy, Score = select)
  return(result)
}

#' Risk Score Body Mass Index
#'
#'
#' @param df A dataframe with columns in the following order: Gender (0 = female, 1 = male), Smoke (0 = non-smoker, 1 = smoker), age (year) , sbp (Sytolic Blood Pressure in mmHG), BMI (Body Mass Index in kg/m^2)
#'
#' @return \code{NULL}
#'
#' @author
#' Erick Cuevas-Fernández
#'
#'
#'
#' @examples
#' Age <- c(44, 57, 66, 50, 71, 37)
#' Gender <- c(0,1,0,1,1, 1)
#' Smoking <- c(1,1,0,1,0, 1)
#' Systolic_Blood_Pressure <- c(120, 137, 144, 178, 163, 139)
#' bmi <- c(20,34,30,27,21, 19)
#' df <- data.frame(Gender, Smoking, Age, Systolic_Blood_Pressure, bmi)
#'
#' Risk_ScoreBMI(df)
#'
#' @rdname Risk_ScoreBMI
#' @export
Risk_ScoreBMI.data.frame <- function(df){

  colnames(df) <- c("Gender", "Smoke", "Age", "SBP", "BMI")
  df_old <- df
  colnames(df_old) <- c("Gender", "Smoke", "Age", "SBP", "BMI")
  age_levels <- function(x){
    if(x < 40){x <- 8}
    if(x > 75){x <- 8}
    if(x >= 70 & x <= 74){x <- 7}
    if(x >= 65 & x <= 69){x <- 6}
    if(x >= 60 & x <= 64){x <- 5}
    if(x >= 55 & x <= 59){x <- 4}
    if(x >= 50 & x <= 54){x <- 3}
    if(x >= 45 & x <= 49){x <- 2}
    if(x >= 40 & x <= 44){x <- 1}
    return(x)
  }

  sbp_levels <- function(x){

    if(x < 120){x <- 1}
    if(x >= 180){x <- 5}
    if(x >= 160 & x <= 179){x <- 4}
    if(x >= 140 & x <= 159){x <- 3}
    if(x >= 120 & x <= 139){x <- 2}

    return(x)
  }

  bmi_levels <- function(x) {
    if(x < 20){x <- 1}
    if(x >= 20 & x <= 24){x <- 2}
    if(x >= 25 & x <= 29){x <- 3}
    if(x >= 30 & x <= 35){x <- 4}
    if(x >= 35){x <- 5}

    return(x)
  }

  df$Age <- sapply(df$Age, age_levels)
  df$SBP <- sapply(df$SBP, sbp_levels)
  df$BMI <- sapply(df$BMI, bmi_levels)

  if(length(which(df$Age == 8)) > 0){
    thebad <- which(df$Age == 8)
    message(paste("The patient number:", paste(which(df$Age == 8), collapse = ","), "has the age out of range to calculate the risk score"))
    # df <- df[df$Age!= 8, ]
  }
  select <- vector()
  for (i in 1:nrow(df)) {
    if(df$Age[i] == 8){
      select[i] <- "NR"
    } else {
      dat <- CardiovascularRisk::ref2
      select[i] <- dat[dat$Gender == df$Gender[i] &
                         dat$Smoke == df$Smoke[i] & dat$Age == df$Age[i] &
                         dat$SBP == df$SBP[i] & dat$BMI == df$BMI[i],
                       "Score"]
    }
  }

  result <- data.frame(df_old, Score = select)
  return(result)


}


