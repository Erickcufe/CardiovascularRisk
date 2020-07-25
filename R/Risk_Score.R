#' Risk Score
#'
#' A function that return the risk level of Cardiovascular Disease in Central Latin America. Based on WHO cardiovascular disease risk.
#'
#' @param diabetes Binary form, non-diabetic = 0; diabetic = 1.
#' @param gender Binary form, 0 = female; 1 = male.
#' @param smoke Binary form, 0 = non-smoker; 1 = smoker.
#' @param age Age in years
#' @param sbp Systolic Blood Pressure in mmHG
#' @param cholesterol Total cholesterol in mmol/l
#'
#' @return
#' A data.frame with the Score of risk level of Cardiovascular Disease
#'
#'
#' @examples
#' Risk_Score(diabetes = 1, gender = 0, smoke = 1, age = 60,
#                              sbp = 169, cholesterol = 8)
#'
#' Age <- c(44, 57, 66, 50, 71, 37)
#' Gender <- c(0,1,0,1,1, 1)
#' Smoking <- c(1,1,0,1,0, 1)
#' Systolic_Blood_Pressure <- c(120, 137, 144, 178, 163, 139)
#' Diabetes <- c(1,1,1,0,1, 1)
#' cholesterol <- c(0, 5.8, 6.5, 0, 7.2, 6)
#' Risk_Score(diabetes = Diabetes, gender = Gender,smoke = Smoking, age = Age, sbp = Systolic_Blood_Pressure,cholesterol = cholesterol)
#'
#' @rdname Risk_Score
#' @export
Risk_Score <- function(diabetes, gender, smoke, age, sbp, cholesterol){
  UseMethod("Risk_Score")
}

#' @return \code{NULL}
#'
#' @rdname Risk_Score
#' @export
Risk_Score.vector <- function(diabetes, gender, smoke, age, sbp, cholesterol){

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

  cho_levels <- function(x) {
    if(x < 4){x <- 1}
    if(x >= 4 & x <= 4.9){x <- 2}
    if(x >= 5 & x <= 5.9){x <- 3}
    if(x >= 6 & x <= 6.9){x <- 4}
    if(x >= 7){x <- 5}

    return(x)
  }

  age_1 <- sapply(age, age_levels)
  sbp_1 <- sapply(sbp, sbp_levels)
  cholesterol_1 <- sapply(cholesterol, cho_levels)


  df_dummy <- data.frame(Diabetes = diabetes, Gender = gender, Smoke = smoke,
                         Age = age,
                         SBP = sbp, Total_Cholesterol = cholesterol)
  df <- data.frame(Diabetes = diabetes, Gender = gender, Smoke = smoke,
                   Age = age_1,
                   SBP = sbp_1, Total_Cholesterol = cholesterol_1)

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
      dat <- CardiovascularRisk::ref1
      select[i] <- dat[dat$Diabetes == df$Diabetes[i] & dat$Gender == df$Gender[i] &
                         dat$Smoke == df$Smoke[i] & dat$Age == df$Age[i] &
                         dat$SBP == df$SBP[i] & dat$Cholesterol == df$Total_Cholesterol[i],
                       "Score"]
    }
  }

  result <- data.frame(df_dummy, Score = select)
  return(result)

}

#' @return \code{NULL}
#'
#' @rdname Risk_Score
#' @export
Risk_Score.numeric <- function(diabetes, gender, smoke, age, sbp, cholesterol){
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

  cho_levels <- function(x) {
    if(x < 4){x <- 1}
    if(x >= 4 & x <= 4.9){x <- 2}
    if(x >= 5 & x <= 5.9){x <- 3}
    if(x >= 6 & x <= 6.9){x <- 4}
    if(x >= 7){x <- 5}

    return(x)
  }

  age_1 <- sapply(age, age_levels)
  sbp_1 <- sapply(sbp, sbp_levels)
  cholesterol_1 <- sapply(cholesterol, cho_levels)


  df_dummy <- data.frame(Diabetes = diabetes, Gender = gender, Smoke = smoke,
                         Age = age,
                         SBP = sbp, Total_Cholesterol = cholesterol)
  df <- data.frame(Diabetes = diabetes, Gender = gender, Smoke = smoke,
                   Age = age_1,
                   SBP = sbp_1, Total_Cholesterol = cholesterol_1)

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
      dat <- CardiovascularRisk::ref1
      select[i] <- dat[dat$Diabetes == df$Diabetes[i] & dat$Gender == df$Gender[i] &
                         dat$Smoke == df$Smoke[i] & dat$Age == df$Age[i] &
                         dat$SBP == df$SBP[i] & dat$Cholesterol == df$Total_Cholesterol[i],
                       "Score"]
    }
  }

  result <- data.frame(df_dummy, Score = select)
  return(result)
}

#' Risk Score
#'
#'
#' @param df A dataframe with columns in the following order: Diabetes (0 = non-diabetic, 1 = diabetic), Gender (0 = female, 1 = male), Smoke (0 = non-smoker, 1 = smoker), age (year) , sbp (Sytolic Blood Pressure in mmHG), cholesterol (Total cholesterol in mmol / l)
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
#' Diabetes <- c(1,1,1,0,1, 1)
#' cholesterol <- c(0, 5.8, 6.5, 0, 7.2, 6)
#' df <- data.frame(Diabetes, Gender, Smoking, Age, Systolic_Blood_Pressure, cholesterol)
#'
#' Risk_Score(df)
#'
#' @rdname Risk_Score
#' @export
Risk_Score.data.frame <- function(df){

  colnames(df) <- c("Diabetes", "Gender", "Smoke", "Age", "SBP", "Total_Cholesterol")
  df_old <- df
  colnames(df_old) <- c("Diabetes", "Gender", "Smoke", "Age", "SBP", "Total_Cholesterol")
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

  cho_levels <- function(x) {
    if(x < 4){x <- 1}
    if(x >= 4 & x <= 4.9){x <- 2}
    if(x >= 5 & x <= 5.9){x <- 3}
    if(x >= 6 & x <= 6.9){x <- 4}
    if(x >= 7){x <- 5}

    return(x)
  }

  df$Age <- sapply(df$Age, age_levels)
  df$SBP <- sapply(df$SBP, sbp_levels)
  df$Total_Cholesterol <- sapply(df$Total_Cholesterol, cho_levels)

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
      dat <- CardiovascularRisk::ref1
      select[i] <- dat[dat$Diabetes == df$Diabetes[i] & dat$Gender == df$Gender[i] &
                         dat$Smoke == df$Smoke[i] & dat$Age == df$Age[i] &
                         dat$SBP == df$SBP[i] & dat$Cholesterol == df$Total_Cholesterol[i],
                       "Score"]
    }
  }

  result <- data.frame(df_old, Score = select)
  return(result)


}


