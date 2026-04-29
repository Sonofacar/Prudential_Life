#############
# Libraries #
#############

library(ggplot2)


#########################
# Import and clean data #
#########################

source("clean_data.R")


train |>
  (\(df) {
     out <- data.frame(Column = character(0), NAs = numeric(0))
     i <- 1
     for (col in colnames(df)) {
       val <- df[[col]] |> is.na() |> sum() |> (\(.) . / nrow(df))()
       out[i, ] <- c(col, val)
       i <- i + 1
     }
     out
  })()
test |>
  (\(df) {
     out <- data.frame(Column = character(0), NAs = numeric(0))
     i <- 1
     for (col in colnames(df)) {
       val <- df[[col]] |> is.na() |> sum() |> (\(.) . / nrow(df))()
       out[i, ] <- c(col, val)
       i <- i + 1
     }
     out
  })()
# I will drop any column that is more than 50% NAs:
# - Family_Hist_3
# - Family_Hist_5
# - Medical_History_10
# - Medical_History_15
# - Medical_History_24
# - Medical_History_32
# This means we need to figure out how to fill NAs in:
# - Employment_Info_1   (continuous)
# - Employment_Info_4   (continuous)
# - Employment_Info_6   (continuous)
# - Insurance_History_5 (continuous)
# - Family_Hist_2       (continuous)
# - Family_Hist_4       (continuous)
# - Medical_History_1   (discrete)
