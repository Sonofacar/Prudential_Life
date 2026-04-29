clean <- function(df) {
  within(df, {
    Product_Info_1 <- as.factor(Product_Info_1)
    Product_Info_2 <- as.factor(Product_Info_2)
    Product_Info_3 <- as.factor(Product_Info_3)
    Product_Info_5 <- as.factor(Product_Info_5)
    Product_Info_6 <- as.factor(Product_Info_6)
    Product_Info_7 <- as.factor(Product_Info_7)
    Employment_Info_2 <- as.factor(Employment_Info_2)
    Employment_Info_3 <- as.factor(Employment_Info_3)
    Employment_Info_5 <- as.factor(Employment_Info_5)
    InsuredInfo_1 <- as.factor(InsuredInfo_1)
    InsuredInfo_2 <- as.factor(InsuredInfo_2)
    InsuredInfo_3 <- as.factor(InsuredInfo_3)
    InsuredInfo_4 <- as.factor(InsuredInfo_4)
    InsuredInfo_5 <- as.factor(InsuredInfo_5)
    InsuredInfo_6 <- as.factor(InsuredInfo_6)
    InsuredInfo_7 <- as.factor(InsuredInfo_7)
    Insurance_History_1 <- as.factor(Insurance_History_1)
    Insurance_History_2 <- as.factor(Insurance_History_2)
    Insurance_History_3 <- as.factor(Insurance_History_3)
    Insurance_History_4 <- as.factor(Insurance_History_4)
    Insurance_History_7 <- as.factor(Insurance_History_7)
    Insurance_History_8 <- as.factor(Insurance_History_8)
    Insurance_History_9 <- as.factor(Insurance_History_9)
    Family_Hist_1 <- as.factor(Family_Hist_1)
    Medical_History_2 <- as.factor(Medical_History_2)
    Medical_History_3 <- as.factor(Medical_History_3)
    Medical_History_4 <- as.factor(Medical_History_4)
    Medical_History_5 <- as.factor(Medical_History_5)
    Medical_History_6 <- as.factor(Medical_History_6)
    Medical_History_7 <- as.factor(Medical_History_7)
    Medical_History_8 <- as.factor(Medical_History_8)
    Medical_History_9 <- as.factor(Medical_History_9)
    Medical_History_11 <- as.factor(Medical_History_11)
    Medical_History_12 <- as.factor(Medical_History_12)
    Medical_History_13 <- as.factor(Medical_History_13)
    Medical_History_14 <- as.factor(Medical_History_14)
    Medical_History_16 <- as.factor(Medical_History_16)
    Medical_History_17 <- as.factor(Medical_History_17)
    Medical_History_18 <- as.factor(Medical_History_18)
    Medical_History_19 <- as.factor(Medical_History_19)
    Medical_History_20 <- as.factor(Medical_History_20)
    Medical_History_21 <- as.factor(Medical_History_21)
    Medical_History_22 <- as.factor(Medical_History_22)
    Medical_History_23 <- as.factor(Medical_History_23)
    Medical_History_25 <- as.factor(Medical_History_25)
    Medical_History_26 <- as.factor(Medical_History_26)
    Medical_History_27 <- as.factor(Medical_History_27)
    Medical_History_28 <- as.factor(Medical_History_28)
    Medical_History_29 <- as.factor(Medical_History_29)
    Medical_History_30 <- as.factor(Medical_History_30)
    Medical_History_31 <- as.factor(Medical_History_31)
    Medical_History_33 <- as.factor(Medical_History_33)
    Medical_History_34 <- as.factor(Medical_History_34)
    Medical_History_35 <- as.factor(Medical_History_35)
    Medical_History_36 <- as.factor(Medical_History_36)
    Medical_History_37 <- as.factor(Medical_History_37)
    Medical_History_38 <- as.factor(Medical_History_38)
    Medical_History_39 <- as.factor(Medical_History_39)
    Medical_History_40 <- as.factor(Medical_History_40)
    Medical_History_41 <- as.factor(Medical_History_41)
    Summed_Medical_Keywords <- cbind(
      Medical_Keyword_1, Medical_Keyword_2, Medical_Keyword_3,
      Medical_Keyword_4, Medical_Keyword_5, Medical_Keyword_6,
      Medical_Keyword_7, Medical_Keyword_8, Medical_Keyword_9,
      Medical_Keyword_10, Medical_Keyword_11, Medical_Keyword_12,
      Medical_Keyword_13, Medical_Keyword_14, Medical_Keyword_15,
      Medical_Keyword_16, Medical_Keyword_17, Medical_Keyword_18,
      Medical_Keyword_19, Medical_Keyword_20, Medical_Keyword_21,
      Medical_Keyword_22, Medical_Keyword_23, Medical_Keyword_24,
      Medical_Keyword_25, Medical_Keyword_26, Medical_Keyword_27,
      Medical_Keyword_28, Medical_Keyword_29, Medical_Keyword_30,
      Medical_Keyword_31, Medical_Keyword_32, Medical_Keyword_33,
      Medical_Keyword_34, Medical_Keyword_35, Medical_Keyword_36,
      Medical_Keyword_37, Medical_Keyword_38, Medical_Keyword_39,
      Medical_Keyword_40, Medical_Keyword_41, Medical_Keyword_42,
      Medical_Keyword_43, Medical_Keyword_44, Medical_Keyword_45,
      Medical_Keyword_46, Medical_Keyword_47, Medical_Keyword_48
    ) |>
      apply(1, sum)
    rm(Family_Hist_3, Family_Hist_5, Medical_History_10, Medical_History_15,
       Medical_History_24, Medical_History_32)
    rm(Medical_Keyword_1, Medical_Keyword_2, Medical_Keyword_3,
       Medical_Keyword_4, Medical_Keyword_5, Medical_Keyword_6,
       Medical_Keyword_7, Medical_Keyword_8, Medical_Keyword_9,
       Medical_Keyword_10, Medical_Keyword_11, Medical_Keyword_12,
       Medical_Keyword_13, Medical_Keyword_14, Medical_Keyword_15,
       Medical_Keyword_16, Medical_Keyword_17, Medical_Keyword_18,
       Medical_Keyword_19, Medical_Keyword_20, Medical_Keyword_21,
       Medical_Keyword_22, Medical_Keyword_23, Medical_Keyword_24,
       Medical_Keyword_25, Medical_Keyword_26, Medical_Keyword_27,
       Medical_Keyword_28, Medical_Keyword_29, Medical_Keyword_30,
       Medical_Keyword_31, Medical_Keyword_32, Medical_Keyword_33,
       Medical_Keyword_34, Medical_Keyword_35, Medical_Keyword_36,
       Medical_Keyword_37, Medical_Keyword_38, Medical_Keyword_39,
       Medical_Keyword_40, Medical_Keyword_41, Medical_Keyword_42,
       Medical_Keyword_43, Medical_Keyword_44, Medical_Keyword_45,
       Medical_Keyword_46, Medical_Keyword_47, Medical_Keyword_48)
  })
}

na_map <- function(df, cols, output = list()) {
  # Return a list of models to fill NA values with
  sapply(
    cols,
    function(col) {
      sub("[0-9]", "", col) |>
        grep(colnames(df)) |>
        (\(.) df[.])() |>
        lm(
          paste0(col, " ~ .") |> as.formula(),
          data = _
        )
    },
    simplify = FALSE
  )
}

apply_na_map <- function(df, map) {
  row_func <- function(row, col) {
    ifelse(
      is.na(row[col]),
      tryCatch(
        predict(
          map[[col]],
          newdata = row |> t() |> as.data.frame()
        )[[1]],
        error = function(e) {
          df[!is.na(df[[col]]), col] |>
            mean()
      }),
      row[col]
    )
  }
  col_func <- function(col) {
    apply(
      df,
      1,
      row_func,
      col = col
    ) |>
      (\(.) {
         rownames(.) <- NULL
         .
      })() |>
      as.double()
  }
  df[names(map)] <- names(map) |>
    sapply(col_func) |>
    as.data.frame()
  df
}

train_raw <- read.csv("train.csv") |>
  clean()
test_raw <- read.csv("test.csv") |>
  clean()

map <- rbind(train_raw, within(test_raw, Response <- NA)) |>
  (\(df) {
     na_map(
       df,
       colnames(train_raw) |>
         sapply(\(.) df[[.]] |> is.na() |> sum()) |>
         (\(.) .[. > 0])() |>
         (\(.) .[order(.) |> rev()])() |>
         names()
     )
  })()

train <- apply_na_map(train_raw, map)
test <- apply_na_map(test_raw, map)

# Clean up
rm(train_raw, test_raw, map, na_map, apply_na_map)
