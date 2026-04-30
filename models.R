##########################
# Set up parallelization #
##########################

library(parallel)
cores <- detectCores() - 2


#########################
# Import and clean data #
#########################

source("clean_data.R")


#####################
# Linear Regression #
#####################

mcmapply(
  function(l, in_data, out_data) {
    in_data |>
      within({
        Response <- Response == l
      }) |>
      glm(Response ~ ., data = _, family = binomial()) |>
      predict(newdata = out_data) |>
      (\(.) {
         names(.) <- NULL
         .
      })()
  },
  1:8,
  MoreArgs = list(in_data = train, out_data = test),
  mc.cores = cores
) |>
  apply(1, \(.) (1:8)[max(.) == .]) |>
  (\(.) {
     data.frame(Id = test[["Id"]], Response = .)
  })() |>
  write.csv("logistic_regression.csv", row.names = FALSE)

