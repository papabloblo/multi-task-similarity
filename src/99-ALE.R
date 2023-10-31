#' ALE ALGORITHM IMPLEMENTATION



# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(randomForest)
library(patchwork)

source("src/00-aux/ALE.R")


# DATA --------------------------------------------------------------------

df <- read_csv("data/tasks_data.csv")


# ALE COMPUTATION ---------------------------------------------------------


# Currently: just one task
task <- 1
df_aux <- df[df$id_task == task,]

# TODO: What happens if there is a variable in the model
#       which we don't want an ALE plot for it?
predictors_name <- c("x1", "x2", "x3", "x4", "x5")
model <- randomForest(y ~ .-id_task, data = df_aux)


ale_by_var <- ale(df, model, features = predictors_name)
ale_by_var <- ale(df, model, features = predictors_name,
                  xALE = quantiles_xALE(df, features = predictors_name ,
                                        n = 250))
# TODO: FIX: ALE doesn't start at minimum nor end at maximum.

ale_plot(ale_by_var, df_aux)
ale_plot(ale_by_var, df_aux, point = TRUE)

ale_plot(ale_by_var, df_aux, features = c("x1", "x2", "x3", "x4", "x5"))
ale_plot(ale_by_var, df_aux, features = c("x1", "x2", "x3"))
ale_plot(ale_by_var, df_aux, features = c("x1", "x5", "x3"))
