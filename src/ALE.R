
library(tidyverse)
library(randomForest)

df <- read_csv("data/tasks_data.csv")

task <- 1
x <- "x1"

xALE <- seq(-8 , 8, length.out = 60)

ale <- function(df, model, xALE, features){
  for (x in features){
    df_aux <- df
    
    n_xALE <- numeric(length(xALE))
    for (i in 1:(length(xALE)-1)){
      n_xALE[i] <- sum(df[[x]] > xALE[i] & df[[x]] <= xALE[i+1])
    }
    
    df_aux$z0 <- NA
    df_aux$z1 <- NA
    df_aux$id <- NA
    
    for(i in 1:nrow(df_aux)){
      id_zk <- which(df_aux[[x]][i] <= xALE)[1]
      df_aux$id[i] <- id_zk
      df_aux$z0[i] <- xALE[id_zk-1]
      df_aux$z1[i] <- xALE[id_zk]
    }
    
    df_aux$y_z0 <- NA
    df_aux$y_z1 <- NA
    
    df_z0 <- df_aux
    df_z0[x] <- NULL 
    names(df_z0)[names(df_z0) == "z0"] <- x
    
    df_aux$y_z0 <- predict(model, newdata = df_z0)
    
    df_z1 <- df_aux
    df_z1[x] <- NULL 
    names(df_z1)[names(df_z0) == "z1"] <- x
    
    df_aux$y_z1 <- predict(model, newdata = df_z1)
    
    df_aux %>% 
      mutate(ale = y_z1 - y_z0) %>% 
      group_by(z1) %>% 
      summarise(
        n = n(),
        sum_ale = sum(ale),
        ale = sum_ale/n) %>% 
      arrange(z1) %>% 
      mutate(ale_cum = cumsum(ale),
             ale_cum_cent = sum(n*ale_cum)/sum(n),
             ale_final = ale_cum - ale_cum_cent)
    
  }
}
df <- df %>% 
  filter(id_task == task) %>% 
  select(-id_task)

model <- randomForest(y ~ ., data = df)

n_partition <- 30
n_k <- (0:n_partition)/n_partition

partition <- quantile(df[[feature]], probs = n_k)
partition[1] <- partition[1]-0.001
partition[length(partition)] <- partition[length(partition)]+0.001

df$z0 <- NA
df$z1 <- NA
df$id <- NA

for(i in 1:nrow(df)){
  id_zk <- which(df[[feature]][i] <= partition)[1]
  df$id[i] <- id_zk
  df$z0[i] <- partition[id_zk-1]
  df$z1[i] <- partition[id_zk]
}

df$y_z0 <- NA
df$y_z1 <- NA

df_z0 <- df %>% 
  select(-x1) %>% 
  rename("x1" = "z0")


df$y_z0 <- predict(mod, newdata = df_z0)

df_z1 <- df %>% 
  select(-x1) %>% 
  rename("x1" = "z1")


df$y_z1 <- predict(mod, newdata = df_z1)

df_aux %>% 
  mutate(ale = y_z1 - y_z0) %>% 
  group_by(z1) %>% 
  summarise(
    n = n(),
    sum_ale = sum(ale),
    ale = sum_ale/n) %>% 
  arrange(z1) %>% 
  mutate(ale_cum = cumsum(ale),
         ale_cum_cent = sum(n*ale_cum)/sum(n),
         ale_final = ale_cum - ale_cum_cent) %>% 
  ggplot(aes(x = z1, y = ale_final)) + 
  geom_line()






