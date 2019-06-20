###############################################################################################
######################################## Packages #############################################
###############################################################################################

install.packages(c("plyr", "dplyr", "tidyr", "rlang", "stats"))

library(plyr)
library(dplyr)
library(tidyr)
library(rlang)
library(stats)

###############################################################################################
######################################## Parameters #############################################
###############################################################################################

n_s <- c(250, 500, 1000, 2500)
# Proporção de registros em cada cluster
prob_k <- c(0.5, 0.3, 0.2)

#lista de pesos
weights_files = list.files(path = "weights/", pattern = "*.csv")

for (k in 1:length(weights_files)){
  print(weights_files[k])
  weight_file_name = unlist(strsplit(weights_files[k], "\\."))[1]
  print(weight_file_name)

  # Ler e editar o formato do dataframe de pesos pra formato long
  prob_generated <- read.csv(paste0("weights/", weights_files[k]), stringsAsFactors = F) %>% 
    gather(cluster, prob, -category, -attribute) %>% 
    mutate(cluster = substr(cluster, 3,3)) 

  ###############################################################################################
  ######################################## Aux functions #########################################
  ###############################################################################################
  
  # Input
  # seed - semente para geração da multinomial
  # prob_generated - dataframe com as probabilidades das categorias das variáveis de cada grupo
  # n - Tamanho final da base de dados
  # prob_k - Percentual de registros em cada cluster
  # Output 
  # df_freq - dataframe com as frequẽncias de registros das categorias das variáveis de cada grupo
  freq_generator <- function(seed, prob_generated, n, prob_k){
    n_cluster <- round(n*prob_k)
    names(n_cluster) <- c("0", "1", "2")

    prob_format_edit <- prob_generated %>% 
      group_by(cluster, attribute) %>% 
      summarise(probabilities = paste0("c(", paste0(prob, collapse = ","), ")"))

    # Gerar distribuição multinomial para cada linha do dataframe data_generator
    list_df <- apply(prob_format_edit, 1, function(row){
      current_prob <- eval(parse(text = row["probabilities"]))
      n_i <- n_cluster[which(row["cluster"] == names(n_cluster))]
      set.seed(seed)
      data.frame(attribute = row["attribute"],
            cluster = row["cluster"],
            category = 0:(length(current_prob)-1),
            freq = rmultinom(1, n_i, prob = current_prob), stringsAsFactors = F)
    })

    # Juntar dados gerados com as probabilidades iniciais
    df_freq <- rbind.fill(list_df) %>% 
      mutate(attribute = as.integer(attribute)) %>% 
      inner_join(prob_generated, by = c("category", "cluster", "attribute")) %>% 
      select(-prob) 
    
    return(df_freq)
  }

  # Inpute
  # df_freq - Base de dados com as frequências das categorias
  # Output
  # df - Base de dados em formato wide pronta para análise
  duplicate_rows <- function(df_freq){
    df <- as.data.frame(lapply(df_freq, rep, df_freq$freq)) %>% 
      select(-freq) %>% 
      mutate(attribute = paste0("X", attribute))
    
    df_split <- split(df, df$attribute) 
    
    list_df_out <- lapply(df_split, function(df){
      df$id <- row.names(df) <- 1:nrow(df)
      names(df)[names(df) == "category"] <- df$attribute[1]
      df %>% select(-attribute)
    }) 
    
    df <- Reduce(function(...) merge(..., by = c('id', "cluster")), list_df_out)
    
    return(df)
  }

  # Input
  # seed - semente para geração da multinomial
  # prob_generated - dataframe com as probabilidades das categorias das variáveis de cada grupo
  # n - Tamanho final da base de dados
  # prob_k - Percentual de registros em cada cluster
  # Output 
  # df - Base de dados em formato wide pronta para análise
  data_generator <- function(seed, prob_generated, n, prob_k){
    df_freq <- freq_generator(seed, prob_generated, n, prob_k)
    df <- duplicate_rows(df_freq) %>% arrange(id)
  }
  
  #################################################################################################
  ########################################## Monte Carlo Simulations #############################
  ################################################################################################
  
  n_montecarlo_replic <- 1
  n_exp <- 1
  
  for(n in n_s){
    initial_seed <- 09091997
    
    for(id_replic in 0:(n_montecarlo_replic-1)){
      df <- data_generator(initial_seed, prob_generated, n, prob_k)
      for(exp in 0:(n_exp-1)){
        print(paste0("data/n_", n, "/df_", id_replic, "_", exp, "_", weight_file_name, ".csv"))
        write.csv(df, paste0("data/n_", n, "/df_", id_replic, "_", exp, "_", weight_file_name, ".csv"), append = "w")
      }
      
      initial_seed <- initial_seed + 1
    }
  }
}