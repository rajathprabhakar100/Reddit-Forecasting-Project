library(deSolve)
library(tidyverse)
# Define the SEIR model function

#Susceptible–Exposed–Infectious–Removed Model with Short-Term Awareness of Risk 
seir_model <- function(t, y, pars) {
  S <- y[1]
  E <- y[2]
  I <- y[3]
  R <- y[4]
  D <- y[5]
  NewInfections <- y[6]
  NewDeaths <- y[7]
  
  Dday <- pars["gamma"] * I * pars["frac_D"]
  
  dS <- -pars["beta"] * S * I / (1 + (Dday / pars["Dcrit"])^pars["awareness"])
  dE <- pars["beta"] * S * I / (1 + (Dday / pars["Dcrit"])^pars["awareness"]) - pars["mu"] * E
  dI <- pars["mu"] * E - pars["gamma"] * I
  dR <- pars["gamma"] * I * (1 - pars["frac_D"])
  dD <- pars["gamma"] * I * pars["frac_D"]
  dNewInfections <- pars["beta"] * S * I / (1 + (Dday / pars["Dcrit"])^pars["awareness"])
  dNewDeaths <- pars["gamma"] * I * pars["frac_D"]
  
  list(c(dS, dE, dI, dR, dD, dNewInfections, dNewDeaths))
}
# Define parameters
params <- c(
  beta = 0.5,      # Transmission rate
  mu = 1/2,        # Incubation rate (1/average incubation period)
  gamma = 1/6,     # Recovery rate (1/average infectious period)
  frac_D = 0.01,   # Fraction of fatal cases
  Dcrit = 50/10000000,      # Critical awareness threshold
  awareness = 4    # Awareness parameter, k
)

# Initial conditions
init_state <- c(S = 0.99, E = 0.01, I = 0, R = 0, D = 0, NewInfections = 0, NewDeaths = 0)

# Time sequence
times <- seq(0, 200, by = 1)

# Solve the system
out <- ode(y = init_state, times = times, func = seir_model, parms = params)

# Convert output to a dataframe
out <- as.data.frame(out)

# Plot results
library(ggplot2)
out_long <- reshape2::melt(out, id = "time")

out_long %>%  
  filter(variable == 'NewInfections') %>%  
  mutate(new_value = value - lag(value)) %>%  
  ggplot(aes(x = time, y = new_value, color = variable)) +
  geom_line() +
  labs(title = "SEIR Model with Awareness",
       subtitle = "k = 4",
       x = "Time",
       y = "Proportion of Population") +
  theme_minimal()
###########################################################################################
##Short-Term Awareness, Long-Term Plateaus, and Oscillations
seir_model1 <- function(t, y, pars) {
  S <- y[1]
  E <- y[2]
  I <- y[3]
  R <- y[4]
  D <- y[5]
  H <- y[6]
  NewInfections <- y[7]
  NewDeaths <- y[8]
  
  Dday <- pars["gamma"] * I * pars["frac_D"]
  dS <- -pars["beta"] * S * I / (1 + (Dday / pars["Dcrit"])^pars["awareness"]) #awareness = k
  dE <- -((pars["beta"] * S * I) / (1 + (Dday / pars["Dcrit"])^pars["awareness"])) - (E * pars["mu"])
  dI <- (pars["mu"] * E) - (pars["gamma"] * I)
  dR <- I * pars["gamma"] * (1 - pars["frac_D"])
  dH <- (pars["frac_D"] * pars["gamma"] * I) - (H * (1/pars["hosp_death"])) #hosp_death = T_H
  dD <- H * (1/pars["hosp_death"])
  dNewInfections <- pars["beta"] * S * I / (1 + (Dday / pars["Dcrit"])^pars["awareness"])
  dNewDeaths <- pars["gamma"] * I * pars["frac_D"]
                        
  list(c(dS, dE, dI, dR, dH, dD, dNewInfections, dNewDeaths))
}

# Define parameters
params <- c(
  beta = 0.5,      # Transmission rate
  mu = 1/2,        # Incubation rate (1/average incubation period)
  gamma = 1/6,     # Recovery rate (1/average infectious period)
  frac_D = 0.01,   # Fraction of fatal cases
  Dcrit = 50/10000000,      # Critical awareness threshold
  awareness = 2,    # Awareness parameter, k
  hosp_death = 28 #T_H
)

# Initial conditions
init_state <- c(S = 0.99, E = 0.01,
                I = 0, R = 0, D = 0, H = 0, NewInfections = 0, NewDeaths = 0)

# Time sequence
times <- seq(0, 200, by = 1)

# Solve the system
out <- ode(y = init_state, times = times, func = seir_model1, parms = params)

# Convert output to a dataframe
out <- as.data.frame(out)

# Plot results
library(ggplot2)
out_long <- reshape2::melt(out, id = "time")

out_long %>%  
  filter(variable == "NewInfections") %>%  
  mutate(new_value = value - lag(value),
         value = value * 1000000) %>% 
#  filter(time >= 50) %>% 
  ggplot(aes(x = time, y = value, color = variable)) +
  geom_line() +
  labs(title = "SEIR Model with Awareness",
       subtitle = "T_H = 28, N = 10,000,000, k=2",
       x = "Time (days)",
       y = "Infections/day") +
  theme_minimal() 
out2 <- out_long %>%  
  filter(variable == "NewInfections") %>%  
  mutate(new_value = (value - lag(value)) * 10000000,
         value = value * 10000000)


###Dynamical Consequences of Short-Term and Long-Term Awareness###

seir_model2 <- function(t, y, pars) {
  S <- y[1]
  E <- y[2]
  I <- y[3]
  R <- y[4]
  D <- y[5]
  H <- y[6]
  NewInfections <- y[7]
  NewDeaths <- y[8]
  
  Dday <- pars["gamma"] * I * pars["frac_D"]
  dS <- -(pars["beta"] * S * I)/(1 + (Dday/pars["Dcrit"])^pars["awareness"] + (D/pars["Dcrit_long"])^pars["awareness"]) #awareness = k
  dE <- -(pars["beta"] * S * I)/(1 + (Dday/pars["Dcrit"])^pars["awareness"] + (D/pars["Dcrit_long"])^pars["awareness"]) - (pars["mu"] * E)
  dI <- (pars["mu"] * E) - (pars["gamma"] * I)
  dR <- I * pars["gamma"] * (1 - pars["frac_D"])
  dH <- (pars["frac_D"] * pars["gamma"] * I) - (H * (1/pars["hosp_death"])) #hosp_death = T_H
  dD <- H * (1/pars["hosp_death"])
  dNewInfections <- (pars["beta"] * S * I)/(1 + (Dday/pars["Dcrit"])^pars["awareness"] + (D/pars["Dcrit_long"])^pars["awareness"])
  dNewDeaths <- pars["gamma"] * I * pars["frac_D"]
  
  list(c(dS, dE, dI, dR, dH, dD, dNewInfections, dNewDeaths))
}

# Define parameters
params <- c(
  beta = 0.5,      # Transmission rate
  mu = 1/2,        # Incubation rate (1/average incubation period)
  gamma = 1/6,     # Recovery rate (1/average infectious period)
  frac_D = 0.01,   # Fraction of fatal cases
  Dcrit = 50/10000000,      # Critical awareness threshold
  Dcrit_long = 10000/10000000,
  awareness = 2,    # Awareness parameter, k
  hosp_death = 14
)

# Initial conditions
init_state <- c(S = 0.99, E = 0.01, I = 0, R = 0, D = 0, H = 0, NewInfections = 0, NewDeaths = 0)

# Time sequence
times <- seq(0, 200, by = 1)

# Solve the system
out <- ode(y = init_state, times = times, func = seir_model2, parms = params)

# Convert output to a dataframe
out <- as.data.frame(out)

# Plot results
library(ggplot2)
out_long <- reshape2::melt(out, id = "time")

out_long %>%  
  filter(variable == 'NewDeaths') %>%  
  mutate(new_value = value - lag(value)) %>%  
  ggplot(aes(x = time, y = new_value, color = variable)) +
  geom_line() +
  labs(title = "SEIR Model with Awareness",
       subtitle = "k = 2, T_H = 14, Dcrit_long = 10,000",
       x = "Time",
       y = "Proportion of Population") +
  theme_minimal()

###Empirical Assessment of Mechanistic Drivers of Asymmetric Peaks in COVID-19 Death Rates
seir_model3 <- function(t, y, pars) {
  S <- y[1]
  E <- y[2]
  I <- y[3]
  R <- y[4]
  D <- y[5]
  H <- y[6]
  NewInfections <- y[7]
  NewDeaths <- y[8]
  
  Dday <- pars["gamma"] * I * pars["frac_D"]
  force_infection <- 1/(1+(D/pars["Dcrit_long"])^pars["awareness"]) #force_infection = G(d)
  
  dS <- -force_infection * pars["beta"] * S * I 
  dE <- (force_infection * pars["beta"] * S * I) - (E * pars["mu"])
  dI <- (pars["mu"] * E) - (pars["gamma"] * I)
  dR <- I * pars["gamma"] * (1 - pars["frac_D"])
  dH <- (pars["frac_D"] * pars["gamma"] * I) - (H * (1/pars["hosp_death"])) #hosp_death = T_H
  dD <- H * (1/pars["hosp_death"])
  dBeta <- (pars["e"]/2) * ((((pars["beta_hat"]/(1+(Dday/pars["Dcrit"])^pars["awareness"]))-pars["beta"])/(1 + (D/pars["Dcrit_long"])^pars["awareness"])) + (pars["beta_hat"] - pars["beta"]))
  
  dNewInfections <- force_infection * pars["beta"] * S * I 
  dNewDeaths <- pars["gamma"] * I * pars["frac_D"]
  
  list(c(dS, dE, dI, dR, dH, dD, dBeta, dNewInfections, dNewDeaths))
}

# Define parameters
params <- c(
  beta = 0.5,      # Transmission rate
  beta_hat = 
  mu = 1/2,        # Incubation rate (1/average incubation period)
  gamma = 1/6,     # Recovery rate (1/average infectious period)
  frac_D = 0.01,   # Fraction of fatal cases
  Dcrit = 50/10000000,      # Critical awareness threshold
  Dcrit_long = 10000/10000000,
  awareness = 2,    # Awareness parameter, k
  hosp_death = 14
)

# Initial conditions
init_state <- c(S = 0.99, E = 0.01, I = 0, R = 0, D = 0, H = 0, NewInfections = 0, NewDeaths = 0)

# Time sequence
times <- seq(0, 200, by = 1)

# Solve the system
out <- ode(y = init_state, times = times, func = seir_model2, parms = params)

# Convert output to a dataframe
out <- as.data.frame(out)

# Plot results
library(ggplot2)
out_long <- reshape2::melt(out, id = "time")

out_long %>%  
  filter(variable == 'D') %>%  
  mutate(new_value = value - lag(value)) %>%  
  ggplot(aes(x = time, y = new_value, color = variable)) +
  geom_line() +
  labs(title = "SEIR Model with Awareness",
       subtitle = "k = 2, T_H = 14, Dcrit_long = 10,000",
       x = "Time",
       y = "Proportion of Population") +
  theme_minimal()




