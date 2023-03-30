library(deSolve)
library(tidyverse)

theme_set(theme_minimal())

sirmod <- function(t, y, parms) {
  # Pull state variables from y vector
  S <- y[1]
  I <- y[2]
  R <- y[3]
  
  # Pull parameter values from parms vector
  beta <- parms["beta"]
  mu <- parms["mu"]
  nu <- parms["nu"] #death
  gamma <- parms["gamma"]
  N <- parms["N"]
  
  # Define equations
  dS <- mu*(S+I+R)  - beta * S * I / N - nu*(S/(S+I+R))
  dI <- beta * S * I / N - (gamma) * I - nu*(I/(S+I+R))
  dR <- gamma * I - nu*(R/(S+I+R))
  res <- c(dS, dI, dR)
  
  # Return list of gradients
  list(res)
}


times <- seq(0, 1000, by = 1)
parms <- c(mu = 0.01, nu = 0.01, N = 1, beta = 1.001, gamma = 1/12)
start <- c(S = 0.999, I = 0.001, R = 0)

out <- ode(y = start, times = times, func = sirmod, parms = parms)
out_df <- as_tibble(out) %>%
  pivot_longer(cols = -time, names_to = "state", values_to = "number") %>%
  mutate(
    time = as.numeric(time),
    number = as.numeric(number),
    state = factor(state, levels = c("S", "I", "R")),
    number = round(number, 6)
  )

ggplot(out_df, aes(x = time, y = number, color = state)) +
  geom_line(linewidth = 2) +
  labs(x = "Time", y = "Number", color = "State")