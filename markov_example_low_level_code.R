# traditional model

# model design ==========
v_states <- c("Moderate", "Severe", "Dead")
v_treatments <- c("Treatment A", "Treatment B")

n_cycles <- 40 # add 1 for cycle 0
v_cycles <- 1:n_cycles
# getting the data ========
uModerate	<- 0.8
uSevere	<- 0.6
uDead	<- 0

cTrtA	<- 1000
cTrtB	<-2500
cModerateNoTrt <- 3000
cSevere	<- 6000
cDead	<- 0 

pMortMod	<- 0.050
pMortSev	<- 0.200
pProgNoTrt <-	0.100

rrProgTrtA<-	0.8
rrProgTrtB<-	0.7

DiscRate<-	0.015

v_life_years <- c(1, 1, 0)

# derived data  ========
n_states <- length(v_states)
pProgTrtA <- pProgNoTrt * rrProgTrtA
pProgTrtB <- pProgNoTrt * rrProgTrtB
cModerateTrtA <- cModerateNoTrt + cTrtA
cModerateTrtB <- cModerateNoTrt + cTrtB


# No treatment ========
v_init_states <- c(1, 0, 0)
m_P <- matrix(0, nrow = n_states, ncol = n_states)
m_trace <- matrix(NA, nrow = n_cycles, ncol = n_states)
v_disc <- (1 / (1 + DiscRate)) ^ v_cycles
rownames(m_P) <- v_states
colnames(m_P) <- v_states
colnames(m_trace) <- v_states
rownames(m_trace) <- paste("Cycle", v_cycles)
# fill in transition prob matrix
# from moderate
m_P["Moderate", "Dead"] <- pMortMod
m_P["Moderate", "Severe"] <- pProgNoTrt * (1 - pMortMod)
m_P["Moderate", "Moderate"] <- 1 - pMortMod - pProgNoTrt + pMortMod * pProgNoTrt
# from severe
m_P["Severe", "Dead"] <- pMortSev
m_P["Severe", "Severe"] <- 1 - pMortSev
# from death
m_P["Dead", "Dead"] <- 1
# run the model
for (j in 1:n_cycles){
  prev_distribion <- if (j == 1){
    v_init_states
  } else {
    m_trace[j - 1, ]
  }
  m_trace[j, ] <- prev_distribion %*% m_P
}
# state based costs and QALYs
v_state_costs <- c(cModerateNoTrt, cSevere, cDead)
v_state_qalys <- c(uModerate, uSevere, uDead)
# undistcounted total costs, QALYs and LYs
noTrt <- list()
noTrt$costs_undiscounted  <-   sum(m_trace %*% v_state_costs)
noTrt$qalys_undiscounted  <-   sum(m_trace %*% v_state_qalys)
noTrt$lifeyrs_undiscounted <-  sum(m_trace %*% v_life_years)
#discounted total costs, QALYs and LYs
noTrt$costs_discounted   <- v_disc %*% m_trace %*% v_state_costs
noTrt$qalys_discounted   <- v_disc %*% m_trace %*% v_state_qalys
noTrt$lifeyrs_discounted <-  v_disc %*% m_trace %*% v_life_years
noTrt


# Treatment A ========
v_intial_states <- c(1, 0, 0)
m_P <- matrix(0, nrow = n_states, ncol = n_states)
m_trace <- matrix(NA, nrow = n_cycles, ncol = n_states)
v_disc <- (1 / (1 + DiscRate)) ^ v_cycles
rownames(m_P) <- colnames(m_P) <- colnames(m_trace) <- v_states
rownames(m_trace) <- v_cycles
# fill in transition prob matrix
# from moderate
m_P["Moderate", "Dead"] <- pMortMod
m_P["Moderate", "Severe"] <- pProgTrtA * (1 - pMortMod)
m_P["Moderate", "Moderate"] <- 1 - pMortMod - pProgTrtA + pMortMod * pProgTrtA
# from severe
m_P["Severe", "Dead"] <- pMortSev
m_P["Severe", "Severe"] <- 1 - pMortSev
# from death
m_P["Dead", "Dead"] <- 1
# run the model
for (j in 1:n_cycles){
  prev_distribion <- if (j == 1){
    v_init_states
  } else {
    m_trace[j - 1, ]
  }
  m_trace[j, ] <- prev_distribion %*% m_P
}
# state based costs and QALYs
v_state_costs <- c(cModerateTrtA, cSevere, cDead)
v_state_qalys <- c(uModerate, uSevere, uDead)
# undistcounted total costs, QALYs and LYs
TrtA <- list()
TrtA$costs_undiscounted  <-   sum(m_trace %*% v_state_costs)
TrtA$qalys_undiscounted  <-   sum(m_trace %*% v_state_qalys)
TrtA$lifeyrs_undiscounted <-  sum(m_trace %*% v_life_years)
#discounted total costs, QALYs and LYs
TrtA$costs_discounted   <- v_disc %*% m_trace %*% v_state_costs
TrtA$qalys_discounted   <- v_disc %*% m_trace %*% v_state_qalys
TrtA$lifeyrs_discounted <-  v_disc %*% m_trace %*% v_life_years
TrtA


# Treatment B ========
v_intial_states <- c(1, 0, 0)
m_P <- matrix(0, nrow = n_states, ncol = n_states)
m_trace <- matrix(NA, nrow = n_cycles, ncol = n_states)
v_disc <- (1 / (1 + DiscRate)) ^ v_cycles
rownames(m_P) <- colnames(m_P) <- colnames(m_trace) <- v_states
rownames(m_trace) <- v_cycles
# fill in transition prob matrix
# from moderate
m_P["Moderate", "Dead"] <- pMortMod
m_P["Moderate", "Severe"] <- pProgTrtB * (1 - pMortMod)
m_P["Moderate", "Moderate"] <- 1 - pMortMod - pProgTrtB + pMortMod * pProgTrtB
# from severe
m_P["Severe", "Dead"] <- pMortSev
m_P["Severe", "Severe"] <- 1 - pMortSev
# from death
m_P["Dead", "Dead"] <- 1
# run the model
for (j in 1:n_cycles){
  prev_distribion <- if (j == 1){
    v_init_states
  } else {
    m_trace[j - 1, ]
  }
  m_trace[j, ] <- prev_distribion %*% m_P
}
# state based costs and QALYs
v_state_costs <- c(cModerateTrtB, cSevere, cDead)
v_state_qalys <- c(uModerate, uSevere, uDead)
# undistcounted total costs, QALYs and LYs
TrtB <- list()
TrtB$costs_undiscounted  <-   sum(m_trace %*% v_state_costs)
TrtB$qalys_undiscounted  <-   sum(m_trace %*% v_state_qalys)
TrtB$lifeyrs_undiscounted <-  sum(m_trace %*% v_life_years)
#discounted total costs, QALYs and LYs
TrtB$costs_discounted   <- v_disc %*% m_trace %*% v_state_costs
TrtB$qalys_discounted   <- v_disc %*% m_trace %*% v_state_qalys
TrtB$lifeyrs_discounted <-  v_disc %*% m_trace %*% v_life_years
TrtB


# RESULTS ============
ICER_TrtA_vs_noTRT <- (TrtA$costs_discounted - noTrt$costs_discounted) / 
  (TrtA$qalys_discounted - noTrt$qalys_discounted)
ICER_TrtA_vs_noTRT
ICER_TrtB_vs_TrtA <- (TrtB$costs_discounted - TrtA$costs_discounted) / 
  (TrtB$qalys_discounted - TrtA$qalys_discounted)
ICER_TrtB_vs_TrtA

# all done! 