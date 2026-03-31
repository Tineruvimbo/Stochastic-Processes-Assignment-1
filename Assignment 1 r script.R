#A1.	Define the matrix:
P <- matrix(c(
  1,   0,   0,   0,   0,
  0.5, 0,   0,   0,   0.5,
  0.2, 0,   0,   0,   0.8,
  0,   0,   1,   0,   0,
  0,   0,   0,   1,   0
), nrow = 5, byrow = TRUE)

P
#(a)	Use R to plot a diagram of the Markov chain. Identify all transient and recurrent classes. Identify all absorbing and reflective states. Find the period of each state.
#install.packages("diagram")   # run once
library(diagram)

plotmat(P, name = 1:5,
        box.size = 0.1, cex.txt = 0.8,
        lwd = 1, self.cex = 0.6)
pos <- matrix(c(
  0,0,
  1,1,
  1,-1,
  2,1,
  2,-1
), byrow = TRUE, ncol = 2)

plotmat(P, pos = pos, name = 1:5,
        box.size = 0.1)
simulate_chain <- function(P, start, n_steps) {
  states <- numeric(n_steps)
  states[1] <- start
  
  for (i in 2:n_steps) {
    states[i] <- sample(1:nrow(P), 1, prob = P[states[i-1], ])
  }
  return(states)
}
#b)	Simulate three trajectories of the chain that start at a randomly chosen state. Comment on what you see.

set.seed(123)

traj1 <- simulate_chain(P, sample(1:5,1), 20)
traj2 <- simulate_chain(P, sample(1:5,1), 20)
traj3 <- simulate_chain(P, sample(1:5,1), 20)

traj1
traj2
traj3
library(expm)
#Comment:
#The simulated trajectories show that regardless of the initial state, the chain eventually moves into state 1 and remains there permanently. This indicates that state 1 is an absorbing state. The other states (2, 3, 4, and 5) are transient, as the process does not stay in them indefinitely. Some trajectories exhibit temporary cycling between states (e.g., 3, 4, and 5), but these cycles eventually lead to absorption in state 1.

#c)	Find the steady-state probabilities and interpret them. Is it an ergodic chain?
#install.packages("expm")

steady <- P %^% 100
steady[1, ]   # any row works


#d)	Plot the unconditional probabilities at time n against time and comment on how fast the probabilities converge to the steady-state distribution.
n <- 20
dist <- matrix(0, n, 5)
dist[1, ] <- c(0.2,0.2,0.2,0.2,0.2)   # initial state

for (i in 2:n) {
  dist[i, ] <- dist[i-1, ] %*% P
}

matplot(1:n, dist, type = "l", lty = 1,
        xlab = "Time", ylab = "Probability")
legend("right", legend = paste("State",1:5), col=1:5, lty=1)

#Comment:
#The plot shows that the probability of State 1 increases steadily and converges to 1, while the probabilities of the other states decrease towards 0. The convergence occurs relatively quickly within a few steps, indicating that the Markov chain rapidly approaches its steady-state distribution due to the presence of an absorbing state.



P2 <- matrix(c(
  0,1,0,0,0,0,0,
  1,0,0,0,0,0,0,
  0,0,0,0.4,0.2,0.2,0.2,
  0,0,0,0,0.2,0.4,0.4,
  0.3,0,0,0.1,0.3,0.1,0.2,
  0,0,0,0.2,0.2,0.3,0.3,
  0,0,0,0.5,0.2,0.2,0.1
), nrow = 7, byrow = TRUE)
P2

plotmat(P2, name = 1:7, box.size = 0.08)

set.seed(123)

trajA <- simulate_chain(P2, sample(1:7,1), 30)
trajB <- simulate_chain(P2, sample(1:7,1), 30)

trajA
trajB

steady2 <- P2 %^% 100
steady2[1, ]



P1 <- matrix(c(
  0.4,0.4,0.2,
  0.3,0.4,0.2,
  0,0.1,0.9
), nrow=3, byrow=TRUE)
P1
P2 <- matrix(c(
  0.1,0.5,0.4,
  0.1,0.3,0.6,
  0,0.1,0.9
), nrow=3, byrow=TRUE)
P2


initial <- c(1,0,0)   # light state

dist_4pm <- initial %*% (P1 %^% 9)
dist_6pm <- dist_4pm %*% (P2 %^% 6)

dist_6pm


simulate_time_dependent <- function(P1, P2, steps1, steps2, start) {
  state <- start
  
  # 1PM to 4PM
  for (i in 1:steps1) {
    state <- sample(1:3, 1, prob = P1[state, ])
  }
  
  # 4PM to 6PM
  for (i in 1:steps2) {
    state <- sample(1:3, 1, prob = P2[state, ])
  }
  
  return(state)
}

set.seed(123)

results <- replicate(10000,
                     simulate_time_dependent(P1, P2, 9, 6, 1))

table(results) / 10000




#2c)	 Simulate two trajectories of the chain that start at a randomly selected state. Discuss what you see in the plot.
#trajA
[1] 7 5 5 6 5 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1
> trajB
[1] 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2


> 
  
  Comment:
  The simulations show that regardless of the starting state, the Markov chain eventually enters the class {1, 2} and remains there. States outside this class (3–7) are only visited temporarily before the chain transitions into the recurrent class. Once in states 1 and 2, the process alternates between them, indicating a periodic behavior with period 2.

2d)	  Calculate the limiting probabilities and interpret them. Is the chain ergodic?
  
  [1] 1 0 0 0 0 0 0
Comment:
  The steady-state distribution is (1, 0, 0, 0, 0), meaning that in the long run the process will be in State 1 with probability 1 regardless of the initial state. This occurs because State 1 is an absorbing state, and all other states eventually transition into it.












A3
#a)	If the traffic starts with the light state at 1PM, what is the distribution of the states at 6PM? 
 # P1
#[,1] [,2] [,3]
##[1,]  0.4  0.4  0.2
#[2,]  0.3  0.5  0.2
#[3,]  0.0  0.1  0.9

##[,1] [,2] [,3]
[1,]  0.1  0.5  0.4
[2,]  0.1  0.3  0.6
[3,]  0.0  0.1  0.9


From 1PM → 4PM = 3 hours = 9 steps
From 4PM → 6PM = 2 hours = 6 steps

[,1]      [,2]      [,3]
[1,] 0.01182491 0.1058621 0.6804706


b)	Simulate 10,000 trajectories to verify the result of the previous part.
results
1      2      3 
0.0147 0.1291 0.8562 


