d <- read.csv("inst/GCB-21-1726 Data.csv") |>
  dplyr::select(Year, PDOann, NPGOann) |>
  dplyr::filter(Year <= 2017)

library(hmmTMB)

# 1. Create MarkovChain (hidden state process)
hid <- MarkovChain$new(data = d, n_states = 2)

# 2. Create Observation model
# Define distributions for PDO and NPGO (both normal)
dists <- list(PDOann = "norm", NPGOann = "norm")

# Initial parameters (state-specific means and SDs)
par0 <- list(
  PDOann = list(mean = c(-0.5, 0.5), sd = c(0.5, 0.5)),
  NPGOann = list(mean = c(-0.5, 0.5), sd = c(0.5, 0.5))
)

obs <- Observation$new(data = d, dists = dists, par = par0)

# 3. Create HMM
hmm <- HMM$new(obs = obs, hid = hid)

# 4. Fit the model
hmm$fit(silent = FALSE)

# 5. Extract Transition Probability Matrix (TPM)
tpm <- hid$tpm()[,,1]  # [,,1] gets the TPM for first time point
print("Transition Probability Matrix:")
print(tpm)

# 6. Extract estimated states (Viterbi algorithm)
states <- hmm$viterbi()
d$state <- states

# Optional: Get state probabilities for each time point
state_probs <- hmm$state_probs()
d$prob_state1 <- state_probs[, 1]
d$prob_state2 <- state_probs[, 2]

library(ggplot2)
d_pdo <- d[,c("Year","PDOann","state")]
d_pdo <- dplyr::rename(d_pdo, value = PDOann)
d_pdo$response <- "PDO"

d_npgo <- d[,c("Year","NPGOann","state")]
d_npgo <- dplyr::rename(d_npgo, value = NPGOann)
d_npgo$response <- "NPGO"

d_comb <- rbind(d_pdo, d_npgo)

ggplot(d_comb, aes(Year, value)) +
  geom_rect(aes(xmin = Year - 0.5, xmax = Year + 0.5,
                ymin = -Inf, ymax = Inf, fill = factor(state)),
            alpha = 0.3) +
  geom_line(color = "grey20", size = 0.8) +
  facet_wrap(~response, ncol = 1, scales = "free_y") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "white", color = "grey50")
  ) +
  scale_fill_viridis_d(option = "magma", begin = 0.3, end = 0.8) +
  labs(fill = "State", y = "Value", x = "Year")
ggsave("inst/hmm_fig.png", width=7, height = 8)
