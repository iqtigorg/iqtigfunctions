load("devel/Dummy_17_1_B.RData")

set.seed(1234)
N <- nrow(D17BDummy)
D17BDummy <- dplyr::mutate(
  D17BDummy,
  alter = runif(N, 18, 90),
  ENTLDIAG_1 = sample(c("T81.62", "A12.34"), N, replace = TRUE),
  IKNRKH = sample(c("A", "B"), N, replace = TRUE),
  praeopminutenMin1 = round(rnorm(N, 2000, 1000)),
  # 2017
  ANTIBIOPROPH = NULL,
  ANTITHROMBMITTEL = NULL,
  POROENTGENAP = NULL,
  POROENTGENAXIA = NULL,
  ERFASSGSTURZRISIKO = sample(c(0, 1), N, replace = TRUE),
  GASTROBLUTUNG = sample(c(NA, 1), N, replace = TRUE),
  HARNWEGSINF = sample(c(NA, 1), N, replace = TRUE),
  NEKROSEWUND = round(runif(N, 1, 5)),
  NIERENINSUFFIZIENZJL = sample(c(NA, 1), N, replace = TRUE),
  PFLEGEGRAD = sample(c(0:5, 9), N, replace = TRUE),
  PRAEVMASSNAHMEN = sample(c(0, 1), N, replace = TRUE),
  TIA = sample(c(NA, 1), N, replace = TRUE),
  WUNDDEHISZE = round(runif(N, 1, 5)),
  # 2018
  ARTMEDDOAKNOAK = sample(c(NA, 1), N, replace = TRUE),
  ARTMEDSONST = sample(c(NA, 1), N, replace = TRUE),
  ARTMEDTHROMBAGGHEMM = sample(c(NA, 1), N, replace = TRUE),
  ARTMEDVITKANT = sample(c(NA, 1), N, replace = TRUE),
  STANDORTOPS = STANDORT
)
D17BDummy[["TDS_B"]] <- seq_len(N)

saveRDS(D17BDummy, "inst/extdata/dummy_17_1_B.rds", version = 2)
