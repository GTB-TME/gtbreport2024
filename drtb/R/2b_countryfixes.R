## for any countries requiring individual quadratic fixes
## libraries
library(here)
library(glue)
library(data.table)
library(ggplot2)
library(MASS)
library(ggpubr)
gh <- function(x) glue(here(x))
rot45 <- theme(axis.text.x = element_text(angle = 45, hjust = 1))


getLGNmu <- function(m) {
  log(m / (1 - m))
}
getLGNsig <- function(m, l, h) {
  M <- log(m / (1 - m))
  L <- log(l / (1 - l))
  H <- log(h / (1 - h))
  (H - L) / 3.92
}


## ============= DATA ===========================

load(here("drtb/data/RPD.Rdata"))
AUT <- RPD[grepl("AUT", id)] # austria
EGY <- RPD[grepl("EGY", id)] # egypt
GEO <- RPD[grepl("GEO", id)] # georgia
MMR <- RPD[grepl("MMR", id)] # myanmar


## graph
ggplot(AUT, aes(x = year, y = RR.mid, ymin = RR.lo, ymax = RR.hi, shape = patients, col = patients)) +
  geom_pointrange() +
  scale_y_sqrt() +
  facet_wrap(~patients)

ggplot(EGY, aes(x = year, y = RR.mid, ymin = RR.lo, ymax = RR.hi, shape = patients, col = patients)) +
  geom_pointrange() +
  scale_y_sqrt() +
  facet_wrap(~patients)

ggplot(GEO, aes(x = year, y = RR.mid, ymin = RR.lo, ymax = RR.hi, shape = patients, col = patients)) +
  geom_pointrange() +
  scale_y_sqrt() +
  facet_wrap(~patients)

ggplot(MMR, aes(x = year, y = RR.mid, ymin = RR.lo, ymax = RR.hi, shape = patients, col = patients)) +
  geom_pointrange() +
  scale_y_sqrt() +
  facet_wrap(~patients)

## add in extras

## AUT
AUT[, mu := getLGNmu(RR.mid / 1e2)]
AUT[, S := getLGNsig(RR.mid / 1e2, RR.lo / 1e2, RR.hi / 1e2)]
AUT[, DT := year - 2000]
AUT[!is.finite(mu), mu := -6.0]
AUT[!is.finite(S), S := 1.0]

## EGY
EGY[, mu := getLGNmu(RR.mid / 1e2)]
EGY[, S := getLGNsig(RR.mid / 1e2, RR.lo / 1e2, RR.hi / 1e2)]
EGY[, DT := year - 2000]

## GEO
GEO[, mu := getLGNmu(RR.mid / 1e2)]
GEO[, S := getLGNsig(RR.mid / 1e2, RR.lo / 1e2, RR.hi / 1e2)]
GEO[, DT := year - 2000]

## MMR
MMR[, mu := getLGNmu(RR.mid / 1e2)]
MMR[, S := getLGNsig(RR.mid / 1e2, RR.lo / 1e2, RR.hi / 1e2)]
MMR[, DT := year - 2000]



## ============= PREVALENCE FIXES ===========================

## use Bayesian LR in log space with known errors
## y = x^Tb+e
## var(e_i) = s_i^2
## S = diag(s_i^2)
## posterior (vague conjugate prior) b ~ MVN(a,V)
## V = (X^TS^{-1}X + L)^{-1}  //L = prior precision for betas
## a = V X^{T}S^{-1}y

## fit the regression
BLR <- function(Y, X, iSIG, L) {
  invV <- t(X) %*% iSIG %*% X + L
  V <- solve(invV)
  a <- V %*% t(X) %*% iSIG %*% Y
  list(a = a, V = V)
}

## simulate hi-lo-mid
simHLM <- function(N, a, V, XF, lgt = TRUE) {
  B <- mvrnorm(N, a, V)
  RR <- exp(B %*% t(XF))
  if (lgt == TRUE) RR <- RR / (1 + RR)
  RR <- 1e2 * RR # N x NY
  A <- as.data.table(t(RR))
  A[, DT := XF[, 2]] # cols are replicates
  A <- melt(A, id = "DT")
  A <- A[, .(
    RR.mid = mean(value),
    RR.lo = quantile(value, 0.025),
    RR.hi = quantile(value, 0.975)
  ),
  by = DT
  ]
  A
}


## === apply:
uselist <- list(AUT = c("new"), EGY = c("new"), GEO = c("new", "ret"), MMR = c("new"))
cndat <- list(AUT = AUT, EGY = EGY, GEO = GEO, MMR = MMR)
syear <- 2000
NY <- max(RPD$year) - syear + 1 # number of years
XF <- cbind(rep(1, NY), 1:NY - 1, (1:NY - 1)^2) # prediction X
PZ <- list()
for (cn in names(cndat)) {
  print(cn)
  plts <- list()
  for (PG in c("new", "ret")) {
    dat <- cndat[[cn]][year >= syear & patients == PG]
    fit <- BLR(
      Y = dat$mu,
      X = cbind(rep(1, nrow(dat)), dat$DT, dat$DT^2),
      iSIG = diag(1 / dat$S^2),
      L = diag(c(1, 1, 100)) # prior
    )
    P <- simHLM(
      N = 1e4,
      a = fit$a,
      V = fit$V,
      XF = XF
    )
    P[, year := DT + syear]
    P[, patients := PG]
    P[, iso3 := cn]
    PZ <- c(PZ, list(P))
    ttl <- paste(cn, PG, sep = ": ")
    if (PG %in% uselist[[cn]]) ttl <- paste0(ttl, " *")
    plts[[PG]] <- ggplot(P, aes(year, RR.mid, ymin = RR.lo, ymax = RR.hi)) +
      geom_ribbon(fill = "grey", col = NA, alpha = 0.5) +
      geom_line() +
      geom_pointrange(data = dat) +
      ylab("RR prevalence (%)") +
      ggtitle(ttl)
  }
  GP <- ggarrange(plotlist = plts)
  ggsave(GP, file = gh("drtb/plots/refit_{cn}.png"), w = 7, h = 5)
}

PZ <- rbindlist(PZ)
PZ[,DT:=NULL]

## keep only those in uselist
keep <- c()
for (cn in names(uselist)) keep <- c(keep, paste(cn, uselist[[cn]]))
PZ <- PZ[paste(iso3, patients) %in% keep]

## save
save(PZ,file = gh("drtb/outdata/PZ.Rdata"))


## what is the model choice
mdl <- scan(here("drtb/R/utils/modelchoice.txt"), what = "char") # NOTE depends choice
cat("Using model: ", mdl, "\n")
fn <- gh("drtb/outdata/KO.{mdl}.Rdata")
load(file = fn)


## swap out PZ
KO <- merge(KO,
  PZ[, .(iso3, year, patients,
    RR.mid.new = RR.mid, RR.lo.new = RR.lo, RR.hi.new = RR.hi
  )],
  by = c("iso3", "year", "patients"),
  all.x = TRUE, all.y = FALSE
)

KO[
  !is.na(RR.mid.new), # those needing to be swapped
  c("RR.mid", "RR.lo", "RR.hi") :=
    .(RR.mid.new, RR.lo.new, RR.hi.new)
]

KO[, c("RR.mid.new", "RR.lo.new", "RR.hi.new") := NULL]


save(KO, file = fn)
