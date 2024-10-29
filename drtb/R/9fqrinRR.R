## simple version of FQR in RR
rm(list = ls())

## libraries
library(here)
library(glue)
library(data.table)
library(ggplot2)
library(scales)
library(metafor)

gh <- function(x) glue(here(x))
ssum <- function(x, ...) sqrt(sum(x^2, ...)) # sqrt sum sqrs
rot45 <- theme(axis.text.x = element_text(angle = 45, hjust = 1))


## utilities &  helper functions for binomial CIs, incidence calculator
source(gh("drtb/R/utils/fun.R"))
source(gh("drtb/R/utils/CIhelpers.R"))
## source(here('drtb/R/utils/incdr.R'))
source(here("import/load_gtb.R"))


## globals
yr <- 2023

## double check
dic <- load_gtb("dic")
setkey(dic, variable_name)
names(dic)


dic["rr_dst_rlt", definition] # N
dic["rr_dr_fq", definition] # k

dic["rr_dst_rlt_fq", definition] # N
dic["rr_fqr", definition] # k



## data
drfq <- load_gtb("drfq")


noests.yet <- FALSE
if (noests.yet) {
  load(here("drtb/tmpdata/est.rda"))
} else {
  est <- load_gtb("est", convert_dashes = TRUE)
}

## 30 hi mdr in different form
## tmp <- unique(grpmbr[,.(iso3,g.mdr=FALSE)])
## tmp[iso3 %in% grpmbr[group_type=='g_hb_mdr',iso3],g.mdr:=TRUE]
key <- unique(est[, .(iso3, g.mdr)])
key[, unique(g.mdr)]



## NOTE example bad
drfq[iso3 == "ZWE", .(iso3, g_whoregion, year, mdr_dr_fq, mdr_dst_rlt)]


## surveillance
FDl <- drfq[, .(iso3, g_whoregion, year,
  rr.k = rr_dr_fq,
  rr.N = rr_dst_rlt,
  `rr+.k` = rr_fqr,
  `rr+.N` = rr_dst_rlt_fq,
  mdr.k = mdr_dr_fq, mdr.N = mdr_dst_rlt
)]


## ---surveys
## first a couple are missing CIs
drfq[iso3 == "BFA"] # this only has k,N. Assume SRS:
tmp <- drfq[
  iso3 == "BFA",
  .(MLH(rr_fqr_pcnt, rr_dst_rlt_fq))
]
drfq[
  iso3 == "BFA",
  c("rr_fqr_pcnt_mid", "rr_fqr_pcnt_lo", "rr_fqr_pcnt_hi") := tmp[[1]]
]
drfq[iso3 == "BFA"] #CHECK

drfq[iso3 == "TGO" & !is.na(rr_fqr_pcnt)] # this only has k,N. Assume SRS:
tmp <- drfq[
  iso3 == "TGO" & !is.na(rr_fqr_pcnt),
  .(MLH(rr_fqr_pcnt, rr_dst_rlt_fq))
]
drfq[
  iso3 == "TGO" & !is.na(rr_fqr_pcnt),
  c("rr_fqr_pcnt_mid", "rr_fqr_pcnt_lo", "rr_fqr_pcnt_hi") := tmp[[1]]
]
drfq[iso3 == "TGO" & !is.na(rr_fqr_pcnt)] # CHECK

## extract
FDy <- drfq[
  !is.na(rr_fqr_pcnt),
  .(iso3, g_whoregion, year,
    type = "rr", N = NA_integer_, k = NA_integer_,
    FQR.mid = rr_fqr_pcnt, FQR.lo = rr_fqr_pcnt_lo, FQR.hi = rr_fqr_pcnt_hi,
    source = "survey"
  )
]

## removal flags
FDl[, droprr := ifelse(is.na(rr.k + rr.N) | rr.N == 0, TRUE, FALSE)]
FDl[, droprrp := ifelse(is.na(`rr+.k` + `rr+.N`) | `rr+.N` == 0, TRUE, FALSE)]
FDl[, dropmdr := ifelse(is.na(mdr.k + mdr.N) | mdr.N == 0, TRUE, FALSE)]


## reshape
FM <- melt(FDl, id = c("iso3", "g_whoregion", "year", "droprr", "droprrp", "dropmdr"))
FM[, c("type", "frac") := tstrsplit(variable, split = "\\.")]

## do removals
FM <- FM[!(droprr == TRUE & type == "rr")]
FM <- FM[!(droprrp == TRUE & type == "rr+")]
FM <- FM[!(dropmdr == TRUE & type == "mdr")]
FM <- FM[, .(value = sum(value)), by = .(iso3, g_whoregion, year, type, frac)] # NOTE a few repeats

## right format
FM <- dcast(FM[, .(iso3, g_whoregion, year, type, frac, value)],
  iso3 + g_whoregion + year + type ~ frac,
  value.var = "value"
)


## add CIs
FM[, c("FQR.mid", "FQR.lo", "FQR.hi") := MLH(k, N)]
FM[, source := "surveillance"]


summary(FM)
summary(FDy)
FDy[is.na(FQR.lo)]


## both types
FB <- rbind(FM, FDy)
save(FB, file = gh("drtb/data/FB.Rdata"))


## plot
for (reg in unique(FB[, g_whoregion])) {
  print(reg)
  GP <- ggplot(FB[g_whoregion == reg], aes(year, FQR.mid,
    ymin = FQR.lo, ymax = FQR.hi,
    shape = type, col = source
  )) +
    geom_line() +
    geom_point() +
    geom_pointrange() +
    scale_y_sqrt() +
    facet_wrap(~iso3, scales = "free") +
    rot45
  ggsave(GP, file = gh("drtb/plots/f_{reg}.pdf"), w = 12, h = 15)
}

## carry on
load(file = gh("drtb/data/FB.Rdata"))


## simple meta-analyses
FB[, range(year)]
FB[, table(iso3)]
FB[, FQR.sd := (FQR.hi - FQR.lo) / 3.92]
cnt <- FB[, .(n = .N), by = iso3] # data count
FB <- merge(FB, cnt, by = "iso3", all.x = TRUE) # merge in


## step 1 MA for countries with >1 point
FBS <- FB[n > 1]
FBR <- list()
for (iso in FBS[, unique(iso3)]) {
  print(iso)
  tmp <- FBS[iso3 == iso]
  fit <- rma(yi = FQR.mid, sei = FQR.sd, data = tmp)
  imp <- fit$b[, 1]
  imp.sd <- fit$se
  if (abs(imp) > 1e-10) { #>0
    if (abs(imp - 100) < 1e-10) { # all(mid==100) use halway from 100 to mean of lo
      imp.ui <- vlohi(tmp[, 50 + 0.5 * mean(FQR.lo)] / 1e2, imp.sd / 1e2) * 1e2
    } else { # normal case 0<mid<100
      imp.ui <- vlohi(unlist(imp) / 1e2, imp.sd / 1e2) * 1e2
    }
  } else { # all(mid==0) use half mean of hi
    imp.ui <- vlohi(tmp[, 0.5 * mean(FQR.hi)] / 1e2, imp.sd / 1e2) * 1e2
  }
  tmp <- tmp[1]
  tmp[, c("year", "N", "k") := NA_integer_]
  tmp[, c("FQR.mid", "FQR.lo", "FQR.hi", "FQR.sd") := .(imp, imp.ui[1], imp.ui[2], imp.sd)]
  FBR[[iso]] <- tmp
}
FBR <- rbindlist(FBR)

## step 2 MA
FBR <- rbind(FBR, FB[n == 1]) # add in the single year ones to make full data set for MA
FBR <- merge(FBR, key, by = "iso3", all.x = TRUE)
fit <- rma(yi = FQR.mid, sei = FQR.sd, data = FBR, mods = ~ g.mdr - 1)
reg <- names(table(FBR$g.mdr))



## extract
imp <- fit$b[, 1]
imp.sd <- fit$se
imp.ui <- vlohi(unlist(imp) / 1e2, imp.sd / 1e2) * 1e2

xtra <- key[!iso3 %in% FBR$iso3]
xtra[, c("FQR.mid", "FQR.lo", "FQR.hi", "FQR.sd") := NA_real_]


## check the order of regions in reg is the same as in imp
all.equal(reg, gsub("g.mdr", "", dimnames(fit$beta)[[1]]))

## loop over regions
for (i in 1:length(reg)) {
  print(reg[i])
  xtra[
    g.mdr == reg[i],
    c("FQR.mid", "FQR.lo", "FQR.hi", "FQR.sd") := .(
      imp[i], imp.ui[1, i], imp.ui[2, i], imp.sd[i]
    )
  ]
}


## join to non-imputed
FQR <- rbind(
  FBR[, .(iso3, FQR.mid, FQR.lo, FQR.hi, FQR.sd)],
  xtra[, .(iso3, FQR.mid, FQR.lo, FQR.hi, FQR.sd)]
)


summary(FQR)
FQR[is.na(FQR.lo)]

save(FQR, file = gh("drtb/data/FQR.Rdata"))



## aggregate reformat, out
FQR_in_RR_country <- FQR
attr(FQR_in_RR_country, "timestamp") <- Sys.Date() # set date
save(FQR_in_RR_country, file = gh("drtb/output/FQR_in_RR_country.rda"))


load(here("drtb/output/db_dr_country.rda"))

WHOkey <- unique(est[, .(iso3, g.whoregion)])
FQR <- merge(FQR, db_dr_country[year == yr, .(iso3, e_inc_rr_num)], by = "iso3")
FQR <- merge(FQR, WHOkey, by = "iso3")


summary(FQR)

## regional
FQR_in_RR_region <- FQR[, .(
  FQR.mid = weighted.mean(FQR.mid, w = e_inc_rr_num),
  FQR.sd = sqrt(sum(FQR.sd^2 * e_inc_rr_num^2) / sum(e_inc_rr_num)^2)
),
by = g.whoregion
]
FQR_in_RR_region[, c("FQR.lo", "FQR.hi") := .(
  pmax(0, FQR.mid - 1.96 * FQR.sd),
  pmin(100, FQR.mid + 1.96 * FQR.sd)
)]
attr(FQR_in_RR_region, "timestamp") <- Sys.Date() #set date
save(FQR_in_RR_region, file = gh("drtb/output/FQR_in_RR_region.rda"))



## global
FQR_in_RR_global <- FQR[, .(
  FQR.mid = weighted.mean(FQR.mid, w = e_inc_rr_num),
  FQR.sd = sqrt(sum(FQR.sd^2 * e_inc_rr_num^2) / sum(e_inc_rr_num)^2)
)]
FQR_in_RR_global[, c("FQR.lo", "FQR.hi") := .(
  pmax(0, FQR.mid - 1.96 * FQR.sd),
  pmin(100, FQR.mid + 1.96 * FQR.sd)
)]
attr(FQR_in_RR_global, "timestamp") <- Sys.Date() #set date
save(FQR_in_RR_global, file = gh("drtb/output/FQR_in_RR_global.rda"))



## ## ============================

## ## libraries
## library(here)
## library(glue)
## library(rstan)
## library(posterior)
## library(data.table)

## gh <- function(x) glue(here(x))
## getmusig <- function(lo,mid,hi){
##   mid <- mid/1e2; lo <- lo/1e2; hi <- hi/1e2 #convert pcnt to number
##   mid[mid==0] <- 1e-6                   #safety for zeros
##   v <- ((hi-lo)/3.92)^2
##   v[v==0] <- 1e-6
##   mu <- log(mid/sqrt(1+v/mid^2))
##   sig2 <- log(1+v/mid^2)
##   list(mu=mu,sig=sqrt(sig2))
## }


## ## === compile model
## fn <- gh('drtb/stan/F_Leroux_Q.stan')
## sm <- stan_model(file=fn)


## ## load data
## load(file=gh('drtb/data/FB.Rdata'))
## load(file=gh('drtb/data/D4S.Rdata'))
## load(file=gh('drtb/data/isoidx5.Rdata')) #NOTE has to be 5
## ckey <- data.table(iso3=isoidx,cid=1:length(isoidx))

## D4F <- list()
## ## === from D4S
## D4F$P <- D4S$P
## D4F$N <- D4S$N
## D4F$X <- D4S$X
## D4F$Nedges <- D4S$Nedges
## D4F$W <- D4S$W
## ## (2sf)
## D4F$h_beta_mu <- 1.5
## D4F$h_beta_sigma <- 1

## ## === new
## D4F$NDP <- nrow(FB)
## tbl <- FB[,.(n=.N),by=iso3]
## tbl[,table(n)] #2 most common, justifying lack of additional hierarchy
## rptiso <- tbl[n>1,iso3]
## D4F$id_repeats <- which(FB[,iso3 %in% rptiso])
## D4F$Nrepeats <- length(D4F$id_repeats)

## ## site to DP mapping!
## D4F$site2dp <- unlist(lapply(FB$iso3,function(x)which(isoidx==x)))

## ## /* --- surveillance data --- N_L */
## D4F$L_id <- which(FB$source=='surveillance')
## all(FB[,N>=k],na.rm=TRUE) #check
## D4F$L_N <- FB[D4F$L_id,N]
## D4F$L_k <- FB[D4F$L_id,k]

## ## /* --- survey data --- N_Y */
## D4F$Y_id <- which(FB$source=='survey')
## tmp <- FB[D4F$Y_id,getmusig(FQR.lo,FQR.mid,FQR.hi)]
## D4F$Y_M <- tmp$mu
## D4F$Y_S <- tmp$sig


## ## /* --- sizes of each case --- */
## D4F$N_L <- length(D4F$L_id)
## D4F$N_Y <- length(D4F$Y_id)


## ## === run model:
## niter <- 5e3
## nchains <- 1
## cores <- 1

## tt1 <- system.time({
##   samps <- sampling(sm,data = D4F,
##                     iter = niter,
##                     chains = nchains,
##                     cores=cores,
##                     verbose = FALSE)
## })
## tt1/60 #100

## ## post-process
## S <- as.data.table(samps)
## kk <- grep('theta_site',names(samps),value=TRUE)
## S <- S[,..kk]
## S[,id:=1:nrow(S)]
## S <- melt(S,id.vars = 'id')
## S[,cid:=as.integer(gsub('\\]','',gsub('theta_site\\[','',variable)))]
## S <- merge(S,ckey,by='cid')
## S[,value:=1/(1+exp(-value))]
## FQR2 <- S[,.(FQR.mid=1e2*mean(value),
##              FQR.lo=1e2*quantile(value,0.025),
##              FQR.hi=1e2*quantile(value,1-0.025)),by=iso3]

## load(file=gh('drtb/data/FQR.Rdata'))


## CF <- merge(FQR,FQR2[,.(iso3,FQR.mid2=FQR.mid,FQR.lo2=FQR.lo,FQR.hi2=FQR.hi)],by='iso3')


## ggplot(CF,aes(FQR.mid2,FQR.mid))+geom_point()+geom_abline(col=2)
