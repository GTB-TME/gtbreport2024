#' ---
#' title: Aggregates estimates of TB incidence and mortality
#' author: Mathieu Bastard
#' date: 25/06/2024


#' # Preamble
#' (Last updated: `r Sys.Date()`)
#' This creates generates aggregates of TB estimates by various levels,
#' global (object global), WHO region (regional), HBCs (hbd), Income group (estimates_income)
#' 
#' 
#' Deps: library data.table and here as for other scripts in the sequence
#'    ~/R/fun.R helper functions
#' 
#' Input: est.rda, old.rda (last year's estimates), tb.rda (notifications),
#'    cty.rda (country attributes)
#'    
#' Output: global.rda, regional.rda, hbc.rda, estimates_income.rda and related timestamped csv files
#'    
#'    
#'    2024 updates:
#'    
#'    - Add aggregated estimates as per type of income (LIC, HIC, UMC. LMC).
#'      This required a small change in the addXY function, with a change in the output format of r.lo as.double
#'    
#'    
#
# Load libraries and data
#
library(data.table)
library(imputeTS)
library(propagate)
library(here)


rm(list=ls())

#Run Functions script
source(here('inc_mort/R code/fun.R'))

#Function to import db from GTB databases
source(here("import/load_gtb.R"))


tb <- load_gtb("tb",convert_dots = FALSE)
cty <- load_gtb("cty",convert_dots = FALSE)
pop <- load_gtb("pop",convert_dots = FALSE)
grpmbr <- load_gtb("grpmbr",convert_dots = FALSE)



#load last year estiamtes data
load(here('inc_mort/estimates2023/old.rda'))
old=copy(est)
rm(est)

#load estimates
load(here('inc_mort/analysis/est.rda'))


#load UNDAIS data
load(here('inc_mort/analysis/unaids.rda'))


# vectorized lohi
#
vlohi <- Vectorize(lohi, c('ev', 'sd'))

m <- 1e5
yr <- 2023


# global incidence
#
global.inc <-
  est[, addXY(inc / m, r.sd = inc.sd / m, weights = pop), by = year]
setnames(
  global.inc,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'inc',
    'inc.lo',
    'inc.hi',
    'inc.sd',
    'inc.num',
    'inc.lo.num',
    'inc.hi.num',
    'pop'
  )
)
global.inc <-
  cbind(global.inc[, -(2:5), with = FALSE] , global.inc[, 2:5, with = FALSE] * m)

# global incidence HIV-
#
global.inc.nh <-
  est[, addXY(inc.nh / m, r.sd = inc.nh.sd / m, weights = pop), by = year]
setnames(
  global.inc.nh,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'inc.nh',
    'inc.nh.lo',
    'inc.nh.hi',
    'inc.nh.sd',
    'inc.nh.num',
    'inc.nh.lo.num',
    'inc.nh.hi.num',
    'pop'
  )
)
global.inc.nh <-
  cbind(global.inc.nh[, -(2:5), with = FALSE] , global.inc.nh[, 2:5, with =
                                                                FALSE] * m)

# global incidence HIV+
#
global.inc.h <-
  est[, addXY(inc.h / m, r.sd = inc.h.sd / m, weights = pop), by = year]
setnames(
  global.inc.h,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'inc.h',
    'inc.h.lo',
    'inc.h.hi',
    'inc.h.sd',
    'inc.h.num',
    'inc.h.lo.num',
    'inc.h.hi.num',
    'pop'
  )
)
global.inc.h <-
  cbind(global.inc.h[, -(2:5), with = FALSE] , global.inc.h[, 2:5, with =
                                                              FALSE] * m)


# global mortality HIV-neg
#
global.mort.nh <-
  est[, addXY(mort.nh / m, r.sd = mort.nh.sd / m, weights = pop), by = year]
setnames(
  global.mort.nh,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'mort.nh',
    'mort.nh.lo',
    'mort.nh.hi',
    'mort.nh.sd',
    'mort.nh.num',
    'mort.nh.lo.num',
    'mort.nh.hi.num',
    'pop'
  )
)
global.mort.nh <-
  cbind(global.mort.nh[, -(2:5), with = FALSE] , global.mort.nh[, 2:5, with =
                                                                  FALSE] * m)


# global mortality HIV-pos
#
global.mort.h <-
  est[, addXY(mort.h / m, r.sd = mort.h.sd / m, weights = pop), by = year]
setnames(
  global.mort.h,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'mort.h',
    'mort.h.lo',
    'mort.h.hi',
    'mort.h.sd',
    'mort.h.num',
    'mort.h.lo.num',
    'mort.h.hi.num',
    'pop'
  )
)
global.mort.h <-
  cbind(global.mort.h[, -(2:5), with = FALSE] , global.mort.h[, 2:5, with =
                                                                FALSE] * m)


# global total mortality
#
global.mort <-
  est[, addXY(mort / m, r.sd = mort.sd / m, weights = pop), by = year]
setnames(
  global.mort,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'mort',
    'mort.lo',
    'mort.hi',
    'mort.sd',
    'mort.num',
    'mort.lo.num',
    'mort.hi.num',
    'pop'
  )
)
global.mort <-
  cbind(global.mort[, -(2:5), with = FALSE] , global.mort[, 2:5, with = FALSE] * m)


# put the whole global thing together
#
rg <- c(2:4, 6:9)
global <-
  cbind(
    global.inc,
    global.inc.nh[, rg, with = FALSE],
    global.inc.h[, rg, with = FALSE],
    global.mort.nh[, rg, with = FALSE],
    global.mort.h[, rg, with = FALSE],
    global.mort[, rg, with = FALSE]
  )

# add tbhiv
#
out <- global[, divXY(inc.h, inc, inc.h.sd, inc.sd)]
out2 <- vlohi(out[[1]], out[[2]])
global$tbhiv <- out[[1]]
global$tbhiv.lo <- out2[1,]
global$tbhiv.hi <- out2[2,]
global[, tbhiv.sd := out[[2]]]


# add c.newinc
#
out <- tb[year > 1999, sum(c.newinc, na.rm = TRUE), by = year]

global$c.newinc <- out$V1
global$newinc <- global$c.newinc * m / global$pop


# add CFR
#
out <- global[, divXY(mort, inc, mort.sd, inc.sd)]
out2 <- vlohi(out[[1]], out[[2]])
global$cfr <- out[[1]]
global$cfr.lo <- out2[1,]
global$cfr.hi <- out2[2,]
global[, cfr.sd := out[[2]]]

attr(global, "timestamp") <- Sys.Date() #set date
save(global, file = here('inc_mort/analysis/global.rda'))
fwrite(global, file = here(paste0('inc_mort/analysis/csv/global_', Sys.Date(), '.csv')))




#Plot
library(gtbreport)

# Incidence
inc_plot <- ggplot(data = subset(global, year>=2010),
                   mapping = aes(year, inc)) +
  geom_line(size=1, colour = I('blue')) +
  geom_ribbon(aes(year, ymin = inc.lo, ymax = inc.hi),
              fill = I('blue'),
              alpha = 0.4) +
  ylab('Rate per 100 000 population per year') + xlab('Year') +
  expand_limits(y = 0) +
  geom_line(aes(year, newinc)) +
  
  theme_gtb()  +
  scale_x_continuous(breaks=seq(2010,2022,2)) +
  
  
  # Get rid of annoying x-axis line and ticks
  theme(axis.line.x = ggplot2::element_blank(),
        axis.ticks.x = element_blank())

ggsave(here('inc_mort/output/checks/global_incidence.png'), plot=inc_plot, width=9, height=6)


# TB deaths
mort_plot <- ggplot(data = subset(global, year>=2010),
                    mapping = aes(year, mort.num / 1e6)) +
  geom_line(size=1, colour = I('grey20')) +
  geom_ribbon(aes(year, ymin = mort.lo.num / 1e6, ymax = mort.hi.num / 1e6),
              fill = I('grey40'),
              alpha = 0.4) +
  ylab('Millions per year') + xlab('Year') +
  expand_limits(y = 0) +
  
  theme_gtb()  +
  scale_x_continuous(breaks=seq(2010,2022,2)) +
  
  # Get rid of annoying x-axis line and ticks
  theme(axis.line.x = ggplot2::element_blank(),
        axis.ticks.x = element_blank())

ggsave(here('inc_mort/output/checks/global_mortality.png'), plot=mort_plot, width=9, height=6)











# Aggregates by WHO region
#
# regional incidence
#
regional.inc <-
  est[, addXY(inc / m, r.sd = inc.sd / m, weights = pop), by = c("g.whoregion", "year")]
setnames(
  regional.inc,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'inc',
    'inc.lo',
    'inc.hi',
    'inc.sd',
    'inc.num',
    'inc.lo.num',
    'inc.hi.num',
    'pop'
  )
)
regional.inc <-
  cbind(regional.inc[, -(3:6), with = FALSE] , regional.inc[, 3:6, with =
                                                              FALSE] * m)

regional.inc.nh <-
  est[, addXY(inc.nh / m, r.sd = inc.nh.sd / m, weights = pop), by = c("g.whoregion", "year")]
setnames(
  regional.inc.nh,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'inc.nh',
    'inc.nh.lo',
    'inc.nh.hi',
    'inc.nh.sd',
    'inc.nh.num',
    'inc.nh.lo.num',
    'inc.nh.hi.num',
    'pop'
  )
)
regional.inc.nh <-
  cbind(regional.inc.nh[, -(3:6), with = FALSE] , regional.inc.nh[, 3:6, with =
                                                                    FALSE] * m)

regional.inc.h <-
  est[, addXY(inc.h / m, r.sd = inc.h.sd / m, weights = pop), by = c("g.whoregion", "year")]
setnames(
  regional.inc.h,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'inc.h',
    'inc.h.lo',
    'inc.h.hi',
    'inc.h.sd',
    'inc.h.num',
    'inc.h.lo.num',
    'inc.h.hi.num',
    'pop'
  )
)
regional.inc.h <-
  cbind(regional.inc.h[, -(3:6), with = FALSE] , regional.inc.h[, 3:6, with =
                                                                  FALSE] * m)


# regional mortality HIV-neg
#
regional.mort.nh <-
  est[, addXY(mort.nh / m, r.sd = mort.nh.sd / m, weights = pop), by = c("g.whoregion", "year")]
setnames(
  regional.mort.nh,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'mort.nh',
    'mort.nh.lo',
    'mort.nh.hi',
    'mort.nh.sd',
    'mort.nh.num',
    'mort.nh.lo.num',
    'mort.nh.hi.num',
    'pop'
  )
)
regional.mort.nh <-
  cbind(regional.mort.nh[, -(3:6), with = FALSE] , regional.mort.nh[, 3:6, with =
                                                                      FALSE] * m)


# regional mortality HIV-pos
#
regional.mort.h <-
  est[, addXY(mort.h / m, r.sd = mort.h.sd / m, weights = pop), by = c("g.whoregion", "year")]
setnames(
  regional.mort.h,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'mort.h',
    'mort.h.lo',
    'mort.h.hi',
    'mort.h.sd',
    'mort.h.num',
    'mort.h.lo.num',
    'mort.h.hi.num',
    'pop'
  )
)
regional.mort.h <-
  cbind(regional.mort.h[, -(3:6), with = FALSE] , regional.mort.h[, 3:6, with =
                                                                    FALSE] * m)


# regional total mortality
#
regional.mort <-
  est[, addXY(mort / m, r.sd = mort.sd / m, weights = pop), by = c("g.whoregion", "year")]
setnames(
  regional.mort,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'mort',
    'mort.lo',
    'mort.hi',
    'mort.sd',
    'mort.num',
    'mort.lo.num',
    'mort.hi.num',
    'pop'
  )
)
regional.mort <-
  cbind(regional.mort[, -(3:6), with = FALSE] , regional.mort[, 3:6, with =
                                                                FALSE] * m)


# put the whole regional thing together
#
rg <- c(3:5, 7:10)
regional <-
  cbind(
    regional.inc,
    regional.inc.nh[, rg, with = FALSE],
    regional.inc.h[, rg, with = FALSE],
    regional.mort.nh[, rg, with = FALSE],
    regional.mort.h[, rg, with = FALSE],
    regional.mort[, rg, with = FALSE]
  )

# add tbhiv
#
out <- regional[, divXY(inc.h, inc, inc.h.sd, inc.sd)]
out2 <- vlohi(out$mean, out$sd)
regional$tbhiv <- out[[1]]
regional$tbhiv.lo <- out2[1,]
regional$tbhiv.hi <- out2[2,]
regional$tbhiv.sd <- out[[2]]




# add c.newinc
#
out <-
  tb[year > 1999, sum(c.newinc, na.rm = TRUE), by = c("g.whoregion", "year")]
regional$c.newinc <- out$V1
regional$newinc <- regional$c.newinc * m / regional$pop


# add CFR
#
out <- regional[, divXY(mort, inc, mort.sd, inc.sd)]
out2 <- vlohi(out[[1]], out[[2]])
regional$cfr <- out[[1]]
regional$cfr.lo <- out2[1,]
regional$cfr.hi <- out2[2,]
regional[, cfr.sd := out[[2]]]


attr(regional, "timestamp") <- Sys.Date() #set date
save(regional, file = here('inc_mort/analysis/regional.rda'))
fwrite(regional, file = here(paste0('inc_mort/analysis/csv/regional_', Sys.Date(), '.csv')))




# PLOTS
reg.plot <- ggplot(data = subset(regional, year>=2010),
              mapping = aes(year, inc)) +
    geom_line(size=1, colour = I('blue')) +
    geom_ribbon(aes(year, ymin = inc.lo, ymax = inc.hi),
                fill = I('blue'),
                alpha = 0.4) +
    ylab('Rate per 100 000 population per year') + xlab('Year') +
    geom_line(aes(year, newinc)) +
    expand_limits(y = 0) +
    facet_wrap( ~ g.whoregion, scales = 'free_y')+
    theme_gtb()  +
    scale_x_continuous(breaks=seq(2010,2022,2)) +
    
    # Get rid of annoying x-axis line and ticks
    theme(axis.line.x = ggplot2::element_blank(),
          axis.ticks.x = element_blank())


ggsave(here('inc_mort/output/checks/regional_incidence.png'), plot=reg.plot, width=9, height=6)


reg.plot.m <- ggplot(data = subset(regional, year>=2010),
                     mapping = aes(year, mort.num / 1e6)) +
  geom_line(size=1, colour = I('grey20')) +
  geom_ribbon(aes(year, ymin = mort.lo.num / 1e6, ymax = mort.hi.num / 1e6),              fill = I('blue'),
              alpha = 0.4) +
  ylab('Millions per year') + xlab('Year') +
  expand_limits(y = 0) +
  facet_wrap( ~ g.whoregion, scales = 'free_y')+
  theme_gtb()  +
  scale_x_continuous(breaks=seq(2010,2022,2)) +
  
  # Get rid of annoying x-axis line and ticks
  theme(axis.line.x = ggplot2::element_blank(),
        axis.ticks.x = element_blank())


ggsave(here('inc_mort/output/checks/regional_mortality.png'), plot=reg.plot.m, width=9, height=6)




# # Aggregates in HBCs
#
select <- est$g.hbc == TRUE

# hbc incidence
#
hbc.inc <-
  est[select][, addXY(inc / m, r.sd = inc.sd / m, weights = pop), by = year]
setnames(
  hbc.inc,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'inc',
    'inc.lo',
    'inc.hi',
    'inc.sd',
    'inc.num',
    'inc.lo.num',
    'inc.hi.num',
    'pop'
  )
)
hbc.inc <-
  cbind(hbc.inc[, -(2:5), with = FALSE] , hbc.inc[, 2:5, with = FALSE] * m)

# hbc incidence HIV+
#
hbc.inc.h <-
  est[select][, addXY(inc.h / m, r.sd = inc.h.sd / m, weights = pop), by =
                year]
setnames(
  hbc.inc.h,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'inc.h',
    'inc.h.lo',
    'inc.h.hi',
    'inc.h.sd',
    'inc.h.num',
    'inc.h.lo.num',
    'inc.h.hi.num',
    'pop'
  )
)
hbc.inc.h <-
  cbind(hbc.inc.h[, -(2:5), with = FALSE] , hbc.inc.h[, 2:5, with = FALSE] * m)


# hbc mortality HIV-neg
#
hbc.mort.nh <-
  est[select][, addXY(mort.nh / m, r.sd = mort.nh.sd / m, weights = pop), by =
                year]
setnames(
  hbc.mort.nh,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'mort.nh',
    'mort.nh.lo',
    'mort.nh.hi',
    'mort.nh.sd',
    'mort.nh.num',
    'mort.nh.lo.num',
    'mort.nh.hi.num',
    'pop'
  )
)
hbc.mort.nh <-
  cbind(hbc.mort.nh[, -(2:5), with = FALSE] , hbc.mort.nh[, 2:5, with = FALSE] * m)


# hbc mortality HIV-pos
#
hbc.mort.h <-
  est[select][, addXY(mort.h / m, r.sd = mort.h.sd / m, weights = pop), by =
                year]
setnames(
  hbc.mort.h,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'mort.h',
    'mort.h.lo',
    'mort.h.hi',
    'mort.h.sd',
    'mort.h.num',
    'mort.h.lo.num',
    'mort.h.hi.num',
    'pop'
  )
)
hbc.mort.h <-
  cbind(hbc.mort.h[, -(2:5), with = FALSE] , hbc.mort.h[, 2:5, with = FALSE] * m)


# hbc total mortality
#
hbc.mort <-
  est[select][, addXY(mort / m, r.sd = mort.sd / m, weights = pop), by =
                year]
setnames(
  hbc.mort,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'mort',
    'mort.lo',
    'mort.hi',
    'mort.sd',
    'mort.num',
    'mort.lo.num',
    'mort.hi.num',
    'pop'
  )
)
hbc.mort <-
  cbind(hbc.mort[, -(2:5), with = FALSE] , hbc.mort[, 2:5, with = FALSE] * m)


# put the whole hbc thing together
#
rg <- c(2:4, 6:9)
hbc <- cbind(hbc.inc, hbc.inc.h[, rg, with = FALSE],
             hbc.mort.nh[, rg, with = FALSE], hbc.mort.h[, rg, with = FALSE],
             hbc.mort[, rg, with = FALSE])

# add tbhiv
#
out <- hbc[, divXY(inc.h, inc, inc.h.sd, inc.sd)]
out2 <- vlohi(out[[1]], out[[2]])
hbc$tbhiv <- out[[1]]
hbc$tbhiv.lo <- out2[1,]
hbc$tbhiv.hi <- out2[2,]
hbc$tbhiv.sd <- out[[2]]


# add c.newinc
#
out <- est[g.hbc == TRUE, sum(c.newinc, na.rm = TRUE), by = c("year")]
hbc$c.newinc <- out$V1
hbc$newinc <- hbc$c.newinc * m / hbc$pop

attr(hbc, "timestamp") <- Sys.Date() #set date
save(hbc, file = here('inc_mort/analysis/hbc.rda'))
fwrite(hbc, file = here(paste0('inc_mort/analysis/csv/hbc_', Sys.Date(), '.csv')))




# PLOTS
hbc.plot <- ggplot(data = subset(hbc, year>=2010),
                   mapping = aes(year, inc)) +
  geom_line(size=1, colour = I('blue')) +
  geom_ribbon(aes(year, ymin = inc.lo, ymax = inc.hi),
              fill = I('blue'),
              alpha = 0.4) +
  ylab('Rate per 100 000 population per year') + xlab('Year') +
  geom_line(aes(year, newinc)) +
  expand_limits(y = 0) +
  theme_gtb()  +
  scale_x_continuous(breaks=seq(2010,2022,2)) +
  
  # Get rid of annoying x-axis line and ticks
  theme(axis.line.x = ggplot2::element_blank(),
        axis.ticks.x = element_blank())


ggsave(here('inc_mort/output/checks/hbc_incidence.png'), plot=hbc.plot, width=9, height=6)


reg.plot.m <- ggplot(data = subset(hbc, year>=2010),
                     mapping = aes(year, mort.num / 1e6)) +
  geom_line(size=1, colour = I('grey20')) +
  geom_ribbon(aes(year, ymin = mort.lo.num / 1e6, ymax = mort.hi.num / 1e6),              fill = I('blue'),
              alpha = 0.4) +
  ylab('Millions per year') + xlab('Year') +
  expand_limits(y = 0) +
  theme_gtb()  +
  scale_x_continuous(breaks=seq(2010,2022,2)) +
  
  # Get rid of annoying x-axis line and ticks
  theme(axis.line.x = ggplot2::element_blank(),
        axis.ticks.x = element_blank())


ggsave(here('inc_mort/output/checks/hbc_mortality.png'), plot=reg.plot.m, width=9, height=6)






# Update 2024
# Aggregates by income type (World Bank)
#
# Income incidence
#

income.inc <-
  est[, addXY(inc / m, r.sd = inc.sd / m, weights = pop), by = c("g.income", "year")]
setnames(
  income.inc,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'inc',
    'inc.lo',
    'inc.hi',
    'inc.sd',
    'inc.num',
    'inc.lo.num',
    'inc.hi.num',
    'pop'
  )
)
income.inc <-
  cbind(income.inc[, -(3:6), with = FALSE] , income.inc[, 3:6, with =
                                                              FALSE] * m)

income.inc.nh <-
  est[, addXY(inc.nh / m, r.sd = inc.nh.sd / m, weights = pop), by = c("g.income", "year")]
setnames(
  income.inc.nh,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'inc.nh',
    'inc.nh.lo',
    'inc.nh.hi',
    'inc.nh.sd',
    'inc.nh.num',
    'inc.nh.lo.num',
    'inc.nh.hi.num',
    'pop'
  )
)
income.inc.nh <-
  cbind(income.inc.nh[, -(3:6), with = FALSE] , income.inc.nh[, 3:6, with =
                                                                    FALSE] * m)

income.inc.h <-
  est[, addXY(inc.h / m, r.sd = inc.h.sd / m, weights = pop), by = c("g.income", "year")]
setnames(
  income.inc.h,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'inc.h',
    'inc.h.lo',
    'inc.h.hi',
    'inc.h.sd',
    'inc.h.num',
    'inc.h.lo.num',
    'inc.h.hi.num',
    'pop'
  )
)
income.inc.h <-
  cbind(income.inc.h[, -(3:6), with = FALSE] , income.inc.h[, 3:6, with =
                                                                  FALSE] * m)


# Income mortality HIV-neg
#
income.mort.nh <-
  est[, addXY(mort.nh / m, r.sd = mort.nh.sd / m, weights = pop), by = c("g.income", "year")]
setnames(
  income.mort.nh,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'mort.nh',
    'mort.nh.lo',
    'mort.nh.hi',
    'mort.nh.sd',
    'mort.nh.num',
    'mort.nh.lo.num',
    'mort.nh.hi.num',
    'pop'
  )
)
income.mort.nh <-
  cbind(income.mort.nh[, -(3:6), with = FALSE] , income.mort.nh[, 3:6, with =
                                                                      FALSE] * m)


# income mortality HIV-pos
#
income.mort.h <-
  est[, addXY(mort.h / m, r.sd = mort.h.sd / m, weights = pop), by = c("g.income", "year")]
setnames(
  income.mort.h,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'mort.h',
    'mort.h.lo',
    'mort.h.hi',
    'mort.h.sd',
    'mort.h.num',
    'mort.h.lo.num',
    'mort.h.hi.num',
    'pop'
  )
)
income.mort.h <-
  cbind(income.mort.h[, -(3:6), with = FALSE] , income.mort.h[, 3:6, with =
                                                                    FALSE] * m)


# income total mortality
#
income.mort <-
  est[, addXY(mort / m, r.sd = mort.sd / m, weights = pop), by = c("g.income", "year")]
setnames(
  income.mort,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'mort',
    'mort.lo',
    'mort.hi',
    'mort.sd',
    'mort.num',
    'mort.lo.num',
    'mort.hi.num',
    'pop'
  )
)
income.mort <-
  cbind(income.mort[, -(3:6), with = FALSE] , income.mort[, 3:6, with =
                                                                FALSE] * m)


# put the whole income thing together
#
rg <- c(3:5, 7:10)
income <-
  cbind(
    income.inc,
    income.inc.nh[, rg, with = FALSE],
    income.inc.h[, rg, with = FALSE],
    income.mort.nh[, rg, with = FALSE],
    income.mort.h[, rg, with = FALSE],
    income.mort[, rg, with = FALSE]
  )

# add tbhiv
#
out <- income[, divXY(inc.h, inc, inc.h.sd, inc.sd)]
out2 <- vlohi(out$mean, out$sd)
income$tbhiv <- out[[1]]
income$tbhiv.lo <- out2[1,]
income$tbhiv.hi <- out2[2,]
income$tbhiv.sd <- out[[2]]




# add c.newinc
#

incomegrp <- grpmbr[group.type == 'g_income']
setnames(incomegrp, 'group.name', 'g.income')
tb2 <-
  merge(tb, incomegrp[, .(iso3, g.income)], by = 'iso3', all.x = TRUE)
tb2$g.income <- as.character(tb2$g.income)


out <-
  tb2[year > 1999, sum(c.newinc, na.rm = TRUE), by = c("g.income", "year")]
income$c.newinc <- out$V1
income$newinc <- income$c.newinc * m / income$pop


# add CFR
#
out <- income[, divXY(mort, inc, mort.sd, inc.sd)]
out2 <- vlohi(out[[1]], out[[2]])
income$cfr <- out[[1]]
income$cfr.lo <- out2[1,]
income$cfr.hi <- out2[2,]
income[, cfr.sd := out[[2]]]


income=income[g.income!=""]


attr(income, "timestamp") <- Sys.Date() #set date
save(income, file = here('inc_mort/analysis/estimates_income.rda'))
fwrite(income, file = here(paste0('inc_mort/analysis/csv/estimates_income_', Sys.Date(), '.csv')))




# PLOTS
income.plot <- ggplot(data = subset(income, year>=2010),
                   mapping = aes(year, inc)) +
  geom_line(size=1, colour = I('blue')) +
  geom_ribbon(aes(year, ymin = inc.lo, ymax = inc.hi),
              fill = I('blue'),
              alpha = 0.4) +
  ylab('Rate per 100 000 population per year') + xlab('Year') +
  geom_line(aes(year, newinc)) +
  expand_limits(y = 0) +
  facet_wrap( ~ g.income, scales = 'free_y')+
  theme_gtb()  +
  scale_x_continuous(breaks=seq(2010,2023,2)) +
  
  # Get rid of annoying x-axis line and ticks
  theme(axis.line.x = ggplot2::element_blank(),
        axis.ticks.x = element_blank())


ggsave(here('inc_mort/output/checks/income_incidence.png'), plot=income.plot, width=9, height=6)


income.plot.m <- ggplot(data = subset(income, year>=2010),
                     mapping = aes(year, mort.num / 1e6)) +
  geom_line(size=1, colour = I('grey20')) +
  geom_ribbon(aes(year, ymin = mort.lo.num / 1e6, ymax = mort.hi.num / 1e6),              fill = I('blue'),
              alpha = 0.4) +
  ylab('Millions per year') + xlab('Year') +
  expand_limits(y = 0) +
  facet_wrap( ~ g.income, scales = 'free_y')+
  theme_gtb()  +
  scale_x_continuous(breaks=seq(2010,2023,2)) +
  
  # Get rid of annoying x-axis line and ticks
  theme(axis.line.x = ggplot2::element_blank(),
        axis.ticks.x = element_blank())


ggsave(here('inc_mort/output/checks/income_mortality.png'), plot=income.plot.m, width=9, height=6)






