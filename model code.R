
####################################
####                           #####
####    Sociality ~ ELA       ######
####                           #####
####################################

library(rethinking)
library(dplyr)

d <- read.csv("Social Data for ELA plus kin.csv", header = TRUE, na.strings = "")

#calculate rate for each behavior
d$ff_a1_rate <- d$ff_a1 / d$totalobs
d$ff_a2_rate <- d$ff_a2 / d$totalobs
d$ff_vg1_rate <- d$ff_vg1 / d$totalobs
d$ff_vg2_rate <- d$ff_vg2 / d$totalobs
d$ff_g1_rate <- d$ff_g1 / d$totalobs
d$ff_g2_rate <- d$ff_g2 / d$totalobs

d$fm_a1_rate <- d$fm_a1 / d$totalobs
d$fm_a2_rate <- d$fm_a2 / d$totalobs
d$fm_vg1_rate <- d$fm_vg1 / d$totalobs
d$fm_vg2_rate <- d$fm_vg2 / d$totalobs
d$fm_g1_rate <- d$fm_g1 / d$totalobs
d$fm_g2_rate <- d$fm_g2 / d$totalobs

#group mean for each year

GroupYearMeans <- d %>%
  group_by(focgrp, year) %>%
  summarize(ff_a1_mean = mean(ff_a1_rate), ff_a2_mean = mean(ff_a2_rate), 
            ff_vg1_mean = mean(ff_vg1_rate), ff_vg2_mean = mean(ff_vg2_rate),
            ff_g1_mean = mean(ff_g1_rate), ff_g2_mean = mean(ff_g2_rate),
            
            fm_a1_mean = mean(fm_a1_rate), fm_a2_mean = mean(fm_a2_rate),
            fm_vg1_mean = mean(fm_vg1_rate), fm_vg2_mean = mean(fm_vg2_rate),
            fm_g1_mean = mean(fm_g1_rate), fm_g2_mean = mean(fm_g2_rate) 
  )

#connect group yearly means to individual data rows for easy calculations
merge <- merge(d, GroupYearMeans)
d <- merge

# CSI with approaches & grooming #

d$ff_a1_index <- (d$ff_a1_rate/d$ff_a1_mean)
d$ff_a2_index <- (d$ff_a2_rate/d$ff_a2_mean)
d$ff_vg1_index <- (d$ff_vg1_rate/d$ff_vg1_mean)
d$ff_vg2_index <- (d$ff_vg2_rate/d$ff_vg2_mean)
d$ff_g1_index <- (d$ff_g1_rate/d$ff_g1_mean) 
d$ff_g2_index <- (d$ff_g2_rate/d$ff_g2_mean) 

#female-male indices
d$fm_a1_index <- (d$fm_a1_rate/d$fm_a1_mean)
d$fm_a2_index <- (d$fm_a2_rate/d$fm_a2_mean)
d$fm_vg1_index <- (d$fm_vg1_rate/d$fm_vg1_mean)
d$fm_vg2_index <- (d$fm_vg2_rate/d$fm_vg2_mean)
d$fm_g1_index <- (d$fm_g1_rate/d$fm_g1_mean) 
d$fm_g2_index <- (d$fm_g2_rate/d$fm_g2_mean) 

# joint -- only approaches and grooming
d$csi_ff <- ( d$ff_a1_index + d$ff_a2_index + 
                d$ff_g1_index + d$ff_g2_index ) / 4
d$csi_fm <- ( d$fm_a1_index + d$fm_a2_index + 
                d$fm_g1_index + d$fm_g2_index ) / 4
#in
d$csi_ff_in <- ( d$ff_a2_index + 
                   d$ff_g2_index ) / 2
d$csi_fm_in <- ( d$fm_a2_index + 
                   d$fm_g2_index ) / 2
#out
d$csi_ff_out <- ( d$ff_a1_index + 
                    d$ff_g1_index  ) / 2
d$csi_fm_out <- ( d$fm_a1_index + 
                    d$fm_g1_index  ) / 2


a <- read.csv("ELA sociality 2021.csv", header = TRUE, na.strings = "")
merge <- merge(d,a)
d <- merge
r <- read.csv("elo.csv", header = TRUE, na.strings = "")

RankYearMeans <- r %>%
  group_by(Focal, year) %>%
  summarize(rank_scaled_mean = mean(EloScaled), rank_ordinal_mean = mean(EloOrdinal)
  )

merge <- merge(d,RankYearMeans)
d <- merge


#clean, prepare, standardize data

### groups
d$phg <- ifelse(d$focgrp=="PHG" , 1 , 0 )
d$enk <- ifelse(d$focgrp=="ENK" , 1 , 0 )
d$ynt <- ifelse(d$focgrp=="YNT" , 1 , 0 )

#female info
d$age <- d$startdate - d$DOB
d$ageYear <- d$age/365
d <- d[d$ageYear>=4, ]
d$s_age <-(d$ageYear - mean(d$ageYear))/sd(d$ageYear)
d$age2 <- d$age^2
d$s_age2 <-(d$age2 - mean(d$age2))/sd(d$age2)

d$s_obs <- (d$totalobs - mean(d$totalobs))/sd(d$totalobs)

# early life experiences

# ELA
d <- d[complete.cases(d$group.size.at.birth2bSubs,d$biomassRev,d$ibidays,d$ageatmomdeathRev,d$Primiparous), ]
d$n_grpsize <- (d$group.size.at.birth2bSubs - min(d$group.size.at.birth2bSubs))/(max(d$group.size.at.birth2bSubs)-min(d$group.size.at.birth2bSubs))
d$n_drought <- (d$biomassRev - min(d$biomassRev))/(max(d$biomassRev)-min(d$biomassRev))
d$n_ibi <-(d$ibidays - min(d$ibidays))/(max(d$ibidays)-min(d$ibidays))
d$n_momlossadj <- (d$ageatmomdeathRevadj - min(d$ageatmomdeathRevadj))/(max(d$ageatmomdeathRevadj)-min(d$ageatmomdeathRevadj))

# ELA index
d$cumadvprimip <- d$n_grpsize + d$n_drought + d$n_ibi + d$n_momlossadj + d$Primiparous
d$s_cumadvprimip <- (d$cumadvprimip - mean(d$cumadvprimip))/sd(d$cumadvprimip)

#standardize rank
d$s_rankScaled <- (d$rank_scaled_mean - mean(d$rank_scaled_mean))/sd(d$rank_scaled_mean)
d$s_rankOrdinal <- (d$rank_ordinal_mean - mean(d$rank_ordinal_mean))/sd(d$rank_ordinal_mean)

#kin 
d$s_kin <- (d$CoresMDS - mean(d$CoresMDS))/sd(d$CoresMDS)

#make character or factors into integers 
d$Focal <- as.factor(d$Focal)
d$Focal <- as.integer(as.factor(d$Focal))

d$Year <- d$year
d$Year <- as.factor(d$Year)
d$Year <- as.integer(as.factor(d$Year))


# Models 


csi_ff <- map2stan(
  alist(
    
    ffsoc ~ dgamma2(mu , scale),
    
    log(mu) <- ap + ap_Focal[Focal] + ap_Year[Year] + bp_enk*enk + bp_ynt*ynt + 
      bp_adv*adv + bp_rank*rankScaled + bp_kin*kin,
    
    c(ap_Focal)[Focal] ~ dnorm(0,sigma_focal) ,
    c(ap_Year)[Year] ~ dnorm(0,sigma_year) ,  
    c(ap, bp_enk,bp_ynt,bp_adv,bp_rank,bp_kin) ~ dnorm(0,2) ,
    c(sigma_focal,sigma_year)  ~ dexp(2)  ,
    scale ~ dexp(1)
    
  ),
  data=list(
    ffsoc = d$csi_ff,
    Focal=d$Focal,
    Year = d$Year,
    phg = d$phg,
    enk = d$enk,
    ynt = d$ynt,
    adv = d$s_cumadvprimip,
    rankScaled = d$s_rankScaled,
    kin = d$s_kin
    
  ),
  
  cores=3 , chains=3 , warmup=4000, iter=9000, WAIC=TRUE, types=list(adapt.delta=0.99)
)


csi_ff_in <- map2stan(
  alist(
    
    ffsocin ~ dgamma2(mu , scale),
    
    log(mu) <- ap + ap_Focal[Focal] + ap_Year[Year] + bp_enk*enk + bp_ynt*ynt + 
      bp_adv*adv +  bp_rank*rankScaled + bp_kin*kin,
    
    c(ap_Focal)[Focal] ~ dnorm(0,sigma_focal) ,
    c(ap_Year)[Year] ~ dnorm(0,sigma_year) ,  
    c(ap, bp_enk,bp_ynt,bp_adv,bp_rank,bp_kin) ~ dnorm(0,2) ,
    c(sigma_focal,sigma_year)  ~ dexp(2)  ,
    scale ~ dexp(1)
    
  ),
  data=list(
    ffsocin = d$csi_ff_in,
    Focal=d$Focal,
    Year = d$Year,
    phg = d$phg,
    enk = d$enk,
    ynt = d$ynt,
    adv = d$s_cumadvprimip,
    age = d$s_age,
    age2 = d$s_age2,
    rankScaled = d$s_rankScaled,
    kin = d$s_kin
    
  ),
  
  cores=3 , chains=3 , warmup=4000, iter=9000, WAIC=TRUE, types=list(adapt.delta=0.99)
)



csi_ff_out <- map2stan(
  alist(
    
    ffsocout ~ dgamma2(mu , scale),
    
    log(mu) <- ap + ap_Focal[Focal] + ap_Year[Year] + bp_enk*enk + bp_ynt*ynt + 
      bp_adv*adv + bp_rank*rankScaled + bp_kin*kin,
    
    c(ap_Focal)[Focal] ~ dnorm(0,sigma_focal) ,
    c(ap_Year)[Year] ~ dnorm(0,sigma_year) ,  
    c(ap, bp_enk,bp_ynt,bp_adv,bp_rank,bp_kin) ~ dnorm(0,2) ,
    c(sigma_focal,sigma_year)  ~ dexp(2)  ,
    scale ~ dexp(1)
    
  ),
  data=list(
    ffsocout = d$csi_ff_out,
    Focal=d$Focal,
    Year = d$Year,
    phg = d$phg,
    enk = d$enk,
    ynt = d$ynt,
    adv = d$s_cumadvprimip,
    age = d$s_age,
    age2 = d$s_age2,
    rankScaled = d$s_rankScaled,
    kin = d$s_kin
    
  ),
  
  cores=3 , chains=3 , warmup=4000, iter=9000, WAIC=TRUE, types=list(adapt.delta=0.99)
)


## individual components ## 

#grooming
g1_ff_poisson <- map2stan(
  alist(
    
    ffg1 ~ dpois(lambda),
    
    log(lambda) <- al + obs + al_Focal[Focal] + al_Year[Year] + bl_enk*enk + bl_ynt*ynt + 
      bl_adv*adv + bl_rank*rankScaled + bl_kin*kin,
    
    c(al_Focal)[Focal] ~ dnorm(0, sigma_focal) ,
    c(al_Year)[Year] ~ dnorm(0, sigma_year) , 
    c(al, bl_enk,bl_ynt,bl_adv,bl_rank,bl_kin) ~ dnorm(0,2) ,
    c(sigma_focal,sigma_year)  ~ dexp(1)
    
  ),
  data=list(
    ffg1 = d$ff_g1,
    Focal=d$Focal,
    Year = d$Year,
    obs = d$s_obs,
    phg = d$phg,
    enk = d$enk,
    ynt = d$ynt,
    adv = d$s_cumadvprimip,
    age = d$s_age,
    age2 = d$s_age2,
    rankScaled = d$s_rankScaled,
    kin = d$s_kin
    
  ),
  
  cores=3 , chains=3 , warmup=4000, iter=9000, WAIC=TRUE, types=list(adapt.delta=0.99)
)

g2_ff_poisson <- map2stan(
  alist(
    
    ffg2 ~ dpois(lambda),
    
    log(lambda) <- al + obs + al_Focal[Focal] + al_Year[Year] + bl_enk*enk + bl_ynt*ynt + 
      bl_adv*adv + bl_rank*rankScaled + bl_kin*kin,
    
    c(al_Focal)[Focal] ~ dnorm(0, sigma_focal) ,
    c(al_Year)[Year] ~ dnorm(0, sigma_year) , 
    c(al, bl_enk,bl_ynt,bl_adv,bl_rank,bl_kin) ~ dnorm(0,2) ,
    c(sigma_focal,sigma_year)  ~ dexp(1)
    
  ),
  data=list(
    ffg2 = d$ff_g2,
    Focal=d$Focal,
    Year = d$Year,
    obs = d$s_obs,
    phg = d$phg,
    enk = d$enk,
    ynt = d$ynt,
    adv = d$s_cumadvprimip,
    age = d$s_age,
    age2 = d$s_age2,
    rankScaled = d$s_rankScaled,
    kin = d$s_kin
    
  ),
  
  cores=3 , chains=3 , warmup=4000, iter=9000, WAIC=TRUE, types=list(adapt.delta=0.99)
)


# approaches
a1_ff_poisson <- map2stan(
  alist(
    
    ffa1 ~ dpois(lambda),
    
    log(lambda) <- al + obs + al_Focal[Focal] + al_Year[Year] + bl_enk*enk + bl_ynt*ynt + 
      bl_adv*adv + bl_rank*rankScaled + bl_kin*kin,
    
    c(al_Focal)[Focal] ~ dnorm(0, sigma_focal) ,
    c(al_Year)[Year] ~ dnorm(0, sigma_year) , 
    c(al, bl_enk,bl_ynt,bl_adv,bl_rank,bl_kin) ~ dnorm(0,2) ,
    c(sigma_focal,sigma_year)  ~ dexp(1)
    
  ),
  data=list(
    ffa1 = d$ff_a1,
    Focal=d$Focal,
    Year = d$Year,
    obs = d$s_obs,
    phg = d$phg,
    enk = d$enk,
    ynt = d$ynt,
    adv = d$s_cumadvprimip,
    age = d$s_age,
    age2 = d$s_age2,
    rankScaled = d$s_rankScaled,
    kin = d$s_kin
    
  ),
  
  cores=3 , chains=3 , warmup=4000, iter=9000, WAIC=TRUE, types=list(adapt.delta=0.99)
)


a2_ff_poisson <- map2stan(
  alist(
    
    ffa2 ~ dpois(lambda),
    
    log(lambda) <- al + obs + al_Focal[Focal] + al_Year[Year] + bl_enk*enk + bl_ynt*ynt + 
      bl_adv*adv + bl_rank*rankScaled + bl_kin*kin,
    
    c(al_Focal)[Focal] ~ dnorm(0, sigma_focal) ,
    c(al_Year)[Year] ~ dnorm(0, sigma_year) , 
    c(al, bl_enk,bl_ynt,bl_adv,bl_rank,bl_kin) ~ dnorm(0,2) ,
    c(sigma_focal,sigma_year)  ~ dexp(1)
    
  ),
  data=list(
    ffa2 = d$ff_a2,
    Focal=d$Focal,
    Year = d$Year,
    obs = d$s_obs,
    phg = d$phg,
    enk = d$enk,
    ynt = d$ynt,
    adv = d$s_cumadvprimip,
    age = d$s_age,
    age2 = d$s_age2,
    rankScaled = d$s_rankScaled,
    kin = d$s_kin
    
  ),
  
  cores=3 , chains=3 , warmup=4000, iter=9000, WAIC=TRUE, types=list(adapt.delta=0.99)
)




















####################################
####                           #####
####    Nice ~ ELA            ######
####                           #####
####################################


d <- read.csv("Sam BehavTabs Ela Elo Nice.csv", header = TRUE, na.strings = "")
d <- d[complete.cases(d$propgrunt,d$ela, d$elo), ]

### groups
d$phg <- ifelse(d$grp=="PHG" , 1 , 0 )
d$enk <- ifelse(d$grp=="ENK" , 1 , 0 )
d$ynt <- ifelse(d$grp=="YNT" , 1 , 0 )

d$age <- (d$startdate - d$DOB)/365
d$age2 <- d$age^2
#limit to over 4 years old
d <- d[d$age>=4 , ]

#standardize actor's ela, rank, age
d$s_ELA <- (d$ela - mean(d$ela ))/sd(d$ela )
d$s_elo <- (d$elo - mean(d$elo ))/sd(d$elo )

#index actor, recipient, group
d$Focal_id <- coerce_index(d$Focal)
d$Year_id <- coerce_index(d$realyr)

#dont need kin because approaches are limited to nonkin for this metric
NiceGrunts_model_aggregate <- map2stan(
  alist(
    
    grunt ~ dbinom(n,p),
    
    logit(p) <- a + a_focal[Focal] + a_year[Year] +
      b_ELA*ELA + b_elo*elo +
      b_enk*enk + b_ynt*ynt,
    
    a_focal[Focal] ~ dnorm(0,sigma_focal) ,
    a_year[Year] ~ dnorm(0,sigma_year) ,
    c(a,b_ELA, b_elo, b_enk, b_ynt) ~ dnorm(0,1) ,
    c(sigma_focal,sigma_year)  ~ dexp(1)
    
  ),
  data=list(
    grunt = d$grunt,
    n = d$allappr,
    Focal = d$Focal_id,
    Year = d$Year_id,
    enk = d$enk,
    ynt = d$ynt,
    ELA = d$s_ELA, 
    elo = d$s_elo
  ),
  
  cores=3 , chains=2 , warmup=3000, iter=6000, WAIC=TRUE, types=list(adapt.delta=0.99)
)












####################################
####                           #####
####  CSI ~ Grunts ; compare   ######
####                           #####
####################################


CSI_ELA_Grunt <- map2stan(
  alist(
    
    ffsoc ~ dgamma2(mu , scale),
    
    log(mu) <- ap + ap_Focal[Focal] + ap_Year[Year] + bp_enk*enk + bp_ynt*ynt + 
      ELA*adv + Grunt*nice + bp_rank*rankScaled + bp_kin*kin,
    
    c(ap_Focal)[Focal] ~ dnorm(0,sigma_focal) ,
    c(ap_Year)[Year] ~ dnorm(0,sigma_year) ,  
    c(ap, bp_enk,bp_ynt,ELA,Grunt,bp_rank,bp_kin) ~ dnorm(0,1) ,
    c(sigma_focal,sigma_year)  ~ dexp(2)  ,
    scale ~ dexp(1)
    
  ),
  data=list(
    ffsoc = d$csi_ff,
    Focal=d$Focal,
    Year = d$Year,
    phg = d$phg,
    enk = d$enk,
    ynt = d$ynt,
    adv = d$s_ELA,
    nice = d$s_nice,
    rankScaled = d$s_elo,
    kin = d$s_kin

  ),
  
  cores=3 , chains=3 , warmup=4000, iter=9000, WAIC=TRUE, types=list(adapt.delta=0.99)
)

CSI_ELA <- map2stan(
  alist(
    
    ffsoc ~ dgamma2(mu , scale),
    
    log(mu) <- ap + ap_Focal[Focal] + ap_Year[Year] + bp_enk*enk + bp_ynt*ynt + 
      ELA*adv + bp_rank*rankScaled + bp_kin*kin,
    
    c(ap_Focal)[Focal] ~ dnorm(0,sigma_focal) ,
    c(ap_Year)[Year] ~ dnorm(0,sigma_year) ,  
    c(ap, bp_enk,bp_ynt,ELA,bp_rank,bp_kin) ~ dnorm(0,1) ,
    c(sigma_focal,sigma_year)  ~ dexp(2)  ,
    scale ~ dexp(1)
    
  ),
  data=list(
    ffsoc = d$csi_ff,
    Focal=d$Focal,
    Year = d$Year,
    phg = d$phg,
    enk = d$enk,
    ynt = d$ynt,
    adv = d$s_ELA,
    nice = d$s_nice,
    rankScaled = d$s_elo,
    kin = d$s_kin
    
  ),
  
  cores=3 , chains=3 , warmup=4000, iter=9000, WAIC=TRUE, types=list(adapt.delta=0.99)
)





