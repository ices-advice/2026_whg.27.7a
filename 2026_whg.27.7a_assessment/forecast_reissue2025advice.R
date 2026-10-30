## LOAD packages & model output ####
library(stockassessment)
library(ggplot2)
library(icesAdvice)
library(tidyverse)
load("./output/output.RData")


## FORECAST settings ####

#  Reference points
F_MSY <- 0.21
F_MSY_lower <- 0.16
F_MSY_upper <- 0.314
Fpa <- 0.78
Flim <- 1.062
Blim <- 1670
Bpa <- 2322
MSY_Btrigger <- 2322

# TAC & advice for previous year
prev.advice <- 200 
prev.TAC <- 646

# Number of simulations
numberOfSims <- 1001
# Note - the number of iterations 10001, up from 1001.
# set to 101 for quick checking
# set to 1001 for routine checking
# set to 10001 preference


##  FORECAST assumptions ####

# Final year in the assessment
final.yr <- max(fit$data$years)

# Resample R from 2004 Rec onwards
R.range <-2004:(final.yr-1)

# Use five year average for mean weights & L-D partition 
av.yrs <- (final.yr-4):(final.yr)

# Use 5 year average selection patter
sel.yrs <- (final.yr-4):(final.yr)

# F status quo is last observed value
f_sq <- fbartable(fit)[as.character(final.yr), "Estimate"]

# F mean over selected years
f_mean <- mean(fbartable(fit)[as.character(sel.yrs),"Estimate"])

# Final assessment year F - needed to run the forecast through next year to get numbers/SSB at start of intermediate year
f_final <- f_sq



## dummy forecast
# 2022F=Fsq then F_MSY - run a dummy forecast to get the SSB at start of TAC year
# RNGkind(sample.kind = "Rounding")
set.seed(1337)
dummy.forecast <- forecast(fit,
                           fscale = c(NA,NA,NA),
                           fval = c(f_final,F_MSY,F_MSY),
                           year.base = final.yr,
                           label = "",
                           rec.years = R.range,
                           ave.years = av.yrs,
                           processNoiseF = FALSE,
                           overwriteSelYears = sel.yrs,
                           splitLD = TRUE,
                           nosim = numberOfSims,
                           savesim = TRUE)

# forecast proper
SSBnext <- attributes(dummy.forecast)$shorttab[3,2]

## HG added:
F_MSY_scaled <- F_MSY*pmin(1,SSBnext/MSY_Btrigger)
F_MSY_lower_scaled <- F_MSY_lower*pmin(1,SSBnext/MSY_Btrigger)
## HG end


attributes(dummy.forecast)$tab # show int year low and high ssb and rec

# scen <- list(
#   "MSY approach: F_MSY" = list(fval = c(f_final, f_sq, F_MSY, F_MSY)),
#   "Precautionary approach: Fpa" = list(fval = c(f_final, f_sq, Fpa, Fpa)),
#   "FMSY upper" = list(fval = c(f_final, f_sq, F_MSY_upper, F_MSY_upper)),
#   "FMSY lower" = list(fval = c(f_final, f_sq, F_MSY_lower, F_MSY_lower)),
#   "F = 0" = list(fval = c(f_final, f_sq, NA, NA), fscale=c(NA,NA,0.0,0.0)),
#   "F = Flim" = list(fval = c(f_final, f_sq, Flim, Flim)),
#   "Fsq"  = list(fval = c(f_final, f_sq, f_sq, f_sq)),
#   "Ftac"  = list(fval = c(f_final, f_sq, NA, NA), catchval = c(NA, NA, prev.TAC, prev.TAC)),
#   # "0.05*Fsq" = list(fval = c(f_final, f_sq, 0.05*f_sq,0.05*f_sq)),
#   # "0.25*Fsq" = list(fval = c(f_final, f_sq, 0.25*f_sq,0.25*f_sq)),
#   # "0.5*Fsq" = list(fval = c(f_final, f_sq, 0.5*f_sq,0.5*f_sq)),
#   # "0.75*Fsq" = list(fval = c(f_final, f_sq, 0.75*f_sq,0.75*f_sq)),
#   "hit Blim" = list(fval = c(f_final, f_sq, NA,NA), nextssb = c(NA, NA, Blim, Blim)),
#   # "hit Bpa" = list(fval = c(f_final, f_sq, NA,NA), nextssb = c(NA, NA, Bpa, Bpa)),
#   "hit MSY Btrigger" = list(fval = c(f_final, f_sq, NA, NA), nextssb = c(NA, NA, MSY_Btrigger, MSY_Btrigger)),
#   "SSB Y+2 = SSB Y+1"  = list(fval = c(f_final, f_sq, NA, NA), nextssb = c(NA, NA, SSBnext, SSBnext))
#   
# )

scen <- list(
  "MSY approach: F_MSY" = list(fval = c(f_final, F_MSY, F_MSY)),
  "Precautionary approach: Fpa" = list(fval = c(f_final, Fpa, Fpa)),
  "FMSY upper" = list(fval = c(f_final,F_MSY_upper, F_MSY_upper)),
  "FMSY lower" = list(fval = c(f_final,  F_MSY_lower, F_MSY_lower)),
  "F = 0" = list(fval = c(f_final,  NA, NA), fscale=c(NA,0.0,0.0)),

  "F = Flim" = list(fval = c(f_final, Flim, Flim)),
  "Fsq"  = list(fval = c(f_final,  f_sq, f_sq)),
  "Ftac"  = list(fval = c(f_final,  NA, NA), catchval = c(NA, prev.TAC, prev.TAC)),
  # "0.05*Fsq" = list(fval = c(f_final, f_sq, 0.05*f_sq,0.05*f_sq)),
  # "0.25*Fsq" = list(fval = c(f_final, f_sq, 0.25*f_sq,0.25*f_sq)),
  # "0.5*Fsq" = list(fval = c(f_final, f_sq, 0.5*f_sq,0.5*f_sq)),
  # "0.75*Fsq" = list(fval = c(f_final, f_sq, 0.75*f_sq,0.75*f_sq)),
  "hit Blim" = list(fval = c(f_final, NA,NA), nextssb = c(NA,  Blim, Blim)),
  # "hit Bpa" = list(fval = c(f_final, f_sq, NA,NA), nextssb = c(NA, NA, Bpa, Bpa)),
  "hit MSY Btrigger" = list(fval = c(f_final,  NA, NA), nextssb = c(NA, MSY_Btrigger, MSY_Btrigger)),
  "SSB Y+2 = SSB Y+1"  = list(fval = c(f_final, NA, NA), nextssb = c(NA, SSBnext, SSBnext)),
  
  # HG added
  "FMSYxSSB/Brig" =    list(fval = c(f_final, F_MSY_scaled, F_MSY) ),
  "FlowerxSSB/Btrig" = list(fval = c(f_final, F_MSY_lower_scaled, F_MSY))
  # HG end
)

# Forecast storage list
FC <- list()
#FC <- vector("list", length(scen))
#names(FC) <- names(scen)

# RUN F scenarios ####
for(i in seq(scen)){
  
  # RNGkind(sample.kind = "Rounding")
  ARGS <- scen[[i]]
  ARGS <- c(ARGS,
            list(fit = fit, 
                 ave.years = av.yrs, 
                 rec.years = R.range, 
                 year.base = final.yr,
                 label = names(scen)[i], 
                 overwriteSelYears = sel.yrs, 
                 splitLD = TRUE,
                 processNoiseF = FALSE,
                 nosim = numberOfSims,
                 savesim = TRUE))  
  set.seed(1337)
  try(FC[[i]] <- do.call(forecast, ARGS))
  
  print(paste0("forecast : ", "'", names(scen)[i], "'", " is complete"))
}


# # FUNCTION: ICES advice rule for MSY ####
# MSYappr <- function(fit, 
#                     Fmsy = NULL, 
#                     Btrig = NULL, 
#                     fscale_init = 1, 
#                     fval_init =NA, 
#                     catchval_init = NA, 
#                     label = NA){
#   
#   fscale <- fscale_init
#   fval <- fval_init
#   catchval <- catchval_init
#   
#   # fscale[2] <- fscale
#   # fval[2] <- fval
#   # catchval[2] <- catchval # pb comment out to remove int year
#   
#   
#   for (i in 2:3){ # pb change from 3:4 to remove int year
#     
#     fscale[i] <- NA
#     fval[i] <- Fmsy
#     catchval[i] <- NA
#     
#     set.seed(1337)
#     f1 <- forecast(fit, fscale = fscale, fval = fval, catchval = catchval, ave.years = av.yrs, rec.years = R.range, overwriteSelYears = sel.yrs)
#     SSB <- attr(f1, "shorttab")[3,i]
#     
#     fval[i] <- min(Fmsy*SSB/Btrig, Fmsy)
#     
#     # a <-  (F_MSY / (Btrig-Blim))
#     # b <- -a*Blim
#     # if(SSB < Btrig && SSB > Blim) fval[i] <- a*SSB + b
#     # if(SSB < Blim) fval[i] <- 0.000001
#   }
#   
#   set.seed(1337)
#   f_final <- forecast(fit, 
#                       fscale = fscale, 
#                       fval = fval, 
#                       catchval = catchval, 
#                       ave.years = av.yrs, 
#                       rec.years = R.range, 
#                       label = label, 
#                       overwriteSelYears = sel.yrs, 
#                       splitLD = TRUE, 
#                       nosim = numberOfSims,
#                       savesim = TRUE)
#   return(f_final)
# }
# 
# 
# # RUN ICES HCR for MSY ####
# FC[[length(FC)+1]] <- MSYappr(fit, Fmsy = F_MSY, Btrig = MSY_Btrigger, fscale_init = NA, fval_init = f_sq, catchval_init = NA, label = paste0(final.yr, "F=Fsq then Fmsy HCR"))
# FC[[length(FC)+1]] <- MSYappr(fit, Fmsy = F_MSY_lower, Btrig = MSY_Btrigger, fscale_init = NA, fval_init = f_sq, catchval_init = NA, label = paste0(final.yr, "F=Fsq then Fmsy HCR lower"))
# # FC[[length(FC)+1]] <- MSYappr(fit, Fmsy = F_MSY_upper, Btrig = MSY_Btrigger, fscale_init = NA, fval_init = f_sq, catchval_init = NA, label = paste0(final.yr, "F=Fsq then Fmsy HCR upper"))



save(FC, file = "./output/forecast.RData")

#### Forecast Outputs ####

# load("./output/forecast.RData")

## Standard forecast plots
# FMSY
jpeg("./report/FMSY_forecast.jpeg", height = 210, width = 210, units = "mm", res = 350)
plot(FC[[1]])
dev.off()

# FSQ
jpeg("./report/FSQ_forecast.jpeg", height = 210, width = 210, units = "mm", res = 350)
plot(FC[[7]])
dev.off()

# HCR
jpeg("./report/HCR_forecast.jpeg", height = 210, width = 210, units = "mm", res = 350)
plot(FC[[12]])
dev.off()


#### Blim risk = 5% brute force option ####
# likely range of catches
catch_range <- c(150:170)

# Create the list of scenarios to trial
scen_blim5 <- lapply(catch_range, function(catch_val) {
  list(
    fval = c(f_final, NA, NA),
    catchval = c(NA, catch_val, catch_val)
  )
})
names(scen_blim5) <- paste0("Catch", catch_range)

FC_blim <- list()

# RUN Blim5 scenarios ####
for(i in seq(scen_blim5)){
  
  # RNGkind(sample.kind = "Rounding")
  ARGS <- scen_blim5[[i]]
  ARGS <- c(ARGS,
            list(fit = fit, 
                 ave.years = av.yrs, 
                 rec.years = R.range, 
                 year.base = final.yr,
                 label = names(scen_blim5)[i], 
                 overwriteSelYears = sel.yrs, 
                 splitLD = TRUE,
                 processNoiseF = FALSE,
                 nosim = numberOfSims,
                 savesim = TRUE))  
  set.seed(1337)
  try(FC_blim[[i]] <- do.call(forecast, ARGS))
  
  print(paste0("forecast : ", "'", names(scen_blim5)[i], "'", " is complete"))
}

cs_blim5 <- data.frame(Basis = NA,
                       catchTACyr = NA,
                       Landings = NA,
                       Discards = NA,
                       Ftotal = NA,
                       Fland = NA,
                       Fdiscard = NA,
                       ssbTACyrplus1 = NA,
                       ssbChange = NA,
                       adviceChange = NA,
                       Risk = NA)

# the 3rd year in the forecast is the advice year because in this stock the last year with data is included in the forecast
# because we have removed the intermediate year I've changed it to the 2nd column in the shorttab
for (i in c(1:length(FC_blim))){
  cs_blim5[i,1] = attributes(FC_blim[[i]])$label
  cs_blim5[i,2] <- attr(FC_blim[[i]],"shorttab")[4,2]
  cs_blim5[i,3] <- attr(FC_blim[[i]],"shorttab")[7,2]
  cs_blim5[i,4] <- attr(FC_blim[[i]],"shorttab")[8,2]
  cs_blim5[i,5] <- attr(FC_blim[[i]],"shorttab")[1,2]
  cs_blim5[i,6] <- attr(FC_blim[[i]],"shorttab")[5,2]
  cs_blim5[i,7] <- attr(FC_blim[[i]],"shorttab")[6,2]
  
  cs_blim5[i,8] <- attr(FC_blim[[i]],"shorttab")[3,3]
  cs_blim5[i,9] <- 100*attr(FC_blim[[i]],"shorttab")[3,3]/SSBnext-100
  cs_blim5[i,10] <- 100*attr(FC_blim[[i]],"shorttab")[4,2]/prev.advice-100
}
cs_blim5[,11] <- 100*unlist(lapply(FC_blim, function(x) sum(x[[3]]$ssb < Blim) / length(x[[3]]$ssb)))

Blim5 <- cs_blim5[which.min(abs(cs_blim5$Risk - 5)), ]
Blim5$Basis <- "Blim 5%"


### Catch table with risk to blim (need to have defined SSBnext, prev.advice and Blim)

cs <- data.frame(Basis = NA,
                 catchTACyr = NA,
                 Landings = NA,
                 Discards = NA,
                 Ftotal = NA,
                 Fland = NA,
                 Fdiscard = NA,
                 ssbTACyrplus1 = NA,
                 ssbChange = NA,
                 adviceChange = NA,
                 Risk = NA)

# the 3rd year in the forecast is the advice year because in this stock the last year with data is included in the forecast
for (i in c(1:length(FC))){
  cs[i,1] = attributes(FC[[i]])$label
  cs[i,2] <- attr(FC[[i]],"shorttab")[4,2]
  cs[i,3] <- attr(FC[[i]],"shorttab")[7,2]
  cs[i,4] <- attr(FC[[i]],"shorttab")[8,2]
  cs[i,5] <- attr(FC[[i]],"shorttab")[1,2]
  cs[i,6] <- attr(FC[[i]],"shorttab")[5,2]
  cs[i,7] <- attr(FC[[i]],"shorttab")[6,2]
  cs[i,8] <- attr(FC[[i]],"shorttab")[3,3]
  cs[i,9] <- 100*attr(FC[[i]],"shorttab")[3,3]/SSBnext-100
  cs[i,10] <- 100*attr(FC[[i]],"shorttab")[4,2]/prev.advice-100
}
cs[,11] <- 100*unlist(lapply(FC, function(x) sum(x[[3]]$ssb < Blim) / length(x[[3]]$ssb)))

cs <- rbind(cs, Blim5)

write.csv(cs, file = "./output/Catch_Option_Table_noRec.csv")


#### Add in Recreational split of advice ####

Rec_catch <- 112
Total_catch <- 1119
Rec_prop <- Rec_catch/Total_catch

Land_prop <- cs$Landings[1]/cs$catchTACyr[1]
Disc_prop <- cs$Discards[1]/cs$catchTACyr[1]


cs_rec <- cs

cs_rec$Rec_Catch <- cs_rec$catchTACyr * Rec_prop
cs_rec$Landings <- (cs_rec$catchTACyr - cs_rec$Rec_Catch) * Land_prop
cs_rec$Discards <- (cs_rec$catchTACyr - cs_rec$Rec_Catch) * Disc_prop

prev_TAC <- 646
####### check this it seems wrong!!!!!!!!!!!! Did it externally last time!
cs_rec$TAC_Change <- 100-((cs$catchTACyr/prev_TAC)*100)

cs_rec2 <- cs_rec %>% select (Basis, catchTACyr, Landings, Discards, Rec_Catch, Ftotal, ssbTACyrplus1, ssbChange, TAC_Change, Risk)

cs_rec2 <- cs_rec2[c(12, 1, 2, 5, 4, 13, 9, 10, 11, 7, 14), ]

write.csv(cs_rec2, file = "./output/Catch_Option_Table_RecSplit_noround.csv")

cs_rec2$Landings <- round(cs_rec2$Landings)
cs_rec2$Discards <- round(cs_rec2$Discards)
cs_rec2$Rec_Catch <- round(cs_rec2$Rec_Catch)
cs_rec2$Ftotal <- icesRound(cs_rec2$Ftotal)
cs_rec2$ssbTACyrplus1 <- round(cs_rec2$ssbTACyrplus1)
cs_rec2$ssbChange <- icesRound(cs_rec2$ssbChange)
cs_rec2$TAC_Change <- icesRound(cs_rec2$TAC_Change)
cs_rec2$Risk <- icesRound(cs_rec2$Risk)

write.csv(cs_rec2, file = "./output/Catch_Option_Table_RecSplit.csv")

#### Contribution of recruitment assumption to catch and SSB ####
# FMSY approach
lapply(FC, function(x){return(attributes(x)$label)})
i <- 12
attr(FC[[i]], "shorttab")

# attributes(FC[[i]])$label
FC[[i]][[2]]$year
catch <- apply(FC[[i]][[2]]$catchatage, 1, median) * colMeans(tail(fit$data$catchMeanWeight,1))      #catchn
FC[[i]][[3]]$year
#SSB <- exp(apply(FC[[i]][[3]]$sim, 2, median)) [1:8]  * colMeans(tail(fit$data$catchMeanWeight * fit$data$propMat[-dim(fit$data$propMat)[1],],1))   #stockn


SSB <- exp(apply(FC[[i]][[3]]$sim, 2, median) [1:7])  * colMeans(tail(fit$data$catchMeanWeight[,,] * fit$data$propMat,1))   #stockn # pb edit


# land <- apply(FC[[i]][[2]]$catchatage, 1, median)*colMeans(tail(fit$data$landMeanWeight,1))*colMeans(tail(fit$data$landFrac,1))
# dis <- apply(FC[[i]][[2]]$catchatage, 1, median)*colMeans(tail(fit$data$disMeanWeight,1))*(1-colMeans(tail(fit$data$landFrac,1)))


year <- 2026
plot.data <- rbind(
  data.frame(recruitment = year + 1:-5, val = catch[,1], type = paste(year+1, "Catch")),
  data.frame(recruitment = year + 1:-5, val = SSB, type = paste(year+2, "SSB"))
)

plot.data$est <- plot.data$recruitment >= year

ggplot(plot.data, aes(x=factor(recruitment), y=val, fill=est)) +
  facet_wrap(~type, scales="free_y") + geom_col() +
  xlab("Recruitment year") +ylab("Contribution (tonnes)") +
  scale_fill_discrete(name="Forecast assumption")+ 
  theme_bw()+
  ggtitle("Forecast contribution") +
  theme(legend.position = "none")

ggsave("report/Forecast makeup.png",
       width = 20, height = 15, units = "cm", dpi = 300, type = "cairo-png")

# % contributions to SSB
rev(plot.data$val[plot.data$type == "2027 SSB"])[6]/sum(rev(plot.data$val[plot.data$type == "2027 SSB"]))
rev(plot.data$val[plot.data$type == "2027 SSB"])[7]/sum(rev(plot.data$val[plot.data$type == "2027 SSB"]))

# % contributions to Catch
rev(plot.data$val[plot.data$type == "2026 Catch"])[6]/sum(rev(plot.data$val[plot.data$type == "2026 Catch"]))
rev(plot.data$val[plot.data$type == "2026 Catch"])[7]/sum(rev(plot.data$val[plot.data$type == "2026 Catch"]))


#### catch at age plot ####

cn_fit <- as.matrix(caytable(fit))
cw_fit <- fit$data$catchMeanWeight[, , "Residual catch"]

common_years <- intersect(rownames(cn_fit), rownames(cw_fit))
common_ages  <- intersect(colnames(cn_fit), colnames(cw_fit))

cn_fit <- cn_fit[common_years, common_ages, drop = FALSE]
cw_fit <- cw_fit[common_years, common_ages, drop = FALSE]

caa_t <- cn_fit * cw_fit


caa2 <- as.data.frame(caa_t) %>%
  tibble::rownames_to_column("Year") %>%
  pivot_longer(
    cols = -Year,
    names_to = "Age",
    values_to = "Catch")

ggplot(caa2 %>% filter (Year >2015), aes(x = Year, y = Catch, fill = Age)) +
  geom_col() +
  scale_fill_manual(values = rainbow(ncol(caa_t))) +
  labs(
    x = "Year",
    y = "Tonnes",
    title = "Catch-at-age through time",
    fill = "Age"
  ) +
  theme_bw()
ggsave("report/Catch_at_age_last10.png",
       width = 20, height = 15, units = "cm", dpi = 300, type = "cairo-png")

ggplot(caa2, aes(x = Year, y = Catch, fill = Age)) +
  geom_col() +
  scale_fill_manual(values = rainbow(ncol(caa_t))) +
  labs(
    x = "Year",
    y = "Tonnes",
    title = "Catch-at-age through time",
    fill = "Age"
  ) +
  theme_bw()
ggsave("report/Catch_at_age.png",
       width = 20, height = 15, units = "cm", dpi = 300, type = "cairo-png")

#### HCR forecast values  ####
attr(FC[[12]],"shorttab")
attr(FC[[4]],"shorttab")
FC[[12]]
