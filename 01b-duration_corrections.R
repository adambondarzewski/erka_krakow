# creating model
model1<-lm(data=DT,mean_duration_person~Band+Gender +AgeYears)

#correction

# correcting single durations
duration_correction <- model1$coefficients['Band']

cols <- c(paste0('Du0', 1:9), paste0('Du', 10:32))

DT_corrected_durations <- DT[, .SD[, cols, with = FALSE] + duration_correction * Band]

setnames(DT_corrected_durations, old = cols, new = paste0('corrected_', cols))
DT <- cbind(DT, DT_corrected_durations)

# correcting mean durations
DT[, corrected_mean_duration_person := mean_duration_person +  duration_correction * Band]

# cols_renaming <- c(paste0('Du0', 1:9), paste0('Du', 10:32))
# 
# names_durations <- c(rbind(cols, paste0('corrected_', cols_renaming)))
# 
# cols_rest <- c(paste0('An0', 1:9), paste0('An', 10:32), c(paste0('Sc0', 1:9)), paste0('Sc', 10:32))
# 
# setcolorder(DT, neworder = c(names(DT)[1:23], names_durations, cols_rest))
