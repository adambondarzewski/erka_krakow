DT_scores_mean_F <- DT_melted[Gender == 'F', .(avg_score_mean_F = mean(avgScore), females_count = .N), by = SubGroup1]
DT_scores_mean_M <- DT_melted[Gender == 'M', .(avg_score_mean_M = mean(avgScore), males_count = .N), by = SubGroup1]

DT_scores_merged <- merge(DT_scores_mean_F, DT_scores_mean_M, by = c('SubGroup1'), all = TRUE)


library(ggplot2)

DT_scores_rest <- DT_melted[, .(
                                    avgScore = mean(avgScore, na.rm = TRUE)
                                  , mean_duration_person = mean(mean_duration_person, na.rm = TRUE)
                                )
                                , .(SubGroup1)]

DT_scores_final <- merge(DT_scores_merged, DT_scores_rest, by = 'SubGroup1')

# ggplot(aes(x = Band, ), data = DT_melted)