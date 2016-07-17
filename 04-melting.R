id_vars <- c('Name', 'AgeYears', 'AgeMonths', 'Gender', 'ClientId'
             , 'ClientCode', 'RegionId', 'Region'
             , 'InstitutionId', 'Institution', 'GroupId'
             , 'Group', 'SubGroupId1', 'SubGroupId2'
             , 'SubGroup1', 'SubGroup2', 'TestResultId'
             , 'DateCompleted', 'DaysSince', 'Score', 'Duration', 'QuestionsCompleted')

names_answers <- names(DT)[(names(DT) %in% id_vars) | (names(DT) %like% 'An')]
DT_answers <- DT[, names_answers, with = FALSE]
DT_answers_melted <- melt(DT_answers, id.vars = id_vars)
setnames(DT_answers_melted, c('variable', 'value'), c('Question_id', 'Answer_question'))
DT_answers_melted[, Question_id := paste0('Question_', substr(Question_id, start = 3, stop = 4))]

names_answers <- names(DT)[(names(DT) %in% id_vars) | (names(DT) %like% 'Du')]
DT_durations <- DT[, names_answers, with = FALSE]
DT_durations_melted <- melt(DT_durations, id.vars = id_vars)
setnames(DT_durations_melted, c('variable', 'value'), c('Question_id', 'Duration_question'))
DT_durations_melted[, Question_id := paste0('Question_', substr(Question_id, start = 3, stop = 4))]

names_answers <- names(DT)[(names(DT) %in% id_vars) | (names(DT) %like% 'Sc')]
DT_scores <- DT[, names_answers, with = FALSE]
DT_scores_melted <- melt(DT_scores, id.vars = id_vars)
setnames(DT_scores_melted, c('variable', 'value'), c('Question_id', 'Value_question'))
DT_scores_melted[, Question_id := paste0('Question_', substr(Question_id, start = 3, stop = 4))]

DT_melted <- merge(merge(DT_answers_melted, DT_durations_melted, by = c(id_vars, 'Question_id')),
                   DT_scores_melted, by = c(id_vars, 'Question_id'))

# use

library(dplyr)

DT_melted[, Score_answers := Score/QuestionsCompleted]
DT_melted[, Score_groups := cut(Score_answers, breaks = c(seq(0, 1, by = .1)))]

arrange(DT_melted[, .(duration = mean(Duration_question, na.rm = TRUE)), by = Score_groups], Score_groups)
