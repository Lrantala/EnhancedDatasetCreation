library("data.table")
library("stringr")
library("textreuse")
library("tm")
library("dplyr")

rm(list = ls())
gc()
data_path <- "./data/"

#m2017satd <- as.data.frame(data.table::fread(file = "./data/technical_debt_dataset.csv", sep = ",", header = TRUE))
m2017satd <- as.data.frame(data.table::fread(file = "./data/cleaned_technical_debt_dataset.csv", sep = ",", header = TRUE))

#Count how many commenta maldonado has which are empty
# Total of 3,946 comments
m2017empty_comments <- m2017satd[(m2017satd$clean_comment == ""),]
table(m2017empty_comments$projectname)

m2017satd <- m2017satd[!(m2017satd$clean_comment == ""),]

test <- m2017satd[m2017satd$clean_comment == "String String",]
test <- m2017satd[m2017satd$clean_comment == "We assume that every output jikes does stands for an error warning XXX Is this correct",]
table(m2017satd$classification)/62275*100
4071/62275

#soccminer10 <- as.data.frame(data.table::fread(file = "./data/soccminer_10_projects.csv", sep = ",", header = TRUE))
#soccminer10 <- as.data.frame(data.table::fread(file = "./data/cleaned_soccminer_10_projects_all_data.csv", sep = ",", header = TRUE))
soccminer10 <- as.data.frame(data.table::fread(file = "./data/cleaned_soccminer_10_projects_v2_comment_file_project.csv", sep = ",", header = TRUE))

soccminer10 <- soccminer10[,c("comment_key", "Serialized_Project_Name", "Comment_Content", "Clean_Comment_Content")]
# Count how many empty columns there are in Soccminer
soccminer10 <- soccminer10[!(soccminer10$Clean_Comment_Content == ""),]
names(soccminer10)[names(soccminer10) == "Serialized_Project_Name"] <- "Project"

#Making sure that the projectnames match between dataframes by renaming hem
m2017satd$projectname[m2017satd$projectname == "argouml"] <- "argouml-VERSION_0_34"
soccminer10$Project[soccminer10$Project == "jEdit"] <- "jEdit-4.2"

m2017satd$maldo_key <- 1:nrow(m2017satd)
m2017satd$maldo_key <- as.character(m2017satd$maldo_key)
table(soccminer10$Project)

# Done: 
# Jmeter
# +Ant
# ArgoUML
# Columba
# EMF
# Hibernate
# jEdit
# jFreechart
# jruby
# Squirrel

#Taking the projects one by one
# Removing duplicate comments
# Remove empty comments
project_name = "emf-2.4.1"
mjedit <- m2017satd[m2017satd$projectname == project_name,]
#mjedit <- mjedit[!duplicated(mjedit),]
mjedit <- mjedit[!(mjedit$clean_comment == ""),]

sjedit <- soccminer10[soccminer10$Project == project_name,]
#sjedit <- sjedit[!duplicated(sjedit[c("Comment_Content")]),]
sjedit <- sjedit[!(sjedit$Clean_Comment_Content == ""),]

# Finding the direct matches based on solely on comment content
#direct_matches <- inner_join(sjedit, mjedit, by=c("Clean_Comment_Content"="clean_comment"))
direct_matches <- inner_join(sjedit, mjedit, by=c("Comment_Content"="commenttext"))
direct_matches$score <- 1
direct_matches$commenttext <- direct_matches$Comment_Content

direct_matches <- direct_matches[!duplicated(direct_matches[c('commenttext', 'comment_key')]), ]

direct_matches <- direct_matches[order(direct_matches$comment_key, decreasing = TRUE), ]
direct_matches <- direct_matches[!duplicated(direct_matches[c('commenttext', 'maldo_key')]), ]

#sjedit[sjedit$Comment_Content== "// Temporary - until we figure a better API",]
#soccminer10[str_detect(soccminer10$Comment_Content, "// Temporary - until we figure a better API"),]
#mjedit[mjedit$commenttext== "// Temporary - until we figure a better API",]
#m2017satd[m2017satd$commenttext== "// Use this to prepend a message to the properties file",]

# Delete the duplicate comments
#direct_matches <- direct_matches[!duplicated(direct_matches$Clean_Comment_Content),]

direct_matches_socc <- direct_matches$comment_key[!duplicated(direct_matches$comment_key)]
direct_matches_maldo <- direct_matches$maldo_key[!duplicated(direct_matches$maldo_key)]

# Create the Maldonado comment (as this is needed later)
#direct_matches$clean_comment = direct_matches$Clean_Comment_Content
direct_matches_pairs_temp <- direct_matches[,c("comment_key", "maldo_key", "score")]
direct_matches_pairs <- unique(direct_matches_pairs_temp[,("comment_key")])
direct_matches_pairs2 <- unique(direct_matches_pairs_temp$maldo_key)
direct_matches_pairs <- as.data.frame(cbind(direct_matches_pairs, unique(direct_matches_pairs_temp$maldo_key)))

direct_matches_pairs <- direct_matches_pairs[!duplicated(direct_matches_pairs[,c("comment_key", "maldo_key")]),]

direct_matches_pairs %>% group_by(comment_key, maldo_key) %>% filter(n()>1)

project <- project[!duplicated(project[c('commenttext', 'match')]), ]
project <- project[order(project$match, decreasing = TRUE), ]
project <- project[!duplicated(project[c('commenttext')]), ]

test_df <- data.frame(a=c(1,1,1,2,2,2,3,3,3), b = c(7,8,9,7,8,9,7,8,9))
test_df <- test_df[unique(test_df$a & test_df$b),]
test_df2 <- unique(test_df$a)
test_df2 <- as.data.frame(cbind(test_df2, unique(test_df$b)))

test_df <- as.data.table(test_df)
data.frame()
test_df[, .SD[!duplicated(a)], by="b"]

test_df[!(duplicated(test_df$b)),]
test_df[!intersect(test_df$a, test_df$b),]

test_df %>% group_by(a) %>% slice()

direct_matches_pairs <- direct_matches_pairs[!duplicated(direct_matches_pairs['maldo_key']), ]
direct_matches_pairs <- direct_matches_pairs[order(direct_matches_pairs$comment_key, decreasing = TRUE), ]

direct_matches_pairs <- direct_matches_pairs %>% distinct(comment_key, maldo_key)
?intersect

direct_matches_pairs[direct_matches_pairs$maldo_key == "5636",]
# Project: all / no duplicates
# Ant: 2,510 / 2,426
# JMeter: 2,707 / 2,420
# ArgoUML: 3,685 / 3,444
# Columba: 3,406 / 3,104
# EMF: 2,124 / 2,048
# Hibernate: 2,127 / 1,978
# JEdit: 3,097 / 2,936
# JFreeChart: 2,471 / 2,280
# JRuby: 3,018 / 2,941
# SQL: 3,361 / 3,176
# Total: 28,506 / 26,753

# Keep only the columns which are kept in the end
#direct_matches <- direct_matches[,c("score", "projectname", "classification", "commenttext", "clean_comment", "Clean_Comment_Content",
                                    #"Comment_Content")]

# Removing the direct matches from the data
#mjedit <- mjedit[!(mjedit$clean_comment %in% direct_matches$Clean_Comment_Content),]
#sjedit <- sjedit[!(sjedit$Clean_Comment_Content %in% direct_matches$Clean_Comment_Content),]
mjedit <- mjedit[!(mjedit$maldo_key %in% direct_matches_maldo),]
sjedit <- sjedit[!(sjedit$comment_key %in% direct_matches_socc),]

#sjedit$Comment_Content <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", sjedit$Comment_Content)
#mjedit$commenttext <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", mjedit$commenttext)
#library(dplyr)
#install.packages("fuzzyjoin")
#library(fuzzyjoin)
#df_out = distinct(
#  rbind(
#    regex_inner_join(mjedit, sjedit, by = c("commenttext" = "Comment_Content"))
#  )
#)
#df_out
# 1. Start of the minhash-processing and matching
# Creating minhash-generator and corpus
minhash <- minhash_generator(200, seed = 16667)
total_corpus <- TextReuseCorpus(text = c(mjedit$clean_comment, sjedit$Clean_Comment_Content), 
                                #meta = list(description = "Maldonado"),
                                tokenizer = tokenize_words,
                                minhash_func = minhash,
                                skip_short = FALSE)
#saveRDS(total_corpus, file = paste0("./save/corpus/",project_name, "_corpus_no_dup.rds"))
#total_corpus <- readRDS(file = "jEdit_corpus_no_dup_dirmatch.rds")
buckets <- lsh(total_corpus, bands = 50, progress = TRUE)
#saveRDS(buckets, file = paste0(project_name, "_buckets_no_dup.rds"))

candidates <- lsh_candidates(buckets)
#saveRDS(candidates, file = "jEdit_candidates_no_dup_dirmatch.rds")
#candidates <- readRDS(file = "jEdit_candidates_no_dup_dirmatch.rds")
#Removing useless data from memory

scores <- lsh_compare(candidates, total_corpus, jaccard_similarity, progress = TRUE)
#rm(total_corpus)
gc()
#saveRDS(scores, file = paste0(project_name, "_scores_no_dup.rds"))
saveRDS(scores, file = paste0(project_name, "_scores_w_dup_v2.rds"))
#scores <- readRDS(file = paste0(project_name, "_scores_w_dup_v2.rds"))

# 1.2. Matching only the comments that are present in Maldonado and in Soccminer

#Removing the doc- from score-names to get the row numbers
scores$anum <- gsub("^doc-*", "", scores$a)
scores$bnum <- gsub("^doc-*", "", scores$b)

# Resetting the row numbers
# We always put Maldonado data first in the corpus, 
# so that the doc-1 matches row 1 from Maldonado and so on.
rownames(mjedit) <- NULL
mjedit$rowid <- rownames(mjedit)

# Soccminer is always placed after Maldonado into the corpus,
# so the first doc-number is number of rows in Maldonado plus 1
rownames(sjedit) <- (nrow(mjedit)+1):(nrow(mjedit) + nrow(sjedit))
sjedit$rowid <- rownames(sjedit)

# Exclude all the pairs, which have rowids either only in Maldonado or Soccminer
# We do not wish to match these within the datasets, only with each other.
scores$maldo <- ifelse(scores$anum %in% mjedit$rowid & scores$bnum %in% mjedit$rowid, NA, "OK")
scores <- scores[!is.na(scores$maldo),]
scores$socc <- ifelse(scores$anum %in% sjedit$rowid & scores$bnum %in% sjedit$rowid, NA, "OK")
scores <- scores[!is.na(scores$socc),]
scores <- scores %>% select(-maldo, -socc, -a, -b)

# Join the rows which match maldonado
scores <- left_join(scores, mjedit, by=c("anum"="rowid"))
scores <- left_join(scores, mjedit, by=c("bnum"="rowid"))
scores <- scores %>% 
    mutate(projectname = coalesce(projectname.x, projectname.y)) %>% 
    select(-projectname.x, -projectname.y) %>%
    mutate(classification = coalesce(classification.x, classification.y)) %>%
    select(-classification.x, -classification.y) %>%
    mutate(commenttext = coalesce(commenttext.x, commenttext.y)) %>%
    select(-commenttext.x, -commenttext.y) %>%
    mutate(clean_comment = coalesce(clean_comment.x,clean_comment.y)) %>% 
    select(-clean_comment.x,-clean_comment.y) %>%
    mutate(maldo_key = coalesce(maldo_key.x, maldo_key.y)) %>%
    select(-maldo_key.x, -maldo_key.y)

#Join the rows which match soccminer
scores <- left_join(scores, sjedit[,c("Clean_Comment_Content", "Comment_Content", "rowid", "comment_key")], by=c("anum"="rowid"))
scores <- left_join(scores, sjedit[,c("Clean_Comment_Content", "Comment_Content", "rowid", "comment_key")], by=c("bnum"="rowid"))
scores <- scores %>% mutate(Clean_Comment_Content = coalesce(Clean_Comment_Content.x,Clean_Comment_Content.y)) %>% 
  select(-Clean_Comment_Content.x,-Clean_Comment_Content.y) %>%
  mutate(Comment_Content = coalesce(Comment_Content.x, Comment_Content.y)) %>% 
  select(-Comment_Content.x,-Comment_Content.y) %>%
  mutate(comment_key = coalesce(comment_key.x, comment_key.y)) %>%
  select(-comment_key.x, -comment_key.y)

# Keep only the highest score matches for each comment pair
# since there is a possibility that one comment was matched with several possibilities.
scores <- scores %>%
  group_by(anum) %>%
  slice(which.max(score))

# Remove the rows, not anymore needed. Every comment which is similar is thought
# to have the same SATD classification.
scores <- scores %>% ungroup() %>% 
  select(-anum, -bnum)

scores <- full_join(direct_matches, scores)

scores$match <- ifelse(scores$score > 0.9, 1, "")
# If there are comments where one has // and the other one doesn't, mark
# it as a non-match
scores$match <- ifelse(scores$match == 1, 1, ifelse((grepl("//", scores$commenttext) & grepl("//", scores$Comment_Content)), "", 0))


#Write the direct matches and scores to a file
scores <- scores[,c("Project", 
                     "comment_key", 
                     "maldo_key", 
                     "classification", 
                     "Comment_Content",
                     "commenttext",
                     "Clean_Comment_Content",
                     "clean_comment",
                     "score",
                     "match")]
scores$Project <- project_name
write.csv2(scores, file = paste0("./save2/csv/", project_name, "_v6.csv"), row.names = FALSE)
table(scores$classification[!duplicated(scores$maldo_key)])
table(m2017satd$classification[m2017satd$projectname == project_name])

#maldo_project <- m2017satd[m2017satd$projectname == project_name,]
#maldo_missing <- maldo_project[!(maldo_project$maldo_key %in% scores$maldo_key),]

### This was to count the annotator agreement
###READ the labeled data
library("psych")

leevi <- read.csv2(file = ".\\data\\apache-jmeter-2.10_no_dup_scores_v2.csv")
murali <- read.csv2(file = ".\\data\\apache-jmeter-2.10_no_dup_scores_v2_annotated.csv")

leeviscore <- leevi$Match[leevi$score >= 0.2 & leevi$score <= 0.9]
muraliscore <- murali$Match[murali$score >= 0.2 & murali$score <= 0.9] 

raterkappa <- cohen.kappa(x = cbind(leeviscore, muraliscore))###
nrow(scores[scores$score < 0.7,])
###
# Counting the remaining comments where we had a match per project and SATD type
###
matches_all_df <- list()
matches_list <- list.files(path=".\\matches\\", pattern = "\\.csv$")
matches_list_path <- lapply(matches_list, function(x) paste0(".\\matches\\", x))
matches_all_df <- lapply(matches_list_path, function(x) read.csv2(file = x, stringsAsFactors = FALSE, strip.white = TRUE))
names(matches_all_df) <- gsub("\\.csv$", "", matches_list)
# List of lists into one df
matches_df <- as.data.frame(do.call(rbind, matches_all_df))
rownames(matches_df) <- NULL
colnames(matches_df)[colnames(matches_df) == "projectname"] <- "Project"
matches_df <- matches_df %>% select(-score)
# Keep only the rows where Match == 1
matches_df <- matches_df[matches_df$match == 1,]
# Joining and creating the final enhanced dataset
soccminer10_1 <- left_join(soccminer10, matches_df)
soccminer10_1 <- soccminer10_1[!is.na(soccminer10_1$classification),]
soccminer10_1 <- soccminer10_1[,c("Project",
                                  "Comment_Source_File",
                                  "File_LOC",
                                  "Class_Name",
                                  "Class_LOC",
                                  "Class_Signature",
                                  "Class_Nested_Level",
                                  "Class_Specifier",
                                  "Class_Type",
                                  "Method_Name",
                                  "Method_LOC",
                                  "Method_Signature",
                                  "Method_Specifier",
                                  "Method_Type",
                                  "Method_Category",
                                  "Comment_Content",
                                  "commenttext",
                                  "classification",
                                  "Comment_Immediate_Preceding_Code",
                                  "Comment_Immediate_Succeeding_Code",
                                  "Rule",
                                  "Rule_Set",
                                  "Priority",
                                  "Description",
                                  "Begin_Line",
                                  "End_Line",
                                  "Comment_Line_No",
                                  "Class_Line_No",
                                  "class.Class_Line_No + class.Class_LOC",
                                  "Method_Line_No",
                                  "method.Method_Line_No + method.Method_LOC")]
names(soccminer10_1)[names(soccminer10_1) == "commenttext"] <- "Original_Comment_Content"
names(soccminer10_1)[names(soccminer10_1) == "Comment_Content"] <- "SoCCMiner_Comment_Content"
names(soccminer10_1)[names(soccminer10_1) == "classification"] <- "SATD_Category"
names(soccminer10_1)[names(soccminer10_1) == "Class_Line_No"] <- "Class_StartLine"
names(soccminer10_1)[names(soccminer10_1) == "Class_Line_No"] <- "Class_StartLine"
names(soccminer10_1)[names(soccminer10_1) == "class.Class_Line_No + class.Class_LOC"] <- "Class_EndLine"
names(soccminer10_1)[names(soccminer10_1) == "Method_Line_No"] <- "Method_StartLine"
names(soccminer10_1)[names(soccminer10_1) == "method.Method_Line_No + method.Method_LOC"] <- "Method_EndLine"
names(soccminer10_1)[names(soccminer10_1) == "Priority"] <- "Rule_Priority"
names(soccminer10_1)[names(soccminer10_1) == "Description"] <- "Rule_Description"
names(soccminer10_1)[names(soccminer10_1) == "Begin_Line"] <- "Rule_Begin_Line"
names(soccminer10_1)[names(soccminer10_1) == "End_Line"] <- "Rule_End_Line"

write.csv2(soccminer10_1, file = "./save/final_replication_dataset_v2.csv")
# Counting the number of comments remaining in the dataset
# by removing PMD-data, which creates duplicates of comments due to different
# rules attached to a single comment
soccminer10_count <- soccminer10_1 %>% select(-Rule, -Rule_Set, -Priority, -Description, -Begin_Line, -End_Line)

# This creates the table with the different SATD types per project Table 1
soccminer10_count <- soccminer10_count[!duplicated(soccminer10_count),]
table(soccminer10_count$Project, soccminer10_count$classification)
table(soccminer10_count$classification)

# Our percentage, 7.41% of SATD
(414+1966+15+695+51)/nrow(soccminer10_count)*100
# Original percentage 6.54% of SATD
(472+2703+54+757+85)/62275*100

# How much of original SATD we capture? 77.16%
(414+1966+15+695+51)/(472+2703+54+757+85)*100
###

# Count the number of files, classes, methods, SATD comments and non-comments, rule violations and avg. severity
soccminer10_count %>% group_by(Project) %>% summarise(n_distinct(Comment_Source_File))
#Different files might have classes with same names, wo we need to group them
soccminer10_count %>% group_by(Project) %>% summarise(n_distinct(Comment_Source_File, Class_Name))
#Files and classes might have methods with similar names, again we group them
soccminer10_count %>% group_by(Project) %>% summarise(n_distinct(Comment_Source_File, Class_Name, Method_Name))

# Counting the number of Rule violations per project
ruledf <- soccminer10_1 %>% group_by(Project, Rule) %>% summarise(Freq=n())
aggregate(ruledf$Freq, by=list(Project=ruledf$Project), FUN=sum)

# Counting the average priority for rule violations
(2.99+2.93+3.00+3.00+2.98+2.95+3.01+2.98+2.98+3.00)/10

# Counting the different priority counts
ruledf <- soccminer10_1 %>% group_by(Project, Priority) %>% summarise(Freq=n())
aggregate(ruledf$Freq, by=list(Priority=ruledf$Priority), FUN=sum)

# Example of usage from ant
ant_count <- soccminer10_count[soccminer10_count$Project == "apache-ant-1.7.0",]
ant_count %>% summarise(n_distinct(Comment_Source_File, Class_Name, Method_Name))

ant_count_with <- ant_count[ant_count$classification != "WITHOUT_CLASSIFICATION",]
ant_count_without <- ant_count[ant_count$classification == "WITHOUT_CLASSIFICATION",]
# Files counts
ant_count_with %>% summarise(n_distinct(Comment_Source_File))
ant_count_without_2 <- anti_join(ant_count_without, ant_count_with, by = c("Comment_Source_File"))
ant_count_without_2 %>% summarise(n_distinct(Comment_Source_File))
# Class counts
ant_count_with %>% summarise(n_distinct(Comment_Source_File, Class_Name))
ant_count_without_2 <- anti_join(ant_count_without, ant_count_with, by = c("Comment_Source_File", "Class_Name"))
ant_count_without_2 %>% summarise(n_distinct(Comment_Source_File, Class_Name))

# Methods counts
ant_count_with %>% summarise(n_distinct(Comment_Source_File, Class_Name, Method_Name))

ant_count_without <- ant_count[ant_count$classification == "WITHOUT_CLASSIFICATION",]
ant_count_without <- anti_join(ant_count_without, ant_count_with, by = c("Class_Name", "Comment_Source_File", "Method_Name"))
ant_count_without %>% summarise(n_distinct(Comment_Source_File, Class_Name, Method_Name))

# Paired t-test to determine difference between satd methods and pmd results
# again with ANT
ant_count <- soccminer10_1[soccminer10_1$Project == "apache-ant-1.7.0",]
ant_count_with <- ant_count[ant_count$classification != "WITHOUT_CLASSIFICATION",]
jmeter_count <- soccminer10_1[soccminer10_1$Project == "apache-jmeter-2.10",]
jmeter_count_with <- jmeter_count[jmeter_count$classification != "WITHOUT_CLASSIFICATION",]
squirrel_count <- soccminer10_1[soccminer10_1$Project == "sql12",]
squirrel_count_with <- squirrel_count[squirrel_count$classification != "WITHOUT_CLASSIFICATION",]


wilcox.test(x = ant_count_with$Priority, y = jmeter_count_with$Priority, paired = FALSE)
wilcox.test(x = ant_count_with$Priority, y = squirrel_count_with$Priority, paired = FALSE)
wilcox.test(x = jmeter_count_with$Priority, y = squirrel_count_with$Priority, paired = FALSE)


###################
###################

library("data.table")
library("stringr")
library("textreuse")
library("tm")
library("dplyr")

rm(list = ls())
gc()
data_path <- "./data/"


# First we load all the data we did with the methods and look for the direct matches
m2017satd <- as.data.frame(data.table::fread(file = "./data/cleaned_technical_debt_dataset.csv", sep = ",", header = TRUE,
                                             strip.white = TRUE))
m2017empty_comments <- m2017satd[(m2017satd$clean_comment == ""),]
table(m2017empty_comments$projectname)

# Give each maldonado_comment_unique_identifier_number
table(m2017satd$classification)
#m2017satd$id <- 1:(nrow(m2017satd))

# This is now the new data, only with comments and project names
soccminer10 <- as.data.frame(data.table::fread(file = "./data/cleaned_soccminer_10_projects_v3.csv", sep = ",", header = TRUE,
                                               strip.white = TRUE))

# Count how many empty columns there are in Soccminer
soccminer10 <- soccminer10[!(soccminer10$Clean_Comment_Content == ""),]


# Secondly, we filter out the matches we had from them from the complete comment dataset
# These are all the matches and their classifications, all of these cane be removed beforehand
#matches_all_df <- list()
#matches_list <- list.files(path=".\\matches\\", pattern = "\\.csv$")
#matches_list_path <- lapply(matches_list, function(x) paste0(".\\matches\\", x))
#matches_all_df <- lapply(matches_list_path, function(x) read.csv2(file = x, stringsAsFactors = FALSE, strip.white = TRUE))
#names(matches_all_df) <- gsub("\\.csv$", "", matches_list)
# List of lists into one df
#matches_df <- as.data.frame(do.call(rbind, matches_all_df))
#rownames(matches_df) <- NULL
#colnames(matches_df)[colnames(matches_df) == "projectname"] <- "Project"
#matches_df <- matches_df %>% select(-score)
# And remove the pairs without 1 match, as the other match might still be out there.
#matches_df <- matches_df[matches_df$match == 1,]

# Just leave the comment and projects
#matches_df_comments_projects <- matches_df[,c("Project", "Comment_Content")]
# Weeding out all the comments we did already earlier from the dataset
#soccminer10 <- anti_join(soccminer10, matches_df_comments_projects, 
#                         by=c('Serialized_Project_Name' = 'Project', 
#                              'Comment_Content' = 'Comment_Content'))

names(soccminer10)[names(soccminer10) == "Serialized_Project_Name"] <- "Project"

#Making sure that the projectnames match between dataframes by renaming hem
m2017satd$projectname[m2017satd$projectname == "argouml"] <- "argouml-VERSION_0_34"
soccminer10$Project[soccminer10$Project == "jEdit"] <- "jEdit-4.2"

table(m2017satd$projectname, m2017satd$classification)
table(soccminer10$Project)

#Taking the projects one by one
# Removing duplicate comments
# Remove empty comments
project_name = "sql12"
mjedit <- m2017satd[m2017satd$projectname == project_name,]

# taking only the ones labeled as satd
mjedit <- mjedit[mjedit$classification != "WITHOUT_CLASSIFICATION",]
#mjedit <- mjedit[!duplicated(mjedit),]
mjedit <- mjedit[!(mjedit$clean_comment == ""),]
rownames(mjedit) <- NULL
mjedit$mrowid <- rownames(mjedit)

sjedit <- soccminer10[soccminer10$Project == project_name,]
#sjedit <- sjedit[!duplicated(sjedit[c("Comment_Content")]),]
sjedit <- sjedit[!(sjedit$Clean_Comment_Content == ""),]

rownames(sjedit) <- NULL
sjedit$srowid <- rownames(sjedit)

# Taking out the duplicates from both sets, based on the raw
# comment. We can match these later back, but they are not needed for classification.
mjedit_dupl <- mjedit[duplicated(mjedit$commenttext)|duplicated(mjedit$commenttext, fromLast = TRUE),]
mjedit <- mjedit[!(mjedit$mrowid %in% mjedit_dupl$mrowid),]

sjedit_dupl <- sjedit[duplicated(sjedit$Comment_Content)|duplicated(sjedit$Comment_Content, fromLast = TRUE),]
sjedit <- sjedit[!(sjedit$srowid %in% sjedit_dupl$srowid),]

# Take out one comment from each duplicated group. This is inserted back to the df from
# which the corpus is being built. The rest are kept for posterity.

#maldo
mjedit_dupl_1_each_group <- mjedit_dupl %>% group_by(commenttext) %>% top_n(1, mrowid) %>% ungroup()
#mjedit_dupl <- mjedit_dupl[!(mjedit_dupl$mrowid %in% mjedit_dupl_1_each_group$mrowid),]

#mjedit_dupl <- mjedit_dupl %>% group_by(commenttext) %>%
#  summarise(mrowid = paste(mrowid, collapse=","))

mjedit <- full_join(mjedit, mjedit_dupl_1_each_group)
rownames(mjedit) <- NULL
mjedit$corpusid <- rownames(mjedit)

#soccminer
sjedit_dupl_1_each_group <- sjedit_dupl %>% group_by(Comment_Content) %>% top_n(1, srowid) %>% ungroup()
#sjedit_dupl <- sjedit_dupl[!(sjedit_dupl$srowid %in% sjedit_dupl_1_each_group$srowid),]

#sjedit_dupl <- sjedit_dupl %>% group_by(Comment_Content) %>%
#  summarise(srowid = paste(srowid, collapse=","))

sjedit <- full_join(sjedit, sjedit_dupl_1_each_group)
rownames(sjedit) <- NULL
sjedit$corpusid <- rownames(sjedit)



# Finding the direct matches based on solely on clean comment content
direct_matches <- inner_join(sjedit, mjedit, by=c("Comment_Content"="commenttext"))
direct_matches$score <- 1
direct_matches$match <- 1

# Create the Maldonado comment (as this is needed later)
direct_matches$clean_comment = direct_matches$Clean_Comment_Content

# Removing the direct matches from the data
mjedit_no_match <- mjedit[!(mjedit$mrowid %in% direct_matches$mrowid),]
sjedit_no_match <- sjedit[!(sjedit$srowid %in% direct_matches$srowid),]

# Possible_direct_matches which match on cleaned comment contents
direct_content_matches <- inner_join(sjedit_no_match, mjedit_no_match, by=c("Clean_Comment_Content"="clean_comment"))
direct_content_matches$clean_comment <- direct_content_matches$Clean_Comment_Content
direct_content_matches$match <- 1
direct_content_matches$match <- ifelse(direct_content_matches$match == 1, 1, ifelse((grepl("//", direct_content_matches$commenttext) 
                                                                             & grepl("//", direct_content_matches$Comment_Content)), "", 0))

direct_content_matches <- direct_content_matches %>% select (-Comment_Immediate_Preceding_Code, -Comment_Immediate_Succeeding_Code)

# These content matches needs to be checked right away and labeled
# Saving the pattern matches for in between check and match marking
#fwrite(direct_content_matches, file =paste0("./direct_content_matches/", project_name, "_direct_content_matches.csv"), sep = ";")

##############################
# MANUAL LABELING PART HERE ##
##############################
# Read the pattern matches back after updating the matches
direct_content_matches <- fread(file =paste0("./direct_content_matches/", project_name, "_direct_content_matches.csv"), 
                              sep = ";",
                              colClasses = sapply(direct_content_matches, class))
direct_content_matches <- direct_content_matches[direct_content_matches$match == 1,]

# Removing the content  matches from the data
mjedit_no_match <- mjedit_no_match[!(mjedit_no_match$mrowid %in% direct_content_matches$mrowid),]
sjedit_no_match <- sjedit_no_match[!(sjedit_no_match$srowid %in% direct_content_matches$srowid),]



# Delete the duplicate comments
#direct_matches <- direct_matches[!duplicated(direct_matches$Comment_Content),]

# Project: all / no duplicates
# Ant: 2,175
# JMeter: 
# ArgoUML: 
# Columba: 
# EMF: 
# Hibernate: 
# JEdit: 
# JFreeChart: 
# JRuby: 
# SQL: 
# Total: 

# Keep only the columns which are kept in the end
#direct_matches <- direct_matches[,c("score", "projectname", "classification", "commenttext", "clean_comment", "Clean_Comment_Content",
#                                    "Comment_Content")]

# Removing the direct matches from the data
#mjedit_no_match <- mjedit[!(mjedit$mrowid %in% direct_matches$mrowid),]
#sjedit_no_match <- sjedit[!(sjedit$srowid %in% direct_matches$srowid),]

#mjedit <- mjedit[!(mjedit$commenttext %in% direct_matches$Comment_Content),]
#sjedit <- sjedit[!(sjedit$Comment_Content %in% direct_matches$Comment_Content),]

# 1. Start of the minhash-processing and matching
# Creating minhash-generator and corpus
# Default: 200
minhash <- minhash_generator(200, seed = 16667)
#minhash <- minhash_generator(240, seed = 16667)
total_corpus <- TextReuseCorpus(text = c(mjedit_no_match$clean_comment, sjedit_no_match$Clean_Comment_Content), 
                                #meta = list(description = "Maldonado"),
                                tokenizer = tokenize_words,
                                minhash_func = minhash,
                                skip_short = TRUE)
#saveRDS(total_corpus, file = paste0("./save/corpus/",project_name, "_corpus_no_dup.rds"))
#total_corpus <- readRDS(file = "jEdit_corpus_no_dup_dirmatch.rds")
#Buckets: default 50
buckets <- lsh(total_corpus, bands = 50, progress = TRUE)
#buckets <- lsh(total_corpus, bands = 80, progress = TRUE)

saveRDS(buckets, file = paste0(project_name, "_buckets_latest.rds"))
#buckets <- readRDS(file = paste0(project_name, "_buckets_latest.rds"))

# This calculates the candidates from the buckets at once
candidates <- lsh_candidates(buckets)
scores <- lsh_compare(candidates, total_corpus, jaccard_similarity, progress = TRUE)

# Comparisons / Pairwise
# Jmeter    68,537  / 628,320
# Ant       359,262 / 231,776
# ArgoUML   707,089 / 1,651,430 = 0.4282
# Columba   193,727 / 335,520
# EMF       2,388,380 / 73,536
# Hibernate
# jEdit
# jFreechart
# jruby
# Squirrel



gc()
saveRDS(scores, file = paste0("./save2/", project_name, "_scores_with_dup_latest.rds"))
saveRDS(direct_matches, file = paste0("./save2/", project_name, "_direct_with_dup_latest.rds"))
saveRDS(direct_content_matches, file = paste0("./save2/", project_name, "_direct_content_with_dup_latest.rds"))

#Reading
scores <- readRDS(file = paste0("./save2/", project_name, "_scores_with_dup_latest.rds"))
direct_matches <- readRDS(file = paste0("./save2/", project_name, "_direct_with_dup_latest.rds"))
direct_content_matches <- readRDS(file = paste0("./save2/", project_name, "_direct_content_with_dup_latest.rds"))
# 1.2. Matching only the comments that are present in Maldonado and in Soccminer

#Removing the doc- from score-names to get the row numbers
scores$anum <- gsub("^doc-*", "", scores$a)
scores$bnum <- gsub("^doc-*", "", scores$b)

# Resetting the row numbers
# We always put Maldonado data first in the corpus, 
# so that the doc-1 matches row 1 from Maldonado and so on.
rownames(mjedit_no_match) <- NULL
#mjedit_no_match$rowid <- rownames(mjedit_no_match)
mjedit_no_match$corpusid <- rownames(mjedit_no_match)

# Soccminer is always placed after Maldonado into the corpus,
# so the first doc-number is number of rows in Maldonado plus 1
rownames(sjedit_no_match) <- (nrow(mjedit_no_match)+1):(nrow(mjedit_no_match) + nrow(sjedit_no_match))
#sjedit_no_match$rowid <- rownames(sjedit_no_match)
sjedit_no_match$corpusid <- rownames(sjedit_no_match)

# Exclude all the pairs, which have rowids either only in Maldonado or Soccminer
# We do not wish to match these within the datasets, only with each other.
# UPDATE 14.4. changed every rowid to corpusid. Rowid is matching the original mjedit, sjedit
scores$maldo <- ifelse(scores$anum %in% mjedit_no_match$corpusid & scores$bnum %in% mjedit_no_match$corpusid, NA, "OK")
table(scores$maldo)
scores <- scores[!is.na(scores$maldo),]
scores$socc <- ifelse(scores$anum %in% sjedit_no_match$corpusid & scores$bnum %in% sjedit_no_match$corpusid, NA, "OK")
table(scores$socc)
scores <- scores[!is.na(scores$socc),]
scores <- scores %>% select(-maldo, -socc, -a, -b)

# Join the rows which match maldonado
scores <- left_join(scores, mjedit_no_match, by=c("anum"="corpusid"))
scores <- left_join(scores, mjedit_no_match, by=c("bnum"="corpusid"))
scores <- scores %>% 
  mutate(projectname = coalesce(projectname.x, projectname.y)) %>% 
  select(-projectname.x, -projectname.y) %>%
  mutate(classification = coalesce(classification.x, classification.y)) %>%
  select(-classification.x, -classification.y) %>%
  mutate(commenttext = coalesce(commenttext.x, commenttext.y)) %>%
  select(-commenttext.x, -commenttext.y) %>%
  mutate(clean_comment = coalesce(clean_comment.x,clean_comment.y)) %>% 
  select(-clean_comment.x,-clean_comment.y) %>%
  mutate(mrowid = coalesce(mrowid.x,mrowid.y)) %>% 
  select(-mrowid.x,-mrowid.y)

#Join the rows which match soccminer
#scores <- left_join(scores, sjedit_no_match, by=c("anum"="rowid"))
#scores <- left_join(scores, sjedit_no_match, by=c("bnum"="rowid"))
scores <- left_join(scores, sjedit_no_match[,c("Clean_Comment_Content", "Comment_Content", "comment_key", "srowid", "corpusid")], by=c("anum"="corpusid"))
scores <- left_join(scores, sjedit_no_match[,c("Clean_Comment_Content", "Comment_Content", "comment_key", "srowid", "corpusid")], by=c("bnum"="corpusid"))
scores <- scores %>% mutate(Clean_Comment_Content = coalesce(Clean_Comment_Content.x,Clean_Comment_Content.y)) %>% 
  select(-Clean_Comment_Content.x,-Clean_Comment_Content.y) %>%
  mutate(srowid = coalesce(srowid.x,srowid.y)) %>% 
  select(-srowid.x,-srowid.y) %>%
  mutate(comment_key = coalesce(comment_key.x,comment_key.y)) %>% 
  select(-comment_key.x,-comment_key.y) %>%
  #mutate(Comment_Assoc_Block_Node = coalesce(Comment_Assoc_Block_Node.x,Comment_Assoc_Block_Node.y)) %>% 
  #select(-Comment_Assoc_Block_Node.x,-Comment_Assoc_Block_Node.y) %>%
  #mutate(Comment_Category = coalesce(Comment_Category.x, Comment_Category.y)) %>% 
  #select(-Comment_Category.x,-Comment_Category.y) %>%
  mutate(Comment_Content = coalesce(Comment_Content.x, Comment_Content.y)) %>% 
  select(-Comment_Content.x,-Comment_Content.y)
  #mutate(Comment_First_Element_In = coalesce(Comment_First_Element_In.x, Comment_First_Element_In.y)) %>% 
  #select(-Comment_First_Element_In.x,-Comment_First_Element_In.y) %>%
  #mutate(Comment_Immediate_Preceding_Code = coalesce(Comment_Immediate_Preceding_Code.x, Comment_Immediate_Preceding_Code.y)) %>% 
  #select(-Comment_Immediate_Preceding_Code.x,-Comment_Immediate_Preceding_Code.y) %>%
  #mutate(Comment_Immediate_Succeeding_Code = coalesce(Comment_Immediate_Succeeding_Code.x, Comment_Immediate_Succeeding_Code.y)) %>% 
  #select(-Comment_Immediate_Succeeding_Code.x,-Comment_Immediate_Succeeding_Code.y) %>%
  #mutate(Comment_Last_Element_In = coalesce(Comment_Last_Element_In.x, Comment_Last_Element_In.y)) %>% 
  #select(-Comment_Last_Element_In.x,-Comment_Last_Element_In.y) %>%
  #mutate(Comment_Level = coalesce(Comment_Level.x, Comment_Level.y)) %>% 
  #select(-Comment_Level.x,-Comment_Level.y) %>%
  #mutate(Comment_Line_No = coalesce(Comment_Line_No.x, Comment_Line_No.y)) %>% 
#select(-Comment_Line_No.x,-Comment_Line_No.y) %>%
#mutate(Comment_Parent_Identifier = coalesce(Comment_Parent_Identifier.x, Comment_Parent_Identifier.y)) %>% 
#select(-Comment_Parent_Identifier.x,-Comment_Parent_Identifier.y) %>%
#mutate(Comment_Parent_Trace = coalesce(Comment_Parent_Trace.x, Comment_Parent_Trace.y)) %>% 
#select(-Comment_Parent_Trace.x,-Comment_Parent_Trace.y) %>%
#mutate(Comment_Preceding_Node = coalesce(Comment_Preceding_Node.x, Comment_Preceding_Node.y)) %>% 
#select(-Comment_Preceding_Node.x,-Comment_Preceding_Node.y) %>%
#mutate(Comment_Succeeding_Node = coalesce(Comment_Succeeding_Node.x, Comment_Succeeding_Node.y)) %>% 
#select(-Comment_Succeeding_Node.x,-Comment_Succeeding_Node.y) %>%
#mutate(Comment_Source_File = coalesce(Comment_Source_File.x, Comment_Source_File.y)) %>% 
#select(-Comment_Source_File.x,-Comment_Source_File.y) %>%
#mutate(Comment_SubCategory = coalesce(Comment_SubCategory.x, Comment_SubCategory.y)) %>% 
#select(-Comment_SubCategory.x,-Comment_SubCategory.y) %>%
#mutate(Comment_SubCatg_Type = coalesce(Comment_SubCatg_Type.x, Comment_SubCatg_Type.y)) %>% 
#select(-Comment_SubCatg_Type.x,-Comment_SubCatg_Type.y) %>%
#mutate(Comment_Type = coalesce(Comment_Type.x, Comment_Type.y)) %>% 
#select(-Comment_Type.x,-Comment_Type.y) %>%
#mutate(Project_ID = coalesce(Project_ID.x, Project_ID.y)) %>% 
#select(-Project_ID.x,-Project_ID.y) %>%
  #mutate(file_key = coalesce(file_key.x, file_key.y)) %>% 
  #select(-file_key.x,-file_key.y) %>%
  #mutate(Project = coalesce(Project.x, Project.y)) %>% 
  #select(-Project.x,-Project.y)
  

# Remove the rows, not anymore needed. Every comment which is similar is thought
# to have the same SATD classification.
scores <- scores %>% ungroup()
scores <- scores[!duplicated(scores),]
table(scores$classification)
table(mjedit$classification)
# If there are comments where one has // and the other one doesn't, mark
# it as a non-match
#scores$match <- ifelse((grepl("//", scores$commenttext) & grepl("//", scores$Comment_Content)), 2, 0)

#scores$match <- mapply(grepl, pattern= scores$commenttext, x=scores$Comment_Content,fixed=TRUE)
#scores$match <- mapply(grepl, pattern= scores$Comment_Content, x=scores$commenttext,fixed=TRUE)
#scores <- scores[scores$match == 2,]
#scores$match <- ""

# First take out the scores with the perfect match of 1
scores_perfect_match <- scores[scores$score == 1,]
table(scores_perfect_match$classification)  
scores_perfect_match$match <- 1



# This checks if the unprocessed string from Maldonado dataset is present in Soccminer and vice versa.
#scores_pattern_match <- scores[scores$score != 1,]
scores_pattern_match <- scores[!(scores$mrowid %in% scores_perfect_match$mrowid |
                                                 scores$srowid %in% scores_perfect_match$srowid),]


scores_pattern_match$match <- ifelse(mapply(grepl, 
                                            pattern= scores_pattern_match$clean_comment, 
                                            x=scores_pattern_match$Clean_Comment_Content,fixed=TRUE), 1,
                                     ifelse(mapply(grepl, 
                                                   pattern= scores_pattern_match$Clean_Comment_Content, 
                                                   x=scores_pattern_match$clean_comment,fixed=TRUE), 1, 0))


table(scores_pattern_match$match)
table(scores_pattern_match$classification, scores_pattern_match$match == 1)

# Lastly, we check for whether the raw comments have // in them both or not,
# as this weeds out //-comments matches purely matched to /* */

scores_pattern_match$match <- ifelse(scores_pattern_match$match == 1, 1, ifelse((grepl("//", scores_pattern_match$commenttext) 
                                                                                 & grepl("//", scores_pattern_match$Comment_Content)), "", 0))

###################
# SLOW DOWN HERE ##
###################


# Saving the pattern matches for in between check and match marking
#fwrite(scores_pattern_match, file =paste0("./pattern/", project_name, "_pattern_match.csv"), sep = ";")

##############################
# MANUAL LABELING PART HERE ##
##############################
# Read the pattern matches back after updating the matches
scores_pattern_match <- fread(file =paste0("./pattern/", project_name, "_pattern_match.csv"), 
                              sep = ";",
                              colClasses = sapply(scores_pattern_match, class))
# Weed out the pattern matches that did not really match
scores_pattern_match <- scores_pattern_match[scores_pattern_match$match == 1,]
scores_pattern_match$match <- as.numeric(scores_pattern_match$match)

# Checking out the scores, which were not present in either perfect matches or
# on the pattern matches
scores_no_match <- scores[!(scores$mrowid %in% scores_perfect_match$mrowid |
                              scores$srowid %in% scores_perfect_match$srowid),]

scores_no_match <- scores_no_match[!(scores_no_match$mrowid %in% scores_pattern_match$mrowid |
                                       scores_no_match$srowid %in% scores_pattern_match$srowid),]




# Combine perfect and partial string matches, and finally combine the direct matches
# and direct content matches
scores_combined_matches <- full_join(scores_perfect_match, scores_pattern_match)
scores_combined_matches <- full_join(scores_combined_matches, direct_matches)
scores_combined_matches <- full_join(scores_combined_matches, direct_content_matches)
table(scores_combined_matches$classification)
table(mjedit$classification)




scores_missing <- mjedit[!(mjedit$mrowid %in% scores_combined_matches$mrowid),]

# These need to be checked, and then the matches are combined with the
# scores_combined_matches
scores_missing <- scores[scores$mrowid %in% scores_missing$mrowid,]
scores_missing$match <- 0

###
#fwrite(scores_missing, file =paste0("./missing/", project_name, "_missing_match.csv"), append = FALSE,
#       sep = ";", sep2 = c("",",",""))

table(scores_missing$classification)
table(scores_missing$match)

#spelling_test <- sjedit[sjedit$Comment_Content %like% "Note this should not be ni",]
#spelling_test <- mjedit[mjedit$commenttext %like% "Note this should not be in",]

##############################
# MANUAL LABELING PART HERE ##
##############################

scores_missing <- fread(file = paste0("./missing/", project_name, "_missing_match.csv"), 
                         sep = ";",
                         colClasses = sapply(scores_missing, class))
scores_missing <- scores_missing[scores_missing$match == 1,]

scores_combined_matches <- full_join(scores_combined_matches, scores_missing)
table(scores_combined_matches$classification)
table(mjedit$classification)


################
# WRITING PART #
################

#fwrite(scores_combined_matches, file =paste0("./combined_matches/", project_name, "_combined_matches", ".csv"), append = FALSE,
#       sep = ";", sep2 = c("",",",""))

#
#
#
#project_name <- "sql12"

scores_combined_matches <- fread(file = paste0("./combined_matches/", project_name, "_combined_matches.csv"), 
                        sep = ";",
                        colClasses = sapply(scores_combined_matches, class))
scores_combined_matches_backup <- scores_combined_matches
#############
#############
#?fread

# Combine back the multiple matches weeded out in the beginning
# 1. First find all the matched comments in maldo and mark them as such
mjedit_dupl_matches <- mjedit_dupl_1_each_group[mjedit_dupl_1_each_group$mrowid %in% scores_combined_matches$mrowid, ]

# I guess we do not need anything else besides comment content and mrowid
mjedit_dupl_matches <- mjedit_dupl_matches[,c("clean_comment", "mrowid")]

# After that take all the matching rows from the duplicated df
mjedit_dupl_matches <- mjedit_dupl[mjedit_dupl$clean_comment %in% mjedit_dupl_matches$clean_comment,]
mjedit_dupl_matches <- mjedit_dupl_matches[,c("clean_comment", "mrowid")]

mjedit_dupl_matches <- left_join(mjedit_dupl_matches, scores_combined_matches, by = c("clean_comment"))
# After combining, we remove the mrowid.y (which is the unduplicated one) and rename mrowid.x to mrowid
# Then we can bind the rows together and we have all the duplicated entries in the data.
mjedit_dupl_matches <- mjedit_dupl_matches %>% select(-mrowid.y)
names(mjedit_dupl_matches)[names(mjedit_dupl_matches) == "mrowid.x"] <- "mrowid"

mjedit_dupl_matches <- mjedit_dupl_matches[c(names(scores_combined_matches))]

# Before joining, taking out the values we used to mark each duplicated group
#mjedit_dupl_matches <- mjedit_dupl_matches[!(mjedit_dupl_matches$mrowid %in% mjedit_dupl_1_each_group$mrowid),]

mjedit_dupl_matches <- rbind(scores_combined_matches,mjedit_dupl_matches)
mjedit_dupl_matches <- mjedit_dupl_matches[!duplicated(mjedit_dupl_matches),]


# Filter out the pairs which have multiple matches with each other, 
#mrow_srow_ids <- scores_combined_matches[,c("mrowid", "srowid")]
#mrow_srow_ids <- scores_combined_matches

mrow_srow_ids <- mjedit_dupl_matches[,c("mrowid", "srowid")]

#df <- mrow_srow_ids

#df[,1:ncol(df)] <- NA

msrow_lists <- mrow_srow_ids %>% 
  group_by(mrowid) %>%
  group_map(~.x, .keep = TRUE)

temp_project_ID <- unique(sjedit$Project_ID)

df <- data.frame(project_id = temp_project_ID,
  mrowid=character(nrow(mrow_srow_ids)),
                 srowid=character(nrow(mrow_srow_ids)))
maldo_ids <- vector()
socc_ids <- vector()

# Create a vector to add all the processed maldonado ids, so we do not go over them again
maldonado_processed_ids <- vector()

for(i in 1:nrow(mrow_srow_ids)){
  
  # If maldoid has not been added, add it to a list
  if (!(mrow_srow_ids$mrowid[[i]] %in% maldonado_processed_ids)){
    #print(mrow_srow_ids$mrowid[[i]])
    #Check rows 
    maldonado_processed_ids <- append(maldonado_processed_ids, mrow_srow_ids$mrowid[[i]])
    df$mrowid[[i]] <- mrow_srow_ids$mrowid[[i]]
    # 1. Take an id from Maldonado and add it to a vector
    maldo_id <- mrow_srow_ids$mrowid[[i]]
    maldo_ids <- append(maldo_ids,maldo_id)
    # 2. Look up all the matching Soccminer matches from the dataframe
    socc_ids <- append(socc_ids, mrow_srow_ids$srowid[mrow_srow_ids$mrowid == mrow_srow_ids$mrowid[[i]]])
    # 3. Iterate over the Soccminer matcher from the dataframe to see if they match more maldonado hits

    for (id in socc_ids){
      #print(paste("Soccid:" ,id))
      #print(paste("Maldoid:" ,mrow_srow_ids$mrowid[mrow_srow_ids$srowid == id]))
      
      # 4. Retrieve all the maldonado ids each soccminer id is attached to
      maldo_ids_another <- mrow_srow_ids$mrowid[mrow_srow_ids$srowid == id]
      
      # 5. Check if the hit produces a new hit from the maldonadoset
      # If it's a new one, add it to the vector
      for (maldo_hit in maldo_ids_another){
        if(!(maldo_hit %in% maldo_ids)){
          maldo_ids <- append(maldo_ids, maldo_hit)
          maldonado_processed_ids <- append(maldonado_processed_ids, maldo_hit)
        }
      
    }
    
      # 6. Append these hits to the maldonado set
      #maldo_ids <- append(maldo_ids, mrow_srow_ids$mrowid[mrow_srow_ids$srowid == id])
    }
    #print(mrow_srow_ids$srowid[mrow_srow_ids$mrowid == mrow_srow_ids$mrowid[[i]]])
    
    
    # Append the lists to the dataframe
    df$srowid[i] <- list(c(mrow_srow_ids$srowid[mrow_srow_ids$mrowid == mrow_srow_ids$mrowid[[i]]]))
    df$mrowid[i] <- list(c(maldo_ids))
    maldo_ids <- vector()
    socc_ids <- vector()
    # This appends a list to df cell
    #df$srowid[[i]] <- list(c(mrow_srow_ids$srowid[mrow_srow_ids$mrowid == mrow_srow_ids$mrowid[[i]]]))
    
  }
}

df <- df[df$mrowid != "",]
#df <- df[df$srowid != "",]
# Saving an individual file. These are then combined later to avoid error
#fwrite(df, file =paste0("./maldomatch_save/individual_matches/", project_name, "_maldo_socc_matches.csv"), append = FALSE,
#       sep = ";", sep2 = c("",",",""))

table(mjedit_dupl_matches$classification)
table(mjedit$classification)

# SAVE THE FINAL CSV FILES FOR DB IMPORT:

# Need: Project_name, comment content, maldo_id as maldo_comment_id, comment_key
mjedit_dupl_matches$commenttext <- ifelse(mjedit_dupl_matches$commenttext == "", 
                                          mjedit_dupl_matches$Comment_Content, 
                                          mjedit_dupl_matches$commenttext)
mjedit_dupl_matches$commenttext <- ifelse(is.na(mjedit_dupl_matches$commenttext), 
                                          mjedit_dupl_matches$Comment_Content, 
                                          mjedit_dupl_matches$commenttext)


mjedit_dupl_matches <- mjedit_dupl_matches[,c("comment_key", 
                                              "commenttext", 
                                              "projectname",
                                              "mrowid")]
names(mjedit_dupl_matches)[names(mjedit_dupl_matches) == "comment_key"] <- "socc_comment_key"
names(mjedit_dupl_matches)[names(mjedit_dupl_matches) == "commenttext"] <- "Maldonado_Comment_Content"
names(mjedit_dupl_matches)[names(mjedit_dupl_matches) == "projectname"] <- "Serialized_Project_Name"
names(mjedit_dupl_matches)[names(mjedit_dupl_matches) == "mrowid"] <- "Maldo_Comment_ID"
mjedit_dupl_matches <- mjedit_dupl_matches[,c("socc_comment_key", 
                                              "Serialized_Project_Name",
                                              "Maldonado_Comment_Content",
                                              "Maldo_Comment_ID")]
mjedit_dupl_matches <- as.data.frame(mjedit_dupl_matches)
mjedit_dupl_matches_backup <- mjedit_dupl_matches
mjedit_dupl_matches$Maldo_Comment_ID <- as.integer(mjedit_dupl_matches$Maldo_Comment_ID)

write.csv(mjedit_dupl_matches, file =paste0("./db_export_files/", project_name, "_export.csv"),
          row.names = FALSE)
library(RSQLite)
library(DBI)
con <- dbConnect(RSQLite::SQLite(), dbname = paste0("./db_export_files/", "soccminer_v3.db"))
dbWriteTable(con, name="maldo_comment", value=mjedit_dupl_matches, append=TRUE)


dbDisconnect(con)

mjedit$clean_comment[mjedit$classification == "IMPLEMENTATION"]
mjedit_dupl_matches[,c(clean_comment, srowid)][mjedit_dupl_matches$classification == "IMPLEMENTATION"]
# Done so far:
# Argo
# Ant
# Jmeter
# Columba


###############
# END #########
###############


library(RSQLite)

# Reading the dataset 
con <- dbConnect(RSQLite::SQLite(), dbname = paste0("./db_export_files/", "soccminer_v3.db"))
query_string <- "SELECT COUNT(*) FROM class, file, project 
                  WHERE class.Class_Source_File = file.Source_File
                  AND file.Project_ID = project.project_key
                  AND project.Serialized_Project_Name = 'sql12'"

query_string <- "SELECT COUNT(*) FROM file, project
                  WHERE file.Project_ID = project.project_key
                  AND project.Serialized_Project_Name = 'sql12'"

dbGetQuery(con, query_string)

#
dbDisconnect(con)


# Count the number of files, classes, methods, SATD comments and non-comments, rule violations and avg. severity
soccminer10_count %>% group_by(Project) %>% summarise(n_distinct(Comment_Source_File))
#Different files might have classes with same names, wo we need to group them
soccminer10_count %>% group_by(Project) %>% summarise(n_distinct(Comment_Source_File, Class_Name))
#Files and classes might have methods with similar names, again we group them
soccminer10_count %>% group_by(Project) %>% summarise(n_distinct(Comment_Source_File, Class_Name, Method_Name))

# Counting the number of Rule violations per project
ruledf <- soccminer10_1 %>% group_by(Project, Rule) %>% summarise(Freq=n())
aggregate(ruledf$Freq, by=list(Project=ruledf$Project), FUN=sum)

# Counting the average priority for rule violations
(2.99+2.93+3.00+3.00+2.98+2.95+3.01+2.98+2.98+3.00)/10

# Counting the different priority counts
ruledf <- soccminer10_1 %>% group_by(Project, Priority) %>% summarise(Freq=n())
aggregate(ruledf$Freq, by=list(Priority=ruledf$Priority), FUN=sum)

# Example of usage from ant
ant_count <- soccminer10_count[soccminer10_count$Project == "apache-ant-1.7.0",]
ant_count %>% summarise(n_distinct(Comment_Source_File, Class_Name, Method_Name))

ant_count_with <- ant_count[ant_count$classification != "WITHOUT_CLASSIFICATION",]
ant_count_without <- ant_count[ant_count$classification == "WITHOUT_CLASSIFICATION",]
# Files counts
ant_count_with %>% summarise(n_distinct(Comment_Source_File))
ant_count_without_2 <- anti_join(ant_count_without, ant_count_with, by = c("Comment_Source_File"))
ant_count_without_2 %>% summarise(n_distinct(Comment_Source_File))
# Class counts
ant_count_with %>% summarise(n_distinct(Comment_Source_File, Class_Name))
ant_count_without_2 <- anti_join(ant_count_without, ant_count_with, by = c("Comment_Source_File", "Class_Name"))
ant_count_without_2 %>% summarise(n_distinct(Comment_Source_File, Class_Name))

# Methods counts
ant_count_with %>% summarise(n_distinct(Comment_Source_File, Class_Name, Method_Name))

ant_count_without <- ant_count[ant_count$classification == "WITHOUT_CLASSIFICATION",]
ant_count_without <- anti_join(ant_count_without, ant_count_with, by = c("Class_Name", "Comment_Source_File", "Method_Name"))
ant_count_without %>% summarise(n_distinct(Comment_Source_File, Class_Name, Method_Name))

# Paired t-test to determine difference between satd methods and pmd results
# again with ANT
ant_count <- soccminer10_1[soccminer10_1$Project == "apache-ant-1.7.0",]
ant_count_with <- ant_count[ant_count$classification != "WITHOUT_CLASSIFICATION",]
jmeter_count <- soccminer10_1[soccminer10_1$Project == "apache-jmeter-2.10",]
jmeter_count_with <- jmeter_count[jmeter_count$classification != "WITHOUT_CLASSIFICATION",]
squirrel_count <- soccminer10_1[soccminer10_1$Project == "sql12",]
squirrel_count_with <- squirrel_count[squirrel_count$classification != "WITHOUT_CLASSIFICATION",]


wilcox.test(x = ant_count_with$Priority, y = jmeter_count_with$Priority, paired = FALSE)
wilcox.test(x = ant_count_with$Priority, y = squirrel_count_with$Priority, paired = FALSE)
wilcox.test(x = jmeter_count_with$Priority, y = squirrel_count_with$Priority, paired = FALSE)



#df2 <- as.data.frame(apply(df,2, function (x) paste(unlist(x),collapse=",")))
#df2 <- data.frame(lapply(df, paste0,  "x"))
#df2 <- data.frame(lapply(df, paste(unlist(),collapse="")))
##############
##############

d <- data.frame(id=1:2, name=c("Jon", "Mark"))
d$children <-  list(c("Mary", "James"), c("Greta", "Sally", "Bob"))
fwrite(d, file ="df_Place.csv", sep = ";")
d2 <- fread(file = "df_Place.csv")


?fwrite
d <- data.frame(lapply(df_Place, as.character), stringsAsFactors=FALSE)

d[] <- apply(d, 2, function (x) paste(unnest(x),collapse=","))

myVector <- c("A","B","C","D")

myList <- list()
myList[[1]] <- c(1, 4, 6, 7)
myList[[2]] <- c(2, 7, 3)
myList[[3]] <- c(5, 5, 3, 9, 6)
myList[[4]] <- c(7, 9)

myDataFrame <- data.frame(row = c(1,2,3,4))

myDataFrame$col1 <- myVector
myDataFrame$col2 <- myList

#################
#################
#############
#############

# Filter out the pairs which have multiple matches with each other, 
#mrow_srow_ids <- scores_combined_matches[,c("mrowid", "srowid")]
mrow_srow_ids <- direct_content_matches[,c("mrowid", "srowid")]

msrow_lists <- mrow_srow_ids %>% 
  group_by(mrowid) %>%
  group_map(~.x, .keep = TRUE)

df <- data.frame(mrowid=character(nrow(mrow_srow_ids)),
                 srowid=character(nrow(mrow_srow_ids)))
maldo_ids <- vector()
socc_ids <- vector()

# Create a vector to add all the processed maldonado ids, so we do not go over them again
maldonado_processed_ids <- vector()

for(i in 1:nrow(mrow_srow_ids)){
  
  # If maldoid has not been added, add it to a list
  if (!(mrow_srow_ids$mrowid[[i]] %in% maldonado_processed_ids)){
    #print(mrow_srow_ids$mrowid[[i]])
    #Check rows 
    maldonado_processed_ids <- append(maldonado_processed_ids, mrow_srow_ids$mrowid[[i]])
    df$mrowid[[i]] <- mrow_srow_ids$mrowid[[i]]
    
    # 1. Take an id from Maldonado and add it to a vector
    maldo_id <- mrow_srow_ids$mrowid[[i]]
    maldo_ids <- append(maldo_ids,maldo_id)
    # 2. Look up all the matching Soccminer matches from the dataframe
    socc_ids <- append(socc_ids, mrow_srow_ids$srowid[mrow_srow_ids$mrowid == mrow_srow_ids$mrowid[[i]]])
    # 3. Iterate over the Soccminer matcher from the dataframe to see if they match more maldonado hits
    for (id in socc_ids){
      #print(paste("Soccid:" ,id))
      #print(paste("Maldoid:" ,mrow_srow_ids$mrowid[mrow_srow_ids$srowid == id]))
      
      # 4. Retrieve all the maldonado ids each soccminer id is attached to
      maldo_ids_another <- mrow_srow_ids$mrowid[mrow_srow_ids$srowid == id]
      
      # 5. Check if the hit produces a new hit from the maldonadoset
      # If it's a new one, add it to the vector
      for (maldo_hit in maldo_ids_another){
        if(!(maldo_hit %in% maldo_ids)){
          maldo_ids <- append(maldo_ids, maldo_hit)
          maldonado_processed_ids <- append(maldonado_processed_ids, maldo_hit)
        }
      }
      # 6. Append these hits to the maldonado set
      #maldo_ids <- append(maldo_ids, mrow_srow_ids$mrowid[mrow_srow_ids$srowid == id])
    }
    #print(mrow_srow_ids$srowid[mrow_srow_ids$mrowid == mrow_srow_ids$mrowid[[i]]])
    
    
    # Append the lists to the dataframe
    df$srowid[i] <- list(c(mrow_srow_ids$srowid[mrow_srow_ids$mrowid == mrow_srow_ids$mrowid[[i]]]))
    df$mrowid[i] <- list(c(maldo_ids))
    maldo_ids <- vector()
    socc_ids <- vector()
    # This appends a list to df cell
    #df$srowid[[i]] <- list(c(mrow_srow_ids$srowid[mrow_srow_ids$mrowid == mrow_srow_ids$mrowid[[i]]]))
    
  }
}

######################
######################

write.csv2(d, file= "test.csv")
# Save this file for annotation
write.csv2(scores_combined_matches[c("score", "Project", "file_key", "mrowid", "srowid", "commenttext", "clean_comment", "Comment_Content", "Clean_Comment_Content", "match")], 
           file = paste0("./save2/csv/", 
                         project_name, "_only_satd_v2.csv"), row.names = FALSE)

#scores_sanity <- scores_combined_matches[scores_combined_matches$classification == "DESIGN" & scores_combined_matches$match == 0,]
# Keep only the highest score matches for each comment pair
# since there is a possibility that one comment was matched with several possibilities.
#scores <- scores %>%
#  group_by(anum) %>%
#  slice(which.max(score))

#







#scores_string_match <- scores[scores$match == 1,]
#table(scores_string_match$classification)  
# Keep only the matches which either have the substring match, or if none are found, then the highest score


#scores_match_found_a <- scores[!(scores$anum %in% scores_string_match$anum | scores$bnum %in% scores_string_match$bnum),]
#table(scores_match_found_a$match)


#scores_match_found_b <- scores %>%
  #group_by(bnum) %>%
  #slice(max(scores$match, scores$score)) %>%
  #ungroup()
#table(scores_match_found_b$match)

#scores_match_found <- full_join(scores_match_found_a, scores_match_found_b)
#table(scores_match_found$match)
#scores_match_found <- scores_match_found[!duplicated(scores_match_found),]
#table(scores_match_found$classification)
# Remove the rows, not anymore needed. Every comment which is similar is thought
# to have the same SATD classification.
scores_combined_matches <- scores_combined_matches %>% ungroup() %>% 
  select(-anum, -bnum)

# Dropping the rowids, they are not neede anymore
mjedit <- mjedit %>% select(-rowid)
sjedit <- sjedit %>% select(-rowid)

scores_final_set <- full_join(direct_matches, scores_combined_matches)

#scores$match <- ifelse(scores$score > 0.9, 1, "")

# If there are comments where one has // and the other one doesn't, mark
# it as a non-match
scores$match <- ifelse(scores$match == 1, 1, ifelse((grepl("//", scores$commenttext) & grepl("//", scores$Comment_Content)), "", 0))

#Write the direct matches and scores to a file
write.csv2(scores_final_set[c("score", "Project", "file_key", "mrowid", "srowid", "commenttext", "clean_comment", "Comment_Content", "Clean_Comment_Content", "match")], file = paste0("./save2/csv/", project_name, "_only_satd_v1.csv"), row.names = FALSE)
scores2 <- read.csv2(file = paste0("./save2/csv/", project_name, "_only_satd_v1.csv"))
table(scores$classification)
table(scores2$classification)

table(m2017satd$classification[m2017satd$projectname == project_name])
table(mjedit$classification[mjedit$projectname == project_name])
#df <- scores[1656:1676,]
#df$match[5:10] <- 1
#df$match <- ifelse(df$match == 1, 1, ifelse((grepl("//", df$commenttext) & grepl("//", df$Comment_Content)), "", 0))
 
### Loading he matches and building the appropriate metrics from db
#install.packages("RSQLite")

rm(list = ls())
gc()

library(DBI)
library(dplyr)

# 0. Create a new table in the SoccMiner database, where we insert Maldonado comments and
# the connected comment_ids and extra column to mark whether we had a match or not
mydb <- dbConnect(RSQLite::SQLite(), "../../PycharmProjects/soccMinerDataToSQLite/save/soccminer_v2.db")

# We need from comment-table: comment_key
dbExecute(mydb, "
CREATE TABLE maldonado_comment(
  maldonado_key INTEGER PRIMARY KEY AUTOINCREMENT, 
  Connected_Comment_Id int,
  Match int,
  Comment text,
  Clean_Comment text,
  Classification text)")


#project_name = "apache-ant-1.7.0"
#project_name = "apache-jmeter-2.10"
#project_name = "argouml-VERSION_0_34"
#project_name = "columba-1.4-src"
#project_name = "emf-2.4.1"
#project_name = "hibernate-distribution-3.3.2.GA"
#project_name = "jEdit-4.2"
#project_name = "jfreechart-1.0.19"
#project_name = "jruby-1.4.0"
project_name = "sql12"

# Done
# +Ant
# +Jmeter
# +Argo
# +Columba
# +EMF
# +Hibernate
# +jEdit
# +Jfreechart
# +JRuby
# +Sql12


# 1. Load the labeled csv.
project <- read.csv2(file = paste0("./save2/csv/", project_name, "_v4_DONE.csv"))
project$match <- replace(project$match, is.na(project$match), 0)
# Deleting the duplicates, which are not needed.
project <- project[!duplicated(project[c('commenttext', 'match')]), ]
project <- project[order(project$match, decreasing = TRUE), ]
project <- project[!duplicated(project[c('commenttext')]), ]
# 1.1. Keep only the matches (match == 1)
#project <- project[project$match == 1,]
#table(project$classification)

# 1.1. Remove comments which have match == 0
project$Clean_Comment_Content <-  ifelse(project$match == 1, project$Clean_Comment_Content, NA)
project$Comment_Content <-  ifelse(project$match == 1, project$Comment_Content, NA)


# 2. retrieve the information (comment-key) from the database-file based on project and
# comments


comments_to_find <- project$Comment_Content
comments_to_find <- gsub("'", "''", comments_to_find)
comments_to_find <- paste0("'", comments_to_find, "'", collapse=", ")
whereIn <- paste0('(', comments_to_find, ')')

# JEdit hack
#project_name = "jEdit"

# Works!
#query = paste0('select distinct pmd.Project_ID,
#            pmd.Project,
#            pmd.Rule,
#            pmd.Description,
#            pmd.Begin_Line,
#            pmd.End_Line,
#            comment.Comment_Content,
#            comment.Comment_Source_File,
#            comment.Comment_Line_No
#            from comment join file on comment.Comment_Source_File = file.Source_File
#            join pmd on pmd.Project_ID = file.Project_ID and pmd.Project = ', paste0("'", project_name, "'"),
#            'where comment.Comment_Content in ', whereIn)

# Works!
query = paste0('select distinct comment.comment_key,
            comment.Comment_Content,
            comment.Comment_Source_File,
            comment.Comment_Line_No
            from comment join file on comment.Comment_Source_File = file.Source_File
            join pmd on pmd.Project_ID = file.Project_ID and pmd.Project = ', paste0("'", project_name, "'"),
               'where comment.Comment_Content in ', whereIn)

outside_methods <- dbGetQuery(mydb, query)

# 2.1 Remove unnecessary columns from the df
project <- left_join(project, outside_methods, by = c("Comment_Content"))
project <- project[,c("comment_key", "match", "commenttext", "clean_comment", "classification")]
colnames(project) <- c("Connected_Comment_Id", "Match", "Comment", "Clean_Comment", "Classification")
table(project$Match)
# 3. Populate the maldonado table from the matching 

dbWriteTable(conn=mydb, name="maldonado_comment", value=project, append=TRUE)

# Disconnecting the db
dbDisconnect(mydb)

# Load the previously done matching data into the database
table(matches_df$Project)

mydb <- dbConnect(RSQLite::SQLite(), "../../PycharmProjects/soccMinerDataToSQLite/save/soccminer_v2.db")

#project_name = "apache-ant-1.7.0"
#project_name = "apache-jmeter-2.10"
#project_name = "argouml-VERSION_0_34"
#project_name = "columba-1.4-src"
#project_name = "emf-2.4.1"
#project_name = "hibernate-distribution-3.3.2.GA"
#project_name = "jEdit-4.2"
#project_name = "jfreechart-1.0.19"
#project_name = "jruby-1.4.0"
 project_name = "sql12"

single_matches_project <- matches_df[matches_df$Project == project_name,]

comments_to_find <- single_matches_project$Comment_Content
comments_to_find <- gsub("'", "''", comments_to_find)
comments_to_find <- paste0("'", comments_to_find, "'", collapse=", ")
whereIn <- paste0('(', comments_to_find, ')')


# Done
# ++Ant
# ++Jmeter
# ++Argo
# ++Columba
# ++EMF
# ++Hibernate
# ++jEdit
# ++Jfreechart
# ++JRuby
# ++Sql12

# JEdit hack
#project_name = "jEdit"

query = paste0('select distinct comment.comment_key,
            comment.Comment_Content,
            comment.Comment_Source_File,
            comment.Comment_Line_No
            from comment join file on comment.Comment_Source_File = file.Source_File
            join pmd on pmd.Project_ID = file.Project_ID and pmd.Project = ', paste0("'", project_name, "'"),
               'where comment.Comment_Content in ', whereIn)

inside_methods <- dbGetQuery(mydb, query)

# 2.1 Remove unnecessary columns from the df
project <- left_join(single_matches_project, inside_methods, by = c("Comment_Content"))
project <- project[,c("comment_key", "match", "commenttext", "clean_comment", "classification")]
colnames(project) <- c("Connected_Comment_Id", "Match", "Comment", "Clean_Comment", "Classification")
# 3. Populate the maldonado table from the matching 

dbWriteTable(conn=mydb, name="maldonado_comment", value=project, append=TRUE)


# Disconnecting the db
dbDisconnect(mydb)

### This was to count the annotator agreement
###READ the labeled data
library("psych")

leevi <- read.csv2(file = ".\\data\\apache-jmeter-2.10_no_dup_scores_v2.csv")
murali <- read.csv2(file = ".\\data\\apache-jmeter-2.10_no_dup_scores_v2_annotated.csv")

leeviscore <- leevi$Match[leevi$score >= 0.2 & leevi$score <= 0.9]
muraliscore <- murali$Match[murali$score >= 0.2 & murali$score <= 0.9] 

raterkappa <- cohen.kappa(x = cbind(leeviscore, muraliscore))###
nrow(scores[scores$score < 0.7,])
###
# Counting the remaining comments where we had a match per project and SATD type
###
matches_all_df <- list()
matches_list <- list.files(path=".\\matches\\", pattern = "\\.csv$")
matches_list_path <- lapply(matches_list, function(x) paste0(".\\matches\\", x))
matches_all_df <- lapply(matches_list_path, function(x) read.csv2(file = x, stringsAsFactors = FALSE, strip.white = TRUE))
names(matches_all_df) <- gsub("\\.csv$", "", matches_list)
# List of lists into one df
matches_df <- as.data.frame(do.call(rbind, matches_all_df))
rownames(matches_df) <- NULL
colnames(matches_df)[colnames(matches_df) == "projectname"] <- "Project"
matches_df <- matches_df %>% select(-score)
# Keep only the rows where Match == 1
matches_df <- matches_df[matches_df$match == 1,]
# Joining and creating the final enhanced dataset
soccminer10_1 <- left_join(soccminer10, matches_df)
soccminer10_1 <- soccminer10_1[!is.na(soccminer10_1$classification),]
soccminer10_1 <- soccminer10_1[,c("Project",
                                  "Comment_Source_File",
                                  "File_LOC",
                                  "Class_Name",
                                  "Class_LOC",
                                  "Class_Signature",
                                  "Class_Nested_Level",
                                  "Class_Specifier",
                                  "Class_Type",
                                  "Method_Name",
                                  "Method_LOC",
                                  "Method_Signature",
                                  "Method_Specifier",
                                  "Method_Type",
                                  "Method_Category",
                                  "Comment_Content",
                                  "commenttext",
                                  "classification",
                                  "Comment_Immediate_Preceding_Code",
                                  "Comment_Immediate_Succeeding_Code",
                                  "Rule",
                                  "Rule_Set",
                                  "Priority",
                                  "Description",
                                  "Begin_Line",
                                  "End_Line",
                                  "Comment_Line_No",
                                  "Class_Line_No",
                                  "class.Class_Line_No + class.Class_LOC",
                                  "Method_Line_No",
                                  "method.Method_Line_No + method.Method_LOC")]
names(soccminer10_1)[names(soccminer10_1) == "commenttext"] <- "Original_Comment_Content"
names(soccminer10_1)[names(soccminer10_1) == "Comment_Content"] <- "SoCCMiner_Comment_Content"
names(soccminer10_1)[names(soccminer10_1) == "classification"] <- "SATD_Category"
names(soccminer10_1)[names(soccminer10_1) == "Class_Line_No"] <- "Class_StartLine"
names(soccminer10_1)[names(soccminer10_1) == "Class_Line_No"] <- "Class_StartLine"
names(soccminer10_1)[names(soccminer10_1) == "class.Class_Line_No + class.Class_LOC"] <- "Class_EndLine"
names(soccminer10_1)[names(soccminer10_1) == "Method_Line_No"] <- "Method_StartLine"
names(soccminer10_1)[names(soccminer10_1) == "method.Method_Line_No + method.Method_LOC"] <- "Method_EndLine"
names(soccminer10_1)[names(soccminer10_1) == "Priority"] <- "Rule_Priority"
names(soccminer10_1)[names(soccminer10_1) == "Description"] <- "Rule_Description"
names(soccminer10_1)[names(soccminer10_1) == "Begin_Line"] <- "Rule_Begin_Line"
names(soccminer10_1)[names(soccminer10_1) == "End_Line"] <- "Rule_End_Line"

#write.csv2(soccminer10_1, file = "./save/final_replication_dataset_v2.csv")
# Counting the number of comments remaining in the dataset
# by removing PMD-data, which creates duplicates of comments due to different
# rules attached to a single comment
soccminer10_count <- soccminer10_1 %>% select(-Rule, -Rule_Set, -Priority, -Description, -Begin_Line, -End_Line)

# This creates the table with the different SATD types per project Table 1
soccminer10_count <- soccminer10_count[!duplicated(soccminer10_count),]
table(soccminer10_count$Project, soccminer10_count$classification)
table(soccminer10_count$classification)

# Our percentage, 7.41% of SATD
(414+1966+15+695+51)/nrow(soccminer10_count)*100
# Original percentage 6.54% of SATD
(472+2703+54+757+85)/62275*100

# How much of original SATD we capture? 77.16%
(414+1966+15+695+51)/(472+2703+54+757+85)*100
###

# Count the number of files, classes, methods, SATD comments and non-comments, rule violations and avg. severity
soccminer10_count %>% group_by(Project) %>% summarise(n_distinct(Comment_Source_File))
#Different files might have classes with same names, wo we need to group them
soccminer10_count %>% group_by(Project) %>% summarise(n_distinct(Comment_Source_File, Class_Name))
#Files and classes might have methods with similar names, again we group them
soccminer10_count %>% group_by(Project) %>% summarise(n_distinct(Comment_Source_File, Class_Name, Method_Name))

# Counting the number of Rule violations per project
ruledf <- soccminer10_1 %>% group_by(Project, Rule) %>% summarise(Freq=n())
aggregate(ruledf$Freq, by=list(Project=ruledf$Project), FUN=sum)

# Counting the average priority for rule violations
(2.99+2.93+3.00+3.00+2.98+2.95+3.01+2.98+2.98+3.00)/10

# Couting the different priority counts
ruledf <- soccminer10_1 %>% group_by(Project, Priority) %>% summarise(Freq=n())
aggregate(ruledf$Freq, by=list(Priority=ruledf$Priority), FUN=sum)

# Example of usage from ant
ant_count <- soccminer10_count[soccminer10_count$Project == "apache-ant-1.7.0",]
ant_count %>% summarise(n_distinct(Comment_Source_File, Class_Name, Method_Name))

ant_count_with <- ant_count[ant_count$classification != "WITHOUT_CLASSIFICATION",]
ant_count_without <- ant_count[ant_count$classification == "WITHOUT_CLASSIFICATION",]
# Files counts
ant_count_with %>% summarise(n_distinct(Comment_Source_File))
ant_count_without_2 <- anti_join(ant_count_without, ant_count_with, by = c("Comment_Source_File"))
ant_count_without_2 %>% summarise(n_distinct(Comment_Source_File))
# Class counts
ant_count_with %>% summarise(n_distinct(Comment_Source_File, Class_Name))
ant_count_without_2 <- anti_join(ant_count_without, ant_count_with, by = c("Comment_Source_File", "Class_Name"))
ant_count_without_2 %>% summarise(n_distinct(Comment_Source_File, Class_Name))

# Methods counts
ant_count_with %>% summarise(n_distinct(Comment_Source_File, Class_Name, Method_Name))

ant_count_without <- ant_count[ant_count$classification == "WITHOUT_CLASSIFICATION",]
ant_count_without <- anti_join(ant_count_without, ant_count_with, by = c("Class_Name", "Comment_Source_File", "Method_Name"))
ant_count_without %>% summarise(n_distinct(Comment_Source_File, Class_Name, Method_Name))

# Paired t-test to determine difference between satd methods and pmd results
# again with ANT
ant_count <- soccminer10_1[soccminer10_1$Project == "apache-ant-1.7.0",]
ant_count_with <- ant_count[ant_count$classification != "WITHOUT_CLASSIFICATION",]
jmeter_count <- soccminer10_1[soccminer10_1$Project == "apache-jmeter-2.10",]
jmeter_count_with <- jmeter_count[jmeter_count$classification != "WITHOUT_CLASSIFICATION",]
squirrel_count <- soccminer10_1[soccminer10_1$Project == "sql12",]
squirrel_count_with <- squirrel_count[squirrel_count$classification != "WITHOUT_CLASSIFICATION",]


wilcox.test(x = ant_count_with$Priority, y = jmeter_count_with$Priority, paired = FALSE)
wilcox.test(x = ant_count_with$Priority, y = squirrel_count_with$Priority, paired = FALSE)
wilcox.test(x = jmeter_count_with$Priority, y = squirrel_count_with$Priority, paired = FALSE)

##################################
#CUT OFF FROM 600+

# Comparisons: made / total
# Argo: 707,089 / 1,651,430 = 42.82%

#######################################
# MULTI-FILE VERSION
######################################

# If candidates runs out of memory, this needs to be done in several parts
# and saved and combined separately.
# Happened for: Argo

# Argo 159 done
# Columba 156 done
individual_buckets <- unique(buckets$buckets)
max_length <- length(individual_buckets)

for(i in 1:max_length){
  individual_buckets[i]
  bucket_sample <- buckets[buckets$buckets %in% individual_buckets[i],]
  candidates <- lsh_candidates(bucket_sample)
  saveRDS(candidates, file =paste0("./candidates/", project_name, "/", project_name, "_candidates_", as.character(i), "_", as.character(max_length), ".rds"))
  if (i%% 100 == 0){
    print(i)
  }
  
}



max_length <- length(buckets$buckets)
increment_length = 1000
for(i in seq(from=1, to= max_length, by = increment_length)){
  if (i == 1){
    end_length <- 1000
    
  } else {
    end_length <- i + (increment_length - 1)
  }
  
  if ((i+ end_length) > max_length){
    end_length <- max_length
  }
  #bucket_sample <- buckets[i:end_length,]
  candidates <- lsh_candidates(buckets[i:end_length,])
  saveRDS(candidates, file =paste0("./candidates/", project_name, "_candidates_", as.character(i), "_", as.character(end_length), ".rds"))
  print(i)
  rm(candidates)
  gc()
}

for(i in seq(from=1, to=78, by=3)){
  #  stuff, such as
  print(i)
}

# Combine files to scores
files = list.files(path="./candidates/", pattern="*.rds", full.names = TRUE)

test_file <- readRDS(file = "./candidates/argouml-VERSION_0_34_candidates_71_516157.rds")

##############
# This part loads up the individual files, and removes the ones, where there are only
# either maldonado or soccminer rows to be compared with one another
##############

# 1. Get all the different bucket ids
individual_buckets <- unique(buckets$buckets)
max_length <- length(individual_buckets)

# 2. Get all the ids from Maldonado dataset, so we can check that buckets
# have these ids as well as soccminer ids
rownames(mjedit_no_match) <- NULL
mjedit_no_match$rowid <- rownames(mjedit_no_match)

maldo_row_ids <- paste0("doc-", mjedit_no_match$rowid)

# After that we iterate over all of them and mark if the ids (buckets) only contain
# maldo or socc texts. These are then excluded from the further analyses

ok_buckets <- c()

for(i in 1:10000){
  individual_buckets[i]
  bucket_sample <- buckets[buckets$buckets %in% individual_buckets[i],]
  in_maldo <- ifelse(bucket_sample$doc %in% maldo_row_ids, 0, 1)
  if (0 %in% in_maldo & 1 %in% in_maldo) {
    ok_buckets <- append(ok_buckets, individual_buckets[i])
  } 
}  

buckets[buckets$buckets %in% "bd4247c1e329c56f6a44d549bb88b29b",]


bucket_sample <- buckets[buckets$buckets %in% individual_buckets[100],]

maldo_row_ids <- paste0("doc-", mjedit_no_match$rowid)
socc_row_ids <- paste0("doc-", sjedit_no_match$rowid)
bucket_sample$maldo <- ifelse(bucket_sample$doc %in% maldo_row_ids, 0, 1)
any(bucket_sample$maldo, 2)
#%in% bucket_sample$maldo & 1 %in% bucket_sample$maldo

if (0 %in% bucket_sample$maldo & 1 %in% bucket_sample$maldo) {
  
}

table(bucket_sample$maldo)
bucket_sample$socc <- ifelse(bucket_sample$doc %in% socc_row_ids, 0, 1)
table(bucket_sample$socc)

test_file$anum <- gsub("^doc-*", "", test_file$a)
test_file$bnum <- gsub("^doc-*", "", test_file$b)

test_file_backup <- test_file
# Resetting the row numbers
# We always put Maldonado data first in the corpus, 
# so that the doc-1 matches row 1 from Maldonado and so on.
rownames(mjedit_no_match) <- NULL
mjedit_no_match$rowid <- rownames(mjedit_no_match)

# Soccminer is always placed after Maldonado into the corpus,
# so the first doc-number is number of rows in Maldonado plus 1
rownames(sjedit_no_match) <- (nrow(mjedit_no_match)+1):(nrow(mjedit_no_match) + nrow(sjedit_no_match))
sjedit_no_match$rowid <- rownames(sjedit_no_match)

test_file$maldo <- ifelse(test_file$anum %in% mjedit_no_match$rowid & test_file$bnum %in% mjedit_no_match$rowid, NA, "OK")
table(test_file$maldo)
test_file <- test_file[!is.na(test_file$maldo),]
test_file$socc <- ifelse(test_file$anum %in% sjedit_no_match$rowid & test_file$bnum %in% sjedit_no_match$rowid, NA, "OK")
table(test_file$socc)
test_file <- test_file[!is.na(test_file$socc),]
test_file <- test_file[,-c(4:7)]
scores <- scores %>% select(-maldo, -socc, -a, -b)

scores_part <- lsh_compare(test_file, total_corpus, jaccard_similarity, progress = TRUE)

################
#################



for(i in 1:max_length){
  #myfiles = do.call(rbind, lapply(files, function(x) read.csv2(x, stringsAsFactors = FALSE)))
  myfiles = do.call(rbind, lapply(files, function(x) readRDS(x)))
  myfiles <- unique(myfiles)
  scores_part <- lsh_compare(myfiles, total_corpus, jaccard_similarity, progress = TRUE)
  
  if (i == 1){
    scores <- scores_part
    
  } else {
    scores <- rbind(scores, scores_part)
    
  }
  
}

###########
# END OF MULTIPLE FILE PART
##########


#rm(total_corpus)
# Comparisons vs. pairwise matches:
# Ant: 1,025,144 /  1,244,390 = 82.38%
# Jmeter: 664,136 / 2,240,190 = 29.65%
# ArgoUML: / 38,252,487 = 
# Ant OLD: 1,038,823 / 1,249,500 = 83.1%