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

test <- m2017satd[m2017satd$clean_comment == "TODO perhaps make more specific than this no point parsing anything else e g GIF",]
test <- m2017satd[m2017satd$clean_comment == "We assume that every output jikes does stands for an error warning XXX Is this correct",]
table(m2017satd$classification)/62275*100
4071/62275

#soccminer10 <- as.data.frame(data.table::fread(file = "./data/soccminer_10_projects.csv", sep = ",", header = TRUE))
soccminer10 <- as.data.frame(data.table::fread(file = "./data/cleaned_soccminer_10_projects_all_data.csv", sep = ",", header = TRUE))
# Count how many empty columns there are in Soccminer
soccminer10 <- soccminer10[!(soccminer10$Clean_Comment_Content == ""),]

#Making sure that the projectnames match between dataframes by renaming hem
m2017satd$projectname[m2017satd$projectname == "argouml"] <- "argouml-VERSION_0_34"
soccminer10$Project[soccminer10$Project == "jEdit"] <- "jEdit-4.2"

# Done: 
# Jmeter
# Ant
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
project_name = "sql12"
mjedit <- m2017satd[m2017satd$projectname == project_name,]
mjedit <- mjedit[!duplicated(mjedit),]
mjedit <- mjedit[!(mjedit$clean_comment == ""),]

sjedit <- soccminer10[soccminer10$Project == project_name,]
sjedit <- sjedit[!duplicated(sjedit[c("Comment_Content")]),]
sjedit <- sjedit[!(sjedit$Clean_Comment_Content == ""),]

# Finding the direct matches based on solely on comment content
direct_matches <- inner_join(sjedit, mjedit, by=c("Clean_Comment_Content"="clean_comment"))
direct_matches$score <- 1

# Delete the duplicate comments
direct_matches <- direct_matches[!duplicated(direct_matches$Clean_Comment_Content),]

# Create the Maldonado comment (as this is neede later)
direct_matches$clean_comment = direct_matches$Clean_Comment_Content
# Keep only the columns which are kept in the end
direct_matches <- direct_matches[,c("score", "projectname", "classification", "commenttext", "clean_comment", "Clean_Comment_Content",
                                    "Comment_Content")]

# Removing the direct matches from the data
mjedit <- mjedit[!(mjedit$clean_comment %in% direct_matches$Clean_Comment_Content),]
sjedit <- sjedit[!(sjedit$Clean_Comment_Content %in% direct_matches$Clean_Comment_Content),]

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
    select(-clean_comment.x,-clean_comment.y)

#Join the rows which match soccminer
scores <- left_join(scores, sjedit[,c("Clean_Comment_Content", "Comment_Content", "rowid")], by=c("anum"="rowid"))
scores <- left_join(scores, sjedit[,c("Clean_Comment_Content", "Comment_Content", "rowid")], by=c("bnum"="rowid"))
scores <- scores %>% mutate(Clean_Comment_Content = coalesce(Clean_Comment_Content.x,Clean_Comment_Content.y)) %>% 
  select(-Clean_Comment_Content.x,-Clean_Comment_Content.y) %>%
  mutate(Comment_Content = coalesce(Comment_Content.x, Comment_Content.y)) %>% 
  select(-Comment_Content.x,-Comment_Content.y)

# Keep only the highest score matches for each comment pair
# since there is a possibility that one comment was matched with several possibilities.
scores <- scores %>%
  group_by(anum) %>%
  slice(which.max(score))

# Remove the rows, not anymore needed. Every comment which is similar is thought
# to hae the sames SATD classification.
scores <- scores %>% ungroup() %>% 
  select(-anum, -bnum)

scores <- full_join(direct_matches, scores)
scores$match <- ifelse(scores$score > 0.9, 1, "")

#Write the direct matches and scores to a file
write.csv2(scores, file = paste0("./save/", project_name, "_v4.csv"), row.names = FALSE)
table(scores$classification)



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
