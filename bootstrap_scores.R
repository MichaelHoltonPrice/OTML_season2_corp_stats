rm(list=ls())

parse_results <- function(path_to_file) {
  # Parse the input results file, downloaded from the Google document as a
  # .csv file

  # We only need columns 5 through 16, which we will give names to after
  # reading in the file
  results <- read.csv(path_to_file, stringsAsFactors=FALSE, skip=1, header=FALSE)
  results <- results[,5:16]

  # Name the columns
  names(results) <- c("id1", "corp1", "score1",
                      "id2", "corp2", "score2",
                      "id3", "corp3", "score3",
                      "mc1","mc2","mc3")
  # Remove entries that are all missing
  rows_to_keep <- which(rowSums(is.na(results)) != 6)
  results <- results[rows_to_keep,]
}

create_score_matrix <- function(results) {
  # For each game (i.e., row in results) assign a score to each corporation,
  # accounting for ties.
  #
  # Points for 1st: 6
  # Points for 2nd: 3
  # Points for 3rd: 1

  # Iterate over rows to assign scores
  score_matrix <- matrix(NA, nrow(results), 3)
  for (r in 1:nrow(results)) {
    if (have_true_tie(results[r, "score1"], results[r, "score2"],
                      results[r, "mc1"]   , results[r, "mc2"])) {
      # Then there is a true tie for first
      if (have_true_tie(results[r, "score2"], results[r, "score3"],
                        results[r, "mc2"]   , results[r, "mc3"])) {
        # Then there is a true three-way tie
        score_matrix[r,] <- c(10/3,10/3,10/3)
      } else {
        # Then there is a true tie for first plus a third place
        score_matrix[r,] <- c(9/2,9/2,1)
      }
    } else {
      # Then there is not a true tie for first
      if (have_true_tie(results[r, "score2"], results[r, "score3"],
                        results[r, "mc2"]   , results[r, "mc3"])) {
        # Then there is a true tie for second plus a first place
        score_matrix[r,] <- c(6,4/2,4/2)
      } else {
        # Then there are distinct first, second, and third places
        score_matrix[r,] <- c(6,3,1)
      }
    }
  }
  return(score_matrix)
}

have_true_tie <- function(score1, score2, mc1, mc2) {
  # Are the players equal on score and mc?
  if (score1 == score2) {
    if (is.na(mc1) || is.na(mc2)) {
      # If either mc1 or mc2 is NA (i.e., blank; not sure why that would
      # happen) then assume this is not a true tie
      return(FALSE)
    }
    return(mc1 == mc2)
  } else {
    return(FALSE)
  }
}


calc_corp_mean_scores <- function(results, score_matrix, all_corps) {
  # Given a results matrix dataframe and score matrix, calculate the mean score
  # for each corporation. The results and score_matrix variables that are input
  # to this function are modified as part of the boostrapping. While there are
  # faster ways to calculate the mean scores using "vectorization", a for loop
  # is sufficient and makes the code reasonbly clear.
  #
  # all_corps, which is a vector of corporation names (including "Forfeit") is
  # input rather than calculated internally on the offchance that a given
  # corporation was not included in the bootstrapping
  num_corps <- length(all_corps)
  num_games  <- rep(0,num_corps)
  score_vect <- rep(0,num_corps)

  for (r in 1:nrow(score_matrix)) {
    # Determine the indices in all_corps of each pertinent corporation
    ind_corp1 <- which(results$corp1[r] == all_corps)
    ind_corp2 <- which(results$corp2[r] == all_corps)
    ind_corp3 <- which(results$corp3[r] == all_corps)

    # Increment the number of games for each pertinent corporation
    num_games[ind_corp1] <- num_games[ind_corp1] + 1
    num_games[ind_corp2] <- num_games[ind_corp2] + 1
    num_games[ind_corp3] <- num_games[ind_corp3] + 1

    # Increment the cumulative score for each pertinent corporation
    score_vect[ind_corp1] <- score_vect[ind_corp1] + score_matrix[r,1]
    score_vect[ind_corp2] <- score_vect[ind_corp2] + score_matrix[r,2]
    score_vect[ind_corp3] <- score_vect[ind_corp3] + score_matrix[r,3]
  }

  # If a corporation has zero games, set its mean to NA. These missing cases
  # are handled external to this function during the bootstrapping
  ind_ok <- which(num_games > 0)
  mean_score <- rep(NA, num_corps)
  mean_score[ind_ok] <- score_vect[ind_ok] / num_games[ind_ok]
  return(mean_score)
}

do_single_bootstrap <- function(results0, score_matrix0, all_corps) {
  # Do a single re-sample calculate the new mean scores vector, and return it.
  # results0 and score_matrix0 are the original inputs, with now resampling.
  ind_samp <- sample.int(nrow(results),nrow(results),replace=T)
  results      <-      results0[ind_samp,]
  score_matrix <- score_matrix0[ind_samp,]
  return(calc_corp_mean_scores(results, score_matrix, all_corps))
}

path_to_file <- "OTML_Season2_Results_2021_09_19.csv"
results <- parse_results(path_to_file)
score_matrix <- create_score_matrix(results)

# Create a vector of all corporations, and check that it is is complete. Do
# this using simple base R commands rather than, say, using testthat, so that
# no package dependencies exist.
all_corps <- sort(unique(c(results$corp1, results$corp2, results$corp3)))
known_corps <- c("Credicor",
                 "Ecoline",
                 "Forfeit",
                 "Helion",
                 "Interplanetary Cinematics",
                 "Inventrix",
                 "Mining Guild",
                 "Phobolog",
                 "Saturn Systems",
                 "Teractor",
                 "Tharsis Republic",
                 "Thorgate",
                 "United Nations Mars Initiative")
if (length(all_corps) != length(known_corps)) {
  stop("all_corps does not have the same length as known_corps")
}
for (n in 1:length(all_corps)) {
  if (all_corps[n] != known_corps[n]) {
    stop("Problem with all_corps versus known_corps")
  }
}

corp_outcomes <- calc_corp_mean_scores(results, score_matrix, all_corps)

# Set a seed for reproducibility
set.seed(10000)

# Do the bootstrapping using 1000 samples
num_samples <- 1000

# mean_score_samples is a list of lists. This is necessary because sometimes a
# corporation has zero games after sampling with replacement in the boostrap.
# It has num_corps, and each entry is the mean score for a bootstrap sample
# for the salient corporation. Each entry in the list can have a different
# length, but the length will be close to the number of samples, 1000.
mean_score_samples <- list()

# Initialize mean_score_samples with -1 for each entry, which corresponds to
# no observations
num_corps <- length(all_corps)
for (corp_num in 1:num_corps){
  mean_score_samples[[corp_num]] <- -1
}

for (s in 1:num_samples) {
  # Get the vector of mean scores (some entries can be NA)
  mean_score_vect <- do_single_bootstrap(results, score_matrix, all_corps)

  # For all entries that are not NA, append the observation
  for (corp_num in 1:num_corps) {
    if ( !is.na(mean_score_vect[corp_num])) {
      if(length(mean_score_samples[[corp_num]]) == 1) {
        # Then this may be the first sample
        if (mean_score_samples[[corp_num]] == -1) {
          # Then this is the first sample
          mean_score_samples[[corp_num]] <- mean_score_vect[corp_num]
        } else {
          # Then this is the second sample
          mean_score_samples[[corp_num]] <- c(mean_score_samples[[corp_num]],
                                              mean_score_vect[corp_num])
        }
      } else {
        # Then this is not the first sample
        mean_score_samples[[corp_num]] <- c(mean_score_samples[[corp_num]],
                                            mean_score_vect[corp_num])
      }
    }
  }
}

# Vector of corporation means (somewhat confusingly, the means of the mean
# scores)
final_means <- unlist(lapply(mean_score_samples,function(samp) {mean(samp)}))
# Vector of corporation standard deviations (somewhat confusingly, the standard
# deviations of the mean scores)
final_sds <- unlist(lapply(mean_score_samples,function(samp) {sd(samp)}))

# The first entry of ranks corresponds to the best corp, etc.
ranks <- rev(order(final_means))

# Reorder by rankj
final_means <- final_means[ranks]
final_sds   <- final_sds[ranks]
final_corps <- all_corps[ranks]

# Replace some long names in final_corps
final_corps[which(final_corps == "Interplanetary Cinematics")] <- "IC"
final_corps[which(final_corps == "Mining Guild")] <- "MG"
final_corps[which(final_corps == "Tharsis Republic")] <- "Tharsis"
final_corps[which(final_corps == "Saturn Systems")] <- "Saturn"
final_corps[which(final_corps == "United Nations Mars Initiative")] <- "UNMI"

# The following warning generate in the ensuing block of code is fine (it
# happens because the standard deviation of the Forfeit "corporation" is zero):
# Warning message:
# In arrows(x0 = 1:num_corps, y0 = final_means - final_sds, x1 = 1:num_corps,  :
#   zero-length arrow is of indeterminate angle and so skipped
pdf("otml_season2_corp_scores.pdf")
  plot(1:num_corps, final_means,
       ylim = range(c(min(final_means - final_sds),
                      max(final_means + final_sds))),
       pch = 16,
       xlab="",
       ylab="Score",
       xaxt="n")
  arrows(x0 = 1:num_corps, y0 = final_means - final_sds,
         x1 = 1:num_corps, y1 = final_means + final_sds,
         length = 0.15, code = 3, angle = 90)
  axis(1, at=1:num_corps, labels=final_corps, las=2)
dev.off()

# Also create a png
png("otml_season2_corp_scores.png", res=300, width=5, height=4, units="in")
  # Margin in inches with ordering bottom, left, top, right:
  par(mar=c(5,4,1,1))
  plot(1:num_corps, final_means,
       ylim = range(c(min(final_means - final_sds),
                      max(final_means + final_sds))),
       pch = 16,
       xlab="",
       ylab="Score",
       xaxt="n")
  arrows(x0 = 1:num_corps, y0 = final_means - final_sds,
         x1 = 1:num_corps, y1 = final_means + final_sds,
         length = 0.15, code = 3, angle = 90)
  axis(1, at=1:num_corps, labels=final_corps, las=2)
dev.off()