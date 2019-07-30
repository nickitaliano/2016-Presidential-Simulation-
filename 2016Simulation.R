#Run on AWS(in parallel)?

# README: the following code was written by Greg of the Yhat blog and edited by Nick Italiano.
# Author:Nick | namii
# Date: 06/21/2017
# Problem:
# Solution:Script scrapes essential core music for mobile djs
# Description:Numerical Analyses, Engineering/Probabalistic Simulation
# Objective:Active DSE Project/tut Review(see what I have and have not) and fundametal reinforcemnt....
# Domain/Tools:
# I/O:
# Results/Benefit:
# Publish/Blog:
# Acknowledgements: "Jasev/Teachers" and "Mentors/Code"
# More Info:
# Contact:

#overextender?validate data?

install.packages("XML")#install prior on Rserver?
install.packages("uuid")
install.packages("stringr")
install.packages("plyr")
install.packages("reshape2")
install.packages("Rcurl")
install.packages("ggplot2")
install.packages("httr")
install.packages("mapproj")
library(mapproj)
library(httr)
library(XML)
library(uuid)
library(stringr)
library(plyr)
library(reshape2)
library(ggplot2)
library(RCurl)
# Import Electoral College Data
f <- "https://raw.githubusercontent.com/chris-taylor/USElection/master/data/electoral-college-votes.csv"
electoral.college <- read.csv(f, header=FALSE)
names(electoral.college) <- c("state", "electoral_votes")
head(electoral.college)

# exclude D.C. from the data pull b/c there aren't any polls!. we'll add it in manually
states <- electoral.college$state[c(1:7, 9:51)]

# function retrives HTML/XML tabular data from "electionprojection.com" with all the poll results per state
# from a specified date period for Clinton v Trump v Johnson v Stein Schneider
results <- ldply(states, function(state) {
  
  url <- "https://www.electionprojection.com/latest-polls/national-presidential-polls-trump-vs-clinton-vs-johnson.php"
  state.fmt <- gsub(" ", "-", tolower(state))#state="alabama"
  url.state <- sprintf(url, state.fmt)
  print(url.state)
  tabs <- getURL(url.state)
  r <- readHTMLTable(tabs, stringsAsFactors=FALSE)[[3]]#4?
  r$state <- state
  r$id <- 1:(nrow(r))
  cols <- c("Dates", "Firm", "state", "Clinton", "Trump", "Johnson","id")#"Sample","Spread",
  r <- r[2:nrow(r),][,cols]
  r <- melt(r, id=c("Dates", "Firm", "state", "id"), variable.name="candidate", value.name="vote")
  #?melt?sim?
  names(r) <- c("date", "poll", "state", "id", "candidate", "vote")
  r$race <- ""
  cols <- c("date", "race", "state", "poll", "candidate", "vote", "id")
  r <- r[,cols]
  #r$vote<-gsub("[^0-9]", "", r$vote)#test
  r$vote <- as.numeric(r$vote)#NA's
  r
})
#levels(results$candidate)#=3 levels, data may be imported incorrectly
#data frame of
# adding D.C. on manually b/c it's slightly different. it also doesn't produce material changes to the
# results
results <- rbind(results, data.frame(
  date='10/20 - 10/28',
  race='',
  state='District of Columbia',
  poll='SurveyMonkey',
  candidate=c("Clinton", "Trump", "Johnson"),#Added my man Stein!,"Stein"
  vote=c(87, 5, 4),
  id=1
))
# viewing the data matrix in different forms for Gravis Marketing(R), Economist/ YouGov, Reuters/Ipsos, CNN/ORC, CBS News
# NBC News/SM
head(results)
tail(results)
table(results$candidate)# tablular results by each candidate has equal number of results
table(results$state)# """ results by state
results <- results[order(results$state, results$id, results$candidate),]

# allocate the results of each state into a new data frame and display # of polls there are per state
# the district of columbia has only 3 polls(added later)
poll.freq <- data.frame(table(results$state))
ggplot(poll.freq, aes(x=Var1, weight=Freq)) +
  geom_bar() + 
  coord_flip() + 
  scale_y_continuous("# of Polls") +
  scale_x_discrete("State", limits=rev(levels(poll.freq$Var1)))


# weight function for polls based on timing (functions)
weight <- function(i) {
  exp(1)*1 / exp(i)
}

# display of poll weighting value per nth poll
w <- data.frame(poll=1:8, weight=weight(1:8))
ggplot(w, aes(x=poll, weight=weight)) +
  geom_bar() +
  scale_x_continuous("nth poll", breaks=1:8) +
  scale_y_continuous("weight")


# election simlator is a nested function that inputs results for each candidate
# by state and by name and applies weight to estimate popular vote from polls.
election.sim <- function() {
  ddply(results, .(state), function(polls.state) {
    polls.state$.id <- NULL
    polls.state$id <- cumsum(!duplicated(polls.state$id))
    polls.state$weight <- weight(polls.state$id)
    polls.state$weighted_vote <- polls.state$vote * polls.state$weight
    
    tally <- ddply(polls.state, .(candidate), function(p) {
      r <- rnorm(nrow(p), 1, .15)
      data.frame(weighted_vote=sum(p$weighted_vote * r))
    })
    tally <- head(tally, 3)
    tally$estimated_popular_vote <- tally$weighted_vote / sum(tally$weighted_vote)
    tally
  })
}
# execute election simulator 
(election <- election.sim())

# display estimated popular vote for each candidate in a boxplot
colormap <- c(Clinton="#179ee0", Trump="#ff5d40", Johnson="#f6b900",Stein="#ffff00")#
ggplot(election, aes(x=candidate, weight=estimated_popular_vote, fill=candidate)) +
  geom_bar() +
  facet_wrap(~state) +
  scale_fill_manual(values=colormap) +
  scale_y_continuous(labels = scales::percent, breaks=c(0, 0.25, 0.5, 0.75, 1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

# Revolution Analytics package for parallel computing (makes the Montecarlo Integration go faster)
install.packages("parallel")
install.packages("doParallel")
install.packages("foreach")
library(foreach)
library(parallel)
library(doParallel)
detectCores()# detects numeber of cores in computer
doParallel::registerDoParallel(cores=4)

# function inndexes values from election simulator and declares a winner, and repeats 10000 times (takes ~5 mins)
simulated.state.results <- ldply(1:10000, function(i) {
  election <- election.sim()
  election.results <- dcast(election, state ~ candidate, value.var="estimated_popular_vote")
  election.results <- merge(election.results, electoral.college, by.x="state", by.y="state", all.x = TRUE)
  candidates <- c("Clinton", "Trump", "Johnson")
  election.results$winner <- candidates[max.col(election.results[,candidates])]
  election.results$sim_id <- UUIDgenerate()
  election.results
}, .progress="text", .parallel=FALSE)

# function determines the statewide votes
simulated.elections <- ddply(simulated.state.results, .(sim_id), function(simulation) {
  clinton <- sum(ifelse(simulation$Clinton > simulation$Trump & simulation$Clinton > simulation$Johnson, simulation$electoral_votes, 0))
  trump <- sum(ifelse(simulation$Clinton < simulation$Trump & simulation$Trump > simulation$Johnson, simulation$electoral_votes, 0))
  johnson <- sum(ifelse(simulation$Johnson > simulation$Trump & simulation$Johnson > simulation$Trump, simulation$electoral_votes, 0))
  data.frame(
    clinton=clinton,
    trump=trump,
    johnson=johnson,
    electoral_votes=simulated.state.results$electoral_votes,
    winner=ifelse(clinton > trump, "Clinton", "Trump")
    
  )
}, .progress="text", .parallel=FALSE)

table(simulated.elections$winner)
table(simulated.elections$winner) / nrow(simulated.elections)
summary(simulated.elections$clinton)
summary(simulated.elections$trump)
summary(simulated.elections$johnson)
head(simulated.elections)#Clinton doesnt win despite sim(neutral?)

ggplot(melt(simulated.elections[,1:4], id.vars = "sim_id"), aes(x=value, fill=variable)) +
  geom_histogram(position="identity", alpha=0.7) +
  scale_fill_manual(values=c(clinton="#179ee0", trump="#ff5d40", johnson="#f6b900"))
#f*** happened to johnson in the sim??

ggplot(melt(simulated.elections[,1:3], id.vars = "sim_id"), aes(x=value, fill=variable)) +
  geom_histogram(position="identity", alpha=0.7) +
  scale_fill_manual(values=c(clinton="#179ee0", trump="#ff5d40", johnson="#f6b900"))

ggplot(melt(simulated.elections[,1:3], id.vars = "sim_id"), aes(x=value, fill=variable)) +
  geom_density(position="identity", alpha=0.7) +
  scale_fill_manual(values=c(clinton="#179ee0", trump="#ff5d40", johnson="#f6b900"))

# popular vote(dot product?)????????????????????????????
clinton <- sum(simulated.elections$clinton * simulated.elections$electoral_votes)
#simulated.state.results$electoral_votes<--------->simulated.elections$electoral_votes(old data schema/format)
trump <- sum(simulated.elections$trump * simulated.elections$electoral_votes)
johnson <- sum(simulated.elections$johnson * simulated.elections$electoral_votes)
total <- sum(clinton, trump, johnson)
data.frame(Clinton=clinton/total, Trump=trump/total, Johnson=johnson/total)

simulated.state.results.agg <- ddply(simulated.state.results, .(state), function(state) {
  data.frame(
    state=state$state[1],
    trump=sum(state$winner=="Trump") / nrow(state),
    clinton=sum(state$winner=="Clinton") / nrow(state),
    johnson=sum(state$winner=="Johnson") / nrow(state),
    electoral_votes=state$electoral_votes[1],
    n=nrow(state)
  )
})
simulated.state.results.agg

data.frame(
  Trump=sum(simulated.state.results.agg$electoral_votes * simulated.state.results.agg$trump),
  Clinton=sum(simulated.state.results.agg$electoral_votes * simulated.state.results.agg$clinton),
  Johnson=sum(simulated.state.results.agg$electoral_votes * simulated.state.results.agg$johnson)
)


us.states <- map_data("state")
simulated.state.results.agg$state.mergecol <- as.character(tolower(simulated.state.results.agg$state))
state.plot <- merge(us.states, simulated.state.results.agg, by.x="region", by.y="state.mergecol")

table(us.states$region)
state.plot$winner <- ifelse(state.plot$trump > state.plot$clinton, "Trump", "Clinton")
# binary outcomes for states
ggplot(state.plot, aes(x=long, y=lat, group=group, fill=winner)) +
  geom_polygon(colour="white") +
  scale_fill_manual(values=colormap) +
  coord_map()

# shaded outcomes for states
ggplot(state.plot, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=trump), colour="grey10") +
  scale_fill_gradient2("", low=colormap["Clinton"], mid="white", high=colormap["Trump"], midpoint=0.5,
                       breaks=c(0, 0.5, 1), labels=c("Clinton", "?", "Trump")) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=12, face="bold")) +
  coord_map()

us.regions <- read.csv("/Users/nami/Projects/DSE/Simulations/2016 Presidential Election Monte Carlo:Parallel Computing Simulation/regions.csv")
#https://gist.github.com/bbroke/5880cd1bc785f8e2e153147d49ca63dd#file-regions-csv
state.plot <- merge(state.plot, us.regions, by.x="state", by.y="state")
head(state.plot)

# http://www.census.gov/econ/census/help/geography/regions_and_divisions.html
# http://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf

ddply(state.plot, .(division), function(div) {
  p <- ggplot(div, aes(x=long, y=lat)) +
    geom_polygon(aes(group=group, fill=trump), colour="grey10") +
    scale_fill_gradient2("", low=colormap["Clinton"], mid="white", high=colormap["Trump"], midpoint=0.5,
                         guide=FALSE) +
    theme_minimal() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.text=element_text(size=12, face="bold")) +
    coord_map() + 
    ggtitle(div$division[1])
  filename <- paste0("~/workspace/github.com/yhat/blog/static/img/election-region-", tolower(str_replace_all(div$division[1], " ", "-")), ".png")
  print(p)
  ggsave(filename)
  NULL
})