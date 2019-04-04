#####################
# Loading Libraries #
#####################

## Function - Set Working Directory ##
set_working_directory <- function(dir_path) {
  
  if(!file.exists(dir_path)){ # Check If Directory Exist
    dir.create(dir_path) # Create Directory
  }
  
  setwd(dir_path) #Set Working Directory
  print(paste("Working Directory Set to :",dir_path)) # Print Message
}



## Block - Load Required Packages ##
install.packages("dplyr")
library(dplyr)
install.packages("sqldf")
library(sqldf)
install.packages("caret")
library(caret)
install.packages("ROCR")
library(ROCR)
install.packages("ModelMetrics")
library(ModelMetrics)
install.packages("randomForest")
library(randomForest)
install.packages("pROC")
library(pROC)
install.packages("kernlab")
library(kernlab)
install.packages("adabag")
library(adabag)
install.packages("plyr")
library(plyr)
install.packages("ranger")
library(ranger)
install.packages("caretEnsemble")
library(caretEnsemble)
install.packages("cowplot")
library(cowplot)
library(gridExtra)

## Block - Setting Working Directory ##
#set_working_directory("C:/Users/aless/Documents/CS 5100/ipl/Raw Files");
set_working_directory("C:/Users/aggarwal.k/Downloads/AI_Project/Raw Files");

##-------------------------------------------------------------------------------
##-------------------------------------------------------------------------------
# 1. Data Acquisition
##-------------------------------------------------------------------------------
deliveries_raw <- read.csv("deliveries.csv")
matches_raw <- read.csv("matches.csv")
match_stats_raw <- read.csv("match_stats.csv")
teamstr <- read.csv("TeamStr.csv")

##-------------------------------------------------------------------------------
# 2. Data Cleaning
##-------------------------------------------------------------------------------


matches_raw <- matches_raw %>% select (-c(umpire1,umpire2,umpire3))

matches_raw <- data.frame(lapply(matches_raw, function(x) {x<-gsub("Sunrisers Hyderabad", "SRH", x)}))
matches_raw <- data.frame(lapply(matches_raw, function(x) {x<-gsub("Pune Warriors", "PW", x)}))
matches_raw <- data.frame(lapply(matches_raw, function(x) {x<-gsub("Kolkata Knight Riders", "KKR", x)}))
matches_raw <- data.frame(lapply(matches_raw, function(x) {x<-gsub("Royal Challengers Bangalore", "RCB", x)}))
matches_raw <- data.frame(lapply(matches_raw, function(x) {x<-gsub("Deccan Chargers", "DC", x)}))
matches_raw <- data.frame(lapply(matches_raw, function(x) {x<-gsub("Chennai Super Kings", "CSK", x)}))
matches_raw <- data.frame(lapply(matches_raw, function(x) {x<-gsub("Rajasthan Royals", "RR", x)}))
matches_raw <- data.frame(lapply(matches_raw, function(x) {x<-gsub("Delhi Daredevils", "DD", x)}))
matches_raw <- data.frame(lapply(matches_raw, function(x) {x<-gsub("Gujarat Lions", "GL", x)}))
matches_raw <- data.frame(lapply(matches_raw, function(x) {x<-gsub("Kings XI Punjab", "KXIP", x)}))
matches_raw <- data.frame(lapply(matches_raw, function(x) {x<-gsub("Rising Pune Supergiants", "RPS", x)}))
matches_raw <- data.frame(lapply(matches_raw, function(x) {x<-gsub("Rising Pune Supergiant", "RPS", x)}))
matches_raw <- data.frame(lapply(matches_raw, function(x) {x<-gsub("Kochi Tuskers Kerala", "KTK", x)}))
matches_raw <- data.frame(lapply(matches_raw, function(x) {x<-gsub("Mumbai Indians", "MI", x)}))

deliveries_raw <- data.frame(lapply(deliveries_raw, function(x) {x<-gsub("Sunrisers Hyderabad", "SRH", x)}))
deliveries_raw <- data.frame(lapply(deliveries_raw, function(x) {x<-gsub("Pune Warriors", "PW", x)}))
deliveries_raw <- data.frame(lapply(deliveries_raw, function(x) {x<-gsub("Kolkata Knight Riders", "KKR", x)}))
deliveries_raw <- data.frame(lapply(deliveries_raw, function(x) {x<-gsub("Royal Challengers Bangalore", "RCB", x)}))
deliveries_raw <- data.frame(lapply(deliveries_raw, function(x) {x<-gsub("Deccan Chargers", "DC", x)}))
deliveries_raw <- data.frame(lapply(deliveries_raw, function(x) {x<-gsub("Chennai Super Kings", "CSK", x)}))
deliveries_raw <- data.frame(lapply(deliveries_raw, function(x) {x<-gsub("Rajasthan Royals", "RR", x)}))
deliveries_raw <- data.frame(lapply(deliveries_raw, function(x) {x<-gsub("Delhi Daredevils", "DD", x)}))
deliveries_raw <- data.frame(lapply(deliveries_raw, function(x) {x<-gsub("Gujarat Lions", "GL", x)}))
deliveries_raw <- data.frame(lapply(deliveries_raw, function(x) {x<-gsub("Kings XI Punjab", "KXIP", x)}))
deliveries_raw <- data.frame(lapply(deliveries_raw, function(x) {x<-gsub("Rising Pune Supergiants", "RPS", x)}))
deliveries_raw <- data.frame(lapply(deliveries_raw, function(x) {x<-gsub("Rising Pune Supergiant", "RPS", x)}))
deliveries_raw <- data.frame(lapply(deliveries_raw, function(x) {x<-gsub("Kochi Tuskers Kerala", "KTK", x)}))
deliveries_raw <- data.frame(lapply(deliveries_raw, function(x) {x<-gsub("Mumbai Indians", "MI", x)}))

deliveries <- deliveries_raw
matches <- sqldf("select * from matches_raw where season < 2017 AND result <> 'no result'")
match_stats <- sqldf("select * from match_stats_raw where season < 2017")


matches_raw <- 
  sqldf("select m1.Id,m1.season,m1.city,m1.date
        ,CASE WHEN m1.team1 = ts.Team1 AND m1.team2 = ts.Team2
        THEN ts.Team1
        WHEN m1.team1 = ts.Team2 AND m1.team2 = ts.Team1
        THEN ts.Team1
        END AS team1
        
        ,CASE WHEN m1.team1 = ts.Team1 AND m1.team2 = ts.Team2
        THEN ts.Team2
        WHEN m1.team1 = ts.Team2 AND m1.team2 = ts.Team1
        THEN ts.Team2
        END AS team2
        
        ,m1.toss_winner,m1.toss_decision,m1.result,m1.dl_applied,m1.winner,m1.win_by_runs,m1.win_by_wickets,m1.player_of_match
        ,m1.venue
        FROM matches_raw m1
        INNER JOIN teamstr ts ON (m1.team1 = ts.Team1 AND m1.team2 = ts.Team2) OR (m1.team1 = ts.Team2 AND m1.team2 = ts.Team1)
        INNER JOIN matches_raw m2 ON m1.Id = m2.Id
        ")



##-------------------------------------------------------------------------------
# 3. Data Exploration and New Feature Designing
##-------------------------------------------------------------------------------

##Team Stats

team_batting_stats <- sqldf("SELECT season as Season, batting_team AS Team
                            ,SUM(CASE WHEN extra_runs <> 0 THEN extra_runs ELSE NULL END) as Extras
                            ,COUNT(CASE WHEN batsman_runs = 1 THEN batsman_runs ELSE NULL END) as Singles
                            ,COUNT(CASE WHEN batsman_runs = 2 THEN batsman_runs ELSE NULL END) as Twos
                            ,COUNT(CASE WHEN batsman_runs = 3 THEN batsman_runs ELSE NULL END) as Threes
                            ,COUNT(CASE WHEN batsman_runs = 4 THEN batsman_runs ELSE NULL END) as Fours
                            ,COUNT(CASE WHEN batsman_runs = 6 THEN batsman_runs ELSE NULL END) as Sixs
                            ,SUM(CASE WHEN is_super_over = 1 THEN total_runs ELSE NULL END) as Super_Over_Runs
                            ,SUM(total_runs) as Total_Runs
                            FROM deliveries d
                            INNER JOIN matches m ON d.match_id = m.id
                            GROUP BY season,batting_team")

team_batting_stats_d <- sqldf("SELECT season as Season, batting_team AS Team
                              ,count (total_runs) as Dots
                              FROM deliveries d
                              INNER JOIN matches m ON d.match_id = m.id
                              WHERE total_runs = 0
                              GROUP BY season,batting_team")

team_batting_stats <- sqldf("SELECT t.*,d.Dots FROM team_batting_stats t
                            INNER JOIN team_batting_stats_d d ON d.Season = t.Season AND d.Team = t.Team")


team_bowling_stats <- sqldf("SELECT season as Season, bowling_team AS Team
                            ,count(player_dismissed) as Wickets_Taken
                            FROM deliveries d
                            INNER JOIN matches m ON d.match_id = m.id
                            WHERE player_dismissed <> '' 
                            AND dismissal_kind NOT IN ('retired hurt','obstructing the field')
                            GROUP BY season,bowling_team")

team_stats <- sqldf("SELECT bt.*,bo.Wickets_Taken
                    FROM team_batting_stats bt
                    INNER JOIN team_bowling_stats bo ON bt.Season = bo.Season AND bt.Team = bo.Team")

#Replacing NA for Super Overs to 0
team_stats$Super_Over_Runs <- replace(team_stats$Super_Over_Runs,is.na(team_stats$Super_Over_Runs),0)


team_overall_stats_td <- sqldf("SELECT season as Season,toss_winner as Team
                               ,COUNT(id) as Toss_Wins
                               ,SUM(CASE WHEN toss_decision = 'bat' THEN 1 ELSE 0 END) as Bat_Toss
                               ,SUM(CASE WHEN toss_decision = 'field' THEN 1 ELSE 0 END) as Field_Toss
                               FROM matches GROUP BY season,toss_winner")

team_overall_stats_b <- sqldf("SELECT season as Season,batting_team as Team
                              ,COUNT (DISTINCT match_id) Bat_First
                              FROM deliveries d
                              INNER JOIN matches m ON m.id = d.match_id 
                              WHERE inning = 1
                              GROUP BY season,batting_team")

team_overall_stats_f <- sqldf("SELECT season as Season,bowling_team as Team
                              ,COUNT (DISTINCT match_id) Field_First
                              FROM deliveries d
                              INNER JOIN matches m ON m.id = d.match_id 
                              WHERE inning = 1
                              GROUP BY season,bowling_team")

team_overall_stats <- sqldf("SELECT b.*,f.Field_First,Bat_Toss,Field_Toss
                            FROM team_overall_stats_b b
                            INNER JOIN team_overall_stats_f f ON b.Season = f.Season AND b.Team = f.Team
                            INNER JOIN team_overall_stats_td td ON td.Season = f.Season
                            AND td.Team = f.Team")


team_overall_stats_wins <- sqldf("SELECT season as Season,winner as Team
                                 ,AVG(CASE WHEN win_by_runs <> 0 THEN win_by_runs ELSE NULL END) AS Win_by_Runs_avg
                                 ,AVG(CASE WHEN win_by_wickets <> 0 THEN win_by_wickets ELSE NULL END) AS Win_by_Wickets_avg
                                 FROM matches
                                 GROUP BY season,winner")

team_overall_stats_wins <- sqldf("SELECT * 
                                 FROM team_overall_stats_wins
                                 WHERE Win_by_Runs_avg IS NOT NULL OR Win_by_Wickets_avg IS NOT NULL")

#Replacing NA in team_overall_stats_wins to 0
team_overall_stats_wins$Win_by_Runs_avg <- replace(team_overall_stats_wins$Win_by_Runs_avg,is.na(team_overall_stats_wins$Win_by_Runs_avg),0)
team_overall_stats_wins$Win_by_Wickets_avg <- replace(team_overall_stats_wins$Win_by_Wickets_avg,is.na(team_overall_stats_wins$Win_by_Wickets_avg),0)


team_overall_stats <- sqldf("SELECT o.*,sw.Win_by_Runs_avg,sw.Win_by_Wickets_avg
                            FROM team_overall_stats o
                            INNER JOIN team_overall_stats_wins sw ON o.Season = sw.Season AND o.Team = sw.Team")


team_overall_stats <- sqldf("SELECT o.*,t.Extras,t.Dots,t.Singles,t.Twos,t.Threes,t.Fours,t.Sixs,t.Total_Runs,t.Wickets_Taken
                            FROM team_stats t
                            INNER JOIN team_overall_stats o ON o.Season = t.Season AND o.Team = t.Team")


stats_team <- sqldf("SELECT o.*,M,W,L,NR,Pts,NRR
                    FROM team_overall_stats o
                    INNER JOIN match_stats m ON m.season = o.Season AND o.Team = m.team")


#head(stats_team)

#Summary of Total_Runs vs Wickets Taken vs Wins

Total_Runs <- sqldf("SELECT Team,sum(Sixs) AS Total_Runs 
              FROM stats_team s
              GROUP BY Team
              ORDER BY 2 desc
              ")

Wicks <- sqldf("SELECT Team,sum(Wickets_Taken) AS Wicks 
              FROM stats_team s
              GROUP BY Team
              ORDER BY 2 desc
              ")

wins <- sqldf("SELECT Team,sum(W) AS Wins 
              FROM stats_team s
              GROUP BY Team
              ORDER BY 2 desc
              ")

plot_Total_Runs <- ggplot(data=Total_Runs, aes(x=factor(Team, levels = Team[order(Total_Runs)]),y=Total_Runs,fill=Total_Runs,label = Total_Runs)) +
  geom_col() + geom_text(nudge_y = 30) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0,18000,3000)) +
  labs(x="Teams",y="Total_Runs", title="Summary of Total_Runs")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

plot_Wicks <- ggplot(data=Wicks, aes(x=factor(Team, levels = Team[order(Wicks)]),y=Wicks,fill=Wicks,label = Wicks)) +
  geom_col() + geom_text(nudge_y = 30) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0,18000,3000)) +
  labs(x="Teams",y="Wickets Taken", title="Summary of Wickets Taken")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

plot_wins <- ggplot(data=wins, aes(x=factor(Team, levels = Team[order(Wins)]),y=Wins,fill=Wins,label = Wins)) +
  geom_col() + geom_text(nudge_y = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0,18000,3000)) +
  labs(x="Teams",y="Wins", title="Summary of Wins")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

grid.arrange(plot_Total_Runs,plot_Wicks, plot_wins, ncol=3)


#---------------------------------------------------------------------------------------------------------------------

#****************
# Indices Match #
#****************

batting_index <- sqldf("Select match_id AS Id
                       ,batting_team AS Team_Bat
                       ,bowling_team as Team_Bowl
                       ,(sum(total_runs)/(cast((count(ball)*6) as float))) as Batting_Run_Rate
                       ,CAST(sum(total_runs) AS FLOAT)/CAST(count(player_dismissed) as FLOAT) as Batting_Average
                       ,COUNT(CASE WHEN player_dismissed <> '' AND dismissal_kind NOT IN ('retired hurt','obstructing the field') 
                       THEN player_dismissed ELSE NULL END)
                       / CAST(COUNT(ball) AS FLOAT) AS Batting_Wicket_Rate
                       ,(sum(total_runs)/cast(count(ball)/6 as float)) * CAST(sum(total_runs) AS FLOAT)
                       / CAST(count(player_dismissed) as FLOAT) as Batting_Index
                       FROM deliveries
                       GROUP BY match_id, batting_team,bowling_team
                       ORDER BY match_id")

batting_index <- sqldf("Select b.Id 
                       ,b.Team_Bat
                       ,b.Team_Bowl
                       ,b.Batting_Run_Rate AS Team_Bat_Batting_Run_Rate
                       ,b.Batting_Average AS Team_Bat_Batting_Average
                       ,b.Batting_Wicket_Rate AS Team_Bat_Batting_Wicket_Rate
                       ,b.Batting_Index AS Team_Bat_Batting_Index
                       ,bo.Batting_Run_Rate AS Team_Bowl_Batting_Run_Rate
                       ,bo.Batting_Average AS Team_Bowl_Batting_Average
                       ,bo.Batting_Wicket_Rate AS Team_Bowl_Batting_Wicket_Rate
                       ,bo.Batting_Index AS Team_Bowl_Batting_Index
                       FROM batting_index b
                       INNER JOIN batting_index bo on bo.Id = b.Id
                       AND bo.Team_Bowl = b.Team_Bat
                       AND b.Team_Bowl = bo.Team_Bat
                       ORDER BY 1
                       ")

odd_indexes<-seq(1,1270,2)
batting_index <- batting_index[odd_indexes,]


bowling_index <- sqldf("Select match_id AS Id
                       ,bowling_team AS Team_Bat
                       ,batting_team as Team_Bowl
                       ,(SUM(total_runs)/CAST(MAX(over) AS FLOAT)) as Bowling_Economy_Rate
                       ,CAST(sum(total_runs) AS FLOAT)/CAST(count(player_dismissed) as FLOAT) as Bowling_Average
                       ,COUNT(CASE WHEN player_dismissed <> '' AND dismissal_kind NOT IN ('retired hurt','obstructing the field') 
                       THEN player_dismissed ELSE NULL END)
                       /CAST(COUNT(ball) AS FLOAT) AS Bowling_Strike_Rate
                       ,(SUM(total_runs)/CAST(MAX(over) AS FLOAT)) * CAST(sum(total_runs) AS FLOAT)
                       / CAST(count(player_dismissed) as FLOAT) as Bowling_Index
                       FROM deliveries
                       GROUP BY match_id, bowling_team,batting_team
                       ORDER BY match_id")

bowling_index <- sqldf("Select b.Id 
                       ,b.Team_Bat
                       ,b.Team_Bowl
                       ,b.Bowling_Economy_Rate AS Team_Bat_Bowling_Economy_Rate
                       ,b.Bowling_Average AS Team_Bat_Bowling_Average
                       ,b.Bowling_Strike_Rate AS Team_Bat_Bowling_Strike_Rate
                       ,b.Bowling_Index AS Team_Bat_Bowling_Index
                       ,bo.Bowling_Economy_Rate AS Team_Bowl_Bowling_Economy_Rate
                       ,bo.Bowling_Average AS Team_Bowl_Bowling_Average
                       ,bo.Bowling_Strike_Rate AS Team_Bowl_Bowling_Strike_Rate
                       ,bo.Bowling_Index AS Team_Bowl_Bowling_Index
                       FROM bowling_index b
                       INNER JOIN bowling_index bo on bo.Id = b.Id
                       AND bo.Team_Bowl = b.Team_Bat
                       AND b.Team_Bowl = bo.Team_Bat
                       ORDER BY 1
                       ")

bowling_index <- bowling_index [odd_indexes,]



Match_Indices <- sqldf("SELECT Season,bt.*,Team_Bat_Bowling_Economy_Rate,Team_Bat_Bowling_Average,Team_Bat_Bowling_Strike_Rate,Team_Bat_Bowling_Index,
                       Team_Bowl_Bowling_Economy_Rate,Team_Bowl_Bowling_Average,Team_Bowl_Bowling_Strike_Rate,Team_Bowl_Bowling_Index,
                       CASE WHEN winner <> bt.Team_Bat THEN 0 ELSE 1 END AS Winner
                       FROM batting_index bt
                       INNER JOIN bowling_index bo ON bt.Id = bo.Id AND bt.Team_Bat = bo.Team_Bat  AND bt.Team_Bowl = bo.Team_Bowl
                       INNER JOIN matches m ON bt.Id = m.Id
                       ORDER by bt.Id")




indices_match <- Match_Indices
#head(indices_match, 5)

indices_match <- 
  sqldf("select m.season,m.Id
        ,CASE WHEN Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2
        THEN ts.Team1
        WHEN Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1
        THEN ts.Team1
        END AS Team1
        
        ,CASE WHEN Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2
        THEN ts.Team2
        WHEN Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1
        THEN ts.Team2
        END AS Team2
        
        ,CASE WHEN Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2
        THEN Team_Bat_Batting_Run_Rate
        WHEN Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1
        THEN Team_Bowl_Batting_Run_Rate
        END AS Team1_Batting_Run_Rate
        
        ,CASE WHEN Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2
        THEN Team_Bat_Batting_Average
        WHEN Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1
        THEN Team_Bowl_Batting_Average
        END AS Team1_Batting_Average
        
        ,CASE WHEN Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2
        THEN Team_Bat_Batting_Wicket_Rate
        WHEN Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1
        THEN Team_Bowl_Batting_Wicket_Rate
        END AS Team1_Batting_Wicket_Rate
        
        ,CASE WHEN Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2
        THEN Team_Bat_Batting_Index
        WHEN Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1
        THEN Team_Bowl_Batting_Index
        END AS Team1_Batting_Index
        
        ,CASE WHEN Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2
        THEN Team_Bowl_Batting_Run_Rate
        WHEN Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1
        THEN Team_Bat_Batting_Run_Rate
        END AS Team2_Batting_Run_Rate
        
        ,CASE WHEN Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2
        THEN Team_Bowl_Batting_Average
        WHEN Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1
        THEN Team_Bat_Batting_Average
        END AS Team2_Batting_Average
        
        ,CASE WHEN Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2
        THEN Team_Bowl_Batting_Wicket_Rate
        WHEN Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1
        THEN Team_Bat_Batting_Wicket_Rate
        END AS Team2_Batting_Wicket_Rate
        
        ,CASE WHEN Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2
        THEN Team_Bowl_Batting_Index
        WHEN Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1
        THEN Team_Bat_Batting_Index
        END AS Team2_Batting_Index     
        
        
        
        
        ,CASE WHEN Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2
        THEN Team_Bat_Bowling_Economy_Rate
        WHEN Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1
        THEN Team_Bowl_Bowling_Economy_Rate
        END AS Team1_Bowling_Economy_Rate
        
        ,CASE WHEN Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2
        THEN Team_Bat_Bowling_Average
        WHEN Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1
        THEN Team_Bowl_Bowling_Average
        END AS Team1_Bowling_Average
        
        ,CASE WHEN Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2
        THEN Team_Bat_Bowling_Strike_Rate
        WHEN Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1
        THEN Team_Bowl_Bowling_Strike_Rate
        END AS Team1_Bowling_Strike_Rate
        
        ,CASE WHEN Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2
        THEN Team_Bat_Bowling_Index
        WHEN Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1
        THEN Team_Bowl_Bowling_Index
        END AS Team1_Bowling_Index
        
        ,CASE WHEN Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2
        THEN Team_Bowl_Bowling_Economy_Rate
        WHEN Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1
        THEN Team_Bat_Bowling_Economy_Rate
        END AS Team2_Bowling_Economy_Rate
        
        ,CASE WHEN Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2
        THEN Team_Bowl_Bowling_Average
        WHEN Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1
        THEN Team_Bat_Bowling_Average
        END AS Team2_Bowling_Average
        
        ,CASE WHEN Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2
        THEN Team_Bowl_Bowling_Strike_Rate
        WHEN Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1
        THEN Team_Bat_Bowling_Strike_Rate
        END AS Team2_Bowling_Strike_Rate
        
        ,CASE WHEN Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2
        THEN Team_Bowl_Bowling_Index
        WHEN Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1
        THEN Team_Bat_Bowling_Index
        END AS Team2_Bowling_Index 
        
        
        ,CASE WHEN m.winner <> ts.Team1 
        THEN 0 
        ELSE 1 END AS Winner
        
        
        FROM indices_match i1
        INNER JOIN teamstr ts ON (Team_Bat = ts.Team1 AND Team_Bowl = ts.Team2) OR (Team_Bat = ts.Team2 AND Team_Bowl = ts.Team1)
        INNER JOIN matches m ON i1.Id = m.Id
        --WHERE Id = 171
        ")

#---------------

#***************
# Indices Team #
#***************

indices_team <- sqldf("SELECT im.Team1, im.Team2
                      ,avg(Team1_Batting_Run_Rate) AS Team1_Batting_Run_Rate
                      ,avg(Team1_Batting_Average) AS Team1_Batting_Average
                      ,avg(Team1_Batting_Wicket_Rate) AS Team1_Batting_Wicket_Rate
                      ,avg(Team1_Batting_Index) AS Team1_Batting_Index
                      ,avg(Team2_Batting_Run_Rate) AS Team2_Batting_Run_Rate
                      ,avg(Team2_Batting_Average) AS Team2_Batting_Average
                      ,avg(Team2_Batting_Wicket_Rate) AS Team2_Batting_Wicket_Rate
                      ,avg(Team2_Batting_Index) AS Team2_Batting_Index
                      ,avg(Team1_Bowling_Economy_Rate) AS Team1_Bowling_Economy_Rate
                      ,avg(Team1_Bowling_Average) AS Team1_Bowling_Average
                      ,avg(Team1_Bowling_Strike_Rate) AS Team1_Bowling_Strike_Rate
                      ,avg(Team1_Bowling_Index) AS Team1_Bowling_Index
                      ,avg(Team2_Bowling_Economy_Rate) AS Team2_Bowling_Economy_Rate
                      ,avg(Team2_Bowling_Average) AS Team2_Bowling_Average
                      ,avg(Team2_Bowling_Strike_Rate) AS Team2_Bowling_Strike_Rate
                      ,avg(Team2_Bowling_Index) AS Team2_Bowling_Index 
                      FROM indices_match im
                      INNER JOIN matches m on im.id = m.id
                      GROUP BY im.Team1, im.Team2")

#head(indices_team,5)

##-------------------------------------------------------------------------------
# Removing Objects
##-------------------------------------------------------------------------------

rm(team_batting_stats)
rm(team_batting_stats_d)
rm(team_bowling_stats)
rm(team_overall_stats_td)
rm(team_overall_stats_b)
rm(team_overall_stats_f)
rm(team_overall_stats_wins)
rm(team_overall_stats)
rm(team_stats)
rm(matches)
rm(deliveries)
rm(deliveries_raw)
rm(match_stats)
rm(match_stats_raw)
rm(batting_index)
rm(bowling_index)
rm(Match_Indices)
##-------------------------------------------------------------------------------
# File Export
##-------------------------------------------------------------------------------

#set_working_directory("C:/Users/aless/Documents/CS 5100/ipl/New Stats");
set_working_directory("C:/Users/aggarwal.k/Downloads/AI_Project/New Stats");
#******************
# Team Statistics #
#******************
write.csv(stats_team,"stats_team.csv")

#****************
# Match Indices #
#****************
write.csv(indices_match,"indices_match.csv")

#***************
# Team Indices #
#***************
write.csv(indices_team,"indices_team.csv")

#set_working_directory("C:/Users/aless/Documents/CS 5100/ipl/Raw Files");
set_working_directory("C:/Users/aggarwal.k/Downloads/AI_Project/Raw Files");

##-------------------------------------------------------------------------------
# 5. Model Preperation
##-------------------------------------------------------------------------------

#Spliting data set into training and validation

test_set <- sqldf("select i.*
                         ,CASE WHEN winner <> m.Team1 THEN 0 ELSE 1 END AS Winner
                         FROM matches_raw m
                         INNER JOIN indices_team i ON (i.Team1 = m.team1 AND i.Team2 = m.team2) 
                         WHERE m.season > 2016 AND result <> 'no result'")

test_set <- test_set[-c(1,2)]

indices_match <- indices_match %>% 
  filter(Team1!='GL', Team1!='RPS')
indices_match <- indices_match %>% 
  filter(Team2!='GL', Team2!='RPS')

indices_match <- indices_match[-c(1,2,3,4)]

set.seed(123)
spec <- c(train = .7, validation = .3)

g <- sample(cut(
  seq(nrow(indices_match)), 
  nrow(indices_match)*cumsum(c(0,spec)),
  labels = names(spec)
))

set.seed(123)
res <- split(indices_match, g)
train_set <- res$train
val_set <- res$validation

ctrl <- trainControl(method="repeatedcv", number=10, repeats=3)

#Normalization

train_set[, 1:16] <- lapply(train_set[, 1:16], function(x) (x-min(x))/(max(x)-min(x)))
train_set_fac <- train_set
train_set_fac$Winner <- as.factor(train_set_fac$Winner)

val_set[, 1:16] <- lapply(val_set[, 1:16], function(x) (x-min(x))/(max(x)-min(x)))
val_set_fac <- val_set
val_set_fac$Winner <- as.factor(val_set_fac$Winner)

test_set[, 1:16] <- lapply(test_set[, 1:16], function(x) (x-min(x))/(max(x)-min(x)))
test_set_fac <- test_set
test_set_fac$Winner <- as.factor(test_set_fac$Winner)

#Implementation of Principal Component Analysis

prin_comp <- prcomp(train_set[, 1:16], scale. = T)
names(prin_comp)

#loadings
prin_comp$rotation

std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")


train_set_pca <- data.frame(Winner = train_set$Winner,prin_comp$x)

#we are interested in first 5 PCA explaining more than 90% variance

train_set_pca  <- train_set_pca [,1:6]
train_set_pca_fac <- train_set_pca
train_set_pca_fac$Winner <- as.factor(train_set_pca_fac$Winner)

val_set_pca <- predict(prin_comp, newdata = val_set[,1:16])
val_set_pca <- data.frame(Winner = val_set$Winner,val_set_pca)
val_set_pca <- val_set_pca[,1:6]
val_set_pca_fac <- val_set_pca
val_set_pca_fac$Winner <- as.factor(val_set_pca_fac$Winner)

test_set_pca <- predict(prin_comp, newdata = test_set[,1:16])
test_set_pca <- data.frame(Winner = test_set$Winner,test_set_pca)
test_set_pca <- test_set_pca[,1:6]
test_set_pca_fac <- test_set_pca
test_set_pca_fac$Winner <- as.factor(test_set_pca_fac$Winner)


##-------------------------------------------------------------------------------
# 5. Model Construction
##-------------------------------------------------------------------------------


##Logistic Regression
#Validation Accuracy: 83.54% (AUC: 0.999)
#Test Accuracy: 66.1% (AUC: 0.999)

log_model_raw <- glm(Winner~., data=train_set,  family=binomial )
summary(log_model_raw)

#Removing Team1_Batting_Average,Team2_Batting_Average,Team1_Bowling_Average,Team1_Bowling_Strike_Rate,Team2_Bowling_Average,Team2_Bowling_Strike_Rate
log_model <- glm(Winner~Team1_Batting_Run_Rate+Team1_Batting_Wicket_Rate+Team1_Batting_Index+Team2_Batting_Run_Rate+Team2_Batting_Wicket_Rate+Team2_Batting_Index
                 +Team1_Bowling_Economy_Rate+Team1_Bowling_Index+Team2_Bowling_Economy_Rate+Team2_Bowling_Index, data=train_set,  family=binomial )
summary(log_model)

#Removing Team2_Bowling_Economy_Rate
log_model <- glm(Winner~Team1_Batting_Run_Rate+Team1_Batting_Wicket_Rate+Team1_Batting_Index+Team2_Batting_Run_Rate+Team2_Batting_Wicket_Rate+Team2_Batting_Index
                 +Team1_Bowling_Economy_Rate+Team1_Bowling_Index+Team2_Bowling_Index, data=train_set,  family=binomial )
summary(log_model)

#Removing Team1_Batting_Run_Rate
log_model <- glm(Winner~Team1_Batting_Wicket_Rate+Team1_Batting_Index+Team2_Batting_Run_Rate+Team2_Batting_Wicket_Rate+Team2_Batting_Index
                 +Team1_Bowling_Economy_Rate+Team1_Bowling_Index+Team2_Bowling_Index, data=train_set,  family=binomial )
summary(log_model)

#Removing Team1_Batting_Wicket_Rate
log_model <- glm(Winner~Team1_Batting_Index+Team2_Batting_Run_Rate+Team2_Batting_Wicket_Rate+Team2_Batting_Index
                 +Team1_Bowling_Economy_Rate+Team1_Bowling_Index+Team2_Bowling_Index, data=train_set,  family=binomial )
summary(log_model)

#Validation Accuracy
pred <- predict(log_model,val_set,type = "response")
pred <- ifelse(pred > 0.8,1,0)

caret::confusionMatrix(data = as.factor(pred), reference = as.factor(val_set$Winner))

#ROC
ROCRpred <- prediction(pred, val_set$Winner)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#AUC
#We can label this an excellent classisfier as its doing a good job in the classification with AUC of 0.999 
#which is significanlty higher than the other models
ModelMetrics::auc(log_model)


#With PCA
#Validation Accuracy: 91.46% (AUC: 0.998)
#Test Accuracy: 66.1% (AUC: 0.998)

log_model_pca_raw <- glm(Winner~., data=train_set_pca,  family=binomial)
summary(log_model_pca_raw)

#Removing PC5
log_model_pca <- glm(Winner~PC1+PC2+PC3+PC4, data=train_set_pca,  family=binomial)
summary(log_model_pca)


pred_pca <- predict(log_model_pca,val_set_pca,type = "response")
pred_pca <- ifelse(pred_pca > 0.8,1,0)

caret::confusionMatrix(data = as.factor(pred_pca), reference = as.factor(val_set_pca$Winner))

#ROC
ROCRpred_pca <- prediction(pred_pca, val_set_pca$Winner)
ROCRperf_pca <- performance(ROCRpred_pca, 'tpr','fpr')
plot(ROCRperf_pca, colorize = TRUE, text.adj = c(-0.2,1.7))

#AUC
ModelMetrics::auc(log_model_pca)



##Random Forest
#Validation Accuracy: 75% (AUC: 0.744)
#Test Accuracy: 66.1% (AUC: 0.564)


#Here we will be tuning the model by varying the value mtry. The value for trees (ntree) of 50 and 500 did not give a 
#very big differnce in AUC and Accuracy, so 50 has been taken for computional ease.

set.seed(123)

accuracy <- 0
for (i in 1:16) {
  model <- randomForest(Winner ~ ., data = train_set, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model, val_set, type = "response")
  predValid <- ifelse(predValid > 0.8,1,0)
  accuracy_test = mean(predValid == val_set$Winner)
  if (accuracy_test>accuracy) {
    accuracy <- accuracy_test 
    optimal_mtry <- i
  }
}

set.seed(123)
rfmodel <- randomForest(Winner ~ ., data = train_set, ntree = 500, mtry = optimal_mtry, importance = TRUE)
rfmodel

predValid <- predict(rfmodel, val_set, type = "response")
predValid <- ifelse(predValid > 0.8,1,0)

caret::confusionMatrix(data = as.factor(predValid), reference = as.factor(val_set$Winner))

#ROC
roc_rf <- roc(val_set$Winner,predValid)
plot(roc_rf)

#AUC
#We can label this a an excellent classisfier but it doing a fair job in the classification with AUC of 0.59
roc_rf


#With PCA
#Validation Accuracy: 88.41% (AUC: 0.8815)
#Test Accuracy: 66.1% (AUC: 0.61)


set.seed(123)

accuracy <- 0
for (i in 1:5) {
  model <- randomForest(Winner ~ ., data = train_set_pca, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model, val_set_pca, type = "response")
  predValid <- ifelse(predValid > 0.8,1,0)
  accuracy_test = mean(predValid == val_set_pca$Winner)
  if (accuracy_test>accuracy) {
    accuracy <- accuracy_test 
    optimal_mtry <- i
  }
}

rfmodel_pca <- randomForest(Winner ~ ., data = train_set_pca, ntree = 500, mtry = optimal_mtry, importance = TRUE)
rfmodel_pca

predValid_pca <- predict(rfmodel_pca, val_set_pca, type = "response")
predValid_pca <- ifelse(predValid_pca > 0.8,1,0)

caret::confusionMatrix(data = as.factor(predValid_pca), reference = as.factor(val_set_pca$Winner))

#ROC
roc_rf_pca <- roc(val_set_pca$Winner,predValid_pca)
plot(roc_rf_pca)

#AUC
roc_rf_pca



#SVM
#Validation Accuracy: polydot & vanilladot -> 85.98% (AUC: 0.856)
#Test Accuracy: polydot & vanilladot -> 66.1% (AUC: 0.592)


#Here we will be tuning the model by varying the kernels
#By varying the value of cost of constraints violation, there was very little difference in the training error. 


set.seed(123)
sv_py <- ksvm(Winner ~ ., data = train_set,kernel = "polydot",type="nu-svc",prob.model=TRUE)

pred_svm_py <- predict(sv_py, val_set,type = "prob")
pred_svm_py <- ifelse(pred_svm_py[,2] > 0.8,1,0)
caret::confusionMatrix(data = as.factor(pred_svm_py), reference = as.factor(val_set$Winner))


sv_va <- ksvm(Winner ~ ., data = train_set,kernel = "vanilladot",type="nu-svc",prob.model=TRUE)

pred_svm_va <- predict(sv_va, val_set,type = "prob")
pred_svm_va <- ifelse(pred_svm_va[,2] > 0.8,1,0)
caret::confusionMatrix(data = as.factor(pred_svm_va), reference = as.factor(val_set$Winner))


#ROC
#We can label this a an excellent classisfier but it doing a fair job in the classification with AUC of 0.925
roc_svm <- roc(val_set$Winner,pred_svm_va)
plot(roc_svm)

#AUC
roc_svm


#With PCA
#Validation Accuracy: polydot & vanilladot -> 86.59% (AUC: 0.592)
#Test Accuracy: polydot & vanilladot -> 66.1% (AUC: 0.61)

set.seed(123)
sv_py_pca <- ksvm(Winner ~ ., data = train_set_pca,kernel = "polydot",type="nu-svc",prob.model=TRUE)

pred_svm_py_pca <- predict(sv_py_pca, val_set_pca,type = "prob")
pred_svm_py_pca <- ifelse(pred_svm_py_pca[,2] > 0.8,1,0)
caret::confusionMatrix(data = as.factor(pred_svm_py_pca), reference = as.factor(val_set_pca$Winner))


sv_va_pca <- ksvm(Winner ~ ., data = train_set_pca,kernel = "vanilladot",type="nu-svc",prob.model=TRUE)

pred_svm_va_pca <- predict(sv_va_pca, val_set_pca,type = "prob")
pred_svm_va_pca <- ifelse(pred_svm_va_pca[,2] > 0.8,1,0)
caret::confusionMatrix(data = as.factor(pred_svm_va_pca), reference = as.factor(val_set_pca$Winner))


#ROC
#We can label this a an excellent classisfier but it doing a fair job in the classification with AUC of 0.925
roc_svm <- roc(test_set_pca$Winner,pred_svm_va_pca)
plot(roc_svm)

#AUC
roc_svm


##kNN
#Validation Accuracy: 78.66% (AUC: 0.781)
#Test Accuracy: 64.41% (AUC: 0.55)


knn_fit <- train(Winner ~., data = train_set_fac, method = "knn",
                 trControl=ctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 3)

pred_knn <- predict(knn_fit, newdata = val_set_fac,type = "prob")
pred_knn <- ifelse(pred_knn[,2] > 0.8,1,0)

caret::confusionMatrix(as.factor(pred_knn),val_set_fac$Winner)


#ROC
roc_knn <- roc(val_set_fac$Winner,pred_knn)
plot(roc_knn)

#AUC
roc_knn


#With PCA
#Validaiton Accuracy: 74.39% (AUC: 0.738)
#Test Accuracy: 62.71% (AUC: 0.518))


knn_fit_pca <- train(Winner ~., data = train_set_pca_fac, method = "knn",
                 trControl=ctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 3)

pred_knn_pca <- predict(knn_fit_pca, newdata = val_set_pca_fac,type = "prob")
pred_knn_pca <- ifelse(pred_knn_pca[,2] > 0.8,1,0)

caret::confusionMatrix(as.factor(pred_knn_pca),val_set_pca_fac$Winner)


#ROC
roc_knn_pca <- roc(val_set_pca_fac$Winner,pred_knn_pca)
plot(roc_knn_pca)

#AUC
roc_knn_pca


#The 4 models i.e. SVM, Random Forest ,kNN, Logistic Regression have been implemented with and without PCA and taken into account 
#5 principal compenents (thereby having dimensionality reduction). But we see that using PCA has helped our models in a few models,
#The accuracy along with AUC values are:

##Logistic Regression with PCA
#Validation Accuracy: 83.54% (AUC: 0.998)

##Random Forest with PCA
#With PCA
#Validation Accuracy: 88.41% (AUC: 0.882)

##SVM (polydot & vanilladot) with PCA
#Validation Accuracy: polydot & vanilladot -> 86.59% (AUC: 0.592)

##kNN
#Validation Accuracy: 78.66% (AUC: 0.781)

#Ideally we would be picking SVM model as our final model, but we will combining all the best three models
#on a weighted average basis into an ensemble model and evaluating it.


#Implmentation of Boosting
#Validation Accuracy: 87.8% (AUC: 0.875)
#Test Accuracy: 66.1% (AUC: 0.573)

adaboost <- adabag::boosting(Winner ~ ., data = train_set_fac)

pred_adaboost <- predict(adaboost, val_set_fac)
pred_adaboost <- ifelse(pred_adaboost$prob[,2] > 0.5,1,0)
caret::confusionMatrix(data = as.factor(pred_adaboost), reference = as.factor(val_set_fac$Winner))

#ROC
roc_ab_pca <- roc(val_set_pca$Winner,pred_adaboost)
plot(roc_ab_pca)

#AUC
roc_ab_pca



#Model Evaluation for SVM
#Validation Accuracy: 90.85% (AUC: 0.907)
#Test Accuracy: 66.1% (AUC: 0.61)

train_set_cv <- train_set_pca
train_set_cv$Winner <- ifelse(train_set_cv$Winner > 0,'Y','N')

val_set_cv <- val_set_pca
val_set_cv$Winner <- ifelse(val_set_cv$Winner > 0,'Y','N')

test_set_cv <- test_set_pca
test_set_cv$Winner <- ifelse(test_set_cv$Winner > 0,'Y','N')

sv_cv_pca <- caret::train(train_set_cv[,2:6],as.factor(train_set_cv$Winner),method='lssvmPoly',trControl=ctrl,tuneLength=3)

pred_svm_cv <- predict(sv_cv_pca,val_set_cv[,2:6])
caret::confusionMatrix(pred_svm_cv, as.factor(val_set_cv$Winner),positive='Y')

#ROC
roc_svm_cv <- roc(val_set_cv$Winner,ifelse(pred_svm_cv != 'Y',0,1))
plot(roc_svm_cv) 

#AUC
roc_svm_cv



#Ensemble Model
#Validation Accuracy: 91.46% (AUC: 0.913)
#Test Accuracy: 66.1% (0.61)

#Assigning weights to the Logistic, kNN and SVM as 0.3,0.6 and 0.1 respectively

train_set_ens <- train_set[-c(1:16)]
train_set_ens$pred_lr_prob <- predict(log_model_pca,train_set_pca,type = "response")*0.3
train_set_ens$pred_rf_prob <- predict(rfmodel_pca, train_set_pca,type = "response")*0.6
train_set_ens$pred_knn_prob <- predict(knn_fit, train_set_fac,type = "prob")[,2]*0.1

val_set_ens <- val_set[-c(1:16)]
val_set_ens$pred_lr_prob <- predict(log_model_pca,val_set_pca,type = "response")*0.3
val_set_ens$pred_rf_prob <- predict(rfmodel_pca, val_set_pca,type = "response")*0.6
val_set_ens$pred_knn_prob <- predict(knn_fit, val_set_fac,type = "prob")[,2]*0.1

test_set_ens <- test_set[-c(1:16)]
test_set_ens$pred_lr_prob <- predict(log_model_pca,test_set_pca,type = "response")*0.3
test_set_ens$pred_rf_prob <- predict(rfmodel_pca, test_set_pca,type = "response")*0.6
test_set_ens$pred_knn_prob <- predict(knn_fit, test_set_fac,type = "prob")[,2]*0.1


train_set_ens$Winner <- ifelse(train_set_ens$Winner > 0,'Y','N')
val_set_ens$Winner <- ifelse(val_set_ens$Winner > 0,'Y','N')
test_set_ens$Winner <- ifelse(test_set_ens$Winner > 0,'Y','N')

#Top Layer as Bagged AdaBoost
model_rf <- caret::train(train_set_ens[,2:4],train_set_ens$Winner,method = "ranger",
                          trControl=ctrl,
                          tuneLength = 3)

#Predict using Bagged AdaBoost top layer model
rf_stacked <- predict(model_rf,val_set_ens[,2:4])
caret::confusionMatrix(rf_stacked, as.factor(val_set_ens$Winner),positive='Y')

#ROC
roc_stacked <- roc(val_set_ens$Winner,ifelse(rf_stacked != 'Y',0,1))
plot(roc_stacked) 

#AUC
roc_stacked




#Alessandro's implementation
train_set_pca_fac_ce <- train_set_pca_fac
train_set_pca_fac_ce$Winner <- ifelse(as.numeric(train_set_pca_fac_ce$Winner) == 2,'Y','N')


control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('rf', 'rpart', 'glm', 'knn')
set.seed(123)
models <- caretList(Winner~., data=train_set_pca_fac_ce, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)








