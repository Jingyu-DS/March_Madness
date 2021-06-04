library("data.table")
set.seed(77)

# Read in the data we need 
test <- fread("./project/volume/data/raw/MSampleSubmissionStage2.csv")
season <- fread("./project/volume/data/raw/MRegularSeasonDetailedResults.csv")
tourney <- fread("./project/volume/data/raw/MNCAATourneyDetailedResults.csv")
ranks <- fread("./project/volume/data/raw/MMasseyOrdinals.csv")

# Clean test
# Split the first column to two team IDs
test <- data.table(matrix(unlist(strsplit(test$ID,"_")), ncol=3, byrow=T))
setnames(test,c("V1","V2","V3"),c("Season","team_1","team_2"))

# test$Season <- 2019
test$DayNum <- 135

test <- test[,.(team_1,team_2,Season,DayNum)]

test$result<-0.5


# table(ranks$SystemName)
# Make train
train <- rbind(season,tourney)
train <- train[,.(WTeamID,LTeamID,Season,DayNum)]
setnames(train,c("WTeamID","LTeamID"),c("team_1","team_2"))
# team 1 is win team
# set the winning probability to 1
train$result<-1

# make master data file
master<-rbind(train,test)
master <- master[order(Season,DayNum)]
master$team_1<-as.character(master$team_1)
master$team_2<-as.character(master$team_2)
master$Season<-as.integer(master$Season)

ranks$DayNum<-ranks$RankingDayNum+1

table(ranks$SystemName)
system_count <- ranks %>%
  group_by(SystemName) %>%
  summarise(count = n())%>%
  arrange(desc(count))

# Use the 6 dominated system name data
system_lst<-c("POM","SAG","MOR","DOK","WLK","MAS")

for (i in 1:length(system_lst)){
  
  one_rank<-ranks[SystemName==system_lst[i]][,.(Season,DayNum,TeamID,OrdinalRank)]
  setnames(one_rank,"TeamID","team_1")
  
  one_rank$team_1<-as.character(one_rank$team_1)
  
  setkey(master,Season,team_1,DayNum)
  setkey(one_rank,Season,team_1,DayNum)
  
  master<-one_rank[master,roll=T]
  setnames(master,"OrdinalRank","team_1_rank")
  
  
  setnames(one_rank,"team_1","team_2")
  setkey(master,Season,team_2,DayNum)
  setkey(one_rank,Season,team_2,DayNum)
  
  master<-one_rank[master,roll=T]
  
  setnames(master,"OrdinalRank","team_2_rank")
  
  master$rank_dif<-master$team_2_rank-master$team_1_rank
  
  master$team_1_rank<-NULL
  master$team_2_rank<-NULL
  
  setnames(master,"rank_dif",paste0(system_lst[i],"_dif"))
  
}

master<-master[!is.na(master$POM_dif)]
# master<-master[!is.na(master$PIG_dif)]
master<-master[!is.na(master$SAG_dif)]
master<-master[!is.na(master$MOR_dif)]
master<-master[!is.na(master$DOK_dif)]
master<-master[!is.na(master$WLK_dif)]
master<-master[!is.na(master$MAS_dif)]

###################
# Individual Stat #
###################

all_game <- rbind(season,tourney)

W_stats<-all_game[,.(Season,DayNum,WTeamID,WScore,WFGM,WFGA,WFGM3,WFGA3,WFTM,WFTA,WOR,WDR,WAst,WTO,WStl,WBlk,WPF)]
L_stats<-all_game[,.(Season,DayNum,LTeamID,LScore,LFGM,LFGA,LFGM3,LFGA3,LFTM,LFTA,LOR,LDR,LAst,LTO,LStl,LBlk,LPF)]

setnames(W_stats, c("WTeamID","WScore","WFGM","WFGA","WFGM3","WFGA3","WFTM","WFTA","WOR","WDR","WAst","WTO","WStl","WBlk","WPF"),
         c("TeamID","Score","FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF"))
setnames(L_stats, c("LTeamID","LScore","LFGM","LFGA","LFGM3","LFGA3","LFTM","LFTA","LOR","LDR","LAst","LTO","LStl","LBlk","LPF"),
         c("TeamID","Score","FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF"))

master_stats <- rbind(W_stats, L_stats)

stats_by_day<-NULL

for (i in 1:max(master_stats$DayNum)){
  sub_master_stats<-master_stats[DayNum < i]
  team_stats_by_day<-dcast(sub_master_stats, TeamID+Season~.,mean,
                           value.var=c("Score","FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF"))
  team_stats_by_day$dayNum<- i
  stats_by_day<-rbind(stats_by_day,team_stats_by_day)
}

stats_by_day <- stats_by_day[order(Season,dayNum)]


stats_by_day$DayNum <- stats_by_day$dayNum + 1
stats_by_day$TeamID <- as.character(stats_by_day$TeamID)


setkey(master,Season,team_1,DayNum)
setkey(stats_by_day,Season,TeamID,DayNum)

master<-stats_by_day[master,roll=T]
setnames(master,c("TeamID","Score","FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF"),
         c("team_1","Score_1","FGM_1","FGA_1","FGM3_1","FGA3_1","FTM_1","FTA_1","OR_1","DR_1","Ast_1","TO_1","Stl_1","Blk_1","PF_1"))


setkey(master,Season,team_2,DayNum)
setkey(stats_by_day,Season,TeamID,DayNum)

master<-stats_by_day[master,roll=T]

setnames(master,c("TeamID","Score","FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF"),
         c("team_2","Score_2","FGM_2","FGA_2","FGM3_2","FGA3_2","FTM_2","FTA_2","OR_2","DR_2","Ast_2","TO_2","Stl_2","Blk_2","PF_2"))

master$Score_dif<-master$Score_1-master$Score_2
master$FGM_dif<-master$FGM_1-master$FGM_2
master$FGA_dif<-master$FGA_1-master$FGA_2
master$FGM3_dif<-master$FGM3_1-master$FGM3_2
master$FGA3_dif<-master$FGA3_1-master$FGA3_2
master$FTM_dif<-master$FTM_1-master$FTM_2
master$FTA_dif<-master$FTA_1-master$FTA_2
master$OR_dif<-master$OR_1-master$OR_2
master$DR_dif<-master$DR_1-master$DR_2
master$Ast_dif<-master$Ast_1-master$Ast_2
master$TO_dif<-master$TO_1-master$TO_2
master$Stl_dif<-master$Stl_1-master$Stl_2
master$Blk_dif<-master$Blk_1-master$Blk_2
master$PF_dif<-master$PF_1-master$PF_2


master <- master[order(Season,DayNum)]

master<-master[,.(team_1,team_2,POM_dif,SAG_dif,MOR_dif,DOK_dif,WLK_dif,MAS_dif,Score_dif,FGM_dif,FGA_dif,FGM3_dif,FGA3_dif,FTM_dif,
                  FTA_dif,OR_dif,DR_dif,Ast_dif,TO_dif,Stl_dif,Blk_dif,PF_dif,result)]


master<-master[!is.na(master$Score_dif)]


test<-master[result==0.5]
train<-master[result==1]

rand_inx<-sample(1:nrow(train),nrow(train)*0.5)
train_a<-train[rand_inx,]
train_b<-train[!rand_inx,]

train_b$result<-0
# train_b$PIG_dif<-train_b$PIG_dif*-1
train_b$SAG_dif<-train_b$SAG_dif*-1
train_b$MOR_dif<-train_b$MOR_dif*-1
train_b$DOK_dif<-train_b$DOK_dif*-1
train_b$POM_dif<-train_b$POM_dif*-1
train_b$WLK_dif<-train_b$WLK_dif*-1
train_b$MAS_dif<-train_b$MAS_dif*-1
train_b$Score_dif <- train_b$Score_dif*-1
train_b$FGM_dif <- train_b$FGM_dif*-1
train_b$FGA_dif <- train_b$FGA_dif*-1
train_b$FGM3_dif <- train_b$FGM3_dif*-1
train_b$FGA3_dif <- train_b$FGA3_dif*-1
train_b$FTM_dif <- train_b$FTM_dif*-1
train_b$FTA_dif <- train_b$FTA_dif*-1
train_b$OR_dif <- train_b$OR_dif*-1
train_b$DR_dif <- train_b$DR_dif*-1
train_b$Ast_dif <- train_b$Ast_dif*-1
train_b$TO_dif <- train_b$TO_dif*-1
train_b$Stl_dif <- train_b$Stl_dif*-1
train_b$Blk_dif <- train_b$Blk_dif*-1
train_b$PF_dif <- train_b$PF_dif*-1


train<-rbind(train_a,train_b)




fwrite(test,'./project/volume/data/interim/test.csv')
fwrite(train,'./project/volume/data/interim/train.csv')

