
# Load the Data
df = read.csv('https://raw.githubusercontent.com/insaid2018/R/master/Projects/deliveries.csv')

# Add Packages
library(dplyr)
library(dbplyr)
library(ggplot2)
library(ggplot2movies)
library(plotly)
library(ggthemes)
library(vtree)
library(tidyverse)
library(plyr)
library(sqldf)
library(esquisse)
library(ggpubr)
# View the data
View(df)

# To see number of columns and rows
dim(df)

# to see the summary of data
summary(df)

# to view first 6 line and last 6 line of data
View(head(df))
View(tail(df))

##############################################################

# to print the columns
colnames(df)

################## Finding NA values #########################

# assigning a variable for number of columns
numColumns = dim(df)[2]
print (numColumns)

vector_NAs = rep(0, numColumns)
# it has 21 zeros

for (i in 1:numColumns) {
  vector_NAs[i] = sum(is.na(df[,i]))
}
print("The missing values in each column:")
print(vector_NAs)

# alternate methodto find missing values
colSums(is.na(df))

# there is no missing values in this data set

###############################################################

# Print the distinct values of each column
rapply(df,function(x)length(unique(x)))
# this data is for 696 matches, 696 distinct values of match_id column
# 14 teams, total over = 20, max ball in over is 9, 488 batsman faces a ball, 
# 378 bowler bowls, there is a super over matches
# 465 players dismissed out of 488, there are 10 different ways 


################## Number of batsman dismissed in IPL #########

sqldf("Select count(batsman) from df where player_dismissed!=''")
# 8157 - total number of wickets in IPL


################# Number of runs scored in IPL ################

sqldf("Select count(total_runs) from df")

#################### See How batsman got dismissed ############

count(df, 'dismissal_kind')
count(df, 'player_dismissed')
ggplot(filter(df, dismissal_kind!=""), aes(x = factor(dismissal_kind))) + coord_flip() + geom_bar(fill="red")


pie(table(filter(df, dismissal_kind!="")$dismissal_kind), col=c("red", "blue", "green","orange","violet","purple","black","brown","pink"), main="Type of dismissal")
legend("right",legend=levels(as.factor(filter(df, dismissal_kind!="")$dismissal_kind)), fill=c("red", "blue", "green","orange","violet","purple","black","brown","pink"), title="Type of Dismissal", box.lty=0)

# distinct(data, dismissal_kind)
#table(data$dismissal_kind)

# From the above graph, we can conclude that max players got out as caught and bowled.
# They try to hit big shots but due to miss time they caught out or they miss to hit the ball.
# Very rare people got out by obstructing the field or retired hurt.

#################### Print Team Names #########################

distinct(data, batting_team)
# ther are 14 teams who played the IPL

############ Players dismissed most number of times ###########

sqldf("SELECT player_dismissed,COUNT(player_dismissed) AS count FROM df where player_dismissed!='' GROUP BY player_dismissed ORDER BY count DESC LIMIT 10")
q1 = sqldf("SELECT player_dismissed,COUNT(player_dismissed) AS count FROM df where player_dismissed!='' GROUP BY player_dismissed ORDER BY count DESC LIMIT 10")

#qplot(q1$value_occurrence,q1$player_dismissed,geom="point")
ggplot(q1, aes(y = player_dismissed, x = count)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = count), vjust = -0.3) + 
  theme_base() +
  ggtitle("Maximum Number of times player got out")

# This are the 10 players who got out maximum number of times.
# Suresh Rains got out maximum number of times, it may be he played maximum number of matches

################## Top 10 Run Scorers in IPL ##################

q2 = sqldf("SELECT batsman, SUM(total_runs) AS total_runs FROM df GROUP BY batsman ORDER BY SUM(total_runs) DESC LIMIT 10")
q2
#qplot(q2$total_runs,q2$batsman,geom="point")
ggplot(q1, aes(y = player_dismissed,x=count))+geom_col(fill="green")

# This are the Top 10 players who scored maximum runs in the IPL
# Suresh Raina is the highest run scorer in the IPL.


#################  Top 10 Wicket Takers in IPL ################

q3 = sqldf("SELECT bowler, count(player_dismissed) AS Wickets FROM df WHERE player_dismissed!='' GROUP BY bowler ORDER BY count(player_dismissed) DESC LIMIT 10")
q3
#qplot(q3$Wickets,q3$bowler,geom="point") + geom_col()
ggplot(q3, aes(y = bowler, x=Wickets)) + geom_col(fill="green")

# This are the Top 10 players who took maximum number of wickets in the IPL
# Lasith Malinga is the highest wicket taken in the IPL

###############################################################

# In how many matches decision is taken by Super Over

sqldf("Select count(distinct(match_id)) from df where is_super_over == 1")
# 7 number of matches where decision is taken by super over


###############################################################

# Fetch the runs of all batsman

#df %>% group_by(batsman) %>% summarise(total_runs = sum(total_runs))
#with(df, barplot(rev(sort(table(total_runs))[1:5])))

# Method 1
aggregate(df$batsman_runs, by=list(Players=df$batsman), FUN=sum)
# Method 2
tapply(df$total_runs, df$batsman, FUN=sum)

#############################################################


# which team concede how many runs as wide runs
aggregate(df$wide_runs, by=list(team=df$bowling_team), FUN=sum)
a1=sqldf("SELECT bowling_team, SUM(wide_runs) AS wide_runs FROM df GROUP BY bowling_team")
ggplot(a1, aes(y = bowling_team,x=wide_runs))+geom_col()

# Mumbai Indians and Royal Challengers concede many runs as wide runs as compared to other teams

##############################################################################

# which team concede how many runs as bye runs
aggregate(df$bye_runs, by=list(team=df$bowling_team), FUN=sum)
a2=sqldf("SELECT bowling_team, SUM(bye_runs) AS bye_runs FROM df GROUP BY bowling_team")
ggplot(a2, aes(y = bowling_team,x=bye_runs))+geom_col()

# RCB concede maximum runs as bye runs as compared to other teams, may be they dont have good wicket keeper

##############################################################################

# which team concede how many runs as lgbye runs
aggregate(df$legbye_runs, by=list(team=df$bowling_team), FUN=sum)
a3=sqldf("SELECT bowling_team, SUM(legbye_runs) AS legbye_runs FROM df GROUP BY bowling_team")
ggplot(a3, aes(y = bowling_team,x=legbye_runs))+geom_col()

# MI concede max runs as legbye runs as compared to other teams


##############################################################################

# which team concede how many runs as noball runs
aggregate(df$noball_runs, by=list(team=df$bowling_team), FUN=sum)
a4=sqldf("SELECT bowling_team, SUM(noball_runs) AS noball_runs FROM df GROUP BY bowling_team")
ggplot(a4, aes(y = bowling_team,x=noball_runs))+geom_col()

# MI concede maximum no_ball runs as compared to other teams 

##############################################################################

# which team concede how many runs as penalty runs
aggregate(df$penalty_runs, by=list(team=df$bowling_team), FUN=sum)
a5=sqldf("SELECT bowling_team, SUM(penalty_runs) AS penalty_runs FROM df GROUP BY bowling_team")
ggplot(a5, aes(y = bowling_team,x=penalty_runs))+geom_col()

# Mumbai Indians and Deccan Charges are the only teams who concede penalty runs

##############################################################################

# which team concede how many runs as extra runs
aggregate(df$extra_runs, by=list(team=df$bowling_team), FUN=sum)
a6=sqldf("SELECT bowling_team, SUM(extra_runs) AS extra_runs FROM df GROUP BY bowling_team")
ggplot(a6, aes(y = bowling_team,x=extra_runs))+geom_col()

# MI concede maximum extra runs as compared to other teams.

#################################################################

# How many runs scored by all the team in each over
b1=sqldf("Select over, batting_team, sum(total_runs) as total_runs from df GROUP BY over,batting_team")
b11 = ggplot(b1, aes(x = over,y=total_runs, fill=batting_team))+geom_col()+theme_calc()
ggplotly(b11)
# from the above graph, we can see that players starts making run 
# in te powerplay from over 3-6 and then target last 4 overs and
# take first 2 overs to settle, read pitch


b2=sqldf("Select over, bowling_team, sum(wide_runs) as wide_runs from df GROUP BY over, bowling_team")
b21 = ggplot(b2, aes(x = over, y=wide_runs, fill=bowling_team))+geom_col()+theme_calc()
ggplotly(b21)

# In 1st over team concede most runs, bowlers need 1-2 overs to settle line and length

###################################################################################


b3=sqldf("Select over,sum(bye_runs) as bye_runs from df GROUP BY over")
ggplot(b3, aes(x = over,y=bye_runs))+geom_col(fill="blue")+theme_calc()

# In 20th over team give max runs as bye runs, if batsman leaves the ball and run as a bye run

###################################################################################

b4=sqldf("Select over,sum(legbye_runs) as legbye_runs from df GROUP BY over")
ggplot(b4, aes(x = over,y=legbye_runs))+geom_col(fill="blue")+theme_calc()

# In the powerplay overs team gives many legbye runs

###################################################################################

b5=sqldf("Select over,sum(noball_runs) as noball_runs from df GROUP BY over")
ggplot(b5, aes(x = over,y=noball_runs))+geom_col(fill="blue")+theme_calc()

# In 19th over team got no ball

###################################################################################
##########################################################################

# esquisser()

################################################################

#data[data==""]<-NA

##########################################################################

sqldf("Select over,sum(total_runs) from df GROUP BY over")
sqldf("Select over,sum(wide_runs) from df GROUP BY over")
sqldf("Select over,sum(bye_runs) from df GROUP BY over")
sqldf("Select over,sum(legbye_runs) from df GROUP BY over")
sqldf("Select over,sum(noball_runs) from df GROUP BY over")


##########################################################################

# Number of overs played by team in a match 
sqldf("Select inning, over,count(distinct(match_id)) from df where inning NOT IN (3,4) GROUP BY inning, over")


##########################################################################

# Number of players got out in Each Over
q4 = sqldf("Select over,count(player_dismissed) AS count from df where player_dismissed!='' GROUP BY over")
ggplot(q4, aes(x = over, y = count)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = count), vjust = -0.3) + 
  theme_base() +
  ggtitle("Number of player got out in Each Over")

# Most number of players got out in the death overs, they tried to hit the big shots and lose there wickets.
##########################################################################