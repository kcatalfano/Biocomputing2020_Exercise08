# Problem 1: use score-by-score info in "UWvMSU_1-22-13.txt" to make 
#a linear analysis using line plot with cumulative score as a 
# function of time (2 teams so 2 lines)

#line graph depicting the cumulative score for each team as a function 
#of time in the game

###tip 1: generate a matrix or dataframe with a cumulative score for each 
#####team whenever either team scores.
###tip 2:  plot(x,y,type='l') in base package, where x and y are vectors and 
#####type=‘l’ specifies a line graph. You can add a second line to this graph
#####with lines(x,y)

#read in "UWvMSU_1-22-13.txt" 
UWvMSU<-read.table("UWvMSU_1-22-13.txt", header=T, stringsAsFactors = F)

# make empty vectors that are the length of the time column for both teams
UWscore_v_time<-c()
cumulative_UW_score<-c()
UW_score_timepoint<-c()
MSUscore_v_time<-c()
cumulative_MSU_score<-c()
MSU_score_timepoint<-c()

#loop through data frame and add each point earned in order it was earned to 
#empty vector
###for loop with if-else statement inside
for(i in 1:nrow(UWvMSU)){
  if(UWvMSU$team[i]=='UW'){
    UWscore_v_time<-append(UWscore_v_time, UWvMSU$score[i])
    UW_score_timepoint<-append(UW_score_timepoint, UWvMSU$time[i])
  }else{
    MSUscore_v_time<-append(MSUscore_v_time, UWvMSU$score[i])
    MSU_score_timepoint<-append(MSU_score_timepoint, UWvMSU$time[i])
  }
}


#create vectors with the cumulative sum of each team's score
cumulative_UW_score<-append(cumulative_UW_score, cumsum(UWscore_v_time))
cumulative_MSU_score<-append(cumulative_MSU_score, cumsum(MSUscore_v_time))


#create a data frame for each teams score and when they got that score.
MSU_data<-data.frame(cumulative_MSU_score, MSU_score_timepoint)
UW_data<-data.frame(cumulative_UW_score, UW_score_timepoint)


#plot the data frames on the same line graph. green=MSU blue=UW
plot(MSU_score_timepoint, cumulative_MSU_score, "l", col="green")+
  lines(UW_score_timepoint, cumulative_UW_score, col="blue")


#Problem 2: Write a game called “guess my number”. The computer will generate 
##a random number between 1 and 100. The user types in a number and the 
##computer replies “lower” if the random number is lower than the guess, 
##“higher” if the random number is higher, and “correct!” if the guess is 
##correct. The player can continue guessing up to 10 times.

###tip 1: take a look at the Input/Output reference I gave you for 
####how to get input from the user
###tip 2: sample() is a function that allows for a random selection from a 
####vector containing a set of integers

#Generate a random number
b<-runif(1, min=1, max=100)
b<-round(b)

#print a statement to tell the user the game is thinking of a number from 1-100
print("I'm thinking of a number between 1 and 100...")

#make a vector to store the guessing attempts
a<-0

#make a loop so that the user guesses and has their guess evaluated 10 times\
while(a!=10){
  guess<-readline(prompt = "Guess:")
  a=a+1
  if(guess<b)
  {
    print("Higher! Guess again!")
  }
  else if(guess>b)
  {
    print("Lower! Guess again!")
  }
  if(guess==b){
    print("you got it!")
    break
  }
  if(a==10){
    print("Too many guesses! The number was:" $b)
  }
}



