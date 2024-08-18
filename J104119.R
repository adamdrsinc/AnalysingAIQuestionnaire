df <- read.csv('AIperceptionsFinal.csv')

#REFERENCES#
#-- Side-by-side barcharts --#
# Barplot in R. (n.d.). Retrieved from R-coder: https://r-coder.com/barplot-r/

#-- ggplot bar charts with percentages --#
# Strozynski, R. (2021, July 5). A Quick How-to on Labelling Bar Graphs in ggplot2. Retrieved from cedricscherer: https://www.cedricscherer.com/2021/07/05/a-quick-how-to-on-labelling-bar-graphs-in-ggplot2/

#-- Picking the best predictors for the multiple linear regression model --#
# Doctors, S. f. (2020, December 16). R Tutorial - #7 - Selecting predictors for multiple linear regression. Retrieved from YouTube: https://www.youtube.com/watch?v=Npa39UoR5hg

# Neath, A.A. and Cavanaugh, J.E. (2012), The Bayesian information criterion: background, derivation, and applications. WIREs Comp Stat, 4: 199-203. https://doi.org/10.1002/wics.199
# Cavanaugh JE, Neath AA. The Akaike information criterion: Background, derivation, properties, application, interpretation, and refinements. WIREs Comput Stat. 2019; 11:e1460. https://doi.org/10.1002/wics.1460
# Mallows, C. L. (1973). Some Comments on Cp. Technometrics, 661-675.
# Robertson, A. W. (2019). Using R and R Studio to Explore Data: An Introduction to Data Analytics. Sulby, Isle of Man.


#Package Installations:
library(dplyr)
library(psych)
library(ggplot2)
library(forcats)
library(car)
library(plotrix)
library(openxlsx)
library(olsrr)

regLikertScale <- c("Strongly agree", "Agree",
                    "Neither agree/disagree",
                    "Disagree", "Strongly disagree")

defaultColours <- c("darkgreen","green","yellow","red","darkred")

#Clean data
df$AIEasyToUseFactor <- factor(df$AIEasyToUse, levels=regLikertScale)
df$AIReliableInfoFactor <- factor(df$AIReliableInfo, levels=regLikertScale)
df$AIProvidesAccurateInfoFactor <- factor(df$AIProvidesAccurateInfo, levels=regLikertScale)
df$AICreateUsefulThingsFactor <- factor(df$AICreateUsefulThings, levels=regLikertScale)
df$UsedAIPreviouslyFactor <- factor(df$UsedAIPreviously, levels=c('Yes', 'No'))
df$IUseAIToSupportStudiesFactor <- factor(df$IUseAIToSupportStudies, levels=regLikertScale)
df$AIIsValuableToMeFactor <- factor(df$AIIsValuableToMe, levels=regLikertScale)
df$AIComplexToUseFactor <- factor(df$AIComplexToUse, levels=regLikertScale)
df$AIEnjoyableToUseFactor <- factor(df$AIEnjoyableToUse, regLikertScale)
df$FriendsUseAIPersonalLivesFactor <- factor(df$FriendsUseAIPersonalLives, levels=regLikertScale)
df$FriendsUseAIStudyLivesFactor <- factor(df$FriendsUseAIStudyLives, levels = regLikertScale)
df$AISystemsPoseNoDangerToUsFactor <- factor(df$AISystemsPoseNoDangerToUs, levels=regLikertScale)
df$AISystemsWillDevelopSafelyFactor <- factor(df$AISystemsWillDevelopSafely, levels=regLikertScale)
df$IFearOthersUseofAIFactor <- factor(df$IFearOthersUseofAI, levels=regLikertScale)
df$GovernmentsMustDoMoreToProtectFromAIFactor <- factor(df$GovernmentsMustDoMoreToProtectFromAI, levels=regLikertScale)
df$GenderFactor <- factor(df$Gender, levels = c('Male', 'Female','Nonbinary', 'Prefer not to say'))
regionLevels <- c("Africa","Americas","Asia","European Union","Isle of Man", "United Kingdom", "Other")
df$RegionFactor <- factor(df$Region, levels=regionLevels)


df$AIHelpsMeLearnFactor <- addNA(df$AIHelpsMeLearn)
levels(df$AIHelpsMeLearnFactor) <- c("Agree", "Disagree", "Strongly agree", "Strongly disagree", "Neither agree/disagree")
df$AIHelpsMeLearnFactor <- factor(df$AIHelpsMeLearnFactor, regLikertScale)

df$IUseAIToHelpMeCreateWorkFactor <- addNA(df$IUseAIToHelpMeCreateWork)
levels(df$IUseAIToHelpMeCreateWorkFactor) <- c("Agree", "Disagree", "Strongly agree", "Strongly disagree", "Neither agree/disagree")
df$IUseAIToHelpMeCreateWorkFactor <- factor(df$IUseAIToHelpMeCreateWorkFactor, regLikertScale)

df$IUseAIToPlanWorkFactor <- addNA(df$IUseAIToPlanWork)
levels(df$IUseAIToPlanWorkFactor) <- c("Agree", "Disagree", "Strongly agree", "Strongly disagree", "Neither agree/disagree")
df$IUseAIToPlanWorkFactor <- factor(df$IUseAIToPlanWorkFactor, regLikertScale)

df$EasyToCheatWithAIFactor <- addNA(df$EasyToCheatWithAI)
levels(df$EasyToCheatWithAIFactor) <- c("Agree", "Disagree", "Strongly agree", "Strongly disagree", "Neither agree/disagree")
df$EasyToCheatWithAIFactor <- factor(df$EasyToCheatWithAIFactor, regLikertScale)

df$EasyCatchAICheatsFactor <- addNA(df$EasyCatchAICheats)
levels(df$EasyCatchAICheatsFactor) <- c("Agree", "Disagree", "Strongly agree", "Strongly disagree", "Neither agree/disagree")
df$EasyCatchAICheatsFactor <- factor(df$EasyCatchAICheatsFactor, regLikertScale)

df$LecturersUseAISupportTeachingFactor <- addNA(df$LecturersUseAISupportTeaching)
levels(df$LecturersUseAISupportTeachingFactor) <- c("Agree", "Disagree", "Strongly agree", "Strongly disagree", "Neither agree/disagree")
df$LecturersUseAISupportTeachingFactor <- factor(df$LecturersUseAISupportTeachingFactor, regLikertScale)

df$UniShouldShowMeHowToUseAIFactor <- addNA(df$UniShouldShowMeHowToUseAI)
levels(df$UniShouldShowMeHowToUseAIFactor) <- c("Agree", "Disagree", "Strongly agree", "Strongly disagree", "Neither agree/disagree")
df$UniShouldShowMeHowToUseAIFactor <- factor(df$UniShouldShowMeHowToUseAIFactor, regLikertScale)

dfMale <- df %>%
  filter(df$GenderFactor=="Male")
dfFemale <- df %>%
  filter(df$GenderFactor=="Female")
dfNotSay <- df %>%
  filter(df$GenderFactor=="Prefer not to say")
UsedAI <- df %>%
  filter(df$UsedAIPreviouslyFactor=="Yes")
dfUsedAIPrevFalse <- df %>%
  filter(df$UsedAIPreviouslyFactor=="No")


# 1. Current use of AI
# i. How many people have used an AI system previously?
# Chosen Column(s): 
# - UsedAIPreviously
# UsedAIPreviously has been chosen as it separates the students who have used AI previously from those who have not.
# 181 students have used AI systems previously, 46 have not, displaying an overwhelming majority. 


table(df$UsedAIPreviouslyFactor)

#Code adapted from Strozynski, 2021
dfUsedAIPrev <- as.data.frame(table(df$UsedAIPreviouslyFactor))
dfUsedAIPrev <- dfUsedAIPrev |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfUsedAIPrev, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=c("Yes", "No")) , stat = "identity") +
  labs(fill="Legend",
       x = "Have you used AI tools previously?",
       y = "Amount of Students",
       title = "How many students have used AI previously?") +
  scale_fill_manual(values=c("green", "red"), breaks=c("Yes","No"))+
  geom_label(aes(label=perc),
            nudge_y=-7,
            size=4,
            fontface="bold",
            fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))
#End of adapted code 

# ii.	What tools did they use? 
# - All columns related to named AI tools were used, as these would display what tools were used,
# and counted together would display how many times tools were used. 
# The students have used a very wide variety of AI tools. The most used AI tool is by far ChatGPT, which almsot every single
# student has used. Dalle, Bard.AI, and Bing.AI saw some usage, and the rest only saw very light usage.



par(mfrow=c(1,1))
numUsesChatGPT = length(which(df$ChatGPT==1 & df$UsedAIPreviouslyFactor=="Yes"))
numUsesDalle = length(which(df$Dalle==1 & df$UsedAIPreviouslyFactor=="Yes"))
numUsesChatSonic = length(which(df$ChatSonic==1 & df$UsedAIPreviouslyFactor=="Yes"))
numUsesBardAI = length(which(df$Bard.AI==1 & df$UsedAIPreviouslyFactor=="Yes"))
numUsesClaude = length(which(df$Claude==1 & df$UsedAIPreviouslyFactor=="Yes"))
numUsesLaMDA = length(which(df$LaMDA==1 & df$UsedAIPreviouslyFactor=="Yes"))
numUsesJasperChat = length(which(df$Jasper.chat==1 & df$UsedAIPreviouslyFactor=="Yes"))
numUsesSocratic = length(which(df$Socratic==1 & df$UsedAIPreviouslyFactor=="Yes"))
numUsesBingAI = length(which(df$Bing.AI==1 & df$UsedAIPreviouslyFactor=="Yes"))
numUsesOther = length(which(df$Other==1 & df$UsedAIPreviouslyFactor=="Yes"))

timesSpecificAIWasUsed = c(numUsesChatGPT,
                           numUsesBingAI,
                           numUsesDalle,
                           numUsesBardAI,
                           numUsesChatSonic,
                           numUsesClaude,
                           numUsesLaMDA,
                           numUsesJasperChat,
                           numUsesSocratic,
                           numUsesOther)


tempLabels = c("ChatGPT","Bing.AI" ,"Dalle", "Bard.AI","ChatSonic", "Claude","LaMDA", "Jasper.Chat", "Socratic", "Other")
barplot(timesSpecificAIWasUsed,
        names.arg = tempLabels,
        ylim = c(0,200),
        col = rainbow(10),
        ylab = "Number of students who have used this AI",
        xlab = "AI Name",
        main = "Number of times a specific AI has been used by students")

legend("topright",
       fill=rainbow(10),
       legend=tempLabels)

# iii.	How frequently did they use AI tools in the last month?

#The following breaks are specific up to 35 before a general "Greater than 35" category.
#The categories were chosen primarily to display frequecny of use of AI tools.
#For example, the category '0' shows no usage, '11-15' shows usage almost every other day, '31-35' shows daily usage,
# and '>35' shows extreme usage, with the student using AI incredibly frequently.
#Most students either did not use AI at all (32.6%) or used it very infrequently, with '1-5' times constituting 44.5% of answers.
#A small percentage (3.1%) of students saw daily usage or more than daily usage (31-35 times or >35 times).
#The mean frequency of usage of AI is 5.471,showing an average usage of 5-6 times in the previous month.


par(mfrow=c(1,1))

freqLabels = c('0','1','1-5', '6-10', '11-15', '16-20', '21-25', '26-30', '31-35','>35')
UsedAI$FrequencyOfAIUse = cut(UsedAI$HowManyTimesUsedLastMonth,
                       breaks=c(-1, 0.1,1.1, 5, 10, 15, 20, 25, 30, 35, 200),
                       labels=freqLabels
)
table(UsedAI$FrequencyOfAIUse)

dfFrequencyOfAIUse <- as.data.frame(table(UsedAI$FrequencyOfAIUse))
dfFrequencyOfAIUse <- dfFrequencyOfAIUse |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfFrequencyOfAIUse, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=freqLabels) , stat = "identity") +
  labs(fill="Legend",
       x = "Times students used AI last month",
       y = "Amount of Students",
       title = "How many times did students use AI last month") +
  scale_fill_manual(values=rainbow(10), breaks = freqLabels)+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))
summary(UsedAI$HowManyTimesUsedLastMonth)


#iv.	What are the most frequently used tools?
# Using the bar plot from 2. ii., the most frequently used tools were ChatGPT, with 96.69% of all students who have used AI previously having used it.
# The second most used tool is Bing.AI, being used by 20.99% of students that have used AI previously.
# Joint third are Dalle and Bard.AI, with 15.47% of students having used both of these tools.

#2. AI technology personal perceptions
#v.	Did they find these tools easy to use?
# The column "AIEasyToUse" is specifically designed to answer this question, so will be used.
# Over 80% of students strongly agree or agree with the statement, and less than 10% disagree or strongly disagree, showing an
# overwhelming consensus that AI tools are easy to use.
# The 'Strongly Agree' column constitues over a third of responses, showing the strength of the feelings of students, clearly displaying
# that they find AI tools easy to use.

par(mfrow=c(1,1))

dfAIEasyToUse <- as.data.frame(table(UsedAI$AIEasyToUseFactor ))
dfAIEasyToUse <- dfAIEasyToUse |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfAIEasyToUse, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=c("Strongly agree", "Agree", "Neither agree/disagree", "Disagree", "Strongly disagree")) , stat = "identity") +
  labs(fill="Legend",
       x = "Is AI easy to use?",
       y = "Amount of Students",
       title = "Responses from students who have used AI previously to the statement
  \"I find AI systems easy to use\"") +
  scale_fill_manual(values=defaultColours, breaks=regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))

  #vi.	Did they find the tools useful?
#-----------------------------------#

#Dataframe Categories Used For This Question
# - AIHelpsMeLearn
# - IUseAIToSupportStudies
# - AICreateUsefulThings
# - IUseAIToHelpMeCreateWork
# - IUseAIToPlanWork
# - AIIsValuableToMe

# The first 5 columns will have their data gathered from those who have used AI tools previously, however,
# AIIsValuable to me will have its data gathered from those who have and those who have not used AI tools previously,
# as this will paint a wider picture regarding how the general student populace views AI tools and their value.

# These columns in this dataframe provide data which can help recognise if the
# student found AI tools to be useful. It can be said that if a student uses the AI to support
# something important to them such as their studies, work, or general learning, then the AI is deemed
# both useful and trustworthy with the information it dispenses.

#Whilst students use AI to help them learn; to support their studies; and to create useful things, 
#they do not use them to help plan and create their work. This could be grounds for stating that AI is not 
#seen as useful enough for students to implement it into every facet of their studies. 
#However, there are several other reasons as to why this may not be the case. One example is that students are often 
#disallowed from using AI tools for the planning and creation fo assignments due to plagiarism rules - the work produced by
#students must be their own, made without the help of generative tools of AI.
#Students do however implement these tools where they can - into their studies and helping them to learn. Due to this,
#it can be said that students find AI to be useful.

par(mfrow=c(1,1))


dfAIHelpsMeLearn <- as.data.frame(table(UsedAI$AIHelpsMeLearnFactor))
dfAIHelpsMeLearn <- dfAIHelpsMeLearn |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfAIHelpsMeLearn, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from students, who have used AI tools previously, to the statement
       \"I use AI systems to help me learn\"") +
  scale_fill_manual(values=defaultColours, breaks=regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=-3,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))
table(UsedAI$AIHelpsMeLearnFactor)


dfAIToSupportStudies <- as.data.frame(table(UsedAI$IUseAIToSupportStudiesFactor))
dfAIToSupportStudies <- dfAIToSupportStudies |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfAIToSupportStudies, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from students, who have used AI tools previously, to the statement
  \"I intend to use AI to support my studies\"") +
  scale_fill_manual(values=defaultColours,breaks=regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=-3,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))
table(UsedAI$IUseAIToSupportStudiesFactor)


dfAICreateUsefulThings <- as.data.frame(table(UsedAI$AICreateUsefulThingsFactor))
dfAICreateUsefulThings <- dfAICreateUsefulThings |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfAICreateUsefulThings, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from students, who have used AI tools previously, to the statement
  \"I use AI systems to help me create useful things\"") +
  scale_fill_manual(values=defaultColours, breaks=regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))
table(UsedAI$AICreateUsefulThingsFactor)


dfHelpCreateWork <- as.data.frame(table(UsedAI$IUseAIToHelpMeCreateWorkFactor))
dfHelpCreateWork <- dfHelpCreateWork |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfHelpCreateWork, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from students, who have used AI tools previously, to the statement
  \"I use AI systems to help me create work\"") +
  scale_fill_manual(values=defaultColours, breaks=regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=-3,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))
table(UsedAI$IUseAIToHelpMeCreateWorkFactor)



dfHelpPlanWork <- as.data.frame(table(UsedAI$IUseAIToPlanWorkFactor))
dfHelpPlanWork <- dfHelpPlanWork |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfHelpPlanWork, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from students, who have used AI tools previously, to the statement
  \"I use AI systems to help me plan my work\"") +
  scale_fill_manual(values=defaultColours, breaks= regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=-3,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))
table(UsedAI$IUseAIToPlanWorkFactor)


dfValuableToMe <- as.data.frame(table(df$AIIsValuableToMeFactor))
dfValuableToMe <- dfValuableToMe |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfValuableToMe, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from all students to the statement
  \"AI is valuable to me\"") +
  scale_fill_manual(values=defaultColours, breaks= regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=-3,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))

newPie <- pie(table(df$AIIsValuableToMeFactor), 
              labels=paste0(round(table(df$AIIsValuableToMeFactor)/sum(table(df$AIIsValuableToMeFactor)) * 100, 2), "%"),
              col=defaultColours,
              main="Responses from those who have used AI previously to the statement
              \"AI is valuable to me\"")
legend("topright",legend=regLikertScale, pch = c(15),
       bty="n",col=defaultColours)

#vii.	Did they find these tools complex to use?

# Dataframe Categories used for this question:
# - AIComplexToUse
# AIComplexToUse directly answers the question.
# Over 70% of students disagree or strongly disagree with the statement, with 36.9% strongly disagreeing, showing that students
# clearly find AI tools simple to use.

par(mfrow=c(1,1))

table(UsedAI$AIComplexToUseFactor )
dfAIComplexToUse <- as.data.frame(table(UsedAI$AIComplexToUseFactor ))
dfAIComplexToUse <- dfAIComplexToUse |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfAIComplexToUse, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=c("Strongly agree", "Agree", "Neither agree/disagree", "Disagree", "Strongly disagree")) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from students who have used AI previously to the statement
  \"I find AI systems complex to use\"") +
  scale_fill_manual(values=defaultColours, breaks=regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))




#viii.	Did they find the tools enjoyable to use?
# - AIEnjoyableToUse
# AIEnjoyableToUse directly answers the question by displaying if students find AI tools enjoyable to use.
# Over two thirds of students find AI systems enjoyable to use, showing a clear majority.


par(mfrow=c(1,1))

dfAIEnjoyableToUse <- as.data.frame(table(UsedAI$AIEnjoyableToUseFactor))
dfAIEnjoyableToUse <- dfAIEnjoyableToUse |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfAIEnjoyableToUse, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from students who have used AI systems previously to the statement
  \"I find AI systems enjoyable to use\"") +
  scale_fill_manual(values=defaultColours, breaks=regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=-3,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))


#ix.	Did they use these tools for personal life activities?
# Data Categories Chosen:
# - FriendsUseAIPersonalLives
# - AICreateUsefulThings
# As no single category directly answers this question,the aforementioned two categories have been selected
# as they may potentially provide some form of satisfactory answer.
# FriendsUseAIPersonalLives would directly answer the question, however this is relating to the friends of the student answering the question,
# who may not be students themselves.
# AICreateUsefulThings is the best category to gather data to answer this question for the students themselves, as this does not relate to
# assignment creation or education, but potentially to AI tools' use in their personal lives.

#Around 70% of students use AI systems to help them create useful things, and just over half of students' friends use
#AI tools in their personal lives, displaying that a majority use these for personal life activities.

dfAICreateUsefulThings <- as.data.frame(table(UsedAI$AICreateUsefulThingsFactor))
dfAICreateUsefulThings <- dfAICreateUsefulThings |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfAICreateUsefulThings, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from students, who have used AI tools previously, to the statement
  \"I use AI systems to help me create useful things\"") +
  scale_fill_manual(values=defaultColours, breaks=regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))
table(UsedAI$AICreateUsefulThingsFactor)



dfFriendsUseAIPersonalLives <- as.data.frame(table(UsedAI$FriendsUseAIPersonalLivesFactor))
dfFriendsUseAIPersonalLives <- dfFriendsUseAIPersonalLives |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfFriendsUseAIPersonalLives, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from students, who have used AI tools previously, to the statement
  \"My friends use AI systems in their personal lives\"") +
  scale_fill_manual(values=defaultColours, breaks=regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))


#3. AI Technologies and education
#-------------------------------#
#x.	How many people have used AI systems to help them learn?

# Data Categories Chosen:
# - AIHelpsMelearn - This directly answers the question, displaying how many students use AI systems to help them learn.
# - FriendsUseAIStudyLives - This allows us to see how many friends use AI to help them learn and study.
# - IUseAIToSupportStudies - This allows us to see how many students use AI tools to help them support their studies, 
#                            which can be said to be a type of learning. 

# Of the 181 students that have used AI systems previously, 110 have used AI systems to help them learn.
# Of the 227 students who have answered this survey, 109 state that their friends have used AI systems to help them learn.
# Of the 181 students that have used AI systems previously, 96 state that they intend to use AI systems to support their studies.

dfAIHelpsMeLearn <- as.data.frame(table(UsedAI$AIHelpsMeLearnFactor))
dfAIHelpsMeLearn <- dfAIHelpsMeLearn |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfAIHelpsMeLearn, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from students, who have used AI tools previously, to the statement
  \"I use AI systems to help me learn\"") +
  scale_fill_manual(values=defaultColours, breaks=regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))
# Those who answered Strongly Agree and Agree, when added together, form the number of people
# who have used AI to help them learn.
table(UsedAI$AIHelpsMeLearnFactor) 

dfFriendsUseAIStudyLives <- as.data.frame(table(UsedAI$FriendsUseAIStudyLivesFactor))
dfFriendsUseAIStudyLives <- dfFriendsUseAIStudyLives |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfFriendsUseAIStudyLives, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from students, who have used AI tools previously, to the statement
 \"My friends use AI systems in their work/studies\"") +
  scale_fill_manual(values=defaultColours, breaks=regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))


dfSupportStudies <- as.data.frame(table(UsedAI$IUseAIToSupportStudiesFactor))
dfSupportStudies <- dfSupportStudies |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfSupportStudies, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from students, who have used AI tools previously, to the statement
 \"I intend to use AI systems to support my studies\"") +
  scale_fill_manual(values=defaultColours, breaks=regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))


#xi.	Is there a concern AI systems can be used to cheat?
# - EasyToCheatWithAI
# - EasyCatchAICheats
# EasyToCheatWithAI will provide insight into students' views regarding how easy it is to cheat with AI systems,
# and EasyCatchAICheats will display students' views regarding the ease of catching cheaters.
# These combined will provide insight into students concerns regarding AI cheating.
# It can be said that if students view AI systems as easy to cheat with, but that these cheaters will be easily caught, then
# there may be only little concern that AI systems can be used to cheat with.

# A clear majority view AI systems as easy to cheat with, however, another majority also view cheating via the use of AI systems
# to be easily caught. This displays that students do not view AI cheating to be a concern.


dfAIEasyCheat <- as.data.frame(table(df$EasyToCheatWithAIFactor))
dfAIEasyCheat <- dfAIEasyCheat |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfAIEasyCheat, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from all students to the statement
       \"It is easy to cheat using AI systems\"") +
  scale_fill_manual(values=defaultColours, breaks=regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=-3,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))


dfEasyCatchCheats <- as.data.frame(table(df$EasyCatchAICheatsFactor))
dfEasyCatchCheats <- dfEasyCatchCheats |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfEasyCatchCheats, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from all students to the statement
  \"It is easy to catch people who have used AI systems to create their assignments\"") +
  scale_fill_manual(values=defaultColours, breaks=regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=-3,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))

table(df$EasyToCheatWithAIFactor)
table(df$EasyCatchAICheatsFactor)

#xii.	How have students been supported by their institutions/teachers/lecturers to use AI systems?
# - LecturersUseAISupportTeaching
# LecturersUseAISupportTeaching has been chosen as this category is best suited among all columns to answer this question.
# No other category relates to this question, as no other category gives insight into how institutions/teachers/lecturers have helped
# students use AI tools, how they have allowed students to use them for assignments, or anything else. 
# This category gives insight into how lecturers have used AI to support their teaching, thereby indirectly supporting students via the use
# of AI systems.

dfLecturersUseAI <- as.data.frame(table(df$LecturersUseAISupportTeachingFactor))
dfLecturersUseAI <- dfLecturersUseAI |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfLecturersUseAI, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from all students to the statement
       \"My teachers/lecturers use AI systems to support their teaching\"") +
  scale_fill_manual(values=defaultColours,  breaks=regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=-3,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))

#4. AI systems and the future

#xiii.	Reveal whether respondents feel AI systems will develop to be safe in the future.
# - AISystemsPoseNoDangerToUs
# - AISystemsWillDevelopSafely
# - IFearOthersUseofAI
# These three categories combined display together students' views on the development of AI systems in the future.
# It can be said that if students who find AI systems to pose no danger, believe they will develop safely, and do not fear others' 
# use of them, then students believe that they will develop to be safe in the future.

# Almost half of students disagree that AI systems pose no danger to humanity
# Almost 40% of students disagree/strongly disagree that AI systems will develop safely, and one third of responses show that students
# are unsure if they will develop safely.
# Over half of students fear how other people will use AI systems.

#These data combined display that students do not believe that AI systems will develop to be safe in the future.

  tempDF <- as.data.frame(table(df$AISystemsPoseNoDangerToUsFactor))
  tempDF <- tempDF |> 
    dplyr::mutate(
      perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
    )
  ggplot(tempDF, aes(x=Var1, y=Freq), fill=Var1) +
    geom_bar(aes(fill=regLikertScale) , stat = "identity") +
    labs(fill="Legend",
         x = "Student's Response",
         y = "Amount of Students",
         title = "Responses from all students to the statement
    \"AI systems pose no danger to people\"") +
    scale_fill_manual(values=defaultColours, breaks=regLikertScale)+
    geom_label(aes(label=perc),
               nudge_y=-3,
               size=4,
               fontface="bold",
               fill="white", label.size=0)+
    theme(plot.title.position = 'plot', 
          plot.title = element_text(hjust = 0.5))

table(df$AISystemsPoseNoDangerToUsFactor)


tempDF <- as.data.frame(table(df$AISystemsWillDevelopSafelyFactor))
tempDF <- tempDF |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(tempDF, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from all students to the statement
  \"I believe AI systems will develop safely\"") +
  scale_fill_manual(values=defaultColours, breaks=regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))

table(df$AISystemsWillDevelopSafelyFactor)


tempDF <- as.data.frame(table(df$IFearOthersUseofAIFactor))
tempDF <- tempDF |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(tempDF, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from all students to the statement 
       \"I fear others use of AI\"") +
  scale_fill_manual(values=defaultColours, breaks=regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))

#xiv.	What is the student perception on whether governments should do more to protect humanity? 
# - GovernmentMustDoMoreToProtectFromAI
# This category answers the question directly, by displaying responses to the statement "Governments must do more to protect people from AI systems".
# Over 60% of students either strongly agree or agree with the statement, and a meager percentage of just under 15% of students
# disagree or strongly disagree, showing that students strong believe that governments must do more to protect humanity.



tempDF <- as.data.frame(table(df$GovernmentsMustDoMoreToProtectFromAIFactor))
tempDF <- tempDF |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(tempDF, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from all students to the statement
  \"Governments must do more to protect people from AI systems\"") +
  scale_fill_manual(values=defaultColours, breaks=regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=-3,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))
table(df$GovernmentsMustDoMoreToProtectFromAIFactor)

#xv.	Using suitable methods present summaries of gender, highest qualification, region of residence and age.
# - Gender
# - QualificationStudied
# - Region
# - YearBirth

# -- Gender summary -- #
# Of the 224 students who answered with their gender:
# - Over half of responses came from male students.
# - Just over 1/3 of responses are from female students.
# - Only a single person who identified as nonbinary responded.
# - 6 Students stated that they prefer not to disclose their gender.

par(mfrow=c(1,1))

dfGender <- as.data.frame(table(df$GenderFactor))
dfGender <- dfGender |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfGender, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=c("Male","Female","Nonbinary","Prefer not to say")) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Gender of Students") +
  scale_fill_manual(values=c("blue","red","yellow","grey"), breaks = c("Male","Female","Nonbinary","Prefer not to say"))+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))
table(df$GenderFactor)


newPie <- pie(table(df$GenderFactor), 
              labels=paste0(round(table(df$GenderFactor)/sum(table(df$GenderFactor)) * 100, 2), "%"),
              col=c("lightblue","pink","yellow","purple"),
              main="Gender of Students")
legend("topright",legend=c("Male","Female","Nonbinary","Prefer not to say"), pch = c(15),
       bty="n",col=c("lightblue","pink","yellow","purple"))

# -- Highest level of study summary -- #
#Of the 227 students who disclosed their highest qualification they have attained/are studying towards:
# - Over 75% disclosed that they obtained theirs from university/are currently studying at university, 
# with 161 stating undergraduate level, and 11 postgraduate.
# - Around 20% stated that they obtained theirs from High School/are currently studying at High School, 
# with 33 stating A-level, 6 GCSE, nd 6 BTEC.
# - Around 5% state that they are studying/have studied for a qualification not stated on the survey.

#Cleaning data
df$QualificationStudiedNew <- df$QualificationStudied
df$QualificationStudiedNew[df$QualificationStudiedNew == 'High school'] <- 'Other'
df$QualificationStudiedNew[df$QualificationStudiedNew == 'Graduate Studies تم'] <- 'Postgraduate masters degree or equivalent (MSc, MA, etc.)'
df$QualificationStudiedNew[df$QualificationStudiedNew == 'Not studying right now'] <- 'Other'
df$QualificationStudiedNew[df$QualificationStudiedNew == 'Medical degree '] <- 'Other'
df$QualificationStudiedNew[df$QualificationStudiedNew == 'MBChB'] <- 'Undergraduate degree or equivalent (BSc, BA etc.)'
df$QualificationStudiedNew[df$QualificationStudiedNew == ''] <- 'Other' 

qualLabels <- c('Postgraduate masters degree or equivalent (MSc, MA, etc.)',
                'Undergraduate degree or equivalent (BSc, BA etc.)',
                'A-level, level 3 or equivalent',
                'GCSE, level 2 or equivalent',
                'BTEC HNC or equivalent',
                'Other')
df$QualificationStudiedNewFactor <- factor(df$QualificationStudiedNew, levels = qualLabels)
levels(df$QualificationStudiedNewFactor) <- c("Postgrad.", "Undergrad.", "A-level/Level 3","GCSE/Level 2",
                                              "BTEC/HNC", "Other")

#Creating bar plot
dfQualificationStudiedNew <- as.data.frame(table(df$QualificationStudiedNewFactor))
dfQualificationStudiedNew <- dfQualificationStudiedNew |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfQualificationStudiedNew, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=qualLabels) , stat = "identity") +
  labs(fill="Legend",
       x = "Qualification",
       y = "Amount of Students",
       title = "Student's Highest Qualification which has been attained
  or they are studying towards") +
  scale_fill_manual(values=rainbow(6), breaks = qualLabels)+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))
table(df$QualificationStudiedNewFactor)

# -- Region Summary -- #
# The vast majority of responses come from those in the United Kingdom, forming over 60% of responses.
# All other responses made up less than 10% of total responses each, with the second highest being the Isle of Man
# at 8.8%, and the third being Africa with 8.4%.

dfRegion <- as.data.frame(table(df$RegionFactor))
dfRegion <- dfRegion |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfRegion, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regionLevels), stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Amount of responses from Students per Region") +
  scale_fill_manual(values=rainbow(7), breaks=regionLevels)+
  geom_label(aes(label=perc),
             nudge_y=-3,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))
table(df$RegionFactor)

# -- Year of Birth Summary -- #
#Of the 223 responses, the majority of responses (55.6%) indicate their age as being between 20-29, 
#followed by those younger than 20 (41.3%). Those aged over 30 make up only 3.1% of responses.
#The mean year of birth of the students is 2002, with the oldest student being born in 1974,
#and the youngest in 2010.

groupedYearBirthLevels <- c('40+','30-39', '20-29', '<20')
df$GroupedYearBirth <- cut(df$YearBirth,
                          breaks=c(0,1980,1992, 2003, 2011),
                          labels=c('40+','30-39', '20-29', '<20')
)
df$GroupedYearBirth <- factor(df$GroupedYearBirth, levels=c('<20','20-29','30-39','40+'))
dfGroupedYearBirth <- as.data.frame(table(df$GroupedYearBirth))
dfGroupedYearBirth <- dfGroupedYearBirth |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfGroupedYearBirth, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=groupedYearBirthLevels), stat = "identity") +
  labs(fill="Legend",
       x = "Student's Age Group",
       y = "Amount of Students",
       title = "The Age of Students") +
  scale_fill_manual(values=rainbow(4), breaks=groupedYearBirthLevels)+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))
table(df$GroupedYearBirth)



#---------------------------------------#
#b.	Using your own judgement, split data using appropriate questions (e.g. has or hasn't used AI 
#systems previously) and compare 3 items of interest (e.g. summaries or distributions of responses to perceptions).

par(mfrow=c(1,1))

# -- Split by Gender -- #
# Note: Due to the statistical insignificance of only a single person, the gender "nonbinary" will be disregarded when making statements regarding
# demographics of genders.


# How did frequency of usage of AI tools in the previous month vary between genders?
#It has been found that the distribution of the usage of AI in the previous month between the genders "Male" and "Female"
#are incredibly alike. The interquartile range shows the same dispersion of responses between 0 and 5.
#The only difference between the male and female responses is the top-whisker, with the top whisker of male being longer
#than female, showing that the upper-quartile range has a large range, and that Males overall show marginally more usage of AI.
#Both "Male" and "Female" boxes show a moderate right skew, with the average being slightly left of centre of the interquartile range.
#The maximum of the male chart is 100 uses, and 20 for the female chart.

par(mfrow=c(1,1))
boxplot(df$HowManyTimesUsedLastMonth~df$GenderFactor, 
        main = "How many times the student used AI last month by their gender",
        ylab = "How many times the student used AI last month", 
        xlab = "Student's identified gender"
)



# What do different genders think regarding the danger of AI systems?
#As can be seen by the distributions of the pie charts, both male and female students heavily disagree with the statement
#"AI systems pose no danger to us". However, it can be noted that a larger percentage of males agree with the statement than females,
#with those males who strongly agree/agree making up around 18% of the chart, and those females making up only around 6%. 
#This displays that almost 3 times more male students agree that AI poses no danger than female students.

customRegLikertScale3DPie <- function(category, titleText, chartColours)
{
  tablePie <- table(category)
  tablePieDF <- as.data.frame(tablePie)
  tablePieDF
  #sliceValues <- tablePieDF[1:(length(unique(category))-1),2]
  data <- c(length(which(category=="Strongly agree")),
            length(which(category=="Agree")),
            length(which(category=="Neither agree/disagree")),
            length(which(category=="Disagree")),
            length(which(category=="Strongly disagree")))
  sliceValues <- paste0(round(data/sum(data) * 100, 2), "%")
  sliceValues
  rownames(tablePie) <- regLikertScale
  lbls <- paste(names(tablePie),"\n", tablePie, sep="")
  pie3D(tablePie, col=defaultColours, labels=sliceValues, explode=0.05, main = titleText)
  legend("topright",legend=rownames(tablePie), pch = c(15),
         bty="n",col=chartColours)
}


customRegLikertScale3DPie(dfMale$AISystemsPoseNoDangerToUsFactor,
            "Male students' responses to \"AI systems pose no danger to people\"",
            defaultColours)

customRegLikertScale3DPie(dfFemale$AISystemsPoseNoDangerToUsFactor,
                          "Female students' responses to \"AI systems pose no danger to people\"",
                          defaultColours)

table(dfMale$AISystemsPoseNoDangerToUs)
table(dfFemale$AISystemsPoseNoDangerToUs)

# Do different genders intend to use AI to support their studies as much as others?
#It appears that both males and females almost equally agree with the statement 
#I intend to use AI to support my studies”, with 47.78% of females and 50.79% of males strongly agreeing/agreeing. 
#This minor 3.01% difference shows very similar intent to use AI in their studies.
#Similarities can also be found in the disagreement of this statement, with 27.77% of females and 28.57% of males strongly disagreeing/disagreeing with the statement.

par(mfrow=c(1,2))

newPie <- pie(table(dfMale$IUseAIToSupportStudiesFactor), 
    labels=paste0(round(table(dfMale$IUseAIToSupportStudiesFactor)/sum(table(dfMale$IUseAIToSupportStudiesFactor)) * 100, 2), "%"),
    col=defaultColours,
    main="Male students' responses to 
    \"I intend to use AI to support my studies\"")

newPie <- pie(table(dfFemale$IUseAIToSupportStudiesFactor), 
              labels=paste0(round(table(dfFemale$IUseAIToSupportStudiesFactor)/sum(table(dfFemale$IUseAIToSupportStudiesFactor)) * 100, 2), "%"),
              col=defaultColours,
              main="Female students' responses to 
          \"I intend to use AI to support my studies\"")

legend("topright",legend=regLikertScale, pch = c(15),
       bty="n",col=defaultColours)



# -- Split by Year of Birth -- #
par(mfrow=c(1,1))

# How did different ages find the ease of using AI tools?
#The dispersion of data appears to vary greatly with each category along the x-axis.
#Despite this variety, the interquartile ranges of all categories appear to overlap often and significantly, 
#with “Disagree” being the exception, as half of its interquartile range does not overlap with any other 
#category, but the other categories have an overlap of their interquartile ranges of at-least two thirds.
#The responses of those who strongly agree and those who neither agree/disagree share the same interquartile range,
#and both display the same moderate left skew. The responses of those who agree show a significant right skew,
#and the opposite can be said of the distribution of those who disagree, with it displaying a significant left skew.
#The category “Strongly disagree” displays a normal distribution of ages.

#Due to the significant overlap, it appears that age plays little role in determining how easy it is to use AI systems,
#though it may be noted that more older people found AI tools difficult to use in general. However, a significant portion of
#younger students found them difficult to use as well.
boxplot(UsedAI$YearBirth~UsedAI$AIEasyToUseFactor, 
        main = "Students' Year of Birth and their responses to the statement
        \"I find AI systems easy to use\"", 
        ylab = "Year of Birth", 
        xlab = "Student's response to \"I find AI systems are easy to use\""
        )

# How do opinions vary in opinion on whether or not schools/colleges/univerisies should show students how to use AI systems?

#The dispersion displayed on figure X varies significantly, with the dispersion decreasing as the Likert scale moves from
#"Strongly agree" towards "Strongly disagree". "Agree" and "Disagree" show a dispersion of their interquartile ranges that are exactly alike.
#"Strongly agree" has the greatest dispersion, whilse "Strongly disagree" has the least.
#The interquartile ranges of each column overlap significantly with one another,
#showing that each category contained responses from those of similar ages. 
#The range of student age appears to decrease as the Likert scale moves from “"trongly agree” to
#"Strongly disagree", with a high variety of ages strongly agreeing or agreeing with the statement,
#and a much more concentrated age group strongly disagreeing or disagreeing. 
#This, combined with the overlap of interquartile ranges and median, displays that age plays little 
#role in a student’s response to this statement, and that a variety of age groups agree with the statement.

boxplot(df$YearBirth~df$UniShouldShowMeHowToUseAIFactor, 
        main = "Students' Year of birth and their responses to the statement:
\"Schools, colleges, and universities should show me how to use AI systems effectively\"", 
        ylab = "Year of Birth", 
        xlab = "Student's response"
)

# How does frequency of use change between ages?
# It appears that frequency of use varies little between age. A large variety of ages seldom use AI tools, and those who do use them frequently
# appear to be younger. However, these interquartile ranges overlap, displaying that age is not an important factor for frequency of use.
freqLabels = c('0','1-5', '6-10', '11-15', '16-20', '21-25', '26-30', '31-35','>35')
df$FrequencyOfAIUse = cut(df$HowManyTimesUsedLastMonth,
                          breaks=c(-0.1,1, 5, 10, 15, 20, 25, 30, 35, 200),
                          labels=freqLabels
)
boxplot(df$YearBirth~df$FrequencyOfAIUse, 
        main = "Students' Year of birth and their frequency of AI tool usage in the previous month.", 
        ylab = "Year of Birth", 
        xlab = "Student's response to \"Schools, colleges and universities should show me how to use AI systems effectively\"")


# -- Split on previous use of AI -- #

likertScalePieChart <- function(variable, titleString, colours)
{
  pie(table(variable), 
      labels=paste0(round(table(variable)/sum(table(variable)) * 100, 2), "%"),
      col=defaultColours,
      main=titleString)
  legend("topright",legend=regLikertScale, pch = c(15),
         bty="n",col=colours)
}

# How do opinions of those who have used AI previously and those who have not regarding the
# danger of AI systems vary?

#It appears that both those who have used AI previously and those who have not disagree with the statement
#“AI systems pose no danger to us”. However, the amount of disagreement varies, with those who have used AI
#previously having 57.78% of their responses be in strong disagreement/disagreement, and those who have not used 
#AI previously having theirs at a higher 69.57%, meaning that 11.79% more “non-AI users” feel that AI poses a danger to people.
#Those who have used AI previously seem to be generally more trustworthy of the safety of AI, as 14.44% of responses
#are in strong agreement/agreement with the statement, whereas of those who have not used AI previously, only 6.52%
#of responses agreed with the statement, and no students at all strongly agreed with it.


likertScalePieChart(UsedAI$AISystemsPoseNoDangerToUsFactor,
                    "Responses from those who have used AI previously to the statement
              \"AI systems pose no danger to us\"",
                    defaultColours)

likertScalePieChart(dfUsedAIPrevFalse$AISystemsPoseNoDangerToUsFactor,
                    "Responses from those who have NOT used AI previously to the statement
              \"AI systems pose no danger to us\"",
                    defaultColours)


# How do opinions of those who have used AI previously and those who have not regarding the
# reliability of information dispensed  by AI systems vary?

#It is found that 34.28% more students who have used AI previously than those who have not 
#state that AI systems provide reliable information, and 9.92% more students who have not 
#used AI previously strongly disagree/disagree with the statement. It can therefore be said that those
#who have not used AI previously find information provided by AI to be unreliable, which may be a contributing
#factor as to why they have not yet used AI, whereas those who have used AI seem more trusting of the information
#it provides, which may indicate trust that has come from use of the tools.

dfReliableInfo <- as.data.frame(table(UsedAI$AIReliableInfoFactor))
dfReliableInfo <- dfReliableInfo |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfReliableInfo, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from students who have used AI systems previously to the
  statement \"AI systems provide reliable information\"") +
  scale_fill_manual(values=defaultColours, breaks = regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))

dfReliableInfo <- as.data.frame(table(dfUsedAIPrevFalse$AIReliableInfoFactor))
dfReliableInfo <- dfReliableInfo |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfReliableInfo, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from students who have NOT used AI systems previously to the
  statement \"AI systems provide reliable information\"") +
  scale_fill_manual(values=defaultColours, breaks = regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))

dfReliableInfo <- as.data.frame(table(UsedAI$AIProvidesAccurateInfoFactor))
dfReliableInfo <- dfReliableInfo |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfReliableInfo, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from students who have used AI systems previously to the
  statement \"I trust AI systems to provide accurate information\"") +
  scale_fill_manual(values=defaultColours, breaks = regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))

dfReliableInfo <- as.data.frame(table(dfUsedAIPrevFalse$AIProvidesAccurateInfoFactor))
dfReliableInfo <- dfReliableInfo |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfReliableInfo, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from students who have NOT used AI systems previously to the
  statement \"I trust AI systems to provide accurate information\"") +
  scale_fill_manual(values=defaultColours, breaks = regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))

# How do opinions of those who have used AI previously and those who have not regarding whether
# or not governments should do more to protect people from AI?

#The responses from those who have used AI previously and those who have not to the statement
#“Governments must do more to protect people from AI systems” significantly contrast with one another. 
#45% of those who have used AI previously believe that the government must do more to protect its citizens
#from AI systems, whereas 78.26% of those who have not used AI previously strongly agree/agree with the statement.
#Only a tiny 4.35% of those who have not used AI previously believe that the government need not do more to protect
#its people, whereas over 6 times as many students who have used AI previously (27.22%) believe that the government
#is not required to do more.

#This trend displays that the majority of those who have and have not used AI previously believe that governments must do more.
#However, it appears that as students use AI systems more, they become more trustworthy of it, and do not believe that the government
#needs to do more.


likertScalePieChart(UsedAI$GovernmentsMustDoMoreToProtectFromAI,
                    "Responses from students who have used AI previously to the statement
              \"Governments must do more to protect people from AI systems\"",
                    defaultColours)

likertScalePieChart(dfUsedAIPrevFalse$GovernmentsMustDoMoreToProtectFromAI,
                    "Responses from students who have NOT used AI previously to the statement
              \"Governments must do more to protect people from AI systems\"",
                    defaultColours)

dfGovernmentMustDoMore <- as.data.frame(table(df$GovernmentsMustDoMoreToProtectFromAIFactor))
dfGovernmentMustDoMore <- dfGovernmentMustDoMore |> 
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", Freq / sum(Freq) * 100), "%"),
  )
ggplot(dfGovernmentMustDoMore, aes(x=Var1, y=Freq), fill=Var1) +
  geom_bar(aes(fill=regLikertScale) , stat = "identity") +
  labs(fill="Legend",
       x = "Student's Response",
       y = "Amount of Students",
       title = "Responses from students who have and have not used AI systems previously to the
  statement \"Governments must do more to protect people from AI systems\"") +
  scale_fill_manual(values=defaultColours, breaks = regLikertScale)+
  geom_label(aes(label=perc),
             nudge_y=0,
             size=4,
             fontface="bold",
             fill="white", label.size=0)+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))

#-------------------------------------------#
#c.	Build a multiple linear regression model that predicts frequency of usage using 5 appropriate survey responses (you may use more if you prefer),
#e.g. year of birth, ease of use, complexity plus 2 others of your choosing.  

#i.	Justify each of the predictors you have chosen.

#Determining the best preditors
#To determine the best predictors, ols_step_all_possible will be used, which will generate every possible model of the multiple linear
#regression model it has been given.
#This will produce a list of 2^(k-1) models, where k = number of predictors in the multiple linear regression model.
#The number of models produced here is 131,072.
my_lm <- lm(HowManyTimesUsedLastMonth ~ RegionFactor + YearBirth + AIEasyToUseFactor + AIHelpsMeLearnFactor + 
              IUseAIToSupportStudiesFactor + AIComplexToUseFactor + UniShouldShowMeHowToUseAIFactor + 
              AIEnjoyableToUseFactor + FriendsUseAIPersonalLivesFactor + AIReliableInfoFactor + 
              AIProvidesAccurateInfoFactor + FriendsUseAIStudyLivesFactor + EasyCatchAICheatsFactor + 
              EasyToCheatWithAIFactor + GenderFactor + AICreateUsefulThingsFactor + LecturersUseAISupportTeachingFactor + 
              AIProvidesAccurateInfoFactor, data=df)

all_lm <- ols_step_all_possible(model=my_lm)

#Now that we have all possible models of the 18 predictors, it must be now found which model is best.
#This will be determined initially based upon the adjusted R-squared score.
#However, before finding out which one has the best adjusted R-squared score, those models that are not well fitted (ones which have
#a bad Mallow's Cp score) will be discarded using the following function:

get_best_row <- function(olsResult, predictorCount)
{
  result <- olsResult %>% filter(between(olsResult$cp, 0, predictorCount+1))
  temp_rows <- result %>% filter(result$n==predictorCount)
  best_row <- temp_rows %>% filter(temp_rows$adjr==max(temp_rows$adjr))
  return(best_row)
}

#This function takes in all possible models, and then returns the model with the best fit based on how many predictors you want that model to have.
#A good Mallow's Cp score is one which is between 0 and the number of predictors+1. 

bestRow5 <- get_best_row(all_lm, 5)
bestRow6 <- get_best_row(all_lm, 6)
bestRow7 <- get_best_row(all_lm, 7)
bestRow8 <- get_best_row(all_lm, 8)
bestRow9 <- get_best_row(all_lm, 9)
bestRow10 <- get_best_row(all_lm, 10)
bestRow11 <- get_best_row(all_lm, 11)
bestRow12 <- get_best_row(all_lm, 12)
bestRow13 <- get_best_row(all_lm, 13)
bestRow14 <- get_best_row(all_lm, 14)
bestRow15 <- get_best_row(all_lm, 15)
bestRow16 <- get_best_row(all_lm, 16)
bestRow17 <- get_best_row(all_lm, 17)
bestRow18 <- get_best_row(all_lm, 18)

#After this, all rows were printed, and from this, the top 5 were singled out based on the highest adjusted R-squared score,
#best AIC score, and SBIC score.
#Looking at all rows to see top 5
testDF <- rbind(bestRow5, bestRow6, bestRow7, bestRow8, bestRow9, bestRow10,
                bestRow11, bestRow12, bestRow13, bestRow14, bestRow15, bestRow16,
                bestRow17, bestRow18)
testDF

#It can be noted that as the number of predictors in the well fitted models increases, the adjusted R-squared score appears to decrease.
#The AIC and SBIC scores appear to vary little between models.

#Top 5 rows
bestRows <- rbind(bestRow5, bestRow6, bestRow7, bestRow8, bestRow9)


#Now that the 5 models with the highest adjusted R-squared scores, that are well fitted according to Mallow's Cp, have been found, it must be found if there
#are signs of multicollinearity. This will be done using the "car" package's "vif" function. GVIF values of 3 or less generally mean that there is 
#very little sign of multicollinearity (Robertson, 2019), meaning that the P-values of the coefficients of the model can be trusted.

vif(lm(HowManyTimesUsedLastMonth ~ RegionFactor +YearBirth+AIEasyToUseFactor+ 
         AIHelpsMeLearnFactor+ FriendsUseAIStudyLivesFactor, data=df))

vif(lm(HowManyTimesUsedLastMonth ~ RegionFactor +YearBirth+ AIEasyToUseFactor+ 
         IUseAIToSupportStudiesFactor+ FriendsUseAIPersonalLivesFactor+ FriendsUseAIStudyLivesFactor, data=df))

vif(lm(HowManyTimesUsedLastMonth ~ RegionFactor +YearBirth+AIEasyToUseFactor+ 
         AIHelpsMeLearnFactor+ IUseAIToSupportStudiesFactor+ FriendsUseAIPersonalLivesFactor+ 
         AIReliableInfoFactor, data=df))

vif(lm(HowManyTimesUsedLastMonth ~ RegionFactor +YearBirth+AIEasyToUseFactor+IUseAIToSupportStudiesFactor+ 
         FriendsUseAIPersonalLivesFactor+ AIReliableInfoFactor+ FriendsUseAIStudyLivesFactor+EasyCatchAICheatsFactor, data=df))

vif(lm(HowManyTimesUsedLastMonth ~ RegionFactor +YearBirth+AIEasyToUseFactor+IUseAIToSupportStudiesFactor+ 
         UniShouldShowMeHowToUseAIFactor+ AIEnjoyableToUseFactor+ FriendsUseAIPersonalLivesFactor+AIReliableInfoFactor+FriendsUseAIStudyLivesFactor, data=df))

#It appears that all models, except the first model with 5 predictors, show significant signs of multicollinearity. The P-values of these models cannot be trusted
#to be accurate.

#The 5 predictor model seems promising, as it displays a relatively high adjusted R-square score, little sign of multicollinearity, and has relatively
#low AIC and SBIC scores. Now it must be seen if it has a low overall P-value.

summary(lm(HowManyTimesUsedLastMonth ~ RegionFactor +YearBirth+AIEasyToUseFactor+ 
             AIHelpsMeLearnFactor+ FriendsUseAIStudyLivesFactor, data=df))

#The overall P-value of the 5-predictor model displays a significantly low P-value of 9.497e-06.

#Of all models which are well-fitted according to its Mallow's Cp score, the 5-predictor model consisting of the responses to
#"Region", "Year of Birth", "I find AI systems easy to use", "I use AI systems to help me learn", and "My friends use AI systems in their work/studies",
#can be said to the be best model for predicting future frequency of use.


#ii.	How strong is the overall model? Evidence using adjusted R squared, t-tests and predictive accuracy.
summary(lm(HowManyTimesUsedLastMonth ~ RegionFactor +YearBirth+AIEasyToUseFactor+ 
             AIHelpsMeLearnFactor+ FriendsUseAIStudyLivesFactor, data=df))

#The overall model displays a weak predictive accuracy. The adjusted R-square sore is 0.1685, displaying an incredibly weak ability
#to accurately predict future frequency of use.
#Of the 20 displayed coefficients, only 7 display any significance. The coefficients RegionFactorAsia and AIHelpsMeLearnFactorDisagree
#show the greatest correlation, as their t-values vary the most from the null hypothesis and have the lowest P-values.


#iii.	With justification, what appears to be the strongest predictor of usage?
#The strongest predictor of usage appears to be the responses to the statement "I use AI systems to help me learn". This predictor has the lowest
#P-values, showing the greatest correlation to frequency of usage.
