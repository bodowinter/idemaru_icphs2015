## Bodo Winter
## November 27, 2014; redone December 8, 2014
## Analysis of first ten participants for collaboration with Kaori Idemaru & Lucien Brown
## Politeness perception task

## Load in data:

setwd("/Users/teeniematlock/Desktop/research/politeness/kaori_perception/icphs_2015_analysis/")
xdata <- read.csv("korean_perception_first10.csv")

## Load in packages:

library(lme4)
library(qpcR)

## Get rid of practice trials:

xdata <- xdata[xdata$Procedure.Block. != "Practice",]

## Look at overall response measures:

xdata$RespK.ACC.SubTrial.
# hist(xdata$RespK.ACC.SubTrial.)
xdata$RespK.RT.SubTrial.
# hist(log(xdata$RespK.RT.SubTrial.))

## Rename:

colnames(xdata)[colnames(xdata) == "RespK.ACC.SubTrial."] <- "ACC"

## Transform RT:

xdata$LogRT <- log(xdata$RespK.RT.SubTrial.+1)

## These are the conditions:

xdata$F0Level
xdata$F0LevelNum

## Center the condition variable:

xdata$F0LevelNum_c <- xdata$F0LevelNum-mean(xdata$F0LevelNum)

## Make a quadratic variable out of it:

xdata$F0LevelNum_c2 <- xdata$F0LevelNum_c^2

## Center log RT variable:

xdata$LogRT_c <- xdata$LogRT-mean(xdata$LogRT)

## Contrast code categorical variables that may participate in interactions:

xdata$SpeakerGender_c <- xdata$SpeakerGender
xdata$Gender_c <- xdata$Gender
contrasts(xdata$SpeakerGender_c) <- contr.sum(2)/2
contrasts(xdata$Gender_c) <- contr.sum(2)/2

## Some analysis:

xmdl_noslopes <- glmer(ACC ~ F0LevelNum_c + F0LevelNum_c2 +
	LogRT_c + Gender_c + 
	F0LevelNum_c:Gender_c + F0LevelNum_c2:Gender_c + 
	(1|SpeakerNum) + (1|Subject) + (1|Scenario.SubTrial.),
	xdata,family="binomial")
summary(xmdl_noslopes)

## With slopes:

xmdl_slopes <- glmer(ACC ~ F0LevelNum_c + F0LevelNum_c2 +
	LogRT_c + Gender_c + 
	F0LevelNum_c:Gender_c + F0LevelNum_c2:Gender_c + 
	(1|SpeakerNum) + (1|Subject) + (1|Scenario.SubTrial.) + 
	(0+F0LevelNum_c|Subject) + (0+F0LevelNum_c2|Subject) +
	(0+F0LevelNum_c|SpeakerNum) + (0+F0LevelNum_c2|SpeakerNum),
	xdata,family="binomial")
summary(xmdl_slopes)

## Is there a listener gender/condition interaction?

xmdl_slopes_no_gender_int <- glmer(ACC ~ F0LevelNum_c + F0LevelNum_c2 +
	LogRT_c + Gender_c + 
	(1|SpeakerNum) + (1|Subject) + (1|Scenario.SubTrial.) + 
	(0+F0LevelNum_c|Subject) + (0+F0LevelNum_c2|Subject) +
	(0+F0LevelNum_c|SpeakerNum) + (0+F0LevelNum_c2|SpeakerNum),
	xdata,family="binomial")
anova(xmdl_slopes_no_gender_int,xmdl_slopes)

## Create a model without the F0 Manipulation effect (this is testing for overall effect of F0):

xmdl_slopes_nomain <- glmer(ACC ~ LogRT_c + Gender_c + 
	(1|SpeakerNum) + (1|Subject) + (1|Scenario.SubTrial.) + 
	(0+F0LevelNum_c|Subject) + (0+F0LevelNum_c2|Subject) +
	(0+F0LevelNum_c|SpeakerNum) + (0+F0LevelNum_c2|SpeakerNum),
	xdata,family="binomial")
anova(xmdl_slopes_nomain,xmdl_slopes_no_gender_int)

## Test all effects:

drop1_res <- drop1(xmdl_slopes,test="Chisq")

## Testing for all slopes:

anova(xmdl_noslopes,xmdl_slopes)

## Testing for speaker-specific slopes:

xmdl_nospeaker_slopes <- glmer(ACC ~ F0LevelNum_c + F0LevelNum_c2 + 
	LogRT_c + Gender_c + 
	F0LevelNum_c:Gender_c + F0LevelNum_c2:Gender_c + 
	(1|SpeakerNum) + (1|Subject) + (1|Scenario.SubTrial.) + 
	(0+F0LevelNum_c|Subject) + (0+F0LevelNum_c2|Subject),
	xdata,family="binomial")
anova(xmdl_nospeaker_slopes,xmdl_slopes)
summary(xmdl_nospeaker_slopes)

## Testing for listener-specific slopes:

xmdl_nolistener_slopes <- glmer(ACC ~ F0LevelNum_c + F0LevelNum_c2 + 
	LogRT_c + Gender_c + 
	F0LevelNum_c:Gender_c + F0LevelNum_c2:Gender_c + 
	(1|SpeakerNum) + (1|Subject) + (1|Scenario.SubTrial.) + 
	(0+F0LevelNum_c|SpeakerNum) + (0+F0LevelNum_c2|SpeakerNum),
	xdata,family="binomial")
anova(xmdl_nolistener_slopes,xmdl_slopes)
summary(xmdl_nolistener_slopes)

## Comparing the two slope models against each other with evidence ratios:

evidence(xmdl_nospeaker_slopes,xmdl_nolistener_slopes)			# in the right direction?

## Testing for a potential speaker-gender effect:

xmdl_with_speaker_gender <- glmer(ACC ~ F0LevelNum_c + F0LevelNum_c2 +
	LogRT_c + Gender_c + SpeakerGender_c +
	F0LevelNum_c:Gender_c + F0LevelNum_c2:Gender_c + 
	(1|SpeakerNum) + (1|Subject) + (1|Scenario.SubTrial.) + 
	(0+F0LevelNum_c|Subject) + (0+F0LevelNum_c2|Subject) +
	(0+F0LevelNum_c|SpeakerNum) + (0+F0LevelNum_c2|SpeakerNum),
	xdata,family="binomial")
anova(xmdl_slopes,xmdl_with_speaker_gender)

## Testing for a potential speaker-gender/listener-gender interaction:

xmdl_with_speaker_gender_interaction <- glmer(ACC ~ F0LevelNum_c + F0LevelNum_c2 +
	LogRT_c + Gender_c + SpeakerGender_c + SpeakerGender_c:Gender_c + 
	F0LevelNum_c:Gender_c + F0LevelNum_c2:Gender_c + 
	(1|SpeakerNum) + (1|Subject) + (1|Scenario.SubTrial.) + 
	(0+F0LevelNum_c|Subject) + (0+F0LevelNum_c2|Subject) +
	(0+F0LevelNum_c|SpeakerNum) + (0+F0LevelNum_c2|SpeakerNum),
	xdata,family="binomial")
anova(xmdl_with_speaker_gender,xmdl_with_speaker_gender_interaction)
	# 



########################################################################
######################## Plot the model:
########################################################################

## For plotting we ignore speaker gender:

xmdl_slopes <- glmer(ACC ~ F0LevelNum_c + F0LevelNum_c2 + LogRT_c + Gender + 
	F0LevelNum_c:Gender + F0LevelNum_c2:Gender + 
	(1|SpeakerNum) + (1|Subject) + (1|Scenario.SubTrial.) + 
	(0+F0LevelNum_c|Subject) + (0+F0LevelNum_c2|Subject) +
	(0+F0LevelNum_c|SpeakerNum) + (0+F0LevelNum_c2|SpeakerNum),xdata,family="binomial")

## Get levels of condition variable to plot:

F0LevelNum_c_levels <- seq(-2,2,1)
F0LevelNum_c2_levels <- F0LevelNum_c_levels^2

## Extract coefficients of the model:

mc <- fixef(xmdl_slopes)

## Get the linear predictor for males and females respectively:

LP_female <- mc["(Intercept)"] + F0LevelNum_c_levels*mc["F0LevelNum_c"] + F0LevelNum_c2_levels*mc["F0LevelNum_c2"]
LP_male <- mc["(Intercept)"] + F0LevelNum_c_levels*(mc["F0LevelNum_c"]+mc["F0LevelNum_c:GenderM"]) +
	F0LevelNum_c2_levels*(mc["F0LevelNum_c2"]+mc["F0LevelNum_c2:GenderM"])

quartz("",8,6);par(mai=c(1,1.6,0.5,0.25))
plot(1,1,type="n",xlim=c(0.5,5.5),ylim=c(-0.05,1.05),xlab="",ylab="",xaxt="n",yaxt="n")
# abline(h=0.5,lty=2)
points(1:5,plogis(LP_female),type="b",lwd=2,pch=19,cex=1.5)
points(1:5,plogis(LP_male),type="b",lwd=2,pch=15,lty=2,cex=1.5)
# axis(side=2,at=c(0,1),labels=c("informal","polite"),las=2,font=2)
axis(side=2,at=seq(0,1,0.25),las=2,font=2,lwd.ticks=3,cex.axis=1.25)
axis(side=1,at=1:5,labels=c("-16%","-8%","0","+8%","+16%"),
	font=2,lwd.ticks=3,cex.axis=1.65)
mtext("F0 Manipulation",side=1,line=3.7,cex=1.85,font=2)
legend("topleft",lty=c(1,2),pch=c(19,15),lwd=c(2,2),
	legend=c("female listeners","male listeners"),cex=1.45)
box(lwd=3)
mtext("Proportion\nof polite responses",side=2,line=4.5,cex=1.75,font=2)

## Bodo Winter, July 30, 2015; Adapted graph for ICPhS talk slides

quartz("",9,6);par(mai=c(1,1.8,1,0.8))
plot(1,1,type="n",xlim=c(0.5,5.5),ylim=c(-0.05,1.05),xlab="",ylab="",xaxt="n",yaxt="n")
# abline(h=0.5,lty=2)
# axis(side=2,at=c(0,1),labels=c("informal","polite"),las=2,font=2)
box(lwd=3)
axis(side=1,at=1:5,labels=c("-16%","-8%","0","+8%","+16%"),
	font=2,lwd.ticks=3,cex.axis=1.65)
mtext("F0 Manipulation",side=1,line=3.7,cex=1.85,font=2)
axis(side=2,at=seq(0,1,0.25),las=2,font=2,lwd.ticks=3,cex.axis=1.25)
mtext("Proportion\nof polite responses",side=2,line=4.5,cex=1.75,font=2)
legend("topleft",lty=c(1,2),pch=c(19,15),lwd=c(2,2),
	legend=c("female listeners","male listeners"),cex=1.45)
points(1:5,logit.inv(LP_female),type="b",lwd=2,pch=19,cex=1.5)
# points(1:5,plogis(LP_male),type="b",lwd=2,pch=15,lty=2,cex=1.5)
# points(1:5,plogis(LP_female),type="b",lwd=4,pch=19,cex=1.7,col="darkred")
points(1:5,plogis(LP_male),type="b",lwd=4,pch=15,lty=2,cex=1.7,col="darkred")



