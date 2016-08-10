# DOCUMENTED LEAVE
# FINAL Model ---------------------------------------------------------------
library(pscl)
library(MASS)
library(boot)
library(vcd)
library(COUNT)
library(gmodels)
library(AER)
library(stargazer)
library(plyr)
library(reporttools)
library(texreg)

# LOAD DATA ---------------------------------------------------------------
# n=22507 #### those with UPL>70days excluded in Excel
#original <- read.csv("file:///Volumes/PRIVATE/Thesis/stripped1213.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
rm(list=setdiff(ls(), "original"))

# SUBSET DATA ---------------------------------------------------------------
AllData <- original
# Excl part year (n=930), WorkersComp (n=239) 
AllData <- (subset(AllData, AllData$PartialYr == 1 & AllData$UPL_WorkersComp == 0)) #remaining = 21,348
# subset based on holidays (n=117)
AllData <- (subset(AllData, AllData$ab_cat_cnt_PL < 245)) #remaining = 21,205

# RENAME VARIABLES --------------------------------------------------------
AllData <- rename(AllData, c("ActualAPSLvl"="Hierarchy"
                             , "APS_Tnr_in_Yrs_At_Extrct_Cnt"="Tenure"
                             , "MgrUPLperFTE"="MgrsAbs" 
                             , "TmAvgUPLp.FTE" = "TmAvgAbs"
                             , "Emple_Grp_Prmncy_Sts_Nm"="Contract"
                             , "Slry_Hrly_Rt"="Wage"
                             , "Exact_Age.1dp."="Age"
                             , "ab_cat_cnt_PL"="Holidays"
                             , "UPL_SickPersonalDoco"="SickDoc"
                             , "UPL_SickPersonalNoDoco"="SickNoDoc"
                             , "UPL_CarersNoDoco"="CarersNoDoco"
                             , "UPL_CarersDoco"= "CarersDoco"
                             , "UPL_Bereavement"= "Bereavement"
                             , "UPL_Mis" = "Mis"
                             , "UPL_LWOP"="LWOP"
                             , "Diversity.NESB"="NESB"
                             , "Diversity.Indigenous"="Indigenous"
                             , "Diversity.Disability"="Disability"
                             , "EduAttain_ordinal"="EduLvl"
                             , "No..of.Opened.Health.in.FY"="HealthCase"
                             , "HDIncntvePay"="Incentive"
                             , "UPL_WorkersComp" = "WrkrsComp"
))

# TODO ## BAR CHART OF LEAVE

# DEPENDENT VARIABLE ------------------------------------------------------
# sum of uncertified leave adjusted for P/T hours N.B. counting eqns need integers for dependent
AllData["Abs"] <- (AllData$CarersDoco + AllData$SickDoc +AllData$CarersNoDoco + AllData$SickNoDoc + AllData$Bereavement + AllData$Mis+ AllData$LWOP)/AllData$FTE
AllData$Abs <- as.integer(AllData$Abs)

# DROP UNNEEDED DATA  ------------------------------------------------------
notneeded <- c("TmSlryStdDev","APSTenStdDev","PartialYr","TenDiffw.TmAvg", "Slrydiffw.TmAvg","TmAgeStdDev","Agediffw.TmAvg","Pos_Loc","No..of.Opened.People.in.FY"
               ,"UPLLessCompo","ab_cat_cnt_UPL", "UPLperFTE","Tax_Ofc_Org_Unt_Id","Job_Fmly_Txt","BSL","Unique_ID","MgrUniqID"
               ,"UPL_WorkersComp"
)
AllData <- AllData[,!(names(AllData) %in% notneeded)]
attach(AllData)

# OFFSET OF TIME ---------------------------------------------------
# assume approx 245 workdays in a year (365-wkends-publichols-xmasclosedown)
AllData["Time"] <- 245 - AllData$Holidays

# INDEPENDENT VARIABLES & FINAL DATA ---------------------------------------------------
#group Hierarchy - too many lvls APS1-3+CAD, APS5-6+VAL+GAA,EL1, EL2.2-Com 
AllData$Hierarchy <- cut(AllData$Hierarchy
                         , breaks = c(-Inf,5,8,10,Inf)
                         , labels = c("Entry","LoMid","HiMid","Snr")
                         , right = FALSE
)
# Edu - Group and Order 
AllData$EduLvl <-factor(AllData$EduLvl
                        , levels=c("NonUni","Undergrad","Postgrad")
)

#health case to binary indicator
AllData$HlthCase[AllData$HealthCase< 1] <- "N"
AllData$HlthCase[AllData$HealthCase>= 1] <- "Y"
AllData$HlthCase <- factor(AllData$HlthCase)
AllData$HealthCase <- NULL

#part-timer to binary
AllData$PartTime[AllData$FTE < 1] <- "Y"
AllData$PartTime[AllData$FTE == 1] <- "N"
AllData$PartTime <- factor(AllData$PartTime)
AllData$FTE <- NULL

#Incentive to binary
AllData$Incentive[AllData$Incentive > 0] <- "Y"
AllData$Incentive[AllData$Incentive == 0] <- "N"
AllData$Incentive <- factor(AllData$Incentive)

# Final data 
include <- c( "IsMgr", "PartTime", "Hierarchy", "EduLvl","Holidays"
              ,"Contract", "MgrsAbs", "Disability", "TeamSize", "TmAvgAbs", "NESB" 
              , "Indigenous", "HlthCase", "Sex", "Tenure", "Age", "Wage","Abs"
              ,"Time", "Incentive")
AllData <- AllData[,(names(AllData) %in% 
                       include)] #final

# DESCRIPTIVE STATS TABLE -------------------------------------------------
# split vars into numeric and nominal dataframes
include <- c("IsMgr", "Contract","Hierarchy", "Sex", "NESB", "Indigenous"
             , "Disability", "EduLvl", "PartTime", "HlthCase","Incentive")

CatTable <- AllData[,(names(AllData) %in% include)]
tN <- tableNominal(CatTable
             ,cap = NULL
             ,lab = NULL
             ,longtable=FALSE
             ,cumsum=FALSE
             )

# Copy latex output for template into:
# save(tN, file = "/Users/lukesingham/Google Drive/Honours/Thesis/Tables Decriptive statistics/autosavetest.tex")
# 
#NUMERIC SUMMARY w.reporttools
NumTable <- AllData[,!(names(AllData) %in% include)]
tableContinuous(NumTable
                ,cap = NULL
                ,lab = NULL
                ,longtable=FALSE
                ,stats = c("n","s","mean","min", "max"))
# detach("package:reporttools", unload=TRUE)
# 
# #NUMERIC SUMMARY
# stargazer(AllData
#           #           , type="html"
#           #           , out="/Users/lukesingham/Google Drive/Honours/Thesis/model_output/Summary/statTable.htm"
# )
#N.B. creates html output. Open with word and save as docx

# CENTRE PREDICTORS -------------------------------------------------------
AllData$MgrsAbs <- scale(AllData$MgrsAbs, center = TRUE, scale = FALSE)
AllData$Tenure <- scale(AllData$Tenure,center = TRUE, scale = FALSE)
AllData$Wage <- scale(AllData$Wage, center = TRUE, scale = FALSE)
AllData$Age <- scale(AllData$Age, center = TRUE, scale = FALSE)
AllData$TmAvgAbs <- scale(AllData$TmAvgAbs, center = TRUE, scale = FALSE)
AllData$TeamSize <- scale(AllData$TeamSize, center = TRUE, scale = FALSE)

#rm unneeded cols
AllData.na <-na.omit(AllData) #limitation of zeroinfl() - doesn't take na.action=na.exclude in 
# attach - i.e. no need to type 'AllData.na' from this point on...
# attach(AllData.na)

# Pearwise Correlations PLOT ------------------------------------------------------------------
# notneeded2 <- c("IncentiveBnry","Time","Disability")
# plot.AllData <- AllData.na[,!(names(AllData.na) %in% notneeded2)]
# library(car)
# scatterplotMatrix(plot.AllData,pch=19,cex=.5,reg.line=F, spread=F,ellipse=T
#                   , col=c('gray60','#2957FF','#FF8000')
#                   , col.axis='gray50'
#                   , plot.points = FALSE
# )
# detach(car, unload=TRUE)
# rm(notneeded2, plot.AllData)

# RUN MODELS ------------------------------------------------------------------
# Poisson model
Poiss <- glm(Abs ~ IsMgr + PartTime + EduLvl + Hierarchy + Incentive+ Disability+Contract + MgrsAbs + TeamSize + TmAvgAbs + NESB + Indigenous + HlthCase + Sex + Tenure + I(Tenure^2)+Age+   offset(log(Time))
                 , family = poisson
                 , data = AllData.na
)

# NB2 model
NB <- glm.nb(Abs ~ IsMgr + PartTime + EduLvl + Hierarchy + Incentive+ Disability+Contract + MgrsAbs + TeamSize + TmAvgAbs + NESB + Indigenous + HlthCase + Sex + Tenure + I(Tenure^2)+Age+   offset(log(Time))
                 , data = AllData.na
)

# for VIF.. include wages
NBw.wages <- glm.nb(Abs ~ IsMgr + PartTime + EduLvl + Hierarchy +Wage+ Incentive+ Disability+Contract + MgrsAbs + TeamSize + TmAvgAbs + NESB + Indigenous + HlthCase + Sex + Tenure + I(Tenure^2)+Age+   offset(log(Time))
                    , data = AllData.na
)


# ZIP model
ZIP <- zeroinfl(Abs ~ IsMgr + PartTime + EduLvl + Hierarchy + Incentive+ Disability+Contract + MgrsAbs + TeamSize + TmAvgAbs + NESB + Indigenous + HlthCase + Sex + Tenure + I(Tenure^2)+Age+   offset(log(Time))
                    , data = AllData.na
                    , dist = "poisson"
                    , EM = TRUE)

# ZINB model #Age^2 not significant
ZINB <- zeroinfl(Abs ~ IsMgr + PartTime + EduLvl + Hierarchy + Incentive+ Disability+Contract + MgrsAbs + TeamSize + TmAvgAbs + NESB + Indigenous + HlthCase + Sex + Tenure + I(Tenure^2)+Age+  offset(log(Time))
                     , data = AllData.na
                     , dist = "negbin"
                     , EM = TRUE)
summary(ZINB)

# RESIDUAL PLOTS ------------------------------------------------------------------
# ZINB
png(filename="/Users/lukesingham/Google Drive/Honours/Thesis/model_output/Summary/ZINB_QQPlot.png")
qqnorm(residuals(ZINB)) #
qqline(residuals(ZINB)) # 
dev.off()

# COMMANDS TO EXTRACT STATS
# summary(NB) 
# confint(NB) 
# exp(coef(NB)) 
# exp(confint(NB)) 

# TEST STATISTICS ---------------------------------------------------------
sapply(list(Poiss,NB,ZIP,ZINB),AIC)
sapply(list(Poiss,NB,ZIP,ZINB),logLik) #closer to zero the better

#Poisson is a terrible fit! WHy? Equidistribution doesn't hold for data.
### Not a great fit due to over-dispersion, look at mean-var relationship
mu <- mean(AllData.na$Abs, na.rm=T) #11.47
variance <- var(AllData.na$Abs, na.rm=T)  # 98.72... over 8x greater
variance/mu
# further overdispersion test is true
x <- dispersiontest(Poiss)
x2 <- matrix(c(x$statistic
               , x$p.value
               , x$null.value
               , x$alternative
               , x$estimate
              )
             , dimnames=list(c("z = ", "p-value", "Null", "Alt True", "Est.Var")
                             #, c("") #Title
                             )) 
DispersionTable <- latex(x2
                         , file = "/Users/lukesingham/Google Drive/Honours/Thesis/LaTeX Final/Tables/OverDispTest"
                         , title="Poisson Overdispersion Test"
                         , caption="Poisson Overdispersion Test Using OLS"
                         )

# Deviance test Poisson vs. NB # Doesn't count theta in DFs
anov <- anova(Poiss, NB) # NB has much lower deviance of residuals
#odTest - tests against a Poisson - favours the NB
#odTest(NB) does the same as dispersiontest()

# Vuong test... can't c.f. Poiss w. NB as nested to each other (i.e. when dispersion = 1, the models are the same)
#A large, positive test statistic provides evidence of the superiority of model 1 over model 2, while a large, negative test statistic is evidence of the superiority of model 2 over model 1.
vuong(NB, ZIP)  # 38.42465 strongly prefers ZIP of p-value 0 
vuong(Poiss, ZINB) # -43.53824 strongly prefers zinb p-val of 0
vuong(NB, ZINB) # -8.928751 strongly prefers zinb p-val of 0
vuong(ZIP, ZINB)  # strongly prefers zinb -39.28297 p-val 0

# Variance Inflation Factor (VIF)
vif(NBw.wages) # wage 5.2322871, once removed i.e. the NB model, all factors below 3
# http://www.statisticalhorizons.com/multicollinearity
vif(NB)

# PREDICTED COUNT PROBABILITIES -------------------------------------------
# PoisNUmon
phat.pois <- predprob(Poiss)  # for each subj, prob of observing each value
phat.pois.mn <- apply(phat.pois, 2, mean) # mean predicted probs
### NB
phat.nb <- predprob(NB)       # for each subj, prob of observing each value
phat.nb.mn <- apply(phat.nb, 2, mean) # mean predicted probs
### ZIP
phat.zip <- predprob(ZIP)           # for each subj, prob of observing each value
phat.zip.mn <- apply(phat.zip, 2, mean) # mean predicted probs
## ZINB
phat.zinb <- predprob(ZINB)           # for each subj, prob of observing each value
phat.zinb.mn <- apply(phat.zinb, 2, mean) # mean predicted probs

# HISTOGRAM PLOT WITH FITTED PROBABILITIES --------------------------------
# plot aesthetics http://www.statmethods.net/advgraphs/parameters.html
png(filename="/Users/lukesingham/Google Drive/Honours/Thesis/FINAL OUTPUTS/Graphs & Plots/cfModelFit.png")
with(AllData.na, {
  hist(Abs, prob = TRUE, col = "grey60", breaks=seq(-0.5, 69.5, 1), xlab = "Days Absent"
       ,main = NULL, ylim=c(0, .09)
  )
  #Poisson
  lines(x = seq(0, 69, 1), y = phat.pois.mn, lty=2, lwd=2, col="red")
  points(x = seq(0, 69, 1), y = phat.pois.mn, cex=1, pch=16, col="red")
  #NB
  lines(x = seq(0, 69, 1), y = phat.nb.mn, lty=5, lwd=2, col="blue")
  points(x = seq(0, 69, 1), y = phat.nb.mn, cex=1, pch=15, col="blue")
  #ZIP
  lines(x = seq(0, 69, 1), y = phat.zip.mn, lty=3, lwd=4, col="green3")
  points(x = seq(0, 69, 1), y = phat.zip.mn, cex=1, pch=17, col="green3")
  #ZINB
  lines(x = seq(0, 69, 1), y = phat.zinb.mn, lty=1, lwd=2, col="black")
  points(x = seq(0, 69, 1), y = phat.zinb.mn, cex=1, pch=18, col="black")
})
legend(45, 0.08, c("Poisson","NB","ZIP","ZINB")
       , lty=1
       , col = c("red","blue","green3","black")
       , pch=c(16,15,17,18)
       )
dev.off()

# PLOT RESIDUAL PROBABILITIES ----------------------------------------------------------
# get original densities and predicted probs in a single df
pred.df <- data.frame(original = table(AllData.na$Abs)/nrow(AllData.na)
                      ,pois = phat.pois.mn 
                      ,nb = phat.nb.mn
                      ,zip = phat.zip.mn
                      ,zinb = phat.zinb.mn)
### get residual probabilities
pred.resid <- pred.df[,3:6] - pred.df[,2]
### plot
png(filename="/Users/lukesingham/Google Drive/Honours/Thesis/FINAL OUTPUTS/Graphs & Plots/cfModelFitResidualProbabilities.png")
matplot(pred.resid, type = "b"
        , lwd= c(2,2,4,4) #line width
        , lty= c(2,5,3,1)
        , pch= c(16,15,17,18)
        , cex = 1
        , col = c("red","blue","green3","black")
        , ylab = "Predicted - Observed"
        , xlab = "Counts") #NOTE: actually counts + 1
abline(h=0)
legend(45, 0.04, c("Poisson","NB","ZIP","ZINB")
       , lty=1
       , col = c("red","blue","green3","black")
       , pch=c(16,15,17,18)
)
dev.off()

# ZINB - OUTLIERS & RESIDUAL PLOTS ---------------------------------------------------------
# n.b. the default is Pearson residuals
png(filename="/Users/lukesingham/Google Drive/Honours/Thesis/FINAL OUTPUTS/Graphs & Plots/ZINBresiduals.png")
plot(residuals(ZINB)) # Residuals Plot
dev.off()

# residuals vs predicted
png(filename="/Users/lukesingham/Google Drive/Honours/Thesis/FINAL OUTPUTS/Graphs & Plots/ZINBresidualsvsPredict.png")
plot(residuals(ZINB)~predict(ZINB)) #residuals for predicted counts
dev.off()

# qqnorm plots - hard to find any legitimacy from sources as to application on ZINB
# png(filename="/Users/lukesingham/Google Drive/Honours/Thesis/FINAL OUTPUTS/Graphs & Plots/ZINBresidualsvsPredict.png")
# qqnorm(residuals(ZINB)) #
# qqline(residuals(ZINB)) # 
# dev.off()

# Pearson residuals with fitted line
plot(fitted(ZINB), residuals(ZINB,"pearson"),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(ZINB), residuals(ZINB,"pearson")), col='red')

# Response/'raw' residuals with fitted line
plot(fitted(ZINB), residuals(ZINB,"response"),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(ZINB), residuals(ZINB,"response")), col='red')

#looks like one outlier impacts adversely
# x <- as.data.frame(fitted(ZINB))
# subset(x, fitted(ZINB)>35)

# DEPENDENT VARIABLE PLOT ------------------------------------------------------
#DISTRIBUTION OF UNDOCUMENTED
# png(filename="/Users/lukesingham/Google Drive/Honours/Thesis/model_output/Summary/Distribution.png")
# hist(mydata.res$UndocAbs, prob = TRUE, col = "grey60", breaks=seq(-0.5, 19.5, 1), xlab = "Days Absent"
#      ,main = "Days Absent (uncertified)")
# dev.off()

# REMOVE OUTLIERS (FOR FINAL ESTIMATES) ---------------------------------------------------------
# Table with residuals to id outliers
# res <- residuals(ZINB)
# mydata.res <- cbind.data.frame(mydata,res) # 8 above with high PLleave and undoc
# mydata.res <- subset(mydata.res, mydata.res$res < 10) #rm outliers
# ZINB <- zeroinfl(UndocAbs ~ IsMgr + PartTime + EduLvl +  Contract + MgrsAbs + Disability + TeamSize + TmAvgAbs + NESB + Indigenous + HlthCase + Sex + Tenure + Age + Wage+ offset(log(Time))
#                     , data = mydata.res #rerun ZINB w/o outilers
#                     , dist = "negbin"
#                     , EM = TRUE
# )

# LATEX SUMMARY REGRESSION TABLE -----------------------------------------------------------
Copy and paste texreg output into Rnw file
texreg(list(Poiss, NB, ZIP, ZINB)
       ,include.bic=FALSE
       ,include.dev=FALSE
       ,include.theta=FALSE
       ,single.row=TRUE
       ,include.count = TRUE #common across 4 models
       ,include.zero = TRUE #only in ZIP/ZINB
       ,digits = 3 #decimal places
       ,leading.zero = FALSE
       # ,beside = TRUE
)

stargazer(Poiss, NB, ZIP, ZINB
          #, type="text"
          , align=TRUE
          ,zero.component=TRUE
          )

#stepwiseAIC using bestglm #  BUT DOCUMENTATION DOESN'T INCLUDE NEGBIN :( 
# library(bestglm)
# include <- c( "IsMgr", "PartTime", "Hierarchy", "EduLvl","Holidays"
#               ,"Contract", "MgrsAbs", "Disability", "TeamSize", "TmAvgAbs", "NESB" 
#               , "Indigenous", "HlthCase", "Sex", "Tenure", "Age", "Wage")
# X <- mydata[,(names(mydata) %in% include)]
# y <- mydata[,25]
# Xy <- cbind.data.frame(X,y)
# # WARNING - computationally expensive (N^2+N)/2 where N=no of vars
# bestglm.out <- bestglm(Xy, IC="AIC",intercept=FALSE,method="backward")
# # top 5 models
# bestglm.out$BestModels #3/5 excl 'Sex', Disability.
# # show stat results of top model
# summary(bestglm.out$BestModel)

# use step() from stats, scope is the upper and lower models i.e. full (all_nb) and null
# is quick - estimates a rather full model!
null = glm.nb(Abs ~ 1, data = AllData.na)
full = glm.nb(Abs ~ IsMgr + PartTime + EduLvl + Hierarchy + Incentive+ Disability+Contract + MgrsAbs + TeamSize + TmAvgAbs + I(Age^2) +NESB + Indigenous + HlthCase + Sex + Tenure + I(Tenure^2)+Age+   offset(log(Time))
                    , data = AllData.na
)
stepF <- step(null, scope=list(lower=null,upper=full), direction="forward")
stepB <- step(full, scope=list(lower=null,upper=full), direction="backward")

# #STEP AIC using glm.nb
# step_nb <- glm.nb(UndocAbs ~ IsMgr + Hierarchy + PartTime + EduLvl + Holidays + Contract + MgrsAbs + Disability + TeamSize + TmAvgAbs + NESB + Indigenous + HlthCase + Sex + Tenure + Age + Wage
#                   , data=omittednas)
# #stepAIC fucntion from MASS - backward
# BstepAIC.out <- stepAIC(step_nb, scope=list(lower=null,upper=full), direction="backward",trace=TRUE)
# #forward
# FstepAIC.out <- stepAIC(step_nb, scope=list(lower=null,upper=full), direction="forward",trace=TRUE)

# use of dredge 
# library(MuMIn)
# dredZINB <- dredge(all_ZINB)

# INTERPRETING COEFFICIENTS -----------------------------------------------

# Interpret count component as a % IRR
100*(exp(coef(ZINB)[2:21]) - 1)   # 1st - 17th coefficients

#Interpret logistic coefficients on % odds-ratio scale
100*(exp(coef(ZINB)[23:42])-1)


xx <- seq(min(Tenure),max(Tenure),len=1000)
yy <- ZINB$coef %*% rbind(1,xx,xx^2)
lines(xx,yy,lwd=2,col=3)


# bootstrap function
boot.zinb <- function(data, indices){
  require(pscl)               # make sure zeroinfl() is available
  data <- data[indices,]      # select obs. in bootstrap sample
  try(mod <- zeroinfl(Abs ~ IsMgr + PartTime + EduLvl + Hierarchy + Incentive+ Disability+Contract + MgrsAbs + TeamSize + TmAvgAbs + NESB + Indigenous + HlthCase + Sex + Tenure + I(Tenure^2)+Age+  offset(log(Time))
                              , data = data
                              , dist = "negbin"
                              , EM = TRUE))
  if (exists("mod")) {
    coef(mod)
  } else {
    NA
  }                   # return coefficient vector
}

# WARNING - user time 2304.841 (38mins) thus a copy of the results are below!
system.time(zinb.boot.out <- boot(data = AllData.na, statistic = boot.zinb, R = 99))
BOOTSTRAP RESULTS -----------------------------------------------
ORDINARY NONPARAMETRIC BOOTSTRAP

Call:
  boot(data = AllData.na, statistic = boot.zinb, R = 99)

Bootstrap Statistics :
  original          bias      std. error
t1*  -2.9145986235 -0.002015633784 0.09677501295
t2*  -0.1600760045  0.003628139272 0.02466278609
t3*   0.1422467463  0.005901179788 0.02283679054
t4*  -0.0590691436  0.003678676104 0.01472429179
t5*  -0.0553594412  0.005585649170 0.02241567283
t6*  -0.1110547108 -0.002147004236 0.02741788415
t7*  -0.1296871738 -0.003367752964 0.03135776418
t8*  -0.2751223198 -0.005792183525 0.04797200911
t9*  -0.1284158530 -0.001968053672 0.02304460710
t10*  0.1156456518 -0.007473524411 0.04854911387
t11*  0.2944418914  0.001452850103 0.09269155493
t12*  0.0049530979  0.000050678098 0.00085041399
t13* -0.0010568227 -0.000151026883 0.00204742248
t14*  0.0132787408 -0.000334463172 0.00135403024
t15*  0.0100180143 -0.001980283909 0.01548890570
t16*  0.1596450316 -0.000357795920 0.07156153732
t17*  0.3398894688 -0.001221546470 0.02297743806
t18* -0.1111446032  0.000311285494 0.01683878608
t19*  0.0131533369  0.000016834680 0.00137189091
t20* -0.0008145074  0.000004609076 0.00009803841
t21*  0.0029546360  0.000111439729 0.00094229984
t22* -9.8561624969 -0.215506529749 0.54294804212
t23*  0.3557084739  0.056275958272 0.22375834812
t24* -2.3407362536 -3.427475138290 4.87368533505
t25*  0.2981758384  0.011888123893 0.22768130964
t26*  0.3094608364 -0.001649771096 0.27118072829
t27* -0.4457082438 -0.020045099012 0.23733400953
t28* -0.6851869144 -0.030625685726 0.31140192884
t29* -0.5696782307 -0.041596170819 0.48490343754
t30*  0.0643836985 -0.019124454093 0.30830083047
t31*  0.0520191196 -0.048180596595 0.48969610225
t32* -0.3967042770  0.121905268363 0.51474290743
t33* -0.0184202626 -0.001286115172 0.01297682561
t34*  0.0888673693  0.001389453964 0.01394123906
t35* -0.2496834336 -0.003375441103 0.02707007967
t36* -0.0268887425  0.003392979159 0.16211377111
t37* -0.0108483037 -0.604576501407 2.21990061062
t38* -1.5746830694 -0.044814194244 0.52271676222
t39*  0.5824659063  0.033619866046 0.17916253040
t40* -0.0398405750 -0.000529706391 0.01695432967
t41*  0.0028483062  0.000024383403 0.00071394410
t42*  0.0251819871 -0.000401545856 0.00863851662


