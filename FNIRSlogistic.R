# Analysis of fNIRS data using logistic regression

# Read data

load("delbox_subj_scores.rdata")
load("delbox_scores_demog.rdata")
load("vft_subj_scores.rdata")
load("vft_score_demog.rdata")

library('pROC')

dat = transform(vft.comb_data)
y = dat[,1]

X1 = X[,1:9]                # Behavioral data
U1 = delbox.subj.scores     # SAT PC scores
QQ = vft.subj.scores[-33,]  # VFT PC scores


# Perform step-wise regression

fit1 = glm(formula = y ~ X1[,1]+X1[,2]+X1[,3]+X1[,4]+X1[,5]+X1[,6]+X1[,7]+X1[,8]+X1[,9], family = binomial(link = "logit"))
fit2 = glm(formula = y ~ U1[,1]+U1[,2]+U1[,3]+U1[,4]+U1[,5]+U1[,6]+U1[,7]+U1[,8]+U1[,9], family = binomial(link = "logit"))
fit3 = glm(formula = y ~ QQ[,1]+QQ[,2]+QQ[,3]+QQ[,4]+QQ[,5]+QQ[,6]+QQ[,7]+QQ[,8]+QQ[,9]+QQ[,10]+QQ[,11]+QQ[,12], family = binomial(link = "logit"))

nothing = glm(formula = y ~ 1, family = binomial(link = "logit"))
backwards1 = step(fit1) 
forwards1 = step(nothing,scope=list(lower=formula(nothing),upper=formula(fit1)), direction="forward")

# > formula(forwards1)
# y ~ X1[, 7] + X1[, 1]
# > formula(backwards1)
# y ~ X1[, 1] + X1[, 7]


backwards2 = step(fit2) 
forwards2 = step(nothing,scope=list(lower=formula(nothing),upper=formula(fit2)), direction="forward")

# > formula(forwards2)
# y ~ U1[, 5] + U1[, 7]
# > formula(backwards2)
# y ~ U1[, 5] + U1[, 7]


backwards3 = step(fit3) 
forwards3 = step(nothing,scope=list(lower=formula(nothing),upper=formula(fit3)), direction="forward")

# > formula(forwards3)
# y ~ QQ[, 2]
# > formula(backwards3)
# y ~ QQ[, 8] + QQ[, 11]



fit4 = glm(formula = y ~ X1[,1]+X1[,7], family = binomial(link = "logit"))
fit5 = glm(formula = y ~ U1[,5]+U1[,7], family = binomial(link = "logit"))
fit6 = glm(formula = y ~ QQ[,8]+QQ[,11], family = binomial(link = "logit"))

fit6 = glm(formula = y ~ QQ[,2]+QQ[,8], family = binomial(link = "logit"))
fit6 = glm(formula = y ~ QQ[,2]+QQ[,17], family = binomial(link = "logit"))


prob4=predict(fit4,type=c("response"))
g4 <- roc(y ~ prob4)

prob5=predict(fit5,type=c("response"))
g5 <- roc(y ~ prob5)

prob6=predict(fit6,type=c("response"))
g6 <- roc(y ~ prob6)



# Plot ROC - SAT
 
rocobj <- plot.roc(y, prob5,  main="ROC curve (SAT)", percent=TRUE,  ci=TRUE,  print.auc=FALSE) 
ciobj <- ci.se(rocobj, # CI of sensitivity
               specificities=seq(0, 100, 5)) # over a select set of specificities  
plot(ciobj, type="shape", col="#1c61b6AA") # plot as a blue shape 


# Plot ROC - VFT

rocobj <- plot.roc(y, prob6,  main="ROC curve (VFT)", percent=TRUE,  ci=TRUE,  print.auc=FALSE) 
ciobj <- ci.se(rocobj, # CI of sensitivity
               specificities=seq(0, 100, 5)) # over a select set of specificities  
plot(ciobj, type="shape", col="#1c61b6AA") # plot as a blue shape 


