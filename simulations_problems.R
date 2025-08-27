

# POISSON NO PROBLEMS -----

set.seed(5)
poisData <- createData(500, intercept = 0.5, 
                       family=poisson(),
                       randomEffectVariance = 0.01, )





# HETEROSCEDASTICITY -----

## DATA ----
set.seed(5) 
heteroData = createData(sampleSize = 500, intercept = 0.5,  
                        overdispersion = function(x){
                          return(rnorm(length(x), sd = 0.45 * abs(x)))}, 
                        family = poisson(), randomEffectVariance = 0.01)

ggplot(heteroData, aes(x=Environment1, y=observedResponse))+
  geom_point() +
  geom_smooth(method="glm", method.args = list(family="poisson"))


## WRONG MODEL ----

heteroModelWrong <- glmmTMB(observedResponse ~ Environment1 + (1|group), 
                       family = "poisson", data = heteroData)
summary(heteroModelWrong)

# unconditional sim
heteroResWrong <- simulateResiduals(heteroModelWrong)
plot(heteroResWrong)
testDispersion(heteroResWrong, plot=F)

## CORRECT MODEL ----
heteroModelCorrect <- glmmTMB(observedResponse ~ Environment1 + (1|group),
                              dispformula = ~Environment1,
                              family = nbinom2(), data = heteroData)
summary(heteroModelCorrect)
heteroResCorrect <- simulateResiduals(heteroModelCorrect)
plot(heteroResCorrect)
testDispersion(heteroResCorrect, plot=F)

## OTHER WRONG MODELS ----

# zero inflated
heteroModelZero <-  glmmTMB(observedResponse ~ Environment1 + (1|group),
                            ziformula =  ~1,
                            family = poisson(), data = heteroData)
heteroResZero <- simulateResiduals(heteroModelZero)
plot(heteroResZero)
testDispersion(heteroResZero, plot=F)

# overdispersed
heteroModelOver <-  glmmTMB(observedResponse ~ Environment1 + (1|group),
                            family = nbinom2(), data = heteroData)
heteroResOver <- simulateResiduals(heteroModelOver)
plot(heteroResOver)
testDispersion(heteroResOver, plot=F)
plotResiduals(heteroResOver, form = heteroData$Environment1, absoluteDeviation = T)

## COMPARING MODELS ----
bbmle::AICctab(heteroModelWrong,heteroModelCorrect,heteroModelZero, heteroModelOver)



# ZERO-INFLATION -----
set.seed(5) 
zeroData <- createData(sampleSize = 500, intercept = 2,
                       randomEffectVariance = 0.01,
                       pZeroInflation = 0.6)
ggplot(zeroData, aes(x=Environment1, y=observedResponse))+
  geom_point() +
  geom_smooth(method="glm", method.args = list(family="poisson"))

zeroModelWrong <- glmmTMB(observedResponse ~ Environment1 + (1|group), 
                     family = "poisson", data = zeroData)
summary(zeroModelWrong)

# unconditional sim
zeroResWrong <- simulateResiduals(zeroModelWrong)
plot(zeroResWrong)
testDispersion(zeroResWrong, plot=F)

# conditional sim
set_simcodes(zeroModelWrong$obj, val="fix")
zeroResWrongC <- simulateResiduals(zeroModelWrong)
testDispersion(zeroResWrongC, plot=F)

# corrected model
zeroModelCorrect <- glmmTMB(observedResponse ~ Environment1 + (1|group),
                            ziformula = ~1,
                            family = poisson(), data = zeroData)
summary(zeroModelCorrect)
zeroResCorrect <- simulateResiduals(zeroModelCorrect)
plot(zeroResCorrect)
testDispersion(zeroResCorrect, plot=F)
# conditional sim
set_simcodes(zeroModelCorrect$obj, val="fix")
zeroResCorrectC<- simulateResiduals(zeroModelCorrect)
testDispersion(zeroResCorrectC, plot=F)


## OTHER WRONG MODELS ----

# heteroscedasticity
zeroModelHetero <-  glmmTMB(observedResponse ~ Environment1 + (1|group),
                            dispformula =  ~ Environment1,
                            family = poisson(), data = zeroData)
zeroResHetero <- simulateResiduals(zeroModelHetero)
plot(zeroResHetero)
testDispersion(zeroResHetero, plot=F)

# overdispersed
zeroModelOver <-  glmmTMB(observedResponse ~ Environment1 + (1|group),
                            family = nbinom2(), data = zeroData)
zeroResOver <- simulateResiduals(zeroModelOver)
plot(zeroResOver)
testDispersion(zeroResOver, plot=F)

## COMPARING MODELS ----
bbmle::AICctab(zeroModelWrong,zeroModelCorrect,zeroModelHetero, zeroModelOver)




# REAL OVERDISPERSION -----
set.seed(5) 
overData <- createData(sampleSize = 500, intercept = 1,
                       randomEffectVariance = 0.01,
                       overdispersion = 0.8)
ggplot(overData, aes(x=Environment1, y=observedResponse))+
  geom_point() +
  geom_smooth(method="glm", method.args = list(family="poisson"))

overModelWrong <- glmmTMB(observedResponse ~ Environment1 + (1|group), 
                     family = "poisson", data = overData)
summary(overModelWrong)

# unconditional sim
overResWrong <- simulateResiduals(overModelWrong)
plot(overResWrong)
testDispersion(overResWrong, plot=F)

# conditional sim
set_simcodes(overModelWrong$obj, val="fix")
overResWrongC <- simulateResiduals(overModelWrong)
testDispersion(overResWrongC, plot=F)

# corrected model
overModelCorrect <- glmmTMB(observedResponse ~ Environment1 + (1|group),
                            family = nbinom2(), data = overData)
summary(overModelCorrect)
overResCorrect <- simulateResiduals(overModelCorrect)
plot(overResCorrect)
testDispersion(overResCorrect, plot=F)
# conditional sim
set_simcodes(overModelCorrect$obj, val="fix")
overResCorrectC<- simulateResiduals(overModelCorrect)
testDispersion(overResCorrectC, plot=F)

## OTHER WRONG MODELS ----

# heteroscedasticity
overModelHetero <-  glmmTMB(observedResponse ~ Environment1 + (1|group),
                            dispformula =  ~ Environment1,
                            family = poisson(), data = overData)
overResHetero <- simulateResiduals(overModelHetero)
plot(overResHetero)
testDispersion(overResHetero)

# zero inflated
overModelZero <-  glmmTMB(observedResponse ~ Environment1 + (1|group),
                            ziformula =  ~1,
                            family = poisson(), data = overData)
overResZero <- simulateResiduals(overModelZero)
plot(overResZero)
testDispersion(overResZero, plot=F)

## COMPARING MODELS ----
bbmle::AICctab(overModelWrong,overModelCorrect,overModelHetero, overModelZero)


