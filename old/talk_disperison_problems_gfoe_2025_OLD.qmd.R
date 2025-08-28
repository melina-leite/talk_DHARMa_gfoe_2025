## DATA ----
set.seed(5) 
heteroData = createData(sampleSize = 500, intercept = 0.5, fixedEffects = -1,  
                        overdispersion = function(x){
                          return(rnorm(length(x), sd = 0.45 * abs(x)))}, 
                        family = poisson(), randomEffectVariance = 0.01)
heteroData$observedResponse2 <- heteroData$observedResponse*-1

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
# conditional sim
set_simcodes(heteroModelWrong$obj, val="fix")
heteroResWrongC <- simulateResiduals(heteroModelWrong)
testDispersion(heteroResWrongC, plot=F)

## CORRECT MODEL ----
heteroModelCorrect <- glmmTMB(observedResponse ~ Environment1 + (1|group),
                              dispformula = ~Environment1,
                              family = nbinom2(), data = heteroData)
summary(heteroModelCorrect)
heteroResCorrect <- simulateResiduals(heteroModelCorrect)
plot(heteroResCorrect)
testDispersion(heteroResCorrect, plot=F)
# conditional sim
set_simcodes(heteroModelCorrect$obj, val="fix")
heteroResCorrectC<- simulateResiduals(heteroModelCorrect)
testDispersion(heteroResCorrectC, plot=F)

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

## COMPARING MODELS ----
bbmle::AICctab(heteroModelWrong,heteroModelCorrect,heteroModelZero, heteroModelOver)

