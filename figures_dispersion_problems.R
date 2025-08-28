#devtools::install_github("gadenbuie/ggpomological")


library(ggpomological)
library(magick)

## HETEROSCEDASTICITY -----
set.seed(5)
poisData <- createData(100, intercept = 1, 
                       family=poisson(),
                       randomEffectVariance = 0, )
set.seed(5) 
heteroData = createData(sampleSize = 100, intercept = 1,  
                        overdispersion = function(x){
                          return(rnorm(length(x), sd = 0.45 * abs(x)))}, 
                        family = poisson(), randomEffectVariance = 0)
#heteroData <- heteroData |> filter(observedResponse < 60)

figData <- bind_rows(list(Poisson = poisData, Heteroscedastic = heteroData),
                     .id="dataset")

predPois <- glm(observedResponse ~ Environment1, data=poisData,
                family=poisson()) |>
  ggpredict(terms="Environment1") |> as.data.frame()
predHetero <- glmmTMB(observedResponse ~ Environment1, 
                      dispformula = ~Environment1,
                      data=heteroData,family=nbinom2()) |>
  ggpredict(terms="Environment1") |> 
  as.data.frame()
preds <- bind_rows(list(Poisson = predPois, Heteroscedastic = predHetero),
                   .id="dataset")

ggplot(figData, aes(x=Environment1, y=observedResponse, col=dataset))+
  geom_point(alpha=0.4, size=4) +
  geom_smooth(data=poisData,aes(x=Environment1, y=observedResponse),
              inherit.aes = F,
              method="glm", method.args = list(family="poisson"),
              size = 4, se=F,
              color="#919c4c") +
  xlab("Enviroment") + ylab("Abundance") +
  scale_color_pomological()+
  theme_pomological()+
  theme(axis.text = element_text(size=20),
       # axis.text.x = element_blank(),
        axis.title = element_text(size=20),
        panel.background = element_rect(linewidth = 3, fill = NA),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        legend.position = "inside",
        legend.position.inside = c(0.3,0.8)) -> test

ggsave(test,filename="images/plot_heteroscedasticity.png",
       width=6, heigh=6)
#paint_pomological(test, res = 110, width=600, height=600) %>%
#  magick::image_write("images/plot_heteroscedasticity.png")


## REAL OVERDISPERSION ------
set.seed(5)
poisData <- createData(100, intercept = 1, 
                       family=poisson(),
                       randomEffectVariance = 0, )
set.seed(5) 
heteroData = createData(sampleSize = 100, intercept = 1,  
                        overdispersion = 0.5, 
                        family = poisson(), randomEffectVariance = 0)


figData <- bind_rows(list(Poisson = poisData, `Extra Poisson` = heteroData),
                     .id="dataset")

predPois <- glm(observedResponse ~ Environment1, data=poisData,
                family=poisson()) |>
  ggpredict(terms="Environment1") |> as.data.frame()
predHetero <- glmmTMB(observedResponse ~ Environment1, 
                      data=heteroData,family=nbinom2()) |>
  ggpredict(terms="Environment1") |> 
  as.data.frame()
preds <- bind_rows(list(Poisson = predPois, `Extra Poisson` = predHetero),
                   .id="dataset")

ggplot(figData, aes(x=Environment1, y=observedResponse, col=dataset))+
  geom_point(alpha=0.4, size=4) +
  geom_smooth(data=poisData, aes(x=Environment1, y=observedResponse),
              inherit.aes = F,
              method="glm", method.args = list(family="poisson"),
              size = 4, se=F,
              color="#919c4c") +
  xlab("Enviroment") + ylab("Abundance") +
  scale_color_pomological() +
  theme_pomological() +
  theme(axis.text= element_text(size=20),
        axis.title = element_text(size=20),
        panel.background = element_rect(linewidth = 3, fill = NA),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        legend.position = "inside",
        legend.position.inside = c(0.3,0.8)) -> test

ggsave(test,filename="images/plot_overdispersion.png",
       width=6, heigh=6)
# paint_pomological(test, res = 110, width=600, height=600) %>%
#   magick::image_write("images/plot_overdispersion.png")


## ZERO-INFLATION -----
set.seed(5)
poisData <- createData(100, intercept = 1, 
                       family=poisson(),
                       randomEffectVariance = 0, )
set.seed(5) 
heteroData = createData(sampleSize = 100, intercept = 1,  
                        pZeroInflation = 0.2,
                        family = poisson(), randomEffectVariance = 0)


figData <- bind_rows(list(Poisson = poisData, Zero_Inflation = heteroData),
                     .id="dataset")

predPois <- glm(observedResponse ~ Environment1, data=poisData,
                family=poisson()) |>
  ggpredict(terms="Environment1") |> as.data.frame()
predHetero <- glmmTMB(observedResponse ~ Environment1, 
                      ziformula = ~1,
                      data=heteroData,family=poisson()) |>
  ggpredict(terms="Environment1") |> 
  as.data.frame()
preds <- bind_rows(list(Poisson = predPois, Zero_Inflation = predHetero),
                   .id="dataset")

figData$dataset <- fct_relevel(figData$dataset, "Zero_Inflation", "Poisson")

ggplot(figData, aes(x=Environment1, y=observedResponse, col=dataset))+
  geom_point(alpha=0.4, size=4) +
  geom_smooth(data=poisData, aes(x=Environment1, y=observedResponse),
              inherit.aes = F,
              method="glm", method.args = list(family="poisson"),
              size = 4, se=F,
              color="#919c4c") +
  xlab("Enviroment") + ylab("Abundance") +
  #geom_point(data=heteroData|>filter(observedResponse==0),size = 4,alpha=0.7,
  #           color="#c03728")+
  scale_color_pomological() +
  theme_pomological() +
  theme(axis.text= element_text(size=20),
        axis.title = element_text(size=20),
        panel.background = element_rect(linewidth = 3, fill = NA),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        legend.position = "inside",
        legend.position.inside = c(0.3,0.8)) -> test

ggsave(test,filename="images/plot_zeroinflation.png",
       width=6, heigh=6)
# paint_pomological(test, res = 110, width=600, height=600) %>%
#   magick::image_write("images/plot_zeroinflation.png")





## CONSEQUENCES FIGURE ----

set.seed(5) 
heteroData = createData(sampleSize = 100, intercept = 1,  
                        overdispersion = 1, 
                        family = poisson(), randomEffectVariance = 0)
#heteroData <- heteroData |> filter(observedResponse < 60)

predPois <- glm(observedResponse ~ Environment1, data=heteroData,
                family=poisson()) |>
  ggpredict(terms="Environment1") |> as.data.frame()
predHetero <- MASS::glm.nb(observedResponse ~ Environment1,
                      data=heteroData) |>
  ggpredict(terms="Environment1") |> 
  as.data.frame()
preds <- bind_rows(list(Poisson = predPois, `negative binomial` = predHetero),
                   .id="model")
#"#c03728" "#919c4c"


ggplot(heteroData, aes(x=Environment1, y=observedResponse))+
  geom_point(alpha=0.1, size=4) + 
  geom_line(data=preds, aes(x=x, y=predicted, col=model), size=2)+
  geom_ribbon(data=preds, aes(x=x, y=predicted, fill=model, ymin=conf.low,
                              ymax=conf.high), col=NA, alpha=0.3) +
  xlab("Enviroment") + ylab("Abundance") +
  scale_color_manual(values =c("#008B00", "#f52300"))+
  scale_fill_manual(values =c("#008B00", "#f52300"))+
  theme_pomological_plain()+
  ylim(0,22)+
  ggtitle("Overdispersed data")+
  theme(axis.text = element_text(size=20),
        # axis.text.x = element_blank(),
        title = element_text(size=25),
        axis.title = element_text(size=20),
        panel.background = element_rect(linewidth = 3, fill = NA),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        legend.position = "inside",
        legend.position.inside = c(0.3,0.8)) -> test

ggsave(test,filename="images/plot_consequence_IC.png",
       width=6, heigh=6)
#paint_pomological(test, res = 110, width=600, height=600) %>%
#  magick::image_write("images/plot_heteroscedasticity.png")


