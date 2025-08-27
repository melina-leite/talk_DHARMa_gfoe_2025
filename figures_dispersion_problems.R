#devtools::install_github("gadenbuie/ggpomological")


library(ggpomological)
library(magick)

## HETEROSCEDASTICITY
set.seed(5)
poisData <- createData(200, intercept = 1, 
                       family=poisson(),
                       randomEffectVariance = 0, )
set.seed(5) 
heteroData = createData(sampleSize = 200, intercept = 1,  
                        overdispersion = function(x){
                          return(rnorm(length(x), sd = 0.45 * abs(x)))}, 
                        family = poisson(), randomEffectVariance = 0)
heteroData <- heteroData |> filter(observedResponse < 60)

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

preds$arrow.up <- preds$predicted+preds$predicted
preds$arrow.down <- c(0,0,1,1,4,0,0,0,1,2)


ggplot(figData, aes(x=Environment1, y=observedResponse, col=dataset))+
  geom_point(alpha=0.3, size=3) +
  geom_smooth(data=poisData,aes(x=Environment1, y=observedResponse),
              inherit.aes = F,
              method="glm", method.args = list(family="poisson"),
              size=2, se=F,
              color="#919c4c") +
  # geom_linerange(data=preds, aes(x=x, y=predicted, ymin=arrow.down, 
  #                                ymax=arrow.up, col=dataset), inherit.aes = F,
  #                position = position_dodge(width = 0.05),
  #                linewidth=2)+
  xlab("Enviroment") + ylab("Abundance") +
  scale_color_pomological()+
  theme_pomological()+
  theme(axis.text = element_text(size=15),
        axis.text.x = element_blank(),
        axis.title = element_text(size=15),
        panel.background = element_rect(linewidth = 2, fill = NA),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        legend.position = "inside",
        legend.position.inside = c(0.3,0.8)) -> test

ggsave(test,filename="images/plot_heteroscedasticity2.png",
       width=6, heigh=6)
paint_pomological(test, res = 110, width=600, height=600) %>%
  magick::image_write("images/plot_heteroscedasticity.png")


## REAL OVERDISPERSION
set.seed(5)
poisData <- createData(200, intercept = 1, 
                       family=poisson(),
                       randomEffectVariance = 0, )
set.seed(5) 
heteroData = createData(sampleSize = 200, intercept = 1,  
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

preds$arrow.up <- preds$predicted+preds$predicted
preds$arrow.down <- c(0,0,1,1,4,0,0,0,1,1)


ggplot(figData, aes(x=Environment1, y=observedResponse, col=dataset))+
  geom_point(alpha=0.3, size=3) +
  geom_smooth(data=poisData, aes(x=Environment1, y=observedResponse),
              inherit.aes = F,
              method="glm", method.args = list(family="poisson"),
              size=2, se=F,
              color="#919c4c") +
  # geom_linerange(data=preds, aes(x=x, y=predicted, ymin=arrow.down, 
  #                                ymax=arrow.up, col=dataset), inherit.aes = F,
  #                position = position_dodge(width = 0.05),
  #                linewidth=2) +
  xlab("Enviroment") + ylab("Abundance") +
  scale_color_pomological() +
  theme_pomological() +
  theme(axis.text= element_text(size=15),
        axis.title = element_text(size=15),
        panel.background = element_rect(linewidth = 2, fill = NA),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        legend.position = "inside",
        legend.position.inside = c(0.3,0.8)) -> test

ggsave(test,filename="images/plot_overdispersion2.png",
       width=6, heigh=6)
paint_pomological(test, res = 110, width=600, height=600) %>%
  magick::image_write("images/plot_overdispersion.png")


## REAL OVERDISPERSION
set.seed(5)
poisData <- createData(200, intercept = 3, 
                       family=poisson(),
                       randomEffectVariance = 0, )
set.seed(5) 
heteroData = createData(sampleSize = 200, intercept = 3,  
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

preds$arrow.up <- preds$predicted+preds$predicted
preds$arrow.down <- c(0,0,1,1,4,0,0,0,1,1)

figData$dataset <- fct_relevel(figData$dataset, "Zero_Inflation", "Poisson")

ggplot(figData, aes(x=Environment1, y=observedResponse, col=dataset))+
  geom_point(alpha=0.3, size=3) +
  geom_smooth(data=poisData, aes(x=Environment1, y=observedResponse),
              inherit.aes = F,
              method="glm", method.args = list(family="poisson"),
              size=2, se=F,
              color="#919c4c") +
  xlab("Enviroment") + ylab("Abundance") +
  geom_point(data=heteroData|>filter(observedResponse==0),size=3,alpha=0.7,
             color="#c03728")+
  scale_color_pomological() +
  theme_pomological() +
  theme(axis.text= element_text(size=15),
        axis.title = element_text(size=15),
        panel.background = element_rect(linewidth = 2, fill = NA),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        legend.position = "inside",
        legend.position.inside = c(0.3,0.8)) -> test

ggsave(test,filename="images/plot_zeroinflation2.png",
       width=6, heigh=6)
paint_pomological(test, res = 110, width=600, height=600) %>%
  magick::image_write("images/plot_zeroinflation.png")



