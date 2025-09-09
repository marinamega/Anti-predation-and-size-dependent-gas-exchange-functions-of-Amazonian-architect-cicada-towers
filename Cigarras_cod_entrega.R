library(readr)
library(ggplot2)
library(lme4)
library(DHARMa)
library(visreg)
library(emmeans)
library(scales)
library(dplyr)

setwd("C:/Users/marin/Documents/Serrapilheira/Curso de Campo/Amazonia")

#Dados do experimento de predação

pred=read.table("Pred_ofc.csv", header = T, sep = ";")
str(pred)

#Agrupando predação e definindo categorias

pred$for_ocor <- pmax(pred$for_ocor_t30, pred$for_ocor_t1h, pred$for_ocor_t3h)
pred$categoria <- ifelse(pred$altura == 0, "solo", "torre")
pred$categoria <- as.factor(pred$categoria)

#Ocorrência de formigas em função da altura

pred1 <- pred[pred$altura > 0,]
summary(pred1)

ggplot(pred1, aes(x = altura, y = for_ocor)) +
  geom_point(size=3) + labs(x = "Altura", y = "Probabilidade de ocorrência de formigas") +
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) + theme_classic() +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title = element_text(size=14))

#Testando relações

teste_pred_subs <- glmer(for_ocor ~ categoria + (1|mancha), family=binomial, data=pred)
summary(teste_pred_subs)

visreg(teste_pred_subs, rug = F, scale = "response", ylim=c(0,1), bty = "l",
        xlab = "Substrato", ylab = "Probabilidade de ocorrência de formigas", cex.lab = 1.3, cex.axis = 1.2)
points(jitter(ifelse(pred$categoria=="solo", 0.25, 0.75)), pred$for_ocor, cex = 1.5, pch = 16)

teste_pred_alt <- glmer(for_ocor ~ altura + (1|mancha), family=binomial, data=pred1)
summary(teste_pred_alt)

visreg(teste_pred_alt, rug = F, scale = "response", ylim=c(0,1), bty = "l",
       xlab = "Altura da torre (cm)", ylab = "Probabilidade de ocorrência de formigas", cex.lab = 1.3, cex.axis = 1.2)
points(pred$altura, pred$for_ocor, cex = 1.5, pch = 16)

#Dados de irrigação/vedação

ir_ved=read.table("irrigacao_vedacao.csv", header = T, sep = ";", fileEncoding = "latin1")
str(ir_ved)

col_obs <- which(names(ir_ved) == "obs")

ir_ved <- ir_ved[, 1:col_obs]

ir_ved$tratamento[ir_ved$tratamento == "água"] <- "encharcamento"
ir_ved$tratamento[ir_ved$tratamento == "preservativo"] <- "vedação"
ir_ved$tratamento <- as.factor(ir_ved$tratamento)
ir_ved$tratamento <- relevel(ir_ved$tratamento, ref="controle")

ir = ir_ved[ir_ved$tratamento == "encharcamento",]
ved = ir_ved[ir_ved$tratamento == "vedação",]
con = ir_ved[ir_ved$tratamento == "controle",]

#Separando tratamentos

ir_con <- ir_ved[ir_ved$tratamento != "vedação",]
teste_inund <- glm(altura_rec ~ altura*tratamento, data=ir_con)
summary(teste_inund)
anova(teste_inund)

cores <- c("#21918c", "#fb9b06")
visreg(teste_inund, "altura", by="tratamento", over=TRUE, bty = "l",
       xlab = "Altura original (cm)", ylab = "Taxa de crescimento (cm/noite)", cex.lab = 1.3, cex.axis = 1.2,
       points=list(cex=1.2, col = cores), line=list(col = cores), fill=list(col = alpha(cores, 0.2)), ylim=c(-2,8))

ved_con <- ir_ved %>% 
  filter(tratamento %in% c('controle', 'vedação'))#,
         #!altura == 47)

teste_ved <- glm(altura_rec ~ altura*tratamento, data=ved_con)
summary(teste_ved)
anova(teste_ved)

cores <- c("#21918c", "#320a5e")
visreg(teste_ved, "altura", by="tratamento", over=TRUE, bty = "l",
       xlab = "Altura original (cm)", ylab = "Altura reconstruída (cm)", cex.lab = 1.3, cex.axis = 1.2,
       points=list(cex=1.2, col = cores), line=list(col = cores), fill=list(col = alpha(cores, 0.2)), ylim=c(-2,8))

coef_mod <- coef(teste_ved)

b2 <- coef_mod["tratamentovedação"]
b3 <- coef_mod["altura:tratamentovedação"]

x_cross <- -b2 / b3
x_cross




# Observações usadas no modelo
obs_usadas <- model.frame(glm(formula = altura_rec ~ altura * tratamento, data = ved_con))

# Observações removidas (com pelo menos um NA nas variáveis do modelo)
obs_removidas <- ved_con[!rownames(ved_con) %in% rownames(obs_usadas), ]

# Visualiza as linhas excluídas
obs_removidas