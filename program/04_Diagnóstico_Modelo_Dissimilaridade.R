#### pegando resultados
rm(list = ls())
library(dplyr)
library(data.table)
library(tidyr)
library(plotly)
library(xtable)
arquivos_modelo <- list.files("data/result/tuning_f/","T_.*_dissimilaridade.rds",full.names = T)
base_modelos_treino <- rbindlist(lapply(arquivos_modelo,readRDS))

resultados_modelos <- base_modelos_treino %>%
  group_by(k,alpha) %>%
  summarise(acurracia_out = mean(acurracia_out))
resultados_modelos <- data.table(resultados_modelos)


kd <- resultados_modelos %>% spread(value = acurracia_out,key =alpha)
xnames <- as.numeric(as.character(as.vector(kd[-1,1]$k)))
ynames <- as.numeric(as.character(as.numeric(names(kd[-1,-1]))))
kd <- as.matrix(kd[-1,-1])


kd <- list(x = xnames,
           y = ynames,
           z = kd)

p <- plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% add_surface()


pdf(file = "report/images/contour_dissimilaridade.pdf")
image(kd$x, kd$y, kd$z,
      xlab = expression(lambda),
      ylab = expression(phi))
contour(kd$x, kd$y, kd$z, col = "pink", add = TRUE, method = "edge",
        vfont = c("sans serif", "plain"))
dev.off()


# SAMPLE
resultados_modelos <- resultados_modelos[acurracia_out==max(acurracia_out),]

set.seed(2609)
ind_mod <- sample(nrow(resultados_modelos),1)
resultados_modelos <- resultados_modelos[ind_mod,]

resultados_modelos <- resultados_modelos %>%
  left_join(base_modelos_treino,by = c("k","alpha"))

resultados_modelos <- resultados_modelos %>%
  mutate(par_x = gsub(".*_(.*)","\\1",nome_theta_est),
         par_y = gsub("(.*)_.*","\\1",nome_theta_est))

resultados_modelos_pars <-  resultados_modelos %>%
  select(par_x,par_y,theta_est)

parm_matrix <- spread(resultados_modelos_pars, par_y, theta_est)


s <- 2014:2008
s_out <- 2015
s_t <- 2016


varsmodelo <- setdiff(parm_matrix$par_x,"Int")

## base
base <- readRDS("data/result/base_modelo_dirichilet_score.rds")
base <- data.table(base)
base$resultado <- base$score_casa-base$score_fora
xrt  <- cbind("Int"=1,base %>% filter(season %in% s_t) %>% select(varsmodelo)) %>%
  select(parm_matrix$par_x)
## colocando na mesma ordem dos parâmetros
xrt <- data.matrix(xrt)

rownames(parm_matrix) <- parm_matrix$par_x
parm_matrix <- parm_matrix %>% select(-par_x)
parm_matrix <- data.matrix(parm_matrix)

ahat_valida <- xrt %*% parm_matrix
yrt_est <- exp(ahat_valida)
yrt_est <- factor(apply(yrt_est,1,which.max),levels = 1:ncol(yrt_est))

yrt <-  data.matrix(cbind(as.numeric(base %>% filter(season%in% s_t) %>%
                                       select(resultado) ==0),
                          as.numeric(base %>% filter(season%in% s_t) %>%
                                       select(resultado) > 0),
                          as.numeric(base %>% filter(season%in% s_t) %>%
                                       select(resultado) < 0)))

yrt <- factor(apply(yrt,1,which.max),levels = 1:ncol(yrt))
cross_t <- table(yrt,yrt_est)
colnames(cross_t)<- rownames(cross_t) <- c("Emp","VA","VB")

acurracia_t <- sum(diag(cross_t)) / sum(cross_t)
acurracias_t <- data.table(t(diag(cross_t/apply(cross_t,1,sum))))
setnames(acurracias_t,names(acurracias_t),
         paste0(c("Emp","VA","VB"),"_acuracia_t"))
cross_tf <- data.frame(cross_t)


## base de dados resultado print

base_print_in <- unique(resultados_modelos %>%
  select(acurracia,VA_acuracia,Emp_acuracia,VB_acuracia))
setnames(base_print_in,
         names(base_print_in),c("Global","Home","Tie","Visitor"))
base_print_tuning <- unique(resultados_modelos %>%
  select(acurracia_out.x,VA_acuracia_out,Emp_acuracia_out,VB_acuracia_out))
setnames(base_print_tuning,
         names(base_print_tuning),c("Global","Home","Tie","Visitor"))
base_print_teste <- cbind(acurracia_t,acurracias_t %>%
              select(VA_acuracia_t,Emp_acuracia_t,VB_acuracia_t))
setnames(base_print_teste,
         names(base_print_teste),
         c("Global","Home","Tie","Visitor"))
base_print <- rbind(base_print_in,
          base_print_tuning,
          base_print_teste)

base_print <- base_print*100

Seasons <- c("2008-2014","2015","2016")
base_print <- cbind(Seasons = Seasons,base_print)

print(xtable(base_print),include.rownames = F)
