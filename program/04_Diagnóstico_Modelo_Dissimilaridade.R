#### pegando resultados

arquivos_modelo <- list.files("data/result","T_.*_full.rds",full.names = T)
base_modelos_treino <- rbindlist(lapply(arquivos_modelo,readRDS))

resultados_modelos <- base_modelos_treino %>%
  group_by(k,alpha) %>%
  summarise(acurracia_out = mean(acurracia_out))
resultados_modelos <- data.table(resultados_modelos)
resultados_modelos <- resultados_modelos[acurracia_out==max(acurracia_out),]

# SAMPLE
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



## base

xrt  <- cbind("Int"=1,base %>% filter(season %in% s_t) %>% select(varsmodelo)) %>%
  select(parm_matrix$par_x)
## colocando na mesma ordem dos parâmetros
xrt <- data.matrix(xrt)

rownames(parm_matrix) <- parm_matrix$par_x
parm_matrix <- parm_matrix %>% select(-par_x)
parm_matrix <- data.matrix(parm_matrix)

ahat_valida <- xrt %*% parm_matrix

ahat_valida[,2] <- (cbind(ahat_valida[,1],gama*exp(-sigma*(ahat_valida[,-1])^2)) %*% parm_matrix)[,2]
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

