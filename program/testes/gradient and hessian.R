##gradient and hessian

base %>%
  group_by(resultado) %>% filter(season==temporada) %>%
  summarise(valor = mean(`Passe curto`))

varsmodel <- c("Passe curto")
varsmodely <- c("VA","Emp","VB")
xr  <- data.matrix(cbind(1,base %>% filter(season==temporada) %>% select(varsmodelo)))
yr <-  data.matrix(cbind(as.numeric(base %>% filter(season==temporada) %>%
                                      select(resultado) == "VA"),
                         as.numeric(base %>% filter(season==temporada) %>%
                                      select(resultado) == "Emp"),
                         as.numeric(base %>% filter(season==temporada) %>%
                                      select(resultado) == "VB")))
yrt <- DR_data(yr)
dxr <- data.frame(xr)
paste0(varsmodelo,collapse = "+")
paste0(names(dxr),collapse = "`+`")
res1_0 <- DirichReg(yrt ~ `Aceleração`+`Altura`+`Cabeceio`+`Carrinho`+`Ch..de.longe`+
                      `Cobr..falta`+`Combativ.`+`Contr..bola`+`Cruzamento`+`Div..em.pé`+
                      `Dribles`+`Duração.Do.Contrato`+`Elast..GL`+`Finalização`+`Fôlego`+
                      `Força`+`Força.chute`+`Idade`+`Lançamento`+`Manejo`+`Marcação`+`Passe.curto`+
                      `Perna.boa`+`Peso`+`Pique`+`Posicion..GL`+`Reação`+`Reflexos`+
                      `ultimo_result_va`+`ultimo_result_vb`,data = dxr,model = "common")

ccsa <- cor(dxr) 

matrix(res1_0$coefficients,nrow=ncol(yr),byrow = T)
res1_0$se
diag((-solve(res1_0$hessian))^.5)
diag((-solve(M))^.5)
diag((-solve(Mt))^.5)
res1_0$coefficients
res1_0$X
k <- 0
xr <- xr[,1]
## modelo proposto
res4 <- maxLik(logDirichregregt,start = rep(.1,ncol(xr)*3),
               method = "SANN")

res4$estimate



M <- res4$hessian
Mt <- M[c(1,4,2,5,3,6),c(1,4,2,5,3,6)]
ncol(xr)
