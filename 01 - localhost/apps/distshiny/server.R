library(shiny)

shinyServer(function(input, output, session) {
  # Variável auxiliar
  aux <- reactiveValues()
  
  # Normal ---------------------------------------------------------------------
  # Calcula z1 e z2 padronizado e retorna a probabilidade entre eles
  rn <- reactive({normal(input$mu,input$sigma,input$x1,input$x2)})
  
  # Valor reativo para média da normal
  mun <- reactive({input$mu})
  
  observe({
    # Este 'se' so serve para a primeira iteração
    if(is.null(aux$mu)) {aux$mu <- mun()}
    
    # Caso haja uma alteração de input$mu, as arrow's mudam igualmente de -3 a 3.
    if(aux$mu != input$mu) {
      updateNumericInput(session,"x1",label = "",value = input$mu - 3 * input$sigma)
      updateNumericInput(session,"x2",label = "",value = input$mu + 3 * input$sigma)
      aux$mu <- input$mu 
    }
  })
  
  
  # Exponencial ----------------------------------------------------------------
  rexp <- reactive({exponencial(input$lambda,input$x1exp,input$x2exp)})
  
  
  # Uniforme Continua ----------------------------------------------------------
  runifcont <- reactive({uniforme_cont(input$a_unif_cont,input$b_unif_cont,
                                       input$x1unif_cont,input$x2unif_cont)})
  comp_unif_cont <- reactive({length(input$a_unif_cont:input$b_unif_cont)-1})
  comp_unif_cont2 <- reactive({ input$x2unif_cont - input$x1unif_cont})
  # Beta -----------------------------------------------------------------------
  rbeta <- reactive({beta(input$a_beta,input$b_beta,input$x1beta,input$x2beta)})
  
  
  # Gamma ----------------------------------------------------------------------
  rgamma <- reactive({gamma(a = input$a_gamma,input$b_gamma,input$x1gamma,
                            input$x2gamma)})
  # Uniforme Discreta ----------------------------------------------------------
  runifdis <- reactive({uniforme_dis(input$k_unif_dis,input$x1unif_dis,
                                     input$x2unif_dis)})
  comp_unif_disc <- reactive({length(input$x1unif_dis:input$x2unif_dis)})
  # Binomial  ------------------------------------------------------------------
  rbinomial <- reactive({binomial(input$n_binomial,input$p_binomial,
                                  input$x1binom,input$x2binom)})
  near_binomial <- reactive({input$x1binom + 1})
  # Poisson --------------------------------------------------------------------
  rpoisson <- reactive({poisson(input$lambda2,input$x1pois,input$x2pois)})
  near_poisson <- reactive({input$x1pois + 1})
  # Geometrico -----------------------------------------------------------------
  rgeom <- reactive({geometrico(input$pgeom,input$x1geom,input$x2geom)})
  near_geom <- reactive({input$x1geom  + 1})
  # Hipergeométrica ------------------------------------------------------------
  rhipergeom <- reactive({hipergeometrica(input$mhiper,input$nhiper,input$khiper,
                                          input$x1hiper,input$x2hiper)})
  N_total_hiper <- reactive({input$nhiper + input$mhiper})
  near_hiper <- reactive({input$x1hiper + 1})
  # Binomial Negativa ----------------------------------------------------------
  rbinomialneg <- reactive({binomialneg(input$kbn,input$pbn,input$x1bn,input$x2bn)})
  near_bn <- reactive({input$x1bn + 1})
    
  
  
  # Descrição ------------------------------------------------------------------
  observeEvent(input$sobre,{
    switch (input$dist,
            # Normal -----------------------------------------------------------
            normal = showModal(modalDialog(
              title = "Modelo Normal",
              "Função densidade de probabilidade:",
              withMathJax(helpText("$$f(x)=\\frac{1}{\\sigma\\sqrt{2\\pi}} e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}, x \\ \\ \\epsilon  \\ \\ (-\\infty,\\infty)$$")),
              "Propriedades:",
              div("i) f(x) é simétrica em relação à ",HTML("&mu;"),style = "margin-left: 10%;"),
              div("ii) f(x) -> 0 quando x", HTML("&rightarrow; &pm;  &infin;"),style = "margin-left: 10%;"),
              div("iii) o valor máximo de f(x) se dá para x = ",HTML("&mu;"),style = "margin-left: 10%;"),
              "Esperança e Variância",
              withMathJax(helpText("$$E(X) = \\mu$$")),
              withMathJax(helpText("$$Var(X) = \\sigma^2$$")),
              "Representação:",
              withMathJax(helpText("$$X \\sim N(\\mu,\\sigma^2)$$")),
              easyClose = TRUE,
              footer = ""
              )),
            # Exponencial ------------------------------------------------------
            exponencial = showModal(modalDialog(
              title = "Modelo Exponencial",
              "Função densidade de probabilidade:",
              withMathJax(helpText("$$f(x) = \\left\\{\\begin{matrix} \\lambda e^{-\\lambda x} &,x \\geq 0 \\\\ 0 &, \\text{caso contrário} \\end{matrix}\\right.$$")),
              "Esperança e Variância",
              withMathJax(helpText("$$E(X) = \\mu = \\frac{1}{\\lambda}$$")),
              withMathJax(helpText("$$Var(X) = \\sigma^2 = \\frac{1}{\\lambda^2}$$")),
              "Representação:",
              withMathJax(helpText("$$X \\sim Exp(\\lambda)$$")),
              easyClose = TRUE,
              footer = ""
            )),
            # Uniforme Contínua ------------------------------------------------
            uniformecont = showModal(modalDialog(
              title = "Modelo Uniforme Contínua",
              "Função densidade de probabilidade:",
              withMathJax(helpText("$$f(x) = \\left\\{\\begin{matrix} \\frac{1}{b-a} &, a \\leq x \\leq b \\\\ 0 &, \\text{caso contrário} \\end{matrix}\\right.$$")),
              "Esperança e Variância",
              withMathJax(helpText("$$E(X) = \\mu = \\frac{a + b}{2}$$")),
              withMathJax(helpText("$$Var(X) = \\sigma^2 = \\frac{(b-a)^2}{12}$$")),
              "Representação:",
              withMathJax(helpText("$$X \\sim U(a,b)$$")),
              easyClose = TRUE,
              footer = ""
            )),
            # Uniforme Discreta ------------------------------------------------
            uniformedis = showModal(modalDialog(
              title = "Modelo Uniforme Discreto",
              "Função distribuição de probabilidade:",
              withMathJax(helpText("$$P(X = j) = \\frac{1}{k}, \\ \\ j \\ \\ = \\ \\ 1,2,3,...,k$$")),
              "Esperança",
              withMathJax(helpText("$$E(X) = \\mu = \\frac{1}{k}$$")),
              "Representação:",
              withMathJax(helpText("$$X \\sim U_D(1,k)$$")),
              easyClose = TRUE,
              footer = ""
            )),
            # Binomial  --------------------------------------------------------
            binomial = showModal(modalDialog(
              title = "Modelo Binomial",
              "Função distribuição de probabilidade:",
              withMathJax(helpText("$$P(X = k) = \\binom{n}{k}p^k(1-p)^{n-k}, \\ \\ k \\ \\ = \\ \\ 0,1,2,3,...,n$$")),
              "Sendo que: ",
              withMathJax(helpText("$$\\binom{n}{k} = \\binom{n!}{k!(n-k)!}$$")),
              "Esperança e Variância",
              withMathJax(helpText("$$E(X) = \\mu = np$$")),
              withMathJax(helpText("$$Var(X) = \\sigma^2 = npq$$")),
              "Representação:",
              withMathJax(helpText("$$X \\sim Bin(n,p)$$")),
              easyClose = TRUE,
              footer = ""
            )),
            # Poisson ----------------------------------------------------------
            poisson = showModal(modalDialog(
              title = "Modelo Poisson",
              "Função distribuição de probabilidade:",
              withMathJax(helpText("$$P(X = k) = \\frac{e^{-\\lambda} \\lambda^k}{k!}, \\ \\ k  \\ \\ = \\ \\ 0,1,2,3,...$$")),
              "Esperança e Variância",
              withMathJax(helpText("$$E(X) = \\mu = \\lambda$$")),
              withMathJax(helpText("$$Var(X) = \\sigma^2 = \\lambda$$")),
              "Representação:",
              withMathJax(helpText("$$X \\sim Poi(\\lambda)$$")),
              easyClose = TRUE,
              footer = ""
            )),
            # Geométrico -------------------------------------------------------
            geometrico = showModal(modalDialog(
              title = "Modelo Geométrico",
              "Função distribuição de probabilidade:",
              withMathJax(helpText("$$P(X = k) = p(1-p)^k, \\ 0 \\leq p \\leq 1, \\ k  \\ = \\ 0,1,2,3 ....$$")),
              "Esperança e Variância",
              withMathJax(helpText("$$E(X) = \\mu = \\frac{1-p}{p}$$")),
              withMathJax(helpText("$$Var(X) = \\sigma^2 = \\frac{1-p}{p^2}$$")),
              "Representação:",
              withMathJax(helpText("$$X \\sim G(p)$$")),
              easyClose = TRUE,
              footer = ""
            )),
            # Hipergeometrico --------------------------------------------------
            hipergeometrica = showModal(modalDialog(
              title = "Modelo Hipergeométrico",
              "Função distribuição de probabilidade:",
              withMathJax(helpText("$$P(X = k) = \\frac{\\binom{m}{k} \\binom{n-m}{r-k}}{\\binom{n}{r}}, \\ k = max(0,r-(n-m)),...,min(r,m)$$")),
              "Esperança e Variância",
              withMathJax(helpText("$$E(X) = \\mu = \\frac{rm}{n}$$")),
              withMathJax(helpText("$$Var(X) = \\sigma^2 = \\frac{rm}{n} \\frac{(n-m)(n-r)}{n(n-1)}$$")),
              "Representação:",
              withMathJax(helpText("$$X \\sim Hiper(m,n,k)$$")),
              easyClose = TRUE,
              footer = ""
            )),
            # Binomial Negativa ------------------------------------------------
            binomialneg = showModal(modalDialog(
              title = "Modelo Binomial Negativo",
              "Função distribuição de probabilidade:",
              withMathJax(helpText("$$P(X = k ) = \\binom{k + r -1}{r-1}p^r(1-p)^k, k \\ = \\ 0,1,...$$")),
              "Esperança e Variância",
              withMathJax(helpText("$$E(X) = \\mu = \\frac{r(1-p)}{p}$$")),
              withMathJax(helpText("$$Var(X) = \\sigma^2 = \\frac{r(1-p)}{p^2}$$")),
              "Representação:",
              withMathJax(helpText("$$X \\sim BN(r,p)$$")),
              easyClose = TRUE,
              footer = ""
            )),
            # Beta -------------------------------------------------------------
            beta = showModal(modalDialog(
              title = "Modelo Beta",
              "Função densidade de probabilidade:",
              withMathJax(helpText("$$f(x) = \\frac{\\Gamma(\\alpha + \\beta)}{\\Gamma(\\alpha) \\Gamma(\\beta)} x^{\\alpha-1} (1-x)^{\\beta-1}, \\ x \\ \\epsilon \\ (0,1) \\ e \\ \\alpha,\\beta \\ > 0$$")),
              HTML("Função Gamma (&Gamma;(x)):"),
              withMathJax(helpText("$$\\Gamma(x) = \\int_0^{\\infty} t^{x-1}e^{-t}dt$$")),
              "Esperança e Variância",
              withMathJax(helpText("$$E(X) = \\mu = \\frac{\\alpha}{\\alpha + \\beta}$$")),
              withMathJax(helpText("$$Var(X) = \\sigma^2 = \\frac{\\alpha \\beta}{(\\alpha + \\beta + 1)(\\alpha + \\beta)^2}$$")),
              "Representação:",
              withMathJax(helpText("$$X \\sim Beta(\\alpha,\\beta)$$")),
              easyClose = TRUE,
              footer = ""
            )),
            # Gamma ------------------------------------------------------------
            gamma = showModal(modalDialog(
              title = "Modelo Gamma",
              "Função densidade de probabilidade:",
              withMathJax(helpText("$$f(x) = \\left\\{\\begin{matrix} \\frac{\\beta^{\\alpha} x^{\\alpha -1} e^{-\\beta x}}{\\Gamma(\\alpha)},\\ \\ x \\geq 0 \\\\ 0 &, \\text{caso contrário} \\end{matrix}\\right.$$")),
              HTML("Função Gamma (&Gamma;(x)):"),
              withMathJax(helpText("$$\\Gamma(x) = \\int_0^{\\infty} t^{x-1}e^{-t}dt$$")),
              "Esperança e Variância",
              withMathJax(helpText("$$E(X) = \\mu = \\frac{\\alpha}{\\beta}$$")),
              withMathJax(helpText("$$Var(X) = \\sigma^2 = \\frac{\\alpha}{\\beta^2}$$")),
              "Representação:",
              withMathJax(helpText("$$X \\sim \\Gamma(\\alpha,\\beta)$$")),
              easyClose = TRUE,
              footer = ""
            ))
    )
           
  })
  
  
  
  
  # Gráfico das distribuições --------------------------------------------------
  output$distPlot <- renderPlot({
    validate(
      need(!(input$x1 >= input$x2 && input$dist == "normal"),
           "x1 não pode ser maior ou igual a x2."),
      need(!(input$x1exp >= input$x2exp && input$dist == "exponencial"),
           "x1 não pode ser maior que x2."),
      need(!(input$lambda <= 0 && input$dist == "exponencial"),
           "lambda não pode ser menor ou igual à 0"),
      need(!(input$x1unif_cont >= input$x2unif_cont && 
               input$dist == "uniformecont"),
           "x1 não pode ser maior que x2."),
      need(!(input$x1unif_cont < input$a_unif_cont && 
               input$dist == "uniformecont" ),
           "x1 não pode ser menor que a."),
      need(!(input$a_unif_cont > input$b_unif_cont &&
               input$dist == "uniformecont"),
           "a não pode ser maior ou igual a b"),
      need(!(input$x2unif_cont > input$b_unif_cont && 
               input$dist == "uniformecont"),
           "x2 não pode ser maior que b"),
      need(!(input$x1unif_dis > input$x2unif_dis && 
               input$dist == "uniformedis"),
           "x1 não pode ser maior que x2."),
      need(!(input$x2unif_dis > input$k_unif_dis && 
               input$dist == "uniformedis"),
           "x2 não pode ser maior que k"),
      need(!(input$x1binom > input$x2binom && 
               input$dist == "binomial"),
           "x1 não pode ser maior que x2."),
      need(!(input$p_binomial >= 1  && 
               input$dist == "binomial" ||
               input$p_binomial <=0),
           "p deve estar contido dentro de (0,1)"),
      need(!(input$x1pois > input$x2pois && 
               input$dist == "poisson"),
           "x1 não pode ser maior que x2."),
      need(!(input$lambda2 <= 0 && input$dist == "poisson"),
           "lambda não pode ser menor ou igual à 0"),
      need(!(input$x1geom > input$x2geom && 
               input$dist == "geometrico"),
           "x1 não pode ser maior que x2."),
      need(!(input$pgeom <= 0 && input$dist == "geometrico" ||
               input$pgeom >= 1),
           "p não pode ser menor ou igual à 0 ou maior ou igual à 1"),
      need(!(input$x1hiper > input$x2hiper && 
               input$dist == "hipergeometrica"),
           "x1 não pode ser maior que x2."),
      need(!(input$khiper > (input$mhiper + input$nhiper) && 
               input$dist == "hipergeometrica"),
           "k não pode ser maior que m + n."),
      need(!(input$x2hiper > input$mhiper && 
               input$dist == "hipergeometrica"),
           "x2 não pode ser maior que m."),
      need(!(input$x1bn > input$x2bn && 
               input$dist == "binomialneg"),
           "x1 não pode ser maior que x2."),
      need(!(input$pbn > 1 || input$pbn <= 0 && 
               input$dist == "binomialneg"),
           "p não pode ser maior que 1 ou menor igual que zero."),
      need(!(input$x1beta >= input$x2beta && 
               input$dist == "beta"),
           "x1 não pode ser maior que x2."),
      need(!(input$a_beta <= 0 || input$b_beta <= 0 && 
               input$dist == "beta"),
           "alpha e beta não podem ser menores que 0."),
      need(!(input$x1gamma >= input$x2gamma && 
               input$dist == "gamma"),
           "x1 não pode ser maior que x2."),
      need(!(input$x1gamma < 0 || input$x2gamma < 0 && 
               input$dist == "gamma"),
           "x1 e x2 devem ser maior ou igual a 0."),
      need(!(input$a_gamma <= 0 || input$b_gamma <= 0 && 
               input$dist == "gamma"),
           "alpha e beta não podem ser menores que 0.")
    )
    switch(input$dist,
           normal = rn()[[1]],
           exponencial = rexp()[[1]],
           uniformecont = runifcont()[[1]],
           uniformedis = runifdis()[[1]],
           binomial = rbinomial()[[1]],
           poisson = rpoisson()[[1]],
           geometrico = rgeom()[[1]],
           hipergeometrica = rhipergeom()[[1]],
           binomialneg = rbinomialneg()[[1]],
           beta = rbeta()[[1]],
           gamma = rgamma()[[1]]
      )
  })
  
  
  # Cálculo de probabilidade das distribuições
  output$formula <- renderUI({
    # Validação dos parâmetros -------------------------------------------------
    validate(
      need(!(input$x1 >= input$x2 && input$dist == "normal"),
           ""),
      need(!(input$x1exp >= input$x2exp && input$dist == "exponencial"),
           ""),
      need(!(input$lambda <= 0 && input$dist == "exponencial"),
           ""),
      need(!(input$x1unif_cont >= input$x2unif_cont && 
               input$dist == "uniformecont"),
           ""),
      need(!(input$x1unif_cont < input$a_unif_cont && 
               input$dist == "uniformecont" ),
           ""),
      need(!(input$a_unif_cont > input$b_unif_cont &&
               input$dist == "uniformecont"),
           ""),
      need(!(input$x2unif_cont > input$b_unif_cont && 
               input$dist == "uniformecont"),
           ""),
      need(!(input$x1unif_dis > input$x2unif_dis && 
               input$dist == "uniformedis"),
           ""),
      need(!(input$x2unif_dis > input$k_unif_dis && 
               input$dist == "uniformedis"),
           ""),
      need(!(input$x1binom > input$x2binom && 
               input$dist == "binomial"),
           ""),
      need(!(input$p_binomial >= 1  && 
               input$dist == "binomial" ||
               input$p_binomial <=0),
           ""),
      need(!(input$x1pois > input$x2pois && 
               input$dist == "poisson"),
           ""),
      need(!(input$lambda2 <= 0 && input$dist == "poisson"),
           ""),
      need(!(input$x1geom > input$x2geom && 
               input$dist == "geometrico"),
           ""),
      need(!(input$pgeom <= 0 && input$dist == "geometrico" ||
               input$pgeom >= 1),
           ""),
      need(!(input$x1hiper > input$x2hiper && 
               input$dist == "hipergeometrica"),
           ""),
      need(!(input$khiper > (input$mhiper + input$nhiper) && 
               input$dist == "hipergeometrica"),
           ""),
      need(!(input$x2hiper > input$mhiper && 
               input$dist == "hipergeometrica"),
           ""),
      need(!(input$x1bn > input$x2bn && 
               input$dist == "binomialneg"),
           ""),
      need(!(input$pbn > 1 || input$pbn <= 0 && 
               input$dist == "binomialneg"),
           "")
    )
    # Switch ~~ ----------------------------------------------------------------
    switch(input$dist,
           # Normal ------------------------------------------------------------
           normal = withMathJax(helpText(
             paste0("$$P(X_1 \\leq X \\leq X_2) = P(",input$x1,"\\leq X \\leq",
              input$x2,")= P(\\frac{X1 - \\mu}{\\sigma} \\leq \\frac{X - \\mu}",
                "{\\sigma} \\leq \\frac{X2 - \\mu}{\\sigma})= P(\\frac{",
                input$x1,"-",input$mu,"}{",input$sigma,"}\\leq Z \\leq \\frac{",
                input$x2,"-",input$mu,"}{",input$sigma,"}) \\\\ P(",rn()[[2]]),
                "\\leq Z \\leq",rn()[[3]],")=",rn()[[4]],"$$")),
           # Beta --------------------------------------------------------------
           beta = withMathJax(helpText(paste0("$$P(",input$x1beta,"\\leq X \\leq",input$x2beta,") = ",
                                              "\\frac{\\Gamma (",input$a_beta,"+",input$b_beta,")}{\\Gamma (",input$a_beta,") \\Gamma (",input$b_beta,")}",
                                              "\\int_{",input$x1beta,"}^{",input$x2beta,"} ",
                                              "x^{",input$a_beta,"-1}(1-x)^{",input$b_beta,"-1}dx =",rbeta()[[2]],"$$"))),
           # Gamma -------------------------------------------------------------
           gamma = withMathJax(helpText(paste0("$$P(",input$x1gamma,"\\leq X \\leq", input$x2gamma,") = ",
                                               "\\frac{",input$b_gamma,"^{",input$a_gamma,"}}{\\Gamma(",input$a_gamma,")}",
                                               "\\int_{",input$x1gamma,"}^{",input$x2gamma,"}x^{",input$a_gamma,"-1}e^{-",input$b_gamma,"x}dx",
                                               "= ", rgamma()[[2]],"$$"))),
           # Exponencial -------------------------------------------------------
           exponencial = withMathJax(helpText("$$P(X_1 \\leq X \\leq X_2) =",
              "P(",input$x1exp,"\\leq X \\leq",input$x2exp,")=",
              "\\int^{X_2}_{X_1}\\lambda e^{-\\lambda x}dx = \\int^{",input$x2exp,
              "}_{",input$x1exp,"}",input$lambda,"e^{-",input$lambda," x}dx =",
              "[-e^{-",input$lambda,"x}]^{",input$x2exp,"}_{",input$x1exp,"} =",
              "e^{-",input$x1exp," x} - e^{-",input$x2exp," x} \\\\ P(",
              input$x1exp,"\\leq X \\leq",input$x2exp,") = ",rexp()[[2]],"$$")),
           # Uniforme Continua -------------------------------------------------
           uniformecont = withMathJax(helpText(paste0("$$P(",input$x1unif_cont,"\\leq X \\leq",input$x2unif_cont,") =",
                                              "\\int_{",input$x1unif_cont,"}^{",input$x2unif_cont,"}\\frac{1}{",comp_unif_cont(),"}dx =",
                                              "\\frac{1}{",comp_unif_cont(),"}\\int_{",input$x1unif_cont,"}^{",input$x2unif_cont,"}dx = ",
                                              "\\frac{1}{",comp_unif_cont(),"}[x]^{",input$x2unif_cont,"-(",input$x1unif_cont,")} =",
                                              "\\frac{1}{",comp_unif_cont(),"}[",input$x2unif_cont,"-(",input$x1unif_cont,")]=",
                                              "\\frac{",comp_unif_cont2(),"}{",comp_unif_cont(),"}=", runifcont()[[2]],"$$"))),
                                              
           # Uniforme discreta -------------------------------------------------
           uniformedis = if(input$x1unif_dis == input$x2unif_dis) {
             withMathJax(helpText(paste0("$$P(X =",input$x2unif_dis,") =",
                                         "\\frac{1}{",input$k_unif_dis,"}=",
                                         runifdis()[[2]],"$$")))
           } else {
             withMathJax(helpText(paste0("$$P(",input$x1unif_dis,"\\leq X \\leq",
                                         input$x2unif_dis,") = ", comp_unif_disc(),
                                         "\\ast \\frac{1}{",
                                         input$k_unif_dis,"} =",runifdis()[[2]],"$$")))
           },
           # Binomial ----------------------------------------------------------
           binomial = if(input$x1binom == input$x2binom) {
             withMathJax(helpText("$$P(X = ",input$x2binom,") =\\binom{",
                                  input$n_binomial,"}{",input$x2binom,"}",
                                  input$p_binomial,"^{",input$x2binom,"}(1-",input$p_binomial,
                                  ")^{",input$n_binomial,"-",input$x2binom,"}=",rbinomial()[[2]],"$$"))
           } else if(near_binomial() == input$x2binom) {
             withMathJax(helpText("$$P(",input$x1binom,"\\leq X \\leq",input$x2binom,") =\\binom{",
                                  input$n_binomial,"}{",input$x1binom,"}",
                                  input$p_binomial,"^{",input$x1binom,"}(1-",input$p_binomial,
                                  ")^{",input$n_binomial,"-",input$x1binom,"} + \\binom{",
                                  input$n_binomial,"}{",input$x2binom,"}",
                                  input$p_binomial,"^{",input$x2binom,"}(1-",input$p_binomial,
                                  ")^{",input$n_binomial,"-",input$x2binom,"} = ",rbinomial()[[2]],"$$"))
           } else {
             withMathJax(helpText("$$P(",input$x1binom,"\\leq X \\leq",input$x2binom,") =\\binom{",
                                  input$n_binomial,"}{",input$x1binom,"}",
                                  input$p_binomial,"^{",input$x1binom,"}(1-",input$p_binomial,
                                  ")^{",input$n_binomial,"-",input$x1binom,"} + ... + \\binom{",
                                  input$n_binomial,"}{",input$x2binom,"}",
                                  input$p_binomial,"^{",input$x2binom,"}(1-",input$p_binomial,
                                  ")^{",input$n_binomial,"-",input$x2binom,"} = ",rbinomial()[[2]],"$$"))
           },
           # Poisson -----------------------------------------------------------
           poisson = if(input$x1pois == input$x2pois) {
             withMathJax(helpText("$$P(X = ",input$x2pois,") = \\frac{e^{-",input$lambda2,"}",input$lambda2,"^{",input$x2pois,"}}",
                                  "{",input$x2pois,"!} = ",rpoisson()[[2]],"$$"))
           } else if (near_poisson() == input$x2pois){
             withMathJax(helpText("$$P(",input$x1pois,"\\leq X \\leq",input$x2pois,
                                  ") = \\frac{e^{-",input$lambda2,"}",input$lambda2,"^{",input$x1pois,"}}",
                                  "{",input$x1pois,"!} + \\frac{e^{-",input$lambda2,"}",input$lambda2,"^{",input$x2pois,"}}",
                                  "{",input$x2pois,"!}",rpoisson()[[2]],"$$"))
           } else {
             withMathJax(helpText("$$P(",input$x1pois,"\\leq X \\leq",input$x2pois,
                                  ") = \\frac{e^{-",input$lambda2,"}",input$lambda2,"^{",input$x1pois,"}}",
                                  "{",input$x1pois,"!} + ... + \\frac{e^{-",input$lambda2,"}",input$lambda2,"^{",input$x2pois,"}}",
                                  "{",input$x2pois,"!}",rpoisson()[[2]],"$$"))
           },
           # Geometrico --------------------------------------------------------
           geometrico = if (input$x1geom == input$x2geom) {
             withMathJax(helpText("$$P(X = ",input$x1geom,") = ", input$pgeom,"(1-",input$pgeom,")^{",input$x2geom,"}=",rgeom()[[2]],"$$"))
           } else if (near_geom() == input$x2geom) {
             withMathJax(helpText("$$P(",input$x1geom,"\\leq X \\leq",input$x2geom,")=",input$pgeom,"(1-",input$pgeom,")^{",input$x1geom,"} +",
                                  input$pgeom,"(1-",input$pgeom,")^{",input$x2geom,"}=",rgeom()[[2]],"$$"))
           } else {
             withMathJax(helpText("$$P(",input$x1geom,"\\leq X \\leq",input$x2geom,")=",input$pgeom,"(1-",input$pgeom,")^{",input$x1geom,"} + ... +",
                                  input$pgeom,"(1-",input$pgeom,")^{",input$x2geom,"}=",rgeom()[[2]],"$$"))
           },
           # Hipergeometrica ---------------------------------------------------
            hipergeometrica = if (input$x1hiper == input$x2hiper) {
              withMathJax(helpText("$$P(X =",input$x1hiper,") = ","\\frac{\\binom{",
                                   input$mhiper,"}{",input$x2hiper,"} \\binom{",N_total_hiper(),"}{",input$nhiper,"-",input$x2hiper,"}}{\\binom{",
                                   N_total_hiper(),"}{",input$nhiper,"}}=",rhipergeom()[[2]],"$$"))
            } else if (near_hiper() == input$x2hiper) {
              withMathJax(helpText("$$P(",input$x1hiper,"\\leq X \\leq",input$x2hiper,")= \\frac{\\binom{",input$mhiper,
                                   "}{",input$x1hiper,"}\\binom{",N_total_hiper(),"}{",input$nhiper,"-",
                                   input$x1hiper,"}}{\\binom{",N_total_hiper(),"}{",input$nhiper,"}}+",
                                   "\\frac{\\binom{",input$mhiper,
                                   "}{",input$x2hiper,"}\\binom{",N_total_hiper(),"}{",input$nhiper,"-",
                                   input$x2hiper,"}}{\\binom{",N_total_hiper(),"}{",input$nhiper,"}}=",rhipergeom()[[2]],"$$"))
            } else {
              withMathJax(helpText("$$P(",input$x1hiper,"\\leq X \\leq",input$x2hiper,")= \\frac{\\binom{",input$mhiper,
                                   "}{",input$x1hiper,"}\\binom{",N_total_hiper(),"}{",input$nhiper,"-",
                                   input$x1hiper,"}}{\\binom{",N_total_hiper(),"}{",input$nhiper,"}}+...+",
                                   "\\frac{\\binom{",input$mhiper,
                                   "}{",input$x2hiper,"}\\binom{",N_total_hiper(),"}{",input$nhiper,"-",
                                   input$x2hiper,"}}{\\binom{",N_total_hiper(),"}{",input$nhiper,"}}=",rhipergeom()[[2]],"$$"))
            },
           # Binomial Negativa -------------------------------------------------
           binomialneg = if (input$x1bn == input$x2bn) {
             withMathJax(helpText("$$P(X=",input$x1bn,")=\\binom{",input$kbn,"-1}{",input$x1bn,"-1}",input$pbn,"^{",input$x1bn,"}",
                                  "(1-",input$pbn,")^{",input$kbn,"-",input$x1bn,"}=",rbinomialneg()[[2]],"$$"))
           } else if (near_bn() == input$x2bn) {
             withMathJax(helpText("$$P(",input$x1bn,"\\leq X \\leq",input$x2bn,") = ",
                                  "\\binom{",input$kbn,"-1}{",input$x1bn,"-1}",input$pbn,"^{",input$x1bn,"}",
                                  "(1-",input$pbn,")^{",input$kbn,"-",input$x1bn,"} +",
                                  "\\binom{",input$kbn,"-1}{",input$x2bn,"-1}",input$pbn,"^{",input$x2bn,"}",
                                  "(1-",input$pbn,")^{",input$kbn,"-",input$x2bn,"}=",
                                  rbinomialneg()[[2]],"$$"))
           } else {
             withMathJax(helpText("$$P(",input$x1bn,"\\leq X \\leq",input$x2bn,") = ",
                                  "\\binom{",input$kbn,"-1}{",input$x1bn,"-1}",input$pbn,"^{",input$x1bn,"}",
                                  "(1-",input$pbn,")^{",input$kbn,"-",input$x1bn,"} + ... + ",
                                  "\\binom{",input$kbn,"-1}{",input$x2bn,"-1}",input$pbn,"^{",input$x2bn,"}",
                                  "(1-",input$pbn,")^{",input$kbn,"-",input$x2bn,"}=",
                                  rbinomialneg()[[2]],"$$"))
           }
           )
  })
  
  
})
