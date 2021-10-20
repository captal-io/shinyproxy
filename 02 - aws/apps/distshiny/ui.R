library(shiny)


# Reseta o ID 
jscode <- "Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                Shiny.onInputChange(variableName, null);
            });"

shinyUI(
  fluidPage(
    titlePanel("Principais Modelos Discretos e Contínuos"),
    withMathJax(),
    sidebarLayout(
    sidebarPanel(
      tags$head(tags$script(jscode)),
      fluidRow(
        column(width = 8,
               div(style = "height:64px;",
                 selectInput("dist",label = "Escolha um Modelo",selected = "normal",
                   choices = list(
                     Continua = c("Normal" = "normal",
                               "Exponencial" = "exponencial",
                               "Uniforme" = "uniformecont",
                               "Beta" = "beta",
                               "Gamma" = "gamma"),
                     Discreta = c("Uniforme" = "uniformedis",
                                  "Binomial" = "binomial",
                                  "Poisson" = "poisson",
                                  "Geométrico" = "geometrico",
                                  "Hipergeométrica" = "hipergeometrica",
                                  "Binomial Negativa" = "binomialneg"))))),
        column(width = 4, align = "center",
               div(style ="height:64px; margin-top: 26px",
                 actionButton("sobre","",icon("book"),width = "100%")))
        ),
      
      
      # Uniforme Discreta ------------------------------------------------------
      conditionalPanel(condition = "input.dist == 'uniformedis'",
                       fluidRow(
                         column(width = 3),
                         column(width = 6,
                                numericInput("k_unif_dis", label = HTML("<div style='width: 164px;'><center>k</center></div>"),value = 10,step = 1)),
                         column(width = 3)
                       ),
                       fluidRow(
                         column(width = 5,
                                numericInput("x1unif_dis",HTML("<div style='width: 132px;'><center>X<sub>1</sub></center></div>"),
                                             value = 1, min = 1)),
                         column(width = 2,
                                HTML("<div style='margin-top:26px; font-size:20px;'>&leq;X&leq;</div>")), # Arrumar a posição
                         column(width = 5,
                                numericInput("x2unif_dis",HTML("<div style='width: 132px;'><center>X<sub>2</sub></center></div>"),
                                             value = 5,min = 1))
                       )),
      # Binomial ---------------------------------------------------------------
      conditionalPanel(condition = "input.dist == 'binomial'",
                       fluidRow(
                         column(width = 6,
                                numericInput("n_binomial", label = HTML("<div style='width: 164px;'><center>n</center></div>"),min = 1,value = 10,step = 1)),
                         column(width = 6,
                                numericInput("p_binomial", label = HTML("<div style='width: 164px;'><center>p</center></div>"),min = 0,max=1,value = 0.5,step = 0.01))
                       ),
                       fluidRow(
                         column(width = 5,
                                numericInput("x1binom",HTML("<div style='width: 132px;'><center>X<sub>1</sub></center></div>"),
                                             value = 1, min = 0)),
                         column(width = 2,
                                HTML("<div style='margin-top:26px; font-size:20px;'>&leq;X&leq;</div>")), # Arrumar a posição
                         column(width = 5,
                                numericInput("x2binom",HTML("<div style='width: 132px;'><center>X<sub>2</sub></center></div>"),
                                             value = 5,min = 1))
                       )),
      # Poisson ----------------------------------------------------------------
      conditionalPanel(condition = "input.dist == 'poisson'",
                       fluidRow(
                         column(width = 3),
                         column(width = 6,
                                numericInput("lambda2", label = HTML("<div style='width: 164px;'><center>&lambda;</center></div>"),min = 0.00001,value = 1,step = 0.01)),
                         column(width = 3)
                       ),
                       fluidRow(
                         column(width = 5,
                                numericInput("x1pois",HTML("<div style='width: 132px;'><center>X<sub>1</sub></center></div>"),
                                             value = 1, min = 0)),
                         column(width = 2,
                                HTML("<div style='margin-top:26px; font-size:20px;'>&leq;X&leq;</div>")), # Arrumar a posição
                         column(width = 5,
                                numericInput("x2pois",HTML("<div style='width: 132px;'><center>X<sub>2</sub></center></div>"),
                                             value = 5,min = 0))
                       )),
      
      # Geometrico -------------------------------------------------------------
      conditionalPanel(condition = "input.dist == 'geometrico'",
                       fluidRow(
                         column(width = 3),
                         column(width = 6,
                                numericInput("pgeom", label = HTML("<div style='width: 164px;'><center>p</center></div>"),min = 0.00001,value = 0.5,step = 0.01)),
                         column(width = 3)
                       ),
                       fluidRow(
                         column(width = 5,
                                numericInput("x1geom",HTML("<div style='width: 132px;'><center>X<sub>1</sub></center></div>"),
                                             value = 1, min = 0)),
                         column(width = 2,
                                HTML("<div style='margin-top:26px; font-size:20px;'>&leq;X&leq;</div>")), # Arrumar a posição
                         column(width = 5,
                                numericInput("x2geom",HTML("<div style='width: 132px;'><center>X<sub>2</sub></center></div>"),
                                             value = 5,min = 0))
                       )),
      # Hipergeométrica --------------------------------------------------------
      conditionalPanel(condition = "input.dist == 'hipergeometrica'",
                       fluidRow(
                         column(width = 4,
                                numericInput("mhiper", label = HTML("<div style='width: 107px;'><center>m</center></div>"),min = 0,value = 4,step = 1)),
                         column(width = 4,
                                numericInput("nhiper", label = HTML("<div style='width: 107px;'><center>n</center></div>"),min = 0,value = 10,step = 1)),
                         column(width = 4,
                                numericInput("khiper", label = HTML("<div style='width: 107px;'><center>k</center></div>"),min = 0,value = 5,step = 1))
                       ),
                       fluidRow(
                         column(width = 5,
                                numericInput("x1hiper",HTML("<div style='width: 132px;'><center>X<sub>1</sub></center></div>"),
                                             value = 1, min = 0)),
                         column(width = 2,
                                HTML("<div style='margin-top:26px; font-size:20px;'>&leq;X&leq;</div>")), # Arrumar a posição
                         column(width = 5,
                                numericInput("x2hiper",HTML("<div style='width: 132px;'><center>X<sub>2</sub></center></div>"),
                                             value = 3,min = 0))
                       )),
      
      # Binomial Negativa ------------------------------------------------------
      conditionalPanel(condition = "input.dist == 'binomialneg'",
                       fluidRow(
                         column(width = 6,
                                numericInput("kbn", label = HTML("<div style='width: 164px;'><center>k</center></div>"),min = 0,value = 10,step = 1)),
                         column(width = 6,
                                numericInput("pbn", label = HTML("<div style='width: 164px;'><center>p</center></div>"),min = 0,value = 0.5,max = 1,step = 0.1))
                       ),
                       fluidRow(
                         column(width = 5,
                                numericInput("x1bn",HTML("<div style='width: 132px;'><center>X<sub>1</sub></center></div>"),
                                             value = 1, min = 0)),
                         column(width = 2,
                                HTML("<div style='margin-top:26px; font-size:20px;'>&leq;X&leq;</div>")), # Arrumar a posição
                         column(width = 5,
                                numericInput("x2bn",HTML("<div style='width: 132px;'><center>X<sub>2</sub></center></div>"),
                                             value = 3,min = 0))
                       )),
      # Normal -----------------------------------------------------------------
      conditionalPanel(condition = "input.dist == 'normal'",
                       fluidRow(
                         column(width = 6,
                               numericInput("mu", label = HTML("<div style='width: 164px;'><center>&mu;</center></div>"), value = 0)),
                         column(width = 6,
                               numericInput("sigma", label = HTML("<div style='width: 164px;'><center>&sigma;</center></div>"),1, min = 1))
                         ),
                       fluidRow(
                         column(width = 5,
                               numericInput("x1",HTML("<div style='width: 132px;'><center>X<sub>1</sub></center></div>"),-3)),
                         column(width = 2,
                               HTML("<div style='margin-top:26px; font-size:20px;'>&leq;X&leq;</div>")), # Arrumar a posição
                         column(width = 5,
                               numericInput("x2",HTML("<div style='width: 132px;'><center>X<sub>2</sub></center></div>"),3))
                        )),
      # Exponencial ------------------------------------------------------------
      conditionalPanel(condition = "input.dist == 'exponencial'",
                       fluidRow(
                         column(width = 3),
                         column(width = 6,
                                numericInput("lambda", label = HTML("<div style='width: 164px;'><center>&lambda;</center></div>"),min = 0.00001,value = 1,step = 0.25)),
                         column(width = 3)
                       ),
                       fluidRow(
                         column(width = 5,
                                numericInput("x1exp",HTML("<div style='width: 132px;'><center>X<sub>1</sub></center></div>"),min = 0,value = 0)),
                         column(width = 2,
                                HTML("<div style='margin-top:26px; font-size:20px;'>&leq;X&leq;</div>")), # Arrumar a posição
                         column(width = 5,
                                numericInput("x2exp",HTML("<div style='width: 132px;'><center>X<sub>2</sub></center></div>"),min = 0, value = 1))
                       )),
      # Beta -------------------------------------------------------------------
      conditionalPanel(condition = "input.dist == 'beta'",
                       fluidRow(
                         column(width = 6,
                                numericInput("a_beta", label = HTML("<div style='width: 164px;'><center>&alpha;</center></div>"),value = 2,min = 1,step = 1)),
                         column(width = 6,
                                numericInput("b_beta", label = HTML("<div style='width: 164px;'><center>&beta;</center></div>"),value = 2,step = 1, min = 1))
                       ),
                       fluidRow(
                         column(width = 5,
                                numericInput("x1beta",HTML("<div style='width: 132px;'><center>X<sub>1</sub></center></div>"),value = 0,min = 0,step = 0.01,max = 1)),
                         column(width = 2,
                                HTML("<div style='margin-top:26px; font-size:20px;'>&leq;X&leq;</div>")), # Arrumar a posição
                         column(width = 5,
                                numericInput("x2beta",HTML("<div style='width: 132px;'><center>X<sub>2</sub></center></div>"), value = 0.5,min = 0,max = 1,step = 0.01))
                       )),
      # Gamma ------------------------------------------------------------------
      conditionalPanel(condition = "input.dist == 'gamma'",
                       fluidRow(
                         column(width = 6,
                                numericInput("a_gamma", label = HTML("<div style='width: 164px;'><center>&alpha;</center></div>"),value = 2,min = 1,step = 1)),
                         column(width = 6,
                                numericInput("b_gamma", label = HTML("<div style='width: 164px;'><center>&beta;</center></div>"),value = 2,step = 1, min = 1))
                       ),
                       fluidRow(
                         column(width = 5,
                                numericInput("x1gamma",HTML("<div style='width: 132px;'><center>X<sub>1</sub></center></div>"),value = 5,min = 0,step = 1)),
                         column(width = 2,
                                HTML("<div style='margin-top:26px; font-size:20px;'>&leq;X&leq;</div>")), # Arrumar a posição
                         column(width = 5,
                                numericInput("x2gamma",HTML("<div style='width: 132px;'><center>X<sub>2</sub></center></div>"), value = 15,min = 0,step = 1))
                       )),
      # Uniforme Continua ---------------------------------------------------------------
      conditionalPanel(condition = "input.dist == 'uniformecont'",
                       fluidRow(
                         column(width = 6,
                                numericInput("a_unif_cont", label = HTML("<div style='width: 164px;'><center>a</center></div>"),value = 0,step = 1)),
                         column(width = 6,
                                numericInput("b_unif_cont", label = HTML("<div style='width: 164px;'><center>b</center></div>"),value = 1,step = 1))
                       ),
                       fluidRow(
                         column(width = 5,
                                numericInput("x1unif_cont",HTML("<div style='width: 132px;'><center>X<sub>1</sub></center></div>"),value = 0)),
                         column(width = 2,
                                HTML("<div style='margin-top:26px; font-size:20px;'>&leq;X&leq;</div>")), # Arrumar a posição
                         column(width = 5,
                                numericInput("x2unif_cont",HTML("<div style='width: 132px;'><center>X<sub>2</sub></center></div>"), value = 0.5))
                       ))
      # ------------------------------------------------------------------------
    ),
    mainPanel(
       plotOutput("distPlot"),
       uiOutput("formula") 
    )
  )
 )
)