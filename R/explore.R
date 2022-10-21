#' Open ExploreR dashboard
#'
#' This function loads the shiny app.
#'
#' Roxygen tag to export functions
#' ensures functions in this document get related to the NAMESPACE when running devtools::document()
#' @export
#' @param theme Shiny theme from shinythemes you wish to set. Defaults to 'sandstone'.
explore = function(theme = 'sandstone'){
  # Define UI based on constructor functions in R/tabs.R
  ui <- shiny::fluidPage(theme = set_theme(theme),
                         shiny::h2("Probability Distribution Viewer"),
                         shiny::tabsetPanel(shiny::tabPanel(title = "Discrete",
                                                            shiny::navlistPanel(tab_binom(),
                                                                                tab_bern(),
                                                                                tab_pois(),
                                                                                tab_nbinom(),
                                                                                tab_geom(),
                                                                                tab_hyper())),
                                            shiny::tabPanel(title = "Continuous",
                                                            shiny::navlistPanel(tab_unif(),
                                                                                tab_norm(),
                                                                                tab_lnorm(),
                                                                                tab_exp(),
                                                                                tab_gamma(),
                                                                                tab_beta(),
                                                                                tab_vonmises()))))
  # Define server logic required to update the plots using functions in R/plotters.R
  server <- function(input, output) {
    # Discrete Plots
    output$plot_binom <- shiny::renderPlot({
      x = seq(input$binom_min, input$binom_max, by = 1)
      pbinomd(x = x, n = input$binom_n, p = input$binom_p)
    })
    output$plot_bern <- shiny::renderPlot({
      pbernd(p = input$bern_p)
    })
    output$plot_pois <- shiny::renderPlot({
      x = seq(input$pois_min, input$pois_max, by = 1)
      ppoisd(x = x, lambda = input$pois_lambda)
    })
    output$plot_nbinom <- shiny::renderPlot({
      x = seq(input$nbinom_min, input$nbinom_max, by = 1)
      pnbinomd(x = x, r = input$nbinom_r, p = input$nbinom_p)
    })
    output$plot_geom <- shiny::renderPlot({
      x = seq(input$geom_min, input$geom_max, by = 1)
      pgeomd(x = x, p = input$geom_p)
    })
    output$plot_hyper <- shiny::renderPlot({
      phyperd(draws = input$hyper_draws, type1 = input$hyper_type1,
              type2 = input$hyper_type2, trials = input$hyper_trials)
    })
    # Continuous Plots
    output$plot_unif <- shiny::renderPlot({
      x = seq(input$unif_min, input$unif_max, by = 0.01)
      punifd(x = x, a = input$unif_a, b = input$unif_b)
    })
    output$plot_norm <- shiny::renderPlot({
      x = seq(input$norm_min, input$norm_max, by = 0.01)
      pnormd(x = x, mu = input$norm_mu, sigma = input$norm_sigma)
    })
    output$plot_exp <- shiny::renderPlot({
      x = seq(input$exp_min, input$exp_max, by = 0.01)
      pexpd(x = x, rate = input$exp_rate)
    })
    output$plot_gamma <- shiny::renderPlot({
      x = seq(input$gamma_min, input$gamma_max, by = 0.01)
      pgammad(x = x, alpha = input$gamma_alpha, beta = input$gamma_beta)
    })
    output$plot_beta <- shiny::renderPlot({
      pbetad(alpha = input$beta_alpha, beta = input$beta_beta)
    })
    output$plot_vonmises <- shiny::renderPlot({
      pvonmisesd(mu = input$vonmises_mu, kappa = input$vonmises_kappa)
    })
  } # end of server function
  shiny::shinyApp(ui = ui, server = server)
}

#' Set shiny theme
#' @export
set_theme = function(theme){
  return(shinythemes::shinytheme(theme))
}
