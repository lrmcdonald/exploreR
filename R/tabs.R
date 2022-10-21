#' Tab Constructors for UI
#'
#' Shiny slider <0 to 1> input function
#' @export
#' @param id The shiny variable used.
#' @param label The label for the slider.
sliderInput01 = function(id, label) {
  return(shiny::sliderInput(id, label = label, min = 0, max = 1, value = 0.5, step = 0.01))
}

#' Discrete Distributions
#' @export
tab_binom = function(){
  return(shiny::tabPanel(title = "Binomial",
                         shiny::h3("Parameters"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Number of Trials:"),
                                         shiny::numericInput("binom_n", label = NULL,
                                                             min = 1, step = 1, value = 1)),
                           shiny::column(6,
                                         shiny::h4("Probability of Success:"),
                                         sliderInput01(id = "binom_p", label = NULL))
                         ), # End row
                         shiny::h3("Inputs"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Minimum Trials:"),
                                         shiny::numericInput("binom_min", label = NULL, value = 1,
                                                             min = 1, step = 1)),
                           shiny::column(6,
                                         shiny::h4("Maximum Trials:"),
                                         shiny::numericInput("binom_max", label = NULL, value = 10,
                                                             step = 1, min = 3))
                         ), # End row
                         shiny::plotOutput("plot_binom")))

}
#' @export
tab_bern = function(){
  return(shiny::tabPanel(title = "Bernoulli",
                         shiny::h3("Parameters"),
                         shiny::fluidRow(
                           shiny::column(12,
                                         shiny::h4("Probability of Success:"),
                                         shiny::sliderInput("bern_p", label = NULL,
                                              min = 0, max = 1, step = 0.01, value = 0.5))
                         ), # End row
                         shiny::plotOutput("plot_bern")))
}
#' @export
tab_pois = function(){
  return(shiny::tabPanel(title = "Poisson",
                         shiny::h3("Parameters"),
                         shiny::fluidRow(
                           shiny::column(12,
                                  shiny::h4("Rate At Which Events Occur:"),
                                  shiny::numericInput("pois_lambda", label = NULL,
                                               min = 0, step = 1, value = 1))
                         ), # End row
                         shiny::h3("Inputs"),
                         shiny::fluidRow(
                           shiny::column(6,
                                  shiny::h4("Minimum Count:"),
                                  shiny::numericInput("pois_min", label = NULL, value = 0,
                                                      min = 0, step = 1)),
                           shiny::column(6,
                                  shiny::h4("Maximum Count:"),
                                  shiny::numericInput("pois_max", label = NULL, value = 10,
                                                      step = 1, min = 3))
                         ), # End row
                         shiny::plotOutput("plot_pois")))
}
#' @export
tab_nbinom = function(){
  return(shiny::tabPanel(title = "Negative Binomial",
                         shiny::h3("Parameters"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Number of Successes Required:"),
                                         shiny::numericInput("nbinom_r", label = NULL,
                                                             min = 1, step = 1, value = 1)),
                           shiny::column(6,
                                         shiny::h4("Probability of Success:"),
                                         sliderInput01(id = "nbinom_p", label = NULL))
                         ), # End row
                         shiny::h3("Inputs"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Minimum Trials Until rth Success:"),
                                         shiny::numericInput("nbinom_min", label = NULL, value = 1,
                                                             min = 1, step = 1)),
                           shiny::column(6,
                                         shiny::h4("Maximum Trials Until rth Success:"),
                                         shiny::numericInput("nbinom_max", label = NULL, value = 10,
                                                             step = 1, min = 3))
                         ), # End row
                         shiny::plotOutput("plot_nbinom")))
}
#' @export
tab_geom = function(){
  return(shiny::tabPanel(title = "Geometric",
                         shiny::h3("Parameters"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Probability of Success:"),
                                         shiny::sliderInput("geom_p", label = NULL,
                                                            min = 0, max = 1, step = 0.01, value = 0.5))
                         ), # End row
                         shiny::h3("Inputs"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Minimum Trials Until 1st Success:"),
                                         shiny::numericInput("geom_min", label = NULL, value = 1,
                                                             min = 1, step = 1)),
                           shiny::column(6,
                                         shiny::h4("Maximum Trials Until 1st Success:"),
                                         shiny::numericInput("geom_max", label = NULL, value = 10,
                                                             step = 1, min = 3))
                         ), # End row
                         shiny::plotOutput("plot_geom")))
}
#' @export
tab_hyper = function(){
  return(shiny::tabPanel(title = "Hyper Geometric",
                         shiny::h3("Parameters"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Population of Type 1's:"),
                                         shiny::numericInput("hyper_type1", label = NULL, value = 5,
                                               min = 1, step = 1)),
                           shiny::column(6,
                                         shiny::h4("Population of Type 2's"),
                                         shiny::numericInput("hyper_type2", label = NULL, value = 5,
                                               step = 1, min = 1))
                         ), # End row
                         shiny::h3("Inputs"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Number of Type 1's Drawn:"),
                                         shiny::numericInput("hyper_draws", label = NULL,
                                               min = 1, step = 1, value = 1)),
                           shiny::column(6,
                                         shiny::h4("Number of Trials"),
                                         shiny::numericInput("hyper_trials", label = NULL,
                                               min = 1, step = 1, value = 1))
                         ), # End row
                         shiny::plotOutput("plot_hyper")))
}

#' Continuous Distributions
#' @export
tab_unif = function(){
  return(shiny::tabPanel(title = "Uniform",
                         shiny::h3("Parameters"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Minimum Value (a):"),
                                         shiny::numericInput("unif_a", label = NULL, value = 6,
                                                             step = 0.5)),
                           shiny::column(6,
                                         shiny::h4("Maximum Value (b):"),
                                         shiny::numericInput("unif_b", label = NULL, value = 9,
                                                             step = 0.5))
                         ), # End row
                         shiny::h3("Inputs"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Minimum Outcome:"),
                                         shiny::numericInput("unif_min", label = NULL, step = 0.5,
                                                             value = 1)),
                           shiny::column(6,
                                         shiny::h4("Maximum Outcome:"),
                                         shiny::numericInput("unif_max", label = NULL, step = 0.5,
                                                             value = 15))
                         ), # End row
                         shiny::plotOutput("plot_unif")))
}
#' @export
tab_norm = function(){
  return(shiny::tabPanel(title = "Normal",
                         shiny::h3("Parameters"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Mean Outcome:"),
                                         shiny::numericInput("norm_mu", label = NULL, value = 5,
                                                             step = 0.1)),
                           shiny::column(6,
                                         shiny::h4("Standard Deviation:"),
                                         shiny::numericInput("norm_sigma", label = NULL, value = 2.5,
                                                             step = 0.1))
                         ), # End row
                         shiny::h3("Inputs"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Minimum Outcome:"),
                                         shiny::numericInput("norm_min", label = NULL, step = 0.5,
                                                             value = 1)),
                           shiny::column(6,
                                         shiny::h4("Maximum Outcome:"),
                                         shiny::numericInput("norm_max", label = NULL, step = 0.5,
                                                             value = 15))
                         ), # End row
                         shiny::plotOutput("plot_norm")))
}
#' @export
tab_lnorm = function(){
  return(shiny::tabPanel(title = "Log-Normal",
                         shiny::h3("Parameters"),
                         shiny::fluidRow(
                           shiny::column(4,
                                         shiny::h4("Mean Outcome:"),
                                         shiny::numericInput("lnorm_mu", label = NULL, value = 2,
                                                             step = 0.1)),
                           shiny::column(4,
                                         shiny::h4("Standard Deviation:"),
                                         shiny::numericInput("lnorm_sigma", label = NULL, value = 2,
                                                             step = 0.1)),
                           shiny::column(4,
                                         shiny::h4("Are Parameters on the Log Scale?"),
                                         shiny::checkboxInput("logpars", label = NULL, value = FALSE, width = NULL))
                         ), # End row
                         shiny::h3("Inputs"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Minimum Outcome:"),
                                         shiny::numericInput("lnorm_min", label = NULL, step = 0.5,
                                                             value = 0)),
                           shiny::column(6,
                                         shiny::h4("Maximum Outcome:"),
                                         shiny::numericInput("lnorm_max", label = NULL, step = 0.5,
                                                             value = 15))
                         ), # End row
                         shiny::plotOutput("plot_lnorm")))
}
#' @export
tab_exp = function(){
  return(shiny::tabPanel(title = "Exponential",
                         shiny::h3("Parameters"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Rate At Which Events Occur:"),
                                         shiny::numericInput("exp_rate", label = NULL, value = 2.5,
                                                             step = 0.1))
                         ), # End row
                         shiny::h3("Inputs"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Minimum Outcome:"),
                                         shiny::numericInput("exp_min", label = NULL, step = 0.5,
                                                             value = 1)),
                           shiny::column(6,
                                         shiny::h4("Maximum Outcome:"),
                                         shiny::numericInput("exp_max", label = NULL, step = 0.5,
                                                             value = 15))
                         ), # End row
                         shiny::plotOutput("plot_exp")))
}
#' @export
tab_gamma = function(){
  return(shiny::tabPanel(title = "Gamma",
                         shiny::h3("Parameters"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Alpha:"),
                                         shiny::numericInput("gamma_alpha", label = NULL, value = 5,
                                                             step = 0.1)),
                           shiny::column(6,
                                         shiny::h4("Beta:"),
                                         shiny::numericInput("gamma_beta", label = NULL, value = 2.5,
                                                             step = 0.1))
                         ), # End row
                         shiny::h3("Inputs"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Minimum Outcome:"),
                                         shiny::numericInput("gamma_min", label = NULL, step = 0.5,
                                                             value = 1)),
                           shiny::column(6,
                                         shiny::h4("Maximum Outcome:"),
                                         shiny::numericInput("gamma_max", label = NULL, step = 0.5,
                                                             value = 15))
                         ), # End row
                         shiny::plotOutput("plot_gamma")))
}
#' @export
tab_beta = function(){
  return(shiny::tabPanel(title = "Beta",
                         shiny::h3("Parameters"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Alpha:"),
                                         shiny::numericInput("beta_alpha", label = NULL, value = 1,
                                                             step = 0.1)),
                           shiny::column(6,
                                         shiny::h4("Beta:"),
                                         shiny::numericInput("beta_beta", label = NULL, value = 1,
                                                             step = 0.1))
                         ), # End row
                         shiny::plotOutput("plot_beta")))
}
#' @export
tab_vonmises = function(){
  return(shiny::tabPanel(title = "Von Mises",
                         shiny::h3("Parameters"),
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h4("Mean Outcome:"),
                                         shiny::numericInput("vonmises_mu", label = NULL, value = 0,
                                                             step = 0.1)),
                           shiny::column(6,
                                         shiny::h4("Concentration:"),
                                         shiny::numericInput("vonmises_kappa", label = NULL, value = 1,
                                                             step = 0.1))
                         ), # End row
                         shiny::plotOutput("plot_vonmises")))
}



