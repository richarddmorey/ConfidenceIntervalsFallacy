require(shiny)

reactiveSvg <- function (outputId) 
{
  HTML(paste("<div id=\"", outputId, "\" class=\"shiny-network-output\"><svg /></div>", sep=""))
}

textInput3<-function (inputId, label, value = "",...) 
{
  div(style="display:inline-block",
      tags$label(text=label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,...))
}

shinyUI(
  navbarPage(
    title="The lost submarine",
    tabPanel(title="Figure 1",
      sidebarLayout(
        sidebarPanel(
          shinyjs::useShinyjs(),
          radioButtons(inputId="whichPlot", label="Plot:" , choices=c("Figure 1A","Figure 1B","Choose your own / Sample")),
          sliderInput(inputId="values", label=HTML("Bubbles:"), value=c(-4,-3.5), min=-5, max=5,step=.001),
          conditionalPanel(
            condition = "input.whichPlot == 'Choose your own / Sample'",
            actionButton("sample", "Sample"),
            actionButton("sample1000", "Sample 1000"),
            radioButtons(inputId="whichValue", label="Test against:" , choices=c("True value (θ)"="true","Other value (θ')"="other")),
            conditionalPanel(
              condition = "input.whichValue == 'other'",
              sliderInput(inputId="testValue", label=HTML("Test value (diff. from θ):"), value=0, min=-7.5, max=7.5,step=.001)
            ),
            textOutput(outputId = "sampleStats"),
            textOutput(outputId = "BayesStats"), 
            textOutput(outputId = "UMPStats"),
            textOutput(outputId = "NonparaStats"),
            textOutput(outputId = "SampDistStats")
          )
        ),
        mainPanel(
          includeHTML("svgfiller.js"),
          reactiveSvg(outputId = "svg.grid")
        )
      )
    ),
    tabPanel(title="Story",
             HTML('<h4>Story</h4>'),
             HTML('<p>A 10-meter-long research submersible with several people on board has lost contact with its surface support vessel. The submersible has a rescue hatch exactly halfway along its length, to which the support vessel will drop a rescue line. Because the rescuers only get one rescue attempt, it is crucial that when the line is dropped to the craft in the deep water that the line be as close as possible to this hatch. The researchers on the support vessel do not know where the submersible is, but they do know that it forms two distinctive bubbles. These bubbles could form anywhere along the craft\'s length, independently, with equal probability, and float to the surface where they can be seen by the support vessel (see the Model tab).'),
             HTML('<p>The situation is shown in the Figure tab. The rescue hatch is the unknown location \\(\\theta\\), and the bubbles can rise from anywhere with uniform probability between \\(\\theta-5\\) meters (the bow of the submersible) to \\(\\theta+5\\) meters (the stern of the submersible). The rescuers want to use these bubbles &mdash; which we will denote \\(y_1\\) and \\(y_2\\) (letting \\(x_1\\) and \\(x_2\\) denote the smaller and the larger, respectively, for convenience) &mdash; to infer where the hatch is located.'),
             HTML('<p>The rescuers first note that from observing two bubbles, it is easy to rule out all values except those within five meters of both bubbles because no bubble can occur further than 5 meters from the hatch. If the two bubble locations were \\(x_1=1\\) and \\(x_2=3\\), then the possible locations of the hatch are between -2 and 6, because only these locations are within 5 meters of both bubbles. The function that describes the probability density of the observed bubbles for a particular value of \\(\\theta\\) is called the "likelihood," and it indexes the information provided by the data about the parameter. In this case, it is positive only when a value \\(\\theta\\) is possible given the observed bubbles (see the Likelihood tab and the Figure tab).'),
             HTML('<p>A group of four statisticians happen to be on board, and the rescuers decide to ask them for help improving their judgments using statistics. The four statisticians suggest four different 50% confidence procedures. See the Confidence Interval tab for details.'),
             HTML('<p>For the rest of the story, read our manuscript, "<a href="https://github.com/richarddmorey/ConfidenceIntervalsFallacy/blob/master/fundamentalError.pdf" target="_blank">The Fallacy of Placing Confidence in Confidence Intervals</a>"!')
    ),
    tabPanel("Model",
      withMathJax(),
      HTML('<h4>Model</h4>'),
      helpText('Let \\(y_1\\) and \\(y_2\\) be independent observations
                from a Uniform distribution:  
                $$\\begin{eqnarray}
                y_1, y_2 &\\stackrel{indep.}{\\sim}&\\mbox{Uniform}(\\theta-5, \\theta+5)
                \\end{eqnarray}
                $$ 
               For convenience, we define:
                $$\\begin{eqnarray}
                x_1&=&\\min(y_1, y_2)\\\\
                x_2&=&\\max(y_1, y_2)\\\\
                \\end{eqnarray}
                $$'),
      HTML('<h4>Likelihood</h4>'),
      helpText('The likelihood a function that is positive for all the possible values of \\(\\theta\\): 
               $$\\begin{eqnarray}
               \\mbox{Likelihood is positive in} &:& \\bar{x} \\pm \\left(5 - \\frac{x_2 - x_1}{2}\\right).
               \\end{eqnarray}
               $$
               The likelihood is positive for all values that are within 5 meters of both bubbles. This makes sense, since no bubble can be more than 5 meters from the hatch (θ).')
    ),    
    tabPanel("Confidence intervals",
      withMathJax(),
      HTML('<h4>Confidence Intervals</h4>'),
      helpText('We consider four 50% confidence intervals for \\(\\theta\\) in our manuscript.'),
      helpText('An interval based on the sampling distribution of \\(\\bar{x}\\):
               $$\\begin{eqnarray}
               \\mbox{Samp. Dist. CI}_{50\\%} &:& \\bar{x}\\pm (5 - 5/\\sqrt{2})
               \\end{eqnarray}
               $$'),
      helpText('A non-parametric interval:
                $$\\begin{eqnarray}
               \\mbox{Nonpara. CI}_{50\\%} &:& \\bar{x} \\pm \\frac{x_2 - x_1}{2}
               \\end{eqnarray}
               $$'),
      helpText('The "most powerful" (UMP) interval:
                $$\\begin{eqnarray}
                  \\mbox{UMP CI}_{50\\%} &:& 
                  \\bar{x} \\pm \\left\\{\\begin{array}{lllr}
                    \\frac{x_2 - x_1}{2} & \\mbox{if} & x_2 - x_1 < 5 & \\mbox{(Nonparametric procedure)}\\\\
                    5 - \\frac{x_2 - x_2}{2} &\\mbox{if} & x_2 - x_1 \\geq 5 & \\mbox{(Likelihood)}
                \\end{array} \\right.               
                \\end{eqnarray}
               $$'),
      helpText('The objective Bayesian (also fiducial) CI:
                $$\\begin{eqnarray}
               \\mbox{Bayes CI}_{50\\%} &:& \\bar{x} \\pm \\frac{1}{2}\\left(5 - \\frac{x_2 - x_1}{2}\\right)
               \\end{eqnarray}
               $$')
     ),
    tabPanel(title="Figure 2",
             sidebarLayout(
               sidebarPanel(
                 shinyjs::useShinyjs(),
                 radioButtons(inputId="whichCI", label="Interval:" , choices=c("Bayes","UMP","Nonparametric","Sampling Distribution")),
                 HTML("<hr/>"),
                 helpText("The shaded region shows data where the particular CI will contain the true value of θ. Panel A has axes corresponding to the data points, and Panel B is a rotated version of Panel A, with axis corresponding to the mean and the difference of the data points."),
                 HTML("<hr/>"),
                 textOutput(outputId = "widthInfo")
               ),
               mainPanel(
                 plotOutput(outputId = "svg.grid2")
               )
             )
    ),
    tabPanel(title="Figure 3",
             sidebarLayout(
               sidebarPanel(
                 shinyjs::useShinyjs(),
                 helpText("Show intervals:"),
                 checkboxInput("fig3ShowBayes", "Bayes", value = TRUE),
                 checkboxInput("fig3ShowUMP", "UMP", value = TRUE),
                 checkboxInput("fig3ShowNP", "Nonparametric", value = TRUE),
                 checkboxInput("fig3ShowSD", "Sampling distribution", value = TRUE)
               ),
               mainPanel(
                 plotOutput(outputId = "svg.grid3")
               )
             )
    ),
    tabPanel(title="Figure 4",
             sidebarLayout(
               sidebarPanel(
                 helpText("Show intervals:"),
                 checkboxInput("fig4ShowBayes", "Bayes", value = TRUE),
                 checkboxInput("fig4ShowUMP", "UMP", value = TRUE),
                 checkboxInput("fig4ShowNP", "Nonparametric", value = TRUE),
                 checkboxInput("fig4ShowSD", "Sampling distribution", value = TRUE),
                 checkboxInput("fig4ShowTrivial", "Trivial", value = TRUE),
                 conditionalPanel(
                   condition = "input.whichValue == 'other'",
                   htmlOutput(outputId = "incProbs")
                 )
               ),
               mainPanel(
                 plotOutput(outputId = "svg.grid4")               )
             )
    ),         
    tabPanel(title="Figure 5",
             sidebarLayout(
               sidebarPanel(
                 shinyjs::useShinyjs(),
                 radioButtons(inputId="whichPrior", label="Prior:" , choices=c("Objective/Noninformative","Informative")),
                 HTML("<hr/>"),
                 helpText(HTML("The prior represents information we have about the parameter &mdash; in this case, θ, the location of the sumbersible's hatch &mdash; before seeing data.")),
                 helpText(HTML("The prior is multiplied by the likelihood &mdash; which represents the information in the data &mdash; to obtain the posterior distribution, which contains our updated conclusions about θ, given the data.")),
                 helpText(HTML("An interval with posterior area 50% can then be selected as a so-called '50% credible interval'.")),
                 textOutput(outputId = "bayesPriorInfo")
               ),
               mainPanel(
                 plotOutput(outputId = "svg.grid5")
               )
             )
    )         
             
  )
)
