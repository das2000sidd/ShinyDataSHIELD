library(DSI)
library(DSOpal)
library(dsBaseClient)
library(dsOmicsClient)
library(shinydashboard)
library(shiny)
library(shinyalert)
library(DT)
library(data.table)
library(shinyjs)
library(shinyBS)
# library(ggplot2)
library(shinycssloaders)
library(shinyWidgets)
library(stringr)

jscode <- '
$(document).keyup(function(event) {
    if ($("#password").is(":focus") && (event.keyCode == 13)) {
        $("#connect_server").click();
    }
});
'

jscode_tab <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}
"

css_tab <- "
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}"

## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    uiOutput("userpanel"),
    menuItem("Connect to server", tabName = "server_connect"),
    hidden(menuItem("Descriptive statistics", tabName = "d_statistics")),
    hidden(menuItem("Statistic models", tabName = "statistic_models")),
    # hidden(menuItem("Mixed statistic models", tabName = "statistic_models_mixed")),
    hidden(menuItem("Genomics", tabName = "genomics",
             menuSubItem("Analysis with BioConductor", tabName = "vcf_files"),
             menuSubItem("Analysis with PLINK", tabName = "plink")
             )),
    hidden(menuItem("Omics", tabName = "omics",
             menuSubItem("LIMMA", tabName = "limma")
             ))
  )
)

body <- dashboardBody(
  useShinyalert(),
  useShinyjs(),
  extendShinyjs(text = jscode_tab, functions = c("enableTab", "disableTab")),
  inlineCSS(css_tab),
  tabItems(
    tabItem(tabName = "server_connect",
            tabPanel('server_connect',
                     fluidRow(
                       # actionButton("stop", "stop"),
                       tabBox(
                         title = p(circleButton("add", size = "sm", icon = icon("plus"), status = "primary"),
                                   circleButton("remove", size = "sm", icon = icon("minus"), status = "primary")
                                   ),
                         id = "tabset1", width = 12,
                         tabPanel("Server1", 
                                  fluidRow(
                                  column(6,
                                         h3("URL"),
                                         textInput("url1", "Opal server", value = "https://opal-demo.obiba.org/")
                                  ),
                                  column(6,
                                         h3("Credentials"),
                                         conditionalPanel(
                                           condition = "input.pat_switch1 == true",
                                           passwordInput("pat1", "Personal Access Token"),
                                         ),
                                         conditionalPanel(
                                           condition = "input.pat_switch1 == false",
                                           textInput("user1", "User", value = "administrator"),
                                           tags$head(tags$script(HTML(jscode))),
                                           passwordInput("password1", "Password", value = "password")
                                         ),
                                         materialSwitch(inputId = "pat_switch1", label = "Use Personal Access Token", status = "primary")
                                  )
                                  ),
                                  hr(),
                                  fluidRow(
                                    column(12,
                                           hidden(switchInput(
                                             inputId = "tbl_res1", value = TRUE, #label = "Tables or resources?",
                                             onLabel = "Tables", offLabel = "Resources",
                                             onStatus = "primary", offStatus = "primary"
                                           ))
                                    )
                                  ),
                                  fluidRow(
                                    column(6,
                                           uiOutput("project_selector1"),
                                    ),
                                    column(6,
                                           uiOutput("resource_selector1"),
                                    )
                                  ),
                                  fluidRow(
                                    column(6,actionButton("connect_server1", "Connect"))
                                  ),
                                  hidden(actionButton("add_server1", "Add selected item(s)"))
                                  # hidden(actionButton("remove_server1", "Remove selected study"))
                                  )
                       )
                     ),
                     fluidRow(
                       column(6,
                              hidden(actionButton("remove_item", "Remove selected item(s)")),
                              hidden(actionButton("connect_selected", "Connect"))
                       )
                     ),
                     dataTableOutput("server_resources_table"),
            )
    ),
    tabItem(tabName = "d_statistics",
            fluidRow(
              tabBox(width = 12, id = "d_statistics_t",
                     tabPanel("Available tables", value = "a_tables",
                              uiOutput("available_tables"),
                              actionButton("select_tables_descr_stats", "Select tables")
                     ),
                     tabPanel("Summary", value = "summary",
                              uiOutput("d_statistics_variable_selector_approach"),
                              uiOutput("d_statistics_variable_selector"),
                              dataTableOutput("descriptive_summary"),
                              downloadButton("descriptive_summary_download", "Download")
                     ),
                     tabPanel("Scatter plot", value = "s_plot",
                              uiOutput("d_statistics_variable_selector_scatter_approach"),
                              uiOutput("d_statistics_variable_selector_scatter"),
                              uiOutput("d_statistics_variable_selector_scatter2"),
                              withSpinner(plotOutput("d_statistics_scatter_plot")),
                              downloadButton("d_statistics_scatter_plot_download", "Download plot")
                     ),
                     tabPanel("Histogram", value = "h_plot",
                              uiOutput("d_statistics_variable_selector_histogram_approach"),
                              uiOutput("d_statistics_variable_selector_histogram"),
                              withSpinner(plotOutput("d_statistics_histogram_plot")),
                              downloadButton("d_statistics_histogram_plot_download", "Download plot")
                     ),
                     tabPanel("Heatmap", value = "hm_plot",
                              uiOutput("d_statistics_variable_selector_heatmap_approach"),
                              uiOutput("d_statistics_variable_selector_heatmap"),
                              uiOutput("d_statistics_variable_selector_heatmap2"),
                              withSpinner(plotOutput("d_statistics_heatmap_plot")),
                              downloadButton("d_statistics_heatmap_plot_download", "Download plot")
                     )
              )
            )
    ),
    tabItem(tabName = "statistic_models",
            fluidRow(
                     tabBox(width = 12, id = "statistic_models_t",
                            tabPanel("Available tables", value = "a_tables_sm",
                                     uiOutput("available_tables_sm"),
                                     actionButton("select_tables_sm", "Select tables")
                                     ),
                            tabPanel("Generalized linear model", value = "glm",
                                        fluidRow(
                                          column(6,
                                                 textInput("glm_formula", "Input GLM formula:"),
                                                 actionButton("trigger_formula_help_glm", "Formula input help"),
                                                 actionButton("perform_glm", "Perform GLM")
                                          ),
                                          column(6,
                                                 selectInput("gml_output_family", "Output family:", c("gaussian", "poisson", "binomial")),
                                                 # actionButton("gml_toggle_variables_table","Toggle variables table")
                                          )
                                        ),
                                              # dataTableOutput("available_variables_type"),
                                              dataTableOutput("glm_results_table"),
                                              hidden(downloadButton("glm_results_table_download", "Download GLM results"))
                                     ),
                            tabPanel("Mixed effects model", value = "mixed_model",
                                     fluidRow(
                                       column(6,
                                              textInput("glmer_formula", "Input GLMer formula:"),
                                              actionButton("trigger_formula_help_glmer", "Formula input help"),
                                              actionButton("perform_glmer", "Perform GLMer")
                                       ),
                                       column(6,
                                              selectInput("gmler_output_family", "Output family:", c("poisson", "binomial")),
                                              # actionButton("gmler_toggle_variables_table","Toggle variables table")
                                       )
                                     ),
                                     # dataTableOutput("available_variables_type2"),
                                     # uiOutput("glmer_server_select"),
                                     uiOutput("glmer_results_select"),
                                     dataTableOutput("glmer_results_table"),
                                     hidden(downloadButton("glmer_results_table_download", "Download GLMer results"))
                                     )
                            )
                     )
    ),
    tabItem(tabName = "plink",
            fluidRow(
              tabBox(width = 12, id = "plink_t",
                     tabPanel("Available SSH resources",
                              uiOutput("available_tables_ssh"),
                              actionButton("select_tables_ssh", "Select SSH")
                              ),
                     tabPanel("PLINK", value = "plink",
                              textInput("command", "PLINK Shell command", width = "100%"),
                              h5("NOTE: we avoid –out to indicate the output file"),
                              h5("NOTE: No need to input plink as in a shell command"),
                              code("plink < >"),
                              h5("can be inputed as"),
                              code("< >"),
                              h5(""),
                              actionButton("run_shell", "Run Shell command"),
                              hidden(actionButton("plink_show_plain", "Show PLINK terminal output")),
                              dataTableOutput("plink_results_table"),
                              hidden(downloadButton("plink_results_table_download", "Download PLINK results")),
                              bsModal("plink_results_terminal", "PLINK Terminal output", "plink_show_plain",
                                      verbatimTextOutput("plink_results_terminal_render")
                              )
                     ),
                     tabPanel("Manhattan Plot", value = "plink_plot",
                              withSpinner(plotOutput("manhattan2")),
                              downloadButton("genomics_manhattan_plink_plot_download", "Download plot")
                     )
              )
            )
    ),
    tabItem(tabName = "vcf_files",
            fluidRow(
              tabBox(width = 12, id = "vcf_files_t",
                tabPanel("Available resources",
                  uiOutput("available_tables_vcf"),
                  actionButton("select_tables_vcf", "Select VCF resource and covariates table")
                ),     
                tabPanel("GWAS", value = "gwas",
                  uiOutput("vcf_selector_var"),
                  uiOutput("vcf_selector_cov"),
                  actionButton("gwas_trigger", "Perform GWAS"),
                  dataTableOutput("vcf_results"),
                  hidden(downloadButton("vcf_results_table_download", "Download GWAS results"))
                ),
                tabPanel("Manhattan Plot", value = "gwas_plot",
                  withSpinner(plotOutput("manhattan")),
                  downloadButton("genomics_manhattan_vcf_plot_download", "Download plot")
                )
              )
            )
    ),
    tabItem(tabName = "limma",
            fluidRow(
              tabBox(width = 12, id = "limma_t",
                     tabPanel("Available resources", value = "limma_a",
                              uiOutput("available_tables_lim"),
                              actionButton("select_tables_lim", "Select resource")
                              ),
                     tabPanel("Limma", value = "limma",
                              fluidRow(
                                column(6,
                                       uiOutput("limma_variables_selector_condition"),
                                       uiOutput("limma_variables_selector_covars"),
                                       uiOutput("limma_sva_selector"),
                                       uiOutput("limma_run")
                                ),
                                column(6,
                                       uiOutput("limma_labels_selector"),
                                       selectInput("limma_data_type", "Data type:", c("RNAseq", "microarray"))
                                )
                              ),
                              fluidRow(
                                column(12,
                                       dataTableOutput("limma_results_table"),
                                       hidden(downloadButton("limma_results_table_download", "Download LIMMA results"))
                                )
                              )
                              )
                     )
            )
    )
  )
)
# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "DataSHIELD"),
  sidebar,
  body
)

