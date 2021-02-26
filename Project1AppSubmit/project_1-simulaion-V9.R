###################################################################################

# Initialization
## Ensure accurate libraries
### State the required libraries to check for prior installation, versions and load
requiredLibraries <- c("shiny", "shinydashboard", "markdown", "DT", "ggplot2", "plyr")

### Build install / update / load function
installUpdateLoadLibraries <- 
  function (requiredLibraries, updateFlag = FALSE) {
    # Find the currently installed libraries
    installedLibraries <- library()$results[,1]
    
    # Find the out of date libraries
    oldLibraries <- old.packages()[,1]
    
    # Return the names of the libraries to install or update
    installInclusion <- !(requiredLibraries %in% installedLibraries) 
    if (updateFlag) {
      installInclusion <- installInclusion | (requiredLibraries %in% oldLibraries)
    }
    packagesToInstall <- requiredLibraries[installInclusion]
    
    # Install the required packages and their dependencies
    install.packages(packagesToInstall, dependencies = TRUE)
    
    # Load the required libraries
    for (currentPackage in requiredLibraries) {
      library(currentPackage, character.only = TRUE)
    }
    
    # Return when complete
    return(TRUE)
  }

## Run install / update
installUpdateLoadLibraries(requiredLibraries)

# Gather data
## https://climate.weather.gc.ca/climate_data/daily_data_e.html?hlyRange=%7C&dlyRange=1954-10-01%7C1955-03-31&mlyRange=1954-01-01%7C1955-12-01&StationID=4346&Prov=ON&urlExtension=_e.html&searchType=stnProx&optLimit=yearRange&Month=3&Day=26&StartYear=1840&EndYear=2021&Year=1955&selRowPerPage=25&Line=2&txtRadius=25&optProxType=city&selCity=45%7C26%7C75%7C42%7COttawa&selPark=&txtCentralLatDeg=&txtCentralLatMin=0&txtCentralLatSec=0&txtCentralLongDeg=&txtCentralLongMin=0&txtCentralLongSec=0&txtLatDecDeg=&txtLongDecDeg=&timeframe=2
## https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=4346&Year=1955&Month=3&Day=1&time=&timeframe=2&submit=Download+Data

# Create a function to read in the desired data in a robust way
readInData <- function(urlIn, altData, failDataOut) {
  outPutF <- tryCatch(
    {
      # Try to read from the website directly
      read.csv(url(urlIn))
    },
    error=function(cond) {
      # Error message contained in cond
      return(readDataOrRandom(altData, failDataOut))
    },
    warning=function(cond) {
      # Error message contained in cond
      return(readDataOrRandom(altData, failDataOut))
    },
    finally={
      # perform this regardless success or failure
    }
  )    
  return(outPutF)
}

# Create a fail catch for data read in
readDataOrRandom <- function(altData,failDataOut) {
  outPutF <- tryCatch(
    {
      # Try to read from the website directly
      read.csv(altData)
    },
    error=function(cond) {
      # Error message contained in cond
      return(failDataOut)
    },
    warning=function(cond) {
      # Error message contained in cond
      return(failDataOut)
    },
    finally={
      # perform this regardless success or failure
    }
  )    
  return(outPutF)
}

###################################################################################
###################################################################################

# Create User Interface
## Use [navbarPage] as opposed to [fluidPage and tabsetPanel]
ui <- 
  
  ## Personal Note
  ### Shiny apps have a sensitivity to multiple areas displaying the same plots
  ### There is also a sensitivity to having multiple ways to declare an input
  
  navbarPage(title = "Project 1",
             id = "masterPage",
             tabPanel(title = "Trends",
                      id = "trendPanel",
                      value = "trendPanel",
                      sidebarLayout(
                        sidebarPanel(
                          wellPanel(
                            radioButtons("plotType", "Plot Type:",
                                         c("Apparent Cases"="apparent",
                                           "Total Cases"="cases", 
                                           "Deaths"="deaths"))),
                          hr(),
                          wellPanel(
                            selectInput("populations", "Populations of Interest:",
                                        c("General Population" = 1,
                                          "Hospital Workers" = 2,
                                          "Hopsital Patients" = 3,
                                          "Long Term Care Workers" = 4,
                                          "Long Term Care Patients" = 5),
                                        selected = c(1,2,3,4,5),
                                        multiple = TRUE)),
                          hr(),
                          wellPanel(
                            selectInput("ageOfInterest", "Ages of Interest:",
                                        c("0 to 9 years" = 0,
                                          "10 to 19 years" = 1,
                                          "20 to 29 years" = 2,
                                          "30 to 39 years" = 3,
                                          "40 to 49 years" = 4,
                                          "50 to 59 years" = 5,
                                          "60 to 69 years" = 6,
                                          "70 to 79 years" = 7,
                                          "80 to 89 years" = 8,
                                          "90+ years"= 9),
                                        selected = c(0,1,2,3,4,5,6,7,8,9),
                                        multiple = TRUE)),
                          hr(),
                          wellPanel(
                            selectInput("sexSelect", "Select Sex:",
                                        c("Male" = 1,
                                          "Female" = 2),
                                        selected = c(1,2),
                                        multiple = TRUE))
                        ),
                        mainPanel(
                          wellPanel(
                            plotOutput("covidPlot"),
                            hr(),
                            actionButton("refreshPlot", "Refresh Plot"),
                            hr(),
                            p("Select the plots and categories of interest and then refresh the plot to explore the data"),
                            p("Apparent Cases: similar to the cases reported by public health units (i.e., the cases that we know of)"),
                            p("Total Cases: similar to the cases reported by public health units (i.e., all the cases in the community)"),
                            p("Deaths: Number of deaths attributed to COVID 19"),
                            p("Go to the simulator to see how to generate your own scenarios"),
                            hr(),
                            actionLink("link_to_sim_dash_pl", "Go to Simulator")
                          )
                        )
                      )
             ),
             navbarMenu(title = "Model Data",
                        tabPanel(title = "Vaccine Parameters",
                                 id = "stratPanel",
                                 value = "stratPanel",
                                 
                                 mainPanel(
                                   h1("Vaccine Parameters"),
                                   hr(),
                                   wellPanel(
                                     h2("Moderna"),
                                     hr(),
                                     numericInput("modernaGap", "Target Time Between Moderna Doses (Days):", 28,
                                                  min = 1),
                                     numericInput("modernaNum", "Number of Moderna Vaccinations (Doses/Day):", 1000000*0.0007,
                                                  min = 0),
                                     numericInput("modernaRes", "Maximum Number of Moderna Vaccinations to Reserve for Second Doses (Doses):", 1000000*0.0007,
                                                  min = 0),
                                     numericInput("modernaCRs", "Current Number of Moderna Vaccinations Stored (Doses):", 0,
                                                  min = 0),
                                     numericInput("modernaIm1", "Proportinate Immunity Provided By First Moderna Dose:", 0.8,
                                                  min = 0, max = 1),
                                     numericInput("modernaIm2", "Proportinate Immunity Provided By Second Moderna Dose:", 0.95,
                                                  min = 0, max = 1)),
                                   hr(),
                                   wellPanel(
                                     h2("Pfizer"),
                                     hr(),
                                     numericInput("pfizerGap", "Target Time Between Pfizer Doses (Days):", 21,
                                                  min = 1),
                                     numericInput("pfizerNum", "Number of Pfizer Vaccinations (Doses/Day):", 1000000*0.0015,
                                                  min = 0),
                                     numericInput("pfizerRes", "Maximum Number of Pfizer Vaccinations to Reserve for Second Doses (Doses):", 1000000*0.0015,
                                                  min = 0),
                                     numericInput("pfizerCRs", "Current Number of Pfizer Vaccinations Stored (Doses):", 0,
                                                  min = 0),
                                     numericInput("pfizerIm1", "Proportinate Immunity Provided By First Pfizer Dose:", 0.52,
                                                  min = 0, max = 1),
                                     numericInput("pfizerIm2", "Proportinate Immunity Provided By Second Pfizer Dose:", 0.95,
                                                  min = 0, max = 1)
                                   ),
                                   hr(),
                                   actionLink("link_to_sim_dash_md", "Go to Simulator")
                                 )
                        ),
                        tabPanel(title = "Baseline Population",
                                 id = "popPanel",
                                 value = "popPanel",
                                 
                                 mainPanel(
                                   h1("Population Variables"),
                                   hr(),
                                   wellPanel(
                                     h2("Global Simulation Variables"),
                                     hr(),
                                     numericInput("populationSize", "Size of Population (People):", 1000000,
                                                  min = 1)),
                                   hr(),
                                   wellPanel(
                                     h2("Population Proportions"),
                                     hr(),
                                     numericInput("propR", "Initial Known Infection Population Proporition:", 0.01860156,
                                                  min = 0, max = 1),
                                     numericInput("propE", "Initial Exposed Population Proporition:", 0.0004481673,
                                                  min = 0, max = 1),
                                     numericInput("propI", "Initial Infectious Population Proporition:", 0.0004447721,
                                                  min = 0, max = 1)
                                   ),
                                   hr(),
                                   wellPanel(
                                     h2("Population Distribution"),
                                     numericInput(
                                       inputId = "pop1Proportion",
                                       label = "General Population:",
                                       min = 0,
                                       max = 1,
                                       value = 1 - (106717 + 20000 + 106717 + 115000)/14733199,
                                     ),
                                     numericInput(
                                       inputId = "pop2Proportion",
                                       label = "Hospital Workers:",
                                       min = 0,
                                       max = 1,
                                       value = 106717/14733199,
                                     ),
                                     numericInput(
                                       inputId = "pop3Proportion",
                                       label = "Hospital Patients:",
                                       min = 0,
                                       max = 1,
                                       value = 20000/14733199,
                                     ),
                                     numericInput(
                                       inputId = "pop4Proportion",
                                       label = "Long Term Care Workers:",
                                       min = 0,
                                       max = 1,
                                       value = 106717/14733199,
                                     ),
                                     numericInput(
                                       inputId = "pop5Proportion",
                                       label = "Long Term Care Patients:",
                                       min = 0,
                                       max = 1,
                                       value = 115000/14733199,
                                     )
                                   ),
                                   hr(),
                                   actionLink("link_to_sim_dash_pd", "Go to Simulator")
                                 )
                        )
             ),
             navbarMenu(title = "Population Specific Variables",
                        tabPanel(title = "General Population Set Up",
                                 id = "pop1Panel",
                                 value = "pop1Panel",
                                 mainPanel(
                                   h1("General Population"),
                                   hr(),
                                   wellPanel(
                                     h2("Infection Dynamics:"),
                                     hr(),
                                     p("Characteristics of an infection in the General Population"),
                                     hr(),
                                     numericInput("r_value_1", "Number of Infections Generated by Infected Members:", 0.9,
                                                  min = 0),
                                     numericInput("latencyTime_1", "Average Time Between Exposure and Infectiousness (days):", 5.5,
                                                  min = 0.01),
                                     numericInput("removalTime_1", "Average Time of Infectiousness (days):", 5,
                                                  min = 0.01)
                                   ),
                                   hr(),
                                   wellPanel(
                                     h2("Susceptibility:"),
                                     hr(),
                                     p("Susceptibility of Various Population Members after exposure to an infection in the General Population"),
                                     hr(),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix11",
                                       label = "General Population:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix12",
                                       label = "Hospital Workers:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix13",
                                       label = "Hospital Patients:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix14",
                                       label = "Long Term Care Workers:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix15",
                                       label = "Long Term Care Patients:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     )
                                   ),
                                   hr(),
                                   actionLink("link_to_sim_dash_gp", "Go to Simulator")
                                 )),
                        tabPanel(title = "Hospital Workers Set Up",
                                 id = "pop2Panel",
                                 value = "pop2Panel",
                                 mainPanel(
                                   h1("Hospital Workers"),
                                   hr(),
                                   wellPanel(
                                     h2("Infection Dynamics:"),
                                     hr(),
                                     p("Characteristics of an infection in Hospital Workers"),
                                     hr(),
                                     numericInput("r_value_2", "Number of Infections Generated by Infected Members:", 0.9,
                                                  min = 0),
                                     numericInput("latencyTime_2", "Average Time Between Exposure and Infectiousness:", 5.5,
                                                  min = 0.01),
                                     numericInput("removalTime_2", "Average Time of Infectiousness:", 5,
                                                  min = 0.01)
                                     #numericInput("doseDistSingle", "Average Time of Infectiousness:", 0.16,
                                     #             min = 0.01)
                                   ),
                                   hr(),
                                   wellPanel(
                                     h2("Susceptibility:"),
                                     hr(),
                                     p("Susceptibility of Various Population Members after exposure to an infection in Hospital Workers"),
                                     hr(),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix21",
                                       label = "General Population:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix22",
                                       label = "Hospital Workers:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix23",
                                       label = "Hospital Patients:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix24",
                                       label = "Long Term Care Workers:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix25",
                                       label = "Long Term Care Patients:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     )
                                   ),
                                   hr(),
                                   actionLink("link_to_sim_dash_hw", "Go to Simulator")
                                 )),
                        tabPanel(title = "Hospital Patients Set Up",
                                 id = "pop3Panel",
                                 value = "pop3Panel",
                                 mainPanel(
                                   h1("Hospital Patients"),
                                   hr(),
                                   wellPanel(
                                     h2("Infection Dynamics:"),
                                     hr(),
                                     p("Characteristics of an infection in Hospital Patients"),
                                     hr(),
                                     numericInput("r_value_3", "Number of Infections Generated by Infected Members:", 0.9,
                                                  min = 0),
                                     numericInput("latencyTime_3", "Average Time Between Exposure and Infectiousness:", 5.5,
                                                  min = 0.01),
                                     numericInput("removalTime_3", "Average Time of Infectiousness:", 5,
                                                  min = 0.01)
                                   ),
                                   hr(),
                                   wellPanel(
                                     h2("Susceptibility:"),
                                     hr(),
                                     p("Susceptibility of Various Population Members after exposure to an infection in Hospital Patients"),
                                     hr(),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix31",
                                       label = "General Population:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix32",
                                       label = "Hospital Workers:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix33",
                                       label = "Hospital Patients:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix34",
                                       label = "Long Term Care Workers:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix35",
                                       label = "Long Term Care Patients:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     )
                                   ),
                                   hr(),
                                   actionLink("link_to_sim_dash_hp", "Go to Simulator")
                                 )),
                        tabPanel(title = "Long Term Care Workers Set Up",
                                 id = "pop4Panel",
                                 value = "pop4Panel",
                                 mainPanel(
                                   h1("Long Term Care Workers"),
                                   hr(),
                                   wellPanel(
                                     h2("Infection Dynamics:"),
                                     hr(),
                                     p("Characteristics of an infection in Long Term Care Workers"),
                                     hr(),
                                     numericInput("r_value_4", "Number of Infections Generated by Infected Members:", 0.9,
                                                  min = 0),
                                     numericInput("latencyTime_4", "Average Time Between Exposure and Infectiousness:", 5.5,
                                                  min = 0.01),
                                     numericInput("removalTime_4", "Average Time of Infectiousness:", 5,
                                                  min = 0.01)
                                   ),
                                   hr(),
                                   wellPanel(
                                     h2("Susceptibility:"),
                                     hr(),
                                     p("Susceptibility of Various Population Members after exposure to an infection in Long Term Care Workers"),
                                     hr(),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix41",
                                       label = "General Population:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix42",
                                       label = "Hospital Workers:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix43",
                                       label = "Hospital Patients:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix44",
                                       label = "Long Term Care Workers:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix45",
                                       label = "Long Term Care Patients:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     )
                                   ),
                                   hr(),
                                   actionLink("link_to_sim_dash_lw", "Go to Simulator")
                                 )),
                        tabPanel(title = "Long Term Care Patients Set Up",
                                 id = "pop5Panel",
                                 value = "pop5Panel",
                                 mainPanel(
                                   h1("Long Term Care Patients"),
                                   hr(),
                                   wellPanel(
                                     h2("Infection Dynamics:"),
                                     hr(),
                                     p("Characteristics of an infection in Long Term Care Patients"),
                                     hr(),
                                     numericInput("r_value_5", "Number of Infections Generated by Infected Members:", 0.9,
                                                  min = 0),
                                     numericInput("latencyTime_5", "Average Time Between Exposure and Infectiousness:", 5.5,
                                                  min = 0.01),
                                     numericInput("removalTime_5", "Average Time of Infectiousness:", 5,
                                                  min = 0.01)
                                   ),
                                   hr(),
                                   wellPanel(
                                     h2("Susceptibility:"),
                                     hr(),
                                     p("Susceptibility of Various Population Members after exposure to an infection in Long Term Care Patients"),
                                     hr(),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix51",
                                       label = "General Population:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix52",
                                       label = "Hospital Workers:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix53",
                                       label = "Hospital Patients:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix54",
                                       label = "Long Term Care Workers:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     ),
                                     sliderInput(
                                       inputId = "susceptibilityMatrix55",
                                       label = "Long Term Care Patients:",
                                       min = 0,
                                       max = 1,
                                       value = 1,
                                     )
                                   ),
                                   hr(),
                                   actionLink("link_to_sim_dash_lp", "Go to Simulator")
                                 ))
             ),
             tabPanel(title = "Vaccine Strategy",
                      id = "strategyPanel",
                      value = "strategyPanel",
                      h1("Vaccine Strategy"),
                      hr(),
                      wellPanel(
                        h2("Dose Division"),
                        hr(),
                        sliderInput(
                          inputId = "doseProp1",
                          label = "General Population:",
                          min = 0,
                          max = 1,
                          value = 1,
                        ),
                        sliderInput(
                          inputId = "doseProp2",
                          label = "Hospital Workers:",
                          min = 0,
                          max = 1,
                          value = 1,
                        ),
                        sliderInput(
                          inputId = "doseProp3",
                          label = "Hospital Patients:",
                          min = 0,
                          max = 1,
                          value = 1,
                        ),
                        sliderInput(
                          inputId = "doseProp4",
                          label = "Long Term Care Workers:",
                          min = 0,
                          max = 1,
                          value = 1,
                        ),
                        sliderInput(
                          inputId = "doseProp5",
                          label = "Long Term Care Patients:",
                          min = 0,
                          max = 1,
                          value = 1,
                        )
                      ),
                      hr(),
                      wellPanel(
                        h2("Population to Prioritize"),
                        hr(),
                        selectInput("ageToPrioritize", "Ages of Interest:",
                                    c("0 to 9 years" = 0,
                                      "10 to 19 years" = 1,
                                      "20 to 29 years" = 2,
                                      "30 to 39 years" = 3,
                                      "40 to 49 years" = 4,
                                      "50 to 59 years" = 5,
                                      "60 to 69 years" = 6,
                                      "70 to 79 years" = 7,
                                      "80 to 89 years" = 8,
                                      "90+ years"= 9),
                                    selected = c(0,1,2,3,4,5,6,7,8,9),
                                    multiple = TRUE)
                      ),
                      hr(),
                      actionLink("link_to_sim_dash_vs", "Go to Simulator")
             ),
             tabPanel(title = "Dash Board",
                      id = "simDashPanel",
                      value = "simDashPanel",
                      h1("Simulation Dashboard"),
                      hr(),
                      wellPanel(
                        h2("Table Selection / Manipulation"),
                        hr(),
                        wellPanel(
                          p("To adjust the starting tables for the simulation visit the ''Baseline Population'' tab under the ''Model Data'' drop down menu"),
                          p("''Reset'' will create a new table based on your selected baseline population values"),
                          p("''Clone to Other Table'' will clone the current table indicated in the ''Fate Table Select'' selection box to the table not selected"),
                          p("All presets were dervied using Ontario data as outlined in the ''Model Development / Assumptions'' tab under the ''About'' drop down menu")),
                        hr(),
                        selectInput("fateTableSelect", "Fate Table Select:",
                                    c("Primary Table" = 1,
                                      "Secondary Table" = 2),
                                    multiple = FALSE),
                        fluidRow(
                          actionButton("reset", "Reset"),
                          actionButton("clone", "Clone to Other Table")
                        )),
                      hr(),
                      wellPanel(
                        h2("Simulation Function"),
                        hr(),
                        wellPanel(
                          p("To adjust the simulation visit the ''Vaccine Parameters'' tab under the ''Model Data'' drop down menu as well as the ''Population Specific Variables'' related tabs and the ''Vaccine Strategy'' tab"),
                          p("''Simulate'' will progress the simulation associated with the ''Fate Table Select'' selection by the indicated ''Duration of Simulation'' using your selected variables")),
                        hr(),
                        fluidRow(
                          numericInput("dayInSimulation", "Duration of Simulation (Days):", 180,
                                       min = 1),
                          actionButton("runSim", "Simulate")
                          
                        )),
                      tags$hr(),
                      wellPanel(
                        h2("Selected Fate Table Data"),
                        hr(),
                        p("Note that the ''age'' column displays the decade group of the patient (e.g., an ''age'' of 0 indicates that the patient is 0 to 9 years of age which corresponds to their mortailty probability after adjusting for on sex"),
                        hr(),
                        DT::dataTableOutput("mod_table")
                      )
             ),
             navbarMenu(title = "About",
                        tabPanel(title = "Model Development / Assumptions",
                                 id = "assuptionsPanel",
                                 value = "assuptionsPanel",
                                 htmlOutput("incAssumptionDetails")
                        ),
                        tabPanel(title = "About",
                                 id = "aboutPanel",
                                 value = "aboutPanel",
                                 h1("About"),
                                 hr(),
                                 wellPanel(
                                   h2("About the Author:"),
                                   hr(),
                                   fluidRow(
                                     column(11, offset = 1,
                                            h3("John Brooks B.Eng.Mgmt"),
                                            p("John graduated from McMaster University in 2008 with a degree in Engineering in Management. He spent 12 years to complete his medical training and returned to academia in 2020 as Master's Student in the Department of Epidemiology at the University of Ottawa.")
                                     ))),
                                 wellPanel(
                                   h2("Where to Find the Code:"),
                                   hr(),
                                   fluidRow(
                                     column(11, offset = 1,
                                            tags$a(href="https://github.com/johnabrooks/STAT5702_Project-1.git", 
                                                   "Source code on Git Hub")
                                     )))
                        )
             )
  )

###################################################################################
###################################################################################

# Create Server functions
server <- 
  function(input, output, session) {
    # Convenience Values for Initialization
    populationSizeInit = 1000000
    location.roleInit = 1:5
    dayInSimulationInit = 10
    
    ## Create a distribution based on known Ontario Data
    findBin <- function(valueIn){
      male <- c(70270.5 + 300151.0 + 389090.5, 402140.5 + 439014.0, 542049.0 + 547777.0, 517712.5 + 484729.0, 444531.0 + 457971.0, 484932.0 + 531677.0, 463751.0 + 377606.0, 310342.0 + 208024.5, 138291.5 + 81227.0, 40799.0)/14639366
      female <- c(70270.5 + 281151.5 + 373686.0, 387816.0 + 417256.5, 494176.5 + 514795.0, 507079.5 + 493702.0, 472968.0 + 478242.5, 495134.0 + 539452.0, 486012.0 + 413875.5, 348756.0 + 244647.0, 177221.5 + 121691.5, 89346.5)/14639366
      reference <- cumsum(c(male,female))
      return(sum(reference < valueIn))
    }
    
    modelValue <- reactiveValues(
      daysPerGeneration = 1,
      dayInSimulation = dayInSimulationInit,
      populationSize = populationSizeInit,
      location.role = location.roleInit,
      population.distribution = c(
        1 - (106717 + 20000 + 106717 + 115000)/14733199,
        106717/14733199,
        20000/14733199,
        106717/14733199,
        115000/14733199
      ),
      modernaGap = 42,
      pfizerGap = 42,
      dosesPerDay = populationSizeInit*c(0.0007,0.0015),
      maxReserve = populationSizeInit*c(0.0007,0.0015),
      currentVaccineReserve = c(0,0),
      firstDoseImmunity = c(0.8,0.52),
      secondDoseImmunity = c(0.95,0.95),
      propS = 0.9805055,
      propE = 0.0004481673,
      propI = 0.0004447721,
      r_value = rep(0.9,length(location.roleInit)),
      latencyTime = rep(5.5,length(location.roleInit)),
      removalTime = rep(5,length(location.roleInit)),
      dose.distribution = c(
        0.16,
        0.16,
        0.16,
        0.16,
        0.16,
        0.2
      ),
      susceptibility.matrix = matrix(
        rep(1,length(location.roleInit)^2),
        ncol = length(location.roleInit),
        nrow = length(location.roleInit)),
      ageToPrioritize = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    )
    
    isolate(modelValueLoad <- modelValue)
    
    # Create shells for fate tables
    ## Fate table for a stored outcome
    storedFateTable <- reactiveValues(values = readRDS("sampleStoreFateTable.Rds"))
    
    # Create shells for fate tables
    ## Fate table for a stored outcome to edit
    ### Isolate blocks the formation of a reactive relationship
    isolate(storedFateTableDisplay <- storedFateTable)
    
    ## Fate table that is being used
    masterFateTable <- reactiveValues(values = readRDS("sampleFateTable.Rds"))
    
    ## Fate table that has been rearranged
    isolate(masterFateTableDisplay <- masterFateTable)
    
    #
    createFateTable <- reactive({
      
      # This is solely to create an invalidating trigger
      input$reset
      
      withProgress(message = 'Generating Table:', value = 0, {
        
        populationSizeIn = modelValue$populationSize
        location.roleIn = modelValue$location.role
        population.distribution = modelValue$population.distribution
        propS = modelValue$propS
        propE = modelValue$propE
        propI = modelValue$propI
        
        # Set up fate table
        fate<-list()
        
        # Create User Inputs
        userInputs<-list()
        
        # Population Information
        ## General
        userInputs$populationSize <- ceiling(populationSizeIn)
        
        ## Roles in the community
        ## 1 = Community
        ## 2 = Hospital worker with patient contact 
        ## 3 = Hospital patient
        ## 4 = Long term care (LTC) worker 
        ## 5 = Long term care (LTC) patient 
        userInputs$location.role <- floor(location.roleIn)
        population.distribution <- population.distribution
        
        ## Population proportions
        userInputs$propS <- propS
        userInputs$propE <- propE
        userInputs$propI <- propI
        
        initVector <- rep(FALSE,userInputs$populationSize)
        initVectorNa <- rep(NA,userInputs$populationSize)
        values<-data.frame(id.Number=1:userInputs$populationSize,
                           age=initVectorNa,
                           sexMale=initVector,
                           location.role=rep(1,userInputs$populationSize),
                           suceptible=initVector,
                           exposed=initVector,
                           exposedDate=initVectorNa,
                           infectious=initVector,
                           infectiousDate=initVectorNa,
                           removed=initVector,
                           removedDate=initVectorNa,
                           dead=initVector,
                           death.Residual=initVectorNa,
                           death.Probability=rep(0,userInputs$populationSize),
                           moderna.1.date=initVectorNa,
                           moderna.1.logical=initVector,
                           moderna.2.date=initVectorNa,
                           moderna.2.logical=initVector,
                           pfizer.1.date=initVectorNa,
                           pfizer.1.logical=initVector,
                           pfizer.2.date=initVectorNa,
                           pfizer.2.logical=initVector,
                           susceptibility.factor=rep(1,userInputs$populationSize))
        
        fate$table <- values
        
        incProgress(1/3, detail = paste("Demographics"))
        
        # Assign age and sex
        ## Generate the age sex specifying random number
        ageSexRand <- runif(userInputs$populationSize)
        
        incProgress(1/3, detail = paste("Demographics (Warning - Longer Step)"))
        ## Find in which bins the random numbers sit
        inBins <- sapply(ageSexRand, findBin)
        isMale <- inBins < 10
        ageDec <- inBins
        ageDec[!isMale] <- ageDec[!isMale] - 10
        
        # We will keep only the decile number for simplicity
        # ageDec <- 10*ageDec
        
        # Store
        fate$table$age <- ageDec
        fate$table$sexMale <- isMale
        
        incProgress(1/3, detail = paste("Mortality Probabilities"))
        
        # bins are from 0 to 19 and proper mortality probabilities need to be decoded
        # Create reference
        maleDeathProb <- c(0, 7.874016e-05, 1.635323e-04, 5.873582e-04, 1.943215e-03, 6.291238e-03,
                           2.706689e-02, 1.055945e-01, 2.495656e-01, 3.644860e-01, 2.291354e-02)
        
        femaleDeathProb <- c(0.0001737619, 0, 0.0001100271, 0.0002319432, 0.0007652573, 0.0035497431,
                             0.0152793252, 0.0662522955, 0.1638440860, 0.2565483931, 0.0238348434)
        
        referenceProb <- c(maleDeathProb,femaleDeathProb)
        fate$table$death.Probability <- referenceProb[inBins+1]
        
        incProgress(1/3, detail = paste("Divide Population"))
        # Initialize infection cohort
        # Create draw vector for fates:
        initialFateVector <- runif(userInputs$populationSize)
        
        # Assess thresholds
        ThresholdS <- quantile(initialFateVector,(userInputs$propS))
        ThresholdE <- quantile(initialFateVector,(userInputs$propS + userInputs$propE))
        ThresholdI <- quantile(initialFateVector,(userInputs$propS + userInputs$propE + userInputs$propI))
        
        # Assign fates
        fate$table$suceptible <- (initialFateVector <= ThresholdS)
        fate$table$exposed    <- (initialFateVector <= ThresholdE)&(initialFateVector > ThresholdS) 
        fate$table$infectious <- (initialFateVector <= ThresholdI)&(initialFateVector > ThresholdE)
        fate$table$removed[!fate$table$suceptible&!fate$table$exposed&!fate$table$infectious] <- TRUE
        
        # People outside the S group are impervious
        fate$table$susceptibility.factor[!fate$table$suceptible] <- 0
        
        # Those not susceptible are set by their status to day 1
        fate$table$exposedDate[fate$table$exposed|fate$table$infectious|fate$table$removed] <- 1
        fate$table$infectiousDate[fate$table$infectious|fate$table$removed] <- 1
        fate$table$removedDate[fate$table$removed] <- 1
        
        # Population division
        role.selections.rand <- runif(userInputs$populationSize)
        lower.bound <- population.distribution[1]
        higher.bound <- population.distribution[1]
        
        for (current.location.role in 2:length(userInputs$location.role)) {
          higher.bound <- higher.bound + population.distribution[current.location.role]
          fate$table$location.role[(role.selections.rand > lower.bound) & (role.selections.rand <= higher.bound)] <-
            current.location.role
          lower.bound <- higher.bound
        }
        
        fate$generationLog <- 1
        
        return(fate)
      })
    })
    
    # Create Simulation function
    covid19Simulate <- reactive({
      
      # Create invalidation
      input$runSim
      
      withProgress(message = 'Simulating:', value = 0, {
        
        # Get priority sequence
        ageToPrioritize = modelValue$ageToPrioritize
        
        # Set up fate table (Needed for Betas)
        fate<-list()
        
        isolate(
          if(input$fateTableSelect == 1){
            fate$table <- rbind(masterFateTable$values$table[masterFateTable$values$table$age %in% ageToPrioritize,],
                                masterFateTable$values$table[!(masterFateTable$values$table$age %in% ageToPrioritize),])
            fate$generationLog <- masterFateTable$values$generationLog
          } else {
            fate$table <- rbind(storedFateTable$values$table[storedFateTable$values$table$age %in% ageToPrioritize,],
                                storedFateTable$values$table[!(storedFateTable$values$table$age %in% ageToPrioritize),])
            fate$generationLog <- storedFateTable$values$generationLog
          })
        
        fate$table$id.Number <- 1:length(fate$table$id.Number)
        
        # Variable lists
        daysPerGeneration = modelValue$daysPerGeneration
        dayInSimulation = modelValue$dayInSimulation
        location.role = modelValue$location.role
        modernaGap = modelValue$modernaGap
        pfizerGap = modelValue$pfizerGap
        dosesPerDay = modelValue$dosesPerDay
        maxReserve = modelValue$maxReserve
        currentVaccineReserve = modelValue$currentVaccineReserve
        firstDoseImmunity = modelValue$firstDoseImmunity
        secondDoseImmunity = modelValue$secondDoseImmunity
        r_value = modelValue$r_value
        latencyTime = modelValue$latencyTime
        removalTime = modelValue$removalTime
        dose.distribution = modelValue$dose.distribution
        
        # The susceptibility.matrix[i,j] is equal to the susceptibility of a j type individual to an exposure from a person in group i
        # modelValue$susceptibility.matrix
        
        # Initialize
        userInputs <- list()
        model.variables <- list()
        
        # Bring susceptibility information in
        model.variables$susceptibility.matrix <- modelValue$susceptibility.matrix
        
        # Simulation Information 
        userInputs$generations <- ceiling(dayInSimulation/daysPerGeneration)
        
        # Vaccine Information
        userInputs$turns.btw.doses[1] <- modernaGap/daysPerGeneration
        userInputs$turns.btw.doses[2] <- pfizerGap/daysPerGeneration
        userInputs$does.per.turn[1] <- floor(dosesPerDay[1]*daysPerGeneration)
        userInputs$does.per.turn[2] <- floor(dosesPerDay[2]*daysPerGeneration)
        userInputs$max.reserve[1] <- floor(maxReserve[1])
        userInputs$max.reserve[2] <- floor(maxReserve[2])
        userInputs$current.vaccine.reserve[1] <- floor(currentVaccineReserve[1])
        userInputs$current.vaccine.reserve[2] <- floor(currentVaccineReserve[2])
        
        userInputs$suceptibility.bonus <- list()
        userInputs$suceptibility.bonus[[1]] <- c(1-firstDoseImmunity[1], 
                                                 (1-secondDoseImmunity[1])/(1-firstDoseImmunity[1]))
        userInputs$suceptibility.bonus[[2]] <- c(1-firstDoseImmunity[2], 
                                                 (1-secondDoseImmunity[2])/(1-firstDoseImmunity[2]))
        
        userInputs$dose.distribution <- dose.distribution
        
        ## Order for future repeat use
        userInputs$dose.distribution.order <- 
          order(userInputs$dose.distribution[1:(length(userInputs$dose.distribution)-1)], decreasing = TRUE)
        
        # Model Variables
        model.variables$r_value <- r_value
        model.variables$latencyTime <- latencyTime/daysPerGeneration
        model.variables$removalTime <- removalTime/daysPerGeneration
        
        # Location.role is a critical variable 
        model.variables$location.role <- floor(location.role)
        
        ## Compute baseline values
        model.variables$alpha <- 1/model.variables$latencyTime
        model.variables$gamma <- 1/model.variables$removalTime
        
        ## Compute Betas for each population context
        ### Note that susceptibility per se is the susceptibility of the aggregate population target
        for(currentLocationRoleI in model.variables$location.role){
          suceptibilityVectorLookup <- model.variables$susceptibility.matrix[currentLocationRoleI,]
          rawSuceptibility <- suceptibilityVectorLookup[fate$table$location.role]*fate$table$susceptibility.factor
          propSu <- sum(rawSuceptibility)/length(rawSuceptibility)
          if(propSu == 0){
            model.variables$beta[currentLocationRoleI] <- 0
          } else {
            model.variables$beta[currentLocationRoleI] <- 
              model.variables$r_value[currentLocationRoleI]*
              (1/propSu)*
              model.variables$gamma[currentLocationRoleI]
          }
        }
        
        ###### Simulator
        for(currentGeneration in (fate$generationLog+1):(fate$generationLog+userInputs$generations)) {
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/(userInputs$generations + 1), detail = paste("Generation", (currentGeneration)))
          
          fate$generationLog <- currentGeneration
          
          # Set the generation number
          N.generation <- 
            sum(fate$table$suceptible) +
            sum(fate$table$exposed[currentGeneration]) +
            sum(fate$table$infectious[currentGeneration]) +
            sum(fate$table$removed[currentGeneration]) 
          
          # Create a fate table copy to edit
          new.fate <- fate$table
          
          # Process new infections (note someone may get two chances to get infected which is permissible)
          for (current.location.role in model.variables$location.role) {
            size.of.population <- sum(fate$table$infectious[fate$table$location.role==current.location.role])
            current.Throws <- rpois(
              1,size.of.population*model.variables$beta[current.location.role])
            
            #### Could we adapt this for speed
            throw.Values <- runif(current.Throws) * N.generation
            select.Indicies <- ceiling(throw.Values)
            infection.chance <- select.Indicies - throw.Values
            
            infection.threshold <- model.variables$susceptibility.matrix[
              current.location.role,
              fate$table$location.role[select.Indicies]] *
              fate$table$susceptibility.factor[select.Indicies]
            
            potential.death.chance <- (infection.chance + infection.threshold - 1)*(1/infection.threshold)
            # may need to handle 0 divided by 0 situation
            
            infections.sustained <- potential.death.chance > 0
            indicies.of.new.infections <- select.Indicies[infections.sustained]
            
            potential.death.residual <- (
              fate$table$death.Probability[indicies.of.new.infections] - 
                potential.death.chance[infections.sustained]) * 
              (1/(1-fate$table$death.Probability[indicies.of.new.infections]))
            # may need to handle 0 divided by 0 situation
            
            death.sustained <- potential.death.residual > 0 
            
            indicies.of.death <- indicies.of.new.infections[death.sustained]
            
            # Update to the new infection to exposed and nullify other categories
            new.fate$exposed[indicies.of.new.infections] <- TRUE
            new.fate$infectious[indicies.of.new.infections] <- FALSE
            new.fate$suceptible[indicies.of.new.infections] <- FALSE
            new.fate$removed[indicies.of.new.infections] <- FALSE
            
            # Log successful exposure date
            new.fate$exposedDate[indicies.of.new.infections] <- currentGeneration
            
            # Patients acutely infected are considered to be impervious to further infection
            new.fate$susceptibility.factor[indicies.of.new.infections] <- 0
            
            # Decide death
            new.fate$dead[indicies.of.death] <- TRUE
            new.fate$death.Residual[indicies.of.death] <- potential.death.residual[death.sustained]
          }
          
          # Migrate infections (alternative would have been Bernoulli trials but slow for large size)
          indicies.of.population.of.interest  <- fate$table$id.Number
          indicies.of.population.of.interest <- indicies.of.population.of.interest[fate$table$exposed]
          size.of.population <- length(indicies.of.population.of.interest)
          migration.variable <- model.variables$alpha[current.location.role]
          
          # Migration from E to I
          current.Throws <- rpois(1,size.of.population*migration.variable)
          current.Throws <- min(current.Throws, size.of.population)
          
          # Select indicies
          select.Indicies <- c()
          
          # Avoid double selection
          throw.Rands <- runif(current.Throws) 
          for(rand.Index in 1:length(throw.Rands)) {
            select.Indicies <- c(select.Indicies,ceiling(throw.Rands[rand.Index]*(size.of.population - rand.Index + 1)))
          }
          
          transfer.indicies <- indicies.of.population.of.interest[select.Indicies]
          
          # Update to the new infection to exposed and nullify other categories
          new.fate$exposed[transfer.indicies] <- FALSE
          new.fate$infectious[transfer.indicies] <- TRUE
          
          # Log transition to infectious date
          new.fate$infectiousDate[transfer.indicies] <- currentGeneration
          
          #######
          # Migration from I to R
          indicies.of.population.of.interest  <- fate$table$id.Number
          indicies.of.population.of.interest <- indicies.of.population.of.interest[fate$table$infectious]
          size.of.population <- length(indicies.of.population.of.interest)
          migration.variable <- model.variables$gamma[current.location.role]
          
          # Throws
          current.Throws <- rpois(1,size.of.population*migration.variable)
          current.Throws <- min(current.Throws, size.of.population)
          
          # Select indicies
          select.Indicies <- c()
          
          # Avoid double selection
          throw.Rands <- runif(current.Throws) 
          for(rand.Index in 1:length(throw.Rands)) {
            select.Indicies <- c(select.Indicies,ceiling(throw.Rands[rand.Index]*(size.of.population - rand.Index + 1)))
          }
          # Gather the indicies that need to indicate the transfer
          transfer.indicies <- indicies.of.population.of.interest[select.Indicies]
          
          # Update to the new infection to exposed and nullify other categories
          new.fate$infectious[transfer.indicies] <- FALSE
          new.fate$removed[transfer.indicies] <- TRUE
          
          # Log transition to removal date
          new.fate$removedDate[transfer.indicies] <- currentGeneration
          
          ###################
          # Set up vaccines #
          ###################
          
          if((userInputs$does.per.turn[[1]] > 0)&
             (userInputs$does.per.turn[[2]] > 0)) {
            # Assess demand
            need.vaccine.2 <- list()
            
            # Acceptable candidate (note that in this model you do not know the patient is infected until the are quarantined)
            potential.for.vaccination <- !fate$table$removed
            
            # General Second Dose Demand
            need.moderna.2 <- (currentGeneration - fate$table$moderna.1.date) >=
              userInputs$turns.btw.doses[1]
            need.moderna.2[is.na(need.moderna.2)] <- FALSE
            need.moderna.2 <- need.moderna.2&potential.for.vaccination&!fate$table$moderna.2.logical
            need.vaccine.2[[1]] <- need.moderna.2
            
            need.pfizer.2 <- (currentGeneration - fate$table$pfizer.1.date) >=
              userInputs$turns.btw.doses[2]
            need.pfizer.2[is.na(need.pfizer.2)] <- FALSE
            need.pfizer.2 <- need.pfizer.2&potential.for.vaccination&!fate$table$pfizer.2.logical
            need.vaccine.2[[2]] <- need.pfizer.2
            
            # General First Dose Demand
            need.vaccine.1 <- !(fate$table$moderna.1.logical|fate$table$pfizer.1.logical)&potential.for.vaccination
            
            # Explore demand
            demand.vaccine.1 <- list()
            demand.vaccine.2 <- list()
            demand.vaccine.2[[1]] <- list()
            demand.vaccine.2[[2]] <- list()
            
            demand.numeric.vaccine.1 <- c()
            demand.numeric.vaccine.2 <- list()
            demand.numeric.vaccine.2[[1]] <- rep(NA,length(model.variables$location.role))
            demand.numeric.vaccine.2[[2]] <- rep(NA,length(model.variables$location.role))
            
            for(current.location.role in model.variables$location.role) {
              base.logic <- 
                (fate$table$location.role==current.location.role)&potential.for.vaccination
              
              demand.vaccine.1[[current.location.role]]<-base.logic&need.vaccine.1
              demand.numeric.vaccine.1 <- c(
                demand.numeric.vaccine.1,
                sum(demand.vaccine.1[[current.location.role]]))
              
              demand.vaccine.2[[1]][[current.location.role]]<-base.logic&need.vaccine.2[[1]]
              demand.numeric.vaccine.2[[1]][current.location.role] <- 
                sum(demand.vaccine.2[[1]][[current.location.role]])
              
              demand.vaccine.2[[2]][[current.location.role]]<-base.logic&need.vaccine.2[[2]]
              demand.numeric.vaccine.2[[2]][current.location.role] <- 
                sum(demand.vaccine.2[[2]][[current.location.role]])
            }
            
            #Set up supply lists
            attempted.vaccine.distribution.this.turn <- list()
            overage.distribution.this.turn <- list()
            vaccines.this.turn <- list()
            
            # Assess Supply
            for(currenVaccineSelect in 1:2) {
              if(tail(userInputs$dose.distribution,1) < 1) {
                
                #First distribute the second vaccine as required
                doses.available <- userInputs$does.per.turn[[currenVaccineSelect]]
                
                vaccine.2.IDs <- fate$table$id.Number[need.vaccine.2[[currenVaccineSelect]]]
                
                #If 2nd vaccine demand overwhelms supply
                if(sum(demand.numeric.vaccine.2[[currenVaccineSelect]])>=doses.available) {
                  
                  # Compute the potential remaining reserve
                  potential.reserve <- userInputs$current.vaccine.reserve[[currenVaccineSelect]] +
                    doses.available -
                    sum(demand.numeric.vaccine.2[[currenVaccineSelect]])
                  
                  # If the reserve can cover
                  if(potential.reserve >= 0) {
                    userInputs$current.vaccine.reserve[[currenVaccineSelect]] <- potential.reserve
                  } else {
                    userInputs$current.vaccine.reserve[[currenVaccineSelect]] <- 0
                    number.to.vaccinate <- sum(demand.numeric.vaccine.2[[currenVaccineSelect]]) + potential.reserve
                    vaccine.2.IDs <- head(vaccine.2.IDs,number.to.vaccinate)
                  }
                  
                  # Vaccinate
                  if(currenVaccineSelect==1){
                    new.fate$moderna.2.date[vaccine.2.IDs] <- currentGeneration
                    new.fate$moderna.2.logical[vaccine.2.IDs] <- TRUE
                    new.fate$susceptibility.factor[vaccine.2.IDs] <-
                      new.fate$susceptibility.factor[vaccine.2.IDs]*userInputs$suceptibility.bonus[[currenVaccineSelect]][2]
                  } else {
                    new.fate$pfizer.2.date[vaccine.2.IDs] <- currentGeneration
                    new.fate$pfizer.2.logical[vaccine.2.IDs] <- TRUE
                    new.fate$susceptibility.factor[vaccine.2.IDs] <-
                      new.fate$susceptibility.factor[vaccine.2.IDs]*userInputs$suceptibility.bonus[[currenVaccineSelect]][2]
                  }
                } else {
                  # If demand is not overwhelmed then all of the related second level targets can be vaccinated
                  if(currenVaccineSelect==1){
                    new.fate$moderna.2.date[vaccine.2.IDs] <- currentGeneration
                    new.fate$moderna.2.logical[vaccine.2.IDs] <- TRUE
                    new.fate$susceptibility.factor[vaccine.2.IDs] <-
                      new.fate$susceptibility.factor[vaccine.2.IDs]*userInputs$suceptibility.bonus[[currenVaccineSelect]][2]
                  } else {
                    new.fate$pfizer.2.date[vaccine.2.IDs] <- currentGeneration
                    new.fate$pfizer.2.logical[vaccine.2.IDs] <- TRUE
                    new.fate$susceptibility.factor[vaccine.2.IDs] <-
                      new.fate$susceptibility.factor[vaccine.2.IDs]*userInputs$suceptibility.bonus[[currenVaccineSelect]][2]
                  }
                  
                  #change the doses available by subtracting from provided doses
                  doses.available <- doses.available - sum(demand.numeric.vaccine.2[[currenVaccineSelect]])
                  
                  vaccines.this.turn[[currenVaccineSelect]] <- c(
                    floor(doses.available*userInputs$dose.distribution[[1]]),
                    floor(doses.available*userInputs$dose.distribution[[2]]),
                    floor(doses.available*userInputs$dose.distribution[[3]]),
                    floor(doses.available*userInputs$dose.distribution[[4]]),
                    floor(doses.available*userInputs$dose.distribution[[5]]),
                    ceiling(doses.available*userInputs$dose.distribution[[6]])
                  )
                  
                  # deal with rounding error
                  overage <- doses.available - sum(vaccines.this.turn[[currenVaccineSelect]])
                  
                  # Bizarre error due to computer rounding, sometimes ceiling will round up 1 entirely
                  # head by a negative number is the the head less the negative from the tail
                  if(overage < 0) {
                    vaccines.this.turn[[currenVaccineSelect]][length(vaccines.this.turn[[currenVaccineSelect]])] <- 
                      vaccines.this.turn[[currenVaccineSelect]][length(vaccines.this.turn[[currenVaccineSelect]])] - 1
                    residual.Overage <- 0
                  }
                  
                  userInputs$current.vaccine.reserve[[currenVaccineSelect]] <- 
                    userInputs$current.vaccine.reserve[[currenVaccineSelect]] +
                    vaccines.this.turn[[currenVaccineSelect]][length(vaccines.this.turn[[currenVaccineSelect]])]
                  
                  vaccines.this.turn[[currenVaccineSelect]][length(vaccines.this.turn[[currenVaccineSelect]])] <- 0
                  
                  # Deflate reserve as necessary to keep reserve from overflowing
                  if(userInputs$current.vaccine.reserve[[currenVaccineSelect]] > userInputs$max.reserve[[currenVaccineSelect]]) {
                    overage <- overage + userInputs$current.vaccine.reserve[[currenVaccineSelect]] -
                      userInputs$max.reserve[[currenVaccineSelect]]
                    userInputs$current.vaccine.reserve[[currenVaccineSelect]] <- userInputs$max.reserve[[currenVaccineSelect]]
                  }
                  
                  # Prepare category blocker
                  block.category <- rep(FALSE,length(model.variables$location.role))
                  
                  # Pre check and inflate overage as needed
                  for(check.location.role in model.variables$location.role){
                    if(vaccines.this.turn[[currenVaccineSelect]][check.location.role] >= 
                       demand.numeric.vaccine.1[[check.location.role]]){
                      block.category[check.location.role] <- TRUE
                      overage <- overage + vaccines.this.turn[[currenVaccineSelect]][check.location.role] - 
                        demand.numeric.vaccine.1[[check.location.role]]
                      vaccines.this.turn[[currenVaccineSelect]][check.location.role] <- 
                        demand.numeric.vaccine.1[[check.location.role]]
                    }
                  }
                  
                  if(sum(block.category)==length(model.variables$location.role)) {
                    userInputs$max.reserve[[currenVaccineSelect]] <- overage + userInputs$max.reserve[[currenVaccineSelect]]
                  } else {
                    # Need to handle condition where all vaccine is reserved
                    subweight <- 1- sum(userInputs$dose.distribution[c(block.category,TRUE)])
                    
                    # Distribute the overage 
                    overage.distribution.this.turn[[currenVaccineSelect]] <- c(
                      ceiling((!block.category[1])*overage*userInputs$dose.distribution[[currenVaccineSelect]]/subweight),
                      floor((!block.category[2])*overage*userInputs$dose.distribution[[currenVaccineSelect]]/subweight),
                      floor((!block.category[3])*overage*userInputs$dose.distribution[[currenVaccineSelect]]/subweight),
                      floor((!block.category[4])*overage*userInputs$dose.distribution[[currenVaccineSelect]]/subweight),
                      floor((!block.category[5])*overage*userInputs$dose.distribution[[currenVaccineSelect]]/subweight)
                    )
                    
                    # The most one could be off is approximately the number of location.roles that are unblocked
                    residual.Overage <- 
                      overage - sum(overage.distribution.this.turn[[currenVaccineSelect]])
                    
                    # Bizarre error due to computer rounding, sometimes ceiling will round up 1 entirely
                    # head by a negative number is the the head less the negative from the tail
                    if(residual.Overage < 0) {
                      overage.distribution.this.turn[[currenVaccineSelect]][1] <- overage.distribution.this.turn[[currenVaccineSelect]][1] - 1
                      residual.Overage <- 0
                    }
                    
                    # Distribute by unblocked weight order
                    number.slide <- 1:length(block.category)
                    
                    # Selection
                    distribute.select <- userInputs$dose.distribution.order[!userInputs$dose.distribution.order %in% c(number.slide[block.category],1)]
                    distribute.select.final <- head(distribute.select,residual.Overage)
                    
                    # Place
                    overage.distribution.this.turn[[currenVaccineSelect]][distribute.select.final] <- 
                      overage.distribution.this.turn[[currenVaccineSelect]][distribute.select.final] + 1
                    
                    adder <- vaccines.this.turn[[currenVaccineSelect]][1:(length(vaccines.this.turn[[currenVaccineSelect]])-1)]
                    
                    vaccination.selector <- overage.distribution.this.turn[[currenVaccineSelect]] + adder
                    
                    resultant.demand <- demand.numeric.vaccine.1 - vaccination.selector
                    
                    adjustment.vector <- resultant.demand < 0
                    
                    userInputs$current.vaccine.reserve[[currenVaccineSelect]] <- 
                      userInputs$current.vaccine.reserve[[currenVaccineSelect]] -
                      sum(resultant.demand[adjustment.vector])
                    
                    vaccination.selector <- resultant.demand*adjustment.vector + vaccination.selector
                    
                    # Find who to vaccinate (note that the population was randomly assigned their ID number 
                    # Hence taking the head is a random sample of patients
                    vaccine.1.IDs <- c()
                    for(select.location.role in 1:length(model.variables$location.role)) {
                      vaccine.1.IDs <- c(vaccine.1.IDs,
                                         head(fate$table$id.Number[demand.vaccine.1[[select.location.role]]],
                                              vaccination.selector[select.location.role]))
                      demand.vaccine.1[[select.location.role]][vaccine.1.IDs] <- FALSE
                    }
                    
                    # Apply vaccinations
                    if(currenVaccineSelect==1){
                      new.fate$moderna.1.date[vaccine.1.IDs] <- currentGeneration
                      new.fate$moderna.1.logical[vaccine.1.IDs] <- TRUE
                      new.fate$susceptibility.factor[vaccine.1.IDs] <-
                        new.fate$susceptibility.factor[vaccine.1.IDs]*userInputs$suceptibility.bonus[[currenVaccineSelect]][1]
                    } else {
                      new.fate$pfizer.1.date[vaccine.1.IDs] <- currentGeneration
                      new.fate$pfizer.1.logical[vaccine.1.IDs] <- TRUE
                      new.fate$susceptibility.factor[vaccine.1.IDs] <-
                        new.fate$susceptibility.factor[vaccine.1.IDs]*userInputs$suceptibility.bonus[[currenVaccineSelect]][1]
                    }
                    
                    # Change demand vector for other vaccines to be applied
                    demand.numeric.vaccine.1 <- demand.numeric.vaccine.1 - vaccination.selector
                  }
                }
              } else {
                # Everything goes to reserve
                userInputs$current.vaccine.reserve <- userInputs$current.vaccine.reserve + userInputs$does.per.turn
              }
            }
          }
          
          # Replace fate table in preparation for the next iteration
          fate$table <- new.fate
        }
        # Increment the progress bar, and update the detail text.
        incProgress(1/(userInputs$generations + 1), detail = paste("Finishing"))
      })
      
      return(fate)
    })
    
    # Plot Data Handling
    plotDataSelect <- reactiveValues(type = "apparent",
                                     ageOfInterest = c(0,1,2,3,4,5,6,7,8,9),
                                     sexSelect = c(1,2),
                                     populations = c(1,2,3,4,5))
    
    # Output COVID plot
    output$covidPlot <- renderPlot({
      withProgress(message = 'Generating Plot:', value = 0, {
        
        #masterFateTable <- covid19Simulate()
        #storedFateTable <- covid19Simulate()
        
        incProgress(1/7, detail = paste("Age Selection"))
        ageSelection <- as.numeric(plotDataSelect$ageOfInterest) 
        ageLogicMF <- masterFateTable$values$table$age %in% ageSelection
        ageLogicSF <- storedFateTable$values$table$age %in% ageSelection
        
        incProgress(1/7, detail = paste("Sex Selection"))
        sexSelection <- as.numeric(plotDataSelect$sexSelect)
        if(sum(sexSelection)==1){
          sexLogicMF <- masterFateTable$values$table$sexMale
          sexLogicSF <- storedFateTable$values$table$sexMale
        } else if(sum(sexSelection)==2){
          sexLogicMF <- !masterFateTable$values$table$sexMale 
          sexLogicSF <- !storedFateTable$values$table$sexMale 
        } else if(sum(sexSelection)==3){
          sexLogicMF <- rep(TRUE,length(masterFateTable$values$table$sexMale))
          sexLogicSF <- rep(TRUE,length(storedFateTable$values$table$sexMale))
        } else {
          sexLogicMF <- rep(FALSE,length(masterFateTable$values$table$sexMale))
          sexLogicSF <- rep(FALSE,length(storedFateTable$values$table$sexMale))
        }
        
        incProgress(1/7, detail = paste("Population Selection"))
        popSelection <- as.numeric(plotDataSelect$populations)
        popLogicMF <- masterFateTable$values$table$location.role %in% popSelection
        popLogicSF <- storedFateTable$values$table$location.role %in% popSelection
        
        selectLogicCompleteMF <- ageLogicMF&sexLogicMF&popLogicMF
        selectLogicCompleteSF <- ageLogicSF&sexLogicSF&popLogicSF
        
        incProgress(1/7, detail = paste("Selecting Plot Type"))
        if(plotDataSelect$type == "apparent"){
          baseDataForPlotMF <- masterFateTable$values$table$removedDate
          baseDataForPlotSF <- storedFateTable$values$table$removedDate
        } else if(plotDataSelect$type == "cases"){
          baseDataForPlotMF <- masterFateTable$values$table$exposedDate
          baseDataForPlotSF <- storedFateTable$values$table$exposedDate
        } else { 
          # For plotDataSelect$type == "deaths"
          baseDataForPlotMF <- masterFateTable$values$table$removedDate
          baseDataForPlotMF[!masterFateTable$values$table$dead] <- NA
          
          baseDataForPlotSF <- storedFateTable$values$table$removedDate
          baseDataForPlotSF[!storedFateTable$values$table$dead] <- NA
        }
        
        incProgress(1/7, detail = paste("Applying Selections"))
        refinedDataForPlotMF <- baseDataForPlotMF
        refinedDataForPlotMF[!selectLogicCompleteMF] <- NA
        
        refinedDataForPlotSF <- baseDataForPlotSF
        refinedDataForPlotSF[!selectLogicCompleteSF] <- NA
        
        # Create Skeletons
        ## Initialize plot vectors
        plotVectorMF <- rep(0,masterFateTable$values$generationLog)
        plotVectorSF <- rep(0,storedFateTable$values$generationLog)
        
        ## Main Frame
        plotSkeletonMF <- table(refinedDataForPlotMF)
        plotSkeletonGenerationsMF <- as.numeric(labels(plotSkeletonMF)[[1]])
        if(!is.null(dim(plotSkeletonMF))){
          plotVectorMF[plotSkeletonGenerationsMF] <- plotSkeletonMF
          plotVectorMF <- cumsum(plotVectorMF)
        }
        
        # Create Skeletons
        ## Stored Frame
        plotSkeletonSF <- table(refinedDataForPlotSF)
        plotSkeletonGenerationsSF <- as.numeric(labels(plotSkeletonSF)[[1]])
        if(!is.null(dim(plotSkeletonSF))){
          plotVectorSF[plotSkeletonGenerationsSF] <- plotSkeletonSF
          plotVectorSF <- cumsum(plotVectorSF)
        }
        
        incProgress(1/7, detail = paste("Plot Framing"))
        
        plot.frame.1 <- data.frame(Day = 1:masterFateTable$values$generationLog, 
                                   Counts = plotVectorMF[1:masterFateTable$values$generationLog],
                                   Simulation = "Primary")
        
        plot.frame.2 <- data.frame(Day = 1:storedFateTable$values$generationLog, 
                                   Counts = plotVectorSF[1:storedFateTable$values$generationLog],
                                   Simulation = "Secondary")
        
        plot.frame <- rbind(plot.frame.1,plot.frame.2)
        
        incProgress(1/7, detail = paste("Send To Screen"))
      })
      
      if(nrow(plot.frame)==2){
        ggplot(plot.frame, aes(x = Day, y = Counts, colour = Simulation)) +
          geom_point()
      } else {
        ggplot(plot.frame, aes(x = Day, y = Counts, colour = Simulation)) +
          geom_line(linetype = "solid")
      }
    })
    
    # UI Handling
    ## Refresh plot button
    observeEvent(input$refreshPlot, {
      plotDataSelect$type <- input$plotType
      plotDataSelect$ageOfInterest <- input$ageOfInterest
      plotDataSelect$sexSelect <- input$sexSelect
      plotDataSelect$populations <- input$populations
    })
    
    ## Reset the data table to a fresh slate button
    observeEvent(input$reset, {
      useDefaults <- FALSE
      
      isolate(
        if(!useDefaults){
          modelValue$populationSize <- abs(input$populationSize)
          modelValue$population.distribution <- abs(
            c(input$pop1Proportion,
              input$pop2Proportion,
              input$pop3Proportion,
              input$pop4Proportion,
              input$pop5Proportion))/sum(abs(
                c(input$pop1Proportion,
                  input$pop2Proportion,
                  input$pop3Proportion,
                  input$pop4Proportion,
                  input$pop5Proportion)
              ))
          
          if(sum(abs(c(input$propR,input$propE,input$propI))) > 1) {
            modelValue$propS <- 0
            modelValue$propE <- abs(input$propE)/sum(abs(c(input$propR,input$propE,input$propI)))
            modelValue$propI <- abs(input$propI)/sum(abs(c(input$propR,input$propE,input$propI)))
          } else {
            modelValue$propS <- 1 - sum(abs(c(input$propR,input$propE,input$propI)))
            modelValue$propE <- abs(input$propE)
            modelValue$propI <- abs(input$propI)
          }
        })
      
      isolate(
        if(input$fateTableSelect == 1){
          masterFateTable$values <- createFateTable()
        } else {
          storedFateTable$values <- createFateTable()
        })
    })
    
    ## Clone the data table to a fresh slate button
    observeEvent(input$clone, {
      isolate(
        if(input$fateTableSelect == 2){
          masterFateTable$values <- storedFateTable$values
        } else {
          storedFateTable$values <- masterFateTable$values
        })
    })
    
    ## Clone the data table to a fresh slate button
    observeEvent(input$runSim, {
      useDefaults <- FALSE
      isolate(
        if(!useDefaults){
          modelValue$dayInSimulation <- abs(input$dayInSimulation)
          modelValue$modernaGap <- abs(input$modernaGap)
          modelValue$pfizerGap <- abs(input$pfizerGap)
          
          modelValue$dosesPerDay = abs(c(input$modernaNum,input$pfizerNum))
          modelValue$maxReserve = abs(c(input$modernaRes,input$pfizerRes))
          modelValue$currentVaccineReserve = abs(c(input$modernaCRs,input$pfizerCRs))
          modelValue$firstDoseImmunity = abs(c(input$modernaIm1,input$pfizerIm1))
          modelValue$secondDoseImmunity = abs(c(input$modernaIm2,input$pfizerIm2))
          
          modelValue$ageToPrioritize <- as.numeric(input$ageToPrioritize)
          
          modelValue$r_value <- abs(c(input$r_value_1,
                                      input$r_value_2,
                                      input$r_value_3,
                                      input$r_value_4,
                                      input$r_value_5))
          
          modelValue$latencyTime <- abs(c(input$latencyTime_1,
                                          input$latencyTime_2,
                                          input$latencyTime_3,
                                          input$latencyTime_4,
                                          input$latencyTime_5))
          
          modelValue$removalTime <- abs(c(input$removalTime_1,
                                          input$removalTime_2,
                                          input$removalTime_3,
                                          input$removalTime_4,
                                          input$removalTime_5))
          
          if(sum(abs(c(input$doseProp1, input$doseProp2, input$doseProp3, input$doseProp4, input$doseProp5))) > 1) {
            modelValue$dose.distribution<- c(abs(c(input$doseProp1, input$doseProp2, input$doseProp3, input$doseProp4, input$doseProp5))/
                                               sum(abs(c(input$doseProp1, input$doseProp2, input$doseProp3, input$doseProp4, input$doseProp5))), 0)
          } else {
            modelValue$dose.distribution<- c(abs(c(input$doseProp1, input$doseProp2, input$doseProp3, input$doseProp4, input$doseProp5)),
                                             1 - sum(abs(c(input$doseProp1, input$doseProp2, input$doseProp3, input$doseProp4, input$doseProp5))))
            
          }
          
          modelValue$susceptibility.matrix[1,] <- c(input$susceptibilityMatrix11,
                                                    input$susceptibilityMatrix12,
                                                    input$susceptibilityMatrix13,
                                                    input$susceptibilityMatrix14,
                                                    input$susceptibilityMatrix15)
          modelValue$susceptibility.matrix[2,] <- c(input$susceptibilityMatrix21,
                                                    input$susceptibilityMatrix22,
                                                    input$susceptibilityMatrix23,
                                                    input$susceptibilityMatrix24,
                                                    input$susceptibilityMatrix25)
          modelValue$susceptibility.matrix[3,] <- c(input$susceptibilityMatrix31,
                                                    input$susceptibilityMatrix32,
                                                    input$susceptibilityMatrix33,
                                                    input$susceptibilityMatrix34,
                                                    input$susceptibilityMatrix35)
          modelValue$susceptibility.matrix[4,] <- c(input$susceptibilityMatrix41,
                                                    input$susceptibilityMatrix42,
                                                    input$susceptibilityMatrix43,
                                                    input$susceptibilityMatrix44,
                                                    input$susceptibilityMatrix45)
          modelValue$susceptibility.matrix[5,] <- c(input$susceptibilityMatrix51,
                                                    input$susceptibilityMatrix52,
                                                    input$susceptibilityMatrix53,
                                                    input$susceptibilityMatrix54,
                                                    input$susceptibilityMatrix55)
        }
      )
      isolate(
        if(input$fateTableSelect == 1){
          masterFateTable$values <- covid19Simulate()
        } else {
          storedFateTable$values <- covid19Simulate()
        })
    })
    
    # Load pages
    getPage<-function() {
      return(includeHTML("STAT-5702-P1-Equations-v4.html"))
    }
    output$incAssumptionDetails<-renderUI({getPage()})
    
    # Create an active data instance at the use phase
    ## Note that invalidation of the loading phase will regenerate the active data
    ## Invalidation of the active data will not however adjust the loaded data
    
    # Create a modifiable table
    output$mod_table <- DT::renderDataTable({
      # Use inactive copy and perpetually syncronize 
      ## allows for mor predictable table behaviour
      ## DT::datatable(values$activeData, editable = TRUE)
      if(input$fateTableSelect == 1){
        DT::datatable(masterFateTable$values$table, editable = FALSE)
      } else {
        DT::datatable(storedFateTable$values$table, editable = FALSE)
      }
    })
    
    # Create an instance of an editable data table
    proxyDataTable <- dataTableProxy("mod_table")
    
    # Observe inputs to the table
    observeEvent(input$mod_table_cell_edit, {
    })
    
    # Give a brief data summary
    output$summary <- renderPrint({
    })
    
    observeEvent(input$link_to_sim_dash_md, {
      updateNavbarPage(session, "masterPage", selected = "simDashPanel")
    })
    
    observeEvent(input$link_to_sim_dash_pd, {
      updateNavbarPage(session, "masterPage", selected = "simDashPanel")
    })
    
    observeEvent(input$link_to_sim_dash_pl, {
      updateNavbarPage(session, "masterPage", selected = "simDashPanel")
    })
    
    observeEvent(input$link_to_sim_dash_gp, {
      updateNavbarPage(session, "masterPage", selected = "simDashPanel")
    })
    
    observeEvent(input$link_to_sim_dash_hw, {
      updateNavbarPage(session, "masterPage", selected = "simDashPanel")
    })
    
    observeEvent(input$link_to_sim_dash_hp, {
      updateNavbarPage(session, "masterPage", selected = "simDashPanel")
    })
    
    observeEvent(input$link_to_sim_dash_lw, {
      updateNavbarPage(session, "masterPage", selected = "simDashPanel")
    })
    
    observeEvent(input$link_to_sim_dash_lp, {
      updateNavbarPage(session, "masterPage", selected = "simDashPanel")
    })
    
    observeEvent(input$link_to_sim_dash_vs, {
      updateNavbarPage(session, "masterPage", selected = "simDashPanel")
    })
  }

###################################################################################
###################################################################################

# Run the app
shinyApp(ui = ui, server = server)

###################################################################################
###################################################################################