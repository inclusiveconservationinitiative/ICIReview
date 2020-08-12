##install.packages("gargle")
##devtools::install_github("tidyverse/googlesheets4", force = TRUE)
library(googledrive)
library(googlesheets4)
library(gargle)
##devtools::install_github("daattali/shinyforms")
library(devtools)
library(rsconnect)
library(shiny)
library(shinyforms)
library(shinyalert)
library(shinydashboard)
library(htmltools)
library(markdown)
library(shinythemes)
library(shinyBS)
##devtools::install_github("carlganz/shinyCleave")
library(shinyCleave)
##install.packages("shinyjs")
library(shinyjs)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(shinydashboardPlus)
##install.packages("rlist")
library(rlist)
##install.packages("zoo")
library(zoo)

##install_github("eastcoasting/shinyWidgets")
library("shinyWidgets")
##setwd("C:/Users/MichaelOwen/Documents/Tinkering/ICI")


##defenders <- read.csv("landdefenders.csv", stringsAsFactors = FALSE)
##colnames(defenders)[which(names(defenders) == "X2016")] <- "2016"
##colnames(defenders)[which(names(defenders) == "X2017")] <- "2017"
##colnames(defenders)[which(names(defenders) == "X2018")] <- "2018"


cbd <- read.csv("cbd.csv", stringsAsFactors = FALSE)
cbd$Report <- paste0("<a href='",cbd$Report,"' target='_blank'>",cbd$Report,"</a>")


cbdScores <- read.csv("cbdScores.csv", stringsAsFactors = FALSE)



##gefproj <- jsonlite::fromJSON("gefproj.json", flatten = TRUE)
##gefproj <- rbindlist(gefproj, fill=TRUE)
##gefproj <- gefproj %>% drop_na("ID")
##gefproj <- subset(gefproj, select = -c(pages, page, count, limit))
##gefproj$Title <- paste0("<a href='",cbd$Report,"' target='_blank'>",cbd$Report,"</a>")
gefproj <- read.csv("gef4.csv", stringsAsFactors = FALSE)
gefproj$IPLC <- as.character(as.integer(gefproj$IPLC))


library(grid)
library(scales)
element_custom <- function() {
  structure(list(), class = c("element_custom", "element_text"))
}
element_grob.element_custom <- function(element, label="", ...)  {
  disect <- strsplit(label, "\\n")[[1]]
  labels <- lapply(disect, function(x) tryCatch(parse(text=x), 
                                                error = function(e) x))
  hl <-  unit(rep(1, length(labels)), 'strheight', data=labels) + unit(0.1,"line")
  yl <- c(list(unit(0,"line")), 
          lapply(seq_along(labels[-length(labels)]), function(ii) sum(hl[1:ii])))
  
  cl <- do.call(gList, Map(function(label, ii) 
    textGrob(label, y = unit(1,"npc")-yl[[ii]], hjust=0, x=0, vjust=1), 
    label = labels, ii = seq_along(labels)))
  
  gTree(children = cl, cl="sl", heights = hl, gp=gpar(col="grey50",fontsize=11))
}
heightDetails.sl <- function(x) sum(x$heights)

## format for thousands/millions


##label_number_si(accuracy=.1)


centroids <- read.csv("centerCoords.csv")
colnames(centroids)[which(names(centroids) == "x")] <- "long"
colnames(centroids)[which(names(centroids) == "y")] <- "lat"




nmsY <- names(gefproj[c(7,8,10)])
nmsX <- names(gefproj[c(3,4)])

options(gargle_quiet = FALSE)
gs4_auth(path = "auth.json")
drive_auth(path = "auth.json")
##eoiScoring <- gs4_create("EOI Scoring")
##ID: 19SnbNQjgS1_kYcefaWNRB61oERL6jSl9f3kGP3MhPaY


dat <- data.frame(
  country = c("Djibouti", "Kenya", "Tanzania", "Somalia", "Mozambique", "South Africa", "Cameroon", "DR Congo", "Republic of the Congo", "Equatorial Guinea", "Gabon", 
              "Mexico", "Guatemala", "Honduras", "Belize", "El Salvador","Nicaragua", "Panama", "Costa Rica", "Colombia", "Ecuador", "Peru", "Brazil", "Guyana", "Suriname", "Argentina", "Chile",
              "India", "Pakistan", "Cambodia", "Thailand", "Vietnam", "Myanmar", "Peninsular Malaysia", "Island Malaysia", "Indonesia", "Philippines", "East Timor" ,"Vanuatu", "Solomon Islands", "Fiji",
              "Papua New Guinea", "West Papua" ,"Easter Island", "Samoa", "Tonga", "Cook Islands", "Tuvalu", "Tokelau", "Niue", "Wallis", "Futuna"),
  OHI = c('<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Djibouti.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Kenya.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Tanzania.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Somalia.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Mozambique.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_SouthAfrica.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Cameroon.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_DemocraticRepublicoftheCongo.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_RepubliqueduCongo.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_EquatorialGuinea.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Gabon.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Mexico.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Guatemala.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Honduras.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Belize.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_ElSalvador.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Nicaragua.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Panama.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_CostaRica.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Colombia.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Ecuador.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Peru.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Brazil.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Guyana.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Suriname.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Argentina.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Chile.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_India.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Pakistan.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Cambodia.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Thailand.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Vietnam.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Myanmar.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Malaysia.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Malaysia.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Indonesia.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Philippines.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_EastTimor.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Vanuatu.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_SolomonIslands.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Fiji.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_PapuaNewGuinea.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Indonesia.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Chile.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Samoa.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Tonga.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_CookIslands.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Tuvalu.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Tokelau.png" height="25%"></img>',
          '<img src="hhttps://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_Niue.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_WallisandFutuna.png" height="25%"></img>',
          '<img src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_WallisandFutuna.png" height="25%"></img>'
  )
)



##df <- data.frame(matrix(NA, nrow = 1000, ncol = 100))
##surveyData <- sheet_write(df)

##EOIScoring %>%
##drive_share(role = "writer", type = "anyone")


fields <- c("name", "email", "date", "eoiNumber", "ind_var_select", "submittingOrg", "score1ai", "evidence1ai", "score1aii", "evidence1aii", "score1bi", "evidence1bi", "score1bii", "evidence1bii", "score1ci", "evidence1ci", "score1di", "evidence1di", "score1dii", "evidence1dii", "score1diii", "evidence1diii", "score1ei", "evidence1ei", "score2ai", "evidence2ai", "score2aii", "evidence2aii", "score2aiii", "evidence2aiii", "score2aiv", "evidence2aiv", "score2av", "evidence2av", "score2bi", "evidence2bi", "score2bii", "evidence2bii", "score2biii", "evidence2biii", "score2ci", "evidence2ci", "score2di", "evidence2di", "score2ei", "evidence2ei", "score3ai", "evidence3ai", "score3bi", "evidence3bi", "score3ci", "evidence3ci", "score3di", "evidence3di", "score3ei", "evidence3ei", "score3eii", "evidence3eii", "finalComment")                                                                               
# Shiny app with 3 fields that the user can submit data for



table <- "responses"


##Custom nav button in JS
##range_clear("1CwPYEDosLIcCXcha-8IniMgR1P0H5Xcdspuglal5IsA")
##sheet_resize("1CwPYEDosLIcCXcha-8IniMgR1P0H5Xcdspuglal5IsA", nrow = 250, ncol = 50, exact = FALSE)



like <- function (vector, pattern, ignore.case = FALSE, fixed = FALSE) 
{
  if (is.factor(vector)) {
    as.integer(vector) %in% grep(pattern, levels(vector), 
                                 ignore.case = ignore.case, fixed = fixed)
  }
  else {
    grepl(pattern, vector, ignore.case = ignore.case, fixed = fixed)
  }
}





saveData <- function(input) {
  # put variables in a data frame
  data <- data.frame(matrix(nrow=1,ncol=0))
  for (x in fields) {
    var <- input[[x]]
    if (length(var) > 1 ) {
      # handles lists from checkboxGroup and multiple Select
      data[[x]] <- list(var)
    } else {
      # all other data types
      data[[x]] <- var
    }
  }
  data$submit_time <- date()

  sheet_append("1CwPYEDosLIcCXcha-8IniMgR1P0H5Xcdspuglal5IsA", data)
}





score1aiList <- 0:5
names(score1aiList) <- c("Not significant", "Low Significance", "Moderate Significance", "Medium-high Significance", "High Significance", "Exceptional Significance")

score1aiiList <- 0:2
names(score1aiiList) <- c("Low carbon values", "Moderate", "High carbon values")


score1biList <- 0:5
names(score1biList) <- c("Not evident", "Marginally under IPLC governance", "Partially under IPLC systems of governance", "Largely under IPLC governance - with significant constraints", "Held and managed under IPLC governance systems - with some limitations", "Strong and active IPLC governance systems")


score1biiList <- 0:2
names(score1biiList) <- c("No explanation", "Significance of site(s) vaguely described", "Unique significance of site(s) clearly explained")


score1ciList <- 0:5
names(score1ciList) <- c("No evident threats", "Low threats", "Moderate threats", "Medium-high threats", "High threats", "Requires urgent action")


score1diList <- 0:3
names(score1diList) <- c("Undermines rights", "Recognize limited rights", "Lack implementing regulations", "Actively promote IPLC governance")


score1diiList <- 0:3
names(score1diiList) <- c("Actively opposed", "Recognized importance of IPLC-led conservation", "Implemented some support for IPLC-led conservation", "Actively promote IPLC-led conservation")


score1diiiList <- 0:3
names(score1diiiList) <- c("None implemented", "Few projects in pilot stages only", "Few projects have been implemented beyond pilot", "Projects have been well established for many years")


score1eiList <- 0:2
names(score1eiList) <- c("Few to none", "Small and/or tangentially related to project goals", "Align strongly with with project goals")


score2aiList <- 0:3
names(score2aiList) <- c("Weakly aligned", "Partially aligned", "Well aligned", "Exceptionally well aligned")

##this counts by 2
score2aiiList <- 0:3
names(score2aiiList) <- c("Lacks clarity",  "Activities and results defined but logic (ToC) is incomplete", "Activities and results well-defined but some aspects require clarification", "Clear objectives and cohesive approach")

score2aiiiList <- 0:3
names(score2aiiiList) <- c("Does not address", "Low contributions", "Over-ambitious contributions", "Realistic and sufficiently ambitious")


score2aivList <- 0:3
names(score2aivList) <- c("Not aligned with EoI", "Partially aligned ", "Well aligned", "Exceptionally well aligned")

score2avList <- 0:3
names(score2avList) <- c("None", "Small ", "Moderate", "Significant")


score2biList <- 0:5
names(score2biList) <- c("Not provided", "Very Low <10k Ha", "Low 10 - 100k Ha", "Moderate 100 - 500k Ha", "High 500k - 1M Ha", "Very high >1M Ha")

score2biiList <- 0:3
names(score2biiList) <- c("Not provided", "Proposed but not clearly aligned", "Proposed and moderately aligned", "Clearly derived from project goals")


score2biiiList <- 0:3
names(score2biiiList) <- c("Not provided", "No clear long-term impact", "Medium-term benefits", "Long-term benefits")


score2ciList <- 0:3
names(score2ciList) <- c("Not provided", "Contributions weakly related", "Contributions tangentially related", "Contributions clearly positioned")


score2diList <- 0:3
names(score2diList) <- c("Approach is absent", "Approach is weak", "Approach is moderately thought through", "Significant and well-thought through approach")


score2eiList <- 0:5
names(score2eiList) <- c("None demonstrated", "Low demonstrated potential", "Moderate demonstrated potential", "Medium-high demonstrated potential", "High demonstrated potential", "Exceptional demonstrated potential")

##Counts by 2
score3aiList <- 0:3
names(score3aiList) <- c("Beneficiaries only", "Combination/partnership - plans clear", "IPLC-led - NGOs in more limited defined roles" ,"Fully IPLC composed and led approach")

score3biList <- 0:3
names(score3biList) <- c("None demonstrated", "Limited demonstration",  "Demonstrated leadership", "Exceptional and long-standing leadership")


score3ciList <- 0:5
names(score3ciList) <- c("No partners defined", "No IPLC partners", "IPLC partners with scope not defined", "IPLC partners with clear roles", "Strong IPLC partnerships", "Strong IPLC partnerships and links to other IPOs")

score3diList <- 0:5
names(score3diList) <- c("None demonstrated", "Capacity has no relation to project", "Capacity has some gaps in relation to project", "Plan to fill capacity gaps during project", "Full capacity but no GEF project experience", "Full capacity and GEF project experience")

score3eiList <- 0:3
names(score3eiList) <- c("Very limited", "Some capacity but would require support", "Moderate capacity", "Very strong with demonstrated past performance")

score3eiiList <- 0:2
names(score3eiiList) <- c("Answered no", "Answered yes but with lacking explanation", "Answered yes with clear explanation of extent")

ui <- navbarPage(title = "ICI Geographies EoI Scoring",
                 theme = shinytheme("flatly"), 
                 

                 
#####INTRO              
                 tabPanel("Introduction", value = "panel-1",
                        tagList(  
                          tags$head(
                            tags$script(type="text/javascript", src = "js/index2.js"),
                            tags$script(type="text/javascript", src = "js/tileImages.js"),
                            tags$style(type = "text/css", ".navbar {margin-bottom: 20px;}"),
                            tags$style(type = "text/css", ".container-fluid .navbar-header 
                                        .navbar-brand {margin-left: 0px;}"),
                            tags$style(HTML("hr {border-top: 1px solid #2C3E50;}")))),

                          fluidPage(
                            h4("To translate this page download the translate extension at the following", tags$a("link.", href="https://chrome.google.com/webstore/detail/google-translate/aapbdbdomjkkjkaonfhkkikfgjllcleb?hl=en", target="_blank")),
                            h4("Para traducir esta página, descargue la extensión de traducción desde", tags$a("el enlace.", href="https://chrome.google.com/webstore/detail/google-translate/aapbdbdomjkkjkaonfhkkikfgjllcleb?hl=es", target="_blank")),
                            h4("Pour traduire cette page, téléchargez l'extension de traduction sur", tags$a(" le lien.", href="https://chrome.google.com/webstore/detail/google-translate/aapbdbdomjkkjkaonfhkkikfgjllcleb?hl=fr", target="_blank")),
                            
                            br(),
                            h3("ICI EoI Assessment Introduction", style = "font-style: bold"),  
                            br(),
                            h5("Dear Reviewer,"),
                            h5("Thank you for taking the time to evaluate these expressions of interests (EoI) for funding under the Inclusive Conservation Initiative (ICI). The ICI is a Global Environment Facility-supported project designed to enhance Indigenous Peoples and Local Communities' (IPLCs) efforts to steward lands, waters and natural resources that deliver global environmental benefits and address the growing drivers of global environmental degradation."),  
                            h5("As a reviewer, you will be asked to use a combination of sources to assess the eligibility of these EoIs for funding. These sources include:"),
                            tags$li("The EoI itself"),
                            tags$li("Various complementary spatial and qualitative sources which will be provided next to relevant questions"),
                            tags$li("Your own expertise"),
                            
                            h5("For each of the evaluation questions, you will be given the option of several potential scores-each with criteria to help you determine the best fit. For each section, you will also be asked to include a 2-3 sentence justification for the score."),
                            h5("If you have any questions please reach out to info@inclusiveconservationinitiative.org."),
                            br(),
                            h5("Warmest regards,"),
                            h5("The ICI teams at Conservation International and IUCN"),
                            
                            br(),
                            hr(),
                            br(),
                            h5("To get started, please fill out the following forms: "),
                            br(),
                            h4("ETHICAL CONDUCT AGREEMENT", align = "center"),        
                            h5("I, _____________________, serve the Inclusive Conservation Initiative (ICI) as a reviewer."),
                            h5("I agree that, as I carry out my duties,"),
                            h5("1. I will act honestly, in good faith and in the best interests of the ICI. I will use the care, skill and diligence that any reasonably prudent person would use in a similar situation;"),
                            h5("2. I will keep secret any confidential or private information about the grant applicants or any other individuals or groups that I become aware of through my role as reviewer. If I am not sure whether certain information should be kept confidential, I will ask the ICI, as appropriate, for a decision on the matter;"),
                            h5("3. I will declare any perceived or real conflict of interest or conflict of loyalties right away. Conflict of interest and conflict of loyalties are defined below."),
                            
                            tags$ul(
                            tags$li("Conflict of Interest: A conflict of interest is normally associated with improper financial gain, whether deliberately sought or innocently arrived at. No person shall accept to review a grant application if there is a material conflict of interest between their role as reviewer and their role in any other capacity. If such a situation arises, the person must eliminate the conflict of interest or step down from their role as reviewer for the grant application in question."),
                            tags$li("Conflict of Loyalties: A conflict of loyalties is present when a person owes a duty or loyalty to two or more parties and cannot reconcile those loyalties by identifying and serving the common interests of the separate parties."),
                            ),
                            h5("4. I will refrain from using information obtained during the review process for my own or another's advantage, or to disadvantage or discredit others."),
                            h5("I hereby certify that I have read and understand the Ethical Conduct Agreement and I commit to conduct myself in compliance with the Agreement."),
                            br(),
                            
                            
                    fluidRow(
                              column(12, textInput("name", "Name: (FIRST NAME, LAST NAME)", "", width = "40%")),
                              column(12, textInput("email", "Email Address", "", width = "40%")),
                              column(12, dateInput("date", "Date of Review", value = NULL, width = "40%"))),
                             
                            h3("EoI Info", style = "font-style: bold"),  
                            
                            fluidRow(
                              
                              column(12, numericInput("eoiNumber", "EOI ID*", "", width = "40%")),
                              column(6,
                                     uiOutput("var_select")),
                              column(6,"Geography-Country pairs are defined by the project location. Key geographies were identifed using avaliable biodiversity and global environmental benefits prioritization maps."),
                              
                              column(12, textInput("submittingOrg", "Applicant Organization", "", width = "40%"))
                              ),
                              
                              
                    br(),
                    fluidRow( 
                      
                      column(12, 
                             align = "center",
                             tags$a(onclick="customHref('panel-2')",
                             actionButton("action1",tags$strong("Begin Evaulation"), style = "background-color: #ECF0F1; color: #6c7985;")))),
                    br(),
                    br()
                    

                            
                     )),
                 
            

 ####Section 1                
                 
                 tabPanel("Section 1", value = "panel-2",
                          tagList(  
                            tags$head(
                              tags$script(type="text/javascript", src = "js/index2.js"),
                              tags$style(type = "text/css", ".navbar {margin-bottom: 20px;}"),
                              tags$style(type = "text/css", ".container-fluid .navbar-header 
                                        .navbar-brand {margin-left: 0px;}"),
                              tags$style(type='text/css', ".irs-single {font-size: 12pt; display: block;}"),
                              tags$style(type='text/css', ".control-label {font-size: 17px;}"),
                              tags$style(HTML("hr {border-top: 1px solid #2C3E50;}")))),
                            chooseSliderSkin(skin = "Flat"),
                          
                          
                            h3(textOutput("eoiNum")),
                          
                            h3("Section 1 - Experience & strengths relevant to the proposed Indigenous territory, landscape/seascape (Total Points: 30)", style = "font-style: bold"),  
                            br(),
                        
                            h4(tags$strong("A) Importance of the landscape/seascape/indigenous territory for biodiversity, with additional consideration to climate benefits.")),
                           

                           fluidRow( 
                            
                             column(7, style='margin-left: 50px;',
                                    sliderLabelInput(inputId = 'score1ai', 
                                                        label = div('1. Is the proposed territory/landscape/seascape a globally important area for biodiversity?'), 
                                                        choices = score1aiList,
                                                        selected = score1aiList[1],
                                                        grid = FALSE,
                                                        width = '80%'),
                                    h5(tags$em('Scoring:')),
                                    h5(tags$ul(
                                      tags$li('Not significant;'),
                                      tags$li('Low Significance;'),
                                      tags$li('Moderate Significance;'),
                                      tags$li('Medium-high Significance;'),
                                      tags$li('High Significance;'),
                                      tags$li('Exceptional Significance')))),
                             column(1),
                             column(4,
                                    wellPanel(
                                      h5(tags$strong("Source in EoI:")),
                                      h5(tags$ul(
                                        tags$li("Question 1"))),
                                      h5(tags$strong("Supporting Spatial Resources:")),
                                      h5(tags$ul(
                                        tags$li(uiOutput("hyperlinkRSR")),
                                        tags$li(uiOutput("hyperlinkIFL")),
                                        tags$li(uiOutput("hyperlinkKBA"))),
                                        h5("Configuration of spatial layers in legend should have RSR as base, then IFLs and KBAs respectively. Interpretation of layers is also included in the spatial dashboard.")
                                      ),
                                      h5(tags$strong("Supporting Tabular Resources:")),
                                      h5(tags$ul(
                                        tags$li(tags$a("Prioritization of biodiversity and GEBs"), onclick="customHref('panel-6')"))),
                                      style = "padding: 2%;", width = "100%", height  = "100%")
                             ), 
                             column(12, textAreaInput("evidence1ai", "Reviewer Justification for Score", "", width = "600px")),
                             
                            ),
                          br(),
                          
                          fluidRow( 
                            
                            column(7, style='margin-left: 50px;', 
                                   sliderLabelInput(inputId = 'score1aii', 
                                                       label = div('2. Is the area important for climate mitigation?'), 
                                                       choices = score1aiiList,
                                                       selected = score1aiiList[1],
                                                       grid = FALSE,
                                                       width = '80%'),
                                   h5(tags$em('Note:'),'Density of carbon surrounding project or country (if location unknown).'),
                                   h5(tags$em('Scoring:')),
                                   h5(tags$ul(
                                     tags$li('>50 t/ha - Low;'),
                                     tags$li('50 - 100 t/ha - Moderate;'),
                                     tags$li('>100 t/ha - High')))),
                            column(1),
                            
                            column(4,
                                   wellPanel(
                                     h5(tags$strong("Source in EoI:")),
                                     h5(tags$ul(
                                       tags$li("Question 1"))),
                                     h5(tags$strong("Supporting Spatial Resources:")),
                                     h5(tags$ul(
                                       tags$li(uiOutput("hyperlinkCarbon"))),
                                       h5("Interpretation of layer is also included in the spatial dashboard.")),
                                     style = "padding: 2%;", width = "100%", height  = "100%")
                            ),
                            br(),
                            column(12, textAreaInput("evidence1aii", "Reviewer Justification for Score", "", width = "600px")),
                        ),
                        hr(),
                        br(),
                        h4(tags$strong("B) Geographical focus in an area under IPLC governance.")),
                        br(),


                          fluidRow( 
                            
                            
                            column(7, style='margin-left: 50px;',  
                                   sliderLabelInput(inputId = 'score1bi', 
                                                       label = div('3. Is the area held and managed by IPLC under community-based governance systems?', br()), 
                                                       choices = score1biList,
                                                       selected = score1biList[1],
                                                       grid = FALSE,
                                                       width = '80%'),
                                   h5(tags$em('Note:'),'Assessing project area alignment (either spatially or politically) with IPLC systems of governance. Governance refers to both land/territorial/resource rights and institutions.'),
                                   h5(tags$em('Scoring:')),
                                   h5(tags$ul(
                                     tags$li('IPLC governance (rights and institutions) not evident;'),
                                     tags$li('Project areas are marginally under IPLC governance (spatially or politically);'),
                                     tags$li('Project areas are partially under IPLC systems of governance (spatially or politically);'),
                                     tags$li('Project areas are largely under IPLC governance, but IPLC rights and/or institutions face significant constraints;'),
                                     tags$li('Project areas are held and managed under IPLC governance systems, with some limitations;'),
                                     tags$li('Project areas are held and managed under strong and active IPLC governance systems')))),
                            column(1),
                            
                            column(4,
                                   wellPanel(
                                     h5(tags$strong("Source in EoI:")),
                                     h5(tags$ul(
                                       tags$li("Question 2"))),
                                     h5(tags$strong("Supporting Spatial Resources:")),
                                     h5(tags$ul(
                                       tags$li(uiOutput("hyperlinkFRTenure")),
                                       tags$li(uiOutput("hyperlinkNFRTenure"))),
                                       h5("Based on available spatial data (not fully comprehensive)."),
                                       h5("Each link should bring up multiple layers as seen in the legend, if this does not load, re-toggle each layer."),
                                       h5("Interpretation of layers is also included in the spatial dashboard.")),
                                     style = "padding: 2%;", width = "100%", height  = "100%")
                            ),
                            column(12, textAreaInput("evidence1bi", "Reviewer Justification for Score", "", width = "600px")),
                          ),
                            
                            br(),
                            fluidRow( 
                              
                              column(7, style='margin-left: 50px;',  
                                     sliderLabelInput(inputId = 'score1bii', 
                                                         label = div('4. Does the proposal explain the unique cultural significance of the area to IPLCs?'), 
                                                         choices = score1biiList,
                                                         selected = score1biiList[1],
                                                         grid = FALSE,
                                                         width = '80%'),
                                     h5(tags$em('Scoring:')),
                                     h5(tags$ul(
                                       tags$li('No explanation given of unique significance to IPLCs;'),
                                       tags$li('Significance of site(s) vaguely described;'),
                                       tags$li('Unique significance of project site(s) clearly explained')))),
                              column(1),
                              
                              column(4,
                                   wellPanel(
                                     h5(tags$strong("Source in EoI:")),
                                     h5(tags$ul(
                                       tags$li("Question 2"))),
                                     style = "padding: 2%;", width = "100%", height  = "100%")
                            ), 
                            
                          column(12, textAreaInput("evidence1bii", "Reviewer Justification for Score", "", width = "600px"))),

                        hr(),
                        br(),
                        h4(tags$strong("C) Vulnerability of the proposed IPLCs as well as their lands/waters/natural resources to threats.")),
                        br(),
                        

                        fluidRow( 

                          column(7, style='margin-left: 50px;',  
                                 sliderLabelInput(inputId = 'score1ci', 
                                                     label = div('5. Is the area vulnerable to threats/current risk of negative impacts to IPLC and biodiversity without action?'), 
                                                     choices = score1ciList,
                                                     selected = score1ciList[1],
                                                     grid = FALSE,
                                                     width = '80%'),
                                 h5(tags$em('Note:'),'Critical sites include those that are threatened on multiple fronts.'),
                                 h5(tags$em('Scoring:')),
                                 h5(tags$ul(
                                   tags$li('No evident threats;'),
                                   tags$li('Low threats;'),
                                   tags$li('Moderate threats;'),
                                   tags$li('Medium-high threats;'),
                                   tags$li('High threats;'),
                                   tags$li('Requires urgent action')))),

                          column(1),
                          column(4,
                                 wellPanel(
                                   h5(tags$strong("Source in EoI:")),
                                   h5(tags$ul(
                                     tags$li("Question 3"))),
                                   h5(tags$strong("Supporting Spatial Resources:")),
                                   h5(tags$ul(
                                     tags$li(uiOutput("hyperlinkGFW")),
                                     tags$li(uiOutput("hyperlinkCDP")),
                                     tags$li(uiOutput("hyperlinkGLD"))),
                                     h5("Configuration of spatial layers in legend should have Forest loss or Cumulative Development Pressures as base layer, with additional insights drawn from the land deals dataset on top. Interpretation of layers is also included in the spatial dashboard.")),
                                   h5(tags$strong("Supporting Tabular Resources:")),
                                   h5(tags$ul(
                                     tags$li(tags$a("Marine Threats and Context", onclick="customHref('panel-9')")),
                                     tags$li(tags$a("Threats to Biodiversity, Environment, and IPLC Regional Context"), onclick="customHref('panel-6_1')"))),
                                     
                                   style = "padding: 2%;", width = "100%", height  = "100%")
                          ),
                          
                          column(12, textAreaInput("evidence1ci", "Reviewer Justification for Score", "", width = "600px")),
                          
                          ),
                        
                        
                        hr(),
                        br(),
                        h4(tags$strong("D) Opportunities for ICI results - including enabling policy conditions, positive government support and presence of successful IPLC-led conservation initiatives that could be scaled up.")),
                        br(),                        
                        
                        fluidRow( 
                          
                          column(7, style='margin-left: 50px;', 
                                 sliderLabelInput(inputId = 'score1di', 
                                                     label = div('6. Are enabling policy conditions in place for IPLC-led conservation in the proposed area?'), 
                                                     choices = score1diList,
                                                     selected = score1diList[1],
                                                     grid = FALSE,
                                                     width = '80%'),
                                 h5(tags$em('Scoring:')),
                                 h5(tags$ul(
                                   tags$li('Legal and policy frameworks in project areas undermine IPLC governance (either actively or through absence);'),
                                   tags$li('Legal and policy frameworks recognize limited rights for IPLCs over their lands and/or resources;'),
                                   tags$li('Legal and policy frameworks recognize rights over lands and resources but with constraints (e.g., lack implementing regulations);'),
                                   tags$li('Legal and policy frameworks actively promote the recognition of IPLC governance ')))),
                          column(1),
                          
                          column(4,
                                 wellPanel(
                                   h5(tags$strong("Source in EoI:")),
                                   h5(tags$ul(
                                     tags$li("Question 4, 5"))),
                                   h5(tags$strong("Supporting Tabular Resources:")),
                                   h5(tags$ul(
                                     tags$li(tags$a("National IPLC support actions highlighted in CBD Reports", onclick="customHref('panel-7')")),
                                     tags$li(tags$a("Legal Framework, Scale of Recognition, & Government Willingness to support scaling up IPLC tenure", onclick="customHref('panel-6_2')")))),
                                   style = "padding: 2%;", width = "100%", height  = "100%")
                          ),
                          
                          column(12, textAreaInput("evidence1di", "Reviewer Justification for Score", "", width = "600px"))),
                        br(),
                        
                        fluidRow( 
                          
                          column(7, style='margin-left: 50px;', 
                                 sliderLabelInput(inputId = 'score1dii', 
                                                  label = div('7. Is there active government support for IPLC-led conservation in the proposed country/area?'), 
                                                  choices = score1diiList,
                                                  selected = score1diiList[1],
                                                  grid = FALSE,
                                                  width = '80%'),
                                h5(tags$em('Scoring:')),
                                h5(tags$ul(
                                  tags$li('National or sub-national governments are actively opposed to IPLC-led conservation;'),
                                  tags$li('National or sub-national governments have recognized the importance of IPLC-led conservation;'),
                                  tags$li('National or sub-national governments have implemented some support for IPLC-led conservation;'),
                                  tags$li('National or sub-national governments are actively engaged in the promotion of IPLC rights and IPLC-led conservation')))),
                                column(1),
                                
                                column(4,
                                       wellPanel(
                                         h5(tags$strong("Source in EoI:")),
                                         h5(tags$ul(
                                           tags$li("Question 4, 5"))),
                                         h5(tags$strong("Supporting Tabular Resources:")),
                                         h5(tags$ul(
                                           tags$li(tags$a("National and/or subnational govt willingness to support scaling up of IPLC rights to lands/territories/resources"), onclick="customHref('panel-6_2')"))),
                                         style = "padding: 2%;", width = "100%", height  = "100%")
                                ),
                                
                                column(12, textAreaInput("evidence1dii", "Reviewer Justification for Score", "", width = "600px"))), 
                        br(),
                        
                        fluidRow( 
                          
                          column(7, style='margin-left: 50px;', 
                                sliderLabelInput(inputId = 'score1diii', 
                                                  label = div('8. Are there successful IPLC-led conservation initiatives in the proposed area that provide a foundation for scaling up?'), 
                                                  choices = score1diiiList,
                                                  selected = score1diiiList[1],
                                                  grid = FALSE,
                                                  width = '80%'),
                                h5(tags$em('Scoring:')),
                                h5(tags$ul(
                                  tags$li('No IPLC-led conservation initiatives have been implemented;'),
                                  tags$li('Few IPLC-led conservation projects have been implemented in pilot stages only;'),
                                  tags$li('Some IPLC-led conservation projects have been implemented beyond pilot stages;'),
                                  tags$li('Relevant IPLC-led conservation projects have been well established for many years')))),
                          column(1),
                          
                          column(4,
                                 wellPanel(
                                   h5(tags$strong("Source in EoI:")),
                                   h5(tags$ul(
                                     tags$li("Question 4, 5"))),
                                   style = "padding: 2%;", width = "100%", height  = "100%")
                          ),
                          

                          
                          column(12, textAreaInput("evidence1diii", "Reviewer Justification for Score", "", width = "600px"))),
                        
                        hr(),
                        br(),
                        h4(tags$strong("E) Synergies with existing investments.")),
                        br(),
                        
                        
                        fluidRow( 

                          column(7, style='margin-left: 50px;', 
                                 sliderLabelInput(inputId = 'score1ei', 
                                                     label = div('9. Are there other initiatives (relevant projects) that provide complementary support for IPLC-led conservation in the geography?'), 
                                                     choices = score1eiList,
                                                     selected = score1eiList[1],
                                                     grid = FALSE,
                                                     width = '80%'),
                                 h5(tags$em('Note:'),'Relevant projects and initiatives that are currently underway.'),
                                 h5(tags$em('Scoring:')),
                                 h5(tags$ul(
                                   tags$li('Few to no complementary projects/investment;'),
                                   tags$li('Complementary projects/investments are small, or are tangentially related to project goals;'),
                                   tags$li('Complementary Projects/investments align strongly with project goals and investments are substantial')))),
                          column(1),
                          
                          column(4,
                                 wellPanel(
                                   h5(tags$strong("Source in EoI:")),
                                   h5(tags$ul(
                                     tags$li("Question 6, 7"))),
                                   h5(tags$strong("Supporting Tabular Resources:")),
                                   h5(tags$ul(
                                     tags$li(tags$a("Current Cofinancing Landscape", onclick="customHref('panel-8')")))),
                                     style = "padding: 2%;", width = "100%", height  = "100%")
                                 ),
                        
                        column(12, textAreaInput("evidence1ei", "Reviewer Justification for Score", "", width = "600px"))),
                       
                      fluidRow( 
                        
                       wellPanel(
                         h5(tags$strong("TOTAL POINTS SECTION 1:")),
                         column (2, verbatimTextOutput("scoreSection1")),
                         br(),
                         br(),
                         style = "padding: 2%;", width = "100%", height  = "100%"),
                       
                       br(),
                       fluidRow( 
                         column(12, align = "center",
                                tags$a(onclick="customHref('panel-1')",
                                       actionButton("action3_",tags$strong("Back"), style = "background-color: #ECF0F1; color: #6c7985;")),
                                HTML('&emsp;'),
                                tags$a(onclick="customHref('panel-3')",
                                       actionButton("action3",tags$strong("Section 2"), style = "background-color: #ECF0F1; color: #6c7985;")))),
                       br(),
                       br(),
                       
                      )),
                 

 ###############SECTION 2
 
 
                     tabPanel("Section 2", value = "panel-3",
                              tagList(  
                                tags$head(
                                  tags$script(type="text/javascript", src = "js/index2.js"),
                                  tags$style(type = "text/css", ".navbar {margin-bottom: 20px;}"),
                                  tags$style(type = "text/css", ".container-fluid .navbar-header 
                                        .navbar-brand {margin-left: 0px;}"),
                                  tags$style(type='text/css', ".irs-single {font-size: 12pt; display: block;}"),
                                  tags$style(type='text/css', ".control-label {font-size: 17px;}"),
                                  tags$style(HTML("hr {border-top: 1px solid #2C3E50;}")))),
                              chooseSliderSkin(skin = "Flat"),
                              
                              h3(textOutput("eoiNum2")),
                              
                              h3("Section 2 - Quality and ability of the proposed approach and interventions to achieve transformational impact that generate the global environmental benefits (Total Points: 40)", style = "font-style: bold"),  
                              br(),
                              
                              h4(tags$strong("A) Quality of proposed approach and ability to support traditional structures, knowledge and community practices in the delivery of global environmental benefits.")),
                              fluidRow( 

                                column(7, style='margin-left: 50px;', 
                                       sliderLabelInput(inputId = 'score2ai', 
                                                        label = div("1. Is the proposed approach well aligned with the overall objective of the ICI to: Enhance Indigenous Peoples' and Local Communities' (IPLCs) efforts to steward land, waters and natural resources to deliver global environmental benefits?"), 
                                                        choices = score2aiList,
                                                        selected = score2aiList[1],
                                                        grid = FALSE,
                                                        width = '80%'),
                                       h5(tags$em('Scoring:')),
                                       h5(tags$ul(
                                         tags$li('Weakly aligned;'),
                                         tags$li('Partially aligned;'),
                                         tags$li('Well aligned;'),
                                         tags$li('Exceptionally well aligned')))),
                             column(1),
                             column(4,
                                    wellPanel(
                                      h5(tags$strong("Source in EoI:")),
                                      h5(tags$ul(
                                        tags$li("Questions 8, 9, 10, 11 - and overall"))),
                                      style = "padding: 2%;", width = "100%", height  = "100%")
                             ), 
                             
                             
                             column(12, textAreaInput("evidence2ai", "Reviewer Justification for Scores", "", width = "600px"))),
                             br(),
                             
                               fluidRow( 
                                 column(7, style='margin-left: 50px;',          
                                       
                                       sliderLabelInput(inputId = 'score2aii', 
                                                        label = div('2. Does the EoI present a clear and convincing set of activities and results?'), 
                                                        choices = score2aiiList,
                                                        selected = score2aiiList[1],
                                                        grid = FALSE,
                                                        width = '80%'),
                                       h5(tags$em('Scoring:')),
                                       h5(tags$ul(
                                         tags$li('The objectives and approach for this project lack clarity and cohesion, and/or do not appear to be realistic for the context;'),
                                         tags$li('Activities & results defined but logic (Theory of Change) is incomplete;'),
                                         tags$li('Activities and results are well-defined and cohesive but some aspects require clarification;'),
                                         tags$li('The project has clear objectives and a cohesive approach with relevant activities for the context and timeline')))),
                                 column(1),
                                 column(4,
                                        wellPanel(
                                          h5(tags$strong("Source in EoI:")),
                                          h5(tags$ul(
                                            tags$li("Questions 8, 9"))),
                                          style = "padding: 2%;", width = "100%", height  = "100%")
                                 ),
                                 column(12, textAreaInput("evidence2aii", "Reviewer Justification for Scores", "", width = "600px"))),
                                 
                             br(),
                             
                            fluidRow( 
                                 
                                 column(7, style='margin-left: 50px;',        
                                       sliderLabelInput(inputId = 'score2aiii', 
                                                        label = div('3. Will the project (objectives and activities) contribute to overcoming identified threats and putting in place necessary enabling opportunities for IPLC-led conservation?'), 
                                                        choices = score2aiiiList,
                                                        selected = score2aiiiList[1],
                                                        grid = FALSE,
                                                        width = '80%'),
                                       h5(tags$em('Note:'),'Ensure that the proposed project aims to respond to the threats and opportunities identified in EoI Section.'),
                                       h5(tags$em('Scoring:')),
                                       h5(tags$ul(
                                         tags$li('Objectives and activities do not clearly address identified threats and opportunities;'),
                                         tags$li('Contributions to addressing the threats and opportunities are low;'),
                                         tags$li('Contributions to addressing threats and enabling conditions are slightly over-ambitious;'),
                                         tags$li("The impact on threats and enabling conditions can be realistically accomplished and are sufficiently ambitious for the projects' context")))),
                                 column(1),
                                 column(4,
                                        wellPanel(
                                          h5(tags$strong("Source in EoI:")),
                                          h5(tags$ul(
                                            tags$li("Questions 8, 9"))),
                                          style = "padding: 2%;", width = "100%", height  = "100%")
                                 ), 

                                 column(12, textAreaInput("evidence2aiii", "Reviewer Justification for Scores", "", width = "600px"))), 
                            br(),
                            
                             fluidRow( 
                               
                               column(7, style='margin-left: 50px;', 
                                       
                                       sliderLabelInput(inputId = 'score2aiv', 
                                                        label = div('4. Are the activities achievable within a $500,000 to $2,000,000 USD budget range in a period of 5 years of project execution?'), 
                                                        choices = score2aivList,
                                                        selected = score2aivList[1],
                                                        grid = FALSE,
                                                        width = '80%'),
                                      h5(tags$em('Note:'),'Consider the overall alignment of proposed results and activities with a $500,000 - $2M range of investment per EoI (including whether the level of ambition exceeds or falls below that range).'),
                                       h5(tags$em('Scoring:')),
                                       h5(tags$ul(
                                         tags$li('Activities/results not aligned with EoI range of investment;'),
                                         tags$li('Activities/results Partially aligned with EoI range of investment ;'),
                                         tags$li('Activities/results Well aligned  with EoI range of investment ;'),
                                         tags$li('Activities/results Exceptionally well aligned with EoI range of investment ')))),
                                column(1),
                                column(4,
                                       wellPanel(
                                         h5(tags$strong("Source in EoI:")),
                                         h5(tags$ul(
                                           tags$li("Questions 8, 9, 17"))),
                                         style = "padding: 2%;", width = "100%", height  = "100%")
                                ), 
                                
                                
                                column(12, textAreaInput("evidence2aiv", "Reviewer Justification for Scores", "", width = "600px"))),
                            br(),
                            
                            fluidRow( 
                              
                              column(7, style='margin-left: 50px;', 
                                       sliderLabelInput(inputId = 'score2av', 
                                                        label = div('5. Does the EoI include significant and concrete sources of co-financing?'), 
                                                        choices = score2avList,
                                                        selected = score2avList[1],
                                                        grid = FALSE,
                                                        width = '80%'),
                                       h5(tags$em('Scoring:')),
                                       h5(tags$ul(
                                         tags$li('None;'),
                                         tags$li('Small;'),
                                         tags$li('Moderate;'),
                                         tags$li('Significant')))),
                                column(1),
                                column(4,
                                       wellPanel(
                                         h5(tags$strong("Source in EoI:")),
                                         h5(tags$ul(
                                           tags$li("Question 6, 7"))),
                                         style = "padding: 2%;", width = "100%", height  = "100%")
                                ), 
                                

                                column(12, textAreaInput("evidence2av", "Reviewer Justification for Scores", "", width = "600px"))),
                                

                              hr(),
                              br(),
                              h4(tags$strong("B) Potential of the proposed activities to achieve IPLC-led transformational impact that generate global environmental benefits.")),
                              br(),
                              
                              
                              fluidRow( 
                                column(7, style='margin-left: 50px;',
                                       sliderLabelInput(inputId = 'score2bi', 
                                                        label = div("6. Are the estimated Global Environmental Benefits (GEF core indicators) substantial and realistic?"), 
                                                        choices = score2biList,
                                                        selected = score2biList[1],
                                                        grid = FALSE,
                                                        width = '80%'),
                                       h5(tags$em('Scoring:')),
                                       h5(tags$ul(
                                         tags$li('Not provided;'),
                                         tags$li('Very Low [below 10,000 Ha];'),
                                         tags$li('Moderate [between 100,000 - 500,000 Ha];'),
                                         tags$li('High [between 500,000 - 1,000,000 Ha];'),
                                         tags$li('Very high  [above 1,000,000 Ha]')))),
                                       column(1),
                                       column(4,
                                              wellPanel(
                                                h5(tags$strong("Source in EoI:")),
                                                h5(tags$ul(
                                                  tags$li("Question 12")),
                                                  style = "padding: 2%;", width = "100%", height  = "100%")
                                              )),
                                       column(12, textAreaInput("evidence2bi", "Reviewer Justification for Score", "", width = "600px"))),
                            br(),
                            
                            fluidRow( 
                              column(7, style='margin-left: 50px;',           
                                      sliderLabelInput(inputId = 'score2bii', 
                                                        label = div('7. Are the additional cultural and livelihoods results contributing to project objectives?'), 
                                                        choices = score2biiList,
                                                        selected = score2biiList[1],
                                                        grid = FALSE,
                                                        width = '80%'),
                                       h5(tags$em('Scoring:')),
                                       h5(tags$ul(
                                         tags$li('No provided cultural or livelihood indicators for the project;'),
                                         tags$li('Indicators proposed but are not clearly aligned with project goals;'),
                                         tags$li('Indicators proposed and are moderately  aligned with project goals;'),
                                         tags$li('Additional cultural and/or livelihood indicators clearly derive from project goals')))),
                                       column(1),
                                       column(4,
                                              wellPanel(
                                                h5(tags$strong("Source in EoI:")),
                                                h5(tags$ul(
                                                  tags$li("Question 13")),
                                                  style = "padding: 2%;", width = "100%", height  = "100%")
                                              )),
                                       column(12, textAreaInput("evidence2bii", "Reviewer Justification for Score", "", width = "600px"))),
                            br(),
                            
                            fluidRow( 
                                column(7, style='margin-left: 50px;',      
                                      sliderLabelInput(inputId = 'score2biii', 
                                                        label = div('8. Does the EoI provide a clear and robust vision for long-term sustainability?'), 
                                                        choices = score2biiiList,
                                                        selected = score2biiiList[1],
                                                        grid = FALSE,
                                                        width = '80%'),
                                       h5(tags$em('Scoring:')),
                                       h5(tags$ul(
                                         tags$li('Vision for long-term sustainability not provided;'),
                                         tags$li('This project does not seem to have a clear long-term impact;'),
                                         tags$li('This project will create medium-term benefits for biodiversity and IPLC governance, which future funding will hopefully build upon;'),
                                         tags$li('This project will ensure long-term benefits to biodiversity and IPLC systems of governance')))),
                                column(1),
                                column(4,
                                       wellPanel(
                                         h5(tags$strong("Source in EoI:")),
                                         h5(tags$ul(
                                           tags$li("Question 16")),
                                           style = "padding: 2%;", width = "100%", height  = "100%")
                                       )),
                                column(12, textAreaInput("evidence2biii", "Reviewer Justification for Score", "", width = "600px"))),

                              
                              hr(),
                              br(),
                              h4(tags$strong("C) IPLC-led conservation that advances national and global  environmental priorities.")),
                              br(),
                              
                              fluidRow( 
                                
                                column(7, style='margin-left: 50px;',
                                       sliderLabelInput(inputId = 'score2ci', 
                                                        label = div("9. Does the EoI build on and contribute to national priorities as defined in NBSAPs and/or NDCs?"), 
                                                        choices = score2ciList,
                                                        selected = score2ciList[1],
                                                        grid = FALSE,
                                                        width = '80%'),
                                       h5(tags$em('Scoring:')),
                                       h5(tags$ul(
                                         tags$li('Contributions not provided;'),
                                         tags$li('The project is weakly related to either national priorities;'),
                                         tags$li('The project appears to be tangentially related to national priorities;'),
                                         tags$li('The proposal reflects an understanding of the national policy priorities and clearly positions the project in relation to those priorities')))),
                                column(1),
                                column(4,
                                       wellPanel(
                                         h5(tags$strong("Source in EoI:")),
                                         h5(tags$ul(
                                           tags$li("Question 14"))),
                                         style = "padding: 2%;", width = "100%", height  = "100%")
                                ),
                                column(12, textAreaInput("evidence2ci", "Reviewer Justification for Score", "", width = "600px"))

                                ),
                              
                              
                              hr(),
                              br(),
                              h4(tags$strong("D) Demonstrated gender mainstreaming in all activities.")),
                              br(),                        
                              
                              fluidRow( 
                                column(7, style='margin-left: 50px;',
                                       sliderLabelInput(inputId = 'score2di', 
                                                        label = div("10. Does the EoI provide a clear and robust approach to gender mainstreaming?"), 
                                                        choices = score2diList,
                                                        selected = score2diList[1],
                                                        grid = FALSE,
                                                        width = '80%'),
                                       h5(tags$em('Scoring:')),
                                       h5(tags$ul(
                                         tags$li('Gender mainstreaming approach is absent;'),
                                         tags$li('Gender mainstreaming approach is weak;'),
                                         tags$li("Gender mainstreaming approach is moderately thought through (if there are a few activities as 'add ons');"),
                                         tags$li('Significant and well-thought through approach to gender mainstreaming')))),
                                column(1),
                                column(4,
                                       wellPanel(
                                         h5(tags$strong("Source in EoI:")),
                                         h5(tags$ul(
                                           tags$li("Question 15"))),
                                         style = "padding: 2%;", width = "100%", height  = "100%")),

                                
                                column(12, textAreaInput("evidence2di", "Reviewer Justification for Score", "", width = "600px"))),
                              
                              hr(),
                              br(),
                              h4(tags$strong("E) Innovation and potential to scale up.")),
                              br(), 
           
                              
                              
                              fluidRow( 
                                
                                column(7, style='margin-left: 50px;', 
                                       sliderLabelInput(inputId = 'score2ei', 
                                                        label = div("11. Do the proposed activities and results demonstrate innovation and potential for transformative results at scale?"), 
                                                        choices = score2eiList,
                                                        selected = score2eiList[1],
                                                        grid = FALSE,
                                                        width = '80%'),
                                       h5(tags$em('Note:'),'Through ICI, aiming to demonstrate potential for large-scale conservation results through investment in IPLC leadership.'),
                                h5(tags$em('Scoring:')),
                                h5(tags$ul(
                                  tags$li('None demonstrated;'),
                                  tags$li('Low demonstrated potential;'),
                                  tags$li('Moderate demonstrated potential;'),
                                  tags$li('Medium-high demonstrated potential;'),
                                  tags$li('High demonstrated potential;'),
                                  tags$li('Exceptional demonstrated potential')))),
                                column(1),
                                column(4,
                                       wellPanel(
                                         h5(tags$strong("Source in EoI:")),
                                         h5(tags$ul(
                                           tags$li("Section 2 - overall")),
                                           style = "padding: 2%;", width = "100%", height  = "100%")
                                       )),

                                column(12, textAreaInput("evidence2ei", "Reviewer Justification for Score", "", width = "600px"))),
                                
                                
                              fluidRow( 
                               wellPanel(
                                  h5(tags$strong("TOTAL POINTS SECTION 2:")),
                                  column (2, verbatimTextOutput("scoreSection2")),
                                  br(),
                                  br(),
                                  style = "padding: 2%;", width = "100%", height  = "100%"),
                                
                                
                                br(),
                                fluidRow( 
                                  column(12, align = "center",
                                         tags$a(onclick="customHref('panel-2')",
                                                actionButton("action5_",tags$strong("Back"), style = "background-color: #ECF0F1; color: #6c7985;")),
                                         HTML('&emsp;'),
                                         tags$a(onclick="customHref('panel-4')",
                                                actionButton("action5",tags$strong("Section 3"), style = "background-color: #ECF0F1; color: #6c7985;")))),
                                br(),
                                br(),
                                
                              )),
 
 
 
#######SECTION 3
 
 
 tabPanel("Section 3", value = "panel-4",
          tagList(  
            tags$head(
              tags$script(type="text/javascript", src = "js/index2.js"),
              tags$style(type = "text/css", ".navbar {margin-bottom: 20px;}"),
              tags$style(type = "text/css", ".container-fluid .navbar-header 
                                        .navbar-brand {margin-left: 0px;}"),
              tags$style(type='text/css', ".irs-single {font-size: 12pt; display: block;}"),
              tags$style(type='text/css', ".control-label {font-size: 17px;}"),
              tags$style(HTML("hr {border-top: 1px solid #2C3E50;}")))),
          chooseSliderSkin(skin = "Flat"),
          
          h3(textOutput("eoiNum3")),
          
          h3("Section 3 - Qualifications and experience of the Organization (Total Points: 30)", style = "font-style: bold"),  
          br(),

          h4(tags$strong("A) Indigenous Peoples or Local Community organization legally recognized under national laws. ")),
          br(), 
          fluidRow( 

            column(7,  style='margin-left: 50px;',
                   sliderLabelInput(inputId = 'score3ai', 
                                    label = div("1. Is the EoI led by an IPLC organization?"), 
                                    choices = score3aiList,
                                    selected = score3aiList[1],
                                    grid = FALSE,
                                    width = '80%'),
                   h5(tags$em('Note:'),'A key objective of ICI is to increase the level of investment going to IPLC organizations for conservation initiatives led by them. While EoIs can involve NGOs, EoIs should demonstrate IPLC leadership in defining the approach, results and activities and EoIs submitted by NGOs should include plans for capacity-building over the project term.'),
                   h5(tags$em('Scoring:')),
                   h5(tags$ul(
                     tags$li('IPLC appear to be beneficiaries only;'),
                     tags$li("Combination/partnership of IPLC organizations and NGOs, and plans to build IPLC capacity over the project term are clear;"),
                     tags$li('IPLC-led approach, NGOs in more limited, defined roles (such as fiduciary);'),
                     tags$li('Fully IPLC composed and led approach')))),
            column(1),
            column(4,
                   wellPanel(
                     h5(tags$strong("Source in EoI:")),
                     h5(tags$ul(
                       tags$li("General Information Section (lead/submitting organization) and Question 20, with consideration to the overall EoI approach"),
                       tags$li("A 'yes' to IPLC prepared (in General Information) is not sufficient to establish IPLC leadership")),
                       style = "padding: 2%;", width = "100%", height  = "100%")
                   )),
            
            column(12, textAreaInput("evidence3ai", "Reviewer Justification for Score", "", width = "600px"))),
          hr(),
          br(),
          h4(tags$strong("B) Demonstrated on the ground leadership related to Indigenous Peoples and/or Local Community Conservation.")),

          
          br(), 
          fluidRow( 

            column(7, style='margin-left: 50px;', 
                   sliderLabelInput(inputId = 'score3bi', 
                                    label = div("2. Does the lead proponent demonstrate on-ground leadership relevant to the proposed work?"), 
                                    choices = score3biList,
                                    selected = score3biList[1],
                                    grid = FALSE,
                                    width = '80%'),
                   h5(tags$em('Scoring:')),
                   h5(tags$ul(
                     tags$li('None demonstrated;'),
                     tags$li("Limited demonstration of relevant on-ground leadership;"),
                     tags$li('Demonstrated on-ground leadership relevant to the proposed work;'),
                     tags$li('Exceptional and long-standing on-ground leadership relevant to the proposed work')))),
            column(1),
            column(4,
                   wellPanel(
                     h5(tags$strong("Source in EoI:")),
                     h5(tags$ul(
                       tags$li("Question 20, 24, 29)"),
                       tags$li("Consider proponent role in existing IPLC initiatives (EoI Question 4) and General Information Section")),
                       style = "padding: 2%;", width = "100%", height  = "100%")
                   )),

            column(12, textAreaInput("evidence3bi", "Reviewer Justification for Score", "", width = "600px"))),
          
          hr(),
          br(),
          h4(tags$strong("C) Proven relevant experience in working with IPLC networks, alliances and organizations/ strength of partnerships on the ground. ")),
          
          br(), 
          fluidRow( 
            column(7, style='margin-left: 50px;', 
                   sliderLabelInput(inputId = 'score3ci', 
                                    label = div("3. Does EoI demonstrate that the lead proponent has strong partnerships, particularly with other IPLC organizations, to carry out the work?"), 
                                    choices = score3ciList,
                                    selected = score3ciList[1],
                                    grid = FALSE,
                                    width = '80%'),
                   h5(tags$em('Scoring:')),
                   h5(tags$ul(
                     tags$li('No partners defined;'),
                     tags$li('No IPLC partners identified;'),
                     tags$li('IPLC organizations are listed as implementing partners but without clear scope (roles in project design or governance);'),
                     tags$li('IPLC organizations are listed as implementing partners with clear roles (in project design or governance);'),
                     tags$li("Strong IPLC partnerships that play a central role in design, governance, and implementation of the project;"),
                     tags$li('Strong IPLC partnerships have a central role in design, governance and implementation of the project and linkages with national or regional IPO networks')))),
            column(1),
            column(4,
                   wellPanel(
                     h5(tags$strong("Source in EoI:")),
                     h5(tags$ul(
                       tags$li("Question 21")),
                       style = "padding: 2%;", width = "100%", height  = "100%")
                   )),
            
            column(12, textAreaInput("evidence3ci", "Reviewer Justification for Score", "", width = "600px"))),
          
          hr(),
          br(),
          h4(tags$strong("D) Technical expertise and capacity to address environmental problems, root causes and barriers.")),

          br(), 
          fluidRow( 
            column(7, style='margin-left: 50px;', 
                   sliderLabelInput(inputId = 'score3di', 
                                    label = div("4. Does EoI demonstrate technical capacity of lead proponent and partners to deliver the proposed results?"), 
                                    choices = score3diList,
                                    selected = score3diList[1],
                                    grid = FALSE,
                                    width = '80%'),
                   h5(tags$em('Scoring:')),
                   h5(tags$ul(
                     tags$li('No skills demonstrated;'),
                     tags$li('The skills and experiences outlined have little or no relation to the project activities;'),
                     tags$li('There is some lack of clarity or some gaps in the capacities necessary to implement the project;'),
                     tags$li('The activities clearly show how they plan to fill capacity gaps over the course of the project;'),
                     tags$li("They seem to have adequate skills and capacity for the project but do not have experience with GEF projects;"),
                     tags$li('The lead organization and project partners clearly communicate that they have all the skills and experience necessary to implement the project activities. Also, have past experience with GEF funded projects.')))),
            column(1),
            column(4,
                   wellPanel(
                     h5(tags$strong("Source in EoI:")),
                     h5(tags$ul(
                       tags$li("Question 20, 27, 29")),
                       style = "padding: 2%;", width = "100%", height  = "100%")
                   )),

            column(12, textAreaInput("evidence3di", "Reviewer Justification for Score", "", width = "600px"))),
          
          hr(),
          br(),
          h4(tags$strong("E) Project Management capacity.")),

          br(), 
          fluidRow( 
            column(7,style='margin-left: 50px;', 
                   sliderLabelInput(inputId = 'score3ei', 
                                    label = div("5. Does the EoI demonstrate project & financial management capacity needed for scale of proposed effort?"), 
                                    choices = score3eiList,
                                    selected = score3eiList[1],
                                    grid = FALSE,
                                    width = '80%'),
                   h5(tags$em('Note:'),'Minimum levels for full management capacity are that the organization has diversified funding streams, at least one project over $200,000, and annual external audits.'),
                   h5(tags$em('Scoring:')),
                   h5(tags$ul(
                     tags$li('Very limited (no criteria met);'),
                     tags$li('Some capacity but would require support (1/3 criteria);'),
                     tags$li('Moderate capacity (2/3 criteria met);'),
                     tags$li('Very strong (all criteria met) with demonstrated past performance')))),
                   column(1),
                   column(4,
                          wellPanel(
                            h5(tags$strong("Source in EoI:")),
                            h5(tags$ul(
                              tags$li("Question 22, 23, 25, 26"),
                              tags$li("From EoI entries on similar projects (Question 18) and past projects (Question 29)")),
                              style = "padding: 2%;", width = "100%", height  = "100%")
                          )),
                   
                   column(12, textAreaInput("evidence3ei", "Reviewer Justification for Score", "", width = "600px"))),
          br(),
           fluidRow( 
             column(7,style='margin-left: 50px;',       
                   sliderLabelInput(inputId = 'score3eii', 
                                    label = div('6. Does lead organization have experience with safeguards and other standards required by GEF?'), 
                                    choices = score3eiiList,
                                    selected = score3eiiList[1],
                                    grid = FALSE,
                                    width = '80%'),
                   h5(tags$em('Scoring:')),
                   h5(tags$ul(
                     tags$li('Answered no;'),
                     tags$li('Answered yes but with weak or lacking explanation to the extent;'),
                     tags$li('Answered yes with clear explanation of the extent')))),
            column(1),
            column(4,
                   wellPanel(
                     h5(tags$strong("Source in EoI:")),
                     h5(tags$ul(
                       tags$li("Question 28")),
                       style = "padding: 2%;", width = "100%", height  = "100%")
                   )),

            column(12, textAreaInput("evidence3eii", "Reviewer Justification for Score", "", width = "600px"))),
          

          fluidRow( 
            
            wellPanel(
              h5(tags$strong("TOTAL POINTS SECTION 3:")),
              column (2, verbatimTextOutput("scoreSection3")),
              br(),
              br(),
              style = "padding: 2%;", width = "100%", height  = "100%")),
            
            
            
            
            
            br(),
            fluidRow( 
              column(12, align = "center",
                     tags$a(onclick="customHref('panel-3')",
                            actionButton("action7_",tags$strong("Back"), style = "background-color: #ECF0F1; color: #6c7985;")),
                     HTML('&emsp;'),
                     tags$a(onclick="customHref('panel-5')",
                            actionButton("action7",tags$strong("Review"), style = "background-color: #ECF0F1; color: #6c7985;")))),
            br(),
            br()
            
            
          ),
 
         

tabPanel("Review", value = "panel-5",
         tagList(  
           tags$head(
             tags$script(type="text/javascript", src = "js/index2.js"),
             tags$style(type = "text/css", ".navbar {margin-bottom: 20px;}"),
             tags$style(type = "text/css", ".container-fluid .navbar-header 
                                        .navbar-brand {margin-left: 0px;}"),
             tags$style(HTML("hr {border-top: 1px solid #2C3E50;}")))),
         
         
         h3(textOutput("eoiNum4")),
         
         h3("Review and Submit EoI", style = "font-style: bold"),  
   
         
         
         fluidRow( 
           column(6,
           wellPanel(
             column(12, h5(tags$strong("TOTAL POINTS SECTION 1 - 3:"))),
             column (3, h5("Section 1"),
                     br(),
                     h5("Section 2"),
                     br(),
                     h5("Section 3")),
             column (3, verbatimTextOutput("scoreSection11") ,
                     verbatimTextOutput("scoreSection22"),
                     verbatimTextOutput("scoreSection33"),
                     br()),

             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             
          
             style = "padding: 2%;", width = "100%", height  = "100%")),
           
           column(6,
           wellPanel(
             column(12, h5(tags$strong("TOTAL POINTS:"))),
             
             column (3,
                     verbatimTextOutput("scoreSection1_3")),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             style = "padding: 2%;", width = "100%", height  = "100%")),
         
           br(),
           column(12, textAreaInput("finalComment", "Final Comments", "", width = "800px")),
           fluidRow( 
          useShinyalert(),
             
             
             column(12, align = "center",
                    actionButton("submitNow", "Submit EoI Assessment", width = "25%"
                    ))),
          #  fluidRow( 
          # column(12, align = "center",
          #        actionButton("downloadNow", "Download EoI Assessment", width = "25%"
          #        ))),
          
           br(),
           
          
           
         )),
                 
##tabPanel("Land Defenders", value = "page-6",
##                          tags$head(
##                            tags$style(type = "text/css", "#map-map {height: calc(100vh - 80px) !important;}")),
##                        column(12,  
##                          mainPanel(
##                            DT::dataTableOutput("tableLD"), style = "width: 100%"
##                          ))
##                 ),

navbarMenu("Data",
           
##Biodiversity and GEBs
tabPanel("Biodiversity and GEBs", value = "panel-6",
        
        fluidRow(
          tags$iframe(
           ## seamless = "seamless",
            scrolling = "auto",
            src="https://docs.google.com/document/d/e/2PACX-1vQty8cwWkPxAsPI0jQBN46MSbISbhtBNIfPcJOKCY-iOuY5Uy8LzavF71ciuyhoajA3V1s0V1gaKysa/pub?embedded=true&chrome=false", 
            style='width:100vw;height:100vh;'
          ),
          
        )
),    
##Threats/Root Causes

tabPanel("Threats/Root Causes", value = "panel-6_1",
         
         fluidRow(
           tags$iframe(
             ## seamless = "seamless",
             scrolling = "auto",
             src="https://docs.google.com/document/d/e/2PACX-1vRV7uR3OHm_PuUXgVLOBg69oJlg_7TisA2hOuIwm-8g_jBw4ao78AJQQnyDdpt1d-EtDvfiT0whdx62/pub?embedded=true&chrome=false", 
             style='width:100vw;height:100vh;'
           ),
           
         )
),

##Legal Framework, Scale of Recognition, & Government Willingness
tabPanel("Legal Framework, Recognition, & Government Context", value = "panel-6_2",
         
         fluidRow(
           tags$iframe(
             ## seamless = "seamless",
             scrolling = "auto",
             src="https://docs.google.com/document/d/e/2PACX-1vSuUvHxmnOoRRwPVHZWIHSYhDTrV10DLsOF90PIvVJhF76da_CiNMrsgOR2Nheo9mfelP2ZVhLLIIeQ/pub?embedded=true&chrome=false", 
             style='width:100vw;height:100vh;'
           ),
           
         )
),
           


tabPanel("CBD", value = "panel-7",
                           tags$head(
                             tags$style(type = "text/css", "#map-map {height: calc(100vh - 80px) !important;}")),
         column(12,
                h5("Analysis and documentation of the Third National Reports on Biodiversity to the CBD. The following data was produced using the reports analyzer to compare relevant IPLC oriented questions between key geography countries. Some countries are not included due to gaps in coverage of the CBD reports analyzer tool."),  
                h5("13 relevant questions from the report are documented below, with additional information included in the linked reports. Scores were calculated based on question responses, and then ranked using quartiles within the ICI key geography countries, as follows:"),  
                h5(tags$ul(
                  tags$li("<25th Percentile Red (0-9);"),
                  tags$li("25th to 75th Percentile Yellow (10-16);"),
                  tags$li(">75th Percentile (17-24)")))),

                             column(12,  
                                    mainPanel(
                                      DT::dataTableOutput("tableCBD2"), style = "width: 100%"
                                    )),
                             
                            column(12,  
                                  mainPanel(
                                    DT::dataTableOutput("tableCBD"), style = "width: 100%"
                                  ))
                  ),
                  
tabPanel("Financing Landscape", value = "panel-8",
         tags$head(
           tags$style(type = "text/css", "#map-map {height: calc(100vh - 80px) !important;}"),
           tags$style(HTML(".shiny-output-error-validation {
                             font-weight: bold; font-size: 25px;}"))),
                                  fluidRow(
                                    
                                    column(12,
                                           plotlyOutput("financingPlot")),
                                   column(2, selectInput('x', 'X-Axis', choices = nmsX, selected = "Grant")),
                                   column(2,     selectInput('y', 'Y-Axis', choices = nmsY, selected = "Fund.Source")),
                                   column(2,   checkboxGroupInput('projRegion', 'Subset by Geographic Scope', 
                                                                  choices=c('National','Transnational'),
                                                                  selected=c('National','Transnational')
                                   )),
                                   column(12,
                                    DT::dataTableOutput("gefprojs")),  style = "width: 100%"
                                  )),

tabPanel("Ocean Health Index", value = "panel-9",
                           tags$head(
                             tags$style(type = "text/css", "#map-map {height: calc(100vh - 80px) !important;}")),
                           column(12,
                             h5("Ocean Health Index scores are calculated for each region for each goal before being combined into an overall Index score. Each goal measures the delivery of specific benefits with respect to a sustainable target. A goal is given a score of 100 if its maximum sustainable benefits are gained in ways that do not compromise the ocean's ability to deliver those benefits in the future.  Lower scores indicate that more benefits could be gained or that current methods are harming the delivery of future benefits."),  
                             h5("Below are country level assessment scores for all coastal nations and territories within the Key ICI Geographies. Flower plots illustrate the scores, with each petal representing an individual goal. Petal lengths convey the score of the goal, thus longer petals are closer to achieving their target. Only goals that are relevant to a region are scored, otherwise they are set to NA with grey petals. All scores are on a scale from 0-100, and the center number is the region's Index score."),  
                             h5("The overall global score was 70.58; the median index among countries was 67.47. Regions with index scores above 80 are considered well performing and indicated below."),  
                             
                             h3("2019 Global Index Map and Flower Plot"),  
                             
                           ),
                           
                           fluidPage(fluidRow(
                             column(6, align="center",
                                    div(style="display: inline-block;",img(src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/maps_by_goal_mol/global_map_Index_2019_mol.png", height="100%", width="100%")),
                             ),
                             column(6, align="center",
                                    div(style="display: inline-block;",img(src="https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2019/Results/figures/FlowerPlots_v2/flower_GlobalAverage.png", height="100%", width="100%"))),
                           
                           column(12,
                                  h3("2019 Country Scores"),  
                                  
                           )),
                          
                        fluidRow(    
                           column(12,  
                                  mainPanel(
                                    DT::dataTableOutput("ohi"), style = "width: 100%"
                                  )),
                           column(12,
                                  
                                  shiny::HTML("<p><span style='color: grey'>Ocean Health Index. 
                                              2020. ohi-global version: Global scenarios data for Ocean Health Index. National Center for Ecological Analysis and Synthesis, University of California, Santa Barbara. 
                                              Available at: https://github.com/OHI-Science/ohi-global/releases</span></p>")

                           )
                           
                           
                  )))
))
                 
                 
                 
           



server = function(input, output, session) {

###Select Inputs  
  output$var_select<-renderUI({
    selectInput("ind_var_select","Geography-Country covered by EoI",
                choices =c(list("",
                  "East Africa Drylands" = list("Burundi", "Djibouti", "Ethiopia", "Kenya", "Rwanda", "Tanzania", "Uganda"),
                                "Coastal East Africa" = list("Somalia", "Kenya", "Tanzania", "Mozambique", "South Africa"),
                                "Congo Basin" = list("Cameroon", "Central African Republic", "DR Congo", "Republic of the Congo", "Equatorial Guinea", "Gabon"),
                                "Mesoamerica" = list("Mexico", "Guatemala", "Honduras", "Belize", "El Salvador", "Nicaragua", "Panama", "Costa Rica"),
                                "Andes/Amazon" = list("Colombia", "Ecuador", "Peru", "Brazil", "Bolivia", "Guyana", "Suriname"),
                                "Southern Cone" = list("Argentina", "Chile"),
                                "Gran Chaco" = list("Bolivia", "Paraguay", "Argentina", "Chile"),
                                "Himalayas" = list("Nepal", "Bhutan", "India", "Pakistan"),
                                "South East Asia (Mainland)" = list("Cambodia", "Laos", "Thailand", "Vietnam", "Myanmar", "Peninsular Malaysia"),
                                "South East Asia (Islands)" = list("Indonesia", "Island Malaysia", "Philippines", "East Timor"),
                                "Melanesia" = list("Vanuatu", "Solomon Islands", "Fiji", "Papua New Guinea", "West Papua"),
                                "Polynesia" = list("Easter Island", "Samoa", "Tonga", "Cook Islands", "Tuvalu", "Tokelau", "Niue", "Wallis", "Futuna"))),
                multiple = FALSE, selected = NULL)
  })
  
  
  ##Req eoi #


  output$eoiNum <- renderText({ 
    req(input$eoiNumber)
    req(input$submittingOrg)
    paste("Reviewing: EOI #", input$eoiNumber, "from", input$submittingOrg)
  })
  
  output$eoiNum2 <- renderText({ 
    req(input$eoiNumber)
    req(input$submittingOrg)
    paste("Reviewing: EOI #", input$eoiNumber, "from", input$submittingOrg)
  })
  
  output$eoiNum3 <- renderText({ 
    req(input$eoiNumber)
    req(input$submittingOrg)
    paste("Reviewing: EOI #", input$eoiNumber, "from", input$submittingOrg)
  })
  
  output$eoiNum4 <- renderText({ 
    req(input$eoiNumber)
    req(input$submittingOrg)
    paste("Reviewing: EOI #", input$eoiNumber, "from", input$submittingOrg)
  })
  
  
###Score Totals
  
  scoreSection1 <- reactive({
    scoreSection1 <- input$score1ai + input$score1aii + input$score1bi + input$score1bii + input$score1ci + input$score1di + input$score1dii + input$score1diii +input$score1ei
  })
  output$scoreSection1 <- renderText(scoreSection1())
 

  scoreSection2 <- reactive({
    scoreSection2 <- input$score2ai + (input$score2aii*2) + input$score2aiii +input$score2aiv + input$score2av + input$score2bi + input$score2bii + input$score2biii + input$score2ci + input$score2di +input$score2ei
  })
  output$scoreSection2 <- renderText(scoreSection2())

  scoreSection3 <- reactive({
    scoreSection3 <- (input$score3ai*2) + (input$score3bi*2) + input$score3ci +input$score3di + (input$score3ei*2) + input$score3eii
  })
  output$scoreSection3 <- renderText(scoreSection3())
  

  scoreSection11 <- reactive({
    scoreSection11 <- input$score1ai + input$score1aii + input$score1bi + input$score1bii + input$score1ci + input$score1di + input$score1dii + input$score1diii +input$score1ei
  })
  output$scoreSection11 <- renderText(scoreSection11())
  
  
  scoreSection22 <- reactive({
    scoreSection22 <- input$score2ai + (input$score2aii*2) + input$score2aiii +input$score2aiv + input$score2av + input$score2bi + input$score2bii + input$score2biii + input$score2ci + input$score2di +input$score2ei
  })
  output$scoreSection22 <- renderText(scoreSection22())
  
  

  scoreSection33 <- reactive({
    scoreSection33 <- (input$score3ai*2) + (input$score3bi*2) + input$score3ci +input$score3di + (input$score3ei*2) + input$score3eii
  })
  output$scoreSection33 <- renderText(scoreSection33())
  
  scoreSection1_3 <- reactive({
    scoreSection1_3 <- input$score1ai + input$score1aii + input$score1bi + input$score1bii + input$score1ci + input$score1di + input$score1dii + input$score1diii +input$score1ei + input$score2ai + (input$score2aii*2) + input$score2aiii +input$score2aiv + input$score2av + input$score2bi + input$score2bii + input$score2biii + input$score2ci + input$score2di +input$score2ei + (input$score3ai*2) + (input$score3bi*2) + input$score3ci +input$score3di + (input$score3ei*2) + input$score3eii
  })
  output$scoreSection1_3 <- renderText(scoreSection1_3())
  
  

###Reactive hyperlinks  
  locationLatLong <- reactive({
    if(!isTruthy(input$ind_var_select)) {
      return(centroids[1,])
    }
    else {
      return(centroids %>% filter(grepl(input$ind_var_select, Country)))
      
    }
  })

  ###RSR  
  output$hyperlinkRSR <- renderUI({
    lat <- locationLatLong()$lat
    long <- locationLatLong()$long
    zoom <- locationLatLong()$zoom
    link <- "https://ici.resilienceatlas.org/map?tab=layers&layers=%5B%7B%22id%22%3A1901%2C%22opacity%22%3A0.7%2C%22order%22%3A0%7D%5D&center="
    link2 <- paste0(link, "lat%3D", lat, "%26lng%3D", long, "&zoom=", zoom)
    tags$a(href = link2, paste("Species Range-Size Rarity (RSR)", input$chooseLink), target="_blank")
  })
  
  ###KBA 
  output$hyperlinkKBA <- renderUI({
    lat <- locationLatLong()$lat
    long <- locationLatLong()$long
    zoom <- locationLatLong()$zoom
    link <- "https://ici.resilienceatlas.org/map?tab=layers&layers=%5B%7B%22id%22%3A1902%2C%22opacity%22%3A1%2C%22order%22%3A0%7D%5D&center="
    link2 <- paste0(link, "lat%3D", lat, "%26lng%3D", long, "&zoom=", zoom)
    tags$a(href = link2, paste("Key Biodiversity Areas (KBAs)", input$chooseLink), target="_blank")
  })
  
  ###IFL 
  output$hyperlinkIFL <- renderUI({
    lat <- locationLatLong()$lat
    long <- locationLatLong()$long
    zoom <- locationLatLong()$zoom
    link <- "https://ici.resilienceatlas.org/map?tab=layers&layers=%5B%7B%22id%22%3A1918%2C%22opacity%22%3A1%2C%22order%22%3A0%7D%5D&center="
    link2 <- paste0(link, "lat%3D", lat, "%26lng%3D", long, "&zoom=", zoom)
    tags$a(href = link2, paste("Intact Forest Landscapes (IFLs)", input$chooseLink), target="_blank")
  })
  
  ###Carbon 
  output$hyperlinkCarbon <- renderUI({
    lat <- locationLatLong()$lat
    long <- locationLatLong()$long
    zoom <- locationLatLong()$zoom
    link <- "https://ici.resilienceatlas.org/map?tab=layers&layers=%5B%7B%22id%22%3A1908%2C%22opacity%22%3A1%2C%22order%22%3A0%7D%5D&center="
    link2 <- paste0(link, "lat%3D", lat, "%26lng%3D", long, "&zoom=", zoom)
    tags$a(href = link2, paste("Irrecoverable Carbon", input$chooseLink), target="_blank")
  })
  
  ###Cumulative Development Pressures
  output$hyperlinkCDP <- renderUI({
    lat <- locationLatLong()$lat
    long <- locationLatLong()$long
    zoom <- locationLatLong()$zoom
    link <- "https://ici.resilienceatlas.org/map?tab=layers&layers=%5B%7B%22id%22%3A1904%2C%22opacity%22%3A1%2C%22order%22%3A0%7D%5D&center="
    link2 <- paste0(link, "lat%3D", lat, "%26lng%3D", long, "&zoom=", zoom)
    tags$a(href = link2, paste("Cumulative Development Pressures", input$chooseLink), target="_blank")
  })
  
  ###Global Forest Change 2000-2019
  output$hyperlinkGFW <- renderUI({
    lat <- locationLatLong()$lat
    long <- locationLatLong()$long
    zoom <- locationLatLong()$zoom
    link <- "https://ici.resilienceatlas.org/map?tab=layers&layers=%5B%7B%22id%22%3A745%2C%22opacity%22%3A1%2C%22order%22%3A0%7D%5D&center="
    link2 <- paste0(link, "lat%3D", lat, "%26lng%3D", long, "&zoom=", zoom)
    tags$a(href = link2, paste("Global Forest Change 2000-2019", input$chooseLink), target="_blank")
  })
  
  ###Neighboring land acquisition (land deals)
  output$hyperlinkGLD <- renderUI({
    lat <- locationLatLong()$lat
    long <- locationLatLong()$long
    zoom <- locationLatLong()$zoom
    link <- "https://ici.resilienceatlas.org/map?tab=layers&layers=%5B%7B%22id%22%3A1903%2C%22opacity%22%3A1%2C%22order%22%3A0%7D%5D&center="
    link2 <- paste0(link, "lat%3D", lat, "%26lng%3D", long, "&zoom=", zoom)
    tags$a(href = link2, paste("Neighboring land acquisition (land deals)", input$chooseLink), target="_blank")
  })
  
  ###Formally recognized tenure
  output$hyperlinkFRTenure <- renderUI({
    lat <- locationLatLong()$lat
    long <- locationLatLong()$long
    zoom <- locationLatLong()$zoom
    link <- "https://ici.resilienceatlas.org/map?tab=layers&layers=%5B%7B%22id%22%3A1913%2C%22opacity%22%3A1%2C%22order%22%3A0%7D%2C%7B%22id%22%3A1920%2C%22opacity%22%3A1%2C%22order%22%3A1%7D%5D&center="
    link2 <- paste0(link, "lat%3D", lat, "%26lng%3D", long, "&zoom=", zoom)
    tags$a(href = link2, paste("Presence of formally recognized IPLC lands", input$chooseLink), target="_blank")
  })
  
  ###Not Formally recognized tenure
  output$hyperlinkNFRTenure <- renderUI({
    lat <- locationLatLong()$lat
    long <- locationLatLong()$long
    zoom <- locationLatLong()$zoom
    link <- "https://ici.resilienceatlas.org/map?tab=layers&layers=%5B%7B%22id%22%3A1964%2C%22opacity%22%3A1%2C%22order%22%3A0%7D%2C%7B%22id%22%3A1915%2C%22opacity%22%3A1%2C%22order%22%3A1%7D%2C%7B%22id%22%3A1963%2C%22opacity%22%3A1%2C%22order%22%3A2%7D%5D&center="
    link2 <- paste0(link, "lat%3D", lat, "%26lng%3D", long, "&zoom=", zoom)
    tags$a(href = link2, paste("Presence of not formally recognized IPLC lands", input$chooseLink), target="_blank")
  })


  
###CBD  
  subsetcbd2 <- reactive({
    subsetcbd2 <- filter(if (is.null(input$ind_var_select)){
      cbd
    }  else {
      cbdScores %>% filter(grepl(input$ind_var_select, Country))
    } 
    )
    
  })
  
  
  
  subsetcbd <- reactive({
    subsetcbd <- filter(if (is.null(input$ind_var_select)){
      cbd
    }  else {
      cbd %>% filter(grepl(input$ind_var_select, Country))
    } 
    )
    
  })
  
  output$tableCBD <- DT::renderDataTable(
    subsetcbd() , escape = FALSE, rownames = FALSE,
    caption = 'Table 2: CBD Third National Report (NR3).'
    
  )
  
  
  
  
  
  output$tableCBD2 <- DT::renderDataTable(
    datatable(subsetcbd2(), escape = FALSE, rownames = FALSE, 
              caption = 'Table 2: Scoring of CBD National Report 3.',
              options = list(
                pageLength = 5
              )) %>%
      formatStyle(
        'Score',
        target = 'row',
        backgroundColor = styleEqual(c(1, 2, 3, 4, 5, 6, 7, 8, 9,
                                       10, 11, 12, 13, 14, 15, 16,
                                       17, 18, 19, 20, 21, 22, 23, 24), 
                                     c('red', 'red', 'red', 'red', 'red', 'red', 'red', 'red', 'red',
                                       'yellow', 'yellow','yellow','yellow','yellow','yellow','yellow',
                                       'green', 'green', 'green', 'green', 'green', 'green', 'green', 'green'))
      ) %>%
      formatStyle(names(subsetcbd2()), # select all columns in table
                  height = '100', 
                  'text-align' = 'center')
  ) 

  

  
  
###GEF  
  
  
  
  dataplot <- reactive({
    dataplot <- filter(if (is.null(input$ind_var_select)){
      gefproj
    }  else {
      gefproj %>% filter(grepl(input$ind_var_select, Countries))
    } 
    )
    
    
  })
  
  subsetScope <- reactive({
    validate(
      need( (!is.null(input$projRegion)), "No current financing data in database for the selected country.")
    )
    subset(dataplot(), subset = Scope %in% input$projRegion)
  })

  
  
  sumProjects <- reactive({
    sumProjects <-subsetScope()[[input$x]] %>% sum()
    label_number_si(accuracy=0.1)(sumProjects)
  })

  
  output$financingPlot <- renderPlotly({
    
    finalFunding <- subsetScope()
    finalFunding <- finalFunding %>% group_by(!!as.name(input$y)) %>% mutate(Total = as.numeric(sum(!! as.name(input$x))))
    validate(
      need( nrow(finalFunding) > 0, "No current financing data in database for the selected country.")
    )
      p <- ggplot(finalFunding, aes_string(x = input$x, y = input$y, fill = input$y)) + 
      geom_col(aes(text=paste('Total:', label_number_si(accuracy=.1)(finalFunding$Total))), position = position_stack(), alpha = 0.8) +
      scale_x_continuous(labels = label_number_si(accuracy=.1), expand = c(0, 0)) +
      labs(title = paste0("Current Cofinancing Landscape for ", input$ind_var_select, " <br /> (", length(subsetScope()$Project), " Projects - ", "US$", sumProjects(), ")"),
           x = "Funding (US$)",
           y = "")+
      (theme_grey() %+replace% theme(plot.caption = element_custom())) +
      theme(plot.title = element_text(face = "bold", size = 20, color = "#1b3955"),
            axis.text=element_text(size=12),
            axis.title = element_text(size = 12, face = "italic"),
            plot.subtitle = element_text(size=14, color = "black"),
           ## plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
            panel.grid.minor = element_blank(),
            legend.position = "none") +
      theme(legend.title = element_blank())
    
    
    toWebGL(ggplotly(p, tooltip = "text")) %>% 
      config(displaylogo = FALSE, 
             toImageButtonOptions = list(
               format = "png",
               filename = "financingPlot",
               width = 2400,
               height = 600
             ),
             modeBarButtonsToRemove = c(
               'sendDataToCloud',
               'select2d',
               'autoScale2d',
               'hoverClosestCartesian',
               'hoverCompareCartesian',
               'lasso2d',
               'zoomIn2d',
               'zoomOut2d',
               'pan2d'
             )) %>%
      layout(autosize = TRUE, margin = list(t = 100))
  })
  
  
  output$gefprojs <- DT::renderDataTable(
    datatable(subsetScope(), escape = FALSE, rownames = FALSE, 
    filter = list(position = 'top', clear = FALSE),
    options = list(
      search = list(regex = TRUE, caseInsensitive = FALSE),
      pageLength = 10
    ), 
    caption = 'Table 3: National or Transnational Projects.')%>%
      formatCurrency(c('Grant', 'Cofinancing'), digits = 0)
    ) 
  
  
  
###Ocean Health Index 
  
  output$ohi <- DT::renderDataTable({
    DT::datatable(dat[,1:2]
                  [dat$country %in% input$ind_var_select,], 
                  escape = FALSE,
                  rownames = FALSE,
                  caption = 'Table 4: Ocean Health Index.'
    ) %>% formatStyle(names(dat), backgroundColor = "white")
  })
  
  
  observeEvent(input$submitNow, {
    shinyalert(
      title = "Review Submitted",
      text = "Please refesh the page to review another EoI.",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "success",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
    }) 
  
  
  
  observeEvent(input$action1, {
    if(!isTruthy(input$eoinumber)) {
      return(
    shinyalert(
      title = "Missing EoI number",
      text = "Please return and add an EoI number on the introduction page before submitting.",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )) }
    else {return(shinyalert(
      title = " EoI number",
      text = "Please return and add an EoI number on the introduction page before submitting.",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    ))}
  })
  
### When the Submit button is clicked, save the form data
  observeEvent(input$submitNow, {
    saveData(input)
  })

  observeEvent(input$action3, {
    saveData(input)
  })
  
  observeEvent(input$action5, {
    saveData(input)
  })

  
  observeEvent(input$action7, {
    saveData(input)
  })
  
}

shinyApp(ui = ui, server = server)

