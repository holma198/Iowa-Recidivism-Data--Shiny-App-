library(shiny)

library(scales)

#OPTION 2: try and redo what was done in step 3. 

# do first: dropdown with district, and recidivism response (A sort of intro)

# fluid row grid example instead of side

# At end have a simple recidivism plot with all of the factors involved.

########### do this for a select all option

iowaR = read.table(file="iowaRecidivism.csv",header=TRUE,sep=',', 
                   na.strings = c("", " ","N/A", "N/A -  ", "N/A - ", "N/A-"))

shinyUI(fluidPage(
  
  fixedPanel(
    h1("Iowa Recidivism Data"),
    h4("By RJ Holman")
    ),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
    fluidRow( 
      column(4,
        fixedPanel( style = "z-index: 5;",
          wellPanel(
        selectInput("districtDropD", choices=c("All", levels(iowaR$Main.Supervising.District)), 
                    label = h4("Select Supervising District"))
          )
        ),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        
        fixedPanel( style = "z-index: 4;",
          wellPanel(
          selectInput("ReleaseDropD", choices=c("All",levels(iowaR$Release.Type)[3:12]), 
                      label = h4("Select Release Type of Offender"))
          )
        ),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        fixedPanel( style = "z-index: 3;",
                    wellPanel(
                      selectInput("OffenseDropD", choices=c("All",levels(iowaR$Convicting.Offense.Subtype)), 
                                  label = h4("Select Offense Type of Offender"))
                    )
        ),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        fixedPanel( style = "z-index: 2;",
                    wellPanel(
                      selectInput("GenOffenseDropD", choices=c("All",levels(iowaR$Convicting.Offense.Type)), 
                                  label = h4("Select General Offense Type of Offender"))
                    )
        ),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        fixedPanel( style = "z-index: 1;",
                    wellPanel(
                      selectInput("AgeDropD", choices=c("All",levels(iowaR$Age.At.Release)), 
                                  label = h4("Select Age group of Offender (At Release)"))
                    )
        )
      ),
  
      column(8,
    h1("Recidivism in Iowa Prisons", style = "text-align: center;"),
    br(),
    h3("RJ Holman", style = "text-align: center;"),
    br(),
    h2("Getting started"),
    h4("This is an investigation in Iowa prison data.  This page may not scale well on smaller
       displays, zooming out (ctrl) may be necessary for proper viewing.  The drop-down menus
       on the left are the 'levels' for each factor variable in this set of data.  Select a 
       desired level to specify the level, this is necessary for most of the plots."),
    h2("Some background"),
    h4("The following data is retrieved from the Iowa Department of Corrections.  
       This data focuses on recidivism in Iowa within 3 years of prisoner release.  
       The recidivism reporting year is the fiscal year (year ending June 30) marking the end of the three year tracking period.  
       The dataset is updated every year, and this data set includes the years 2013-2016.  
       The state of Iowa has no operating private prisons, so this dataset covers only public institutions.
       This data is available at 'https://data.iowa.gov/Public-Safety/3-Year-Recidivism-for-Offenders-Released-from-Pris/mw8r-vqy4'.
       "),
    br(),
    h2("The Goal"),
    h4("After this investigation, there will idealy be a deeper understanding of what causes recidivism within these prisons.  
      Within the state of Iowa there are several 'supervising districts' that contain certain groups of offenders.  
The second main goal is understanding what makes the recidivism rates within the districts differ.  These goals will be 
achieved by first looking at each of the factors in the sidebar, individually, then looking at all of the factors together.  
In the data generalization step, it was determined that Release Types, Offense Types, and Age ranges were among the most influential
 in determining recidivism.  These will be the factors looked into for this project.  The below graph looks into the amount of recidivisim, offenders who commit crimes after being released from prison, 
       in order to see which of the main supervising districts contain the most and least cases of recidivism."),
    br(),
    h2("Recidivism in Iowa Prisons"),
    plotOutput("recidivismDistPlot"),
    
    br(),
    h4("Notice how 5JD has the largest portion of recidivism cases, and ISC and Interstate
       Compact have much less than the typical amount of recidivism, but also note that these districts have
much less than the average amount of offenders.  Below will be a series of graphs
       explaining some of the differences found in each district."),
    br(),
    h2("District VS Release type"),
    plotOutput("releaseTypePlot"),
    
    h2("Recidivism for Release Type"),
    plotOutput("releaseRecidPlot"),
    
    h4("The first graph displays what percentage of the selected release type Occurs
       in the selected district (Ex. ISC had over 80% Parole Release Types).  The second plot
       displays the amount of recidivism cases tied to that release type.  What I discovered in
       Step 3 of the project was that Parole release types were much higher for the suspect districts, however without
       the linear model and focusing specifically on the percentage of release types per district 
       reveals that only ISC and Interstate Compact were out of the ordinary for percentages of these release types.
         This appears to be the most influential release type for this dataset, as all districts have
       a majority of these release types.  ISC and Interstate Compact had almost exclusively this release type
       , and as shown in the bottom graph (and discovered in part 3) this release type is not associated with
       too much recidivism.  The second most common release type for these districts would
       be Parole Granted releases, which have an increased proportion of recidivism, IC and ISC completely lack
       this release type.  This explains some of why ISC and IC had less than average recidivism."),
    br(),
    
    br(),
    h2("Offense Type vs. District"),
    br(),
    plotOutput("districtVcrime"),
    
    h5("This plot shows how offenders with a selected offense type are distributed among the districts.
         Since the districts have differing numbers of inmates, the below graph, which shows proportion
         of offense types per district, the makeup of the districts themselves, may be more helpful."),
    br(),
    plotOutput("crimePercentByDistrict"),
    
    h5("This plot shows the percent of each offense type for each district"),
    br(),
    h2("Offense Type vs. Recidivism"),
    br(),
    plotOutput("offenseTypeVrecidivism"),
    
    h5("This is a visual representation for offense types and associated recidivsm."),
    br(),
    h4("With so many different levels for offense types, it gets more difficult to find solid patterns.
        A noticible pattern would be Drug possession and Alchohol offenders, both types of offenses have
        much lower values for ISC and Interstate Compact, while 5JD and some of the more average districts
        have much more than ISC and Interstate Compact.  Sex crimes appear to have less recidivism cases, 
        Interstate Compact has more of a share of this offense type than average, and 5JD has very little.  
        Special sentence revocation has a very high recidivism rate, although none of the districts have a
        large share of this release type, 4JD has much more than the average, which may contribute to it being
        the district with the second most cases of recidivism.  In this dataset of Iowa Prison Recidivism data, 
        there is a more general classification of offense type that can be investigated."),
    br(),
    h2("General Offense Types and Recidivism"),
    plotOutput("genDistrictVcrime"),
    h5("This plot shows how offenders with a selected general offense type are distributed among the districts.
         Since the districts have differing numbers of inmates, the below graph, which shows proportion
         of general offense types per district, the makeup of the districts themselves, may be more helpful."),
    br(),
    plotOutput("genCrimePercentByDistrict"),
    h5("This plot shows the percent of each general offense type for each district"),
    br(),
    plotOutput("genOffenseTypeVrecidivism"),
    h5("This is a visual representation for general offense types and associated recidivsm."),
    br(),
    h4("Perhaps the most interesting find with the general offense types, is that violent crimes tend
       to have less recidivism than the rest, and ISC and Interstate Compact both have the highest proportion
       of this offense type, while 5JD had the least."),
    br(),
    h2("Age Groups"),
    br(),
    plotOutput("AgeVdistrict"),
    br(),
    plotOutput("AgeVrecidivism"),
    br(),
    h4("From the recidivism data on age data it is clear that offenders released at older ages are
       less likely to commit crimes than those released at younger ages, however, there does not seem
        to be any trends among the districts regarding age.  This means that there is no particular age group advantage
         given to ISC or Interstate Compact that gave them lower recidivism rates"),
    br(),
    h1("Combining All Factors"),
    br(),
    h2("Effect in Districts"),
    h5("The plot below examines differences in recidivism accross Districts for multiple factors."),
    br(),
    plotOutput("RecidInEachDistrict"),
    h4("This plot represents the amount of recidivism in each district, with the four other
       factors taken into account.  The width of each bar represents what share of the total
       selected offender population is distributed throughout the districts.  Selecting Factors will
       filter only that selected level to be included in the population represented by the graph.
         Looking at this graph, it is apparent that recidivism tied to a specific crime can differ
        greatly, so more factors are affecting recidivism differences in 5JD, Interstate Compact and ISC."),
    br(),
    br(),
    h2("Effect on Recidivism"),
    br(),
    h5("Both of these plots are a representation of how all of the factors (Except District) affect recidivism.  
       Choosing too many at once will often yield no results, as certain combinations do not exist in this dataset.
         I would strongly caution against selecting a general and specific offense type at once, as this is either redundent
        or will yield no results."),
    br(),
    plotOutput("pieAllRecidivism"),
    br(),
    plotOutput("BarAllRecidivism"),
    br(),
    h4("This section is intended to see what causes recidivism to happen in the first place, rather 
        than focusing on the districts specifically.  After playing around with the factors, it may
        become evident that certain factors have a large impact collectively accross all the districts.  
        Special sentence, release types, for instance, has a majority of recidivism cases."),
    br(),
    h1("Conclusion"),
    h4("It is clear that many factors are at play when determining recidivism rates.  The first main goal
        was to have an understanding of what factors can affect recidivism in general.  After this investigation
        it is clear that several factors had contributed to higher recidivism rates.  Among these were Special Sentence
        Release types, Iowa Datainer Parole Release Types, to a lesser degree Parole Granted Release types.  Offenses that
        seemed to be tied to high levels of recidivism were Other Criminal, Special Sentence Revocation, and theft crime types.
       Finally, Younger criminals are much more likely to commit crimes again."),
    br(),
    h4("The second main goal was to determine why certain 
       districts differred so much in terms of recidivism.  ISC and Interstate Compact had much lower rates of recidivism, a factor
        that could be in control of the district would be release types.  Both ISC and Interstate Compact had no Parole Granted 
        release types.  This release type accounted for around 35-30% of releases in the other districts, and had been
        associated with higher recidivism rates.  ISC and Interstate Compact were able to avoid some particularly nasty release
       types, such as 'Released to Special Sentence' and Iowa Detainer releases.  Parole Releases were by far the most common, 60-70% in 
       other districts and 90%+ in ISC and Interstate Compact.  Looking at the 'Effect in the Districts' chart reveals
        that recidivism for this specific release type was also lower in these districts than others.  Clearly other factors
       are at play.  One specific crime that contributes a good deal to recidivism is Drug Possession.  Both ISC and Interstate Compact(IC)
        had a makeup of only around 1-2% of these offenders, while most other districts had a proportion of at least double that.  Looking more
       generally at types of crimes yields more compelling data.  Violent crimes had the least amount of recidivism out of the basic
        offense types.  ISC and Interstate compact had a little more than average, and 5JD (The district with the most recidivism) had the smallest
        proportion of these crime types.  While Age goups have an enormously significant effect on recidivism (As determined in the data generalization
        step through t-tests), there have been fairly consistent distribution of age groups throughout all of the districts, so it is not
        contributing to ISC and IC's low recidivism rates."),
    br(),
    h4("Overall, steps to reduce recidivism can be taken.  The most obvious area that can be improved is release types, 
       as these are completely controllable.  Certain parole releases, like Iowa Detainment and Special Sentence Release Types,
        should definately be avoided.  Another option would be targeting groups of offenders that have statistically higher 
       recidivism rates, such as younger offenders and 'Other' Offense type offenders, with programs aimed at reducing recidivism.  
        The state of Iowa has already begun a system of targeting suspect re-offenders, however, some improvements could be made."),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
    
    )
  )
  
  
))
