library(shiny)
library(shinyWidgets)

source("./battleground.R")

# Modal module UI
dlgCombatModsModuleUI <- function(id, i18n) {
    ns <- NS(id)
    actionButton(ns("btnCombatMods"), i18n$t("Combat"), icon = gicon("abacus"))
}



#' dlgCombatModsModuleServer
#' Modal module server for a dialog box that allows users to compute combat 
#' modifiers by specifying environment and opponent.
#' @param id An ID string that identifies the module and, thus,  corresponds with 
#' the ID used to call the module's UI function.
#' @param i18n a shiny.i18n translation object
#' @param Weapon An R6 WeaponBase class
#' @return
dlgCombatModsModuleServer <- function(id, i18n, WeaponName, WeaponType, WeaponRange, WeaponSkills) {
    moduleServer(
        id,

        function(input, output, session) { # Shiny module server function
            ns <- session$ns

            ModalDlgFunction <- function() {
#browser(expr = WeaponName() == "Waffenlos")
                if (isTRUE(WeaponType() == names(.WeaponType["Ranged"])))
                    WeaponsRangeChoices <- i18n$t(names(.RangedCombatRange))
                else
                    WeaponsRangeChoices <- i18n$t(names(.CloseCombatRange))
                
                Defaults <- defaultCombatEnvironment(WeaponType())
                
                modalDialog(
                    title = i18n$t("Combat Modifiers"),
                    
                    fluidRow(
                        column(4,
                               h4(i18n$t("Hero")),
                               tableOutput(ns("outWeaponDetails")),
                               hr(),
                               selectInput(
                                   ns("cmbHeroMeansOfMovement"), i18n$t("Movement"), 
                                   choices = i18n$t(names(.MeansOfMovement)),
                                   selected = i18n$t(names(Defaults$Hero$MeansOfMovement))),
                               sliderTextInput(
                                   ns("rdbHeroMovement"), label = NULL, grid = TRUE, force_edges = TRUE,
                                   choices = "-", # reactive value
                                   ),
                               wellPanel(
                                   h4(i18n$t("Values")),
                                   tableOutput(ns("outCombatModifiers"))
                               )
                        ),
                        column(4,
                               h4(i18n$t("Opponent")),
                               sliderTextInput(
                                   ns("rdbOpponentWeapon"), label = i18n$t("Weapon Reach"),
                                   choices = WeaponsRangeChoices, grid = TRUE, force_edges = TRUE),
                               sliderTextInput(
                                   ns("rdbOpponentSize"), label = i18n$t("Opponent Size"),
                                   grid = TRUE, force_edges = TRUE,
                                   choices = i18n$t(names(.TargetSize)),
                                   selected = i18n$t(names(Defaults$Opponent$TargetSize))
                               ),
                               sliderTextInput(
                                   ns("rdbOpponentDistance"), label = i18n$t("Distance"),
                                   choices = i18n$t(names(.RangedCombatRange)), 
                                   grid = TRUE, force_edges = TRUE),
                               #-textOutput("txOpponentDONOTKNOWWHATTHISISYET", inline = FALSE),
                               sliderTextInput(
                                   ns("rdbOpponentMovement"), label = i18n$t("Movement"),
                                   choices = i18n$t(names(.Movement)), grid = TRUE, force_edges = TRUE),
                               checkboxGroupInput("chbOpponentEvasive", NULL, choices = i18n$t("Evasive Maneuvers"))
                        ),
                        column(4,
                               h4(i18n$t("Environment")),
                               selectInput(
                                   ns("cmbCombatEnvVision"), i18n$t("Visibility"), 
                                   i18n$t(names(.Visibility))
                               ),
                               checkboxGroupInput(ns("cmbCombatEnvCramped"), NULL, choices = i18n$t("Cramped")),
                               selectInput(
                                   ns("cmbCombatEnvWater"), i18n$t("Water Depth"), 
                                   i18n$t(names(.UnderWater))
                              )
                        ),
                        
                    ),
                    footer = modalButton(i18n$t("btnClose")), fade = TRUE, size = "l",
                    easyClose = TRUE
                )
            }
            
            #' Enable / Disable the UI controls depnding on weapon type
            observeEvent(input$btnCombatMods, { #WeaponType(),
                ##WeaponType()
#browser()
                if (isTRUE(WeaponType() == names(.WeaponType["Ranged"]))) {
                    ToDisable <- c("cmbCombatEnvCramped", "rdbOpponentWeapon")
                    ToEnable <- c("cmbHeroMeansOfMovement", "rdbHeroMovement",
                                  "rdbOpponentDistance", "rdbOpponentMovement",
                                  "chbOpponentEvasive")
                }
                else {
                    ToEnable  <- c("cmbCombatEnvCramped", "rdbOpponentWeapon")
                    ToDisable <- c("cmbHeroMeansOfMovement", "rdbHeroMovement",
                                   "rdbOpponentDistance", "rdbOpponentMovement",
                                   "chbOpponentEvasive")
                }
                for (i in ToEnable) enable(i)
                for (i in ToDisable) disable(i)
            })
            
            
            #' 
            EffectiveValues <- reactive({
                # All_Inputs <- vapply(paste0('axis',1:3),
                #                      function(x){isTruthy(input[[x]])},
                #                      logical(1))
                if (isTruthy(WeaponType()) &&
                    isTruthy(input$cmbHeroMeansOfMovement) &&
                    isTruthy(input$rdbOpponentWeapon) &&
                    isTruthy(input$cmbCombatEnvVision)) {
    #-browser()
                    # Convert to numeric index
                    Evasive <- .EvasiveMovement[as.integer(isTruthy(input$chbOpponentEvasive)) + 1]
                    Cramped <- .CrampedSpace[as.integer(isTruthy(input$cmbCombatEnvCramped)) + 1]
                    
                    # Which options are valid? Close or ranged combat?
                    if (isTRUE(WeaponType() == names(.WeaponType["Ranged"])))
                        WeaponsRangeChoices <- i18n$t(names(.RangedCombatRange))
                    else
                        WeaponsRangeChoices <- i18n$t(names(.CloseCombatRange))

                    #TODO: risky not to use codes/ids but the translated string
                    Environment <- initCombatEnvironment(
                        Type  = ifelse(isTruthy(WeaponType()),  WeaponType(),  names(.WeaponType["Melee"])), 
                        Range = ifelse(isTruthy(WeaponRange()), WeaponRange(), names(.CloseCombatRange["Short"])), 
                        HeroMoves  = which(i18n$t(names(.MeansOfMovement))  == input$cmbHeroMeansOfMovement),
                        HeroSpeed  = which(i18n$t(names(.Movement))         == input$rdbHeroMovement),
                        EnemyRange = which(WeaponsRangeChoices              == input$rdbOpponentWeapon),
                        EnemySize  = which(i18n$t(names(.TargetSize))       == input$rdbOpponentSize),
                        EnemySpeed = which(i18n$t(names(.Movement))         == input$rdbOpponentMovement),
                        Evasive    = Evasive,
                        Visibility = which(i18n$t(names(.Visibility))       == input$cmbCombatEnvVision),
                        ElbowRoom  = Cramped,
                        Underwater = which(i18n$t(names(.UnderWater))       == input$cmbCombatEnvWater)
                    )
                    ESV <- ModifyCheck(WeaponSkills(), Environment) # Effective skill value
                } else {
                    ESV <- WeaponSkills()
                }
                
                return(ESV)
            })
            
            Modifiers <- reactive({
                return(EffectiveValues() - isolate(WeaponSkills()))
            })
            

            #' Handle dependencies between means of movement and levels of movement.
            observeEvent(input$cmbHeroMeansOfMovement, {
                #TODO: risky not to use codes/ids but the translated string
                Condition <- input$cmbHeroMeansOfMovement == i18n$t(names(.MeansOfMovement["OnFoot"])) 
                if (isTRUE(Condition))
                    Items <- .Movement
                else if (isFALSE(Condition))
                    Items <- .MountedMovement
                else
                    Items <- "N/A"
                updateSliderTextInput(session, "rdbHeroMovement", choices = i18n$t(names(Items)))
            })
            

            #'
            #' Display basic weapon characteristics: Name, Type, Reach/Range
            output$outWeaponDetails <- renderTable({
                if (isTruthy(WeaponName()))
                    Name <- WeaponName()
                else
                    Name = "/"
                
                if (isTRUE(WeaponType() == .WeaponType["Ranged"])) {
                    Type <- "Ranged Combat"
                    if (isTruthy(WeaponRange()))
                        Range <- paste(WeaponRange(), collapse = " / ")
                    else
                        Range = "/"
                }
                else {
                    Type <- "Close Combat"
                    if (isTruthy(WeaponRange()))
                        Range <- names(.CloseCombatRange[WeaponRange()])
                    else
                        Range = "/"
                }
                
                df <- data.frame(i18n$t(c("Weapon", "Mode", "Range")),
                                 c(Name, i18n$t(Type), i18n$t(Range)))
                df
            }, colnames = FALSE, spacing = "s", width = "100%", hline.after = NULL)

            
            #'
            #' Display the result after modifying the values
            output$outCombatModifiers <- renderTable({
                req(EffectiveValues(), WeaponSkills())

                df <- data.frame(F = i18n$t(c("AT", "PA", "DO")),
                                 FW = WeaponSkills(), 
                                 EFW = EffectiveValues())
                df
            }, spacing = "s", width = "100%")
            
            
            #' Show modal dialog on start up
            observeEvent(
                input$btnCombatMods, ignoreNULL = TRUE, showModal(ModalDlgFunction())
            )

            return(reactive(Modifiers()))
        } #server function
    )#moduleServer
}
