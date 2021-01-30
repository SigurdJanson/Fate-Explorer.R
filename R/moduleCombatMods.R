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

            # CombatSkills <- 11:9L
            # 
            # observe({
            #     if (isTruthy(WeaponSkills())) {
            #         CombatSkills <- WeaponSkills()
            #     } else {
            #         CombatSkills <- rep(0L, 3L)
            #     }
            #     names(CombatSkills) <- names(.CombatAction)
            # })

            
            ModalDlgFunction <- function() {
                if (isTRUE(WeaponType != names(.WeaponType["Ranged"])))
                    WeaponsRangeChoices <- i18n$t(names(.CloseCombatRange))
                else
                    WeaponsRangeChoices <- i18n$t(names(.RangedCombatRange))
                
                modalDialog(
                    title = i18n$t("Combat Modifiers"),
                    
                    fluidRow(
                        column(4,
                               h4(i18n$t("Hero")),
                               textOutput(ns("txtHeroWeapon"), inline = FALSE),
                               textOutput(ns("txtHeroWeaponRanges"), inline = FALSE),
                               hr(),
                               selectInput(
                                   ns("cmbHeroMeansOfMovement"), i18n$t("Movement"), 
                                   i18n$t(names(.MeansOfMovement))),
                               sliderTextInput(
                                   ns("rdbHeroMovement"), label = NULL, 
                                   choices = "N/A", # reactive value
                                   grid = FALSE, force_edges = TRUE
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
                                   choices = i18n$t(names(.TargetSize))
                               ),
                               sliderTextInput(
                                   ns("rdbOpponentDistance"), label = i18n$t("Distance"),
                                   choices = i18n$t(names(.RangedCombatRange)), 
                                   grid = TRUE, force_edges = TRUE),
                               textOutput("txOpponentDONOTKNOWWHATTHISISYET", inline = FALSE),
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
            
            EffectiveValues <- reactive({
                if (isTruthy(input$cmbHeroMeansOfMovement) &&
                    isTruthy(input$rdbOpponentWeapon) &&
                    isTruthy(input$cmbCombatEnvVision)) {
                    
                    if (isTRUE(WeaponType != names(.WeaponType["Ranged"])))
                        WeaponsRangeChoices <- i18n$t(names(.CloseCombatRange))
                    else
                        WeaponsRangeChoices <- i18n$t(names(.RangedCombatRange))
                    
                    #TODO: risky not to use codes/ids but the translated string
                    Environment <- initCombatEnvironment(
                        Type  = ifelse(isTruthy(WeaponType),  WeaponType,  names(.WeaponType["Melee"])), 
                        Range = ifelse(isTruthy(WeaponRange), WeaponRange, names(.CloseCombatRange["Short"])), 
                        HeroMoves  = which(i18n$t(names(.MeansOfMovement))  == input$cmbHeroMeansOfMovement),
                        HeroSpeed  = which(i18n$t(names(.Movement))         == input$rdbHeroMovement),
                        EnemyRange = which(WeaponsRangeChoices              == input$rdbOpponentWeapon),
                        EnemySize  = which(i18n$t(names(.TargetSize))       == input$rdbOpponentSize),
                        EnemySpeed = which(i18n$t(names(.Movement))         == input$rdbOpponentMovement),
                        Evasive    = input$chbOpponentEvasive,
                        Visibility = which(i18n$t(names(.Visibility))       == input$cmbCombatEnvVision),
                        ElbowRoom  = input$cmbCombatEnvCramped,
                        Underwater = which(i18n$t(names(.UnderWater))       == input$cmbCombatEnvWater)
                    )
                    ESV <- ModifyCheck(WeaponSkills(), Environment) # Effective skill value
                } else {
                    ESV <- WeaponSkills()
                }
                
                return(ESV)
            })
            
            Modifiers <- reactive({
                #if (length(EffectiveValues() == length(WeaponSkills())) == length(.CombatAction))
                return(EffectiveValues() - isolate(WeaponSkills()))
            })
            

            #' Handle dependencies between means of movement and levels of movement.
            observeEvent(input$cmbHeroMeansOfMovement, {#TODO: change to reactiveEvent
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
            
            
            #' Display the name of the weapon if it has one
            output$txtHeroWeapon <- renderText({
                ifelse(isTruthy(WeaponName), WeaponName, "-----")
            })

            #' Display weapons range 
            #' (singe value for close combat and 3 values for ranged)
            output$txtHeroWeaponRanges <- renderText({
                req(WeaponRange)
                paste(WeaponRange, collapse = " / ")
            })

            
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
