library(shiny)
library(shinyWidgets)

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
dlgCombatModsModuleServer <- function(id, i18n, Weapon) {
    moduleServer(
        id,

        function(input, output, session) { # Shiny module server function
            ns <- session$ns

            browser()
            if(isTruthy(Weapon)) {
                if (isTruthy(Weapon$Skills)) {
                    CombatSkills <- unlist(Weapon$Skills)
                } else {
                    CombatSkills <- rep(0, 3)
                }
                names(CombatSkills) <- c("AT", "PA", "DO")
            }
            
            # TODO: get this from caller - return it to caller
            #-print(input$rdbOpponentSize)
            
            # CombatEnv <- list(
            #         Hero = list(
            #             Weapon = list(
            #                 WeaponType = Weapon$Type,
            #                 RangedCombatRange = sample(.RangedCombatRange, 1),
            #                 MeansOfMovement   = sample(.MeansOfMovement, 1),
            #                 Movement          = sample(.Movement, 1)
            #             )
            #         ),
            #         Opponent = list(
            #             TargetSize        = input$rdbOpponentSize,
            #             RangedCombatRange = sample(.RangedCombatRange, 1),
            #             Movement          = sample(.Movement, 1),
            #             EvasiveMovement   = sample(.EvasiveMovement, 1),
            #             TargetDistance    = sample(.TargetDistance, 1)
            #         ),
            #         Environment = list(
            #             Visibility   = .Visibility["Impaired"], 
            #             CrampedSpace = .CrampedSpace["Cramped"],
            #             UnderWater   = .UnderWater["Dry"]
            #         )
            #     )
            
            ModalDlgFunction <- function() {
                if (isTruthy(Weapon) && isTRUE(Weapon$Type != names(.WeaponType["Ranged"])))
                    WeaponsRange <- i18n$t(names(.CloseCombatRange))
                else
                    WeaponsRange <- i18n$t(names(.RangedCombatRange))
                
                modalDialog(
                    title = i18n$t("Combat Modifiers"),
                    
                    fluidRow(
                        column(4,
                               h4(i18n$t("Hero")),
                               #selectInput(ns("cmbHeroWeapon"), i18n$t("Selected Weapon"), HeroWeapons),
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
                                   choices = WeaponsRange, grid = TRUE, force_edges = TRUE),
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
            
            Modified <- reactive({
                browser()
                Environment <- list(
                    Hero = list(WeaponType = Weapon$Type, 
                                CloseCombatRange  = Weapon$Range, 
                                RangedCombatRange = Weapon$Range,
                                MeansOfMovement = input$cmbHeroMeansOfMovement,
                                Movement = input$rdbHeroMovement
                    ),
                    Opponent = list(TargetSize = input$rdbOpponentSize,
                                    CloseCombatRange  = input$rdbOpponentWeapon, 
                                    RangedCombatRange = input$rdbOpponentWeapon,
                                    Movement = input$rdbOpponentMovement,
                                    EvasiveMovement = input$chbOpponentEvasive, # ??
                                    TargetDistance  = input$rdbOpponentDistance),
                    Environment = list(Visibility   = input$cmbCombatEnvVision, 
                                       CrampedSpace = input$cmbCombatEnvCramped,
                                       UnderWater   = input$cmbCombatEnvWater)
                )
                Mods <- ModifyCheck(CombatSkills, Environment)
                return(Mods)
            })
            

            #' Handle dependencies between means of movement and levels of movement.
            observe({
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
                ifelse(isTruthy(Weapon) & isTruthy(Weapon$Name), Weapon$Name, "-----")
            })

            #' Display weapons range 
            #' (singe value for close combat and 3 values for ranged)
            output$txtHeroWeaponRanges <- renderText({
                req(Weapon$Range)
                paste(Weapon$Range, collapse = " / ")
            })

            
            output$outCombatModifiers <- renderTable({
                #req(input$cmbHeroWeapon, Modified())
                df <- data.frame(F = i18n$t(c("AT", "PA", "DO")), 
                                 FW = CombatSkills, 
                                 EFW = Modified())
                df
            }, spacing = "s", width = "100%")
            
            
            #' Show modal dialog on start up
            observeEvent(
                input$btnCombatMods, ignoreNULL = TRUE, showModal(ModalDlgFunction())
            )

            return(Modified)
        } #server function
    )#moduleServer
}


