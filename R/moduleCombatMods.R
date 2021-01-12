library(shiny)
library(shinyWidgets)

# Modal module UI
dlgCombatModsModuleUI <- function(id) {
    ns <- NS(id)
    actionButton(ns("btnCombatMods"), "Combat Modifiers")
}



# Modal module server
dlgCombatModsModuleServer <- function(id, i18n) {
    moduleServer(
        id,

        function(input, output, session) {
            ns <- session$ns
            
            BattleGround <- reactive({
                
            })
            
            ModalDlgFunction <- function() {
                HeroWeapons  <- LETTERS[1:5]
                WeaponsRange <- i18n$t(names(.CloseCombatRange))
                HeroMovement <- i18n$t(names(ifelse(TRUE, .Movement, .MountedMovement)))
                
                modalDialog(
                    title = i18n$t("Combat Modifiers"),
                    
                    fluidRow(
                        column(4,
                               h4(i18n$t("Hero")),
                               selectInput(ns("cmbHeroWeapon"), "Selected Weapon", HeroWeapons),
                               textOutput(ns("txtHeroWeaponRanges"), inline = FALSE),
                               hr(),
                               selectInput(
                                   ns("cmbHeroMeansOfMovement"), "Movement", 
                                   i18n$t(names(.MeansOfMovement))),
                               sliderTextInput(
                                   ns("rdbHeroMovement"), label = NULL, 
                                   choices = c("Stationary", "Slow", "Fast"), grid = FALSE, force_edges = TRUE
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
                               checkboxGroupInput("chbOpponentEvasive", NULL, choices = "Evasive Maneuvers")
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
                    footer = modalButton(i18n$t("Close")), fade = TRUE, size = "l"
                )
            }
            
            # Show modal dialog on start up
            observeEvent(
                input$btnCombatMods, ignoreNULL = TRUE, showModal(ModalDlgFunction())
            )
            
            output$txtHeroWeaponRanges <- renderText({
                paste(input$cmbHeroWeapon, "is selected")
            })
            
            output$outCombatModifiers <- renderTable({
                req(input$cmbHeroWeapon)
                df <- data.frame(F = c("AT", "PA", "DO"), FW = 1:3, EFW = 3:1)
                df
            }, spacing = "s", width = "100%")
            
            return(BattleGround)
        }
    )#moduleServer
}


