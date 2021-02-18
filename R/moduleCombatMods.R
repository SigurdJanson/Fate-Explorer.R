require(shiny)
require(shinyWidgets)

source("./battleground.R")

#' Module UI definition of the Combat Modifiers Dialog
#' @param id Module identifier
#' @param i18n shiny.i18n Translator object
dlgCombatModsModuleUI <- function(id, i18n) {
    ns <- NS(id)
    actionButton(ns("btnCombatMods"), i18n$t("Combat"), icon = gicon("abacus"))
}



#' dlgCombatModsModuleServer
#' Modal module server for a dialog box that allows users to compute combat
#' modifiers by specifying environment and opponent.
#' @param id An ID string that identifies the module and, thus,  corresponds with
#' the ID used to call the module's UI function.
#' @param i18n a shiny.i18n Translator object
#' @param WeaponName Name of the current weapon (string).
#' @param WeaponType Weapon type (`.WeaponType`).
#' @param WeaponRange Range of weapon (either `.CloseCombatRange` or `.RangedCombatRange`).
#' @param WeaponSkills Base values of combat skills (`.CombatAction`)
#' @return A vector of the format `.CombatAction`. Each value has to be subtracted from
#' the base value to get the effective skill value.
dlgCombatModsModuleServer <- function(id, i18n, WeaponName, WeaponType, WeaponRange, WeaponSkills) {
    moduleServer(
        id,

        #' Shiny module server function
        function(input, output, session) {
            ns <- session$ns
            .CombatEnv <- NULL # Variable to hold the current combat environment


            #' @title ModalDlgFunction (Module "CombatMods")
            #' @description Opens the modal dialog
            #' @note The implementation heavily depends on naming conventions. The widget names that
            #' map to properties of the combat environment MUST end with the correct property names.
            ModalDlgFunction <- function() {

                Defaults <- CombatEnv()$GetCombatEnvironment(WeaponType())

                modalDialog(
                    title = i18n$t("Combat Modifiers"),

                    fluidRow(
                        column(4,
                               h4(i18n$t("Hero")),
                               tableOutput(ns("outWeaponDetails")),
                               hr(),
                               selectInput(
                                   ns("sel.Hero.MeansOfMovement"), i18n$t("Movement"),
                                   choices  = i18n$t(names(.MeansOfMovement)),
                                   selected = i18n$t(names(Defaults$Hero$MeansOfMovement))
                               ),
                               sliderTextInput(
                                   ns("sel.Hero.Movement"), label = NULL, grid = TRUE, force_edges = TRUE,
                                   choices = "-",
                                   selected = NULL
                               ),
                               wellPanel(
                                   h4(i18n$t("Values")),
                                   tableOutput(ns("outCombatModifiers"))
                               )
                        ),
                        column(4,
                               h4(i18n$t("Opponent")),
                               sliderTextInput(
                                   ns("sel.Opponent.CloseCombatRange"), label = i18n$t("Weapon Reach"),
                                   grid = TRUE, force_edges = TRUE,
                                   choices = i18n$t(names(.CloseCombatRange)),
                                   select  = i18n$t(names(Defaults$Opponent$CloseCombatRange))),
                               sliderTextInput(
                                   ns("sel.Opponent.Size"), label = i18n$t("Opponent Size"),
                                   grid = TRUE, force_edges = TRUE,
                                   choices  = i18n$t(names(.TargetSize)),
                                   selected = i18n$t(names(Defaults$Opponent$TargetSize))
                               ),
                               sliderTextInput(
                                   ns("sel.Opponent.TargetDistance"), label = i18n$t("Distance"),
                                   grid = TRUE, force_edges = TRUE,
                                   choices  = i18n$t(names(.TargetDistance)),
                                   selected = i18n$t(names(Defaults$Opponent$TargetDistance))),
                               sliderTextInput(
                                   ns("sel.Opponent.Movement"), label = i18n$t("Movement"),
                                   grid = TRUE, force_edges = TRUE,
                                   choices  = i18n$t(names(.Movement)),
                                   selected = i18n$t(names(Defaults$Opponent$Movement))),
                               checkboxGroupInput(ns("cb.Opponent.EvasiveMovement"), NULL, choices = i18n$t("Evasive Maneuvers"))
                        ),
                        column(4,
                               h4(i18n$t("Environment")),
                               selectInput(
                                   ns("sel.Environment.Visibility"), i18n$t("Visibility"),
                                   choices  = i18n$t(names(.Visibility)),
                                   selected = i18n$t(names(Defaults$Environment$Visibility))
                               ),
                               checkboxGroupInput(ns("cb.Environment.CrampedSpace"), NULL, choices = i18n$t("Cramped")),
                               selectInput(
                                   ns("sel.Environment.UnderWater"), i18n$t("Water Depth"),
                                   choices  = i18n$t(names(.UnderWater)),
                                   selected = i18n$t(names(Defaults$Environment$UnderWater))
                              ),
                              # Space
                              hr(),
                              actionBttn(ns("btnResetValue"), i18n$t("btnReset"),
                                         style = "bordered", color = "primary", size = "sm", block = TRUE)
                        )
                    ),
                    footer = modalButton(i18n$t("btnClose")), fade = TRUE, size = "l",
                    easyClose = TRUE
                )
            } # UI


            #' HandleDisabled
            #' Enable / Disable the UI controls depending on weapon type
            HandleDisabled <- function() {
                if (isTRUE(.WeaponType[WeaponType()] == .WeaponType["Ranged"])) {
                    ToDisable <- c("sel.Opponent.CloseCombatRange", "cb.Environment.CrampedSpace")
                } else {
                    ToDisable <- c("sel.Hero.MeansOfMovement", "sel.Hero.Movement",
                                   "sel.Opponent.TargetDistance", "sel.Opponent.Movement",
                                   "cb.Opponent.EvasiveMovement")
                }
                ToEnable <- names(input)
                ToEnable <- ToEnable[!(names(input) %in% ToDisable)] # enable everything except `ToDisable`
                for (i in ToEnable) shinyjs::enable(i)
                for (i in ToDisable) shinyjs::disable(i)

                return(invisible(NULL))
            }

            #' UpdateHeroMovement
            #' Handles the dependency between the two settings "hero: means of movement" (on foot or riding)
            #' and "movement" speed.
            UpdateHeroMovement <- function() {
                #TODO: risky not to use codes/ids but the translated strings
                Condition <- input[["sel.Hero.MeansOfMovement"]] == i18n$t(names(.MeansOfMovement["OnFoot"]))

                if (isTRUE(Condition))
                    Items <- .Movement
                else if (isFALSE(Condition))
                    Items <- .MountedMovement
                else
                    Items <- "N/A"
                updateSliderTextInput(session, "sel.Hero.Movement", choices = i18n$t(names(Items)))
            }


            #' UpdateAll
            #' Populate all widgets in the modal dialog with the values from the combat environment.
            UpdateAll <- function() {
                Widgets <- names(input)
                Parts   <- strsplit(Widgets, ".", fixed = TRUE)
                for (w in 1:length(Widgets)) {
                    Len <- length(Parts[[w]])
                    if (!startsWith(Widgets[w], "btn")) { # exclude buttons
                      # the new value
                      if (Parts[[w]][1] == "sel")
                          NewValue <- CombatEnv()$getValue(Parts[[w]][Len-1], Parts[[w]][Len])
                      else if (Parts[[w]][1] == "cb")
                          NewValue <- as.logical(CombatEnv()$getValue(Parts[[w]][Len-1], Parts[[w]][Len])-1)
                      # Send message to widget for value update
                      # Use names "value" and "selected" because Shiny and ShinyWidgets do not use a consistent naming patterns
                      message <- list(value = i18n$t(names(NewValue)), selected = i18n$t(names(NewValue)))
                      session$sendInputMessage(Widgets[w], message)
                    }
                }
            }


            #' Reactive: CombatEnv
            #' Returns the current combat environment or creates a new one if it has not
            #' been initialised, yet.
            CombatEnv <- reactive({
                if (!isTruthy(.CombatEnv)) {
                    if (isTruthy(WeaponType()))
                        .CombatEnv <<- CombatEnvironment$new(WeaponType())
                    else
                        .CombatEnv <<- CombatEnvironment$new(.WeaponType["Melee"])
                } else {
                    .CombatEnv$WeaponType <- WeaponType()
                }
                if (isTruthy(WeaponRange()))
                    .CombatEnv$CombatRange <- WeaponRange()

                return(.CombatEnv)
            })


            #' Reactive: EffectiveValues
            #' Returns the effective skill values depending on the given combat environment.
            EffectiveValues <- reactive({
                # Check if all widgets in this module are valid (except check boxes)
                # Also exclude the button that is used to call the modal dialog
                WidgValid <- vapply(names(input),
                                    function(x) { if (startsWith(x, "sel")) isTruthy(input[[x]]) else TRUE },
                                    logical(1))
                WidgValid <- length(WidgValid) > 1 & allTruthy(WidgValid, WeaponType(), CombatEnv())

                if (WidgValid) {
                    # Convert to numeric index to enum
                    Evasive <- .EvasiveMovement[as.integer(isTruthy(input[["cb.Opponent.EvasiveMovement"]])) + 1]
                    Cramped <- .CrampedSpace[as.integer(isTruthy(input[["cb.Environment.CrampedSpace"]])) + 1]

                    # Which options are valid? Close or ranged combat?
                    if (isTRUE(WeaponType() == names(.WeaponType["Ranged"]))) {
                        if (input[["sel.Hero.MeansOfMovement"]] == i18n$t(names(.MeansOfMovement["OnFoot"])))
                            Movement <- .Movement
                        else
                            Movement <- .MountedMovement
                    } else {
                        Movement <- .Movement
                    }

                    #TODO: risky not to use codes/ids but the translated string
                    CombatEnv()$initCombatEnvironment(
                        weaponType = .WeaponType[ifelse(isTruthy(WeaponType()),  WeaponType(),  .WeaponType["Melee"])],
                        closeRange = ifelse(isTruthy(WeaponRange()), WeaponRange(), names(.CloseCombatRange["Short"])),
                        HeroMoves  = which(i18n$t(names(.MeansOfMovement))  == input[["sel.Hero.MeansOfMovement"]]),
                        HeroSpeed  = which(i18n$t(names(Movement))          == input[["sel.Hero.Movement"]]),
                        EnemyRange = which(i18n$t(names(.CloseCombatRange)) == input[["sel.Opponent.CloseCombatRange"]]),
                        EnemySize  = which(i18n$t(names(.TargetSize))       == input[["sel.Opponent.Size"]]),
                        EnemyDistance = which(i18n$t(names(.TargetDistance))== input[["sel.Opponent.TargetDistance"]]),
                        EnemySpeed = which(i18n$t(names(.Movement))         == input[["sel.Opponent.Movement"]]),
                        Evasive    = Evasive,
                        Visibility = which(i18n$t(names(.Visibility))       == input[["sel.Environment.Visibility"]]),
                        crampedSpace = Cramped,
                        Underwater = .UnderWater[which(i18n$t(names(.UnderWater)) == input[["sel.Environment.UnderWater"]])]
                    )
                    Environment <- CombatEnv()$GetCombatEnvironment()
                    ESV <- ModifyCheck(WeaponSkills(), Environment) # Effective skill value

                } else { # if no Combat Environment can be constructed
                    ESV <- WeaponSkills()
                }

                return(ESV)
            })


            #' Reactive: modifiers
            #' Gets the modifier as difference between actual and effective skill values
            Modifiers <- reactive({
                if(isTruthy(EffectiveValues()) && isTruthy(WeaponSkills()))
                    return(EffectiveValues() - isolate(WeaponSkills()))
                else
                    return(NULL)
            })


            #' Observer
            #' Handle dependencies between means of movement and levels of movement.
            observeEvent(input[["sel.Hero.MeansOfMovement"]], {
                UpdateHeroMovement()
            })


            #' Observer
            #' Reset the environment on user's request
            observeEvent(input$btnResetValue, {

                if (!isTruthy(.CombatEnv)) {
                    if (isTruthy(WeaponType()))
                        .CombatEnv <<- CombatEnvironment$new(WeaponType())
                    else
                        .CombatEnv <<- CombatEnvironment$new(.WeaponType["Melee"])
                } else {
                    if (isTruthy(WeaponRange())) .CombatEnv$CombatRange <- WeaponRange()
                    .CombatEnv$resetToDefault( WeaponType() )
                }

                UpdateAll()
            }, ignoreNULL = TRUE)


            #' Output
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


            #' Output
            #' Display the result after modifying the values
            output$outCombatModifiers <- renderTable({
                req(EffectiveValues(), WeaponSkills())

                df <- data.frame(F = i18n$t(c("AT", "PA", "DO")),
                                 FW = WeaponSkills(),
                                 EFW = EffectiveValues())
                df
            }, spacing = "s", width = "100%")



            #' Observer
            #' Show modal dialog on start up
            observeEvent(
                input$btnCombatMods, ignoreNULL = TRUE,
                {
                    showModal(ModalDlgFunction())
                    HandleDisabled()
                    UpdateHeroMovement()
                }
            )

            return(reactive(Modifiers()))
        } #server function
    )#moduleServer
}
