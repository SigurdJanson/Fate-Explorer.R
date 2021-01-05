# SETUP

fluidPage(
  fluidRow(
    column(12, h1(img(src="FateExplorer_Blood_Logo.svg", alt="Logo", width="45%"), align = "center"))
  ),
fluidRow(
  column(3, imageOutput("imgAboutMagic"), imageOutput("imgAboutSkeletons")),
  column(6, i18n$t("The Fate Explorer helps you play the Dark Eye table top games. "), 
         i18n$t("Using you can roll your dice easier. "), 
         i18n$t("Consecutive checks are done automatically"), i18n$t("and checked."), 
         br(), br(), 
         i18n$t("The app is an ongoing project and I doubt it'll ever be finished. "),
         i18n$t("But it serves us well."),
         br(), br(),
         "Fate Explorer", i18n$t(" has been created by"), " Jan Seifert.",
         
         i18n$t("This is release"), " 2 ", i18n$t("code named"), " Aves's Boot Heel.",
         
         hr(),
         h3(i18n$t("Honorable mentions")), br(), 
         i18n$t("This work would not have been possible without the support of"),
         "Sadik al'Dudei, Moran Lichtpfeil, Meschok, Sohn des Ssok, Ahgans Guelden (R.I.P),",
         "Alassar von Tundrien, Anjin Siebenstich, Sina ibn Rushd, Junis Djelef ibn Yakuban, Torfin Hardsgersen & Asleif Phileasson Foggwulf.",
         br(), br(),
         i18n$t("Furthermore"),
         a(i18n$t("the creator of the Optolith"), href="https://github.com/elyukai", .noWS = "after"), 
         ", ", 
         a(i18n$t("the creator of the"), "Game Icons", i18n$t("library"), href="https://github.com/seiyria/gameicons-font", .noWS = "after"),
         i18n$t("including everyone"), 
         a(i18n$t("who contributed to it"), href="https://game-icons.net/", .noWS = "after"),
         ", ", 
         a(paste(i18n$t("the creator of the"), i18n$t("illustrations on this page")), href="https://pixabay.com/users/parker_west-7094318/", .noWS = "after"), 
         ", ", 
         i18n$t("all the heroes from"), 
         a("Wiki Aventurica", href="https://www.wiki-aventurica.de/", .noWS = "after"),
         " ", i18n$t("and"), " ",
         a(i18n$t("Ulisses publishers"), href="https://ulisses-spiele.de/", .noWS = "after"), "."
         ),
  column(3, imageOutput("imgAboutBat"), imageOutput("imgAboutBarbarian"))
),
fluidRow(
  column(12)
))


