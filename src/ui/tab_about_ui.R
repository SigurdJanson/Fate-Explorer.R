# SETUP

fluidPage(
  fluidRow(
    column(12, h1("DSA5 Fate Explorer", align = "center"))
  ),
fluidRow(
  column(3, imageOutput("imgAboutShield")),
  column(6, i18n$t("The Fate Explorer helps you play the Dark Eye table top games. "), 
         i18n$t("Using you can roll your dice easier. "), 
         i18n$t("Consecutive checks are done automatically."), i18n$t("and checked"), 
         br(), br(), 
         i18n$t("The app is an ongoing project and I doubt it'll ever be finished. "),
         i18n$t("But it serves us well."),
         br(), br(),
         "Fate Explorer", i18n$t(" has been created by"), " Jan Seifert",
         hr(),
         h3(i18n$t("Honorable mentions")), br(), 
         i18n$t("This work would not have been possible without the support of"),
         "Sadik al'Dudei, Moran Lichtpfeil, Meschok, Sohn des Ssok, Ahgans Guelden (R.I.P),",
         "Alassar von Tundrien, Anjin Siebenstich, Sina ibn Rushd, Junis Djelef ibn Yakuban, Torfin Hardsgersen & Asleif Phileasson Foggwulf.",
         br(), br(),
         i18n$t("Furthermore"),
         a(i18n$t("the creator of the Optolith client"), href="https://github.com/elyukai"), 
         ", ", a(i18n$t("the creator of the Game Icons library"), href="https://github.com/seiyria/gameicons-font"), 
         i18n$t("including everyone"), a(i18n$t("who contributed to it"), href="https://game-icons.net/"),
         i18n$t("all the heroes from"), a("Wiki Aventurica", href="https://www.wiki-aventurica.de/"),
         " ", i18n$t("and"), " ",
         a(i18n$t("Ulysses publishers"), href="https://ulisses-spiele.de/"), "."
         ),
  column(3, imageOutput("imgAboutGods"))
),
fluidRow(
  column(12)
))


