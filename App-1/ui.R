## UI.R ##
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(DT)
library(scales)

rep <- "C:/Users/gottavianoni/Desktop/GESTAE/App-1/temp/"
#rep <- "C:/Users/Sapie/Nextcloud Sylvie/SAPIE (2)/COOPERATIVE D'ACTIVITES/BASE DE DONNEES COOPERATEURS/"

notification <- "8 Nouveaux Avis a Traiter"
df2 <- read.csv2(paste0(rep,"/bd_cae.csv"),colClasses = "character")
date_nais <- paste0(substr(df2$NAIS,7,10),"/",substr(df2$NAIS,4,5),"/",substr(df2$NAIS,1,2))
date_nais <- ifelse(date_nais == "//","",date_nais)
df2$AGE <- floor(difftime(Sys.Date(),as.Date(date_nais),units = "weeks") / 52.25)
df2$AGE <- ifelse(substr(df2$AGE,1,1) == "-",NA,df2$AGE)
df2$AGE <- as.character(cut(df2$AGE,breaks = c(0,25,30,35,40,45,50,150), c("Moins de 25 ans","Entre 25 et 30 ans","Entre 30 et 35 ans","Entre 35 et 40 ans",
                                                                               "Entre 40 et 45 ans","Entre 45 et 50 ans","Plus de 50 ans")))

write.csv2(df2,paste0(rep,"bd_cae.csv"),row.names = F)
write.csv2(df2,paste0(rep,"Archive/bd_cae_",gsub(" ","_",substr(Sys.time(),1,13)),".csv"),row.names = F)

ville_coop <- df2$VILLE
nom_coop <- paste(df2$PRENOM,df2$NOM)

dashboardPage(skin = "blue",title = "GESTAE | SAPIE" ,
              dashboardHeader(title = p(strong("GESTAE | SAPIE")),
                              dropdownMenu(type = "notifications",badgeStatus = "warning",
                                           notificationItem(
                                             text = notification,
                                             icon("cogs")
                                           ))
              ),
              dashboardSidebar(
                sidebarUserPanel(name = "Bienvenue", subtitle = "Guillaume", image = "enedis.png"),
                sidebarMenu(
                  menuItem("BD CAE",tabName = "BD_CAE", icon = icon("database"),
                           menuSubItem("Saisie",tabName = "BD_SAIS",icon = icon("user-plus")),
                           menuSubItem("Modification",tabName = "BD_MOD",icon = icon("keyboard-o")),
                           menuSubItem("Rapport",tabName = "BD_REP",icon = icon("line-chart"))),
                         
                  menuItem("SUIVI E/S",tab_name = "SUIV_ES",icon = icon("random"),
                           menuSubItem("Rapport des Dates",tabName = "SUIV_DELTA",icon = icon("exclamation"))
                          # menuSubItem("Consultation",tabName = "SUIV_CONS",icon = icon("paper-plane"))
                           
                           )
                
                )),
              dashboardBody(
                tabItems(
                  tabItem(tabName = "BD_SAIS",
                          h1("Saisie d'un profil Cooperateur"),
                          h2(),
                          fluidRow(box(title =  "Etat Civil",status = "success",solidHeader =F,width = 12,collapsible = F,collapsed = F,
                                       fluidRow(column(2,selectInput("SAIS_CIV", "Civilite",choices = c("M","Mme"),multiple = F)),
                                                column(2,textInput("SAIS_NOM", "Nom")),
                                                column(2,textInput("SAIS_PRENOM", "Prenom")),
                                                column(2,textInput("SAIS_NOMJF", "Nom de J Fille")),
                                                column(2,selectInput("SAIS_SEXE", "Sexe",choices = c("H","F"),multiple = F)),
                                                column(2,selectInput("SAIS_PARET", "1 des Parents Etrangers",choices = c("Oui","Non","Ne souhaite pas repondre"),multiple = F))
                                                ),
                                       fluidRow(column(2,textInput("SAIS_SECU", "N Securite Sociale")),
                                                column(2,textInput("SAIS_CAF", "N CAF")),
                                                column(2,selectInput("SAIS_SIT", "Situation Familiale",choices = c("Celibataire","Veuf","Marie","Pacse","Vie maritale","Separe","Divorce"))),
                                                column(2,textInput("SAIS_NBP", "Nombre de Personnes a Charge")),
                                                column(2,selectInput("SAIS_FMONOP", "Famille monop avec enfants",choices = c("Oui","Non"),multiple = F)),
                                                column(2,selectInput("SAIS_MENEMP", "Menage avec Emploi et Enfants",choices = c("Oui sans enfant","Non","Oui, avec enfant"),multiple = F))
                                                ),
                                      fluidRow(column(2,dateInput("SAIS_NAIS", "Date de Naissance",format = "dd-mm-yyyy")),
                                            column(2,textInput("SAIS_CPN", "Code Postal ville de Naissance")),
                                            column(2,textInput("SAIS_NAT", "Nationnalite")),
                                            column(2,selectInput("SAIS_AGE", "Age",choices = c("Moins de 25 ans","Entre 25 et 30 ans","Entre 30 et 35 ans","Entre 35 et 40 ans",
                                                                                               "Entre 40 et 45 ans","Entre 45 et 50 ans","Plus de 50 ans"),multiple = F))
                                   ))),
                          fluidRow(box(title =  "Coordonnees",status = "success",solidHeader =F,width = 12,collapsible = F,collapsed = F,
                                       fluidRow(column(2,textInput("SAIS_NADR", "N Adresse")),
                                                column(2,textInput("SAIS_VOIE", "Nom de Voie")),
                                                column(2,textInput("SAIS_COMP", "Complement")),
                                                column(2,textInput("SAIS_CPA", "Code Postal")),
                                                column(2,textInput("SAIS_VILLE", "Ville")),
                                                column(2,selectInput("SAIS_QUART", "Quartier PV",choices = c("Oui","Non"),multiple = F))
                                       ),
                                       fluidRow(column(2,textInput("SAIS_TEL1", "Telephone 1")),
                                                column(2,textInput("SAIS_TEL2", "Telephone 2")),
                                                column(2,textInput("SAIS_MAILPE", "Mail Perso")),
                                                column(2,textInput("SAIS_MAILPR", "Mail Pro")),
                                                column(2,selectInput("SAIS_SDF", "Sans Domicile fixe",choices = c("Oui","Non"),multiple = F)),
                                                column(2,selectInput("SAIS_EXC", "En cours d'exclusion",choices = c("Oui","Non"),multiple = F))
                                       ))),
                          fluidRow(box(title =  "Situation",status = "success",solidHeader =F,width = 12,collapsible = F,collapsed = F,
                                       fluidRow(column(2,textInput("SAIS_DIP", "Diplomes")),
                                                column(2,selectInput("SAIS_QUAL", "Qualification",choices = c("Primaire","Brevet College","BAC","CAP","BEP","BTS","BAC+2","BAC+3","BAC+4","BAC+5","Doctorat"),multiple = F)),
                                                column(2,selectInput("SAIS_SITAC", "Situation Actuelle",choices = c("Salarie","Independant","Conge parental temps complet","DE non indemnise","DE indemnise","RSA","ASS","AAH"),multiple = F)),
                                                column(2,selectInput("SAIS_SALAR", "Situation Salarie",choices = c("Emploi durable","Emploi temporaire","Emploi aide","Independant"))),
                                                column(2,selectInput("SAIS_HANDI", "Reconnaissance travailleur handi",choices = c("Oui","Non"),multiple = F)),
                                                column(2,selectInput("SAIS_RECHEMP", "Recherche active emploi",choices = c("Non","Moins de 6 mois","Entre 6 et moins de 12 mois","Entre 12 et moins de 24 mois","24 mois et plus"),multiple = F))
                                       ),
                                       fluidRow(column(2,selectInput("SAIS_RECHEMP2", "Recherche active emploi",choices = c("Oui","Non"))))
                             )),
                          fluidRow(box(title =  "Secteur d'Activite",status = "success",solidHeader =F,width = 12,collapsible = F,collapsed = F,
                                       fluidRow(column(4,selectInput("SAIS_ORIEN", "Orientation",choices = c("Bouche a oreille et reseau sapie","Site web, news, presse","Consulaires","Autres structures creations","Pole Emploi","Service insertion","Autres structures (OF,.)"))),
                                                column(4,textInput("SAIS_ACTIV", "Activite")),
                                                column(4,selectInput("SAIS_SECTACT", "Secteur d'Activite",choices = c("Animations","Art et artisanat","Bien etre et soin","Communication et metiers de l'ecrit","Conseils","Developpement informatique","Developpement local","Environnement et paysages","Formation","Ingenierie","Solution web"),multiple = F))
                                                )
                          )),
                          fluidRow(box(title =  "Infos CAE",status = "success",solidHeader =F,width = 12,collapsible = F,collapsed = F,
                                       fluidRow(column(2,dateInput("SAIS_DATENT", "Date entree",format = "dd-mm-yyyy")),
                                                column(2,selectInput("SAIS_CONT", "Type de Contrat",choices = c("CAPE","CESA"),multiple = F)),
                                                column(2,dateInput("SAIS_DATER1", "Date Renouvellement CAPE 1",format = "dd-mm-yyyy")),
                                                column(2,dateInput("SAIS_DATER2", "Date Renouvellement CAPE 2",format = "dd-mm-yyyy")),
                                                column(2,dateInput("SAIS_DATEFIN", "Date Fin de Contrat",format = "dd-mm-yyyy")),
                                                column(2,selectInput("SAIS_MOTIF", "Motif de fin",choices = c("Abandon","Emploi","Formation","Creation entreprise","Maladie", "Maternite","Demenagement","Autre"),multiple = F))),
                                       fluidRow(column(2,selectInput("SAIS_SITSOR", "Situation a la sortie",choices=c("Oui","Non"))),
                                                column(2,selectInput("SAIS_SORSAL", "Situation sortie si salarie",choices = c("emploi durable","emploi temporaire","emploi aide","independant"),multiple = F)),
                                                column(2,selectInput("SAIS_SORNSAL",  "Situation sortie si non salarie",choices = c("recherche d'emploi","formation qualifiante","formation non qualifiante","apprentissage ou contrat pro","en stage","autre"))),
                                                column(2,textInput("SAIS_NEWADR", "N Adresse si demenagement")))
                          )),
                          
                          fluidRow(box(status = "success",solidHeader =F,width=12,collapsible = F,collapsed = F,
                                       fluidRow(column(2,offset = 5,actionButton("SAIS_VALIDATE", "VALIDEZ LES INFORMATIONS")))))
                  ),
                          tabItem(tabName = "BD_MOD",
                                  h1("Modification d'un profil Cooperateur"),
                                  fluidRow(box(title =  "Selection du cooperateur a Modifier",status = "success",solidHeader =F,width = 12,collapsible = F,collapsed = F,
                                               fluidRow(column(3,selectInput("MOD_PRENOM_NOM",label = "Selectionner le membre de la CAE",choices = nom_coop)))
                                               )),
                                  h2(),
                                  fluidRow(box(title =  "Etat Civil",status = "success",solidHeader =F,width = 12,collapsible = T,collapsed = T,
                                               fluidRow(column(2,selectInput("MOD_CIV", "Civilite",choices = c("M","Mme"),multiple = F)),
                                                        column(2,textInput("MOD_NOM", "Nom")),
                                                        column(2,textInput("MOD_PRENOM", "Prenom")),
                                                        column(2,textInput("MOD_NOMJF", "Nom de J Fille")),
                                                        column(2,selectInput("MOD_SEXE", "Sexe",choices = c("H","F"),multiple = F)),
                                                        column(2,selectInput("MOD_PARET", "1 des Parents Etrangers",choices = c("Oui","Non","Ne souhaite pas repondre"),multiple = F))
                                               ),
                                               fluidRow(column(2,textInput("MOD_SECU", "N Securite Sociale")),
                                                        column(2,textInput("MOD_CAF", "N CAF")),
                                                        column(2,selectInput("MOD_SIT", "Situation Familiale",choices = c("Celibataire","Veuf","Marie","Pacse","Vie maritale","Separe","Divorce"))),
                                                        column(2,textInput("MOD_NBP", "Nombre de Personnes a Charge")),
                                                        column(2,selectInput("MOD_FMONOP", "Famille monop avec enfants",choices = c("Oui","Non"),multiple = F)),
                                                        column(2,selectInput("MOD_MENEMP", "Menage avec Emploi et Enfants",choices = c("Oui sans enfant","Non","Oui, avec enfant"),multiple = F))
                                               ),
                                               fluidRow(column(2,dateInput("MOD_NAIS", "Date de Naissance",format = "dd-mm-yyyy")),
                                                        column(2,textInput("MOD_CPN", "Code Postal ville de Naissance"),format = "dd-mm-yyyy"),
                                                        column(2,textInput("MOD_NAT", "Nationnalite")),
                                                        column(2,selectInput("MOD_AGE", "Age",choices = c("Moins de 25 ans","Entre 25 et 30 ans","Entre 30 et 35 ans","Entre 35 et 40 ans",
                                                                                                           "Entre 40 et 45 ans","Entre 45 et 50 ans","Plus de 50 ans"),multiple = F))
                                               ))),
                                  fluidRow(box(title =  "Coordonnees",status = "success",solidHeader =F,width = 12,collapsible = T,collapsed = T,
                                               fluidRow(column(2,textInput("MOD_NADR", "N Adresse")),
                                                        column(2,textInput("MOD_VOIE", "Nom de Voie")),
                                                        column(2,textInput("MOD_COMP", "Complement")),
                                                        column(2,textInput("MOD_CPA", "Code Postal")),
                                                        column(2,textInput("MOD_VILLE", "Ville")),
                                                        column(2,selectInput("MOD_QUART", "Quartier PV",choices = c("Oui","Non"),multiple = F))
                                               ),
                                               fluidRow(column(2,textInput("MOD_TEL1", "Telephone 1")),
                                                        column(2,textInput("MOD_TEL2", "Telephone 2")),
                                                        column(2,textInput("MOD_MAILPE", "Mail Perso")),
                                                        column(2,textInput("MOD_MAILPR", "Mail Pro")),
                                                        column(2,selectInput("MOD_SDF", "Sans Domicile fixe",choices = c("Oui","Non"),multiple = F)),
                                                        column(2,selectInput("MOD_EXC", "En cours d'exclusion",choices = c("Oui","Non"),multiple = F))
                                               ))),
                                  fluidRow(box(title =  "Situation",status = "success",solidHeader =F,width = 12,collapsible = T,collapsed = T,
                                           fluidRow(column(2,textInput("MOD_DIP", "Diplomes")),
                                                    column(2,selectInput("MOD_QUAL", "Qualification",choices = c("Primaire","Brevet College","BAC","CAP","BEP","BTS","BAC+2","BAC+3","BAC+4","BAC+5","Doctorat"),multiple = F)),
                                                    column(2,selectInput("MOD_SITAC", "Situation Actuelle",choices = c("Salarie","Independant","Conge parental temps complet","DE non indemnise","DE indemnise","RSA","ASS","AAH"),multiple = F)),
                                                    column(2,selectInput("MOD_SALAR", "Situation Salarie",choices = c("Emploi durable","Emploi temporaire","Emploi aide","Independant"))),
                                                    column(2,selectInput("MOD_HANDI", "Reconnaissance travailleur handi",choices = c("Oui","Non"),multiple = F)),
                                                    column(2,selectInput("MOD_RECHEMP", "Recherche active emploi",choices = c("Non","Moins de 6 mois","Entre 6 et moins de 12 mois","Entre 12 et moins de 24 mois","24 mois et plus"),multiple = F))
                                           ),
                                           fluidRow(column(2,selectInput("MOD_RECHEMP2", "Recherche active emploi",choices = c("Oui","Non"))))
                                  )),
                                  fluidRow(box(title =  "Secteur d'Activite",status = "success",solidHeader =F,width = 12,collapsible = T,collapsed = T,
                                               fluidRow(column(4,selectInput("MOD_ORIEN", "Orientation SAPIE",choices = c("Bouche a oreille et reseau sapie","Site web, news, presse","Consulaires","Autres structures creations","Pole Emploi","Service insertion","Autres structures (OF,.)"))),
                                                        column(4,textInput("MOD_ACTIV", "Activite")),
                                                        column(4,selectInput("MOD_SECTACT", "Secteur Activite",choices = c("Animations","Art et artisanat","Bien etre et soin","Communication et metiers de l'ecrit","Conseils","Developpement informatique","Developpement local","Environnement et paysages","Formation","Ingenierie","Solution web"),multiple = F))
                                               )
                                  )),
                                  fluidRow(box(title =  "Infos CAE",status = "success",solidHeader =F,width = 12,collapsible = T,collapsed = T,
                                               fluidRow(column(2,dateInput("MOD_DATENT", "Date entree",format = "dd-mm-yyyy")),
                                                        column(2,selectInput("MOD_CONT", "Type de Contrat",choices = c("CAPE","CESA"),multiple = F)),
                                                        column(2,dateInput("MOD_DATER1", "Date Renouvellement CAPE 1",format = "dd-mm-yyyy")),
                                                        column(2,dateInput("MOD_DATER2", "Date Renouvellement CAPE 2",format = "dd-mm-yyyy")),
                                                        column(2,dateInput("MOD_DATEFIN", "Date Fin de Contrat",format = "dd-mm-yyyy")),
                                                        column(2,selectInput("MOD_MOTIF", "Motif de fin",choices = c("Abandon","Emploi","Formation","Creation entreprise","Maladie", "Maternite","Demenagement","Autre"),multiple = F))),
                                               fluidRow(column(2,selectInput("MOD_SITSOR", "Situation a la sortie",choices=c("Oui","Non"))),
                                                        column(2,selectInput("MOD_SORSAL", "Situation sortie si salarie",choices = c("emploi durable","emploi temporaire","emploi aide","independant"),multiple = F)),
                                                        column(2,selectInput("MOD_SORNSAL",  "Situation sortie si non salarie",choices = c("recherche d'emploi","formation qualifiante","formation non qualifiante","apprentissage ou contrat pro","en stage","autre"))),
                                                        column(2,textInput("MOD_NEWADR", "N Adresse si demenagement")))
                                               
                                  )),
                                  fluidRow(box(status = "success",solidHeader =F,width=12,collapsible = F,collapsed = F,
                                               fluidRow(column(2,offset = 3,actionButton("MOD_VALIDATE", "VALIDEZ LES MODIFICATIONS")),
                                                        column(2,offset = 2,actionButton("MOD_DELETE", "SUPPRIMER LE PROFIL")))))
                                  )
                  ,
                  tabItem(tabName = "BD_REP",
                          h1("Rapport d'activite"),
                          fluidRow(box(title =  "Filtre a Selectionner",status = "success",solidHeader =F,width = 12,collapsible = F,collapsed = F,
                                       fluidRow(column(2,selectInput("REP_SEXE",label = "SEXE",choices = c("","HOMME","FEMME"))),
                                                column(2,selectInput("REP_CONT",label = "CONTRAT",choices = c("","CESA","CAPE"))),
                                                column(2,selectInput("REP_AGE",label = "TRANCHE AGE",choices =c("","Moins de 25 ans","Entre 25 et 30 ans","Entre 30 et 35 ans","Entre 35 et 40 ans",
                                                                                                                "Entre 40 et 45 ans","Entre 45 et 50 ans","Plus de 50 ans"))),
                                                column(6,selectInput("REP_VILLE",label = "VILLES",choices = c("",ville_coop),multiple = T))
                                                ),
                                       fluidRow(column(4,selectInput("REP_RAPPR",label = "SEXE",choices = c("Proportion Homme Femme","Vue des Villes","Tranches D'ages"))))
                                      )
                          ),
                          fluidRow(box(title =  "Graphiques",status = "success",solidHeader =F,width = 12,collapsible = F,collapsed = F,
                                       fluidRow(column(12,plotOutput("REP_GRAPH")))
                                       
                          ))
                      
                          ),

                  tabItem(tabName = "SUIV_DELTA",
                          h1("Suivi des Entrees "," Sorties"),
                          fluidRow(box(title =  "Selection de la Periode",status = "success",solidHeader =F,width = 12,collapsible = F,collapsed = F,
                                       fluidRow(column(3,selectInput("SUIV_PERIO",label = "Suivi des E/S sur periode",choices = c("","Mensuel","Trimestre","Annee","Date passee")))))),
                          fluidRow(box(title =  "Tableau de vigilence",status = "success",solidHeader =F,width = 12,collapsible = F,collapsed = F, 
                                      fluidRow(column(6,offset = 3,tableOutput("SUIV_TABLE_ES")))))
                                       
                          )
                )
              )
                  #tabItem(tabName = "SUIV_CONS",
                   #       h1("Suivi des Entrees "," Sorties"),
                    #      fluidRow(box(title =  "Selection du cooperateur",status = "success",solidHeader =F,width = 12,collapsible = F,collapsed = F,
                     #                  fluidRow(column(3,selectInput("REP_PRENOM_NOM",label = "Selectionner le membre de la CAE",choices = nom_coop)))
                                       
                      #    ))
                  #)                 

)
