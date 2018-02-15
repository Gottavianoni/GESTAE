## app.R ##
#SERVEUR

server <- function(input,output,session) {
  
  rep <- "C:/Users/gottavianoni/Desktop/GESTAE/App-1/temp/"
  #rep <- "C:/Users/Sapie/Nextcloud Sylvie/SAPIE (2)/COOPERATIVE D'ACTIVITES/BASE DE DONNEES COOPERATEURS/"
  
  observeEvent(input$SAIS_VALIDATE,{
    nomcols <- c("ID","CIV","NOM","PRENOM","NOMJF","SEXE","PARET","SECU","CAF","SIT","NBP","FMONOP","MENEMP","NAIS","CPN","NAT","AGE","NADR","VOIE","COMP","CPA","VILLE",
                 "QUART","TEL1","TEL2","MAILPE","MAILPR","SDF","EXC","DIP","QUAL","SITAC","SALAR","HANDI","RECHEMP","RECHEMP2","ORIEN","ACTIV","SECTACT","DATENT","CONT","DATER1","DATER2",
                 "DATEFIN","MOTIF","SITDOR","SORSAL","SORNSAL","NEWADR")
    
    if(length(input$SAIS_NAIS) == 0) val_nais <- "" else val_nais <- as.character(input$SAIS_NAIS)
    if(length(input$SAIS_DATENT) == 0) val_datent <- "" else val_datent <- as.character(input$SAIS_DATENT)
    if(length(input$SAIS_DATER1) == 0) val_dater1 <- "" else val_dater1 <- as.character(input$SAIS_DATER1)
    if(length(input$SAIS_DATER2) == 0) val_dater2 <- "" else val_dater2 <- as.character(input$SAIS_DATER2)
    if(length(input$SAIS_DATEFIN) == 0) val_datefin <- "" else val_datefin <- as.character(input$SAIS_DATEFIN)
    
    for(val in c("val_nais","val_datent","val_dater1","val_dater2","val_datefin")) assign(val,paste0(substr(get(val),9,10),"/",substr(get(val),6,7),"/",substr(get(val),1,4)))
    #ADDING 01/02/18
    for(val in c("val_nais","val_datent","val_dater1","val_dater2","val_datefin")) if(get(val) == "//") assign(val,"")
    
    new_coop <- cbind(c(input$SAIS_CIV,input$SAIS_NOM,input$SAIS_PRENOM,input$SAIS_NOMJF,input$SAIS_SEXE,input$SAIS_PARET,input$SAIS_SECU,
                        input$SAIS_CAF,input$SAIS_SIT,input$SAIS_NBP,input$SAIS_FMONOP,input$SAIS_MENEMP,val_nais,input$SAIS_CPN,
                        input$SAIS_NAT,input$SAIS_AGE,input$SAIS_NADR,input$SAIS_VOIE,input$SAIS_COMP,input$SAIS_CPA,input$SAIS_VILLE,
                        input$SAIS_QUART,input$SAIS_TEL1,input$SAIS_TEL2,input$SAIS_MAILPE,input$SAIS_MAILPR,input$SAIS_SDF,input$SAIS_EXC,
                        input$SAIS_DIP,input$SAIS_QUAL,input$SAIS_SITAC,input$SAIS_SALAR,input$SAIS_HANDI,input$SAIS_RECHEMP,input$SAIS_RECHEMP2,input$SAIS_ORIEN,input$SAIS_ACTIV,input$SAIS_SECTACT,
                        val_datent,input$SAIS_CONT,val_dater1,val_dater2,val_datefin,input$SAIS_MOTIF,input$SAIS_SITSOR,
                        input$SAIS_SORSAL,input$SAIS_SORNSAL,input$SAIS_NEWADR))
    
    df2 <- read.csv2(paste0(rep,"bd_cae.csv"),colClasses = "character")
    df <- data.frame(t(new_coop))
    names(df) <- nomcols[2:length(nomcols)]
    df$ID <- max(as.numeric(df2$ID)) + 1
    
    df <- df[,nomcols]
    df2 <- rbind(df2,df)
    
    df2 <- df2[order(df2$NOM),]
    
    write.csv2(df2,paste0(rep,"bd_cae.csv"),row.names = F)
    updateSelectInput(session, inputId = "MOD_PRENOM_NOM",choices = paste(df2$PRENOM,df2$NOM))
    
    updateTextInput(session, inputId = "MOD_CIV" ,value = NULL)
    updateTextInput(session, inputId = "MOD_NOM" , value = NULL)
    updateTextInput(session, inputId = "MOD_PRENOM",value = NULL)
    updateTextInput(session, inputId = "MOD_NOMJF", value = NULL)
    updateTextInput(session, inputId = "MOD_SEXE", value = NULL)
    updateTextInput(session, inputId = "MOD_PARET", value = NULL)
    updateTextInput(session, inputId = "MOD_SECU",value = NULL)
    updateTextInput(session, inputId = "MOD_CAF", value = NULL)
    updateTextInput(session, inputId = "MOD_SIT", value = NULL)
    updateTextInput(session, inputId = "MOD_NBP", value = NULL)
    updateTextInput(session, inputId = "MOD_FMONOP", value = NULL)
    updateTextInput(session, inputId = "MOD_MENEMP",value = NULL)
    updateDateInput(session, inputId = "MOD_NAIS", value = NULL)
    updateTextInput(session, inputId = "MOD_CPN", value = NULL)
    updateTextInput(session, inputId = "MOD_NAT", value = NULL)
    updateTextInput(session, inputId = "MOD_AGE", value = NULL)
    updateTextInput(session, inputId = "MOD_NADR", value = NULL)
    updateTextInput(session, inputId = "MOD_VOIE", value = NULL)
    updateTextInput(session, inputId = "MOD_COMP", value = NULL)
    updateTextInput(session, inputId = "MOD_CPA", value = NULL)
    updateTextInput(session, inputId = "MOD_VILLE", value = NULL)
    updateTextInput(session, inputId = "MOD_QUART", value = NULL)
    updateTextInput(session, inputId = "MOD_TEL1", value = NULL)
    updateTextInput(session, inputId = "MOD_TEL2", value = NULL)
    updateTextInput(session, inputId = "MOD_MAILPE", value = NULL)
    updateTextInput(session, inputId = "MOD_MAILPR", value = NULL)
    updateTextInput(session, inputId = "MOD_SDF", value = NULL)
    updateTextInput(session, inputId = "MOD_EXC", value = NULL)
    updateTextInput(session, inputId = "MOD_DIP", value = NULL)
    updateTextInput(session, inputId = "MOD_QUAL", value = NULL)
    updateTextInput(session, inputId = "MOD_SITAC", value = NULL)
    updateTextInput(session, inputId = "MOD_SALAR", value = NULL)
    updateTextInput(session, inputId = "MOD_HANDI", value = NULL)
    updateTextInput(session, inputId = "MOD_RECHEMP", value = NULL)
    updateTextInput(session, inputId = "MOD_RECHEMP2", value = NULL)
    updateTextInput(session, inputId = "MOD_ORIEN", value = NULL)
    updateTextInput(session, inputId = "MOD_ACTIV", value = NULL)
    updateTextInput(session, inputId = "MOD_SECTACT", value = NULL)
    updateDateInput(session, inputId = "MOD_DATENT", value = NULL)
    updateTextInput(session, inputId = "MOD_CONT", value = NULL)
    updateDateInput(session, inputId = "MOD_DATER1", value = NULL)
    updateDateInput(session, inputId = "MOD_DATER2", value = NULL)
    updateDateInput(session, inputId = "MOD_DATEFIN", value = NULL)
    updateTextInput(session, inputId = "MOD_MOTIF", value = NULL)
    updateTextInput(session, inputId = "MOD_SITSOR", value = NULL)
    updateTextInput(session, inputId = "MOD_SORSAL", value = NULL)
    updateTextInput(session, inputId = "MOD_SORNSAL", value = NULL)
    updateTextInput(session, inputId = "MOD_NEWADR", value = NULL)

    })
  
    observeEvent(input$MOD_PRENOM_NOM,{
    df2 <- read.csv2(paste0(rep,"bd_cae.csv"),colClasses = "character")
    df2$NAIS <- as.Date(df2$NAIS,format = "%d/%m/%Y")
    df2$DATENT <- as.Date(df2$DATENT,format = "%d/%m/%Y")
    df2$DATER1 <- as.Date(df2$DATER1,format = "%d/%m/%Y")
    df2$DATER2 <- as.Date(df2$DATER2,format = "%d/%m/%Y")
    df2$DATEFIN <- as.Date(df2$DATEFIN,format = "%d/%m/%Y")
    id <- df2[paste(df2$PRENOM,df2$NOM) == input$MOD_PRENOM_NOM,"ID"]
    ind <- which(paste(df2$ID) == id)
    ligne <- df2[ind[1],]
    if(length(ind) == 0) updateSelectInput(session, inputId = "MOD_PRENOM_NOM",choices = paste(df2$PRENOM,df2$NOM))
    updateTextInput(session, inputId = "MOD_CIV" ,value = ligne$CIV)
    updateTextInput(session, inputId = "MOD_NOM" , value = ligne$NOM)
    updateTextInput(session, inputId = "MOD_PRENOM",value = ligne$PRENOM)
    updateTextInput(session, inputId = "MOD_NOMJF", value = ligne$NOMJF)
    updateTextInput(session, inputId = "MOD_SEXE", value = ligne$SEXE)
    updateTextInput(session, inputId = "MOD_PARET", value = ligne$PARET)
    updateTextInput(session, inputId = "MOD_SECU",value = ligne$SECU)
    updateTextInput(session, inputId = "MOD_CAF", value = ligne$CAF)
    updateTextInput(session, inputId = "MOD_SIT", value = ligne$SIT)
    updateTextInput(session, inputId = "MOD_NBP", value = ligne$NBP)
    updateTextInput(session, inputId = "MOD_FMONOP", value = ligne$FMONOP)
    updateTextInput(session, inputId = "MOD_MENEMP",value = ligne$MENEMP)
    updateDateInput(session, inputId = "MOD_NAIS", value = ligne$NAIS)
    updateTextInput(session, inputId = "MOD_CPN", value = ligne$CPN)
    updateTextInput(session, inputId = "MOD_NAT", value = ligne$NAT)
    updateTextInput(session, inputId = "MOD_AGE", value = ligne$AGE)
    updateTextInput(session, inputId = "MOD_NADR", value = ligne$NADR)
    updateTextInput(session, inputId = "MOD_VOIE", value = ligne$VOIE)
    updateTextInput(session, inputId = "MOD_COMP", value = ligne$COMP)
    updateTextInput(session, inputId = "MOD_CPA", value = ligne$CPA)
    updateTextInput(session, inputId = "MOD_VILLE", value = ligne$VILLE)
    updateTextInput(session, inputId = "MOD_QUART", value = ligne$QUART)
    updateTextInput(session, inputId = "MOD_TEL1", value = ligne$TEL1)
    updateTextInput(session, inputId = "MOD_TEL2", value = ligne$TEL2)
    updateTextInput(session, inputId = "MOD_MAILPE", value = ligne$MAILPE)
    updateTextInput(session, inputId = "MOD_MAILPR", value = ligne$MAILPR)
    updateTextInput(session, inputId = "MOD_SDF", value = ligne$SDF)
    updateTextInput(session, inputId = "MOD_EXC", value = ligne$EXC)
    updateTextInput(session, inputId = "MOD_DIP", value = ligne$DIP)
    updateTextInput(session, inputId = "MOD_QUAL", value = ligne$QUAL)
    updateTextInput(session, inputId = "MOD_SITAC", value = ligne$SITAC)
    updateTextInput(session, inputId = "MOD_SALAR", value = ligne$SALAR)
    updateTextInput(session, inputId = "MOD_HANDI", value = ligne$HANDI)
    updateTextInput(session, inputId = "MOD_RECHEMP", value = ligne$RECHEMP)
    updateTextInput(session, inputId = "MOD_RECHEMP2", value = ligne$RECHEMP2)
    updateTextInput(session, inputId = "MOD_ORIEN", value = ligne$ORIEN)
    updateTextInput(session, inputId = "MOD_ACTIV", value = ligne$ACTIV)
    updateTextInput(session, inputId = "MOD_SECTACT", value = ligne$SECTACT)
    updateDateInput(session, inputId = "MOD_DATENT", value = ligne$DATENT)
    updateTextInput(session, inputId = "MOD_CONT", value = ligne$CONT)
    updateDateInput(session, inputId = "MOD_DATER1", value = ligne$DATER1)
    updateDateInput(session, inputId = "MOD_DATER2", value = ligne$DATER2)
    updateDateInput(session, inputId = "MOD_DATEFIN", value = ligne$DATEFIN)
    updateTextInput(session, inputId = "MOD_MOTIF", value = ligne$MOTIF)
    updateTextInput(session, inputId = "MOD_SITSOR", value = ligne$SITSOR)
    updateTextInput(session, inputId = "MOD_SORSAL", value = ligne$SORSAL)
    updateTextInput(session, inputId = "MOD_SORNSAL", value = ligne$SORNSAL)
    updateTextInput(session, inputId = "MOD_NEWADR", value = ligne$NEWADR)
  })
  
    
    #observeEvent(input$MOD_PRENOM_NOM,{
     # df2 <- read.csv2(paste0(rep,"bd_cae.csv"),colClasses = "character")
      #updateSelectInput(session, inputId = "MOD_PRENOM_NOM",choices = paste(df2$NOM,df2$PRENOM))
    #})
  
    observeEvent(input$MOD_VALIDATE,{
    
    nomcols <- c("ID","CIV","NOM","PRENOM","NOMJF","SEXE","PARET","SECU","CAF","SIT","NBP","FMONOP","MENEMP","NAIS","CPN","NAT","AGE","NADR","VOIE","COMP","CPA","VILLE",
                 "QUART","TEL1","TEL2","MAILPE","MAILPR","SDF","EXC","DIP","QUAL","SITAC","SALAR","HANDI","RECHEMP","RECHEMP2","ORIEN","ACTIV","SECTACT","DATENT","CONT","DATER1","DATER2",
                 "DATEFIN","MOTIF","SITDOR","SORSAL","SORNSAL","NEWADR")
    
    if(length(input$MOD_NAIS) == 0) val_nais <- "" else val_nais <- as.character(input$MOD_NAIS)
    if(length(input$MOD_DATENT) == 0) val_datent <- "" else val_datent <- as.character(input$MOD_DATENT)
    if(length(input$MOD_DATER1) == 0) val_dater1 <- "" else val_dater1 <- as.character(input$MOD_DATER1)
    if(length(input$MOD_DATER2) == 0) val_dater2 <- "" else val_dater2 <- as.character(input$MOD_DATER2)
    if(length(input$MOD_DATEFIN) == 0) val_datefin <- "" else val_datefin <- as.character(input$MOD_DATEFIN)
    
    for(val in c("val_nais","val_datent","val_dater1","val_dater2","val_datefin")) assign(val,paste0(substr(get(val),9,10),"/",substr(get(val),6,7),"/",substr(get(val),1,4)))
    #ADDING 01/02/18 
    for(val in c("val_nais","val_datent","val_dater1","val_dater2","val_datefin")) if(get(val) == "//") assign(val,"")
    
    new_coop <- cbind(c(input$MOD_CIV,input$MOD_NOM,input$MOD_PRENOM,input$MOD_NOMJF,input$MOD_SEXE,input$MOD_PARET,input$MOD_SECU,
                        input$MOD_CAF,input$MOD_SIT,input$MOD_NBP,input$MOD_FMONOP,input$MOD_MENEMP,val_nais,input$MOD_CPN,
                        input$MOD_NAT,input$MOD_AGE,input$MOD_NADR,input$MOD_VOIE,input$MOD_COMP,input$MOD_CPA,input$MOD_VILLE,
                        input$MOD_QUART,input$MOD_TEL1,input$MOD_TEL2,input$MOD_MAILPE,input$MOD_MAILPR,input$MOD_SDF,input$MOD_EXC,
                        input$MOD_DIP,input$MOD_QUAL,input$MOD_SITAC,input$MOD_SALAR,input$MOD_HANDI,input$MOD_RECHEMP,input$MOD_RECHEMP2,input$MOD_ORIEN,input$MOD_ACTIV,input$MOD_SECTACT,
                        val_datent,input$MOD_CONT,val_dater1,val_dater2,val_datefin,input$MOD_MOTIF,input$MOD_SITSOR,
                        input$MOD_SORSAL,input$MOD_SORNSAL,input$MOD_NEWADR))
    
    df2 <- read.csv2(paste0(rep,"bd_cae.csv"),colClasses = "character")
    df <- data.frame(t(new_coop))
    names(df) <- nomcols[2:length(nomcols)]
    df$ID <- as.character(df2[input$MOD_PRENOM_NOM == paste(df2$PRENOM,df2$NOM),"ID"])
    df <- df[,nomcols]
    df2 <- read.csv2(paste0(rep,"bd_cae.csv"),colClasses = "character")
    #ADDING 01/02/18
    ind <- which(df2$ID == df$ID)
    if(length(ind) > 0) df2 <- df2[-ind,] else print("Pas de Correspondance")
    df2 <- rbind(df2,df)
    df2 <- df2[order(df2$NOM),]
    write.csv2(df2,paste0(rep,"bd_cae.csv"),row.names = F)
    updateSelectInput(session, inputId = "MOD_PRENOM_NOM",choices = paste(df2$PRENOM,df2$NOM))
    
  })
  
  observeEvent(input$MOD_DELETE,{
  df2 <- read.csv2(paste0(rep,"bd_cae.csv"),colClasses = "character")
  ind <- which(paste(df2$PRENOM,df2$NOM) == input$MOD_PRENOM_NOM)
  if(length(ind) > 0) df2 <- df2[-ind,] else print("Pas de Correspondance")
  df2 <- df2[order(df2$NOM),]
  write.csv2(df2,paste0(rep,"bd_cae.csv"),row.names = F)
  updateSelectInput(session, inputId = "MOD_PRENOM_NOM",choices = paste(df2$PRENOM,df2$NOM))
  })
  
  observeEvent(input$REP_RAPPR,{
    df2 <- read.csv2(paste0(rep,"bd_cae.csv"),colClasses = "character")
    
    sexe <- input$REP_SEXE
    contrat <- input$REP_CONT
    tranche <- input$REP_AGE
    ville <- input$REP_VILLE
    
    if(sexe != "" ) df2 <- df2[df2[,"SEXE"] == substr(sexe,1,1),]
    if(contrat != "" ) df2 <- df2[df2[,"CONT"] == contrat,]
    if(tranche != "" ) df2 <- df2[df2[,"AGE"] == tranche,]
    if(length(ville) == 0 ) ville <- ""
    if(ville != "" ) df2 <- df2[df2[,"VILLE"] == ville,]
    
    if(input$REP_RAPPR == "Proportion Homme Femme")  {
      
                 test <- data.frame(table(df2$SEXE[df2$SEXE %in% c("H","F")]))
                 names(test)[1] <- "Sexe"
                 
                 bp <- ggplot(test, aes(x="", y=Freq, fill=Sexe))+
                   geom_bar(width = 1, stat = "identity")
                 
                 pie <- bp  +  coord_polar("y",start = 0)
                 
                 blank_theme <- theme_minimal()+
                   theme(
                     axis.title.x = element_blank(),
                     axis.title.y = element_blank(),
                     panel.border = element_blank(),
                     panel.grid=element_blank(),
                     axis.ticks = element_blank(),
                     plot.title=element_text(size=14, face="bold")
                   )
                 
                final <-  pie + scale_fill_brewer("Sexe") + blank_theme +
                   theme(axis.text.x=element_blank()) + 
                   geom_text(aes(y = Freq/2 + c(0, cumsum(Freq)[-length(Freq)]), 
                                 label = gsub("%|\\,0{1,}","",Freq)), size=5)
                
                output$REP_GRAPH <- renderPlot(final)
      }
    
    if(input$REP_RAPPR == "Tranches D'ages")  {
      
      test <- data.frame(table(df2$AGE))
      names(test)[1] <- "Tranches"
      
      bp <-ggplot(data=test, aes(x=Tranches, y=Freq, fill = Tranches)) +
      geom_bar(stat="identity") + theme_minimal() +
      geom_text(aes(label = Freq, y = (Freq) - (0.5 * Freq)), size = 4)
      
      
      
        
      output$REP_GRAPH <- renderPlot(bp)
    }
    
    
    if(input$REP_RAPPR == "Vue des Villes")  {
      
      test <- data.frame(table(toupper(df2$VILLE)))
      names(test)[1] <- "Villes"
      
      bp <-ggplot(data=test, aes(x=Villes, y=Freq, fill = Villes)) +
        geom_bar(stat="identity") + theme_minimal() +
        geom_text(aes(label = Freq, y = (Freq) - (0.5 * Freq)), size = 4)
      
    
      output$REP_GRAPH <- renderPlot(bp)
    }
    
  })
  
  observeEvent(input$SUIV_PERIO,{
    df2 <- read.csv2(paste0(rep,"bd_cae.csv"),colClasses = "character")
    df <- df2[df2$CONT == "CAPE",c("NOM","PRENOM","CONT","DATEFIN")]
    df$delta <- difftime(as.Date(df$DATEFIN,format = "%d/%m/%Y"),Sys.Date(),unit = "days")
    
    if(input$SUIV_PERIO == "" ) df <- df[df$delta > 10000 ,]
    if(input$SUIV_PERIO == "Mensuel" ) df <- df[df$delta < 30 & df$delta >= 0 ,]
    if(input$SUIV_PERIO == "Trimestre" ) df <- df[df$delta < 90 & df$delta >= 0,]
    if(input$SUIV_PERIO == "Annee" ) df <- df[df$delta < 350 & df$delta >= 0,]
    if(input$SUIV_PERIO == "Date passee" ) df <- df[df$delta < 0,]
    
    df <- df[order(df$delta),]
    
    names(df)[5] <- "Fin CAPE Dans"
    if(input$SUIV_PERIO == "Date passee" )  names(df)[5] <- "Date CAPE Depassee"
            
    output$SUIV_TABLE_ES <- renderTable(df)
    
  })
  
  
}