fileConn<-file("CCFraudRShinyApp/cleardisplay.txt")
writeLines(c("FALSE"), fileConn)
close(fileConn)