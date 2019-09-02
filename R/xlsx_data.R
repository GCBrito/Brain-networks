
#Name: exceldata()
#Objective: Saves r,p and graph metrics data to excel spreadsheet (xlsx)
#Requirements: processed correlation matrix and network 
#Outside variables needed: r, p, G_attr, V_attr
#Output: 1.xlsx file containing correlation data in the working folder ; 2. xlsx file containing graph metrics data
#To do: 


exceldata <-  function() {
  
  if(file.exists("all_data.xlsx"))   { file.remove("all_data.xlsx")}
  if(file.exists("graph_metrics.xlsx"))   { file.remove("graph_metrics.xlsx")}
  
for (i in 1:length(r)) {
  
    temp1 <- paste("r_T", i, sep="") 
    temp2 <- paste("p_T", i, sep="") 

    write.xlsx2(r[[i]] ,"all_data.xlsx", sheetName = temp1, append = TRUE )
    write.xlsx2(p[[i]] ,"all_data.xlsx", sheetName = temp2, append = TRUE )
    
}
  
  write.xlsx2(V_attr ,"graph_metrics.xlsx", sheetName = "vertex_metrics", append=TRUE)
  write.xlsx2(G_attr ,"graph_metrics.xlsx", sheetName = "network_metrics", append=TRUE)
  
  rm("exceldata", pos = 1)
}