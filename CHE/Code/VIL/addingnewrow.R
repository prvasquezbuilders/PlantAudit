library(xlsx)

df = data.frame(a=c(1,2,3),b=c(2,3,4))
titles = c( "A", "B","C")

wb<- createWorkbook(type = "xlsx")
sheets <- getSheets(wb)
z <- length(titles)

# creating the style
SUB_TITLE_STYLE <- CellStyle(wb) + 
  Font(wb,  heightInPoints=14, 
       isItalic=TRUE, isBold=FALSE)

# loop
for (i in (1:z)) {
  createSheet(wb, sheetName=paste0("Sheet",i))
  sheets <- getSheets(wb)
  sheet <- sheets[[i]]
  # Add sub title
  
  addDataFrame(df, sheet, col.names = TRUE, row.names = FALSE,
               startRow = 1, startColumn = 1)  
  rows <-createRow(sheet,rowIndex=1)
  xlsx.addTitle(sheet, rowIndex=1, 
                title= paste0(titles[i]),
                titleStyle = SUB_TITLE_STYLE)
}

saveWorkbook(wb, "tmp.regioes.xlsx")