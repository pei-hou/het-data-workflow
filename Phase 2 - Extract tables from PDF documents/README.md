In this phase, please download Tabula on your machine.

Once complete, execute tabula.exe and go to http://127.0.0.1:8080/

The GUI for extracting tables from PDF will show. 

You could interact with the GUI by 

first importing the PDF documents

selecting the area of the target table (Although you could select all tables you want in this step, but this sometimes will be problematic for extracting more tables at once.)

saving one table in one csv file.

In our usecase, the output table in the csv format has column squeezing problem, which means the a lot of columns are squeezed in one column.

To solve this problem, we propose to use text similarity metric and a given correct header to iteratively modify the columns (deciding to split them or not).

Note that a given correct header could be given is due to the data tables are all in the same format and it's affordable to provide one template manually.
