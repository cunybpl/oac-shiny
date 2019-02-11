# NO-BAS: Outside Air Control

### Overview 

1. Upload Data
2. Make an Occupancy Schedule (optional)
3. Plot and Adjust Parameters
4. Name and Export Plot

### Uploading Data

To use this application you need _at least_ one **.csv** file corresponding logger data from:

* Discharge Air Temperature (**DAT**)
* Mixed Air Temperature (**MAT**)
* Outside Air Temperature (**OAT**)
* Return Air Temperature (**RAT**)

Optionally, you can include:

* Fan Status
* Occupancy Schedule 



![Upload Panel](https://raw.githubusercontent.com/cunybpl/oac-shiny/master/oac-shiny/img/upload_window.png)

**1.** To upload a trend click "**Browse...**" From the file browser pop-up, locate the appropriate **.csv** file and select it. 

**2.** _Optionally_, restrict the date range to trend using the calendar menu. Restricting the date range may improve performance with large data sets. 

**3.** After uploading your **.csv** files, Click "**Update Plot!**" to update the plotting window on the right. 



### Manipulating your Plot

From the plotting window you can zoom in/out, hide /show trends, rename your plot, and export your finished product to a **.png** file. 

![Plotting Window](https://raw.githubusercontent.com/cunybpl/oac-shiny/master/oac-shiny/img/plot_window.png)

**1.** Click on an icon from the tool-bar to use it on your plot. 

![tool-bar](https://raw.githubusercontent.com/cunybpl/oac-shiny/master/oac-shiny/img/tools.png) 

From Left to Right: 

* The "_Download plot as png_"  tool downloads your current plot to **.png** file you can save and share. 
* The "_Zoom_" tool (selected by default), allows zoom into a selected area of the plot. To use _zoom_, _click and drag_ on the plotting window. 
* The "_Pan_" tool allows you to move the plotting window. To _pan, click and drag_ on the plotting window. 
* The "_Zoom in_" and "_Zoom out_" buttons zoom in and out of the plot by a set increment. 
* The "_Autoscale_" and "_Reset Axes_" buttons reset the plot to it's starting position.

**2.**  If you wish to hide a specific trend, click the appropriate check-box under the plotting window. Click "Update Plot!"(**4**) touncheckuncheck