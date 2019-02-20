# noBAS: Outside Air Control

### Overview 

This tutorial describes the proccess of uploading and trending **.csv** files for Ouside Air Control analysis. 

In order for this tool to function properly, all **.csv** files must be obtained through [_HOBOware_](https://www.onsetcomp.com/products/software/hoboware) or [_HOBOmobile_](https://www.onsetcomp.com/products/software/hobomobile). 

***

### <a name="upload"></a>Uploading Data

To use this application you need _at least_ one **.csv** file corresponding to logger data from:

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


***

### Manipulating your Plot

From the plotting window you can zoom in/out, hide /show trends, rename your plot, and export your finished product to a **.png** file. 

![Plotting Window](https://raw.githubusercontent.com/cunybpl/oac-shiny/master/oac-shiny/img/plot_window.png)

**1.** Click on an icon from the tool-bar to use it on your plot. 

![tool-bar](https://raw.githubusercontent.com/cunybpl/oac-shiny/master/oac-shiny/img/tools.png) 

From Left to Right: 

* The "_Download plot as png_"  tool downloads your current plot to a .png file you can share with others. 
* The "_Zoom_" tool (selected by default), allows zoom into a selected area of the plot. To use _zoom_, _click and drag_ on the plotting window. 
* The "_Pan_" tool allows you to move the plotting window. To _pan, click and drag_ on the plotting window. 
* The "_Zoom in_" and "_Zoom out_" buttons zoom in and out of the plot by a set increment. 
* The "_Autoscale_" and "_Reset Axes_" buttons reset the plot to it's starting position.

**2.**  If you wish to hide a specific trend, click the appropriate check-box under the plotting window. Click "**Update Plot!**"(**4**) touncheckuncheck

**3.** Enter a title into the "**Plot Title**" text box to rename your plot. Then click '**Update Plot!**"(**4**).

***

### Adding an Occupancy Schedule

To create an occupancy schedule, navigate to the occupancy wndow by clicking **Occupancy** in the top navigation bar. 
Adjust the parameters for each respective day of the week to form your schedule.

![Slider](https://raw.githubusercontent.com/cunybpl/oac-shiny/master/oac-shiny/img/slider.png)

**1.** To set a day as _unoccupied_, uncheck the the "**occupied**" checkbox.

**2.** Drag the sliders for _Startup_ and _Occupied Hours_ to change your schedule.

![download-preview](https://raw.githubusercontent.com/cunybpl/oac-shiny/master/oac-shiny/img/download_preview.png)

**3.** After adjusting all slliders (Monday,Tuesday...etc.), click "**Update Preview**" to review your schedule. 

**4.** When your schedule is complete, click "**Download Occupancy Schedule**" to download the schedule to your computer. Save the file somewhere you can easily find it. 

Click "**Plotting**" in the top navigation bar to return to the plotting area. Upload the occupancy schedule file by following the steps described in [Uploading Data](#upload).
