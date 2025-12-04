# Fish ID App How To Use
## 1. Take pictures of fish
Put them onto the harddrive, use one folder per day and dive/snorkel site.<br>
This app only works with pictures (jpg & png). If you have a video, I recommend making a screenshot and using that picture.

## 2. Start App
Use the link "Start FishID" on the desktop.<br>
Alternatively, run launch_app.R in RStudio.<br>
Make sure to have the Excel with the database and the one with the species master list closed, otherwise the App will not work.<br>
**Do not move those files!**<br>
The database is located in: "Database\Coral Reef monitoring\Fish_ID_App"<br>
The species master list is located in: "Database\Coral Reef monitoring\Fish ID database_V3"<br>
If a species is missing, you can add it in this Excel and restart the app!<br>
If changing these Excels in inevitable - contact me! (See credits)


## 3. Specify Setup of Entry (Folder, Date and Dive/Snorkel Site)
Choose the folder with pictures via the button "Choose Folder".<br>
Usually they are stored on the harddrive (called DataDir) in "Marine Obersavtions (Snorkel and Dive) dates" - folder.<br>
For the case that no harddrive is available, you can switch to the local User Directory (called UserDir) via the dropdown menu in the upper right.<br>
Furthermore, specify the day, month, year and snorkel/dive site.<br>
These settings **will remain the same for all entries**! If you want to do another folder, just restart the app.<br>
When finished, click the button "Confirm Setup". This might take a while, since the app needs to load all pictures!

## 4. Make an Entry
Note: You can make the picture larger or smaller by hovering over the corners/edges.<br>
Date, snorkel/dive site and reference folder are automatically set by the input in point 3.<br>
**Enter either Species or Common Name**, the **other** one as well as Fish Group, Order, Family and Genus **will be autocompleted!**<br>
If you chose a wrong value, just go to the next pictures and back again, this will reset everything.<br>
Rename the picture according to the scientific name.<br>
When all relevant fields are filled out, click button "Save entry". The app will do two things:<br>
- Your entry will be saved in the Excel-database, so there is no need to touch Excel during the whole process!
- The picture will be renamed in the folder to your choosen name. If the choosen name already exists, there will be **no** new entry and the file is not renamed! Please choose another name!
<br>
You can browse through all pictures in the folder via the previous and next buttons.
Generally, if the picture name is already the scientific name, this fish has already been entered into the database!<br>
If there are two pictures of the same fish for one dive/snorkel event, it will only be saved to the Excel in the first time. 
If you are finished, you may just close the app.