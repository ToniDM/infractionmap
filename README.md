# infractionmap
Interactive map of fines register in Gijon from years 2015 to 2017
User can apply the following filters: 
-Period selection 
-Time range (from 0h to 24h) 
-Type of infraction 
-Calification of infraction 
-Fee 
-Points 

Every single point in the map, contains a tooltip with information of: 
-Number of fines 
-Type of infraction (more usual) 
-Average fee 
-Average discount 
-Average points 
-If type is speed, speed and speeding in that point 

From original dataset provided with 183212 fees, some are excluded due to: 
-Coordinates were not found (even in google API) - 45 cases 
-Location out of Asturias - 5 cases 
-No information of place/street in dataset - 19 cases 
Finally 183.143 infractions represented 

Data preprocessing was done for: 
-Enrich latitude and longitude fields, filling missing values using Google map API 
-Create a new more detailed classification of infractions based on texts from explicative field. Resulting in 13 categories 

Category description: 
- alcohol: drive under the influence of drugs or alcohol 
- distraction: use of mobile phone or other not allowed devices 
- driving: drive recklessly 
- lane change: change lane in inappropriate moment or place 
- no collaboration: do not show documentation to authorities or do not collaborate with them 
- other 
- overtaking: in a not allowed place 
- parking: in a not allowed place(or exceed time) 
- restricted area: access to a restricted area without permission 
- seat belt: not use the seat belt 
- sign post: omit a traffic signal 
- speed: exceed speed limits 
- traffic light: omit a traffic light 


The repositoy contains:
1.input: raw data provided and files of grouped categories
2.preprocesado.R: r script that process data. The result is dd.rdata file 
3. app: shiny app
-app.R: R shiny script
-styles.css: css file defining html styles
-dd.rdata: processed data from script preprocesado.r
