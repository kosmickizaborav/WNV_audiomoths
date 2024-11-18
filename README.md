# WNV_audiomoths

the list of all the species that BirdNet was trained on (i.e. BirdNET_GLOBAL_6K_V2.4_Labels.txt)
was taken from BirdNET-Analyzer/checkpoints/V2.4, available upon the BirdNET-Alalyzer download.

we got the list of the species that was used to analyze the recordings,
using the specified coordinates, which contained 282 species.

```
python3 /home/nina/BirdNET-Analyzer/species.py 
    --o /home/nina/R_projects/WNV_audiomoths/Data/species_cortalet.txt 
    --lat 42.225039 
    --lon 3.092257
```


BirdNet Analyzer was ran using the code below. the coordinates of El Cortalet taken from:
https://parcsnaturals.gencat.cat/ca/xarxa-de-parcs/aiguamolls-emporda/gaudeix-del-parc/com-accedir-hi/

```
python3 /home/nina/BirdNET-Analyzer/analyze.py 
    --i /home/nina/Audiomoths/Files 
    --o /home/nina/Audiomoths/Results 
    --lat 42.225039
    --lon 3.092257
```

to convert confidence score to the probability that a prediction is a true positive in a given species. 
we extract clips that represent a range of confidence score that you would like to include. 

```
python3 /home/nina/BirdNET-Analyzer/segments.py 
    --audio /home/nina/Audiomoths/Files/ 
    --results /home/nina/Audiomoths/Results/ 
    --o /home/nina/Audiomoths/Segments/ 
    --min_conf 0.1 
    --max_segments 20 
    --seg_length 3 
    --threads 4
```


