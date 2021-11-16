import requests

#submit API request with 1 day 
x = "1"
resp = requests.get("http://127.0.0.1:9937/diabetes", params={"Age":"53","Gender":"Male",
"Polyuria":"Yes",
"Polydipsia":"Yes",
"SuddenWeightLoss":"Yes",
"Weakness":"Yes",
"Polyphagia":"Yes",
"GenitalThrush":"Yes",
"VisualBlurring": "Yes",
"Itching":"Yes",
"Irritability":"No",
"DelayedHealing":"No",
"PartialParesis":"No",
"MuscleStiffness":"No",
"Alopecia":"Yes",
"Obesity":"Yes",}, verify=False)

#resp2 = requests.get(("http://127.0.0.1:8339/connection"),params={"age":"53", "gender":"Male"})
#print the prediction 
print(resp.content)
#success
