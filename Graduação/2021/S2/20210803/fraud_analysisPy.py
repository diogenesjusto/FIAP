import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn import tree
from sklearn.metrics import confusion_matrix

df = pd.read_csv('https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/fraude_study.csv') 

Y = df.iloc[:,12]
X = df.iloc[:,[6,11]]

X_d = pd.get_dummies(X["DESC_MUNICIPIO"])
X_d["FATURADO"] = X["FATURADO"]

X_train, X_test, y_train, y_test = train_test_split(X_d, Y, test_size=0.4, random_state=42)

mod = tree.DecisionTreeClassifier()
mod = mod.fit(X_train, y_train)

y_pred = mod.predict(X_test)

confusion_matrix(y_test, y_pred)
