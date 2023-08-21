import matplotlib
import numpy as np
import matplotlib.pyplot as plt

from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix, accuracy_score
from sklearn.metrics import classification_report
from sklearn import metrics



from sklearn.datasets import load_iris

import pandas as pd
import seaborn as sb


Y_data = np.genfromtxt('ionosphere.data', delimiter=',', dtype=str, usecols=34)
X_data = np.genfromtxt('ionosphere.data', delimiter=',')[:, :34]
#
Y_data = np.where(Y_data == 'g', 1, 0)



X_train, X_test, y_train, y_test = train_test_split(X_data, Y_data, test_size=0.2, stratify=Y_data)

print(f"{sum(y_test) / len(y_test)}")
print(f"{sum(y_train) / len(y_train)}")

model = LogisticRegression()

model.fit(X_train, y_train)

y_pre_train_sk = model.predict(X_train)
y_pre_test_sk = model.predict(X_test)

print(confusion_matrix(y_test, y_pre_test_sk))
print(accuracy_score(y_test, y_pre_test_sk))

print(classification_report(y_test, y_pre_test_sk))


y_test_pred_prob = model.predict_proba(X_test)[:, 1]
fpr, tpr, _ = metrics.roc_curve(y_test, y_test_pred_prob)
auc = metrics.roc_auc_score(y_test, y_test_pred_prob)

plt.plot(fpr, tpr, label="AUC="+str(auc))
plt.ylabel('True Positive Rate')
plt.xlabel('False Positive Rate')
plt.legend()
plt.show()


thresholds = np.linspace(0, 1, 100)
fpr = []
tpr = []
for thr in thresholds:
    y_pre_test = 1 * (y_test_pred_prob > thr)
    tn, fp, fn, tp = confusion_matrix(y_test, y_pre_test).ravel()
    fpr.append(fp/(fp+tn))
    tpr.append(tp/(fn+tp))
# plt.plot(fpr, tpr)
# plt.ylabel('True Positive Rate')
# plt.xlabel('False Positive Rate')
# plt.title("Drugi")
# plt.show()


fig = plt.figure(figsize=(10, 5))

plt.subplot(1, 2, 1)
plt.plot(thresholds, fpr)
plt.xlabel("Threshold")
plt.ylabel('False Positive Rate')

plt.subplot(1, 2, 2)
plt.plot(thresholds, tpr)
plt.xlabel("Threshold")
plt.ylabel('True Positive Rate')

plt.show()
