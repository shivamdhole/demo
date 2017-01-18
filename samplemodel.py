import pandas.io.data as web
import numpy as np
import datetime

start = datetime.datetime(2016,1,1)
end = datetime.date.today()

apple = web.DataReader("AAPL","yahoo",start,end)
type(apple)
print(apple.head())
f = open('AirPassengers.csv','r')
i = 0
for line in f:
	print(line)
	i += 1
	if i > 5:
		break
print('File read complete')