#!/usr/bin/env python

import csv
import datetime

with open ('pomodori.csv', 'a') as csvfile:
    pomowriter = csv.writer(csvfile)
    pomowriter.writerow([datetime.datetime.now().isoformat(), "pomodoro"])
