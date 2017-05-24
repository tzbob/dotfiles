#!/usr/bin/env python

import csv
import datetime

with open('pomodori.csv', 'r') as csvfile:
    pomoreader = csv.reader(csvfile)
    for row in pomoreader:
        print(row)
