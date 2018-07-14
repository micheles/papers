import os
import time
import random
from datetime import date, timedelta

NUMCODES = 10000


def save_data(day, datadir):
    print('Saving data for %s' % day)
    with open('%s/%s.csv' % (datadir,  day), 'w') as f:
        for code in range(NUMCODES):
            price = random.random() * 10 + 10
            f.write('%06d,%s,%s\n' % (code, day, price))


def main(datadir):
    t0 = time.time()
    if not os.path.exists(datadir):
        os.mkdir(datadir)
    today = date.today()
    dates = []
    for i in range(730):
        day = today - timedelta(i)
        if day.weekday() in (1, 2, 3, 4, 5):
            dates.append(str(day))
            save_data(day, datadir)
    dt = time.time() - t0
    print('Saved %d files in %d seconds' % (len(dates), dt))


if __name__ == '__main__':
    main('/tmp/data')
