import os
import time
import subprocess


def import_data(path):
    cmd = "COPY price FROM '%s' WITH DELIMITER ','" % path
    subprocess.call(['psql', '-c', cmd])


def main(datadir):
    t0 = time.time()
    n = 0
    fnames = (os.path.join(datadir, f) for f in os.listdir(datadir))
    for _ in map(import_data, fnames):
        n += 1
    dt = time.time() - t0
    print('Imported %d files in %d seconds' % (n, dt))


if __name__ == '__main__':
    subprocess.call(['psql', '-f', 'db.sql'])  # create the db
    main('/tmp/data')
