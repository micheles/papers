import os
import time
import subprocess
from openquake.baselib.performance import Monitor
from openquake.baselib.parallel import Starmap
from openquake.commonlib.datastore import hdf5new


def import_data(path, mon):
    cmd = "COPY price FROM '%s' WITH DELIMITER ','" % path
    subprocess.call(['psql', '-c', cmd])


def main(datadir):
    t0 = time.time()
    n = 0
    mon = Monitor('import_data', hdf5new().path)
    iterargs = ((os.path.join(datadir, f), mon.new())
                for f in os.listdir(datadir))
    for _ in Starmap(import_data, iterargs):
        n += 1
    dt = time.time() - t0
    print('Imported %d files in %d seconds' % (n, dt))
    print('Generated %s' % mon.hdf5path)

if __name__ == '__main__':
    subprocess.call(['psql', '-f', 'db.sql'])  # create the db
    main('/tmp/data')
