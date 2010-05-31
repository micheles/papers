from datetime import datetime

def main(dsn, *scripts):
    "Run the given scripts on the database"
    for script in scripts:
        print('executing %s' % script)

if __name__ == '__main__':
    import clap; clap.call(main)
