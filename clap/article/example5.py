from datetime import datetime

def main(dsn, today=datetime.today()):
    "Do something on the database"
    print(dsn, today)

if __name__ == '__main__':
    import clap; clap.call(main)
