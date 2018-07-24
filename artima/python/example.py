'A trivial application exemplifying the call_with_conf configuration mechanism.'

dev_conf = {
    'host' : 'localhost',
    'port': '8080',
    'dsn' : 'postgres://user:pwd@localhost/development_db',
    }
 
def main(conf): # just print out the configuration
    for k, v in conf.iteritems():
        print k, v

if __name__ == '__main__': # by default use the development configuration
    main(dev_conf)
