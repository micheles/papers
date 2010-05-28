def print_(color, txt):
    code = {'black': 30, 'red': 31}[color]
    print '\x1b[%dm%s\x1b[0m' % (code, txt)

def main(delete, delete_all, color):
    """usage: %prog
    -c, --color=black: set default color
    -d, --delete=: delete the given file
    -a, --delete-all: delete all files
    """
    if delete_all:
        print_(option.color, "Delete all files")
    elif delete:
        print_(option.color, "Delete the file %s" % option.delete)
    else:
        clap.exit()

if __name__=='__main__':
    import clap; clap.call(main)

