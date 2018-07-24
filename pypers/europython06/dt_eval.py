"""
Doc tester evaluator
"""

def evaldoctest():
    code = ''.join(line[4:] for line in raw_input().splitlines())
    print code

if __name__ == '__main__':
    while True:
        evaldoctest()
