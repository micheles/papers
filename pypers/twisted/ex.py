from quixote.server import simple_server
from quixote.publish import Publisher
from quixote.directory import Directory

class MySite(Directory):
    _q_exports = ["hello"]
    def hello(self):
        return "hello"


def factory():
    return Publisher(MySite())

if __name__ == "__main__":
    simple_server.run(factory, "localhost", 7080)
