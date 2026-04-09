class Greeting:
    def __init__(self, prefix, name):
        self.prefix = prefix
        self.name = name

def main():
    g = Greeting("Hello, ", "world")
    return println_str(str_concat(prefix(g), name(g)))
