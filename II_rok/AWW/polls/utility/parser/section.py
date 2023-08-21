class Section:
    def __init__(self, start, end, type):
        self.start = start
        self.end = end
        self.type = type

    def __str__(self):
        return f"Section({self.start}, {self.end}, '{self.type}')"


class Number:
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return f"{self.value}"

def increment(num):
    num.value += 1