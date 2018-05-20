inputs = [4, 4]
outputs = []


def read():
    return inputs.pop(0)


def write(x):
    outputs.append(x)


a = read()
b = read()

s = 3
while a:
    c = a**s
    d = 2**a
    write(c)
    write(d)
    a = a-1
write(a)


print(outputs)
