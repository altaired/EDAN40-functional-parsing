inputs = [1024, 2]
outputs = []


def read():
    return inputs.pop(0)


def write(x):
    outputs.append(x)


n = read()
b = read()

m = 1
s = 0
p = 1
while n > 0:
    q = n/b
    r = n - q*b
    write(r)
    s = p*r+s
    p = p*10
    n = q
write(s)


print(outputs)
