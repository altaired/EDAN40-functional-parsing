inputs = [3, 16]
outputs = []


def read():
    return inputs.pop(0)


def write(x):
    outputs.append(x)


k = read()
n = read()

m = 1
s = 0
p = 1
while n - m > 0:
    if m - m//k*k:
        pass
    else:
        write(m)
    m = m + 1


print(outputs)
