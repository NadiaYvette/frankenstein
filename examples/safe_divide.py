def safe_divide(x, y):
    try:
        if y == 0:
            raise 99
        return x // y
    except:
        return 0

def main():
    return safe_divide(10, 0) + safe_divide(20, 4)
