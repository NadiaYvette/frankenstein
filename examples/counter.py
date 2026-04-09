def main():
    r = new_ref(0)
    set_ref(r, get_ref(r) + 1)
    set_ref(r, get_ref(r) + 1)
    set_ref(r, get_ref(r) + 40)
    return get_ref(r)
