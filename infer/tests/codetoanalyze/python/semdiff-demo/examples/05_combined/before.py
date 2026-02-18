import os

def lookup(table, key):
    value = table.get(key)
    return value

def format_entry(name):
    if name is None:
        return "<unknown>"
    return name.upper()
