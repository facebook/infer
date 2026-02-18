from pathlib import Path

def lookup(table: str, key: str) -> int:
    value: int = table.get(key)
    return value

def format_entry(name: str) -> str:
    if name is None:
        return "<unknown>"
    return name.upper()
