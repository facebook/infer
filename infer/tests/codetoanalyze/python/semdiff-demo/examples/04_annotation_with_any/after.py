from typing import Any

def fetch(url: str) -> Any:
    return download(url)

def parse(data: str) -> list[str]:
    return data.split(",")
