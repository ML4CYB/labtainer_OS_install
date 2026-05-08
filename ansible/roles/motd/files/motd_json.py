#!/usr/bin/env python3

import json
import os
import subprocess
import time


MAX_MOTD_CHARS = 8000


def read_file(path):
    try:
        with open(path, encoding="utf-8", errors="replace") as file:
            return file.read()
    except OSError:
        return ""


def dynamic_motd():
    if not os.path.isdir("/etc/update-motd.d"):
        return ""

    try:
        completed = subprocess.run(
            ["run-parts", "/etc/update-motd.d"],
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
            timeout=10,
        )
    except (OSError, subprocess.TimeoutExpired):
        return ""

    return completed.stdout


def motd_text():
    dynamic_text = dynamic_motd().strip()
    if dynamic_text:
        return dynamic_text

    return read_file("/etc/motd").strip()


print(
    json.dumps(
        {
            "exoMotd": {
                "epoch": int(time.time() * 1000),
                "text": motd_text()[:MAX_MOTD_CHARS],
            }
        },
        separators=(",", ":"),
    )
)
