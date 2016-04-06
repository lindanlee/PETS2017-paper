import datetime
import json
import re
import sys

class Record(object):
    def __init__(self, exec_id, date, event):
        self.exec_id = exec_id
        self.date = date
        self.event = event

    def __repr__(self):
        return "%s,%s,%s" % (self.exec_id, self.date, self.event)

def parse_date_string(s):
    m = re.match(r'^(.*)(\.\d\d\d)Z$', s)
    d = datetime.datetime.strptime(m.group(1), "%Y-%m-%dT%H:%M:%S")
    return d.replace(microsecond=int(float(m.group(2)) * 1000000))

# Returns a list of Records.
def parse_records(f):
    records = []
    for line in f:
        parts = line.strip().split(" ", 3)
        if parts[0] != "INST":
            raise ValueError("expected an INST line: %r" % line)
        exec_id = parts[1]
        date = parse_date_string(parts[2])
        event = json.loads(parts[3])
        records.append(Record(exec_id, date, event))
    return records

# Return an iterator over input (filename, file) tuples (stdin if no files are named).
def input_files(args):
    if not args:
        yield "stdin", sys.stdin
        return
    for filename in args:
        with open(filename) as f:
            yield filename, f
