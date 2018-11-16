#!/usr/bin/env python3

import sys
import os
import glob
import csv

def parse_and_write_file(run_file, writer):
    params = os.path.basename(run_file).split('+')
    with open(run_file) as f:
        lines = f.readlines()

    # Read first line of a log
    dying = lines[0].split(';')
    table = lines[2:]

    for line in table:
        writer.writerow(params + [','.join(dying)] + line.split())

def parse_and_write(log_dir, writer):
    for run_file in glob.glob('{}/*'.format(log_dir)):
        if '.csv' in run_file:
            continue
        if 'hostfile' in run_file:
            continue
        if '.env' in run_file:
            continue
        if '.sh' in run_file:
            continue
        parse_and_write_file(run_file, writer)

def main():
    log_dir = sys.argv[1]

    with open(log_dir + '/table.csv', 'w') as f:
        writer = csv.writer(f)
        writer.writerow(['TreeType', 'LameK', 'CorrType', 'Nnodes', 
                         'Corr', 'FaultCount',
                         'GossipSeed', 'GossipRounds',
                         'Per_node', 'Nproc',
                         'i', 'FaultList', 'Size',
                         'AvgTime', 'MinTime', 'MaxTime', 'Iterations'])
        parse_and_write(log_dir, writer)


if __name__ == '__main__':
    main()
