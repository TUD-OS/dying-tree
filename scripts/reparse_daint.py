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
    in_params = lines[0].split()
    corr_type, corr, nproc, nnodes = in_params[:4]
    per_node = int(nproc) // int(nnodes)
    dying = in_params[4:]
    table = lines[2:]
    print(nnodes, nproc, per_node)

    for line in table:
        writer.writerow([nnodes, per_node] + params + [','.join(dying)] + line.split())

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
        writer.writerow(['Nnodes', 'Per_node', 'TreeType', 'LameK', 'CorrType', 'Nproc',
                         'Corr', 'FaultCount',
                         'i', 'FaultList', 'Size',
                         'AvgTime', 'MinTime', 'MaxTime', 'Iterations'])
        parse_and_write(log_dir, writer)


if __name__ == '__main__':
    main()
