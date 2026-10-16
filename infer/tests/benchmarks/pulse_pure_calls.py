#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""Recreated stress cases from the review; capture once, analyze both binaries.

These are independent reconstructions, not copies of the unavailable generators.
CPU/RSS cover `analyze` only. Runs are serial, have a timeout, and record commands.
"""
import argparse
from contextlib import closing
import hashlib
import json
import pathlib
import sqlite3
import subprocess

TAILS = {
    'eq': 'if (b == a) return r + 1; return r;',
    'gt': 'if (b > a) return r + 1; return r;',
    'gt2': 'if (b > a) return r + 1; if (b < a) return r - 1; return r;',
    'two': 'if (b > a && d < c) return r + 1; if (b >= c) return r - 1; return r;',
    'ret': 'if (b > a) return r + b - a; return r;',
    'ret2x': 'if (b > a) return r + 2*b - 2*a; return r;',
    'ret3x': 'if (b > a) return r + 3*b - 3*a; return r;',
    'r2d': 'if (2*b > 2*a) return r + 2*(b-a); return r;',
    'r2c': 'if (2*b > 2*a+1) return r + 2*(b-a); return r;',
    'r6': 'if (6*b > 6*a+5) return r + 6*b - 6*a; return r;',
    'm23': 'if (b > a && d > c) return r + 2*(b-a) + 3*(d-c); return r;',
    'mod': 'if ((b-a) % 2) return r + 1; return r;',
    'shr': 'if ((b-a) >> 1) return r + 1; return r;',
    'div': 'if ((b-a) / 2 > 0) return r + 1; return r;',
    'band': 'if ((b^a) & 1) return r + 1; return r;',
    'half': 'if (b > a) return r / 2 + (b-a); return r;',
    'modr': 'return r + (b-a) % 3;',
    'modr2': 'return r + (b-a) % 3;',
}


def source(kind, depth):
    const = '' if kind == 'modr2' else 'const '
    lines = [f'struct S {{ int x; int y; }}; int q({const}struct S* s); '
             'int p(const struct S* s);',
             f'int f{depth}(struct S* s, int k) {{ return s->x + k; }}']
    for i in reversed(range(depth)):
        arithmetic_kind = kind.removeprefix('mixed_')
        if arithmetic_kind in ('kdiv', 'kmod', 'kmul'):
            expr = {'kdiv': 'k / 3', 'kmod': 'k % 3', 'kmul': 'k * k'}[arithmetic_kind]
            body = f'int r = f{i+1}(s, k+1); return r + {expr};'
            if kind.startswith('mixed_'):
                body = f'q(s); s->y = {i}; ' + body
        else:
            second_before = 'int c = p(s);' if kind in ('two', 'm23') else ''
            second_after = 'int d = p(s);' if kind in ('two', 'm23') else ''
            body = (f'int a = q(s); {second_before} int r = f{i+1}(s, k); '
                    f's->x = r+1; int b = q(s); {second_after} {TAILS[kind]}')
        lines.append(f'int f{i}(struct S* s, int k) {{ {body} }}')
    return '\n'.join(lines) + '\n'


def run_case(args, kind, depth):
    bins = {'base': str(args.base), 'pr': str(args.candidate)}
    work = args.output / f'{kind}-{depth}'
    work.mkdir(parents=True, exist_ok=True)
    src = work / 'chain.c'
    src.write_text(source(kind, depth))
    out = work / 'infer-out'
    capture = [bins['pr'], 'capture', '-j', '1', '-o', str(out), '--',
               str(args.clang), '-c', str(src), '-o', str(work / 'chain.o')]
    (work / 'capture-command.json').write_text(json.dumps(capture))
    with (work / 'capture.log').open('w') as log:
        subprocess.run(capture, stdout=log, stderr=subprocess.STDOUT, check=True)
    with closing(sqlite3.connect(out / 'capture.db')) as connection:
        count = connection.execute('SELECT count(*) FROM procedures').fetchone()[0]
        if count < depth + 1:
            raise RuntimeError(f'Incomplete capture: {count} procedures at depth {depth}')
    for rep in range(args.repeats):
        for label in (('base', 'pr') if rep % 2 == 0 else ('pr', 'base')):
            stats = work / f'{label}-{rep}.time'
            analyze = [bins[label], 'analyze', '--reanalyze', '--pulse-only', '-j', '1', '-o', str(out)]
            cmd = ['/usr/bin/time', '-f', '%U %S %e %M', '-o', str(stats),
                   'timeout', '--kill-after=5', str(args.timeout), *analyze]
            with (work / f'{label}-{rep}.log').open('w') as log:
                p = subprocess.run(cmd, stdout=log, stderr=subprocess.STDOUT)
            row = dict(shape=kind, depth=depth, variant=label, repeat=rep,
                       exit=p.returncode, command=analyze)
            user, system, wall, rss = stats.read_text().splitlines()[-1].split()
            row.update(user_s=float(user), system_s=float(system), wall_s=float(wall), rss_kb=int(rss))
            db = out / 'results.db'
            row['db_bytes'] = db.stat().st_size if db.exists() else None
            # SQLite retains free pages after --reanalyze. Report live storage too,
            # so alternating binaries does not confuse allocation with summary growth.
            if db.exists() and p.returncode == 0:
                with closing(sqlite3.connect(db)) as connection:
                    pages = connection.execute('PRAGMA page_count').fetchone()[0]
                    free = connection.execute('PRAGMA freelist_count').fetchone()[0]
                    size = connection.execute('PRAGMA page_size').fetchone()[0]
                    row['db_live_bytes'] = (pages - free) * size
                    row['pulse_summary_bytes'] = connection.execute(
                        'SELECT sum(length(Pulse)) FROM specs').fetchone()[0]
            report = out / 'report.json'
            row['issues'] = (len(json.loads(report.read_text()))
                             if p.returncode == 0 and report.exists() else None)
            with (args.output / 'measurements.jsonl').open('a') as f:
                f.write(json.dumps(row) + '\n')
            print(json.dumps({k: v for k, v in row.items() if k != 'command'}), flush=True)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--base', type=pathlib.Path, required=True)
    parser.add_argument('--candidate', type=pathlib.Path, required=True)
    parser.add_argument('--clang', type=pathlib.Path, required=True)
    parser.add_argument('--output', type=pathlib.Path, required=True)
    shapes = list(TAILS) + ['kdiv', 'kmod', 'kmul',
                            'mixed_kdiv', 'mixed_kmod', 'mixed_kmul']
    parser.add_argument('--shapes', nargs='+', choices=shapes, default=shapes)
    parser.add_argument('--depths', nargs='+', type=int, default=[20, 40, 80])
    parser.add_argument('--timeout', type=int, default=40)
    parser.add_argument('--repeats', type=int, default=1)
    args = parser.parse_args()
    for name in ('base', 'candidate', 'clang', 'output'):
        # Preserve compiler symlinks: Infer dispatches capture using argv[0].
        setattr(args, name, getattr(args, name).absolute())
    if args.output.exists():
        parser.error('--output must be a new directory to avoid mixing measurements')
    if min(args.depths) < 1 or args.repeats < 1 or args.timeout < 1:
        parser.error('depths, repeats, and timeout must be positive')
    args.output.mkdir(parents=True)
    versions = {label: subprocess.check_output([str(exe), '--version'], text=True)
                for label, exe in [('base', args.base), ('candidate', args.candidate), ('clang', args.clang)]}
    (args.output / 'versions.json').write_text(json.dumps(versions, indent=2))
    digests = {}
    for label, exe in [('base', args.base), ('candidate', args.candidate), ('clang', args.clang)]:
        digest = hashlib.sha256()
        with exe.open('rb') as stream:
            for chunk in iter(lambda: stream.read(1024 * 1024), b''):
                digest.update(chunk)
        digests[label] = digest.hexdigest()
    (args.output / 'metadata.json').write_text(json.dumps(
        {'arguments': vars(args), 'binary_sha256': digests}, default=str, indent=2))
    for shape in args.shapes:
        for n in args.depths:
            run_case(args, shape, n)
