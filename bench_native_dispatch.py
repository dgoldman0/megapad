"""Bounded native dispatch kernels; not physical typing evidence."""
import argparse, hashlib, json, sys, time, statistics
from pathlib import Path
root=Path(__file__).resolve().parent
parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--extension-dir', type=Path, help='directory with an earlier native extension to compare')
parser.add_argument('--output', type=Path)
args=parser.parse_args()
sys.path.insert(0,str(root))
if args.extension_dir is not None: sys.path.insert(0, str(args.extension_dir.resolve()))
from simulator.runtime import MegaForthRuntime
import _megaforth_native
cases={
 'arithmetic': b': RUN 0 20000 BEGIN DUP WHILE SWAP OVER 3 * 1+ + SWAP 1- REPEAT DROP ;',
 'field_reads': b'CREATE RECORD 64 ALLOT : A 16 + ; : B 24 + ; : RUN 11 RECORD A ! 22 RECORD B ! 0 20000 BEGIN DUP WHILE RECORD A @ RECORD B @ + ROT + SWAP 1- REPEAT DROP ;',
 'loop_calls': b': DOUBLE 2 * ; : RUN 0 20000 0 DO I DOUBLE + LOOP ;',
 'dynamic_calls': b": DOUBLE 2 * ; : RUN 0 20000 0 DO I ['] DOUBLE EXECUTE + LOOP ;",
 'scattered_reads': b'CREATE RECORD 16384 ALLOT : RUN 0 20000 BEGIN DUP WHILE DUP 511 AND 8 * RECORD + @ ROT + SWAP 1- REPEAT DROP ;',
}
result={'extension_sha256':hashlib.sha256(Path(_megaforth_native.__file__).read_bytes()).hexdigest(), 'cases':{}}
for name,source in cases.items():
 r=MegaForthRuntime(execution_backend='native');r.evaluate(source,step_budget=300000)
 trials=[]
 for _ in range(4):
  r.main_context.data.clear()
  start=time.perf_counter_ns();run=r.execute('RUN',step_budget=1500000);elapsed=time.perf_counter_ns()-start
  trials.append({'ns':elapsed,'steps':run.semantic_steps,'data':r.main_context.data.snapshot()})
 result['cases'][name]={'trials':trials,'median_ns':statistics.median(t['ns'] for t in trials[1:])}
report=json.dumps(result, indent=2) + '\n'
if args.output is not None: args.output.write_text(report)
print(report, end='')
