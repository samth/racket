#!/usr/bin/env python3
"""Persist this experiment's summaries as ordinary reviewable repository data."""
from pathlib import Path
import argparse,json,subprocess,time,datetime,re
p=argparse.ArgumentParser();p.add_argument('--run',required=True);p.add_argument('--repo',default='samth/racket');p.add_argument('--wait',action='store_true');a=p.parse_args()
def api(path):
 r=subprocess.run(['gh','api',path],capture_output=True,text=True);assert r.returncode==0,'GitHub metadata request failed';return json.loads(r.stdout)
if a.wait:
 deadline=time.monotonic()+1200
 while api(f'repos/{a.repo}/actions/runs/{a.run}')['status']!='completed':
  assert time.monotonic()<deadline,'benchmark did not finish before collection timeout'
  time.sleep(20)
 # Separate hosted collection: normal Actions artifact access; no local proxy,
 # environment snapshot, credentials, or raw log contents are published.
 dest=Path('incoming');dest.mkdir(exist_ok=True)
 r=subprocess.run(['gh','run','download',a.run,'--repo',a.repo,'--pattern','evidence-*','--dir',str(dest)],capture_output=True,text=True)
 assert r.returncode==0,'artifact download failed with exit '+str(r.returncode)
selection=json.loads(Path('.github/scripts/chez-replay-revisions.json').read_text())
summaries={}
for f in Path('incoming').rglob('summary.json'):
 r=json.loads(f.read_text());summaries[r['name']]=r
jobs=api(f'repos/{a.repo}/actions/runs/{a.run}/jobs?per_page=100')['jobs']
parse=lambda s:datetime.datetime.fromisoformat(s.replace('Z','+00:00'))
def duration(x):return (parse(x['completed_at'])-parse(x['started_at'])).total_seconds() if x.get('completed_at') and x.get('started_at') else None
jobdata={}
for j in jobs:
 m=re.match(r'compare \(([0-9a-f]{8}),',j['name'])
 if not m:continue
 reuse=any(s['name']=='Restore parent state' and s.get('conclusion')=='success' for s in j.get('steps',[]));mode='reuse' if reuse else 'cold'
 jobdata[m[1]+'-'+mode]=dict(id=j['id'],url=f'https://github.com/{a.repo}/actions/runs/{a.run}/job/{j["id"]}',status=j['status'],conclusion=j['conclusion'],job_seconds=duration(j),steps=[dict(name=s['name'],conclusion=s.get('conclusion'),seconds=duration(s)) for s in j.get('steps',[])])
rows=[]
for item in selection['revisions']:
 short=item['sha'][:8];cold=summaries.get(short+'-cold');reuse=summaries.get(short+'-reuse');cj=jobdata.get(short+'-cold');rj=jobdata.get(short+'-reuse')
 row=dict(item,cold=cold,reuse=reuse,cold_job=cj,reuse_job=rj)
 if cold and reuse:
  row['build_saved_seconds']=cold['build_seconds']-reuse['build_seconds'];row['build_percent_faster']=100*(1-reuse['build_seconds']/cold['build_seconds'])
 if cj and rj and cj['job_seconds'] and rj['job_seconds']:
  row['net_job_saved_seconds']=cj['job_seconds']-rj['job_seconds'];row['net_job_percent_faster']=100*(1-rj['job_seconds']/cj['job_seconds'])
 rows.append(row)
report=dict(run_id=a.run,url=f'https://github.com/{a.repo}/actions/runs/{a.run}',rows=rows,parent_summaries=[r for r in summaries.values() if r['mode']=='prepare'],jobs=jobs)
out=Path('benchmark-results');out.mkdir(exist_ok=True);(out/f'chez-ci-replay-{a.run}.json').write_text(json.dumps(report,indent=2)+'\n')
lines=['# Recent Chez commits: measured Linux CI replay','',f'[GitHub Actions run]({report["url"]})','',
'Each target is compared with its immediate parent. Both arms include the audited dependency fixes. The restored arm uses the explicit CI key and native Racket action cache; Chez state is discarded when that key changes.','',
'| Commit | Change | Fresh build | Restored build | Native cache hits | Target job saving |','|---|---|---:|---:|---:|---:|']
for r in rows:
 c,n=r['cold'],r['reuse'];subject=r['subject'].replace('|','\\|')
 if c and n:
  net=f'{r["net_job_saved_seconds"]:.0f}s ({r["net_job_percent_faster"]:.1f}%)' if 'net_job_saved_seconds'in r else 'unavailable'
  lines.append(f'| `{r["sha"][:8]}` | {subject} | {c["build_seconds"]:.1f}s | {n["build_seconds"]:.1f}s | {n["native_cache_hits"]}/10 | {net} |')
 else:lines.append(f'| `{r["sha"][:8]}` | {subject} | incomplete | incomplete | — | — |')
lines += ['','Fresh/restored build time sums the Chez/PB bootstrap and native Racket compilation phases. Target job saving includes checkout, configuration, artifact download/restoration and cleanup; queue time is excluded. Parent preparation is prior-run cache-warming cost and is recorded in the JSON, not counted as free target-PR work.','',
'One pair per commit, on standard Ubuntu 22.04 hosted runners with four build jobs. Small differences are subject to runner noise. This does not measure package setup, the full desktop test step, macOS or iOS; it is not an end-to-end PR speedup claim.','',
'Every successful arm checks Racket bootstrap consistency, runs arithmetic/flfma smoke checks, and records compiler/boot hashes and all 19 native artifact hashes. Detailed phases, source materialization and cache invalidation decisions are retained in the JSON and workflow artifacts.']
(out/f'chez-ci-replay-{a.run}.md').write_text('\n'.join(lines)+'\n')
print('\n'.join(lines))
