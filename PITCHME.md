Tips for the Scientific Programmer
----------------------------------

Michele Simionato@[GEM Foundation](https://www.globalquakemodel.org)

+++?image=risk_map_small.png

+++

**This talk is not about micro-optimizations**

- old tricks like replacing
  sqrt(x^2 + y^2) < R => x\*x + y\*y < R\*R
  are not really interesting
- what makes the difference is using the right library
  (i.e. *scipy.spatial.distance*)
- *and* using the right architecture

---

**Input/output formats**

- *never*, EVER change the input formats
- there is more freedom with the output formats

---

**Inputs formats we are using**

- INI (good, but TOML would have been better)
- XML/NRML/XSD (could have been simpler)
- CSV (should have been used more)
- HDF5 (UCERF3, GMPE tables)

+++

**Output formats we are using**

- XML / NRML: we are removing it
- CSV with pre-header: we are using it more and more
- HDF5: used sometimes
- NPZ: by necessity

+++?image=hazard_outputs.png

+++

**Internal formats we are using**

- .hdf5
- .toml
- .sqlite

---

**The choice of the input/outputs format has a big performance impact**

- XML/CSV exporters
- XML/CSV importers
- Clearly the choice of the internal formats is even more important:
  @color[green](HDF5 is the way to go)

---

**Task distribution**

- we are using *multiprocessing* on a single machine
- and celery/rabbitmq on a cluster

![rabbitmq](rabbitmq.png)
![celery](celery.jpeg)

- celery/rabbitmq is not ideal for our use case but it worked
  until now, including the @color[green](REVOKE) functionality
  
+++?image=slow-task.png
**slow tasks :-(**

+++

** Breakthrough: subtasks **

```python
def task_splitter(elements, arg1, arg2, ...):
    blocks = split_in_blocks(elements, maxweight)
    for block in blocks[:-1]:
         yield (task_func, block, arg1, arg2, ...)
    yield task_func(block[-1], arg1, arg2, ...)
```
We made the task launcher able to recognize tuples of the form
`(callable, arg1, arg2, ...)`;
this made it possible to define task splitters

---

**Data transfer**

- we use zmq to return the outputs
- we use NFS to read the inputs

+++
---

**what's behind: @color[green](zmq)**

![zeromq](zeromq-logo.jpg)

- I studied the zmq book after EuroPython 2017
- I implemented what I needed in 2-3 days @fa[thumbs-up]
- it worked really well, even if not at the first attempt

+++

**what's behind: @color[green](zmq)**

- data is sent back via zmq now (PUSH/PULL pattern)
- we bypassed celery/rabbitmq limits completely
- one must be careful: packets keep circulating
- we have a plan B if celery/rabbitmq should fail us again, but we are
  not going to reinvent the wheel

---

**what we might be using: @color[gray](dask)**

- not used until now because we are conservative
- dask documentation has improved a lot
- `dask.distributed.Client.map` is the easy migration path I was looking for
- we are not using it in production but we have experimental support for it
- we'd love to hear from you :-)

---

**what we are NOT using**

- @color[red](C extensions)
- @color[red](Cython)
- @color[red](numba)
- @color[red](Intel Python)

+++

**@color[red](C extensions)**

- we had C extensions in the past, before the wheels
- gcc was required to install the software
- otherwise a slower fallback was used
- there was duplication between Python and C
- migrating to Python 3 would have been hard
- but I was able to speedup the Python(numpy) part!
- now the code base is 100% pure Python @fa[thumbs-up]

+++

**@color[red](Cython)**

- Cython is better than plain C
- and we have wheels now
- however I could not get impressive speedups
- in any case most our code is not suitable for Cythonization
- it was deemed not worthy

+++

**@color[red](numba)**

- I tried it on our code computing the distances
- the only way I could get a substantial speedup was by using the parallel
  option
- but we are already parallelizing, so we would have risked oversubscription
- I was also worried about memory consumption
- at the end @color[green](scipy.spatial.distance) saved the day
